# Load the packages
library("cansim")
library("DT")
library("ggplot2")
library("lubridate")
library("plotly")
library("plyr")
library("readxl")
library("scales")
library("shiny")
library("shinycssloaders")
library("shinythemes")
library("tidyverse")
library("writexl")

# Define UI
ui <- navbarPage(
    windowTitle = HTML("COVID-19 cases in Canada"),
    title = div("COVID-19 cases in Canada", img(src = "maple-leaf.png", style = "margin-left: 10px; margin-right: 5px; height: 20px; width: auto;")),
    theme = shinytheme("cerulean"),
    tabPanel("Home",
        sidebarPanel(
            uiOutput("snapshot"),
            uiOutput("summary_type"),
            uiOutput("age_group"),
            uiOutput("gender"),
            uiOutput("hospital_status"),
            uiOutput("death"),
            uiOutput("transmission"),
            uiOutput("x_axis"),
            uiOutput("x_axis_note"),
            width = 3
        ),
        mainPanel(
            tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }",
                "a:hover { text-decoration: none !important; }"
            ),
            div(tags$strong("Please use with caution: "), "this data is preliminary and subject to change. Please visit ", tags$a(href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310078101", target = "_blank", style = "color: #c27571; font-weight: bold; text-decoration: underline;", "this page"), "to learn more about the data.", style = "background-color: #f4e4e4; color: #c27571; border: 1px solid #efd5d9; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(),
            div(textOutput("case_count") %>% withSpinner(color = "#44ade9"), style = "font-weight: bold; font-size: 1.75rem; text-align: center;") %>% withSpinner(color = "#44ade9"), br(), br(),
            plotlyOutput("get_line_plot") %>% withSpinner(color = "#44ade9"), br(), br(), br(),
            #plotlyOutput("get_incidence_plot") %>% withSpinner(color = "#44ade9"), br(), br(),
            DTOutput("get_plot_table") %>% withSpinner(color = "#44ade9"), br(), br(),
            width = 9
        )
    )
)

# Define server logic
server <- function(input, output) {
    # Cache select data structures
    cached <- reactiveValues()

    # Get data for the comparison of data snapshots
    get_comparison_data <- function(x) {
        if(is.null(x) | ! x %in% list.files()) {
            return(list(d = NULL, crosstab = NULL))
        } else {
            # Get data
            d <- readRDS(x)

            # Isolate date
            snapshot <- strsplit(x, "updated ")[[1]][2]
            snapshot <- gsub(".Rdata", "", snapshot)

            # Reformt the date
            snapshot <- format(as.Date(snapshot), "%B %d, %Y")

            # Add d to list
            l <- list(d = d)

            # Create crosstab for plot
            # Convert episode date to date object
            d$`Episode Date` <- as.Date(d$`Episode Date`, format = "%d-%b-%y")

            # Remove cases with no episode date
            d <- d %>% filter(! is.na(`Episode Date`))

            # Sort data by episode date
            d <- d[order(d$`Episode Date`),]

            # Create a crosstab
            crosstab <- d %>% group_by(`Episode Date`) %>% tally()

            # Rename the n vector
            names(crosstab)[ncol(crosstab)] <- "Incidence"

            # Compute cumulative incidence
            crosstab <- crosstab %>% mutate(`Cumulative Incidence` = cumsum(Incidence))

            # Create an age group vector
            crosstab <- crosstab %>% mutate(`Age Group` = rep("All ages", nrow(crosstab)))

            # Reorder columns
            crosstab <- crosstab %>% select(`Age Group`, everything())

            # Add day column
            crosstab <- crosstab %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = cached$day1))
            crosstab$Snapshot <- rep(snapshot, nrow(crosstab))

            # Create a crosstab by age group
            crosstab2 <- d %>% group_by(`Age Group`, `Episode Date`) %>% tally()

            # Rename the n vector
            names(crosstab2)[ncol(crosstab2)] <- "Incidence"

            # Restructure as tibble
            crosstab2 <- as_tibble(crosstab2)

            # Compute cumulative incidence
            #crosstab2 <- crosstab2 %>% group_by(`Age Group`) %>% mutate(`Cumulative Incidence` = cumsum(Incidence))
            crosstab2$`Cumulative Incidence` <- unlist(aggregate(crosstab2$Incidence, by = list(crosstab2$`Age Group`), cumsum)$x)

            # Restructure as tibble
            crosstab2 <- as_tibble(crosstab2)

            # Add day column
            crosstab2 <- crosstab2 %>% group_by(`Age Group`) %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = cached$day1))
            crosstab2$Snapshot <- rep(snapshot, nrow(crosstab2))

            # Restructure as tibble
            crosstab2 <- as_tibble(crosstab2)

            # Combine both crosstabs
            crosstab <- rbind(crosstab, crosstab2)

            # Ensure that the age group vector is a factor
            crosstab$`Age Group` <- factor(crosstab$`Age Group`)
            crosstab$Snapshot <- factor(crosstab$Snapshot)

            # Relevel the age group factor
            crosstab$`Age Group` <- relevel(crosstab$`Age Group`, ref = "All ages")

            # Add crosstab to list
            l$crosstab <- as.data.frame(crosstab)

            return(l)
        }
    }

    # Crosstab function for plots
    get_crosstab <- function(combo) {
        d <- cached$d
        e1 <- e2 <- e3 <- e4 <- e5 <- e6 <- c()
        if(length(grep("\\All\\b", combo$age_group)) == 0) {
            e1 <- "`Age Group` %in% combo$age_group &"
        }
        if(length(grep("\\All\\b", combo$gender)) == 0) {
            e2 <- "Gender %in% combo$gender &"
        }
        if(length(grep("\\All\\b", combo$hospital_status)) == 0) {
            e3 <- "`Hospital Status` %in% combo$hospital_status &"
        }
        if(length(grep("\\All\\b", combo$death)) == 0) {
            e5 <- "Death %in% combo$death &"
        }
        if(length(grep("\\All\\b", combo$transmission)) == 0) {
            e6 <- "`Transmission` %in% combo$transmission &"
        }

        if(length(e1) > 0 | length(e2) == 0 | length(e3) == 0 | length(e4) == 0 | length(e5) == 0 | length(e6) == 0) {
            e <- paste0("d %>% filter(", e1, e2, e3, e4, e5, e6, ")")
            e <- gsub("&)", ")", e)

            d <- eval(parse(text = e))
        }

        # Convert episode date to date object
        d$`Episode Date` <- as.Date(d$`Episode Date`, format = "%d-%b-%y")

        # Remove cases with no episode date
        d <- d %>% filter(! is.na(`Episode Date`))

        # Sort data by episode date
        d <- d[order(d$`Episode Date`),]

        # Create a crosstab
        crosstab <- d %>% group_by(`Episode Date`) %>% tally()

        # Rename the n vector
        names(crosstab)[ncol(crosstab)] <- "Sum"

        # Compute cumulative incidence
        crosstab <- crosstab %>% mutate(`Cumulative Sum` = cumsum(Sum))

        # Create an age group vector
        crosstab <- crosstab %>% mutate(`Age Group` = rep(combo$age_group, nrow(crosstab)), Gender = rep(combo$gender, nrow(crosstab)), `Hospital Status` = rep(combo$hospital_status, nrow(crosstab)), Death = rep(combo$death, nrow(crosstab)), `Transmission` = rep(combo$transmission, nrow(crosstab)))

        # Reorder columns
        crosstab <- crosstab %>% select(`Age Group`, Gender, `Hospital Status`, Death, `Transmission`, everything())

        # Add day column
        crosstab <- crosstab %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = cached$day1))

        return(crosstab)
    }

    # Get the data from a snapshot
    get_data <- function(snapshot) {
        if(is.null(snapshot)) {
            return()
        } else {
            if(snapshot %in% list.files()) {
                d <- readRDS(snapshot)
            } else {
                d <- NULL
            }
            return(readRDS("+Table 13-10-0781-01 - updated 2020-12-12.Rdata"))
        }
    }

    # Compute the day numbers from day 1
    get_days <- function(x, day1) {
        day1 <- day1 - 1
        day <- c()
        for(i in 1:length(x)) {
            day[i] <- as.numeric(x[i] - day1)
        }
        return(day)
    }

    # Build age group menu based on the number of available age groups
    output$age_group <- renderUI({
        d <- get_data(input$snapshot)
        cached$d <- d
        cached$day1 <- sort(as.Date(d$`Episode Date`, format = "%d-%b-%y"))[1]
        if(is.null(d)) {
            return()
        } else {
            options <- factor(c("All ages", d$`Age Group`))
            options <- relevel(options, ref = "All ages")
            options <- levels(options)
            names(options) <- levels(options)
            cached$age_options <- options
            checkboxGroupInput("age_group", label = "Age groups", choices = options, selected = options[options != "All ages" & options != "Not stated"])
        }
    })

    # Build age group menu based on the number of available age groups
    output$age_group2 <- renderUI({
        if(is.null(cached$age_options)) {
            return()
        } else {
            selectInput("age_group2", label = "Age groups", choices = cached$age_options)
        }
    })

    # Print the number of cases selected
    output$case_count <- renderText({
        if(is.null(cached$d) | is.null(input$age_group) | is.null(input$gender) | is.null(input$hospital_status) | is.null(input$death) | is.null(input$transmission)) {
            return()
        } else {
            if("All ages" %in% input$age_group) {
                age_selections <- unique(cached$d$`Age Group`)
            } else {
                age_selections <- input$age_group
            }
            if(input$gender == "All genders") {
                gender_selections <- unique(cached$d$Gender)
            } else {
                gender_selections <- input$gender
            }
            if(input$hospital_status == "All conditions") {
                hospital_selections <- unique(cached$d$`Hospital Status`)
            } else {
                hospital_selections <- input$hospital_status
            }
            if(input$death == "All conditions") {
                death_selections <- unique(cached$d$Death)
            } else {
                death_selections <- input$death
            }
            if(input$transmission == "All conditions") {
                transmission_selections <- unique(cached$d$`Transmission`)
            } else {
                transmission_selections <- input$transmission
            }
            cases <- cached$d %>% filter(`Age Group` %in% age_selections & Gender %in% gender_selections & `Hospital Status` %in% hospital_selections & Death %in% death_selections & `Transmission` %in% transmission_selections)
            prop <- nrow(cases) / nrow(cached$d) * 100
            return(paste0(format(round(prop, 1), nsmall = 1), "% of cases (", format(nrow(cases), big.mark = ","), " out of ", format(nrow(cached$d), big.mark = ","), ") in this data snapshot match the current search criteria."))
        }
    })

    # Build age group menu based on the number of available age groups
    output$death <- renderUI({
        d <- cached$d
        if(is.null(d)) {
            return()
        } else {
            options <- factor(c("All conditions", d$Death))
            options <- relevel(options, ref = "All conditions")
            options <- levels(options)
            names(options) <- levels(options)
            cached$death_options <- options
            radioButtons("death", label = "Deceased", choices = options)
        }
    })

    # Build age group menu based on the number of available age groups
    output$transmission <- renderUI({
        d <- cached$d
        if(is.null(d)) {
            return()
        } else {
            options <- factor(c("All conditions", d$`Transmission`))
            options <- relevel(options, ref = "All conditions")
            options <- levels(options)
            names(options) <- levels(options)
            cached$transmission_options <- options
            radioButtons("transmission", label = "Transmission", choices = options)
        }
    })

    # Build gender menu based on the number of available age groups
    output$gender <- renderUI({
        d <- cached$d
        if(is.null(d)) {
            return()
        } else {
            options <- factor(c("All genders", d$Gender))
            options <- relevel(options, ref = "All genders")
            options <- levels(options)
            names(options) <- levels(options)
            cached$gender_options <- options
            radioButtons("gender", label = "Genders", choices = options)
        }
    })

    # Build line plot
    output$get_line_plot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        combos <- expand.grid(list(age_group = input$age_group, gender = input$gender, hospital_status = input$hospital_status, death = input$death, transmission = input$transmission), KEEP.OUT.ATTRS = FALSE)
        crosstab <- data.frame()
        for(row in 1:nrow(combos)) {
            crosstab <- rbind(crosstab, get_crosstab(combos[row,]))
        }
        if(is.null(crosstab) | is.null(input$age_group) | is.null(input$gender)) {
            return()
        } else {
            if(input$summary_type == "Sum by month") {
                lookup <- data.frame(
                    short = c(paste0("0", 1:9), 10:12),
                    long = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                )
                crosstab$Month <- substr(crosstab$`Episode Date`, 6, 7)
                crosstab$Month <- unname(sapply(crosstab$Month, function(x) lookup$long[lookup$short == x]))
                crosstab$Month <- factor(crosstab$Month, levels = lookup$long)
                crosstab <- crosstab %>% group_by(Month, `Age Group`, Gender, `Hospital Status`, Death, Transmission) %>% tally(Sum)
                names(crosstab)[ncol(crosstab)] <- "Sum"
                cached$crosstab <- crosstab
                point_size <- 1
                element_text_size <- 12
                x_label <- "Date"
                ggplotly(ggplot(crosstab, aes(x = Month, y = `Sum`, group = `Age Group`)) +
                 geom_point(stat = "summary", aes(color = `Age Group`), size = point_size) +
                 stat_summary(fun = sum, geom = "line", aes(color = `Age Group`)) +
                 xlab(x_label) +
                 ylab("Sum of cases") +
                 scale_y_continuous(labels = comma) +
                 theme_minimal() +
                 theme(
                     plot.title = element_text(size = element_text_size),
                     axis.title.x = element_text(size = element_text_size),
                     axis.title.y = element_text(size = element_text_size),
                     legend.text = element_text(size = element_text_size),
                     legend.title = element_blank()
                 ))
            } else {
                cached$crosstab <- crosstab
                point_size <- 0.5
                element_text_size <- 12
                x_label <- ifelse(input$x_axis == "Day", "Day number (since first case)", "Date")
                ggplotly(ggplot(crosstab, aes(x = !!rlang::sym(input$x_axis), y = `Cumulative Sum`)) +
                 geom_line(aes(color = `Age Group`), size = point_size) +
                 xlab(x_label) +
                 ylab("Cumulative sum of cases") +
                 scale_y_continuous(labels = comma) +
                 theme_minimal() +
                 theme(
                     plot.title = element_text(size = element_text_size),
                     axis.title.x = element_text(size = element_text_size),
                     axis.title.y = element_text(size = element_text_size),
                     legend.text = element_text(size = element_text_size),
                     legend.title = element_blank()
                 ))
            }
        }
    })

    # Render plot data in a searchable/sortable table
    output$get_plot_table <- renderDT(
        {
            if(input$summary_type == "Sum by month") {
                cached$crosstab %>% filter(`Age Group` %in% input$age_group & Gender %in% input$gender & `Hospital Status` %in% input$hospital_status) %>% arrange(desc(Month)) %>% select(Month, Sum, `Age Group`, Gender, everything())
            } else {
                cached$crosstab %>% filter(`Age Group` %in% input$age_group & Gender %in% input$gender & `Hospital Status` %in% input$hospital_status) %>% arrange(desc(Day)) %>% select(`Episode Date`, Day, Sum, `Cumulative Sum`, `Age Group`, Gender, everything())
            }
        },
        extensions = c("Buttons", "Scroller"),
        rownames = FALSE,
        options = list(
            columnDefs = list(list(visible = FALSE, targets = c())),
            pageLength = 500,
            dom = "Bfrtip",
            buttons = c("colvis", "copy", "csv", "excel", "pdf"),
            deferRender = TRUE,
            searchDelay = 500,
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#111'});",
                "}"
            )
        )
    )

    # Build age group menu based on the number of available age groups
    output$hospital_status <- renderUI({
        d <- cached$d
        if(is.null(d)) {
            return()
        } else {
            options <- factor(c("All conditions", d$`Hospital Status`))
            options <- relevel(options, ref = "All conditions")
            options <- levels(options)
            names(options) <- levels(options)
            cached$hospital_options <- options
            radioButtons("hospital_status", label = "Hospital status", choices = options)
        }
    })

    # Build data snapshot menu based on the number of snapshots available
    output$snapshot <- renderUI({
        cached$files <- list.files(pattern = "^[^~|+]*.Rdata")
        if (is.null(cached$files)) {
            return()
        } else {
            # Get a list of data files that currently exist
            files <- sort(cached$files, decreasing = TRUE)

            # Check to see if new snapshot needs to be created
            now <- now(tzone = "UTC") - hours(4)

            # Check if shiny app is running on local server (don't want to get new data if on a production server)
            on_localhost <- Sys.getenv('SHINY_PORT') == ""

            if (on_localhost & ! paste0("Table 13-10-0781-01 - updated ", format(now, "%Y-%m-%d"), ".Rdata") %in% list.files() & ! paste0("+Table 13-10-0781-01 - updated ", format(now, "%Y-%m-%d"), ".Rdata") %in% list.files() & as.integer(format(now, "%H")) >= 9) {
                # Import a new snapshot
                new_snapshot <- get_cansim("13-10-0781-01", refresh = TRUE)

                # Convert it to wide format
                new_snapshot <- wrangle_data(new_snapshot)

                # Compare it to the last snapshot
                last_snapshot <- readRDS(files[1])
                compare <- all.equal(new_snapshot, last_snapshot)

                # If it's different from the last snapshot, store it and rebuild the snapshot menu
                if(isTRUE(compare) | nrow(new_snapshot) == nrow(last_snapshot)) {
                    saveRDS(as.data.frame(new_snapshot), paste0("+Table 13-10-0781-01 - updated ", format(now, "%Y-%m-%d"), ".Rdata"), compress = "xz")
                } else {
                    saveRDS(as.data.frame(new_snapshot), paste0("Table 13-10-0781-01 - updated ", format(now, "%Y-%m-%d"), ".Rdata"), compress = "xz")
                    cached$files <- list.files(pattern = "^[^~]*.Rdata")
                    files <- sort(cached$files, decreasing = TRUE)
                }
            }

            # Isolate the date portion of the file name(s) to use in the drop down menu
            file_names <- unname(sapply(files, function(x) strsplit(x, "updated ")[[1]][2]))
            file_names <- gsub(".Rdata", "", file_names)

            # Reformt the date
            names(files) <- format(as.Date(file_names), "%B %d, %Y")
            cached$files2 <- files

            selectInput("snapshot", "Data snapshots", choices = files)
        }
    })

    # Build summary type menu
    output$summary_type <- renderUI({
        d <- cached$d
        if(is.null(d)) {
            return()
        } else {
            radioButtons("summary_type", label = "Summary type", choices = c("Cumulative sum by day", "Sum by month"))
        }
    })

    # Build x-axis menu
    output$x_axis <- renderUI({
        d <- cached$d
        if(is.null(d) | input$summary_type == "Monthly sum") {
            return()
        } else {
            options <- c("Episode date*" = "Episode Date", "Day number since first case" = "Day")
            cached$x_axis_options <- options
            radioButtons("x_axis", label = "Horizontal axis", choices = options)
        }
    })

    # Build x-axis note
    output$x_axis_note <- renderUI({
        d <- cached$d
        if(is.null(d)) {
            return()
        } else {
            div(tags$strong("* "), "This data contains only episode year and episode week. For each case, the first day of the case's episode week was assumed in order to create a complete episode date.", style = "background-color: #f4e4e4; color: #c27571; border: 1px solid #efd5d9; border-radius: 3px; width: 100%; padding: 10px;")
        }
    })

    # Wrangle the raw data
    wrangle_data <- function(d) {
        # Reshape data from long to wide format
        d_wide <- spread(d %>% select("Case identifier number", "Case information", VALUE, REF_DATE), "Case information", VALUE)

        # Add leading zeros to case identifier number
        d_wide$`Case identifier number` <- str_pad(d_wide$`Case identifier number`, width = nchar(max(as.numeric(d$`Case identifier number`))), pad = "0")

        # Identify select vectors
        vectors_to_factor <- c("Age group", "Gender", "Region", "Occupation", "Asymptomatic", "Transmission", "Hospital status", "Recovered", "Death")

        # Restructure as factors
        d_wide[vectors_to_factor] <- lapply(d_wide[vectors_to_factor], factor)

        # Add semantic labels
        d_wide$`Age group` <- revalue(d_wide$`Age group`, c("1" = "0-19", "2" = "20-29", "3" = "30-39", "4" = "40-49", "5" = "50-59", "6" = "60-69", "7" = "70-79", "8" = "80+", "99" = "Not stated"), warn_missing = FALSE)
        d_wide$Gender <- revalue(d_wide$Gender, c("1" = "Male", "2" = "Female", "3" = "Non-binary", "7" = "Non-binary", "9" = "Not stated"), warn_missing = FALSE)
        d_wide$Region <- revalue(d_wide$Region, c("1" = "Atlantic", "2" = "Quebec", "3" = "Ontario and Nunavut", "4" = "Prairies and the Northwest Territories", "5" = "British Columbia and Yukon"), warn_missing = FALSE)
        d_wide$Occupation <- revalue(d_wide$Occupation, c("1" = "Health care worker", "2" = "School or daycare worker/attendee", "3" = "Long term care resident", "4" = "Other", "9" = "Not stated"), warn_missing = FALSE)
        d_wide$Asymptomatic <- revalue(d_wide$Asymptomatic, c("1" = "Yes", "2" = "No", "9" = "Not stated"), warn_missing = FALSE)
        d_wide$Transmission <- revalue(d_wide$Transmission, c("1" = "Domestic acquisition", "2" = "International travel", "9" = "Not stated"), warn_missing = FALSE)
        d_wide$`Hospital status` <- revalue(d_wide$`Hospital status`, c("1" = "Hospitalized and in intensive care unit", "2" = "Hospitalized, but not in intensive care unit", "3" = "Not hospitalized", "9" = "Not stated/unknown"), warn_missing = FALSE)
        d_wide$Recovered <- revalue(d_wide$Recovered, c("1" = "Yes", "2" = "No", "9" = "Not stated"), warn_missing = FALSE)
        d_wide$Death <- revalue(d_wide$Death, c("1" = "Yes", "2" = "No", "9" = "Not stated"), warn_missing = FALSE)

        # Add day (select first day of the week since not given), month and reference year vectors together and structure as a date object
        d_wide$`Episode date` <- as.Date(paste(d_wide$REF_DATE, str_pad(d_wide$`Episode week`, width = 2, pad = 0), 1, sep = "-"), "%Y-%U-%u")

        # Change format to %d-%b-%y
        d_wide$`Episode date` <- strftime(d_wide$`Episode date`, format = "%d-%b-%y")

        # Remove unwanted vectors from data
        d_wide <- d_wide %>% select("Case identifier number", "Episode date", Gender, "Age group", "Region", "Occupation", Asymptomatic, Transmission, "Hospital status", Recovered, Death)

        # Rename vectors
        names(d_wide) <- c("CaseID", "Episode Date", "Gender", "Age Group", "Region", "Occupation", "Asymptomatic", "Transmission", "Hospital Status", "Recovered", "Death")

        # Order data by case ids in ascending order
        d_wide <- d_wide %>% arrange(CaseID)

        return(d_wide)
    }
}

# Run the application
shinyApp(ui = ui, server = server)
