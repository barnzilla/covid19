# Load the packages
library("cansim")
library("DT")
library("ggplot2")
library("plotly")
library("plyr")
library("readxl")
library("scales")
library("shiny")
library("shinycssloaders")
library("shinythemes")
library("tidyverse")
library("xlsx")

# Define UI
ui <- navbarPage(
  "COVID-19 cases in Canada",
  theme = shinytheme("cerulean"),
  tabPanel("Age comparisons",
     sidebarPanel(
       uiOutput("data_snapshot"),
       uiOutput("age_group"),
       width = 3
     ),
     mainPanel(
       textOutput("number_of_cases"), br(), plotlyOutput("get_cumulative_incidence_plot") %>% withSpinner(color = "#44ade9"), br(), br(), plotlyOutput("get_incidence_plot") %>% withSpinner(color = "#44ade9"), br(), br(), DTOutput("get_crosstab_table") %>% withSpinner(color = "#44ade9"), br(), br(),
       width = 9
     )
  ),
  tabPanel("Compare data snapshots",
     sidebarPanel(
       uiOutput("age_group2"),
       uiOutput("data_snapshot2"),
       width = 3
     ),
     mainPanel(
       plotlyOutput("get_data_snapshot_plot") %>% withSpinner(color = "#44ade9"), br(), br(),
       width = 9
     )
  ),
  tabPanel("Data source",
     sidebarPanel(
       uiOutput("data_snapshot3"),
       uiOutput("age_group3"),
       width = 3
     ),
     mainPanel(
     p(tags$strong("Source: "), "Statistics Canada.", tags$a(href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310076601", target = "_blank", "Table  13-10-0766-01."), " Detailed confirmed cases of coronavirus disease (COVID-19) (Preliminary data), Public Health Agency of Canada."), br(), DTOutput("get_cansim_data") %>% withSpinner(color = "#44ade9"), br(), br(),
     width = 9
     )
  )
)

# Define server logic 
server <- function(input, output) {
  # Get data for the app
  get_data <- function(snapshot = input$data_snapshot) {
    if(is.null(snapshot)) {
      return(list(d = NULL, crosstab = NULL))
    } else {
      # Check to see if a stored version exists; otherwise, pull from Statcan website
      if(snapshot %in% list.files()) {
        d <- read_xlsx(snapshot)
      } else {
        d <- wrangle_data(get_cansim("13-10-0766-01", refresh = TRUE))
      }
      
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
      crosstab <- crosstab %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = min(crosstab$`Episode Date`)))
      
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
      crosstab2 <- crosstab2 %>% group_by(`Age Group`) %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = min(crosstab$`Episode Date`)))
      
      # Restructure as tibble
      crosstab2 <- as_tibble(crosstab2)
      
      # Combine both crosstabs
      crosstab <- rbind(crosstab, crosstab2)
      
      # Ensure that the age group vector is a factor
      crosstab$`Age Group` <- factor(crosstab$`Age Group`)
      
      # Relevel the age group factor
      crosstab$`Age Group` <- relevel(crosstab$`Age Group`, ref = "All ages")
      
      # Add crosstab to list
      l$crosstab <- crosstab
      
      return(l)
    }
  }
  
  # Get data for the comparison of data snapshots
  get_data2 <- function(x) {
    if(is.null(x) | ! x %in% list.files()) {
      return(list(d = NULL, crosstab = NULL))
    } else {
      # Get data
      d <- read_xlsx(x)
      
      # Isolate date
      snapshot <- strsplit(x, "updated ")[[1]][2]
      snapshot <- gsub(".xlsx", "", snapshot)
      
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
      crosstab <- crosstab %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = min(crosstab$`Episode Date`)), Snapshot = rep(snapshot, nrow(crosstab)))
      
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
      crosstab2 <- crosstab2 %>% group_by(`Age Group`) %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = min(crosstab$`Episode Date`)), Snapshot = rep(snapshot, nrow(crosstab2)))
      
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
      l$crosstab <- crosstab
      
      return(l)
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
    if(is.null(get_data()$crosstab)) { 
      return() 
    } else {
      options <- levels(get_data()$crosstab$`Age Group`)
      names(options) <- levels(get_data()$crosstab$`Age Group`)
      if(! is.null(input$age_group3)) {
        default_selections <- input$age_group3
      } else {
        default_selections <- options[options != "All ages" & options != "Not stated"]
      }
      checkboxGroupInput("age_group", label = "Age groups", choices = options, selected = default_selections)
    }
  })
  
  # Build age group menu based on the number of available age groups
  output$age_group2 <- renderUI({
    if(is.null(get_data()$crosstab)) { 
      return() 
    } else {
      options <- levels(get_data()$crosstab$`Age Group`)
      names(options) <- levels(get_data()$crosstab$`Age Group`)
      selectInput("age_group2", label = "Age groups", choices = options)
    }
  })
  
  # Build age group menu based on the number of available age groups
  output$age_group3 <- renderUI({
    if(is.null(get_data()$crosstab)) { 
      return() 
    } else {
      options <- levels(get_data()$crosstab$`Age Group`)
      names(options) <- levels(get_data()$crosstab$`Age Group`)
      if(! is.null(input$age_group)) {
        default_selections <- input$age_group
      } else {
        default_selections <- options[options != "All ages" & options != "Not stated"]
      }
      checkboxGroupInput("age_group3", label = "Age groups", choices = options, selected = default_selections)
    }
  })
  
  # Build data snapshot menu based on the number of snapshots available
  output$data_snapshot <- renderUI({
    if(length(list.files(pattern = "*.xlsx")) == 0) { 
      return() 
    } else {
      # Get a list of data files that currently exist
      files <- sort(list.files(pattern = "*.xlsx"), decreasing = TRUE)
      
      if(! paste0("Table 13-10-0766-01 - updated ", format(Sys.time(), "%Y-%m-%d"), ".xlsx") %in% list.files()) {
        files <- c(paste0("Table 13-10-0766-01 - updated ", format(Sys.time(), "%Y-%m-%d"), ".xlsx"), files)
      }
      
      # Isolate the date portion of the file name(s) to use in the drop down menu
      file_names <- unname(sapply(files, function(x) strsplit(x, "updated ")[[1]][2]))
      file_names <- gsub(".xlsx", "", file_names)
      
      # Reformt the date
      names(files) <- format(as.Date(file_names), "%B %d, %Y")
      
      selectInput("data_snapshot", "Data snapshots", choices = files)
    }
  })
  
  # Build data snapshot menu based on the number of snapshots available
  output$data_snapshot2 <- renderUI({
    if(length(list.files(pattern = "*.xlsx")) == 0) { 
      return() 
    } else {
      # Get a list of data files that currently exist
      files <- sort(list.files(pattern = "*.xlsx"), decreasing = TRUE)
      
      if(! paste0("Table 13-10-0766-01 - updated ", format(Sys.time(), "%Y-%m-%d"), ".xlsx") %in% list.files()) {
        files <- c(paste0("Table 13-10-0766-01 - updated ", format(Sys.time(), "%Y-%m-%d"), ".xlsx"), files)
      }
      
      # Isolate the date portion of the file name(s) to use in the drop down menu
      file_names <- unname(sapply(files, function(x) strsplit(x, "updated ")[[1]][2]))
      file_names <- gsub(".xlsx", "", file_names)
      
      # Reformt the date
      names(files) <- format(as.Date(file_names), "%B %d, %Y")
      
      checkboxGroupInput("data_snapshot2", label = "Data snapshots", choices = files, selected = c())
    }
  })
  
  # Build data snapshot menu based on the number of snapshots available
  output$data_snapshot3 <- renderUI({
    if(length(list.files(pattern = "*.xlsx")) == 0) { 
      return() 
    } else {
      # Get a list of data files that currently exist
      files <- sort(list.files(pattern = "*.xlsx"), decreasing = TRUE)
      
      if(! paste0("Table 13-10-0766-01 - updated ", format(Sys.time(), "%Y-%m-%d"), ".xlsx") %in% list.files()) {
        files <- c(paste0("Table 13-10-0766-01 - updated ", format(Sys.time(), "%Y-%m-%d"), ".xlsx"), files)
      }
      
      # Isolate the date portion of the file name(s) to use in the drop down menu
      file_names <- unname(sapply(files, function(x) strsplit(x, "updated ")[[1]][2]))
      file_names <- gsub(".xlsx", "", file_names)
      
      # Reformt the date
      names(files) <- format(as.Date(file_names), "%B %d, %Y")
      
      selectInput("data_snapshot3", "Data snapshots", choices = files)
    }
  })
  
  # Render the CANSIM data in a searchable/sortable table
  output$get_cansim_data <- renderDT(
    {
      if("All ages" %in% input$age_group3) {
        get_data(input$data_snapshot3)$d
      } else {
        get_data(input$data_snapshot3)$d %>% filter(`Age Group` %in% input$age_group3)
      }
    },
    extensions = c("Buttons", "Scroller"), 
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible = FALSE, targets = c())),
      pageLength = 100, 
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
  
  # Render plot data in a searchable/sortable table
  output$get_crosstab_table <- renderDT(
    get_data()$crosstab %>% filter(`Age Group` %in% input$age_group) %>% arrange(desc(Day)) %>% select(`Age Group`, `Episode Date`, Day, everything()),
    extensions = c("Buttons", "Scroller"), 
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible = FALSE, targets = c())),
      pageLength = 100, 
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
  
  # Build cumulative incidence plot
  output$get_cumulative_incidence_plot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    if(is.null(get_data()$d) | is.null(input$age_group)) {
      return()
    } else {
      point_size <- 0.5
      element_text_size <- 12
      #plot_width <- 900
      #plot_height <- 614
      ggplotly(ggplot(get_data()$crosstab %>% filter(`Age Group` %in% input$age_group), aes(x = `Episode Date`, y = `Cumulative Incidence`)) +
       geom_line(aes(color = `Age Group`), size = point_size) +
       #ggtitle("Cumulative incidence") +
       xlab("Date") +
       ylab("Cumulative incidence") +
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
  })
  
  # Build data snapshot plot
  output$get_data_snapshot_plot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    if(is.null(input$data_snapshot2) | length(input$data_snapshot2) < 2 | is.null(input$age_group2)) {
      return()
    } else {
      d <- tibble()
      for(i in 1:length(input$data_snapshot2)) {
        d <- rbind(d, get_data2(input$data_snapshot2[i])$crosstab)
      }
      point_size <- 0.5
      element_text_size <- 12
      #plot_width <- 900
      #plot_height <- 614
      ggplotly(ggplot(d %>% filter(`Age Group` %in% input$age_group2), aes(x = `Episode Date`, y = `Cumulative Incidence`)) +
       geom_line(aes(color = Snapshot), size = point_size) +
       #ggtitle("Cumulative incidence") +
       xlab("Date") +
       ylab("Cumulative incidence") +
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
  })
  
  # Build incidence plot
  output$get_incidence_plot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    if(is.null(get_data()$d) | is.null(input$age_group)) {
      return()
    } else {
      point_size <- 0.5
      element_text_size <- 12
      #plot_width <- 900
      #plot_height <- 614
      ggplotly(ggplot(get_data()$crosstab %>% filter(`Age Group` %in% input$age_group), aes(x = `Episode Date`, y = Incidence)) +
         geom_line(aes(color = `Age Group`), size = point_size) +
         #ggtitle("Incidence") +
         xlab("Date") +
         ylab("Incidence") +
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
  })
  
  # Print the number of cases selected
  output$number_of_cases <- renderText({
    if(is.null(get_data()$d) | is.null(input$age_group)) {
      return()
    } else {
      if("All ages" %in% input$age_group) {
        cases <- get_data()$d
        prop <- 100
      } else {
        cases <- get_data()$d %>% filter(`Age Group` %in% input$age_group)
        prop <- nrow(cases) / nrow(get_data()$d) * 100
      }
      return(paste0(format(round(prop, 1), nsmall = 1), "% of cases (", format(nrow(cases), big.mark = ","), " out of ", format(nrow(get_data()$d), big.mark = ","), ") in this data snapshot match the current search criteria."))
    }
  })
  
  # Wrangle the raw data
  wrangle_data <- function(d) {
    # Reshape data from long to wide format
    d_wide <- spread(d %>% select("Case identifier number", "Case information", VALUE, REF_DATE), "Case information", VALUE)
    
    # Add leading zeros to case identifier number
    d_wide$`Case identifier number` <- str_pad(d_wide$`Case identifier number`, width = nchar(max(as.numeric(d$`Case identifier number`))), pad = "0")
    
    # Identify select vectors
    vectors_to_factor <- c("Age group", "Gender", "Transmission", "Hospitalization", "Intensive care unit", "Death")
    
    # Restructure as factors
    d_wide[vectors_to_factor] <- lapply(d_wide[vectors_to_factor], factor)
    
    # Add semantic labels
    d_wide$`Age group` <- revalue(d_wide$`Age group`, c("1" = "0-19", "2" = "20-29", "3" = "30-39", "4" = "40-49", "5" = "50-59", "6" = "60-69", "7" = "70-79", "8" = "80+", "99" = "Not stated"))
    d_wide$Gender <- revalue(d_wide$Gender, c("1" = "Male", "2" = "Female", "7" = "Non-binary", "9" = "Not stated"))
    d_wide$Transmission <- revalue(d_wide$Transmission, c("1" = "Travel exposure", "2" = "Community exposure", "3" = "Pending"))
    d_wide$Hospitalization <- revalue(d_wide$Hospitalization, c("1" = "Yes", "2" = "No", "9" = "Not stated"))
    d_wide$`Intensive care unit` <- revalue(d_wide$`Intensive care unit`, c("1" = "Yes", "2" = "No", "9" = "Not stated"))
    d_wide$Death <- revalue(d_wide$Death, c("1" = "Yes", "2" = "No", "9" = "Not stated"))
    
    # Add day, month and reference year vectors together and structure as a date object
    d_wide$`Episode date` <- as.Date(paste0(d_wide$REF_DATE, "-", str_pad(d_wide$`Episode date - month`, 2, pad = "0"), "-", str_pad(d_wide$`Episode date - day`, 2, pad = "0")), format = "%Y-%m-%d")
    
    # Change format to %d-%b-%y
    d_wide$`Episode date` <- format(d_wide$`Episode date`, format = "%d-%b-%y")
    
    # Remove unwanted vectors from data
    d_wide <- d_wide %>% select("Case identifier number", "Episode date", Gender, "Age group", Transmission, Hospitalization, "Intensive care unit", Death)
    
    # Rename vectors
    names(d_wide) <- c("CaseID", "Episode Date", "Gender", "Age Group", "Exposure Setting", "Hospitalized", "Intensive Care Unit", "Death")
    
    # Order data by case ids in ascending order
    d_wide <- d_wide %>% arrange(CaseID)
    
    # Export data to Excel
    write.xlsx2(as.data.frame(d_wide), paste0("Table 13-10-0766-01 - updated ", format(Sys.time(), "%Y-%m-%d"), ".xlsx"), row.names = FALSE, showNA = FALSE)
    
    return(d_wide)
  }
}

# Run the application 
shinyApp(ui = ui, server = server)