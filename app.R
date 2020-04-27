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
  windowTitle = HTML("COVID-19 cases in Canada"),
  title = div("COVID-19 cases in Canada", img(src = "maple-leaf.png", style = "margin-left: 10px; margin-right: 5px; height: 20px; width: auto;")),
  theme = shinytheme("cerulean"),
  tabPanel("Home",
     sidebarPanel(
       uiOutput("data_snapshot"),
       uiOutput("age_group"),
       uiOutput("gender"),
       #uiOutput("hospitalized"),
       width = 3
     ),
     mainPanel(
       tags$style(type="text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }",
          "a:hover { text-decoration: none !important; }"
       ),
       div(tags$strong("Please use with caution: "), "this data is preliminary and subject to change. Please visit ", tags$a(href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310076601", target = "_blank", style = "color: #c27571; font-weight: bold; text-decoration: underline;", "this page"), "to learn more about the data.", style = "background-color: #f4e4e4; color: #c27571; border: 1px solid #efd5d9; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(),
       textOutput("number_of_cases") %>% withSpinner(color = "#44ade9"), br(),
       plotlyOutput("get_cumulative_incidence_plot") %>% withSpinner(color = "#44ade9"), br(), br(), 
       plotlyOutput("get_incidence_plot") %>% withSpinner(color = "#44ade9"), br(), br(), 
       DTOutput("get_crosstab_table") %>% withSpinner(color = "#44ade9"), br(), br(),
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
       tags$style(type="text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"
       ),
       div(tags$strong("Note: "), "at least two data snapshots must be selected for a comparison to be made.", style = "background-color: #fcf9e7; color: #a99368; border: 1px solid #faefd4; border-radius: 3px; width: 100%; padding: 10px;"), br(), 
       plotlyOutput("get_data_snapshot_plot") %>% withSpinner(color = "#44ade9"), br(), br(),
       DTOutput("get_crosstab_table2") %>% withSpinner(color = "#44ade9"), br(), br(),
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
       tags$style(type="text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"
       ),
      div(tags$strong("Source: "), "Statistics Canada.", tags$a(href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310076601", target = "_blank", style = "color: #a99368; font-weight: bold; text-decoration: underline;", "Table  13-10-0766-01."), " Detailed confirmed cases of coronavirus disease (COVID-19) (Preliminary data), Public Health Agency of Canada.", style = "background-color: #fcf9e7; color: #a99368; border: 1px solid #faefd4; border-radius: 3px; width: 100%; padding: 10px;"), br(), 
      DTOutput("get_cansim_data") %>% withSpinner(color = "#44ade9"), br(), br(),
      width = 9
     )
  ),
  tabPanel("About",
     mainPanel(
       tags$style(type="text/css",
                  ".shiny-output-error { visibility: hidden; }",
                  ".shiny-output-error:before { visibility: hidden; }"
       ),
       p("This R Shiny app was developed by ", tags$a(href = "https://www.barnzilla.ca", target = "_blank", "Joel Barnes.")), br(), 
       width = 12
     )
  )
)

# Define server logic 
server <- function(input, output) {
  cached <- reactiveValues()
  # Get data for the app
  get_data <- function(snapshot = input$data_snapshot, compute_crosstabs = TRUE) {
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
      
      if(compute_crosstabs) {
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
        crosstab <- crosstab %>% mutate(`Age Group` = rep("All ages", nrow(crosstab)), Gender = rep("All genders", nrow(crosstab)))
        
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
        crosstab2$Gender <- rep("All genders", nrow(crosstab2))
       
        # Restructure as tibble
        crosstab2 <- as_tibble(crosstab2)
        
        # Create a crosstab by gender
        crosstab3 <- d %>% group_by(`Age Group`, Gender, `Episode Date`) %>% tally()
        
        # Rename the n vector
        names(crosstab3)[ncol(crosstab3)] <- "Incidence"
        
        # Restructure as tibble
        crosstab3 <- as_tibble(crosstab3)
        
        # Compute cumulative incidence
        #crosstab2 <- crosstab2 %>% group_by(`Age Group`) %>% mutate(`Cumulative Incidence` = cumsum(Incidence))
        crosstab3$`Cumulative Incidence` <- unlist(aggregate(crosstab3$Incidence, by = list(crosstab3$Gender, crosstab3$`Age Group`), cumsum)$x)
        
        # Restructure as tibble
        crosstab3 <- as_tibble(crosstab3)
        
        # Add day column
        crosstab3 <- crosstab3 %>% group_by(`Age Group`, Gender) %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = min(crosstab$`Episode Date`)))
        
        # Restructure as tibble
        crosstab3 <- as_tibble(crosstab3)
        
        # Combine both crosstabs
        crosstab <- rbind(crosstab, crosstab2, crosstab3)
        
        # Ensure that the age group vector is a factor
        crosstab$`Age Group` <- factor(crosstab$`Age Group`)
        
        # Relevel the age group factor
        crosstab$`Age Group` <- relevel(crosstab$`Age Group`, ref = "All ages")
        
        # Ensure that the age group vector is a factor
        crosstab$Gender <- factor(crosstab$Gender)
        
        # Relevel the age group factor
        crosstab$Gender <- relevel(crosstab$Gender, ref = "All genders")
        
        # Add crosstab to list
        l$crosstab <- crosstab
      }
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
      crosstab <- crosstab %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = min(crosstab$`Episode Date`)))
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
      crosstab2 <- crosstab2 %>% group_by(`Age Group`) %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = min(crosstab$`Episode Date`)))
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
    d <- get_data(compute_crosstabs = FALSE)$d
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
  output$gender <- renderUI({
    d <- get_data(compute_crosstabs = FALSE)$d
    if(is.null(d)) { 
      return() 
    } else {
      options <- factor(c("All genders", d$Gender))
      options <- relevel(options, ref = "All genders")
      options <- levels(options)
      names(options) <- levels(options)
      cached$gender_options <- options
      selectInput("gender", label = "Genders", choices = options)
    }
  })
  
  # Build age group menu based on the number of available age groups
  output$hospitalized <- renderUI({
    d <- get_data(compute_crosstabs = FALSE)$d
    if(is.null(d)) { 
      return() 
    } else {
      options <- factor(c("All conditions", d$Hospitalized))
      options <- relevel(options, ref = "All conditions")
      options <- levels(options)
      names(options) <- levels(options)
      cached$hospitalized_options <- options
      selectInput("hospitalized", label = "Hospitalized", choices = options)
    }
  })
  
  
  # Build age group menu based on the number of available age groups
  output$age_group2 <- renderUI({
    #d <- get_data(compute_crosstabs = FALSE)$crosstab
    if(is.null(cached$age_options)) { 
      return() 
    } else {
      #options <- levels(d$`Age Group`)
      #names(options) <- levels(d$`Age Group`)
      selectInput("age_group2", label = "Age groups", choices = cached$age_options)
    }
  })
  
  # Build age group menu based on the number of available age groups
  output$age_group3 <- renderUI({
    
    if(is.null(cached$age_options)) { 
      return() 
    } else {
      #options <- levels(get_data()$crosstab$`Age Group`)
      #names(options) <- levels(get_data()$crosstab$`Age Group`)
      options <- cached$age_options
      checkboxGroupInput("age_group3", label = "Age groups", choices = options, selected = options[options != "All ages" & options != "Not stated"])
    }
  })
  
  # Build data snapshot menu based on the number of snapshots available
  output$data_snapshot <- renderUI({
    cached$files <- list.files(pattern = "*.xlsx")
    if(is.null(cached$files)) { 
      return() 
    } else {
      # Get a list of data files that currently exist
      files <- sort(cached$files, decreasing = TRUE)
      
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
    if(is.null(cached$files)) { 
      return() 
    } else {
      # Get a list of data files that currently exist
      files <- sort(cached$files, decreasing = TRUE)
      
      if(! paste0("Table 13-10-0766-01 - updated ", format(Sys.time(), "%Y-%m-%d"), ".xlsx") %in% list.files()) {
        files <- c(paste0("Table 13-10-0766-01 - updated ", format(Sys.time(), "%Y-%m-%d"), ".xlsx"), files)
      }
      
      # Isolate the date portion of the file name(s) to use in the drop down menu
      file_names <- unname(sapply(files, function(x) strsplit(x, "updated ")[[1]][2]))
      file_names <- gsub(".xlsx", "", file_names)
      
      # Reformt the date
      names(files) <- format(as.Date(file_names), "%B %d, %Y")
      
      checkboxGroupInput("data_snapshot2", label = "Data snapshots", choices = files, selected = c(input$data_snapshot))
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
      pageLength = 50, 
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
    cached$crosstab %>% filter(`Age Group` %in% input$age_group & Gender %in% input$gender) %>% arrange(desc(Day)) %>% select(`Age Group`, Gender, `Episode Date`, Day, everything()),
    extensions = c("Buttons", "Scroller"), 
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible = FALSE, targets = c())),
      pageLength = 50, 
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
  output$get_crosstab_table2 <- renderDT(
    cached$d %>% filter(`Age Group` %in% input$age_group2) %>% arrange(desc(Day)) %>% select(`Age Group`, `Episode Date`, Day, Snapshot, everything()),
    extensions = c("Buttons", "Scroller"), 
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible = FALSE, targets = c())),
      pageLength = 50, 
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
    crosstab <- get_data()$crosstab
    if(is.null(crosstab) | is.null(input$age_group) | is.null(input$gender)) {
      return()
    } else {
      cached$crosstab <- crosstab
      point_size <- 0.5
      element_text_size <- 12
      #plot_width <- 900
      #plot_height <- 614
      ggplotly(ggplot(crosstab %>% filter(`Age Group` %in% input$age_group & Gender %in% input$gender), aes(x = `Episode Date`, y = `Cumulative Incidence`)) +
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
      cached$d <- d
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
    if(is.null(cached$crosstab) | is.null(input$age_group)) {
      return()
    } else {
      point_size <- 0.5
      element_text_size <- 12
      #plot_width <- 900
      #plot_height <- 614
      ggplotly(ggplot(cached$crosstab %>% filter(`Age Group` %in% input$age_group & Gender %in% input$gender), aes(x = `Episode Date`, y = Incidence)) +
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
    cached$d <- get_data(compute_crosstabs = FALSE)$d
    if(is.null(cached$d) | is.null(input$age_group) | is.null(input$gender)) {
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
      cases <- cached$d %>% filter(`Age Group` %in% age_selections & Gender %in% gender_selections)
      prop <- nrow(cases) / nrow(cached$d) * 100
      return(paste0(format(round(prop, 1), nsmall = 1), "% of cases (", format(nrow(cases), big.mark = ","), " out of ", format(nrow(cached$d), big.mark = ","), ") in this data snapshot match the current search criteria."))
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