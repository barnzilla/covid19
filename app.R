# Load the packages
library(dplyr)
library(DT)
library(ggplot2)
library(htmlwidgets)
library(plotly)
library(scales)
library(shiny)
library(shinycssloaders)
library(shinythemes)

# Define UI
ui <- navbarPage(
  windowTitle = HTML("COVID-19 cases in Canada"),
  title = div("COVID-19 cases in Canada", img(src = "maple-leaf.png", style = "margin-left: 10px; margin-right: 5px; height: 20px; width: auto;")),
  theme = shinytheme("cerulean"),
  tabPanel("Home",
   sidebarPanel(
     uiOutput("snapshot"),
     uiOutput("summary_type"),
     uiOutput("grouping_variable"),
     uiOutput("age_group"),
     uiOutput("death"),
     uiOutput("gender"),
     uiOutput("hospital_status"),
     uiOutput("occupation"),
     uiOutput("region"),
     uiOutput("transmission"),
     uiOutput("x_axis_note"),
     width = 3
   ),
   mainPanel(
     tags$style(type="text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }",
      "a:hover { text-decoration: none !important; }",
      ".col-sm-9 { position: fixed; right: 0; }"
     ),
     div(tags$strong("Please use with caution: "), "this data is preliminary and subject to change. Please visit ", tags$a(href = "https://www150.statcan.gc.ca/n1/pub/13-26-0003/132600032020001-eng.htm", target = "_blank", style = "color: #c27571; font-weight: bold; text-decoration: underline;", "this page"), "to learn more about the data.", style = "background-color: #f4e4e4; color: #c27571; border: 1px solid #efd5d9; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(),
     plotlyOutput("get_line_plot") %>% withSpinner(color = "#44ade9"), br(), br(), br(),
     width = 9
   )
  )
)

# Define server logic
server <- function(input, output) {

  # Get a data snapshot
  get_data <- function(x) {
    d <- readRDS(paste0("data/", x))
    d <- d[order(d$`Episode date`),]
    return(d)
  }

  # Build age group menu based on the number of available age groups
  output$age_group <- renderUI({
    if(is.null(input$snapshot)) {
      return()
    } else {
      d <- get_data(input$snapshot)
      options <- factor(d$`Age group`)
      options <- levels(options)
      names(options) <- levels(options)
      checkboxGroupInput("age_group", label = "Age groups", choices = options, selected = options[options != "Not stated"])
    }
  })

  # Build death menu based on the number of available options
  output$death <- renderUI({
    if(is.null(input$snapshot)) {
      return()
    } else {
      d <- get_data(input$snapshot)
      options <- factor(d$Death)
      options <- levels(options)
      names(options) <- levels(options)
      checkboxGroupInput("death", label = "Death", choices = options, selected = options)
    }
  })

  # Build age group menu based on the number of available age groups
  output$gender <- renderUI({
    if(is.null(input$snapshot)) {
      return()
    } else {
      d <- get_data(input$snapshot)
      options <- factor(d$Gender)
      options <- levels(options)
      names(options) <- levels(options)
      checkboxGroupInput("gender", label = "Gender", choices = options, selected = options)
    }
  })

  # Build line plot
  output$get_line_plot <- renderPlotly({
    if(is.null(input$snapshot)) {
      return()
    } else {
      req(input$grouping_variable)
      # Set plot settings
      point_size <- 0.5
      element_text_size <- 12
      x_label <- "Date"

      # Get data
      d <- get_data(input$snapshot)

      # Subset data
      d <- d %>% filter(
        `Age group` %in% input$age_group,
        `Death` %in% input$death,
        `Gender` %in% input$gender,
        `Hospital status` %in% input$hospital_status,
        `Occupation` %in% input$occupation,
        `Region` %in% input$region,
        `Transmission` %in% input$transmission
      )

      # Create a crosstab
      l <- paste0("list(d$`Episode date`, d$`", input$grouping_variable, "`)")
      crosstab <- aggregate(
        d$Counts,
        eval(parse(text = l)),
        sum
      )

      # Compute cumulative sums
      crosstab <- cbind(
        crosstab,
        csum = ave(crosstab$x, crosstab$Group.2, FUN = cumsum)
      )

      # Rename columns
      variable_names <- paste0('c("Episode date", "', input$grouping_variable, '", "Counts", "Cumulative Counts")')
      names(crosstab) <- eval(parse(text = variable_names))

      # If user wants cumulative sum, update the crosstab
      if(input$summary_type %in% c("Cumulative cases")) {
        y_variable <- "Cumulative Counts"
        y_label <- "Cumulative cases"
      } else {
        y_variable <- "Counts"
        y_label <- "New cases"
      }

      # Render plot
      ggplotly(ggplot(crosstab, aes(x = `Episode date`, y = !!rlang::sym(y_variable), group = !!rlang::sym(input$grouping_variable))) +
      geom_line(aes(color = !!rlang::sym(input$grouping_variable)), size = point_size) +
      xlab(x_label) +
       ylab(y_label) +
       scale_y_continuous(labels = comma) +
       theme_minimal() +
       theme(
         plot.title = element_text(size = element_text_size),
         axis.title.x = element_text(size = element_text_size),
         axis.title.y = element_text(size = element_text_size),
         legend.text = element_text(size = element_text_size * 0.8),
         legend.title = element_blank(),
         legend.position = "bottom"
       ))
    }
  })

  # Build grouping variable menu based on the number of available options
  output$grouping_variable <- renderUI({
    if(is.null(input$snapshot)) {
      return()
    } else {
      d <- get_data(input$snapshot)
      options <- factor(names(d %>% select(-`Episode date`, -Counts)))
      options <- levels(options)
      names(options) <- levels(options)
      selectInput("grouping_variable", label = "Group by", choices = options, selected = options[1])
    }
  })

  # Build hospital status menu based on the number of available options
  output$hospital_status <- renderUI({
    if(is.null(input$snapshot)) {
      return()
    } else {
      d <- get_data(input$snapshot)
      options <- factor(d$`Hospital status`)
      options <- levels(options)
      names(options) <- levels(options)
      checkboxGroupInput("hospital_status", label = "Hospital Status", choices = options, selected = options)
    }
  })

  # Build region menu based on the number of available options
  output$occupation <- renderUI({
    if(is.null(input$snapshot)) {
      return()
    } else {
      d <- get_data(input$snapshot)
      options <- factor(d$Occupation)
      options <- levels(options)
      names(options) <- levels(options)
      checkboxGroupInput("occupation", label = "Occupation", choices = options, selected = options)
    }
  })

  # Build region menu based on the number of available regions
  output$region <- renderUI({
    if(is.null(input$snapshot)) {
      return()
    } else {
      d <- get_data(input$snapshot)
      options <- factor(d$Region)
      options <- levels(options)
      names(options) <- levels(options)
      checkboxGroupInput("region", label = "Region", choices = options, selected = options)
    }
  })

  # Build data snapshot menu based on the number of snapshots available
  output$snapshot <- renderUI({
    files <- list.files(
      path = paste0(getwd(), "/data"),
      pattern = "*.Rdata"
    )
    if (is.null(files)) {
      return()
    } else {
      # Get a list of data files that currently exist
      files <- sort(files, decreasing = TRUE)

      # Isolate the date portion of the file name(s) to use in the drop down menu
      file_names <- unname(
        sapply(
          files, function(x) {
            file_name <- strsplit(gsub("[.]", "-", x), "-")
            file_name <- paste(file_name[[1]][3:5], collapse = "-")
          }
        )
      )

      # Reformt the date
      names(files) <- format(as.Date(file_names), "%B %d, %Y")

      # Return the menu
      selectInput("snapshot", "Data snapshots", choices = files)
    }
  })

  # Build summary type menu
  output$summary_type <- renderUI({
    if(is.null(input$snapshot)) {
      return()
    } else {
      selectInput("summary_type", label = "Summary type", choices = c("Cumulative cases", "New cases"), selected = "Cumulative cases")
    }
  })

  # Build transmission menu based on the number of available options
  output$transmission <- renderUI({
    if(is.null(input$snapshot)) {
      return()
    } else {
      d <- get_data(input$snapshot)
      options <- factor(d$Transmission)
      options <- levels(options)
      names(options) <- levels(options)
      checkboxGroupInput("transmission", label = "Transmission", choices = options, selected = options)
    }
  })

  # Build x-axis note
  output$x_axis_note <- renderUI({
    if(is.null(input$snapshot)) {
      return()
    } else {
      div(tags$strong("* "), "This data contains only episode year and episode week. For each case, the first day of the case's episode week was assumed in order to create a complete episode date.", style = "background-color: #f4e4e4; color: #c27571; border: 1px solid #efd5d9; border-radius: 3px; width: 100%; padding: 10px;")
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
