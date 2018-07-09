# load libraries
library(shiny)
library(tidyverse)
library(DT)

# load data
load("movies.Rdata")

ui <- fluidPage(
  titlePanel("Movies DB - Basic shiny app !"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # select Y axis input
      selectInput(
        inputId = "y",
        label = " Y-axis",
        choices = c("imdb_rating", "imdb_num_votes", "critics_score",
                    "audience_score", "runtime"),
        # set default value
        selected = "critics_score"
      ),

      # select x axis input
      selectInput(
        inputId = "x",
        label = "X-axis",
        choices = c("imdb_rating", "imdb_num_votes", "critics_score",
                    "audience_score", "runtime"),
        # set default value
        selected = "audience_score"
      ),

      # select color by input
      selectInput(
        inputId = "color",
        label = "Select Input to color by",
        choices = c("title_type", "genre", "mpaa_rating", "critics_rating"),
        # set default value
        selected = "mpaa_rating"
      ),
      
      textInput(inputId = "graph_title", 
                label = "Add title to graph:", 
                value = "Add dynamic title here"),

      # tick to show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data summary",
                    value = TRUE
                    
      ),

      # slider for alpha
      sliderInput(inputId = "alpha", 
                  label = "select alpha", 
                  min = 0.0, 
                  max = 1.0, 
                  value = 0.5, 
                  step = 0.1),

      # slider for dot size
      sliderInput(inputId = "size", 
                  label = "select size", 
                  min = 1, 
                  max = 5, 
                  value = 3, 
                  step = 1),
      
      
      # make checkbox input to select movie title type
      checkboxGroupInput(inputId = "check", 
                         label = "Select title type: ", 
                         choices = levels(movies$title_type),
                         # select all title type by default
                         selected = levels(movies$title_type)
    ),
    
    actionButton(inputId = "write_csv",
                 label = "Save as CSV")

    ),
    
    mainPanel(
      # display scatter plot
      plotOutput(outputId = "scatterplot"),
      
      # display summary table
      DT::dataTableOutput(outputId = "movietable")
    )
  )
)
  
server <- function(input, output) {
  
  # make reactive movies_filtered data on the basis of user input about title_type
  movies_filtered <- reactive({
    movies %>% 
      filter(title_type %in% input$check)
  })

  # make a ggplot
  output$scatterplot <- renderPlot({
    ggplot(movies_filtered(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point(alpha = input$alpha,
                 size = input$size) + 
      labs(title = input$graph_title)
  })
  
  # make a summary table
  output$movietable <- DT::renderDataTable({
    if(input$show_data) {
      DT::datatable(data = movies %>%
        select(1:6), options = list(pageLength = 10),
        rownames = FALSE)
    }

  })
  
  # write csv data to local drive
  observeEvent(input$write_csv, {
    filename <- paste0("movies", ".csv")
    write_csv(movies_filtered(), path = filename)
    
  })
  
  
}

shinyApp(ui = ui, server = server)