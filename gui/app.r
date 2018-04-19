library(shiny)
library(GMSE)
source('notebook/goose_predict.R')

input_name <- '~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset/toy_data.csv'

progress_i <- 0
assign("progress_i", progress_i, envir = globalenv())
updateProgress <- function() {
    value <- progress$getValue()
    value <- value + (progress$getMax() - value) / 5
}


ui <- fluidPage(
  
  titlePanel(
      "Goose-GMSE", windowTitle = "Goose-GMSE"
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("yrs_in", "Number of years to project:",
                  min = 1, max = 50,
                  value = 5),
      
      sliderInput("sims_in", "Number of simulations:",
                  min = 5, max = 100, value = 5, step=5),
      
      sliderInput("target_in", "Population target:",
                  min = 5000, max = 50000,
                  value = 26000, step=1000),
      
      sliderInput("maxHB_in", "Cull per year:",
                  min = 0, max = 10000, value = 2000, step = 1000),
    
      actionButton('run_in', 'Run simulations')
      
    ),
    
    mainPanel(
      
      tabsetPanel(id = "inTabset",
        tabPanel(title = "Introduction", value = "intro_tab", 
                 h2("Goose-GMSE"),
                 h3("Projecting population size under uncertainty and yearly culls"),
                 p("The text below needs editing and expanding", style = "color:red; font-weight: bold"),
                 p("Goose-GMSE allows the user to easily run a series of population models for the Islay population of Greenland Barnacle Goose", 
                   em("Branta leucopsis.")),
                 p("The models used here are based on logistic growth models where population carrying capacity and growth rate 
                   are functions of (i.e. constrained by) a number of predictor variables. In brief, these predictors are empirical data on (1) previous yearly 
                   winter goose counts (averages), (2) climatic variability in previous years in both Greenland and on Islay, (3) the available area of improved 
                   grassland on Islay in previous years, (4) the numbers of geese killed (hunted) on Iceland and Greenland."),
                 p("Uncertainty in population numbers in previous years is implemented in two ways. First, observation uncertainty is taken as a fixed variance 
                   derived
                   from the intra-winter differences in counts made on subsequent days.", 
                   span("This observation uncertainty cannot currently be changed by the user.", style = "color:red"),
                   "Second, uncertainty regarding environmental 
                   conditions is currently represented by randomly sampling from environmental data measured over all previous years and re-running the population 
                   models a given number of times (no. of simulations set by the user)."),
                 p("The user first updates and loads a data set with previous years' data, and then sets the parameters of interest in the menu on the left."
                   ),
                 br(),
                 actionButton('done', 'Go to output')
                 ),
        tabPanel(title = "Output", value = "out_tab",
                    plotOutput('plot', height='500px'),
                    textOutput('text_summary')
                 )

      )
    )
  )
) 


server <- function(session, input, output) {
  
  
  observeEvent(input$run_in, {
     updateTabsetPanel(session=session, inputId="inTabset", selected="out_tab")
    })
            
  observeEvent(input$done, updateTabsetPanel(session=session, inputId="inTabset", selected="out_tab"))

  calcPlot <- eventReactive(input$run_in, {
    
    updateTabsetPanel(session=session, inputId="inTabset", selected="out_tab")

    progress_i <- 0
    assign("progress_i", progress_i, envir = globalenv())
    progress$initialize(min = 0, max = input$sims_in*(input$yrs_in+1))
    on.exit(progress$close())
    assign("progress", progress, envir = globalenv())
    
    progress$set(message = "Running simulations...", value = 0)
    
    sims <- gmse_goose_multiplot(data_file=input_name, 
                         iterations=input$sims_in, 
                         proj_yrs = input$yrs_in, 
                         max_HB=input$maxHB_in, 
                         manage_target = input$target_in)

  })
  
  output$plot <- renderPlot({
    calcPlot()
  })
  
  output$text_summary <- renderText({
      "The summarised results are as follows:",
      gmse_goose_summarise(sims)$end_yr
      # p("In", gmse_goose_summarise(sims)$end_yr, ", the goose population is projected to be between",
      #   floor(gmse_goose_summarise(sims)$end_min),"and", floor(gmse_goose_summarise(sims)$end_max)) 
  })
  

}

shinyApp(ui = ui, server = server)