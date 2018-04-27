rm(list=ls()) # Housekeeping for testing
library(shiny)
library(DT)

source('../notebook/goose_predict.R')

### for testing only:
# input <- list(input_name=data.frame(datapath=as.vector('~/Documents/toy_data.csv')), sims_in=5, yrs_in=5, maxHB_in=2000, target_in=26000)
# input$input_name$datapath <- as.vector(input$input_name$datapath)

progress_i <- 0
assign("progress_i", progress_i, envir = globalenv())
updateProgress <- function() {
    value <- progress$getValue()
    value <- value + (progress$getMax() - value) / 5
}

valInput <- function(val_filename) {
    val_file <- load_input(val_filename)
    return(validate_input(val_file))
}

cull_table_format <- htmltools::withTags(table(
    class = 'display',
    thead(
        tr(
            th('Year'),
            th('Projected mean populaton size'),
            th('Mean culled'),
            th('SD culled'),
            th('Min. culled'),
            th('Max. culled')
        )
    )
))


ui <- fluidPage(
  
  titlePanel(
      "Goose-GMSE", windowTitle = "Goose-GMSE"
  ),
  
  sidebarLayout(
    sidebarPanel(
      br(),
      fileInput("input_name", "Choose data file (XLS, XLSX or CSV)",
                accept = c("application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", ".xls", ".xlsx",
                    "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
        
      sliderInput("yrs_in", "Number of years to project:",
                  min = 1, max = 50,
                  value = 5),
      
      sliderInput("sims_in", "Number of simulations:",
                  min = 5, max = 100, value = 5, step=5),
      
      sliderInput("target_in", "Population target:",
                  min = 5000, max = 50000,
                  value = 26000, step=1000),
      
      sliderInput("maxHB_in", "Max. cull per year:",
                  min = 0, max = 10000, value = 2000, step = 1000),
    
      actionButton('run_in', 'Run simulations'),
      br(),
      br(),
      br(),
      img(src = "GMSE_logo_name.png", width='90%')
      
    ),
    
    mainPanel(
      
      tabsetPanel(id = "inTabset",
        tabPanel(title = "Introduction", value = "intro_tab", 
                 h2("Goose-GMSE"),
                 h3("Projecting population size under uncertainty and yearly culls"),
                 p("The text below needs editing and expanding", style = "color:red; font-weight: bold"),
                 p("Goose-GMSE allows the user to easily run a series of population models for the Islay population of Greenland Barnacle Goose", 
                   em("Branta leucopsis.")),
                 p("The models used here are based on logistic growth models where po pulation carrying capacity and growth rate 
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
        tabPanel(title = "Help", value = "help_tab", 
                 h3("Extra help for running Goose-GMSE simulations"),
                 br(),
                 p("The text below needs editing and expanding", style = "color:red; font-weight: bold"),
                 strong("Key points to note:"),
                 p(),
                 tags$ul(
                     tags$li("A base data (input) file", strong("must"), "be loaded and formatted correctly. The extact format details are as follows: XXX"),
                     tags$li("The output graph and summarised output are the result of simulations including observation uncertainty and some process
                             uncertainty regarding future environmental conditions. Thus, it should be noted that the output and any inferences from 
                             them are stochastic (i.e. contain a degree of uncertainty).", strong("Thus, any interpretation and recommendations derived
                             from this output should take account of this uncertainty: neither the projected population sizes nor the numbers culled should be 
                     )        taken as absolute recommendation.")), 
                     tags$li("Third list item")
                 )
                 ),
        tabPanel(title = "Output: summary", value = "out_tab",
                    plotOutput('plot', height='500px'),
                    htmlOutput('text_summary'),
                    dataTableOutput('out_culls')
                 ),
        tabPanel(title = "Output: culls", value = "out_tab2",
                 dataTableOutput('out_culls2')
                 ),
        tabPanel(title = "Output: all", value = "out_tab3",
                 ""
                 #tableOutput('out_all')
                 )
        
        )
        

      )
    )
  )



server <- function(session, input, output) {
  
  ### Switch to main output tab when run button is pressed
  observeEvent(input$run_in, {
     updateTabsetPanel(session=session, inputId="inTabset", selected="out_tab")
    })

  ### calcPlot() runs when run button is pressed.
  ### Runs main simulations (gmse_goose_multiplot() given input variables)
  calcPlot <- eventReactive(input$run_in, {

    validate(
        need(try(input$input_name), "Please select a base data input file first.")
    )

    #updateTabsetPanel(session=session, inputId="inTabset", selected="out_tab")
    
    progress_i <- 0
    assign("progress_i", progress_i, envir = globalenv()) 
    progress <- shiny::Progress$new(session = session, min = 0, max = input$sims_in*(input$yrs_in+1))
    on.exit(progress$close())
    assign("progress", progress, envir = globalenv())
    
    progress$set(message = "Running simulations...", value = 0)
    
    sims <- gmse_goose_multiplot(
        data_file=input$input_name$datapath, 
        iterations=input$sims_in, 
        proj_yrs = input$yrs_in, 
        max_HB=input$maxHB_in, 
        manage_target = input$target_in)
    
    assign("sims", sims, envir = globalenv())

    
  })
  
  ### genSummaryText() runs when run button is pressed.
  ### Generates summary output text from simulations.
  genSummaryText <- eventReactive(input$run_in, { 
    if(!exists("sims")) {
        div(
            "Please load a base data file (see 'Help' for more info)"
        )
    } else {
        res <- gmse_goose_summarise(sims, input)
        
        if(is.na(res$first_overlap)) {
            first_overlap <- paste("The projected population does not overlap the target in any of the projected years")
        } else {
            first_overlap <- paste("The range of projected population sizes overlapped the target of", 
                                   input$target_in, "individuals in", sum(res$target_overlap), "out of", res$end_yr-res$last_obs_yr, "projected years.",
                                   "The first year in which the range of projected population sizes overlapped with the population target was", res$first_overlap)
        }
        
        p1 <- paste("In", res$end_yr,"the projected population size was between", floor(res$end_min), "and", floor(res$end_max), "individuals.") 
        p2 <- paste("The projected population in ", res$end_yr, 
                    switch(as.character(res$end_min<input$target_in & res$end_max>input$target_in), 'TRUE'='does', 'FALSE'='does not'),
                    "overlap with the population target of", input$target_in
        )
        p3 <- paste("After ", res$end_yr-res$last_obs_yr, "projected years, the mean population size is predicted to be", 
                    abs(floor(res$end_mean)-input$target_in), "individuals", 
                    switch(as.character(sign(floor(res$end_mean)-input$target_in)), "-1"="below", "1"="above"), 
                    "the population target of", input$target_in
        )
        p4 <- first_overlap
        
        div(
            tags$ul(
                h4("Summarised simulation results", style='align=left'),
                tags$li(p1),
                tags$li(p2),
                tags$li(p3),
                tags$li(p4, style = "color:red; font-weight: bold")
            )
        )
    }
  })
  
  ### cullTable() runs when calcPlot() has been run.
  ### Generates summary table of mean population estimates and mean numbers culled (across simulations)
  cullTable <- eventReactive(input$run_in, {
    validate(need(try(input$input_name), ""))
     
    #updateTabsetPanel(session=session, inputId="inTabset", selected="out_tab2") 

    res <- gmse_goose_summarise(sims, input)
        
    cull_summary <- cbind( (res$last_obs_yr+1):(res$end_yr-1) ,
                               floor(res$proj_y_mn),
                               floor(res$mean_HB),
                               floor(res$sd_HB),
                               floor(res$min_HB),
                               floor(res$max_HB))
    cull_summary <- as.data.frame(cull_summary)
    coln <- c('Year','Projected mean populaton size','Mean culled','SD culled','Min. culled','Max. culled')
    datatable(cull_summary, colnames=coln, rownames=FALSE, options = list(dom = 't'))
    
  })
  
  output$plot <- renderPlot({
    calcPlot()
  })
  
  output$text_summary <- renderPrint({
    genSummaryText()
  })
  
  output$out_culls <- renderDataTable({
    cullTable()
  })

  output$out_all <- renderTable({
    allTable()
  })

}

shinyApp(ui = ui, server = server)