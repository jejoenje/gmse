rm(list=ls()) # Housekeeping for testing
library(shiny)
library(DT)

source('goose_predict_gui.R')

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
                  min = 1, max = 15,
                  value = 5),
      
      sliderInput("sims_in", "Number of simulations:",
                  min = 5, max = 1000, value = 5, step=5),
      
      sliderInput("target_in", "Population target:",
                  min = 5000, max = 50000,
                  value = 32000, step=1000),
      
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
                 p("Please note that this is a test release only and the interface is still under construction", style = "color:red; font-weight: bold"),
                 p("Goose-GMSE allows the user to easily run a series of population models for the Islay population of Greenland Barnacle Goose", 
                   em("Branta leucopsis.")),
                 p("The models used here are based on logistic growth models where population carrying capacity and growth rate 
                   are functions of (i.e. constrained by) a number of predictor variables. In brief, these predictors are empirical data on (1) previous yearly 
                   winter goose counts (averages), (2) climatic variability in previous years in both Greenland and on Islay, (3) the available area of improved 
                   grassland on Islay in previous years, (4) the numbers of geese killed (hunted) on Iceland and Greenland.", br(),
                   "These models were structured and parameterised on the basis of previous studies of this system, e.g. ",
                   a("Mason et al. 2018", href="https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/1365-2664.12969"),
                     "and",
                   a("Trinder et al. 2014.", href="http://www.snh.org.uk/pdfs/publications/commissioned_reports/568.pdf"),
                   "The model is described in more detail in SNH Commissioned Report", em("\"Development of a population model tool to predict shooting levels of 
                   Greenland barnacle geese on Islay\""), "(Bunnefeld et al 2018 in review)."
                   ),
                 p("Uncertainty in population numbers in previous years is implemented in two ways. First, observation uncertainty is taken as a fixed variance 
                   derived
                   from the intra-winter differences in counts made on subsequent days.", 
                   span("This observation uncertainty cannot currently be changed by the user.", style = "color:red"),
                   "Second, uncertainty regarding environmental 
                   conditions is currently represented by randomly sampling from environmental data measured over all previous years and re-running the population 
                   models a given number of times (no. of simulations set by the user)."),
                 p("Please choose and load a data set with previous years' data in the top of the menu on the left, 
                    and set the desired modelling parameters. Then press \"run simulations\"", 
                    em("(note that this may take a few minutes to complete particularly if a large number of years or 
                        simulations are chosen)."), 
                   br(), br(),
                   "More information, e.g. on the formatting of this data set and the simulation parameters can be 
                   found in the Help tab." ,style="font-weight:bold"),
                 br()
                 ),
        tabPanel(title = "Help", value = "help_tab", 
                 h3("Extra help for running Goose-GMSE simulations"),
                 br(),
                 p(strong("Input file requirements")),
                 p("A base data (input) file", strong("must"), "be loaded and formatted correctly.", br(),
                   "This file must be in .csv, .xls or .xlsx  format only. Its content must be layed out as the", em("example_data.csv file"), 
                   "and must be in order of year (first column). Projections of future years are taken from the last year (last line) in 
                   the data file, and the user is expected to add new data as this becomes available. It is expected that all data
                   for all years is available (i.e. all rows are complete).", br(),
                   tags$ul(
                       tags$li("Column 1 ('Year'): year in four-digit format."),
                       tags$li("Column 2 ('Count'): the average over-winter count of geese on Islay (in the example data set this was based 
                               on XXX counts over XXX months."),
                       tags$li("Column 3 ('IcelandCull'): the number of geese culled on Iceland in the month prior to the current winter."),
                       tags$li("Column 4 ('IslayCull'): the number of geese culled on Islay over winter."),
                       tags$li("Column 5 ('GreenlandCull'): the number of geese culled on Greenland in the month prior to the current winter."),
                       tags$li("Column 6 ('AIG'): the area of improved grassland in hectares available on Islay."),
                       tags$li("Column 7 ('IslayTemp'): the average temperature (degrees C) on Islay in the preceding winter."),
                       tags$li("Column 8 ('AugRain'): average rainfall (mm) in Greenland during the preceding August."),
                       tags$li("Column 9 ('AugTemp'): temperature (degrees C) in Greenland during the preceding August.")
                   ),
                   em("Please note that any deviations from the above format may cause the model not to run properly and/or projections to be unreliable.")
                   ),
                 p(strong("Interpretation of output")),
                 tags$ul(
                     tags$li(strong("Uncertainty."), "The output graph and summarised output are the result of simulations including observation uncertainty and some process
                              uncertainty regarding future environmental conditions. Thus, it should be noted that the output and any inferences from 
                              them are inherently variable (i.e. contain a degree of uncertainty). Thus, any interpretation and recommendations derived
                              from this output should take account of this uncertainty: neither the projected population sizes nor the numbers culled should be 
                              taken as an absolute recommendation for numbers to be culled."),
                     tags$li(strong("Number of simulations."), "In particular, the interpretation of the model output should take account of the number of simulations that was used to 
                              produce the output: a lower number of simulations will limit the ability of the user to make fair judgements of the 
                              likely variability in projected population sizes. To minimise processing time, we suggest that trial runs of the model 
                              could be made with a small number of simulations, before a final run with a larger number of simulations (e.g 100), on which 
                              inferences could be based.")
                     
                    ),
                 p(strong("Extra information on key parameters")),
                 tags$ul(
                     tags$li(strong("Number of years projected."), "Because of the various sources of uncertainty included in the model (see above and the report),
                             it is important to note that it should be assumed that uncertainty is going to be greater further in the future. For example, the 
                             model currently makes the very simplistic assumption that future climatic conditions will vary randomly within the range measured to 
                             date. In other words, no systematic climate change and how this may affect population dynamics is taken account of.", br(),
                             "Thus, it is best to concentrate interpretation on 1-2 years in the future. Moreover, it is very important that simulations are re-
                             run as more data becomes available)."),
                     tags$li(strong("Max. cull per year."), "It is very important to note that this parameter represent the maximum number culled only and is not
                             a soecific recommendation (it is set by the user). The actual number of individuals culled in each year projected may be lower than 
                             this and the mean, variation and range of this across simulations is provided in the output table. In some cases (e.g. when the population 
                             size exceeds the population target by a large amount), it is likely that the number culled simply equals the maximum. Note that this 
                             reflects a situation when there is no error in the number actually culled (e.g. marksmen are able to hit their target exactly).")
                 )

                 ),
        tabPanel(title = "Output", value = "out_tab",
                    plotOutput('plot', height='500px'),
                    htmlOutput('text_summary'),
                    dataTableOutput('out_culls')
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
        
    cull_summary <- cbind( (res$last_obs_yr+1):(res$end_yr) ,
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