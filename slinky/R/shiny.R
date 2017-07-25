install.packages("shinythemes")
library(shinythemes)

obj1 <- readRDS("eset_Y5.rds")
obj2 <- readRDS("eset_Y10.rds")


slinky_live <- function(obj1, obj2, settings = sleuth_live_settings(),
                        options = list(port = 42427), ...) {
  
  if(!(is(obj1, 'ExpressionSet'))){
    stop("Please supply an ExpressionSet for object 1 before running this command")
  }  
  if(!(is(obj2, 'ExpressionSet'))){
    stop("Please supply an ExpressionSet for object 2 before running this command")
  } 
  if ( !require('shiny') ) {
    stop("'slinky_live()' requires 'shiny'. Please install it using
         install.packages('shiny')")
  }
  if ( !require('shinythemes') ) {
    stop("'slinky_live()' requires 'shinythemes'. Please install it using
         install.packages('shinythemes')")
  }
  
  # insert code to manipulate the esets here 
  # get pheno data
  obj1_pheno <- pData(obj1)
  obj2_pheno <- pData(obj2)
  
  # get factors only for the group
  obj1_pheno[,1] <- as.character(obj1_pheno[,1])
  col_factor1 <- NULL
  for (i in 1:ncol(obj1_pheno)){
    col_factor1[i] <- is.factor(obj1_pheno[,i])
  }
  
  # get factors only for the group
  obj2_pheno[,1] <- as.character(obj2_pheno[,1])
  col_factor2 <- NULL
  for (i in 1:ncol(obj2_pheno)){
    col_factor2[i] <- is.factor(obj2_pheno[,i])
  }
  obj2_var <- varLabels(obj2)[col_factor1]

  idx_var_same <- which()
  
  
  # generate the UI using this command
  p_layout <- navbarPage(
    a('slinky', href = 'https://github.com/GreallyLab/BAARMY', target = '_blank',
      style = 'color: black;'),
    windowTitle = 'slinky',
    theme = shinytheme("flatly")
    tabPanel('overview',
             fluidRow(
               div(h3('slinky live'), align = 'center')
             ),
             fluidRow(
               column(10, offset = 1,
                      p('This Shiny app is designed for exploratory data analysis of
                        multi-dimensional data. There are four menu tabs
                        that can be used to choose plots to view'),
                      p(strong('Brought to you by:')),
                      h2("Team BAARMY")
                      )
             )),
    navbarMenu('Visualize',
      tabPanel('Params',
        fluidRow(
         column(3,
          selectInput('groupby', label = 'units:', choices = obj1_var, selected = obj1_var[1])
         ),
            electInput('gv_var_color_by',
           label = 'color by: ',
           choices = c(NULL, poss_covars),
           selected = NULL))
      )
    )
  )

  # server function 
  server_fun <- function(input, output) {
    # Reactive master object that stores all plots and tables for downloading later
    saved_plots_and_tables <- reactiveValues(
      pca_plt = NULL,
      samp_heat_plt = NULL,
      ma_plt = NULL,
      ma_var_plt = NULL,
      ma_table = NULL,
      test_table = NULL,
      volcano_plt = NULL,
      volcano_table = NULL,
      mv_plt = NULL,
      scatter_plt = NULL,
      scatter_var_plt = NULL,
      scatter_table = NULL,
      qq_plt = NULL,
      cond_dens_plt = NULL,
      samp_dens_plt = NULL,
      sample_table = NULL,
      kallisto_table = NULL,
      hm_plt = NULL,
      bs_var_plt = NULL
    )
    user_settings <- reactiveValues(save_width = 45, save_height = 11)
    # TODO: Once user settings are available, read these values from input
    
    # this is a reactive UI command.. that we may 
    output$which_beta_ctrl_qq <- renderUI({
      current_ui <- NULL
      poss_tests <- list_tests(obj, input$settings_test_type)
      if (settings$test_type == 'wt') {
        poss_tests <- poss_tests[[input$which_model_qq]]
        current_ui <- selectInput('which_test_qq', 'beta: ',
                                  choices = poss_tests, selected = poss_tests[1])
      } else {
        # TODO: I believe this code is defunct due to the conditionalPanel()
        current_ui <- selectInput('which_test_qq', 'test: ',
                                  choices = poss_tests, selected = poss_tests[1])
      }
      
      current_ui
    })
    
    # generate the plot filler 
    output$qqplot <- renderPlot({
      poss_tests <- list_tests(obj, input$settings_test_type)
      current_test <- NULL
      if (input$settings_test_type == 'wt') {
        poss_tests <- poss_tests[[input$which_model_qq]]
      }
      current_test <- poss_tests[1]
      
      qq_plt <- plot_qq(obj, current_test,
                        test_type = input$settings_test_type,
                        which_model = input$which_model_qq,
                        sig_level = input$max_fdr_qq)
      saved_plots_and_tables$qq_plt <- qq_plt
      qq_plt
    })
    
    # download plot pdf filler
    output$download_qq_plt <- downloadHandler(
      filename = function() {
        "qq_plot.pdf"
      },
      content = function(file) {
        ggsave(file, saved_plots_and_tables$qq_plt,
               width = user_settings$save_width,
               height = user_settings$save_height,
               units = "cm")
      })
  }

  
  obj1_var
  
  
  # initilize shiny 
  shinyApp(ui = p_layout, server = server_fun, options = options)
}
