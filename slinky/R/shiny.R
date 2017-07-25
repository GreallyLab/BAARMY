### To run demo: slinky_live(obj1, obj2) ###



group1 <- obj1_pheno[,colnames(obj1_pheno)=="female"]
group2 <- obj2_pheno[,colnames(obj2_pheno)=="female"]
axis_choice
object1_axis_one <- axis_choice[1]
object1_axis_two <- axis_choice[2]
object2_axis_one <- axis_choice[1]
object2_axis_two <- axis_choice[2]
d3_plot <- plotMultiSurface(pca1, pca2, group1=group1,group2=group2,
                            object1_axis_one=object1_axis_one, object1_axis_two=object1_axis_two,
                            object2_axis_one=object2_axis_one, object2_axis_two=object2_axis_two)
class(d3_plot)
plot.new()

??scatterplot3d

#install.packages("shinythemes")
#install.packages("scatterplot3d")
library(scatterplot3d)
library(shinythemes)
library(RColorBrewer)
library(ggplot2)
source("R/plotMultiSurface.R")

obj1 <- readRDS("../social_isolation_data/eset_Y5.rds")
obj2 <- readRDS("../social_isolation_data/eset_Y10.rds")

obj1 <- readRDS("../social_isolation_data/eset_Y5_ov.rds")
obj2 <- readRDS("../social_isolation_data/eset_Y10_ov.rds")



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
  if ( !require('RColorBrewer') ) {
    stop("'slinky_live()' requires 'RColorBrewer'. Please install it using
         install.packages('RColorBrewer')")
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
  obj1_var <- varLabels(obj1)[col_factor1]

  # get factors only for the group
  obj2_pheno[,1] <- as.character(obj2_pheno[,1])
  col_factor2 <- NULL
  for (i in 1:ncol(obj2_pheno)){
    col_factor2[i] <- is.factor(obj2_pheno[,i])
  }
  obj2_var <- varLabels(obj2)[col_factor2]
  # get factors present in both eSETS
  idx_var_same <- which(obj2_var %in% obj1_var)
  pheno_both <- obj2_var[idx_var_same]

  # run PCA on expression data
  obj1_expr <- exprs(obj1)
  obj2_expr <- exprs(obj2)

  pca1 <- prcomp(na.omit(obj1_expr))$rotation
  pca2 <- prcomp(na.omit(obj2_expr))$rotation

  # get axis choices for ui
  axis_choice <- colnames(pca1)

  # Things needed for plot function:
  #PC matrices, group to color by, PCs to plot (obj1_x, obj1_y, obj2_x, obj2_y)

  # generate the UI using this command
  p_layout <- navbarPage(
    a('slinky', href = 'https://github.com/GreallyLab/BAARMY', target = '_blank',
      style = 'color: black;'),
    windowTitle = 'slinky',
    theme = shinytheme("flatly"),
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
         column(6,offset = 3,
          selectInput('groupby', label = 'color by:', choices = pheno_both, selected = obj1_var[1])
         )
        ),
        fluidRow(
          column(3,
            selectInput('obj1_x',
            label = 'object 1 x-axis:',
            choices = axis_choice,
            selected = NULL)),
          column(3,
                 selectInput('obj1_y',
                             label = 'object 1 y-axis:',
                             choices = axis_choice,
                             selected = NULL)),
          column(3,
                 selectInput('obj2_x',
                             label = 'object 2 x-axis:',
                             choices = axis_choice,
                             selected = NULL)),
          column(3,
                 selectInput('obj2_y',
                             label = 'object 2 y-axis:',
                             choices = axis_choice,
                             selected = NULL))
        ),
        fluidRow(
          actionButton('plot_3d_go', "Plot")
          ),
        fluidRow(
          plotOutput('d3_plot')
        ),
        fluidRow(
          uiOutput("download_3d_plot_button")
        )
      )
    )
  )

  # server function
  server_fun <- function(input, output) {
    # Reactive master object that stores all plots and tables for downloading later
    saved_plots_and_tables <- reactiveValues(
      d3_plot = NULL
    )
    user_settings <- reactiveValues(save_width = 45, save_height = 11)
    # TODO: Once user settings are available, read these values from input

    # this is a reactive UI command.. that we may
    #output$which_beta_ctrl_qq <- renderUI({
    #  current_ui <- NULL
    #  poss_tests <- list_tests(obj, input$settings_test_type)
    #  if (settings$test_type == 'wt') {
    #    poss_tests <- poss_tests[[input$which_model_qq]]
    #    current_ui <- selectInput('which_test_qq', 'beta: ',
    #                              choices = poss_tests, selected = poss_tests[1])
    #  } else {
    #    # TODO: I believe this code is defunct due to the conditionalPanel()
    #    current_ui <- selectInput('which_test_qq', 'test: ',
    #                              choices = poss_tests, selected = poss_tests[1])
    #  }

    #  current_ui
    #})

    plot_button <- eventReactive(input$plot_3d_go, {
      group1 <- obj1_pheno[,colnames(obj1_pheno)==input$groupby]
      group2 <- obj2_pheno[,colnames(obj2_pheno)==input$groupby]
      saved_plots_and_tables$d3_plot <- plotMultiSurface(pca1, pca2, group1=group1,group2=group2,
                                  object1_axis_one=input$obj1_x , object1_axis_two=input$obj1_y,
                                  object2_axis_one=input$obj2_x, object2_axis_two=input$obj2_y)
      # generate the download button
      output$download_3d_plot_button <- renderUI({
        div(
          align = "right",
          downloadButton("download_3d_plot", "Download Plot"))
      })
      saved_plots_and_tables$d3_plot
    })

    # generate the plot filler
    output$d3_plot <- renderPlot({
      plot_button()
    })

    # download plot pdf filler
    output$download_3d_plot <- downloadHandler(
      filename = function() {"threeD_plot.pdf"},
      content = function(file) {
        pdf(file, width = 6,height = 4)
        print(d3_plot)
        dev.off()
      },
      contentType = "pdf")
  }

  # initilize shiny
  shinyApp(ui = p_layout, server = server_fun, options = "launch.browser")
}

slinky_live(obj1, obj2)
