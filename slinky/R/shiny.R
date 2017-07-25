### To run demo: slinky_live(obj1, obj2) ###

# install the packages
#install.packages("shinythemes")
#install.packages("scatterplot3d")
library(scatterplot3d)
library(shinythemes)
library(RColorBrewer)
library(ggplot2)
library(Cairo)
source("R/plotMultiSurface.R")

# obj1 <- readRDS("../social_isolation_data/eset_Y5.rds")
# obj2 <- readRDS("../social_isolation_data/eset_Y10.rds")

#
#obj1 <- readRDS("../social_isolation_data/eset_Y5_ov.rds")
#obj2 <- readRDS("../social_isolation_data/eset_Y10_ov.rds")

obj1 <- readRDS("/Volumes/home/greally-lab/Hackathon/social_isolation_data/eset_Y5_ov_updated.RDS")
obj2 <- readRDS("/Volumes/home/greally-lab/Hackathon/social_isolation_data/eset_Y8_ov_updated.RDS")
obj3 <- readRDS("/Volumes/home/greally-lab/Hackathon/social_isolation_data/eset_Y10_ov_updated.RDS")




slinky_live <- function(obj1, obj2, cell_prop=NULL,
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

  obj2_pheno[1,]
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
                        multi-dimensional data. The Visualize tab will list the
                        visualizations available based on your input.'),
                      p(strong('Brought to you by:')),
                      h3("Team BAARMY")
             ))),
    navbarMenu('Visualize',
      tabPanel('3D plot',
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
          column(10, offset= 1,
                 actionButton('plot_3d_go', "Plot")
          )
        ),
        fluidRow(
          plotOutput('d3_plot',
                     click = "plot1_click",
                     brush = brushOpts(
                       id = "plot1_brush"
                     )
          )

        ),
        fluidRow(
          column(width=8,
                 h4("Points near click"),
                 verbatimTextOutput("click_info"))
        ),
        fluidRow(
          uiOutput("download_3d_plot_button")
        )
      ),
      if (!(is.null(cell_prop))){
        tabPanel('Cell Proportion',
                 fluidRow(
                   column(10, offset = 1,
                          p("If you would like to adjust for cell type proportions
                            please select the button below"))
                 ),
                 fluidRow(
                   actionButton('plot_adjust_go', "Plot Adjusted")
                 ),
                 fluidRow(
                   plotOutput('d3_adjusted_plot')
                 ),
                 fluidRow(
                   uiOutput("download_adjusted_plot_button")
                 )
        )
      }
    )
  )

  # server function
  server_fun <- function(input, output) {
    # Reactive master object that stores all plots and tables for downloading later
    saved_plots_and_tables <- reactiveValues(
      d3_plot = NULL,
      d3_cellProp_plot = NULL
    )
    user_settings <- reactiveValues(save_width = 45, save_height = 11)

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
          style = "margin-right:20px; margin-top: 10px; margin-bottom:10px",
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
        print(saved_plots_and_tables$d3_plot)
        dev.off()
      },
      contentType = "pdf")

    # plot the adjusted graph for cell subtype prop
    if (!(is.null(cell_prop))){
      plot_button <- eventReactive(input$plot_adjust_go, {

        group1 <- obj1_pheno[,colnames(obj1_pheno)==input$groupby]
        group2 <- obj2_pheno[,colnames(obj2_pheno)==input$groupby]
        saved_plots_and_tables$d3_cellProp_plot <- plotMultiSurface(pca1, pca2, group1=group1,group2=group2,
                                                         object1_axis_one=input$obj1_x , object1_axis_two=input$obj1_y,
                                                         object2_axis_one=input$obj2_x, object2_axis_two=input$obj2_y)
        # generate the download button
        output$download_adjusted_plot_button <- renderUI({
          div(
            align = "right",
            style = "margin-right:15px; margin-top: 10px; margin-bottom:10px",
            downloadButton("download_adjust_plot", "Download Plot"))
        })
        saved_plots_and_tables$d3_cellProp_plot
      })

      # download cell prop adjusted plot pdf
      output$download_adjust_plot <- downloadHandler(
        filename = function() {"threeD_cellPropAdj_plot.pdf"},
        content = function(file) {
         pdf(file, width = 6,height = 4)
         print(saved_plots_and_tables$d3_cellProp_plot) # change to real
          dev.off()
        },
        contentType = "pdf")
      # TODO: fix brush click
      output$click_info = renderPrint({
        nearPoints(obj1_pheno,input$plot1_click,addDist = TRUE)
      })
    }
  }

  # initilize shiny
  shinyApp(ui = p_layout, server = server_fun, options = "launch.browser")
}

# cell prop dummy
cell_prop_table <- c("Dummy")

slinky_live(obj1, obj2, cell_prop = cell_prop_table)
slinky_live(obj1, obj2)
