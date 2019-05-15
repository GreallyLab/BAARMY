# slinky with scalability

#' slinky_scale_live_2
#'
#' Shiny data visualization
#'
#' @param list_esets A list of expression sets >=2
#' @import shiny
#' @import shinythemes
#' @import RColorBrewer
#' @import plotly
#' @import webshot
#' @export

install.packages("ggplot2")
# This is just to load and test sample data; this will be removed when finished
library(webshot)
devtools::install_github("ropensci/plotly", force=TRUE)
# then had to remove ggplot2
install.packages("ggplot2")
library(plotly)
install.packages("RSelenium")
library(RSelenium)
Sys.setenv("plotly_username" = "Greally_Lab")
Sys.setenv("plotly_api_key" = "GreallyLab1")

sessionInfo()
devtools::install_github("ropensci/plotly", force=TRUE)
list_sample <- list()
list_sample[[1]] <- obj1
list_sample[[2]] <- obj2
list_sample[[3]] <- obj3
list_esets <- list_sample[1:3]
 
slinky_scale_live(list_sample)

# need to source any functions created for testing
source("R/plotMultiSurface_scale.R")


# this is the long slinky live function
slinky_scale_live3 <- function(list_esets, cell_prop=NULL,
                               options = list(port = 42427), ...) {
  # a bunch of stop if inappropriate data given
  if(!(is(list_esets, 'list'))){
    stop("Please supply a list of expression sets for list_eset before running this command")
  }
  if(length(list_esets)<2){
    stop(paste("You have supplied only supplied 1 expression set. Please supply
               at least two expression sets in list before running this command", sep=""))
  }
  # TODO: write stops if list is not composed of esets
  
  # STOPs if required pacakges are not downloaded (this is largely uneccesary once part of the package)
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
  
  # This code manipulates the esets to ready them for input into plot functions
  # This could be put into seperate functions, but is not too cumbersome at the moment.
  # get pheno data
  cur_list <- list()
  pheno_list <- list()
  head(list_esets)
  for (i in 1:length(list_esets)){
    obj_list <- list()
    eset <- list_esets[[i]]
    pheno <- pData(eset)
    pheno_list[[i]] <- pheno
    col_factor <- NULL
    for (j in 1:ncol(pheno)){
      col_factor[j] <- is.factor(pheno[,j])
    }
    eset_var <- varLabels(eset)[col_factor]
    if (i > 1){
      eset_var_final <- eset_var_final[which(eset_var_final %in% eset_var)]
    }
    if (i == 1) {
      eset_var_final <- eset_var
    }
    eset_expr <- exprs(eset)
    cur_list[[i]] <- prcomp(na.omit(eset_expr))$rotation
  }
  # get axis choices for ui
  axis_choice <- colnames(cur_list[[1]])
  
  l_axis_choices <- list()
  for (i in 1:length(list_esets)) {
    l_axis_choices[[i]] <- colnames(cur_list[[i]])
  }
  
  # Things needed for plot function:
  #list of PC matrices, group to color by, list of vectors PCs to plot (obj1_x, obj1_y, obj2_x, obj2_y)
  # cur_list = PC matrices
  # group vectors  = group to color by
  # axis_choice = vectors to plot by
  
  # generate the UI using this command
  p_layout <- navbarPage(
    a('slinky', href = 'https://github.com/GreallyLab/BAARMY', target = '_blank',
      style = 'color: black;'),
    windowTitle = 'slinky',
    theme = shinytheme("flatly"),
    tabPanel('Overview',
             fluidRow(
               div(h1('slinky live'), align = 'center')
             ),
             fluidRow(
               column(10, offset = 1,
                      p('This Shiny app is designed for exploratory data analysis of
                        multi-dimensional data. The Visualize tab will list the
                        visualizations available based on your input.'),
                      p(strong('Brought to you by:')),
                      p(strong("Team BAARMY"))
                      )
               )),
    navbarMenu('Visualize',
               # 2D plots
               tabPanel('2D plots',
                        fluidRow(
                          div(h1('2D Plots'), align = 'center')
                        ),
                        fluidRow(
                          p("You can make two dimensional plots by selecting the dataset
                            and the axes that you Please select which dataset that you
                            wish to view"),
                          p("Please select the dataset that you which to view.
                            The dataset number is determined by their order in
                            the list_esets argument.")
                          ),
                        fluidRow(
                          column(6,offset = 3,
                                 selectInput('dataset_choice', label = 'dataset:',
                                             choices = as.character(seq(1, length(list_esets),1)), selected = "1")
                          )
                        ),
                        uiOutput("d2_groupby_controls"),
                        uiOutput("d2_axis_controls"),
                        fluidRow(
                          column(10, offset= .5,
                                 actionButton("plot_2d_int_go", "Plot")
                          )
                        ),
                        fluidRow(
                          plotlyOutput('d2_int_plot')
                        ),
                        fluidRow(
                          column(width=8,
                                 verbatimTextOutput("d2_int_event")
                          )
                        )
                        
                          ),
               # non-interactive 3D plot
               tabPanel('3D plot',
                        fluidRow(
                          div(h1("3D Plot"), align = "center")
                        ),
                        fluidRow(
                          column(6,offset = 3,
                                 selectInput("groupby", label = "color by:", choices = eset_var_final, selected = eset_var_final[1])
                          )
                        ),
                        lapply(1:length(list_esets), function(k) {
                          fluidRow(
                            column(3, offset = 2,
                                   selectInput(paste0("obj",k,"x"),
                                               paste0("object ",k," x-axis:"),
                                               choices = axis_choice,
                                               selected = NULL)
                            ),
                            column(3, offset = 2,
                                   selectInput(paste0("obj",k,"y"),
                                               paste0("object ",k," y-axis:"),
                                               choices = axis_choice,
                                               selected = NULL)
                            )
                          )
                        }),
                        fluidRow(
                          column(10, offset= .5,
                                 actionButton("plot_3d_go", "Plot")
                          )
                        ),
                        fluidRow(
                          plotOutput('d3_plot')
                        ),
                        fluidRow(
                          column(width=4, offset = 8,
                                 uiOutput("download_3d_plot_button"))
                        )
               ),
               # interactive 3D plot using plotly
               tabPanel('Interactive 3D plot',
                        fluidRow(
                          div(h1('Interactive 3D Plot'), align = 'center')
                        ),
                        fluidRow(
                          column(6,offset = 3,
                                 selectInput('groupby_3D', label = 'color by:', choices = eset_var_final, selected = eset_var_final[1])
                          )
                        ),
                        lapply(1:length(list_esets), function(k) {
                          fluidRow(
                            column(3, offset = 2,
                                   selectInput(paste0("obj_3D",k,"x"),
                                               paste0("object ",k," x-axis:"),
                                               choices = axis_choice,
                                               selected = NULL)
                            ),
                            column(3, offset = 2,
                                   selectInput(paste0("obj_3D",k,"y"),
                                               paste0("object ",k," y-axis:"),
                                               choices = axis_choice,
                                               selected = NULL)
                            )
                          )
                        }),
                        fluidRow(
                          column(10, offset= .5,
                                 actionButton("plot_3d_int_go", "Plot")
                          )
                        ),
                        fluidRow(
                          plotlyOutput('d3_int_plot')
                        ),
                        fluidRow(
                          column(width=8,
                                 verbatimTextOutput("d3_int_event")),
                          column(width=4,
                                 uiOutput("download_3d_int_plot_button"))
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
      d3_int_plot = NULL,
      d3_cellProp_plot = NULL,
      d2_int_plot = NULL
    )
    user_settings <- reactiveValues(save_width = 45, save_height = 11)
    
    ############################################################################
    ################## Creating the 2D interactive plot ########################
    ############################################################################
    
    # generate the axis control UI based on dataset input
    output$d2_groupby_controls <- renderUI({
      dataset_num <- input$dataset_choice
      fluidRow(
        column(6,offset = 3,
               selectInput("groupby_2d_int",
                           label = 'color by:',
                           choices = eset_var_final,
                           selected = eset_var_final[1])
        )
      )
    })
    
    output$d2_axis_controls <- renderUI({
      # dataset_num <- 1
      dataset_num <- input$dataset_choice
      d2_axes <- l_axis_choices[[dataset_num]]
      fluidRow(
        column(3, offset = 2,
               selectInput(inputId = "d2_int_xaxis",
                           label = paste0("Dataset ",dataset_num," x-axis:"),
                           choices = axis_choice,
                           selected = axis_choice[1])
        ),
        column(3, offset = 2,
               selectInput(inputId = "d2_int_yaxis",
                           label = paste0("Dataset ",dataset_num," y-axis:"),
                           choices = axis_choice,
                           selected = axis_choice[2])
        )
      )
    })
    
    # 2D plot reactive event
    plot_2d_int_button <- eventReactive(input$plot_2d_int_go, {
      # input$dataset_choice <- 1
      
      # generate vector with chosen axes for each eset
      v_axis <- NULL
      v_axis <- c(input$d2_int_xaxis, input$d2_int_yaxis)
      # v_axis <- c("PC1", "PC2")
      
      # grab correct dataset and get correct columns to graph
      dataset_2d_int <- NULL
      dataset_2d_int <- cur_list[[input$dataset_choice]]
      dataset_2d_int <- dataset_2d_int[,colnames(dataset_2d_int) == v_axis]
      
      # get vector of phenotype selected by  groupby
      v_pheno <- NULL
      pheno <- pheno_list[[input$dataset_choice]]
      groupby_d2 <- NULL
      groupby_d2 <- input$groupby_2d_int
      v_pheno <- pheno[,colnames(pheno) == groupby_d2]
      
      # make dataframe containing data and phenotype for plotting
      df_2d_int <- data.frame(x = dataset_2d_int[,1], y = dataset_2d_int[,2],
                              phenotype = v_pheno)
      
      # generate the plot ### NEED TO BUILD FUNCTION ###
      saved_plots_and_tables$d2_int_plot <-
        plot_ly(data = df_2d_int, x= ~x, y= ~y,
                color = ~phenotype, colors = "Dark2")
      
      # There is no download button because the plot is made with plotly
      
      # generate the output text
      output$d2_int_event <- renderPrint({
        d <- event_data("plotly_hover")
        if (is.null(d)) "Hover on a point!" else d
      })
      
      saved_plots_and_tables$d2_int_plot
    })
    
    
    # output the interactive 2D plot
    output$d2_int_plot <- renderPlotly({
      p <- plot_2d_int_button()
      p$elementId <- NULL
      p
    })
    
    ############################################################################
    ######################## Creating the 3D plot ##############################
    ############################################################################
    
    # 3D plot reactive event
    plot_3D_button <- eventReactive(input$plot_3d_go, {
      
      # generate the phenotype list for selected groupby
      l_pheno <- list()
      for (k in 1:length(list_esets)){
        pheno <- pheno_list[[k]]
        l_pheno[[k]] <- pheno[,colnames(pheno)==input$groupby]
      }
      names(l_pheno) <- rep(input$groupby, length(l_pheno))
      
      # generate list with chosen axis for each eset
      l_axis <-list()
      v_axis<-rep(0,2)
      for (k in 1:length(list_esets)){
        v_axis[1] <- input[[paste0("obj",k,"x")]]
        v_axis[2] <- input[[paste0("obj",k,"y")]]
        l_axis[[k]] <- v_axis
      }
      
      #group1 <- obj1_pheno[,colnames(obj1_pheno)==input$groupby]
      #group2 <- obj2_pheno[,colnames(obj2_pheno)==input$groupby]
      
      # generate the plot
      saved_plots_and_tables$d3_plot <- plotMultiSurface_scale(matrixList=cur_list,
                                                               groupList=l_pheno,
                                                               axisList=l_axis)
      
      # generate the download button
      output$download_3d_plot_button <- renderUI({
        div(
          align = "right",
          style = "margin-right:15px; margin-top: 10px; margin-bottom:10px",
          downloadButton("download_3d_plot", "Download Plot"))
      })
      saved_plots_and_tables$d3_plot
    })
    
    # generate 3D plot
    output$d3_plot <- renderPlot({
      plot_3D_button()
    })
    
    # download 3D plot pdf
    output$download_3d_plot <- downloadHandler(
      filename = function() {"threeD_plot.pdf"},
      content = function(file) {
        pdf(file, width = 6,height = 4)
        print(saved_plots_and_tables$d3_plot)
        dev.off()
      },
      contentType = "pdf")
    
    
    
    plot_3d_int_button <- eventReactive(input$plot_3d_int_go, {
      #input[[paste("obj",k,"x", sep="_")]]
      
      # generate the pheno type list
      l_pheno <- list()
      for (k in 1:length(list_esets)){
        pheno <- pheno_list[[k]]
        l_pheno[[k]] <- pheno[,colnames(pheno)==input$groupby_3D]
      }
      names(l_pheno) <- rep(input$groupby_3D, length(l_pheno))
      
      # generate list with chosen axis for each eset
      l_axis <-list()
      v_axis<-rep(0,2)
      for (k in 1:length(list_esets)){
        v_axis[1] <- input[[paste0("obj_3D",k,"x")]]
        v_axis[2] <- input[[paste0("obj_3D",k,"y")]]
        l_axis[[k]] <- v_axis
      }
      
      # l_axis <- list()
      # l_axis[[1]] <- c(1,2)
      # l_axis[[2]] <- c(1,2)
      # l_axis[[3]] <- c(1,2)
      #group1 <- obj1_pheno[,colnames(obj1_pheno)==input$groupby]
      #group2 <- obj2_pheno[,colnames(obj2_pheno)==input$groupby]
      
      # generate the 3D interactive plot using plotly
      saved_plots_and_tables$d3_int_plot <- plotMultiSurface_int_scale(matrixList=cur_list,
                                                                       groupList=l_pheno,
                                                                       axisList=l_axis)
      
      # generate the download button
      output$download_3d_int_plot_button <- renderUI({
        div(
          align = "right",
          style = "margin-right:15px; margin-top: 10px; margin-bottom:10px",
          downloadButton("download_3d_int_plot", "Download Plot"))
      })
      
      # generate the output text
      output$d3_int_event <- renderPrint({
        d <- event_data("plotly_hover")
        if (is.null(d)) "Hover on a point!" else d
      })
      
      saved_plots_and_tables$d3_int_plot
    })
    
    # output the interactive 3D plot
    output$d3_int_plot <- renderPlotly({
      p <- plot_3d_int_button()
      p$elementId <- NULL
      p
    })
    
    # download 3D interactive plot pdf
    output$download_3d_int_plot <- downloadHandler(
      filename = function() {"threeD_interactive_plot.png"},
      content = function(file) {
        #if (requireNamespace("RSelenium")) {
        rD <- RSelenium::rsDriver(port = 4569L, browser = "chrome")
        tmpFile <- tempfile(fileext = ".png")
        export(saved_plots_and_tables$d3_int_plot, tmpFile, rD)
        browseURL(tmpFile)
        #}
        #tmpFile <- tempfile(fileext = ".png")
        #export(saved_plots_and_tables$d3_int_plot, file = tmpFile)
        #browseURL(tmpFile)
        # Need a personal account to do this:
        #Sys.setenv("plotly_username" = "Greally_Lab")
        #Sys.setenv("plotly_api_key" = "GreallyLab1")
        # plotly_IMAGE(saved_plots_and_tables$d3_int_plot, format = "png",
        #             out_file = file)
      })
    
    # TODO: fix brush click
    output$click_info = renderPrint({
      nearPoints(obj1_pheno,input$plot1_click,addDist = TRUE)
    })
    
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
    }
  }
  
  # initilize shiny
  shinyApp(ui = p_layout, server = server_fun, options = "launch.browser")
  }

slinky_scale_live2(list_sample[1:2])
slinky_scale_live3(list_sample[1:5])


### cell proportion
### fix the aesthetics ''/
webshot::install_phantomjs()
??ExpressionSet