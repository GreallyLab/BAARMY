# slinky with scalability

slinky_scale_live(list_sample)

list_sample <- list()
list_sample[[1]] <- obj1
list_sample[[2]] <- obj2
list_sample[[3]] <- obj3

list_esets <- list_sample

source("R/plotMultiSurface_test.R")

slinky_scale_live <- function(list_esets, cell_prop=NULL,
                        options = list(port = 42427), ...) {
  if(!(is(list_esets, 'list'))){
    stop("Please supply a list of expression sets for list_eset before running this command")
  }
  if(length(list_esets)<2){
    stop(paste("You have supplied", length(list_esets), " expression sets. Please supply
               at least two expression sets in list before running this command", sep=""))
  }
  # TODO: write stops if list is not composed of esets

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
  axis_choice <- colnames(cur_list[[i]])

  # Things needed for plot function:
  #list of PC matrices, group to color by, list of vectors PCs to plot (obj1_x, obj1_y, obj2_x, obj2_y)

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
                        multi-dimensional data. The menu tabs
                        can be used to choose plots to view'),
                      p(strong('Brought to you by:')),
                      h2("Team BAARMY")
                      )
               )),
    navbarMenu('Visualize',
               tabPanel('3D plot',
                        fluidRow(
                          column(6,offset = 3,
                                 selectInput('groupby', label = 'color by:', choices = eset_var_final, selected = eset_var_final[1])
                          )
                        ),
                        lapply(1:length(list_esets), function(k) {
                          fluidRow(
                            column(3, offset = 3,
                              selectInput(paste0("obj",k,"x"),
                                          paste0("object ",k," x-axis:"),
                                      choices = axis_choice,
                                      selected = NULL)
                              ),
                              column(3, offset = 3,
                                     selectInput(paste0("obj",k,"y"),
                                                 paste0("object ",k," y-axis:"),
                                                 choices = axis_choice,
                                                 selected = NULL)
                              )
                          )
                        }),
                        fluidRow(
                          column(10, offset= .5,
                                 actionButton('plot_3d_go', "Plot")
                          )
                        ),
                        fluidRow(
                          plotOutput('d3_plot')
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
      input[[paste("obj",k,"x", sep="_")]]

      # generate the pheno type list
      l_pheno <- list()
      for (k in 1:length(list_esets)){
        pheno <- pheno_list[[k]]
        l_pheno[[k]] <- pheno[,colnames(pheno)==input$groupby]
      }

      l_axis <-list()
      v_axis<-rep(0,2)
      for (k in 1:length(list_esets)){
        v_axis[1] <- input[[paste0("obj",k,"x")]]
        v_axis[2] <- input[[paste0("obj",k,"y")]]
        l_axis[[k]] <- v_axis
      }
      group1 <- obj1_pheno[,colnames(obj1_pheno)==input$groupby]
      group2 <- obj2_pheno[,colnames(obj2_pheno)==input$groupby]

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
    }
  }

  # initilize shiny
  shinyApp(ui = p_layout, server = server_fun, options = "launch.browser")
}

slinky_scale_live(list_sample)
