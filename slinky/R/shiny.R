install.packages("shinythemes")
library(shinythemes)

slinky_live <- function(obj1, obj2=NULL, settings = sleuth_live_settings(),
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
  
  p_layout <- navbarPage(
    a('slinky', href = 'https://github.com/GreallyLab/BAARMY', target = '_blank',
      style = 'color: black;'),
    windowTitle = 'slinky',
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
           selectInput('gv_var_color_by',
           label = 'color by: ',
           choices = c(NULL, poss_covars),
           selected = NULL))
  
  
  
  theme = shinytheme("flatly")
               
 }
}
