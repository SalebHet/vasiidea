#' config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets
mod_config_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    
    # Input: Select a file ----
    # fileInput(ns("expr"), "Load expression data",
    #           multiple = FALSE,
    #           accept = c("text/csv",
    #                      "text/comma-separated-values,text/plain",
    #                      ".csv",
    #                      ".txt"
    #           )),
    # actionButton(ns("UploadMatrice"), label = "Upload"),
    # actionButton(ns("createExpression"),label = "generate ExpressionData"),
    # 
    # # Input: Select separator ----
    # radioButtons(ns("sep_expr"), "Separator",
    #              choices = c(Comma = ",",
    #                          Semicolon = ";",
    #                          Tab = "\t"),
    #              selected = ","),
    
    #),
    
    #sidebarPanel(
    
    # Horizontal line ----
    # tags$hr(),
    # 
    # # Input: Select a file ----
    # fileInput(ns("design"), "Load metadata",
    #           multiple = FALSE,
    #           accept = c("text/csv",
    #                      "text/comma-separated-values,text/plain",
    #                      ".csv",
    #                      ".txt"
    #           )),
    # actionButton(ns("UploadMeta"), label = "Upload"),
    # actionButton(ns("createMeta"),label = "generate metaData"),
    # # Input: Select separator ----
    # radioButtons(ns("sep_design"), "Separator",
    #              choices = c(Comma = ",",
    #                          Semicolon = ";",
    #                          Tab = "\t"),
    #              selected = ","),
    shinyWidgets::materialSwitch(ns("preprocessed"), label = "Preprocessed Data", value = TRUE,status = "primary"),
    selectizeInput(ns("test"),label = "Column Var Test",
                   choices =c(Choose = "", NULL),
                   options = list(placeholder = 'Please select a variable below')),
    #selectizeInput(ns("varCompare"),label = "Var Compare",choices =c(Choose = "", NULL),multiple=TRUE,
    #               options = list(placeholder = 'Please select a variable below')),
    selectizeInput(ns("covariable"),label = "covariable columns",multiple = TRUE,choices =c(Choose = "", NULL),
                   options = list(placeholder = 'Please select a variable below')),
    # selectizeInput(ns("filterVars"),label = "Filter Vars",choices =c(Choose = "", NULL),multiple=TRUE,
    #                options = list(placeholder = 'Please select a variable below')),
    # selectizeInput(ns("id"),label = "id",choices =c(Choose = "", NULL),
    #                options = list(placeholder = 'Please select a variable below')),
    # selectizeInput(ns("sample"),label = "sample",choices =c(Choose = "", NULL),
    #                options = list(placeholder = 'Please select a variable below')),
    actionButton(ns("compute"),label = "Compute")
    
  )
}


    
#' config Server Functions
#'
#' @noRd 
mod_config_server <- function(id,metaData,expressionData){
  
  moduleServer( id, function(input, output, session){
    # result <- array()
    # ns <- session$ns
    # # Your application server logic 
    # nsims <- 2 #100
    # res <- numeric(nsims)
    # n <- 1000 #number of genes
    # nr=5 #number of measurements per subject (grouped data)
    # ni=50 #number of subjects
    # r <- nr*ni #number of measurements
    # t <- matrix(rep(1:nr), ni, ncol=1, nrow=r) # the variable to be tested
    # sigma <- 0.5
    # b0 <- 1
    # #under the null:
    # b1 <- 0
    # 
    # 
    # 
    # observeEvent(input$createMeta,{
    #   cat("click")
    #   metaData <<- matrix(1, ncol=1, nrow=r)
    #   cat("metaData: ")
    #   cat(str(metaData))
    #   result <<- c(result,metaData)
    #   output$metadata <- DT::renderDataTable(metaData)
    # })
    # 
    # observeEvent(input$createExpression,{
    #   y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
    #   expressionData <<- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
    #                          matrix(rep(y.tilde, n), ncol=n, nrow=r))
    #   result <<- c(result,expressionData)
    #   cat("expressionData: \n")
    #   cat(str(expressionData))
    #   cat("End mod_config: ")
    #   cat(result)
    #   return(result) 
    # })
    result <- NULL
    result$session <- session
    result$input <- input
    return(result)
  })

}
    
## To be copied in the UI
# mod_config_ui("config_ui_1")
    
## To be copied in the server
# mod_config_server("config_ui_1")
