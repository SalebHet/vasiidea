#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
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
  # metaData <- NULL
  # expressionData <- NULL
  metaData <- NULL
  expressionData <- NULL
  # observeEvent(input$createMeta,{
  #   cat("click")
  #   metaData <<- matrix(1, ncol=1, nrow=r)
  #   cat("metaData: ")
  #   cat(str(metaData))
  #   output$metadata <- DT::renderDataTable(metaData)
  # })
  # 
  # observeEvent(input$createExpression,{
  #   y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
  #   expressionData <<- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
  #            matrix(rep(y.tilde, n), ncol=n, nrow=r))
  #   cat("expressionData: \n")
  #   cat(str(expressionData))
  # })
  config <- mod_config_server("config_ui_1")
  #cat("config: ")
  #cat(str(config))
  #cat("Res mod_config_server: ")
  #str(res)
  #metaData <- res[1]
  #expressionData <- res[2]
  mod_result_server(id = "result_ui_1",parent =  config) #metaData,expressionData,
}
