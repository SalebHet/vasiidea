#' result UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_result_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tabsetPanel(type = "tabs", id=ns("tabPanel"),
                tabPanel("MetaData",DT::dataTableOutput(ns("metadata"))),
                tabPanel("Graph",plotOutput(ns("result"))),
                tabPanel("result",DT::dataTableOutput(ns("contents")))
    )
  )
}
    
#' result Server Functions
#'
#' @noRd 
mod_result_server <- function(id,parent){
  moduleServer( id, function(input, output, session){
    #ns <- session$ns
    
    
    result <- array()
    ns <- session$ns
    # Your application server logic 
    nsims <- 2 #100
    res <- numeric(nsims)
    n <- 1000 #number of genes
    nr=5 #number of measurements per subject (grouped data)
    ni=50 #number of subjects
    r <- nr*ni #number of measurements
    t <- matrix(rep(1:nr), ni, ncol=1, nrow=r) # the variable to be tested
    sigma <- 0.5
    b0 <- 1
    #under the null:
    b1 <- 0
    
    
    
    observeEvent(parent$createMeta,{
      #cat("click")
      metaData <<- matrix(1, ncol=1, nrow=r)
      #cat("metaData: ")
      #cat(str(metaData))
      #result <<- c(result,metaData)
      output$metadata <- DT::renderDataTable(metaData)
    })
    
    observeEvent(parent$createExpression,{
      y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
      expressionData <<- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
                             matrix(rep(y.tilde, n), ncol=n, nrow=r))
      #result <<- c(result,expressionData)
      #cat("expressionData: \n")
      #cat(str(expressionData))
      #cat("End mod_config: ")
      #cat(result)
      #return(result) 
    })
    
    observeEvent(parent$compute,{
      cat("Click compute !")
      cat("expressionData: ")
      cat(str(expressionData))
      cat("metaData: ")
      cat(str(metaData))
      res_genes <- dear_seq(exprmat=expressionData, covariates=metaData, variables2test=t,
                            sample_group=rep(1:ni, each=nr),
                            which_test='asymptotic',
                            which_weights='none', preprocessed=TRUE)
      #proportion of raw p-values>0.05
      cat("res: ")
      cat(str(res_genes))
      mean(res_genes$pvals[, 'rawPval']>0.05)
      #quantiles of raw p-values
      quantile(res_genes$pvals[, 'rawPval'])
      #proportion of raw p-values<0.05 i.e. proportion of DE genes
      res <- mean(res_genes$pvals[, 'rawPval']<0.05)
      output$contents <- DT::renderDataTable(res_genes$pvals)
      output$result <- renderPlot(plot(res_genes$pvals)) 
      #browser()
    })
 
  })
}
    
## To be copied in the UI
# mod_result_ui("result_ui_1")
    
## To be copied in the server
# mod_result_server("result_ui_1")
