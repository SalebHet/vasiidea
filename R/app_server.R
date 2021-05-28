#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
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
  
  
  metaData <- NULL
  expressionData <- NULL
  observeEvent(input$createMeta,{
    cat("click")
    metaData <<- matrix(1, ncol=1, nrow=r)
    cat("metaData: ")
    cat(str(metaData))
    output$metadata <- DT::renderDataTable(metaData)
  })
  
  observeEvent(input$createExpression,{
    y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
    expressionData <<- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
             matrix(rep(y.tilde, n), ncol=n, nrow=r))
    cat("expressionData: \n")
    cat(str(expressionData))
  })
  
  observeEvent(input$compute,{
    res_genes <- dear_seq(exprmat=expressionData, covariates=metaData, variables2test=t,
                          sample_group=rep(1:ni, each=nr),
                          which_test='asymptotic',
                          which_weights='none', preprocessed=TRUE)
    #proportion of raw p-values>0.05
    mean(res_genes$pvals[, 'rawPval']>0.05)
    #quantiles of raw p-values
    quantile(res_genes$pvals[, 'rawPval'])
    #proportion of raw p-values<0.05 i.e. proportion of DE genes
    res <- mean(res_genes$pvals[, 'rawPval']<0.05)
    output$contents <- DT::renderDataTable(res_genes$pvals)
    output$result <- renderPlot(plot(res_genes$pvals)) 
    #browser()
  })
}
