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


mod_result_server <- function(id,parent,parentSession){
  options(shiny.maxRequestSize = 500*1024^2) 
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
    metaData <- NULL
    expressionData <- NULL
    
    observeEvent(parent$UploadMeta,{
      inFile <- parent$design
      metaData <<- read.csv(inFile$datapath)
      #cat(str(metaData))
      lstpossi <- colnames(metaData)
      #cat('colnames(metaData): \n')
      #cat(lstpossi)
      #browser()
      #cat("parent$compare: ")
      #cat(str(parent$ns),"\n")
      updateSelectizeInput(parentSession, "compare",#session, parent$compare,
                           selected = '',
                           choices = c('',lstpossi),
                           options = list(placeholder = 'Please select a variable below')
      )
      updateSelectizeInput(parentSession, "filter",#session, parent$filer,#'filter',
                           selected = '',
                           choices = c('',lstpossi),
                           options = list(placeholder = 'Please select a variable below')
      )
      updateSelectizeInput(parentSession,"id",#session, parent$id,#'id',
                           selected = '',
                           choices = c('',lstpossi),
                           options = list(placeholder = 'Please select a variable below')
      )
      output$metadata <- DT::renderDataTable(metaData)
    })

    observeEvent(parent$UploadMatrice,{
      inFile <- parent$expr
      #expressionData <<- read.csv(inFile$datapath)
      expressionFile <- read.csv(inFile$datapath)
      library(janitor)
      #browser()
      #labkey.data <- labkey.data[,-which(names(labkey.data) %in% c("Specimen ID","Participant ID","Visit ID","Date","Target Study"))]#subset(labkey.data,-c("Specimen ID","Participant ID","Visit ID","Date"))
      #browser()
      expressionFile <- t(as.matrix(expressionFile))
      expressionFile <- as.data.frame(expressionFile)
      expressionFile <- janitor::row_to_names(expressionFile,1)
      #browser()
      expressionData <<- expressionFile
      cat(str(expressionData))
      cat("ExprData Uploaded")
      #output$metadata <- DT::renderDataTable(metaData)
    })
    
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
    
     library('Rlabkey')
     observe({
       #browser()
       query <- parseQueryString(session$clientData$url_search)
       if (!is.null(query[['key']])) {
    
         #updateSliderInput(session, "bins", value = query[['bins']])
         key <<- query[['key']]
         set <<- paste0("apikey|",key)
    
         Rlabkey::labkey.setDefaults(apiKey=set)#"apikey|73ea3ff0973f38d52f5b1bbd8980f62c")
         Rlabkey::labkey.setDefaults(baseUrl = "https://labk.bph.u-bordeaux.fr/")#(baseUrl="https://labkey.bph.u-bordeaux.fr:8443/")
         labkey.data <- labkey.selectRows(
           baseUrl="https://labk.bph.u-bordeaux.fr",
           #folderPath="/EBOVAC/assays/EBL2001/ICS",
           folderPath="/COVERAGE-Immuno/RNAseq/4-Counts-Matrices/",
           #schemaName="assay.General.MetaData-RNAseq",
           schemaName = "assay.General.MetaData_VASI_DM",
           queryName="data",
           viewName="",
           colSort="",
           #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
           containerFilter=NULL
         )
    
         #cat("Result request metadata => ")
         #cat(str(labkey.data),"\n")
         labkey.data <- clean_names(labkey.data)
         metaData <<- labkey.data
         #browser()
         lstpossi <- colnames(metaData)
         #cat('colnames(metaData): \n')
         #cat(lstpossi)
         #browser()
         #cat("parent$compare: ")
         #cat(str(parent$ns),"\n")
         updateSelectizeInput(parentSession, "compare",#session, parent$compare,
                              selected = '',
                              choices = c('',lstpossi),
                              options = list(placeholder = 'Please select a variable below')
         )
         updateSelectizeInput(parentSession, "filter",#session, parent$filer,#'filter',
                              selected = '',
                              choices = c('',lstpossi),
                              options = list(placeholder = 'Please select a variable below')
         )
         updateSelectizeInput(parentSession,"id",#session, parent$id,#'id',
                              selected = '',
                              choices = c('',lstpossi),
                              options = list(placeholder = 'Please select a variable below')
         )
         updateSelectizeInput(parentSession,"sample",#session, parent$id,#'id',
                              selected = '',
                              choices = c('',lstpossi),
                              options = list(placeholder = 'Please select a variable below')
         )
         
         output$metadata <- DT::renderDataTable(metaData)
       }
     })

     observe({
       query <- parseQueryString(session$clientData$url_search)
       if (!is.null(query[['key']])) {

    #     #updateSliderInput(session, "bins", value = query[['bins']])
         #browser()
         key <<- query[['key']]
         set <<- paste0("apikey|",key)

         Rlabkey::labkey.setDefaults(apiKey=set)#"apikey|73ea3ff0973f38d52f5b1bbd8980f62c")
         Rlabkey::labkey.setDefaults(baseUrl = "https://labk.bph.u-bordeaux.fr/")#(baseUrl="https://labkey.bph.u-bordeaux.fr:8443/")
         labkey.data <- labkey.selectRows(
           baseUrl="https://labk.bph.u-bordeaux.fr",
           #folderPath="/EBOVAC/assays/EBL2001/ICS",
           folderPath="/COVERAGE-Immuno/RNAseq/4-Counts-Matrices/",
           schemaName="assay.General.Count_Matrix_Reverse",
           queryName="data",
           viewName="",
           colSort="",
           #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
           containerFilter=NULL
         )
         #cat("Result request expression => ")
         #cat(str(labkey.data),"\n")
         library(janitor)
         #browser()
         labkey.data <- labkey.data[,-which(names(labkey.data) %in% c("Specimen ID","Participant ID","Visit ID","Date","Target Study"))]#subset(labkey.data,-c("Specimen ID","Participant ID","Visit ID","Date"))
         labkey.data <- t(labkey.data)
         labkey.data <- as.data.frame(labkey.data)
         labkey.data <- row_to_names(labkey.data,1)
         #browser()
         #cat("Result request expression => ")
         #cat(str(labkey.data),"\n")
         #browser()
         expressionData <<- labkey.data
         #browser()
         #cat("Matrix: ")
         #cat(str(expressionData))
         #cat(str(metaData))
         #output$metadata <- DT::renderDataTable(expressionData)
       }
     })

    # observeEvent(parent$compare,{
    #   available_var <- metaData[,parent$compare]
    #   updateSelectizeInput(parentSession,"varCompare",
    #                        selected = '',
    #                        choices = c('',levels(as.factor(available_var))),
    #                        options = list(placeholder = 'Please select a variable below'))
    # })
    #browser()
    observeEvent(parent$filter,{
      #browser()
      if(parent$filter != ""){
        available_var <- metaData[,parent$filter]
        #cat(str(available_var))
        updateSelectizeInput(parentSession,"filterVars",
                             selected = '',
                             choices = c('',levels(as.factor(available_var))),
                             options = list(placeholder = 'Please select a variable below'))
      }
    })
    #browser()
    observeEvent(parent$compute,{
      library(dplyr)
      cat("\n Click compute ! \n")
      #cat("expressionData: ")
      #cat(str(expressionData))
      #browser()
      raw_counts_all <- expressionData
      #raw_counts_all <- as.matrix(expressionData)
      metaDataCom <- as.matrix(metaData)

      # metaData$Série.Extraction <- factor(metaData$Série.Extraction, levels = c(1,2,3,4,5,6,7,8,9,10,11))
      # metaData_analysis <- metaData %>% filter(Prick.test...Tempus == "Prick test")
      # design_prick <- as.matrix(model.matrix(~Série.Extraction,data = metaData_analysis[,"Série.Extraction",drop = FALSE]))
      # browser()
      # count_prick <- t(as.matrix(raw_counts_all[metaData_analysis$Sample.name.sample.sheet,]))
      # cat("metaData: ")
      # cat(str(metaData))
      # res_genes <- dear_seq(exprmat = count_prick,variables2test =  design_prick[,2,drop = FALSE],
      #                       covariates = design_prick[,1,drop = FALSE],
      #                       sample_group =  metaData_analysis$Sample.name,which_test = "asymptotic", which_weights = 'none')

      # res_genes <- dear_seq(exprmat=expressionData, covariates=metaData, variables2test=t,
      #                       sample_group=rep(1:ni, each=nr),
      #                       which_test='asymptotic',
      #                       which_weights='none', preprocessed=TRUE)
      #proportion of raw p-values>0.05

      rownames(raw_counts_all)<- raw_counts_all[,1]
      rownames(metaDataCom)<-metaDataCom[,1]
      raw_counts_all <- as.data.frame(raw_counts_all[,-1])

      #browser()
      metaDataCom <- metaDataCom[,-1]
      metaDataCom <- as.data.frame(metaDataCom)
      #browser()
      metaDataCom[,parent$compare] <- factor(metaDataCom[,parent$compare], unique(metaDataCom[,parent$compare]))#levels = c(1,2,3,4,5,6,7,8,9,10,11))
      #metaDataCom$Série.Extraction <- factor(metaDataCom[,parent$compare], unique(metaDataCom[,parent$compare]))#levels = c(1,2,3,4,5,6,7,8,9,10,11))
      #filtre <- parent$filterVars
      #browser()
      #metaData_analysis <- metaDataCom %>% filter(metaDataCom[,parent$filter] == parent$filterVars)
      #metaData_analysis <- metaDataCom %>% filter(parent$filter == parent$filterVars)#metaData_analysis <- metaDataCom %>% filter(Prick.test...Tempus == "Prick test")
      metaData_analysis <- metaDataCom %>% filter(.data[[parent$filter]]== parent$filterVars)#metaData_analysis <- metaDataCom %>% filter(Prick.test...Tempus == "Prick test")
      # design_prick <- as.matrix(model.matrix(~Série.Extraction,data = metaData_analysis[,"Série.Extraction",drop = FALSE]))
      # count_prick <- t(as.matrix(raw_counts_all[metaData_analysis$Sample.name.sample.sheet,]))
      count_prick <- t(as.matrix(raw_counts_all[metaData_analysis[,parent$id],]))
      count_prick <- matrix(as.numeric(count_prick),
                            ncol = ncol(count_prick))
      #j <- 1
      # while (j <= ncol(count_prick)){
      #   #browser()
      #   count_prick[,j] = as.numeric(count_prick[,j])
      #   j = j+1
      # }
      #browser()
      i<-1
      #browser()
      while(i <= nrow(metaData_analysis)){
        if(is.na(metaData_analysis[i,parent$compare])){#if(is.na(metaData_analysis[i,"Série.Extraction"])){
          #browser()
          nom <- metaData_analysis[i,parent$id]#nom <- metaData_analysis[i,"Sample.name.sample.sheet"]
          cat("nom: ")
          cat(nom)
          cat("\n")
          count_prick <- select(as.data.frame(count_prick),-c(nom))
          metaData_analysis <- metaData_analysis[-c(i),]
          cat("nrow(metaData_analysis): ")
          cat(nrow(metaData_analysis))
          cat("\n")

        }else{
          i = i+1
          # cat(i)
          # cat("\n")
        }
      }
      #cat("Start Compute \n")
      #browser()
      formule <- as.formula(paste0("~",parent$compare))
      design_prick <- as.matrix(model.matrix(formule,data = metaData_analysis[,parent$compare,drop = FALSE]))
      
      #design_prick <- as.matrix(model.matrix(~Série.Extraction,data = metaData_analysis[,parent$compare,drop = FALSE]))
      #design_prick <- as.matrix(model.matrix(~Série.Extraction,data = metaData_analysis[,"Série.Extraction",drop = FALSE]))
      #browser()
      res_genes <- dearseq::dear_seq(exprmat = as.matrix(count_prick),variables2test =  design_prick[,2,drop = FALSE],
                      covariates = design_prick[,1,drop = FALSE],sample_group =  metaData_analysis[,parent$sample],which_test = "asymptotic")



      cat("res: ")
      cat(str(res_genes))
      mean(res_genes$pvals[, 'rawPval']>0.05)
      #quantiles of raw p-values
      quantile(res_genes$pvals[, 'rawPval'])
      #proportion of raw p-values<0.05 i.e. proportion of DE genes
      res <- mean(res_genes$pvals[, 'rawPval']<0.05)
      output$contents <- DT::renderDataTable(res_genes$pvals)
      #output$result <- renderPlot(plot(res_genes$pvals))
      output$result <- renderPlot(plot(res_genes))
      #browser()
    })
 
  })
}
    
## To be copied in the UI
# mod_result_ui("result_ui_1")
    
## To be copied in the server
# mod_result_server("result_ui_1")
