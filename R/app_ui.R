#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      h1("VASIDEA"),
      
      # Horizontal line ----
      tags$hr(),
      
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          mod_config_ui("config_ui_1"),
          
        #   # Input: Select a file ----
        #   fileInput("expr", "Load expression data",
        #             multiple = FALSE,
        #             accept = c("text/csv",
        #                        "text/comma-separated-values,text/plain",
        #                        ".csv",
        #                        ".txt"
        #                        )),
        #   
        #   actionButton("createExpression",label = "generate ExpressionData"),
        #   
        #   # Input: Select separator ----
        #   radioButtons("sep_expr", "Separator",
        #                choices = c(Comma = ",",
        #                            Semicolon = ";",
        #                            Tab = "\t"),
        #                selected = ","),
        #   
        # #),
        # 
        # #sidebarPanel(
        # 
        # # Horizontal line ----
        # tags$hr(),
        # 
        #   # Input: Select a file ----
        #   fileInput("design", "Load metadata",
        #             multiple = FALSE,
        #             accept = c("text/csv",
        #                        "text/comma-separated-values,text/plain",
        #                        ".csv",
        #                        ".txt"
        #             )),
        #   
        #   actionButton("createMeta",label = "generate metaData"),
        #   # Input: Select separator ----
        #   radioButtons("sep_design", "Separator",
        #                choices = c(Comma = ",",
        #                            Semicolon = ";",
        #                            Tab = "\t"),
        #                selected = ","),
        #   actionButton("compute",label = "Compute")
          
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          mod_result_ui("result_ui_1")
          #DT::dataTableOutput("metadata")
          # Output: Data file ----
           # tabsetPanel(type = "tabs", id="tabPanel",
           #   tabPanel("MetaData",DT::dataTableOutput("metadata")),
           #   tabPanel("Graph",plotOutput("result")),
           #   tabPanel("result",DT::dataTableOutput("contents"))
           # )

        )
        
      )
      
      
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'VASIDEA'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

