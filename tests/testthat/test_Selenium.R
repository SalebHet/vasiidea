library(RSelenium)
library(testthat)

waitFor <- function(how,id){
  #webElem <- NULL
  
  i <- 0
  while(i<=1000){
    webElem <<- tryCatch({remDr$findElement(using = how, value = id)},
                         error = function(e){NULL})
    #loop until element with name <value> is found in <webpage url>
    
    i <- i+1
    if(!is.null(webElem)){
      break()
    }
  }
  if(!is.null(webElem)){
    return(webElem)
  }
  else{
    stop(paste0(id," not found \n"))
  }
}

wd <- getwd()

wd <- getwd()

#Uncomment this for local test

# rD <- RSelenium::rsDriver(
#   browser = "firefox",
#   extraCapabilities = list(
#     "moz:firefoxOptions" = list(
#       args = list()#('--headless')
#     )
#   )
# )

remDr <-  remoteDriver(browserName = "firefox",port=4455L)#Use this for local test =>  rD$client 
remDr$open(silent = FALSE)
remDr$setTimeout(type = "page load", milliseconds = 5000)
appURL <- "http://127.0.0.1:8080"
#app %<-% vici::run_app()


test_that("can connect to app", {
  #skip_on_cran()
  x <- processx::process$new( 
    "R", 
    c(
      "-e", 
      "VASIDEA::run_app()"
    )
  )
  Sys.sleep(5)
  remDr$navigate(appURL)
  #sys.wa
  webElem <- waitFor("xpath","/html/body/div/h1")#remDr$findElement(using = "xpath", value = "/html/body/div[2]/h2")
  textWebElem <- webElem$getElementText()
  expect_equal(as.character(textWebElem), "VASIDEA")
  x$kill()
})