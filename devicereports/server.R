require(shiny)
require(lubridate)
if (!require('openfda') ) {
  devtools::install_github("ropenhealth/openfda")
  library(openfda)
}

source('sourcedir.R')


simpleCap <- function(x) {
  s <- tolower(x)
  s <- strsplit(s, " ")[[1]]
  first <- paste(toupper(substring(s[1], 1, 1)), substring(s[1], 2),
                 sep = "", collapse = " ")
  out <-  paste( s[-1], sep = "", collapse = " ")
  out <- paste(first, out)
}



listtocvect <- function(s, delim=', ', trunc=25){
if (is.null( s) ) { 
  return('')
  }
  out <- paste0( s, sep='', collapse=delim)  
  out <- strtrim(out, trunc)
  return(out)
}

listtodf <- function(lis, delim=', ', trunc=100){
  
  out <- data.frame(rownames=1:length(lis[[1]]), stringsAsFactors =FALSE )
  for (i in seq_along(lis) )
  {
    if (is.list(lis[[i]]))
      {
      tmp <- sapply(lis[[i]], function(x) listtocvect(x, delim, trunc) )
      out[[i]] <-   tmp
    } else {
      out[[i]] <-   ''
    }
  }
 # print(lis[[i]])
  out <- data.frame(out, stringsAsFactors =FALSE)
  names(out) <- names(lis)
  return(out)
}

listtostring <- function(s, delim=';')
  {
  myevents <- gsub('\"', '', s, fixed=TRUE) 
  myevents <- gsub('c(', '', myevents, fixed=TRUE)
  myevents <- gsub(')', '', myevents, fixed=TRUE)
  myevents <- gsub(',', delim, myevents, fixed=TRUE)
  return(myevents)
  }

getdf <- function(mydf, name, message='Empty Table')
  {
#  print(name)
#  print(head(mydf) )
  err <- data.frame( Note=message )
   if ( is.data.frame(mydf)  ) {
      if (name %in% names(mydf) ) {
            tmp <- mydf[, name]
            if ( is.data.frame(tmp)  ) {
              return(tmp) 
              }
            else {
              return(err)
              }
      }
    }
    return( err ) 
    
  }

#**************************************
shinyServer(function(input, output, session) {
 
getskip <- reactive({
  return( input$skip-1 )
})
ntext <- eventReactive( input$nextrow, {
  myskip <- getskip()
  mydf <- getfullquery()
  numrecs <- mydf$df.meta$results$total
  maxlim <- getopenfdamaxrecords( numrecs )
  updateSliderInput( session, 'skip', value= min(myskip+2, maxlim), min=1, step= 1, max=maxlim)
})
gett1 <- function(){
  anychanged()
  s <- toupper( input$t1 )
  return( s )
}
gett2 <- function(){
  s <- toupper( input$t2 )
  return( s )
}
gett3 <- function(){
  s <- toupper( input$t3 )
  return( s )
}
getv1 <- function(){
  s <- ( input$v1 )
  return( s )
}
getv2 <- function(){
  s <- ( input$v2)
  return( s )
}
getv3 <- function(){
  s <- ( input$v3 )
  return( s )
}

updatevars <- reactive({
  input$update
  isolate( {
#     updateTextInput(session, "v1", value=( input$v1_2 ) )
#     updateTextInput(session, "t1", value= ( input$t1_2 ) )
#     updateTextInput(session, "v2", value=( input$v2_2 ) )
#     updateTextInput(session, "t2", value= ( input$t2_2 ) )
#     updateTextInput(session, "v3", value=( input$v3_2 ) )
#     updateTextInput(session, "t3", value= ( input$t3_2 ) )
    updateviewerinputs(session)
  })
})



anychanged <- reactive({
  a <- input$t1
  b <- input$v1
  c <- input$t2
  d <- input$v2
  c <- input$t3
  d <- input$v3
  
  closeAlert(session, 'erroralert')
})

output$mymodal <- renderText({
  if (input$update > 0)
  {
    updatevars()    
    toggleModal(session, 'modalExample1', 'close')
  }
  return('')
})


output$ntext <- renderText( {
  ntext()
  return('')
})

ptext <- eventReactive( input$prevrow, {
  myskip <- getskip()
  mydf <- getfullquery()
  numrecs <- mydf$df.meta$results$total
  maxlim <- getopenfdamaxrecords( numrecs )
  updateSliderInput( session, 'skip', value= max(myskip, 1), min=1, step= 1, max=maxlim)
})

output$ptext <- renderText( {
  ptext()
  return('')
})

getreportid <- reactive({
  mydf <- getquery()
  tmp <- mydf$df.results
  id <- tmp$report_number
  if (is.null(id)){
    id = 'Missing Report Number'
  }
  return(id)
})

getfullquery <- reactive({
  if ( input$t1==''  & input$t2 == '' & input$t3 == '' ){
    v1 = '_exists_'
    t1 = 'report_number'
    v2 <- ''
    t2 <- ''
    v3 <- ''
    t3 <- ''
  } else {
    v1 <- c(input$v1, input$v2, input$v3)
    t1 <- c(gett1(), gett2(), gett3() ) 
  }
  myurl <- buildURL(v1, t1, limit=1, db='/device/')
  mydf <- fda_fetch_p(session, myurl)
  out <- c(df=mydf, url=myurl)
  return(out)
})    
getquery <- reactive({
  if (  input$t1 == '' & input$t2 == '' & input$t3 == ''){
    v1 = '_exists_'
    t1 = 'report_number'
    v2 <- ''
    t2 <- ''
    v3 <- ''
    t3 <- ''
  } else {
    v1 <- c(input$v1, input$v2, input$v3)
    t1 <- c(gett1(), gett2(), gett3() ) 
  }
  myurl <- buildURL(v1, t1,  limit=1, skip=getskip(), db='/device/')
  mydf <- fda_fetch_p(session, myurl)
  #   print('url')
  out <- c(df=mydf, url=myurl )
  return(out)
})    
output$v1 <- renderText({
  s <- getv1()
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<br><b>Variable:<i>', s, '</i></b>' )
  return(out)
})

output$v2 <- renderText({
  s <- getv2()
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<b>Variable:<i>', s, '</i></b>' )
  return(out)
})

output$v3 <- renderText({
  s <- getv3()
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<b>Variable:<i>', s, '</i></b>' )
  return(out)
})
output$t1 <- renderText({
  s <- gett1()
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<br><b>Term:<i>', s, '</i></b>' )
  return(out)
})
output$t2 <- renderText({
  s <- gett2()
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<br><b>Term:<i>', s, '</i></b>' )
  return(out)
})
output$t3 <- renderText({
  s <- gett3()
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<br><b>Term:<i>', s, '</i></b>' )
  return(out)
})

#EVENT**********************
output$eventtabletitle <- renderText({  
  s  <- paste('<h4>Report Number=', getreportid(), '<br><br>Event</h4>' )
      return( s )
  })
  
output$eventtable <- renderTable({  
 # if (input$t1=='') {return(data.frame(Drug='Please enter drug name', Count=0))}
    mydf <- getquery()$df.results
    myvars <- geteventvars()
    return(extractcols(mydf, myvars))
  })


#Device********************************

output$deviceindextabletitle <- renderText({  
  s  <- paste('<h4>Device Index Variables</h4>' )
  s  <- paste('<h4>Report Number=', getreportid(), '<br><br>Device Index Variables</h4>' )
  return( s )
})

output$deviceindex <- renderTable({  
  mydf <- getquery()
  mydf <- mydf$df.results$device[[1]]
#  browser()
  myvars <- gsub( 'device.','',  getdeviceindexvars(), fixed=TRUE)
  
  return(extractcols(mydf, myvars))
})


output$deviceidentification <- renderTable({  
  mydf <- getquery()
  mydf <- mydf$df.results$device
  #  browser()
  myvars <- gsub( 'device.','',  getdeviceidentificationvars(), fixed=TRUE)
  return(extractcols(mydf[[1]], myvars))
})


output$deviceidentificationtabletitle <- renderText({  
  s  <- paste('<h4>Device Identification Variables</h4>' )
  return( s )
})

output$devicemodtitle <- renderText({  
  s  <- paste('<h4>Device Model Variables</h4>' )
  return( s )
})


output$devicemodel <- renderTable({  
  mydf <- getquery()
  mydf <- mydf$df.results$device
 #   browser()
  myvars <- gsub( 'device.','',  c( getdevicemodelvars(), getdevicagevars()), fixed=TRUE)
  return(extractcols(mydf[[1]], myvars))
})

#REPORT TEXT********************************************
output$mdrtabletitle <- renderText({  
  s  <- paste('<h4>Report Number =', getreportid(), ' <br><br>Report Text</h4>' )
  return( s )
})

output$mdr <- renderTable({  
  
  mydf <- getquery()
  mydf <- mydf$df.results$mdr_text[[1]]
  tmp <- mydf
  types <- (sapply(tmp, class))
  typesval <- types[types =='character']
  mydf <- tmp[ , names(typesval) ]
#  mydf <- mydf[, which(mydf[,names(typesval)]!='' ) ]
  return(mydf)
})

#OpenFDA****************************
output$openfdatabletitle <- renderText({  
  s  <- paste('<h4>Report Number=', getreportid(), '<br><br>OpenFDA variable</h4>') 
  return( s )
})

output$openfda <- renderTable({ 
  mydf <- getquery()
  mydf <- mydf$df.results$device[[1]]$openfda
  tmp <- mydf
  types <- (sapply(tmp, class))
  typesval <- types[types =='character']
  mydf <- tmp[ , names(typesval) ]
  #  mydf <- mydf[, which(mydf[,names(typesval)]!='' ) ]
  return(mydf)
})

#SOURCE**************************
output$sourcetabletitle <- renderText({  
  s  <- paste('<h4>Report Number =', getreportid(), '<br><br>Source</h4>' )
  return( s )
})

output$source <- renderTable({  
  mydf <- getquery()
  mydf <- (mydf$df.results[, getsourcevars() ])    
  types <- (sapply(mydf, class))
  typesval <- types[types!='data.frame' & types!='list']
  #     typesdf <- types[types=='data.frame']
#      print(typesval)
  #     print('dfs')
  #print(typesdf)
  mydf <- mydf[ , names(typesval) ]
  if ( is.data.frame( mydf ) ){
    return(mydf) 
  } else {
  return( data.frame(Note='No patient data'))
}

})

#PATIENT*********************************************
output$patienttabletitle <- renderText({  
  s  <- paste('<h4>Report ID=', getreportid(), '<br><br>Patient</h4>' )
  return( s )
})

output$patient <- renderTable({  
  mydf <- getquery()
  mydf <- (mydf$df.results$patient)
  tmp <- mydf[[1]]
  types <- (sapply(tmp, class))
  typesval <- types[types!='data.frame' & types!='list']
  #    typesdf <- types[types=='data.frame']
 #      print(types)
  #     print('dfs')
  mydf <- tmp[ , names(typesval) ]
#  print(head(tmp$openfda))
  return(mydf) 
})

#user_fi**************************
output$user_fitabletitle <- renderText({  
  s  <- paste('<h4>Report Number =', getreportid(), '<br><br>User Facility/Importer</h4>' )
  return( s )
})

output$user_fi <- renderTable({  
  mydf <- getquery()
  mydf <- mydf$df.results 
 # browser()
  myvars <- getuser_fivars()
  return(extractcols(mydf, myvars))
})
#user_dm**************************
output$user_dmtabletitle <- renderText({  
  s  <- paste('<h4>Report Number =', getreportid(), '<br><br>Device Manufacturere</h4>' )
  return( s )
})

output$user_dm <- renderTable({  
  mydf <- getquery()
  mydf <- mydf$df.results 
  # browser()
  myvars <- getuser_dmvars()
  return(extractcols(mydf, myvars))
})
#suspect**************************
output$suspecttabletitle <- renderText({  
  s  <- paste('<h4>Report Number =', getreportid(), '<br><br>Suspect device manufacturer</h4>' )
  return( s )
})

output$suspect <- renderTable({  
  mydf <- getquery()
  mydf <- mydf$df.results 
  # browser()
  myvars <- getsuspectvars()
  return(extractcols(mydf, myvars))
})

#keys**************************
output$keystabletitle <- renderText({  
  s  <- paste('<h4>Report Number =', getreportid(), '<br><br>Keys and flags</h4>' )
  return( s )
})

output$keys <- renderTable({  
  mydf <- getquery()
  mydf <- mydf$df.results 
  # browser()
  myvars <- getkeyvars()
  return(extractcols(mydf, myvars))
  
})
#Other**************************
output$othertabletitle <- renderText({  
  s  <- paste('<h4>Report Number =', getreportid(), '<br><br>Patient</h4>' )
  return( s )
})

output$other <- renderTable({  
  mydf <- getquery()
  mydf <- (mydf$df.results[, getothervars() ])    
  types <- (sapply(mydf, class))
  typesval <- types[types!='data.frame' & types!='list']
  #     typesdf <- types[types=='data.frame']
  #      print(typesval)
  #     print('dfs')
  #print(typesdf)
  mydf <- mydf[ , names(typesval) ]
    mydf <- mydf[, which(mydf[,names(typesval)]!='' ) ]
  if ( is.data.frame( mydf ) ){
    return(mydf) 
  } else {
    return( data.frame(Note='No patient data'))
  }
  
})


#META**************************
output$querytitle <- renderText({ 
  return( paste('<h4>Report Number =', getreportid(), '<br><br>Meta Data and Query </h4>' ))
})
output$metatext <- renderText({ 
   mydf <- getfullquery()
   mydf2 <- getquery()
#    "meta": {
#      "disclaimer": "openFDA is a beta research project and not for clinical use. While we make every effort to ensure that data is accurate, you should assume all results are unvalidated.",
#      "license": "http://open.fda.gov/license",
#      "last_updated": "2014-08-01",
#      "results": {
#        "skip": 0,
#        "limit": 1,
#        "total": 1355
#print(mydf)
#print(link)
out <- paste(
  'Disclaimer = ', mydf$df.meta$disclaimer, 
  '<br>License = ', mydf$df.meta$license, 
  '<br>Last Update=', mydf$df.meta$last_updated, 
  '<br>Total=', mydf$df.meta$results$total, 
  '<br> Limit=', mydf$df.meta$results$limit, 
  '<br> Skip=', mydf$df.meta$results$skip
  )
#print( ('output$querytext') )
 return(out)
  })


output$json <- renderText({ 
  myurl <- getquery()$url
  out <- getjson( myurl )
  return( out )
})

output$querytext <- renderText({ 
  mydf2 <- getquery()
  out <- paste( '<br> URL =', removekey( makelink(mydf2$url) ),
                '<BR><BR><b>JSON Output = </b><BR>' 
  )
  return(out)
})


output$reportid <- renderUI({
  p( paste('Report Number=', getreportid() ) )
})

output$currec <- renderUI({ 
  mydf <- getfullquery()
  numrecs <- mydf$df.meta$results$total
  maxlim <- getopenfdamaxrecords( numrecs )
  updateSliderInput( session, 'skip', value=getskip()+1, min=1, step= 1, max=maxlim)
  out <- paste( 'Viewing #', getskip()+1, 'of', numrecs, 'selected reports')
  return(out)
})

getcururl <- reactive({
  mypath <- extractbaseurl( session$clientData$url_pathname )
  s <- paste0( session$clientData$url_protocol, "//", session$clientData$url_hostname,
               ':',
               session$clientData$url_port,
               mypath )
  
  return(s)
})

output$applinks <- renderText({ 
  return( makeapplinks(  getcururl() )  )
})

output$date1 <- renderText({ 
  l <- getdaterangedeviceAE()
  paste( '<b>', l[3] , 'from', as.Date(l[1],  "%Y%m%d")  ,'to', as.Date(l[2],  "%Y%m%d"), '</b>')
})

geturlquery <- observe({
   q <- parseQueryString(session$clientData$url_search)
    t1 <- gsub('"[', '[', q$t1, fixed=TRUE)
    t1 <- gsub(']"', ']', t1, fixed=TRUE)
    t1 <- gsub('""', '"', t1, fixed=TRUE)
    updateTextInput(session, "t1", value = t1)
    updateTextInput(session, "t1_2", value = t1)
  
  t2 <- gsub('"[', '[', q$t2, fixed=TRUE)
  t2 <- gsub(']"', ']', t2, fixed=TRUE)
  t2 <- gsub('""', '"', t2, fixed=TRUE)
  updateTextInput(session, "t2", value = t2)
  updateTextInput(session, "t2_2", value = t2)
  
  if(!is.null(q$t3) )
  {  
  t3 <- gsub('"[', '[', q$t3, fixed=TRUE)
  t3 <- gsub(']"', ']', t3, fixed=TRUE)
  t3 <- gsub('""', '"', t3, fixed=TRUE)
  updateTextInput(session, "t3", value = t3)
  updateTextInput(session, "t3_2", value = t3)
  }
  

if(!is.null(q$v1) )
  {
  v1 <- gsub('"', '', q$v1, fixed=TRUE)
  updateSelectizeInput(session, inputId = "v1", selected = v1)
  updateSelectizeInput(session, inputId = "v1_2", selected = v1)
} 
if(!is.null(q$v2) )
  {
  v2 <- gsub('"', '', q$v2, fixed=TRUE)
  updateSelectizeInput(session, inputId = "v2", selected = v2)
  updateSelectizeInput(session, inputId = "v2_2", selected = v2)
  }
if(!is.null(q$v3) )
  { 
  v3 <- gsub('"', '', q$v3, fixed=TRUE)
  updateSelectizeInput(session, inputId = "v3", selected = v3)
  updateSelectizeInput(session, inputId = "v3_2", selected = v3)
  }
#   
#   updateNumericInput(session, "skip", value = q$skip)
#   return(q)
})

output$help <- renderUI({
  #  print('test')
  s <- input$sidetabs
 # print(s)
  out <- switch(s, 
                'Graph Options'=loadhelp('graphoptions'),
                'Data Options'=loadhelp('dataoptions'),
                'Axis Options'=loadhelp('axisoptions'),
                'Select Vars'= loadhelp('selectvars'),
                'Load Data'= loadhelp('loaddata'),
                'Overview'= loadhelp('overview'),
                'Overviewside'= loadhelp('overviewside'),
                'none')
  return( HTML(out[[1]]) )
})
})