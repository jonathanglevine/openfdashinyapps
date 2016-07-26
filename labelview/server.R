require(shiny)
require(lubridate)
if (!require('openfda') ) {
  devtools::install_github("ropenhealth/openfda")
  library(openfda)
}

source('sourcedir.R')
 



#**************************************
shinyServer(function(input, output, session) {
  
  gett1v1 <- reactive({ 
    return( c(t1=input$t1, v1=input$v2) )
  })
  
  gett1 <- function(){
    anychanged()
    s <- input$t1
    if (getv1() != '_exists_')
    {
      s <- toupper( s )
    }
    return( s )
  }
  gett2 <- function(){
    s <- input$t2
    if (getv2() != '_exists_')
    {
      s <- toupper( s )
    }
    return( s )
  }
  gett3 <- function(){
    s <- input$t3
    if (getv3() != '_exists_')
    {
      s <- toupper( s )
    }
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

updatevars <- reactive({
  input$update
  isolate( {
    updateviewerinputs(session)
  })
})


output$mymodal <- renderText({
  if (input$update > 0)
  {
    updatevars()    
    toggleModal(session, 'modalExample1', 'close')
  }
  return('')
})

# updatevars2 <- reactive({
#   input$update2
#   isolate( {
#     updateTextInput(session, "v2", value=( input$v2_2 ) )
#     updateTextInput(session, "t2", value= ( input$t2_2 ) )
#   })
# })
# 
# 
# output$mymodal2 <- renderText({
#   if (input$update2 > 0)
#   {
#     updatevars2()    
#     toggleModal(session, 'modalExample2', 'close')
#   }
#   return('')
# })
# 
# updatevars3 <- reactive({
#   input$update3
#   isolate( {
#     updateTextInput(session, "v3", value=( input$v3_2 ) )
#     updateTextInput(session, "t3", value= ( input$t3_2 ) )
#   })
# })

# 
# output$mymodal3 <- renderText({
#   if (input$update3 > 0)
#   {
#     updatevars3()    
#     toggleModal(session, 'modalExample3', 'close')
#   }
#   return('')
# })

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
getquery <- reactive({
  
#  print('query')
  if (  gett1() == '' & gett2() == '' & gett3() == ''){
    v1 = '_exists_'
    t1 = 'spl_id'
    v2 <- ''
    t2 <- ''
    v3 <- ''
    t3 <- ''
  } else {
    v1 <- c(getv1(), getv2(), getv3())
    t1 <- c(gett1(), gett2(),  gett3()) 
    }
  myurl <- buildURL(v1, t1,  limit=1, skip=getskip(), type='label')

  mydf <- fda_fetch_p(session, myurl)
#   print('url')
#   print( (myurl) )
#   print(mydf$meta)
  out <- c(df=mydf, url=myurl )

  return(out)
})    

getquerydownload <- function(skip=0, limit=10 ){
  
  #  print('query')
  if (  gett1() == '' & gett2() == '' & gett3() == ''){
    v1 = '_exists_'
    t1 = 'spl_id'
    v2 <- ''
    t2 <- ''
    v3 <- ''
    t3 <- ''
  } else {
    v1 <- c(getv1(), getv2(), getv3())
    t1 <- c(gett1(), gett2(),  gett3()) 
  }
  myurl <- buildURL(v1, t1,  limit=limit, skip=skip, type='label')
  
  mydf <- fda_fetch_p(session, myurl)
  out <- c(df=mydf, url=myurl )
  
  return(out)
}  


getdownload <- function(tabname, skip=1, limit=100)
{  
  section <- varprefixes[[ which(tabnames== tabname ) ]]
  special <-  c(
    "Overview",
    "ID and version", 
    "Other",
    "OpenFDA")
  mydf <- getquerydownload(skip, limit)
  if( !(tabname %in% special) )
    {
    tmp <- mydf$df.results
    realcols <- names(tmp)
    knowncols <- getallvars( allvars(),  'text', section=section)
    mycols <- intersect (knowncols, realcols)
    outdf <- extractdfcols(tmp, mycols, numrows=nrow(tmp) )
    } else if( (tabname == 'OpenFDA') )
    {
      tmp <- mydf$df.results$openfda
      realcols <- names(tmp)
      knowncols <- getallvars( allvars(),  'text', section=varprefixes['openfda'])
      mycols <- intersect (knowncols, realcols)
      outdf <- extractdfcols(tmp, mycols, numrows=nrow(tmp) )
    } else if( tabname == "ID and version" )
    {
    mycols <- c("set_id",
                "id",
                "version",
                "effective_time")
    tmp <- mydf$df.results
    outdf <- tmp[mycols]
  } else if( tabname == "Other" )
    {  
    tmp <- mydf$df.results
    realcols <- gettextnames( names(tmp) )
    realcols <- realcols[ which( realcols != 'openfda') ]
    knowncols <- getallvars( allvars(),  'text')
    mycols <- realcols
    unknowncols <- setdiff(realcols , knowncols)
    xxcols <- getallvars( allvars(),  'text', section='xx')
    xxcols <- intersect (realcols, xxcols)
    mycols <- union(unknowncols, xxcols)
    outdf <- extractdfcols(tmp, mycols, numrows=nrow(tmp) )
  } else if( tabname == "Overview" )
  {  
#     mycols <- c('unii',
#                 'spl_id',
#                 'product_ndc',
#                 'substance_name',
#                 'spl_set_id',
#                 'product_type',
#                 'pharm_class_cs',
#                 'manufacturer_name',
#                 'brand_name',
#                 'pharm_class_pe',
#                 'route',
#                 'generic_name',
#                 'application_number') 
#     
#     outdf2 <- getselectedcols(tmp, 'text', c('of'))
#     tmp <- mydf$df.results
#     outdf <- getselectedcols(tmp, 'text', c('sp', '11'))
#     
#     realcols <- names(tmp)
#     knowncols <- getallvars( allvars(),  'text', section=section)
#     mycols <- intersect (knowncols, realcols)
    
    tmp <- mydf$df.results$openfda 
    realcols <- names(tmp)
    mycols <- getallvars( allvars(),  'text', section=c('of'))
    mycols <- intersect (mycols, realcols)
    outdf2 <- extractdfcols(tmp, mycols, numrows=nrow(tmp) )
    tmp <- mydf$df.results
    realcols <- names(tmp)
    mycols <- getallvars( allvars(),  'text', section=c('sp', '11') )
    mycols <- intersect (mycols, realcols)
    outdf <- extractdfcols(tmp, mycols, numrows=nrow(tmp) )
    outdf <- data.frame(outdf, outdf2)
  }
  return( outdf ) 
}

getsection <- function( section )
  {  
#  browser()
  mydf <- getquery()
  tmp <- mydf$df.results
  realcols <- names(tmp)
  knowncols <- getallvars( allvars(),  'text', section= section)
  mycols <- intersect (knowncols, realcols)
  outdf <- extractdfcols(tmp, mycols)
  return( outdf )
}

getsectiontab <- function( section )
{ 
  #   mydf <- getquery()
  #   tmp <- mydf$df.results
  #   realcols <- names(tmp)
  #   knowncols <- getallvars( allvars(),  'table', section=c( '01', '02','03' ) )
  # mycols <- intersect (knowncols, realcols)
  #   outdf <- extractdfcols(tmp, mycols)
  #   return(  gettables(outdf) ) 
  mydf <- getquery()
  tmp <- mydf$df.results
  realcols <- names(tmp)
  knowncols <- getallvars( allvars(),  'table', section= section)
  mycols <- intersect (knowncols, realcols)
  outdf <- extractdfcols(tmp, mycols)
  return(  gettables(outdf) )
}
getreportid <- reactive({
  mydf <- getquery()
  tmp <- mydf$df.results
  id <- tmp$set_id
  if (is.null(id)){
    id = 'Missing Set ID'
  }
  out <- paste('<h5>Set ID=', id, 
        '<br> Brand Name:', getbrandname(),
        '<br> Generic Name:', getgenericname(),
        '<br></h5>')
        
  return( out )
})

getgenericname <- reactive({
  mydf <- getquery()
  tmp <- mydf$df.results$openfda
  id <- tmp$generic_name
  if (is.null(id)){
    id = 'Missing Generic Name'
  }
 s <- paste(id)
  s <- strsplit(s, ', ')
  names <- c('v1', 't1')
  values <- c('patient.drug.openfda.generic_name' ) 
#  s <- coltohyper( s[[1]], 'DA' , mybaseurl = getcururl() )
  s <- numcoltohyper(s[[1]], s[[1]], names, values, type='DA', mybaseurl = getcururl(), addquotes=TRUE )
  s <- paste(s, sep='; ', collapse='; ')
  return(s)
})

getbrandname <- reactive({
  mydf <- getquery()
  tmp <- mydf$df.results$openfda
  id <- tmp$brand_nam
  if (is.null(id)){
    id = 'Missing Brand Name'
  }
  s <- paste(id)
  s <- strsplit(s, ', ')
  names <- c('v1', 't1')
  values <- c('patient.drug.openfda.brand_name' ) 
#  s <- coltohyper( s[[1]], 'DA' , mybaseurl = getcururl() )
  s <- numcoltohyper(s[[1]], s[[1]], names, values, type='DA', mybaseurl = getcururl(), addquotes=TRUE )
  s <- paste(s, sep='; ', collapse='; ')
  return(s)
})

getfullquery <- reactive({

  if ( gett1()==''  & gett2() == '' & gett3() == '' ){
    v1 = '_exists_'
    t1 = 'spl_id'
    v2 <- ''
    t2 <- ''
    v3 <- ''
    t3 <- ''
  } else {
    v1 <- c(getv1(), getv2(), getv3())
    t1 <- c(gett1(), gett2(),  gett3()) 
  }
  myurl <- buildURL(v1, t1, limit=1, type='label')
  mydf <- fda_fetch_p(session, myurl)
  out <- c(df=mydf, url=myurl)
  
  return(out)
})    

output$v1 <- renderText({
  s <- getv1()
  if(s == '') {
    s <- 'None'
  }
  out <- renderterm2(s, 'Variable:' )
  return(out)
})

output$v2 <- renderText({
  s <- getv2()
  if(s == '') {
    s <- 'None'
  }
  out <- renderterm2(s, 'Variable:' )
  return(out)
})

output$v3 <- renderText({
  s <- getv3()
  if(s == '') {
    s <- 'None'
  }
  out <- renderterm2(s, 'Variable:')
  return(out)
})
output$t1 <- renderText({
  s <- gett1()
  if(s == '') {
    s <- 'None'
  }
  out <- renderterm2( s, 'Term:' )
  return(out)
})
output$t2 <- renderText({
  s <- gett2()
  if(s == '') {
    s <- 'None'
  }
  out <- renderterm2(s,  'Term:' )
  return(out)
})
output$t3 <- renderText({
  s <- gett3()
  if(s == '') {
    s <- 'None'
  }
  out <- renderterm2( s, 'Term:')
  return(out)
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

#Overview**********************
output$overviewtitle <- renderText({  
  s  <- paste( getreportid(), '<br>Header' )
  return( s )
})

output$overviewtable <- renderTable({ 
  mycols <- c('unii',
                              'spl_id',
                              'product_ndc',
               'substance_name',
               #               'rxcui',
                              'spl_set_id',
               'product_type',
               'pharm_class_cs',
                'manufacturer_name',
               'brand_name',
               'pharm_class_pe',
               'route',
               #'nui',
               #'pharm_class_moa',
               #               'package_ndc',
               #'pharm_class_epc',
               'generic_name',
               'application_number') 
  mydf <- getquery()
  tmp <- mydf$df.results$openfda
  outdf2 <- getselectedcols(tmp, 'text', c('of'))
  tmp <- mydf$df.results
  outdf <- getselectedcols(tmp, 'text', c('sp', '11'))
  outdf <- data.frame(outdf, outdf2)
  return(outdf)
})


# 1. Indications, usage, and dosage********************************

output$indtitle <- renderText({  
  s  <- paste( getreportid(), '<br>Indications, usage, and dosage' )
  return( s )
})

output$ind <- renderTable({ 
  outdf <- getsection( section=varprefixes[['one']] )
  return(outdf)
})

output$indtabs <- renderText({ 
  outdf <- getsectiontab( section=varprefixes[['one']] )
  return(outdf)
})

#HEADER**********************
output$headertabletitle <- renderText({  
  s  <- paste(getreportid(), '<br>ID and Version' )
      return( s )
  })
  
output$headertable <- renderTable({  
 # if (gett1=='') {return(data.frame(Drug='Please enter drug name', Count=0))}
    mydf <- getquery()
    mycols <- c("set_id",
    "id",
    "version",
    "effective_time")
    tmp <- mydf$df.results
    return( tmp[mycols] )
  })

# 5. Warnings and precautions****************************
output$warntabletitle <- renderText({  
  s  <- paste(getreportid(), '<br>Warnings and precautions' )
  return( s )
})

output$warn <- renderTable({  
  outdf <- getsection( section=varprefixes[['four']] )
  return(outdf)
})


output$warntabs <- renderText({ 
  outdf <- getsectiontab( section=varprefixes[['four']] )
  return(outdf)
})

# 6. 7.  Adverse effects and interactions*********************************************

output$aeinttabletitle <- renderText({  
  s  <- paste(getreportid(), '<br>Adverse effects and interactions' )
  return( s )
})

output$aeint <- renderTable({  
  outdf <- getsection( section=varprefixes[['six']] )
  return(outdf)
})

output$aetabs <- renderText({ 
  outdf <- getsectiontab( section=varprefixes[['six']] )
  return(outdf)
})

# 8. special****************************

output$specialtabletitle <- renderText({  
  s  <- paste(getreportid(), '<br>Special populations') 
  return( s )
})

output$special <- renderTable({  
  outdf <- getsection( section=varprefixes[['eight']] )
  return(outdf)
})


output$specialtabs <- renderText({  
  outdf <- getsectiontab( section=varprefixes[['eight']] )
  return(outdf)
})

# 9. 10. ABUSEOD**************************

output$abuseodtabletitle <- renderText({  
  s  <- paste(getreportid(), '<br>Abuse and overdosage' )
  return( s )
})

output$abuseod <- renderTable({  
  outdf <- getsection( section=varprefixes[['nine']] )
  return(outdf)
})


output$abuseodtabs <- renderText({  
  outdf <- getsectiontab( section=varprefixes[['nine']] )
  return(outdf)
})


# 12. Clinical pharmacology****************************
output$clinpharmtabletitle <- renderText({  
  s  <- paste( getreportid(), '<br>Clinical pharmacology' )
  return( s )
})

output$clinpharm <- renderTable({  
  outdf <- getsection( section=varprefixes[['twelve']] )
  return(outdf)
})

output$clinpharmtabs <- renderText({ 
  outdf <- getsectiontab( section=varprefixes[['twelve']] )
  return(outdf)
})



# 13.  Nonclinical toxicology****************************

output$nonclintoxtabletitle <- renderText({  
  s  <- paste(getreportid(),'<br>Nonclinical toxicology' )
  return( s )
})


output$nonclintox <- renderTable({  
#  Nonclinical toxicology
 outdf <- getsection( section=varprefixes[['thirteen']] )
 return(outdf)
})


output$nonclintoxtabs <- renderText({ 
  outdf <- getsectiontab( section=varprefixes[['thirteen']] )
  return(outdf)
})

# 14 References****************************

output$referencetabletitle <- renderText({  
  s  <- paste(getreportid(),'<br>References' )
  return( s )
})


output$reference <- renderTable({  
  #  References
  outdf <- getsection( section=varprefixes[['fourteen']] )
  return(outdf)
})

referencedownload <- function(skip=1, limit=1)
  {  
  #  References
  
  mydf <- getquerydownload()
  tmp <- mydf$df.results
  realcols <- names(tmp)
  knowncols <- getallvars( allvars(),  'text', section='14')
  knowncols <- c(knowncols,  getallvars( allvars(),  'text', section='15') )
  mycols <- intersect (knowncols, realcols)
  #  browser()
  outdf <- extractdfcols(tmp, mycols, numrows=nrow(tmp) )
  return(outdf)
}

output$referencetabs <- renderText({ 
  outdf <- getsectiontab( section=varprefixes[['fourteen']] )
  return(outdf)
})


# 16 Supply, storage, and handling****************************

output$supplytabletitle <- renderText({  
  s  <- paste(getreportid(), '<br>References' )
  return( s )
})

output$supply <- renderTable({  
  outdf <- getsection( section=varprefixes[['sixteen']] )
  return(outdf)
})


output$supplytabs <- renderText({ 
  outdf <- getsectiontab( section=varprefixes[['sixteen']] )
  return(outdf)
})


# 17. Patient information****************************

output$patinfotabletitle <- renderText({  
  s  <- paste(getreportid(),'<br> Patient information') 
  return( s )
})

output$patinfo <- renderTable({  
  outdf <- getsection( section=varprefixes[['patient']] )
  return(outdf)
})

output$patinfotabs <- renderText({ 
  outdf <- getsectiontab( section=varprefixes[['patient']] )
  return(outdf)
})


#OPENFDA ********************************************
output$patientdrugopenfdatabletitle <- renderText({  
  s  <- paste(getreportid(), ' <br>OpenFDA' )
  return( s )
})

output$openfda <- renderTable({  
  mydf <- getquery()
  tmp <- mydf$df.results$openfda
  outdf <- getselectedcols(tmp, 'text', varprefixes['openfda'] )
  return( outdf ) 
})

# Other fields*********************************************
output$othertabletitle <- renderText({  
  s  <- paste(getreportid(), '<br>Other Fields' )
  return( s )
})

output$other <- renderTable({  
  mydf <- getquery()
  tmp <- mydf$df.results
  realcols <- gettextnames( names(tmp) )
  realcols <- realcols[ which( realcols != 'openfda') ]
  knowncols <- getallvars( allvars(),  'text')
  mycols <- realcols
  unknowncols <- setdiff(realcols , knowncols)
  xxcols <- getallvars( allvars(),  'text', section='xx')
  xxcols <- intersect (realcols, xxcols)
  mycols <- union(unknowncols, xxcols)
  outdf <- extractdfcols(tmp, mycols)
  return(outdf)
})


output$othertabs <- renderText({ 
  mydf <- getquery()
  tmp <- mydf$df.results
  realcols <- gettablenames( names(tmp) )
  knowncols <- getallvars( allvars(),  'table')
  mycols <- setdiff(realcols , knowncols)
  print(mycols)
  outdf <- extractdfcols(tmp, realcols)
  s <- gettables(outdf)
  return( s )
})

#META**************************
output$querytitle <- renderText({ 
  return( paste(getreportid(), '<br>Meta Data and Query' ))
})
output$metatext <- renderText({ 
   mydf <- getfullquery()
   mydf2 <- getquery()
link <- paste0('<a href="', ( mydf$url ), '">', removekey( mydf$url ), '</a>')
#print(link)
out <- paste(
  'Disclaimer = ', mydf$df.meta$disclaimer, 
  '<br>License = ', mydf$df.meta$license, 
  '<br>Last Update=', mydf$df.meta$last_updated, 
  '<br>Total=', mydf$df.meta$results$total, 
  '<br> Limit=', mydf$df.meta$results$limit, 
  '<br> Skip=', mydf$df.meta$results$skip, 
  '<br> Error=', mydf$df.meta$error, 
      '<br> URL =', removekey( makelink(mydf2$url) ), 
  '<BR><BR><b>JSON Output = </b><BR>'
  )
#print( ('output$querytext') )
 return(out)
  })

output$json <- renderText({ 
  myurl <- getquery()$url
  out <- getjson( myurl )
  return( out )
})


output$reportid <- renderUI({
  p( paste(getreportid() ) )
})

output$currec <- renderUI({ 
  mydf <- getfullquery()
  numrecs <- mydf$df.meta$results$total
  maxlim <- getopenfdamaxrecords( numrecs )
  updateSliderInput( session, 'skip', value=getskip()+1, min=1, step= 1, max=maxlim)
  out <- paste( 'Viewing #', getskip()+1, 'of', numrecs, 'Selected Labels')
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
  l <- getdaterangelabel()
  paste( '<b>', l[3] ,'from', as.Date(l[1],  "%Y%m%d")  ,'to', as.Date(l[2],  "%Y%m%d"), '</b>')
})

geturlquery <- observe({
   q <- parseQueryString(session$clientData$url_search)
   updateTabsetPanel(session, 'maintabs', selected=q$curtab)
   
   if(!is.null(q$t1) )
    { 
    t1 <- gsub('"[', '[', q$t1, fixed=TRUE)
    t1 <- gsub(']"', ']', t1, fixed=TRUE)
    t1 <- gsub('""', '"', t1, fixed=TRUE)
    updateTextInput(session, "t1", value = t1)
    updateTextInput(session, "t1_2", value = t1)
    }
   
   if(!is.null(q$t2) )
    { 
    t2 <- gsub('"[', '[', q$t2, fixed=TRUE)
    t2 <- gsub(']"', ']', t2, fixed=TRUE)
    t2 <- gsub('""', '"', t2, fixed=TRUE)
    updateTextInput(session, "t2", value = t2)
    updateTextInput(session, "t2_2", value = t2)
    }
  
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
  v1 <- gsub('patient.drug.', '', v1, fixed=TRUE)
  updateSelectizeInput(session, inputId = "v1", selected = v1)
  updateSelectizeInput(session, inputId = "v1_2", selected = v1)
} 
if(!is.null(q$v2) )
  {
  v2 <- gsub('"', '', q$v2, fixed=TRUE)
  v2 <- gsub('patient.drug.', '', v2, fixed=TRUE)
  updateSelectizeInput(session, inputId = "v2", selected = v2)
  updateSelectizeInput(session, inputId = "v2_2", selected = v2)
  }
if(!is.null(q$v3) )
  { 
  v3 <- gsub('"', '', q$v3, fixed=TRUE)
  v3 <- gsub('patient.drug.', '', v3, fixed=TRUE)
  updateSelectizeInput(session, inputId = "v3", selected = v3)
  updateSelectizeInput(session, inputId = "v3_2", selected = v3)
  }
#   
#   updateNumericInput(session, "skip", value = q$skip)
#   return(q)
})

output$downloadData <- downloadHandler(
  filename = function() {
    paste(input$download, Sys.time(), '.csv', sep='')
  },
  content = function(con) {
    write.csv( getdownload( input$download, skip = input$downloadstart-1 ), con)
  },
  contentType="text/csv"
)

})