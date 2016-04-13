require(shiny)
require(shinyBS)
require('lubridate')
require('bcp')
require('changepoint')
require('zoo')
if (!require('openfda') ) {
  devtools::install_github("ropenhealth/openfda")
  library(openfda)
#  print('loaded open FDA')
}


source( 'sourcedir.R')
 

#**************************************************
#CPA
#**************************************************
shinyServer(function(input, output, session) {
 
#Getters
  getwaittime <- reactive({ 
    if(session$clientData$url_hostname == '10.12.207.87')
    {
      return( 0.75)
    } else if(session$clientData$url_hostname == '127.0.0.1') {
      return (0.25)
    }
    return(0.0)
  })
  
  getqueryvars <- function( num = 1 ) {
    s <- vector(mode = "character", length = 7)
    #Dashboard
    s[1] <- paste0( input$t1, '&v1=', input$v1 )
    
    #PRR for a Drug
    s[2] <- paste0( input$t1, '&v1=', input$v1 )
    
    #PRR for an Event
    s[3] <- paste0( input$t2, '&v1=', input$v1 )
    
    #Dynamic PRR
    s[4] <- paste0( input$t1 , '&v1=', input$v1, '&t2=', input$t2  )
    
    #CPA
    s[5] <- paste0( input$t1 , '&v1=', input$v1,  '&t2=', input$t2  )
    
    #Reportview
    s[6] <- paste0( input$t1, '&v1=', input$v1,  '&t2=', input$t2 , '&v2=', getaevar() )
    
    #labelview
    s[7] <- paste0( input$t1, '&v1=', input$v1 )
    
    #LRTest
    s[8] <- paste0( input$t1, '&v1=', input$v1 )
    return(s)
  }  
  
getdrugvar <- function(){
  
 anychanged()
  return(input$v1)
}

getaevar <- function(){
  return ('patient.reaction.reactionmeddrapt')
}

gettimevar <- function(){
  anychanged()
  return (input$v2)
}

getexactdrugvar <- function(){
  return ( paste0(getdrugvar(), '.exact') )
}

getexactaevar <- function(){
  return ( paste0(getaevar(), '.exact')  )
}

getbestdrugvar <- function(){
  exact <-   ( getexactvals()$exactd)
  if (exact){
    return( getexactdrugvar() )
  } else {
    return( getdrugvar() )
  }
}

getbestaevar <- function(){
  exact <-   ( getexactvals()$exacte)
  if (exact){
    return( getexactaevar() )
  } else {
    return( getaevar() )
  }
}

# getterm1 <- function(quote=TRUE){
#   s <- toupper( input$t1 )
#   if (quote){
#     s <-  gsub('"', '', s , fixed=TRUE)
#     return(paste0('%22', s, '%22'))
#   } else {
#     return( s )
#   }
# }
# 
# getterm2 <- function(quote=TRUE){
#   s <- toupper( input$t2 )
#   if (quote){
#     s <-  gsub('"', '', s , fixed=TRUE)
#     return(paste0('%22', s, '%22'))
#   } else {
#     return( s )
#   }
# }

getbestterm1 <- function(quote=TRUE){
  exact <-   ( getexactvals()$exactd )
  return( getterm1( session,exact))
}

getbestterm2 <- function(quote=TRUE){
  exact <-   ( getexactvals()$exacte)
  return( getterm2( session,exact))
}

gettimerange <- reactive({
  geturlquery()
  mydates <- getstartend()
  start <- mydates[1]
  end <-  mydates[2]
  timerange <- paste0('[', start, '+TO+', end, ']')
  return(timerange)
})
#End Getters

#Timerange string from 1 December 1999 to present
getstartend <- reactive({
  geturlquery()
  start <- ('1906-07-01')
  end <- as.character( Sys.Date() ) 
  return( c(start, end))
})

#Reactive Queries
fixInput <- reactive({
  
  updateTextInput(session, "t1", value= (input$t1) )
  updateTextInput(session,"t2", value=(input$t2))   
})
fetchalldata <- reactive({
  a <- getqueryde()
  a <- getexactvals()
  a <- geturlquery()
  a <- gettotalquery()
  a <- gettotaldaterangequery()
})

getexactvals <- reactive({
  geturlquery()
  exactD <- input$useexactD
  if ( exactD=='exact' )
  { 
    exactd = TRUE
  } else {
    exactd = FALSE
  }
  exactE <- input$useexactE
  if ( exactE=='exact' )
  { 
    exacte = TRUE
  } else {
    exacte = FALSE
  }
  return( list( exacte=exacte, exactd=exactd) )
}) 


gettotalquery <- reactive({
  geturlquery()
  toggleModal(session, 'updatemodal', 'close')
  v1 <- getbestdrugvar()
  t1 <- c(getbestterm1() ) 
  myurl <- buildURL(v1, t1, count='', limit=5 )
  mydf <- fda_fetch_p( session, myurl, wait = getwaittime())
  # print(mydf)
  mydf <- list(result=mydf$result, url=myurl, meta=mydf$meta)
  return(mydf)
})    

gettotaldaterangequery <- reactive({
  geturlquery()
  v1 <- c( getbestdrugvar(), gettimevar() )
  t1 <- c(getbestterm1(), gettimerange() ) 
  myurl <- buildURL(v1, t1, count='', limit=5)
  mydf <- fda_fetch_p( session, myurl, wait = getwaittime(), reps=4)
  mydf <- list(result=mydf$result, url=myurl, meta=mydf$meta)
  return(mydf)
})  

#Other reactives
getdrugeventtotal <- reactive({
  mysum <- getquerydata()$mydfin$total
  return( mysum )
})

getstartend <- function(){
  geturlquery()
  start <- input$daterange[1]
  end <- input$daterange[2]
  return( c(start, end))
}

getqueryde <- reactive({
  geturlquery()
  v <- c( getbestdrugvar(), getbestaevar() , gettimevar() )
  t <- c( getbestterm1(), getbestterm2(), gettimerange() )  
  myurl <- buildURL(v, t, count=gettimevar() )
  out <- fda_fetch_p( session, myurl, wait = getwaittime(), reps=5 )
  return( list(out=out, myurl=myurl ) )
})

getquerydata <- reactive({
  mydf <- getqueryde()
  tmp <- mydf$out$result    
  createAlert(session, 'alert', 'calcalert',
              title='Calculating...', 
              content = 'Calculating Time Series...', 
              dismiss = FALSE)
  mydfin <- gettstable( tmp )
  closeAlert(session,  'calcalert')
  return( list( mydfin= mydfin, mydf=mydf, myurl= mydf$myurl, mysum = mydfin$total ) )
})

getcodruglist <- reactive({
  v <- c(getbestdrugvar(), getbestaevar())
  t <- c( getbestterm1(),  getbestterm2())
  myurl <- buildURL( v, t, 
                     count= getexactdrugvar(), limit=999 )
  mydf <- fda_fetch_p( session, myurl)
  mydf <- mydf$result[1:999,]
  mydf <- mydf[!is.na(mydf[,2]), ]
  mydf <- data.frame(mydf, cumsum= cumsum(mydf[,2]))
  return( list( mydf=mydf, myurl=myurl) )
})

getcoeventlist <- reactive({
  v <- c(getbestdrugvar(), getbestaevar())
  t <- c( getbestterm1(),  getbestterm2())
  myurl <- buildURL( v, t, 
                     count= getexactaevar(), limit=999 )
  mydf <- fda_fetch_p( session, myurl)
  mydf <- mydf$result[1:999,]
  mydf <- mydf[!is.na(mydf[,2]), ]
  mydf <- data.frame(mydf, cumsum= cumsum(mydf[,2]))
  return( list( mydf=mydf, myurl=myurl) )
})

getcocountsE <- reactive({
  
  return( getcocounts('E') )
})

getcocountsD <- reactive({
  
  return( getcocounts('D') )
})
#**************************
# Concomitant drug table
getcocounts <- function(whichcount = 'D'){
  geturlquery()
  if ( is.null( getterm1( session) ) ){
    return(data.frame( c(paste('Please enter a drug and event name'), '') ) )
  }
  if( whichcount=='D')
  {
    mylist <- getcodruglist()
  } else (
    mylist <- getcoeventlist()
  )
  mydf <- mylist$mydf
  myurl <- mylist$myurl
  sourcedf <- mydf
  #    print(names(mydf))
  #Drug Table
  if (whichcount =='D'){
    colname <- 'Drug Name'
    if (input$v1 != 'patient.drug.medicinalproduct')
    {
      drugvar <- gsub( "patient.drug.","" , input$v1, fixed=TRUE)
      drugvar <- paste0( "&v1=", drugvar )
      medlinelinks <- coltohyper( paste0( '%22' , sourcedf[,1], '%22' ), 'L', 
                                  mybaseurl = getcururl(), 
                                  display= rep('L', nrow( sourcedf ) ), 
                                  append= drugvar )
      
      drugvar <- paste0( "&v1=", input$v1 )
      dashlinks <- coltohyper( paste0( '%22' , sourcedf[, 'term' ], '%22' ), 'DA', 
                               mybaseurl = getcururl(), 
                               display= rep('D', nrow( sourcedf ) ), 
                               append= drugvar )
      
      mydf <- data.frame(D=dashlinks, L=medlinelinks, mydf)
      mynames <- c( 'D', 'L', colname, 'Count', 'Cumulative Sum') 
    }
    else {
      medlinelinks <- rep(' ', nrow( sourcedf ) )
      mynames <- c('-', colname, 'Count') 
    }
    names <- c('v1','t1', 'v2', 't2')
    values <- c(getbestaevar(), getbestterm2(), getexactdrugvar() ) 
    #Event Table
  } else {
    colname <- 'Preferred Term'
    mynames <- c('M', colname, 'Count', 'Cumulative Sum') 
    medlinelinks <- makemedlinelink(sourcedf[,1], 'M')          
    mydf <- data.frame(M=medlinelinks, mydf) 
    names <- c('v1','t1', 'v2', 't2')
    values <- c(getbestdrugvar(), getbestterm1(), getexactaevar() ) 
  }
  mydf[,'count'] <- numcoltohyper(mydf[ , 'count' ], mydf[ , 'term'], names, values, mybaseurl = getcururl(), addquotes=TRUE )
  mydf[,'term'] <- coltohyper(mydf[,'term'], whichcount , mybaseurl = getcururl(), 
                              append= paste0( "&v1=", input$v1) )
  names(mydf) <- mynames
  return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
}   

gettstable <- function( tmp ){
  if ( length(tmp)!=0  )
  {

    mydf <- data.frame(count=tmp$count, 
                       date= as.character( floor_date( ymd( (tmp[,1]) ), 'month' ) ), stringsAsFactors = FALSE )
    mydaterange <- getstartend()
     mydf2 <- seq( as.Date(  mydf$date[1] ), as.Date( mydaterange[2] ), 'months' )
    
    mydf2 <-data.frame(date=as.character(mydf2), count=0L)
    mydf2<-rbind(mydf, mydf2)
    mydf2[,'date'] <- sub('-01', '', mydf2[,'date'],fixed=TRUE) 
    mydf <- aggregate(mydf2[,c('count')], by=list(mydf2$date), FUN=sum)
    mysum <- sum(mydf[,2])
    mydf[, 3] <- cumsum(mydf[,2])
    
    start <- paste0( '[',  mydf[,1], '01')
    start <- gsub('-', '', start)
    lastdate <- ymd(start[ length( start)] )
    month(lastdate) <- month(lastdate)+1
    mydates <- c( ymd(start[2:length(start)] ), lastdate  ) 
    mydates <- round_date(mydates, unit='month')
    mydates <- rollback(mydates)
    mydates <- gsub('-', '', as.character( mydates ))
    mycumdates <- paste0(start[1],  '+TO+', mydates ,']')
    mydates <- paste0(start,  '+TO+', mydates ,']')
    names(mydf) <- c('Date', 'Count', 'Cumulative Count')
#    mydf <- mydf[ (mydf[,'Cumulative Count'] > 0), ]
    mydf_d <- mydf
    names <- c('v1','t1', 'v2' ,'t2', 'v3', 't3')
    values <- c( getbestdrugvar(), getbestterm1(), getbestaevar(), getbestterm2(), gettimevar() )
    mydf_d[,2] <- numcoltohyper(mydf[ , 2], mydates, names, values, type='R', mybaseurl = getcururl(), addquotes=TRUE )
    mydf_d[,3] <- numcoltohyper(mydf[ , 3], mycumdates, names, values, type='R', mybaseurl = getcururl(), addquotes=TRUE )

    } else {
    mydf <- NULL
    mydf_d <- NULL
    mysum <- NULL
  }
  mydf <- list(result=mydf, display=mydf_d, total= mysum )
  
  return(mydf)
}    

getts <- reactive({
  data <-  getquerydata()$mydfin$result
  ( mydates <- ymd(data[,'Date'] ) )
  ( mymonths <- month( ymd(data[,'Date'], truncated=2 ) ) )
  ( myyears <- year( ymd(data[,'Date'], truncated=2 ) ) )
  ( startmonth <- mymonths[1])
  ( endmonth <- mymonths[length(mymonths)])
  ( yrange <- range(myyears)) 
  #Check date variable in the dataset to see whether it is in Date format
  #Assign variable you want to run CPA to x
  datax<-data[,2]
  myts <- zoo( datax, data[, 1] )
  return(myts)
})

calccpmean<- reactive({
   myts <- getts()
   datax_changepoint <- cpt.mean(myts, Q=input$maxcp, method='BinSeg')
    return(datax_changepoint)
 }) 

calccpvar<- reactive({
  myts <- getts()
  datax_changepoint <- cpt.var(myts, Q = input$maxcp, method='BinSeg')
  return(datax_changepoint)
}) 
calccpbayes<- reactive({
  myts <- getts()
  mydf <- getquerydata()$mydfin$result[, c(1,2)]
  bcp.flu<-bcp(as.double(myts),p0=0.3)
  return(list(bcp.flu=bcp.flu, data=mydf) )
}) 

updatevars <- reactive({
  input$update
  isolate( {
    updateTextInput(session, "t1", value=( input$drugname ) )
    updateTextInput(session, "t2", value= ( input$eventname ) )
    updateNumericInput(session, "maxcp", value=input$maxcp2)
          })
})

anychanged <- reactive({
  a <- input$t1
  b <- input$v1
  c <- input$t2
  d <- input$v2
  e <- input$useexactD
  f <- input$useexactE
  
  closeAlert(session, 'erroralert')
})
#SETTERS
output$mymodal <- renderText({
  if (input$update > 0)
    {
    updatevars()    
    toggleModal(session, 'modalExample', 'close')
  }
  return('')
})
output$cotitle <- renderText({ 
  return( ( paste0('<h4>Most Common Drugs In Selected Reports</h4><br>') ) )
})

output$cotitleE <- renderText({ 
  return( ( paste0('<h4>Most Common Events In Selected Reports</h4><br>') ) )
})

output$querycotext <- renderText({ 
  l <- getcocountsD()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})

output$querycotextE <- renderText({ 
  l <- getcocountsE()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})

output$coquery <- renderTable({  
  #if ( getterm1() =='') {return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, URL=''))}
  codrugs <- getcocountsD()$mydf
  if ( is.data.frame(codrugs) )
  { 
    return(codrugs) 
  } else  {
    return( data.frame(Term=paste( 'No Events for', getterm1( session) ) ) )
  }  
}, sanitize.text.function = function(x) x)  

output$coqueryE <- renderTable({  
  #if ( getterm1() =='') {return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, URL=''))}
  codrugs <- getcocountsE()$mydf
  if ( is.data.frame(codrugs) )
  { 
    return(codrugs) 
  } else  {
    return( data.frame(Term=paste( 'No Events for', getterm1( session ) ) ) )
  }  
}, sanitize.text.function = function(x) x)

output$cloudcoquery <- renderPlot({  
  mydf <- getcocountsD()$sourcedf
  if ( is.data.frame(mydf) )
  {
    mytitle <- paste('Drug in Reports That Contain', getterm1( session ) )
    return( getcloud(mydf, title=mytitle ) ) 
  } else  {
    return( data.frame(Term=paste( 'No events for', getterm1( session ) ) ) )
  }  
  
}, height=900, width=900 )

output$cloudcoqueryE <- renderPlot({  
  mydf <- getcocountsE()$sourcedf
  if ( is.data.frame(mydf) )
  {
    mytitle <- paste('Events in Reports That Contain', getterm1( session ) )
    return( getcloud(mydf, title=mytitle ) ) 
  } else  {
    return( data.frame(Term=paste( 'No events for', getterm1( session ) ) ) )
  }  
  
}, height=900, width=900 )

output$drugname <- renderText({
  s <- getterm1( session,FALSE)
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<br><b>Drug Name:<i>', s, '</i></b><br><br>' )
  return(out)
})

output$eventname <- renderText({
  s <- getterm2( session,FALSE)
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<b>Event Term:<i>', s, '</i></b><br><br>' )
  return(out)
})

output$maxcp <- renderText({
  s <- input$maxcp
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<b>Maximum Number of Changepoints:<i>', s, '</i></b>' )
  return(out)
})

output$query <- renderTable({  
  fetchalldata()
#   if (input$term1=='') {return(data.frame(Drug='Please enter drug name', Count=0))}
    mydf <- getquerydata()$mydfin
  if ( is.data.frame(mydf$display) )
{
    return(mydf$display) 
  } else  {return(data.frame(Drug=paste( 'No events for drug', input$term1), Count=0))}
  }, include.rownames = FALSE, sanitize.text.function = function(x) x)
output$coquery <- renderTable({  
  #if ( getterm1() =='') {return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, URL=''))}
  codrugs <- getcocountsD()$mydf
  if ( is.data.frame(codrugs) )
  { 
    return(codrugs) 
  } else  {
    return( data.frame(Term=paste( 'No Events for', getterm1( session) ) ) )
  }  
}, sanitize.text.function = function(x) x)  

output$coqueryE <- renderTable({  
  #if ( getterm1() =='') {return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, URL=''))}
  codrugs <- getcocountsE()$mydf
  if ( is.data.frame(codrugs) )
  { 
    return(codrugs) 
  } else  {
    return( data.frame(Term=paste( 'No Events for', getterm1( session ) ) ) )
  }  
}, sanitize.text.function = function(x) x)

output$cloudcoquery <- renderPlot({  
  mydf <- getcocountsD()$sourcedf
  if ( is.data.frame(mydf) )
  {
    mytitle <- paste('Drug in Reports That Contain', getterm1( session ) )
    return( getcloud(mydf, title=mytitle ) ) 
  } else  {
    return( data.frame(Term=paste( 'No events for', getterm1( session ) ) ) )
  }  
  
}, height=900, width=900 )

output$cloudcoqueryE <- renderPlot({  
  mydf <- getcocountsE()$sourcedf
  if ( is.data.frame(mydf) )
  {
    mytitle <- paste('Events in Reports That Contain', getterm1( session ) )
    return( getcloud(mydf, title=mytitle ) ) 
  } else  {
    return( data.frame(Term=paste( 'No events for', getterm1( session ) ) ) )
  }  
  
}, height=900, width=900 )

 output$querytext <- renderText({ 
   fetchalldata()
   mylist <-getquerydata()
   mydf <- mylist$mydfin
   mydf2 <- gettotaldaterangequery()
#    "meta": {
#      "disclaimer": "openFDA is a beta research project and not for clinical use. While we make every effort to ensure that data is accurate, you should assume all results are unvalidated.",
#      "license": "http://open.fda.gov/license",
#      "last_updated": "2014-08-01",
#      "results": {
#        "skip": 0,
#        "limit": 1,
#        "total": 1355
#print(mydf)
meta <- mydf2$meta
out <- paste('<b>Total =</b>', prettyNum( getdrugeventtotal(), big.mark=',' ) , 'for drug-event combination<br>',
             '<b>Query =</b>', removekey( makelink(mylist$myurl) ), '<br><br>' )
 return(out)
  })
output$metatext <- renderText({ 
  fetchalldata()
  mydf <- gettotaldaterangequery()
  meta <- mydf$meta
#  print(meta)
  out <- paste(
    '<br><b>Total =</b>', prettyNum( meta$results$total, big.mark=',' ), 'reports for', getterm1( session,quote=FALSE), 
    'for dates from', gettimerange(),
    '<br><b>Query =</b>', removekey( makelink(mydf$url) ), '<br><br>' )
  return(out)
})
output$allquerytext <- renderText({ 
  fetchalldata()
  mydf <- gettotalquery()
  meta <- mydf$meta
  out <- paste('<h4>Meta Data</h4>',
    '<b>Last Update =</b>', meta$last_updated, 
      '<br><b>Total =</b>', prettyNum( meta$results$total, big.mark=',' ), 'reports for', getterm1( session,quote=FALSE),
    '<br> <b>Query =</b>', removekey(  makelink(mydf$url) ) )
  return(out)
})


output$cpmeantext <- renderText ({
  mydf <-getquerydata()$mydfin$result
  if (length(mydf) > 0)
    {
    createAlert(session, 'alert', 'calclert',
                title='Calculating...', 
                content = 'Calculating meanCP', 
                dismiss = FALSE)

    s <- calccpmean()
    mycpts <- attr( s@data.set, 'index')[s@cpts[1:length(s@cpts)-1] ]
    mycpts <-paste(mycpts, collapse=', ')
    out <- paste( 'Changepoint type      : Change in', s@cpttype, '<br>' )
    out <- paste(out,  'Method of analysis    :' , s@method , '<br>' )
    out <- paste(out, 'Test Statistic  :' , s@test.stat, '<br>' )
    out <- paste(out, 'Type of penalty       :' , s@pen.type, 'with value', round(s@pen.value, 6), '<br>' )
    out <- paste(out, 'Maximum no. of cpts   : ' , s@ncpts.max, '<br>' )
    out <- paste(out, 'Changepoint Locations :' , mycpts , '<br>' )
    closeAlert(session, 'calclert')
    } else {
      out <- "Insufficient data"
    }
return(out)
})

output$cpmeanplot <- renderPlot ({
  
  mydf <-getquerydata()$mydfin$result
  if (length(mydf) > 0)
    {
    s <- calccpmean()
    labs <-    index( getts() )
    pos <- seq(1, length(labs), 3)
    
    if ( getterm1( session, FALSE )==''  )
      {
      mydrugs <- 'All Drugs'
      }
    else 
      {
        mydrugs <- getterm1( session, FALSE )
      }
    if ( getterm2( session, FALSE )=='' )
      {
        myevents <- 'All Events'
      }
    else 
      {
        myevents <- getterm2( session, FALSE )
      }
    mytitle <- paste( "Change in Mean Analysis for", mydrugs, 'and', myevents )
    plot(s, xaxt = 'n', ylab='Count', xlab='', main=mytitle)
    axis(1, pos,  labs[pos], las=2  )
    grid(nx=NA, ny=NULL)
    abline(v=pos, col = "lightgray", lty = "dotted",
           lwd = par("lwd") )
    }
})

output$cpvartext <- renderText ({
  mydf <-getquerydata()$mydfin$result
  if (length(mydf) > 0)
    {
    s <- calccpvar()
    mycpts <- attr( s@data.set, 'index')[s@cpts[1:length(s@cpts)-1] ]
    mycpts <-paste(mycpts, collapse=', ')
    out <- paste( 'Changepoint type      : Change in', s@cpttype, '<br>' )
    out <- paste(out,  'Method of analysis    :' , s@method , '<br>' )
    out <- paste(out, 'Test Statistic  :' , s@test.stat, '<br>' )
    out <- paste(out, 'Type of penalty       :' , s@pen.type, 'with value', round(s@pen.value, 6), '<br>' )
    out <- paste(out, 'Maximum no. of cpts   : ' , s@ncpts.max, '<br>' )
    out <- paste(out, 'Changepoint Locations :' , mycpts , '<br>' )
    return(out)
    } else {
      return ( 'Insufficient Data' )
    }
})

output$cpvarplot <- renderPlot ({
  mydf <-getquerydata()$mydfin$result
  if (length(mydf) > 0)
    {
    s <- calccpvar()
    labs <-    index( getts() )
    pos <- seq(1, length(labs), 3)
    if ( getterm1( session, FALSE ) == ''  )
    {
      mydrugs <- 'All Drugs'
    }
    else 
    {
      mydrugs <- getterm1( session,FALSE)
    }
    if ( getterm2( session,FALSE)=='' )
    {
      myevents <- 'All Events'
    }
    else 
    {
      myevents <- getterm2( session,FALSE)
    }
    mytitle <- paste( "Change in Variance Analysis for", mydrugs, 'and', myevents )
    plot(s, xaxt = 'n', ylab='Count', xlab='', main=mytitle)
    axis(1, pos,  labs[pos], las=2  )
    grid(nx=NA, ny=NULL)
    abline(v=pos, col = "lightgray", lty = "dotted",
           lwd = par("lwd") )
    }
})

output$cpbayestext <- renderPrint ({
  mydf <-getquerydata()$mydfin$result
  if (length(mydf) > 0)
    {
    mycp <- calccpbayes()
    data <- mycp$data
    bcp.flu <- mycp$bcp.flu
    data$postprob <- bcp.flu$posterior.prob
    data2<-data[order(data$postprob,decreasing = TRUE),]
    data2[1:input$maxcp,]
    } else {
      return ( 'Insufficient Data', file='' )
    }
})
output$cpbayesplot <- renderPlot ({
  mydf <-getquerydata()$mydfin$result
  if (length(mydf) > 0)
    {
    s <- calccpbayes()$bcp.flu
    labs <-    index( getts() )
    plot(s)
    grid()
    }
})
output$querytitle <- renderText({ 
  return( paste('<h4>Counts for', getterm1( session,FALSE), 'with event "', getterm2( session,FALSE), '"</h4>') )
})

output$urlquery <- renderText({ 
  return( getcururl()  )
})

output$applinks <- renderText({ 
  return( makeapplinks(  getcururl(), getqueryvars() )  )
})


output$date1 <- renderText({ 
  l <- getdaterange()
  paste( '<b>', l[3] , 'from', as.Date(l[1],  "%Y%m%d")  ,'to', as.Date(l[2],  "%Y%m%d"), '</b>')
})

#URL management
getcururl <- reactive({
  mypath <- extractbaseurl( session$clientData$url_pathname )
  s <- paste0( session$clientData$url_protocol, "//", session$clientData$url_hostname,
               ':',
               session$clientData$url_port,
               mypath )
  return(s)
})

geturlquery <- reactive({
  q <- parseQueryString(session$clientData$url_search)
  
  updateSelectizeInput(session, inputId = "v1", selected = q$drugvar)
  updateTextInput(session, "t1", value=q$term1)
  updateTextInput(session,"t2", value=q$term2)   
  updateTextInput(session, "drugname", value=q$term1)
  updateTextInput(session,"eventname", value=q$term2) 
  updateSelectizeInput(session, inputId = "v1", selected = q$v1)
  updateTextInput(session, "t1", value=q$t1)
  updateTextInput(session,"t2", value=q$t2) 
  updateTextInput(session, "drugname", value=q$t1)
  updateTextInput(session,"eventname", value=q$t2) 
  updateDateRangeInput(session,'daterange',  start = q$start, end = q$end)
  updateNumericInput(session,'maxcp', value=q$maxcps)
  updateNumericInput(session,'maxcp2', value=q$maxcps)
  updateRadioButtons(session, 'useexactD', selected = q$exactD)
  updateRadioButtons(session, 'useexactE', selected = q$exactE)
  return(q)
})


})