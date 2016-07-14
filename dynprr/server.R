require(shiny)
require('lubridate')
require('curl')
require('zoo')
if (!require('openfda') ) {
  devtools::install_github("ropenhealth/openfda")
  library(openfda)
  print('loaded open FDA')
}

source('sourcedir.R')
 

#**************************************************
#DYNPRR
#*********************************************
shinyServer(function(input, output, session) {
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
  exact <-   ( getquery_d()$exact)
  if (exact){
    return( getexactdrugvar() )
  } else {
    return( getdrugvar() )
  }
}

getbestaevar <- function(){
  exact <-   ( getquery_e()$exact)
  if (exact){
    return( getexactaevar() )
  } else {
    return( getaevar() )
  }
}


getbestterm1 <- function(quote=TRUE){
  quote <-   ( getquery_d()$exact)
  return( getterm1( session, quote))
}

getbestterm2 <- function(quote=TRUE){
  quote <-   ( getquery_e()$exact)
  return( getterm2( session, quote))
}

gettimerange <- reactive({
  geturlquery()
  mydates <- getstartend()
  start <- mydates[1]
  end <-  mydates[2]
  timerange <- paste0('[', start, '+TO+', end, ']')
  return(timerange)
})

#Functions

#Build a time series vector rolled up to month 
gettstable <- function(tmp){
  if (!is.null(tmp) )
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
    names <- c('v1','t1', 'v2' ,'t2', 'v3', 't3')
    values <- c( getbestdrugvar(), getbestterm1(), getbestaevar(), getbestterm2(), gettimevar() )
    mydf_d <- mydf
    mydf_d[,2] <- numcoltohyper(mydf[ , 2], mydates, names, values, type='R', mybaseurl = getcururl(), addquotes=TRUE )
    mydf_d[,3] <- numcoltohyper(mydf[ , 3], mycumdates, names, values, type='R', mybaseurl = getcururl(), addquotes=TRUE )
    #mydf[,2] <- '<b>b</b>'
  } else {
    mydf <- NULL
    mydf_d <- NULL
    mysum <- NULL
  }
mydf <- list(result=mydf, display=mydf_d, total= mysum)

return(mydf)
}

#Reactive queries


#Queries for drug, drug-event, event and all
getquery_de <- reactive({
  geturlquery()
  getquery_d()
  getquery_e()
  v <- c( '_exists_', '_exists_', getbestdrugvar(), getbestaevar() , gettimevar() )
  t <- c(getdrugvar(), getaevar(), getbestterm1(), getbestterm2(), gettimerange() ) 
  myurl <- buildURL(v, t, count=gettimevar() )
  mylist <- fda_fetch_p( session, myurl)
  return( list( mydf=mylist, myurl=myurl) )
})  
  

getquery_d <- reactive({
  geturlquery()
  exactD <- input$useexactD
  if ( exactD=='exact' )
  {
    exact <- TRUE
    v <- c( '_exists_', '_exists_', getexactdrugvar(),  gettimevar() )
    t <- c( getdrugvar(), getaevar(), getterm1( session, quote=TRUE ), gettimerange() )   
    myurl <- buildURL(v, t, count=gettimevar() )
    mylist <- fda_fetch_p( session, myurl)
  } else {
    exact <- FALSE
    v <- c( '_exists_', '_exists_', getdrugvar(),  gettimevar() )
    t <- c( getdrugvar(), getaevar(), getterm1( session, quote=FALSE ), gettimerange() )   
    myurl <- buildURL(v, t, count=gettimevar())
    mylist <- fda_fetch_p( session, myurl)
    }
  return( list( mydf=mylist, myurl=myurl, exact=exact) )
}) 



getquery_e <- reactive({
  geturlquery()
  exactE <- input$useexactE
  if ( exactE=='exact' )
  {
    exact <- TRUE
    v <- c( '_exists_', '_exists_', getexactaevar() , gettimevar() )
    t <- c( getaevar(), getdrugvar(), getterm2( session, quote=TRUE ), gettimerange() )  
    myurl <- buildURL(v, t, count=gettimevar() )
  } else {
    exact <- FALSE
    exact <- FALSE
    v <- c( '_exists_', '_exists_', getaevar(),  gettimevar() )
    t <- c( getaevar(), getdrugvar(), getterm2( session, quote=FALSE ), gettimerange() )   
    myurl <- buildURL(v, t, count=gettimevar() )
  }
  mylist <- fda_fetch_p( session, myurl)
  return( list( mydf=mylist, myurl=myurl, exact=exact) )
})    

getquery_all <- reactive({
  geturlquery()
  v <- c( '_exists_', '_exists_', gettimevar() )
  t <- c(getdrugvar(), getaevar(), gettimerange() )  
  myurl <- buildURL(v, t, count=gettimevar() )
  print(myurl)
  mydf <- fda_fetch_p( session, myurl)
  meta <- mydf$meta
  tmp <- mydf$result
  mydfin <- gettstable(tmp)
  mydf <- list(result=mydfin$result, display=mydfin$display, url=myurl, meta=meta, total= mydfin$total)
  return(mydf)
})    

#*******************************************************
#Reactive Other
#*******************************************************

getvars_de <- reactive({
  mylist <- getquery_de()
  meta <- mylist$mydf$meta
  tmp <- mylist$mydf$result
  myurl <- mylist$myurl
  mydfin <- gettstable(tmp)
  mydf <- list(result=mydfin$result, display=mydfin$display, 
               url=myurl, meta=meta, total= mydfin$total)
  return(mydf)
}) 

getvars_e <- reactive({
  mylist <- getquery_e()
  meta <- mylist$mydf$meta
  tmp <- mylist$mydf$result
  myurl <- mylist$myurl
  mydfin <- gettstable(tmp)
  mydf <- list(result=mydfin$result, display=mydfin$display, 
               url=myurl, meta=meta, total= mydfin$total)
  return(mydf)
}) 

getvars_d <- reactive({
  mylist <- getquery_d()
  meta <- mylist$mydf$meta
  tmp <- mylist$mydf$result
  myurl <- mylist$myurl
  mydfin <- gettstable(tmp)
  mydf <- list(result=mydfin$result, display=mydfin$display, 
               url=myurl, meta=meta, total= mydfin$total)
  return(mydf)
})   

#Timerange string from 1 December 1999 to present
getstartend <- reactive({
  geturlquery()
  start <- ('1989-06-30')
  end <- as.character( Sys.Date() ) 
  return( c(start, end))
})

#Merger time series vectors
buildmergedtable <- reactive({
  mydf1 <- getvars_de()$result
  mydf2 <- getvars_d()$result
  mydf3 <-getvars_e()$result
  mydf4 <- getquery_all()$result
  if ( length(mydf1)*length(mydf2)*length(mydf3)*length(mydf4)> 0 )
    { 
    mydf_d <- merge(mydf1[, c(1,3)], mydf2[, c(1,3)], by.x='Date', by.y='Date')
    names(mydf_d) <- c('Date', 'Drug_Event Counts', 'Drug Counts')
    mydf_all <- merge(mydf3[, c(1,3)], mydf4[, c(1,3)], by.x='Date', by.y='Date')
    names(mydf_all) <- c('Date', 'Event Counts', 'Total Counts')
    mydf <- merge(mydf_d, mydf_all, by.x='Date', by.y='Date')
    comb <- mydf[ mydf[ , 'Event Counts' ] >0, ]
    comb <- comb[ comb[ ,'Total Counts' ] >2 , ]
    oldnames <- names(comb)
    nij <- comb[,'Drug_Event Counts']
    n.j <- comb[, 'Drug Counts' ]
    ni. <- comb[, 'Event Counts' ]
    n.. <- comb[, 'Total Counts' ]
    prrci <- prre_ci( n.., ni., n.j, nij )
    comb <- data.frame(comb, prr=round(prrci[['prr']], 2), sd=round(prrci[['sd']], 2), lb=round(prrci[['lb']], 2), ub=round(prrci[['ub']], 2) )
    names(comb) <-c(oldnames, 'prr', 'sd', 'lb', 'ub')
   start <- paste0( '[',  comb[,1], '01')
   start <- gsub('-', '', start)
   start[1] <- '[19060630'
   lastdate <- ymd(start[ length( start)] )
   month(lastdate) <- month(lastdate)+1
   mydates <- c( ymd(start[2:length(start)] ), lastdate  ) 
   mydates <- round_date(mydates, unit='month')
   mydates <- rollback(mydates)
   mydates <- gsub('-', '', as.character( mydates ))
   mycumdates <- paste0(start[1],  '+TO+', mydates ,']')
   
   
   v <- c( '_exists_', '_exists_', getbestdrugvar(), getbestaevar() , gettimevar() )
   t <- c(getdrugvar(), getaevar(), getbestterm1(), getbestterm2(), gettimerange() ) 
   
   names <- c('v1','t1', 'v2' ,'t2', 'v3', 't3')
   values <- c( getbestdrugvar(), getbestterm1(), getbestaevar(), getbestterm2(), gettimevar() )
   comb[,'Drug_Event Counts'] <- numcoltohyper(comb[,'Drug_Event Counts'], mycumdates, names, values, type='R', mybaseurl = getcururl(), addquotes=FALSE )
    
   names <- c('v1','t1', 'v2' ,'t2', 'v3', 't3')
   values <- c( getbestdrugvar(), getbestterm1(), '_exists_', getbestaevar(), gettimevar() )
   comb[,'Drug Counts'] <- numcoltohyper(comb[,'Drug Counts'], mycumdates, names, values, type='R', mybaseurl = getcururl(), addquotes=FALSE )
   
   names <- c('v1','t1', 'v2','t2', 'v3', 't3')
   values <- c( '_exists_', getbestdrugvar(), getbestaevar(), getbestterm2(), gettimevar() )
   comb[,'Event Counts'] <- numcoltohyper(comb[,'Event Counts'], mycumdates, names, values, type='R', mybaseurl = getcururl(), addquotes=TRUE )
  
   names <- c( 'v1','t1', 'v2','t2','v3', 't3')
   values <- c( '_exists_', getbestdrugvar(), '_exists_', getbestaevar(),  gettimevar() )
   comb[,'Total Counts'] <- numcoltohyper(comb[,'Total Counts'], mycumdates, names, values, type='R', mybaseurl = getcururl(), addquotes=TRUE )
   
    return(comb)
    } else {
      return(NULL)
    }
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
      mynames <- c('-', colname, 'Count', 'Cumulative Sum') 
    }
    names <- c('v1','t1', 'v2', 't2')
    values <- c(getbestaevar(), getbestterm2(), getexactdrugvar() ) 
#Event Table
  } else {
    colname <- 'Preferred Term'
    medlinelinks <- makemedlinelink(sourcedf[,1], 'M')          
    mydf <- data.frame(M=medlinelinks, mydf) 
    mynames <- c('M', colname, 'Count', 'Cumulative Sum' ) 
    names <- c('v1','t1', 'v2', 't2')
    values <- c(getbestdrugvar(), getbestterm1(), getexactaevar() ) 
  }
  mydf[,'count'] <- numcoltohyper(mydf[ , 'count' ], mydf[ , 'term'], names, values, mybaseurl = getcururl(), addquotes=TRUE )
  mydf[,'term'] <- coltohyper(mydf[,'term'], whichcount , mybaseurl = getcururl(), 
                              append= paste0( "&v1=", input$v1) )
  names(mydf) <- mynames
  return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
}   


getdrugeventtotal <- reactive({
  mysum <- getvars_de()$total
  return( mysum )
})

getdrugtotal <- reactive({
  mysum <- getvars_d()$total
  return( mysum )
})
geteventtotal <- reactive({
  mysum <- getvars_e()$total
  return( mysum )
})
getalltotal <- reactive({
  mysum <- getquery_all()$total
  return( mysum )
})


getts <- reactive({
  data <-  getvars_de()$result
  #Check date variable in the dataset to see whether it is in Date format
  #Assign variable you want to run CPA to x
  datax<-data[,2]
  myts <- zoo( datax, data[,1])
  return(myts)
})

updatevars <- reactive({
  input$update
  isolate( {
    updateTextInput(session, "t1", value=( input$drugname ) )
    updateTextInput(session, "t2", value= ( input$eventname ) )
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
#****************************
#Display queries and meta data
#******************************
#**********Drugs in reports

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

output$coquery2 <- renderDataTable({  
  #if ( getterm1() =='') {return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, URL=''))}
  codrugs <- getcocountsD()$mydf
  if ( is.data.frame(codrugs) )
  { 
    return(codrugs) 
  } else  {
    return( data.frame(Term=paste( 'No Events for', getterm1( session) ) ) )
  }  
}, escape=FALSE)   

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

output$coqueryE2 <- renderDataTable({  
  #if ( getterm1() =='') {return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, URL=''))}
  codrugs <- getcocountsE()$mydf
  if ( is.data.frame(codrugs) )
  { 
    return(codrugs) 
  } else  {
    return( data.frame(Term=paste( 'No Events for', getterm1( session ) ) ) )
  }  
}, escape=FALSE)

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
  s <- getterm1( session, FALSE)
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<br><b>Drug Name:<i>', s, '</i></b><br><br>' )
  return(out)
})

output$eventname <- renderText({
  s <- getterm2( session, FALSE)
  if(s == '') {
    s <- 'None'
  }
  out <- paste( '<b>Event Term:<i>', s, '</i></b><br><br>' )
  return(out)
})

output$query_counts <- renderTable({  
#  if (input$t1=='') {return(data.frame(Drug='Please enter drug name', Count=0))}
    mydf <- buildmergedtable()
 #   print(head(mydf))
  if ( is.data.frame(mydf) )
    {
    return( mydf) 
    } else  {return(data.frame(Drug=paste( 'No events for drug', getterm1( session, FALSE) ), Count=0))}
  }, include.rownames = FALSE, sanitize.text.function = (function(x) x) )

output$query_counts2 <- renderDataTable({  
  #  if (input$t1=='') {return(data.frame(Drug='Please enter drug name', Count=0))}
  mydf <- buildmergedtable()
  #   print(head(mydf))
  if ( is.data.frame(mydf) )
  {
    return( mydf) 
  } else  {return(data.frame(Drug=paste( 'No events for drug', getterm1( session, FALSE) ), Count=0))}
}, escape=FALSE )

output$allquerytext <- renderText({ 
  mydf <- getquery_all()
  meta <- mydf$meta
  out <- paste('<h4>Meta Data</h4>',
               '<b>Last Update =</b>', meta$last_updated, 
               '<br><b>Total =</b>', prettyNum( mydf$total, big.mark=',' ), 'total reports',
               'for dates from', gettimerange(),
               '<br> <b>Query =</b>', removekey(  makelink(mydf$url) ) )
  return(out)
})

 output$drugeventquerytext <- renderText({ 
   mydf <- getvars_de()
    out <- paste('<b>Total =</b>', prettyNum( getdrugeventtotal(), big.mark=',' ), 'reports for', getterm1( session, FALSE), 'and' , getterm2( session, FALSE), 
                 'for dates from', gettimerange(),
             '<br><b>Query =</b>', removekey( makelink(mydf$url) ), '<br><br>' )
  return(out)
  })

output$eventquerytext <- renderText({ 
  mydf <- getvars_e()
  out <- paste('<b>Total =</b>', prettyNum( geteventtotal(), big.mark=',' ),  'reports for', getterm2( session, FALSE),
               'for dates from', gettimerange(),
               '<br><b>Query =</b>', removekey( makelink(mydf$url) ), '<br><br>' )
  return(out)
})

output$drugquerytext <- renderText({ 
  mydf <- getvars_d()
  meta <- mydf$meta
  out <- paste(
    '<br><b>Total =</b>', prettyNum( mydf$total, big.mark=',' ), 'reports for', getterm1( session, FALSE), 
    'for dates from', gettimerange(),
    '<br><b>Query =</b>', removekey( makelink(mydf$url) ), '<br><br>' )
  return(out)
} )


output$prrplot <- renderPlot ({
  mydf <- buildmergedtable()
  mydf <- mydf[ is.finite(mydf[ , 'sd' ] ) , ]  
  if ( getterm1( session, FALSE)==''  )
  {
    mydrugs <- 'All Drugs'
  }
  else 
  {
    mydrugs <- getterm1( session, FALSE)
  }
  if ( getterm2( session, FALSE)=='' )
  {
    myevents <- 'All Events'
  }
  else 
  {
    myevents <- getterm2( session, FALSE)
  }
  #****************************************
  #* maketiff
#   if ( !is.null(mydf) & (1==2) )
#   {
#     filename <- paste0( getterm1(), getterm2(), '.tif')
# #    print(filename)
#     filename <- gsub('%22', '', filename)
#     if ( nrow(mydf) >0 )
#     {
#       print(filename)
#       tiff(file = filename, res=600, compression='lzw',height=4, width=6, units="in",  bg = "transparent" )
#       showdates <- seq( as.Date(  input$daterange[1] ), as.Date(input$daterange[2] ), 'months' )
#       showdates <- substr(showdates, 1, 7)
#       mydf <- mydf[mydf$Date %in% showdates,]
#       myylim <- c( min(.5, min(mydf$lb)), max(2, max(mydf$ub) ) )
#       xloc <- ymd( mydf$Date, truncated=2 )
#       labs <- mydf$Date
#       
#       lbgap <-   exp(log(mydf$lb) + .96*mydf$sd) #exp ( log( prr ) - 1.96*sd )
#       ubgap <-   exp(log(mydf$ub) - .96*mydf$sd)
#       #   title <- paste( 'PRR Plot for', input$t1,  'and', input$t2 )  
#       mytitle <- paste( "PRR Plot for", mydrugs, 'and', myevents )
#       plot( xloc, mydf$prr, ylim=myylim, ylab='95% Confidence Interval for PRR',
#             xlab='', las=2, xaxt='n', bg='red', cex=.5,  main=mytitle, pch=21)
#       axis(1, at=xloc[index(xloc)%%6==0], labels=labs[index(labs)%%6==0], las=2   )
#       if( ! isTRUE( all.equal(mydf$prr, mydf$lb) ) )
#       {
#         arrows(x0=xloc[ mydf$prr!=mydf$lb ], x1=xloc[ mydf$prr!=mydf$lb ],
#                y0=lbgap[ mydf$prr!=mydf$lb ], y1=mydf$lb[ mydf$prr!=mydf$lb ], angle=90, length=.025)
#         arrows(x0=xloc[ mydf$prr!=mydf$ub ], x1=xloc[ mydf$prr!=mydf$ub ],
#                y1=mydf$ub[ mydf$prr!=mydf$ub ], y0=ubgap[ mydf$prr!=mydf$ub ], angle=90, length=.025)
#       }
#       abline(h=1)
#       grid()
#     } else  {
#       mytitle <- paste( "PRR Plot for", mydrugs, 'and', myevents )
#       plot( xloc, mydf$prr, ylim=myylim, ylab='95% Confidence Interval for PRR',
#             xlab='', las=2, xaxt='n', bg='red', cex=.5,  main=mytitle, pch=21)
#       }
#     dev.off()
#   }
  #end maketiff
  if ( !is.null(mydf) & getterm1( session, FALSE)!='' & getterm2( session, FALSE)!='' )
    {
    if ( nrow(mydf) >0 )
      {
      showdates <- seq( as.Date(  input$daterange[1] ), as.Date(input$daterange[2] ), 'months' )
      showdates <- substr(showdates, 1, 7)
      mydf <- mydf[mydf$Date %in% showdates,]
      myylim <- c( min(.5, min(mydf$lb)), max(2, max(mydf$ub) ) )
      xloc <- ymd( mydf$Date, truncated=2 )
      labs <- mydf$Date
      
      lbgap <-   exp(log(mydf$lb) + .96*mydf$sd) #exp ( log( prr ) - 1.96*sd )
      ubgap <-   exp(log(mydf$ub) - .96*mydf$sd)
   #   title <- paste( 'PRR Plot for', input$t1,  'and', input$t2 )    
      if ( getterm1( session, FALSE)==''  )
      {
        mydrugs <- 'All Drugs'
      }
      else 
      {
        mydrugs <- getterm1( session, FALSE)
      }
      if ( getterm2( session, FALSE ) =='' )
      {
        myevents <- 'All Events'
      }
      else 
      {
        myevents <- getterm2( session, FALSE )
      }
      mytitle <- paste( "PRR Plot for", mydrugs, 'and', myevents )
      plot( xloc, mydf$prr, ylim=myylim, ylab='95% Confidence Interval for PRR',
            xlab='', las=2, xaxt='n', bg='red', cex=.5,  main=mytitle, pch=21)
      axis(1, at=xloc[index(xloc)%%6==0], labels=labs[index(labs)%%6==0], las=2   )
      if( ! isTRUE( all.equal(mydf$prr, mydf$lb) ) )
        {
        arrows(x0=xloc[ mydf$prr!=mydf$lb ], x1=xloc[ mydf$prr!=mydf$lb ],
             y0=lbgap[ mydf$prr!=mydf$lb ], y1=mydf$lb[ mydf$prr!=mydf$lb ], angle=90, length=.025)
        arrows(x0=xloc[ mydf$prr!=mydf$ub ], x1=xloc[ mydf$prr!=mydf$ub ],
             y1=mydf$ub[ mydf$prr!=mydf$ub ], y0=ubgap[ mydf$prr!=mydf$ub ], angle=90, length=.025)
        }
      abline(h=1)
      grid()
    } 
  } else  {
    mytitle <-  "Please select a drug and event" 
    plot( c(0,1), c(0,1),  main=mytitle )
    text(.5, .5, "Please select a drug and event")
  }
})


output$querytitle <- renderText({ 
  return( paste('<h4>Counts for', getterm1( session, FALSE), 'with event "', getterm2( session, FALSE), '"</h4>') )
})


#URL Management 
getcururl <- reactive({
  mypath <- extractbaseurl( session$clientData$url_pathname )
  s <- paste0( session$clientData$url_protocol, "//", session$clientData$url_hostname,
               ':',
               session$clientData$url_port,
               mypath )
  return(s)
})

output$applinks <- renderText({ 
  return( makeapplinks(  getcururl(), getqueryvars() )  )
})

output$date1 <- renderText({ 
  l <- getdaterange()
  paste( '<b>Reports from', as.Date(l[1],  "%Y%m%d")  ,'to', as.Date(l[2],  "%Y%m%d"), '</b>')
})
geturlquery <- reactive({
  q <- parseQueryString(session$clientData$url_search)
  updateSelectizeInput(session, inputId = "v1", selected = q$drugvar)
  updateTextInput(session, "t1", value=q$term1)
  updateTextInput(session,"t2", value=q$term2) 
  updateTextInput(session, "drugname", value=q$term1)
  updateTextInput(session,"eventname", value=q$term2) 
  updateDateRangeInput(session,'daterange',  start = q$start, end = q$end)
  updateSelectizeInput(session, inputId = "v1", selected = q$v1)
  updateTextInput(session, "t1", value=q$t1)
  updateTextInput(session,"t2", value=q$t2) 
  updateTextInput(session, "drugname", value=q$t1)
  updateTextInput(session,"eventname", value=q$t2) 
  updateDateRangeInput(session,'daterange',  start = q$start, end = q$end)
  updateRadioButtons(session, 'useexactD', selected = q$exactD)
  updateRadioButtons(session, 'useexactE', selected = q$exactE)
  return( q )
})



output$urlquery <- renderText({ 
  return( getcururl()  )
  })

}) #End shinyServer