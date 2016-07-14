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

ISOtoQuarters <- function( myISO)
  {
    mydates <- as.Date(myISO, '%Y%m%d')
    s <- quarters(mydates)
    labs <- paste( substr( myISO, 1, 4 ), s )
    return( labs )
  }
getaevar <- function(){
  return ('patient.reaction.reactionmeddrapt')
}

gettimevar <- function(){
  anychanged()
  return (input$v2)
}



#Functions
getmaptable <- reactive({
  geturlquery() 
  if ( !exists( 'cleanmpmap' ) ){
    load( paste0( DATADIR, 'cleanmpmap.RData') )
  }
  outmap <- cleanmpmap
  s <- getterm1( session)
  if ( s=='')
  {
    mydf <- outmap
    mydf <- mydf[order(mydf$activesubstancename ),]
  } else {
    curinds <-  which( outmap$activesubstancename ==s )
    mydf <- outmap[ curinds ,]
    mydf <- mydf[order(mydf$medicinalproduct ),]
  }
  #   browser()
  return( list(mydf=mydf, outmap = outmap ) )
})

# #Timerange string from 1 December 1999 to present
# getstartend <- reactive({
#   geturlquery()
#   start <- ('1989-06-30')
#   end <- as.character( Sys.Date() ) 
#   return( c(start, end))
# })

#Merger time series vectors
getdebyquarter <- function( s1='aspirin', s2='flushing', quarters= c( '20150930', '20151231')) {
  geturlquery()
  outdf <- NULL
  flist <- list.files( paste0( DATADIR, 'quarters/'), all.files = TRUE, 
                          ignore.case = TRUE, full.names = FALSE, no..=TRUE)
  quarters <- substr(flist, 1, 8 )
  mydates <- as.Date(quarters, '%Y%m%d')
  qtext <- paste(  substr(quarters, 1, 4 ), quarters(mydates) )
  mytitle <- 'Fetching results for '
  for ( i in seq_along(quarters))
    { 
    createAlert(session, 'alert', 'fetchalert',title=mytitle, content = qtext[i], dismiss = FALSE)
    load( paste0( DATADIR, 'quarters/', quarters[i], '.RData') )
    prrtab <- detable
#    browser()
    mydf <- prrtab
      curinds <-  which( prrtab$d==s1 & prrtab$e == s2 )
      if ( length(curinds > 0) )
        {
        mydf <- data.frame( quarters[i], prrtab[ curinds ,] )
        outdf <- rbind(outdf, mydf)
      }
      closeAlert(session, 'fetchalert')
    }
  return(outdf)
}

buildmergedtable <- reactive({
  geturlquery()
  if ( !exists( 'detable' ) ){
  }
  curquarter <- '20151231'
  load( paste0( DATADIR, 'quarters/', curquarter, '.RData') )
  prrtab <- detable
  s1 <- getterm1(session)
  s2 <- getterm2(session)
  mydf <- prrtab
  if ( s1 == '' | s2 == '' )
  {
    comb <- NULL
  } else {
    curinds <-  which( prrtab$d==s1 & prrtab$e == s2 )

    mydf <- prrtab[ curinds ,]
    mydf <- getdebyquarter(s1, s2)
    comb <- mydf
  }
  return(comb)
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


output$mpvalues <- renderText({ 
  mydf = getmaptable()$mydf
  if ( getterm1(session)!= '')
  {
    s <- paste0(mydf$medicinalproduct, collapse =   '\n')
    return( s )
  }
  else
  {
    return( 'No drug selected')
  }
})



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

# output$query_counts <- renderTable({  
# #  if (input$t1=='') {return(data.frame(Drug='Please enter drug name', Count=0))}
#     mydf <- buildmergedtable()
#  #   print(head(mydf))
#   if ( is.data.frame(mydf) )
#     {
#     return( mydf) 
#     } else  {return(data.frame(Drug=paste( 'No events for drug', getterm1( session, FALSE) ), Count=0))}
#   }, include.rownames = FALSE, sanitize.text.function = (function(x) x) )

getcountstable <- function()
  {
  geturlquery()
  mydf <- buildmergedtable()
  if ( is.data.frame(mydf) )
    {
    mycounts <- c(mydf[1, 'A'],  diff(mydf[, 'A']))
    myquarters <- ISOtoQuarters(mydf[,1] )
    out <- data.frame( myquarters, 
                       mycounts, 
                       mydf[, 'A'], 
                       mydf[, 'PRR'], 
                       mydf[, 'lb'], 
                       mydf[, 'ub'], 
                       mydf[, 's'],
                       stringsAsFactors = TRUE)
    names(out) <- c('Quarter',
                    'Count',
                    'Cumulative Count',
                    'PRR',
                    'LB',
                    'UB',
                    'SD' )
    return(out) 
  } else  {
      return(data.frame(Drug=paste( 'No events for drug', getterm1( session, FALSE) ), Count=0))}
    }

output$query_counts2 <- renderDataTable({  
  #  if (input$t1=='') {return(data.frame(Drug='Please enter drug name', Count=0))}
  mydf <- getcountstable()
  #   print(head(mydf))
  if ( is.data.frame(mydf) )
  {
    return( mydf) 
  } else  {return(data.frame(Drug=paste( 'No events for drug', getterm1( session, FALSE) ), Count=0))}
}, escape=FALSE )



prrplot <- function( type = 'prr')
{
  
  mydf <- buildmergedtable()
  mydf <- mydf[ is.finite(mydf[ , 's' ] ) , ]  
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
      #       showdates <- seq( as.Date(  input$daterange[1] ), as.Date(input$daterange[2] ), 'months' )
      #       showdates <- substr(showdates, 1, 7)
      #       mydf <- mydf[mydf$Date %in% showdates,]
      xloc <- seq(1, nrow(mydf), 1 )
      labs <- mydf[, 1]
      mydates <- as.Date(labs, '%Y%m%d')
      s <- quarters(mydates)
      labs <- paste( substr(labs,1,4 ), s )
      lbgap <-   exp(log(mydf$lb) + .96*mydf$s) #exp ( log( prr ) - 1.96*sd )
      ubgap <-   exp(log(mydf$ub) - .96*mydf$s)
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
      # browser()
      if (type == 'prr')
        {
        myylim <- c( min(.5, min(mydf$lb)), max(2, max(mydf$ub) ) )
        mytitle <- paste( "PRR Plot for", mydrugs, 'and', myevents )
        plot( xloc, mydf$PRR, ylim=myylim, ylab='95% Confidence Interval for PRR',
              xlab='', las=2, xaxt='n', bg='red', cex=.5,  main=mytitle, pch=21)
        axis(1, at=xloc, labels=labs, las=2   )
        if( ! isTRUE( all.equal(mydf$PRR, mydf$lb) ) )
        {
          arrows(x0=xloc[ mydf$PRR!=mydf$lb ], x1=xloc[ mydf$PRR!=mydf$lb ],
                 y0=lbgap[ mydf$PRR!=mydf$lb ], y1=mydf$lb[ mydf$PRR!=mydf$lb ], angle=90, length=.025)
          arrows(x0=xloc[ mydf$PRR!=mydf$ub ], x1=xloc[ mydf$PRR!=mydf$ub ],
                 y1=mydf$ub[ mydf$PRR!=mydf$ub ], y0=ubgap[ mydf$PRR!=mydf$ub ], angle=90, length=.025)
        }
        abline(h=1)
        grid()
      } else {
        
        mydf <- getcountstable()
        mytitle <- paste( "Counts for", mydrugs, 'and', myevents )
        plot( xloc, mydf$Count,  ylab='Count',
              xlab='', las=2, xaxt='n', bg='red', cex=1,  main=mytitle, pch=21)
        axis(1, at=xloc, labels=labs, las=2   )
      }
    } 
  } else  {
    mytitle <-  "Please select a drug and event" 
    plot( c(0,1), c(0,1), yaxt='n', xaxt='n', xlab='', ylab='', main=mytitle )
    text(.5, .5, "Please select a drug and event")
  }
}
output$prrplot <- renderPlot ({
  prrplot()
})

output$countplot <- renderPlot ({
  prrplot(type='count')
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

output$term1 <- renderText({ 
  getterm1( session ) 
} )
geturlquery <- reactive({
  q <- parseQueryString(session$clientData$url_search)
  updateSelectizeInput(session, inputId = "v1", selected = q$v1)
  updateTextInput(session, "t1", value=q$t1)
  updateTextInput(session,"t2", value=q$t2) 
  return( q )
})



output$urlquery <- renderText({ 
  return( getcururl()  )
  })

}) #End shinyServer