

#*****************************************************
shinyServer(function(input, output, session) {

# Getters ======  

  
  getquarter <- reactive({
    geturlquery()
    end <- input$quarter
    return( end )
  })
  getstartquarter <- reactive({
    geturlquery()
    start <- input$startquarter
    return( start )
  })
  getqueryvars <- function( num = 1 ) {
   s <- vector(mode = "character", length = 7)
   if (getwhich() == 'D')
     {
     #Dashboard
     s[1] <- paste0( input$t1, '&v1=', input$v1 )
     
     #PRR for a Drug
     s[2] <- paste0( input$t1, '&v1=', input$v1 )
     
     #PRR for an Event
     s[3] <- paste0( '', '&v1=', input$v1 )
     
     #Dynamic PRR
     s[4] <- paste0( input$t1 , '&v1=', input$v1 )
     
     #CPA
     s[5] <- paste0(input$t1 , '&v1=', input$v1 )
     
     #Reportview
     s[6] <- paste0( input$t1, '&v1=', input$v1 )
     
     #labelview
     s[7] <- paste0( input$t1, '&v1=', input$v1 )
     
     #LRTest
     s[8] <- paste0( input$t1, '&v1=', input$v1, gettimeappend() )
     
     #LRTestE
     s[9] <- paste0( '', '&v1=', input$v1 , gettimeappend())
     
   } 
   return(s)
 }


  getsearchtype <- reactive({ 
    if (getwhichprogram() == 'E'){
      return(   "Reaction" )
    } else {
      return( 'Drug' )
    }
  })
  
  getwhichprogram <- reactive( {
    return( getwhich() )
  })
  


  
  getterm1 <- function(session=NULL, quote=TRUE){
    #    browser()
    geturlquery()
    s <- input$t1
    len <- length( s )
    #     if (len >2)
    #     {
    #       browser()
    #     }
    out <- s
    if ( len > 1)
    {
      out[1:(len-1)] <- paste0(out[1:(len-1)], ' OR ')
      out <- paste0( out, collapse = '')
    }
    if (len == 0)
    {
      out <- ''
    }
    return( out )
  }
  

  gettimeappend <- reactive({
    geturlquery()
    s <- paste0('&start=', 20040101 , '&end=', getquarter() )
    return( s )
  }) 

  #end getters
# Queries =======
  
  output$drugname <- renderText({
    s <- getterm1(session)
    if(s == '') {
      s <- 'None'
    }
    out <- s
    return(out)
  })
  

#************************************
# Get Drug-Event Query
#*********************
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
  

  getdownload <- function(tabname, skip=1, limit=100)
  {  
    mydf <- prrsource()
    outdf <- prr()
    outdf[[2]] <- mydf[['term']]
    return( outdf[, 2:5 ] ) 
  }
  

loadquarter <- reactive({
  curquarter <- getquarter()
  startquarter <- getstartquarter()
  if (curquarter < startquarter)
  {
    return( list( prrtab=NULL, fulltable=NULL ) )
  }
  mytitle <- 'Getting counts.'
  createAlert(session, 'alert', 'countalert',
              title=mytitle, content = 'This may take a while.', dismiss = FALSE)
  quartervals <- unlist(getquarters())
  startloc <- min( which( quartervals %in% startquarter ) + 1 , length(quartervals) )
#   print( startloc )
#   print(startquarter)
#   print( quartervals[startloc] )
  load( paste0( DATADIR, 'quarters/', curquarter, '.RData') )
  fulltable <- detable
  # A = drug-event count
  # C = Event count minus drug-event count
  # AC = Total Event count
  # B = Other drugs with event
  # CD = Total Other events
  # AB = Total Reports for drug
  detable <- detable[, c('e', 'd', 'PRR', 'A', 'AB', 'AC', 'C', 'CD')]
  prrtab <- detable
  alleventcounts <- data.frame( Event=prrtab[, 'e'], Count=prrtab[, 'AC'] )
  alleventcounts <- unique(alleventcounts)
  #Sum over all events regardless of number reports
  totalevents <- sum( alleventcounts[, 'Count'] )
#  browser()
  mydrug <- getterm1()
  if ( mydrug == '')
  {
    prrtab <- prrtab[order(prrtab$AC, decreasing = TRUE ),]
  } else {
    curinds <-  which( prrtab$d==mydrug )
    prrtab <- prrtab[ curinds ,]
  }
  if (startloc < length( quartervals ) & ( mydrug != '') )
  {
    load( paste0( DATADIR, 'quarters/', quartervals[ startloc], '.RData') )
    detable <- detable[, c('e', 'd', 'A', 'AB', 'AC', 'C', 'CD')]
    
    alleventcounts <- data.frame( Event=detable[, 'e'], Count=detable[, 'AC'] )
    alleventcounts <- unique(alleventcounts)
    totalevents <- totalevents - sum( alleventcounts[, 'Count'] )
    
    curinds <-  which( detable$d==mydrug )
    detable <- detable[ curinds ,]
    starttable <- detable
#    browser()
    # A = drug-event count
    # C = Event count minus drug-event count
    # AC = Total Event count
    # B = Other drugs with event
    # CD = Total Other events
    # AB = Total Reports for drug
    mycols <- c( 'A', 'AB', 'AC', 'C', 'CD' )
    for ( i in  1:nrow(prrtab) )
    {
    k <- match( prrtab[i, 'e'],  starttable$e )
    if( !is.na(k) )
      {
        prrtab[i ,mycols] <- prrtab[i ,mycols] - starttable[ k ,mycols]
      }
    }
  } 
  closeAlert(session, 'countalert')
  return( list( prrtab=prrtab, totalevents=totalevents, fulltable=fulltable ) )
})
# Calculations ========= #Calculate PRR and put in merged table
  getprr <- reactive({
    geturlquery() 
    detable <- loadquarter()$prrtab
    if ( is.null( detable ) )
      {
      return( list(comb=NULL, 
                   colname=NULL, prrtab=NULL, mydf=NULL, 
                   sourcedf=NULL , critval=list(critval=NA, critval01=NA, mymax=NA) ) )
    }
    if ( nrow( detable )<1 )
    {
      return( list(comb=detable, 
                   colname=NULL, prrtab=NULL, mydf=NULL, 
                   sourcedf=NULL , critval=list(critval=NA, critval01=NA, mymax=NA) ) )
    }
    prrtab <- detable
    mydrug <- getterm1()
    mydf <- prrtab
    # A = drug-event count
    # C = Event count - drug-event count
    # AC = Event count
    # B = Other drugs with event
    # CD = Other events
    # AB = Reports for drug

#     sourcedf <- data.frame(Event=mydf$e, drugcounts= as.integer(mydf$A), allcounts=mydf$AC, 
#                            PRR= round(mydf$PRR, digits=2), LB=round(mydf$lb, digits=2), UB=round(mydf$ub, digits=2), row.names=NULL)
# Get event counts for selected drug( A) and all drugs (AC)   
    sourcedf <- data.frame(Event=mydf$e, drugcounts= as.integer(mydf$A), 
                           allcounts=mydf$AC, row.names=NULL)
    comb <- sourcedf
    eventcounts <- unique( comb[, c('Event', 'drugcounts', 'allcounts')] )
    totals <- list( total=loadquarter()[['totalevents']], 
                    totaldrug=sum( eventcounts[, 'drugcounts'] ) )
# count.x is A, count.y is AC
    names(eventcounts) <- c('term', 'count.x', 'count.y')
    #    Add prr link about here
    if ( mydrug != '')
    {    
      mytitle <- 'Calculating LLRs.'
      createAlert(session, 'alert', 'lrtalert',
                  title=mytitle, content = 'This may take a while.', dismiss = FALSE)
      #n.. is total events in db
      n.. <- totals$total
      #Total events for drug j
      n.j <- totals$totaldrug
      #p.j is observed prob for drug in database
      p.j <- n.j/n..
      
      #Total reports for DE combination
      nij <-  eventcounts$count.x
      #Total reports for event i
      ni. <- eventcounts$count.y
     # pi. is observed prob of event in database 
      pi. <- ni./(n.. )
      a <- nij
      b <- ni. - nij
      c <- n.j - nij
      d <- n.. - ni. - n.j + nij
      mystats <- calcLRTstats(totals, eventcounts, allevents=n..)
      comb[,'Event'] <- coltohyper( comb[,'Event'], 
                                    'LREAS', 
                                    mybaseurl = getcururl(), 
                                    append= paste0( "&v1=", input$v1, "&quarter=", getquarter(), "&startquarter=", getstartquarter() ) )
      names <- c('v1', 't1', 't2')
      values <- c( input$v1, getterm1())
      tmp <- getdynprr_as_links(sourcedf[,'Event'],
                                names=names,
                                values=values, 
                                mybaseurl=getcururl()
      )
      comb <- data.frame(comb, tmp[[1]])
      
      comb <- data.frame(comb, sigcol="NS",  LLR=mystats$LLRE, rrr=mystats$RR, stringsAsFactors = FALSE)
      sourcedf <- data.frame( sourcedf, LLR=comb[, 'LLR'] )
      comb <- data.frame( comb, a, b, c, d, 
                          pi., 
                          p.j, nij,  n.j, ni.,  n..)
      sourcedf <- sourcedf[order(sourcedf$LLR, decreasing = TRUE ),]
      comb <- comb[order(comb$LLR, decreasing = TRUE ),]
      names( comb ) <- c('Preferred Term',	paste('Counts for', mydrug ), 	'Counts for All Reports', 	
                         # 'PRR', 'Lower 95% CI for PRR', 'Upper 95% CI for PRR', 
                         'Dynamic PRR',
                         'Significant?', 'LLR', 'RRR',
                         'a', 'b', 'c','d',
                         'pi.',
                         'p.j', 'nij',  'n.j', 'ni.',  'n..')
      numsims <- getnumsims(session)
#      browser()
      closeAlert(session, 'lrtalert')
      mycritval <- getCritVal2(session, numsims, comb$n.j[1], comb$ni., comb$n..[1], comb$pi., .95)
      critval05 <- mycritval$critval
      comb[ comb$LLR > critval05  , "Significant?"] <- "p < 0.05"
      keptcols <- c('Preferred Term',	paste('Counts for', mydrug ), 	'Counts for All Reports', 
                    'Significant?', 'LLR', 'RRR', 'nij' )
    } else {
      
      mycritval <- list(critval=NA, critval01=NA, mymax=NA)
      critval05 <- NA
      keptcols <- names(comb)
    }
    colname <- 'Preferred Term'
    
    return( list(comb=comb[, keptcols], 
                 colname=colname, prrtab=prrtab, mydf=mydf, sourcedf=sourcedf , critval=mycritval) )
  })

# setters ======
#Tab 1: LRT Results based on Total Events
prr <- reactive({  
  if (getterm1(session)=="") {
    return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, Count=0, PRR=0, ROR=0))
  } 
  comb <- checkdf( getprr()[['comb']], getsearchtype() )
  if ( nrow(comb)< 1 ) {
    return(data.frame(Term=paste('No results for', getsearchtype(), getterm1() ), Count=0, Count=0, PRR=0, ROR=0))
  }
  comb[, 'LLR'] <- round( comb[, 'LLR'], digits=2 )
  comb[, 'RRR'] <- round( comb[, 'RRR'], digits=2 )
  return( comb )
})
prrsource <- reactive({  
  if (getterm1(session)=="") {
    return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, Count=0, PRR=0, ROR=0))
  }
  return( checkdf( getprr()[['sourcedf']], getsearchtype() ) )
})

prrnohyper <- reactive({  
  myprr <- prr()
  mysource <- prrsource()
  myprr[,1] <- mysource[,1]
  out <- myprr
  return(out)
})

output$prr <- renderDataTable({   
  prr()
},  escape=FALSE)

getcloudprrscale <- reactive({
#  browser()
  scale <- getcloud_try(getprr()$sourcedf, name=1, freq='LLR',  scale1=9 )
  return(scale)
})
cloudprr <- function(){ 
 # print(head(getprr()$sourcedf))
#  scale <- getcloudprrscale()
  cloudplot( mydf = getprr()$sourcedf, session, scale1=9, 
             name=1, freq = 'LLR', stattext='LLR' )
}
output$cloudprr <- renderPlot({  
  cloudprr()
}, height=900, width=900)

output$cloudwarnings <- renderText({  
  warnings()
} )

textplot <- function(){ 
  if (getterm1( session )!="") {
    mylist <- getprr()
    mydf <- mylist$comb
    mydf <- mydf[ which(mydf[,'LLR'] > 0 ),]
    y <- mydf[,'LLR']
    x <- mydf[, 2 ]
    w <- getvalvectfromlink( mydf[, mylist$colname ] )
    refline <- mylist$critval$critval
  } else {
    w <- NULL
    y <-NULL
    x <- NULL
    refline <- 1
  }
  #plot with no overlap and all words visible
  return ( mytp(x, y, w, refline ) )
  #cloudout(mydf, paste('PRR for Events in Reports That Contain', getterm1( session ) ) )  
}
output$textplot <- renderPlot({ 
  textplot()
}, height=400, width=900)

prrtitle <- reactive({ 
 # maxLRT <- getprr()$maxLRT
  critval <- getprr()[['critval']][['critval']]
  return( paste( '<h4>Reporting Ratios</h4>',
                 'Critical Value =',  round( critval, 2),
                 '<br># of Simulations =',  getnumsims( session )
  ) )
})
output$prrtitle <- renderText({ 
  prrtitle()
})

info <- reactive(
  { 
    mylist <- getprr()
    mydf <- mylist$comb
    brushedPoints(mydf, input$plot_brush, yvar = "LLR", xvar = 'nij' )
  }
)
output$info <- renderTable({
  info()
},  sanitize.text.function = function(x) x)
##
# Tab 2: Simulation Results for Event Based LRT

simplot <- function(){  
  getcururl()
  if (getterm1(session)=="") {
    return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, Count=0, PRR=0, ROR=0))
  } else {
    mydf <- getprr()
    mycrit <- mydf$critval$critval
    vals <- mydf$critval$mymax
    myrange <- range(vals)
    interval <- (mycrit - myrange[1])/20
    mybreaks <- c( seq(myrange[1], mycrit, interval ),  seq(mycrit+interval,  myrange[2] + interval, interval ) )
    mytruehist <- hist(vals , breaks=mybreaks, 
             main="Histogram of Simulated Distribution of LLR", 
             xlab='Loglikelihood Ratio', xaxt='n', col='cyan' )
    text(mycrit, .75*max(mytruehist$counts), paste('Rejection Region,\n LLR >', round(mycrit, 2) ), pos=4, col='red')
    mystep <- max(1, floor( ( myrange[2] - myrange[1] )/20) )
    smallbreaks <- seq(0, max(mybreaks), mystep )
    
    smallbreaks <-  c( round(mycrit, 2), smallbreaks )
    axis(1, smallbreaks, las=3 )
    abline(v=mycrit, col='red', lwd=2)
    if ( is.data.frame(mydf) ) 
    {
    } else {
      return(data.frame(Term= paste('No records for', getterm1(session)), Count=0))
    }
  }
}
output$simplot <- renderPlot({  
  getcururl()
  simplot()
} )



# URL Stuff =====
geturlquery <- reactive({
  q <- parseQueryString(session$clientData$url_search)
  updateNumericInput(session, "limit", value = q$limit)
  updateNumericInput(session, "limit2", value = q$limit)
  if( getwhich()== 'D'){
    updateSelectizeInput(session, 't1', selected= q$drug)
    updateSelectizeInput(session, 't1', selected= q$t1)
    updateSelectizeInput(session, 'drugname', selected= q$drug)
    updateSelectizeInput(session, 'drugname', selected= q$t1)   
} else {
  updateSelectizeInput(session, 't1', selected= q$event)
  updateSelectizeInput(session, 't1', selected= q$t1)
  updateSelectizeInput(session, 'drugname', selected= q$event)
  updateSelectizeInput(session, 'drugname', selected= q$t1)    
}
  updateSelectizeInput(session, inputId = "v1", selected = q$drugvar)
  updateSelectizeInput(session, inputId = "v1", selected = q$v1)
  updateSelectizeInput(session, inputId = "quarter", selected = q$quarter)
  updateSelectizeInput(session, inputId = "startquarter", selected = q$startquarter)
  return(q)
})
# Return the components of the URL in a string:
output$urlText <- renderText({
  paste(sep = "",
        "protocol: ", session$clientData$url_protocol, "\n",
        "hostname: ", session$clientData$url_hostname, "\n",
        "pathname: ", session$clientData$url_pathname, "\n",
        "port: ",     session$clientData$url_port,     "\n",
        "search: ",   session$clientData$url_search,   "\n"
  )
  return(getbaseurl('E') )
  
})

# Parse the GET query string
output$queryText <- renderText({
  query <- geturlquery()
  # Return a string with key-value pairs
  paste(names(query), query, sep = "=", collapse=", ")
})


getcururl <- reactive({
  mypath <- extractbaseurl( session$clientData$url_pathname )
  s <- paste0( session$clientData$url_protocol, "//", session$clientData$url_hostname,
               ':',
               session$clientData$url_port,
               mypath )
  
  return(s)
})


  output$urlquery <- renderText({ 
    return( getcururl()  )
  })
  
  output$applinks <- renderText({ 
    return( makeapplinks(  getcururl(), getqueryvars( 1 ) )  )
  }) 
  

  output$downloadReport <- downloadHandler(
#     filename=filename(session, 'my-report', filetype=getfiletype() ),
#     content = content(session, file, filetype=getfiletype())
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      rmdfile <- 'report.Rmd'
      src <- normalizePath( rmdfile )
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, rmdfile, overwrite = TRUE)
      
      library(rmarkdown)
      out <- render(rmdfile, switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
 output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$t1, Sys.time(), '.csv', sep='')
    },
    content = function(con) {
      write.csv( getdownload( NULL, skip = 1 ), con, row.names=FALSE)
    },
    contentType="text/csv"
  )
 
 output$fulltable <- renderDataTable({  
    mytable <- getprr()[['prrtab']]
    mytable[, 'PRR'] <- round( mytable[, 'PRR'], digits = 2 )
    mynames <- names(mytable)
    mynames[1] <- 'Event'
    mynames[2] <- 'Drug'
    tableout(mydf = mytable, mynames=mynames,
             error = paste( 'No events for', getsearchtype(), getterm1( session ) ) 
    )
  })
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
 
 output$mpmaptext <- renderText({ 
   paste( 'Activesubstancename represents medicinalproduct values as shown in the table below.', '<a href="cleanmpmap.csv"  target="_blank">Download complete activesubstancename-medicinalproduct mapping </a>' , '' )
 })
  
})