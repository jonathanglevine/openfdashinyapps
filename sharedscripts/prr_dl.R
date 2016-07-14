
#*****************************************************
shinyServer(function(input, output, session) {
#Getters ===============================================================
  getqueryvars <- function( num = 1 ) {
   s <- vector(mode = "character", length = 7)
   if (getwhich() == 'D')
     {
     #Dashboard
     s[1] <- paste0( input$t1, '&v1=', input$v1 )
     
     #PRR for a Drug
     s[2] <- paste0( input$t1, '&v1=', input$v1, gettimeappend() )
     
     #PRR for an Event
     s[3] <- paste0( '', '&v1=', input$v1, gettimeappend() )
     
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
     
   } else {
     #Dashboard
     s[1] <- paste0( '', '&v1=', input$v1 )
     
     #PRR for a Drug
     s[2] <- paste0( '', '&v1=', input$v1 , gettimeappend())
     
     #PRR for an Event
     s[3] <- paste0( input$t1, '&v1=', input$v1, gettimeappend() )
     
     #Dynamic PRR
     s[4] <- paste0( '' , '&v1=', input$v1,  '&t2=', input$t1 )
     
     #CPA
     s[5] <- paste0( '' , '&v1=', input$v1, '&t2=', input$t1 )
     
     #Reportview
     s[6] <- paste0( '', '&t2=', input$t1 )
     
     #labelview
     s[7] <- paste0( '',  input$v1,  '&t2=', input$t1)
     
     #LRTest
     s[8] <- paste0( input$t1, '&v1=', input$v1, gettimeappend() )  
     
     #LRTestE
     s[9] <- paste0( '', '&v1=', input$v1, gettimeappend() )
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
  
  getterm1 <- function(session, quote=TRUE){
#    browser()
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
  

  
  getquarter <- reactive({
    geturlquery()
    end <- input$quarter
    return( end )
  })
  
  gettimeappend <- reactive({
    geturlquery()
    mytime <- getquarter()
   s <- paste0('&start=', '19680101', '&end=', mytime[2] )
    return( s )
  })
# Input SETTERS ====================================================================
  updatevars <- reactive({
    input$update
    closeAlert(session, 'erroralert')
    isolate( {
      updateSelectInput(session, "t1", selected=( input$drugname ) )
      updateNumericInput(session, "limit", value= ( input$limit2 ) )
      updateNumericInput(session, "start", value= ( input$start2 ) )
    })
  })
  
  anychanged <- reactive({
    a <- input$t1
    b <- input$v1
    c <- input$useexact
    closeAlert(session, 'erroralert')
    })
  
  output$mymodal <- renderText({
    if (input$update > 0)
    {
      updatevars()    
      toggleModal(session, 'modalExample', 'close')
    }
    return('')
  })
  
  geturlquery <- reactive({
    q <- parseQueryString(session$clientData$url_search)
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
    updateTabsetPanel(session, 'maintabs', selected=q$curtab)
    return(q)
  })
  
  
  output$drugname <- renderText({ 
    s <- getterm1description( exact='exact', getterm1( session ) )
    renderterm( s, 'Drug Name:') 
    } )
  output$term1 <- renderText({ 
    getterm1( session ) 
  } )
  output$eventname <- renderText({ 
    s <- getterm1description( input$useexact, getterm1( session ) )
    renderterm( s, 'Event Name:') 
  } )
  output$limit <- renderText({ renderterm( getlimit( session ), 'Limit Analysis to', 'most frequent terms') } )
  output$start <- renderText({ 
    startfont <- '<i><font color="dodgerblue" size="4">'
    endfont <- '</font></i>'
    renderterm( getstart( session ), 'Start analysis at ranked frequency count # ',
                              label2=paste( '<br>Analyzing counts with ranked frequencies from',
                                      startfont, getstart( session ) , endfont,
                                     'to', 
                                     startfont, getstart( session )+getlimit( session )-1, endfont  ) ) 
    } )

  output$curtab <- renderText({
    renderterm( input$limit )
    } ) 
# General Reactives ============================================================
   
#Get total counts in database for each event and Total reports in database
  gettotals <- reactive({
    geturlquery()
    comb <- getprr()$mydf
    s <- getterm1()
    total <- comb[ 1, 'AB'] + comb[ 1, 'CD']
    if ( s == '')
      {
      totaldrug <- total
      } else { 
        if ( getwhich() == 'D')
          {
          totaldrug <- comb[ 1, 'AB']
          }
        else
          {
          totaldrug <- comb[ 1, 'AC']
          }
      }
    out <- list(total=total, totaldrug=totaldrug)
    return(out)
  }) 

  getprr <- reactive({
    if ( getwhich() == 'D')
    {
      getprrD()
    }
    else
    {
      getprrE()
    }
  })
  
  #Calculate PRR and put in merged table
  getprrD <- reactive({
    geturlquery() 
    curquarter <- getquarter()
    if ( !exists( 'detable' ) ){
    }
    load( paste0( DATADIR, 'quarters/', curquarter, '.RData') )
    prrtab <- detable
    s <- getterm1()
    mydf <- prrtab
    if ( s == '')
    {
      mydf <- mydf[order(mydf$AC, decreasing = TRUE ),]
      eventcounts <- data.frame( Event=mydf[, 'e'], Count=mydf[, 'AC'] )
    } else {
      curinds <-  which( prrtab$d==s & prrtab$A >= getlimit( session ) )
      mydf <- prrtab[ curinds ,]
      mydf <- mydf[order(mydf$PRR, decreasing = TRUE ),]
      eventcounts <- data.frame( Event=mydf[, 'e'], Count=mydf[, 'A'] )
    }
    
    alleventcounts <- data.frame( Event=mydf[, 'e'], Count=mydf[, 'AC'] )
    
    sourcedf <- data.frame(Event=mydf$e, drugcounts= as.integer(mydf$A), allcounts=mydf$AC, 
                       PRR= round(mydf$PRR, digits=2), LB=round(mydf$lb, digits=2), UB=round(mydf$ub, digits=2), row.names=NULL)
    comb <- sourcedf
#    Add prr link about here
    if ( s != '')
      {
      comb[,'Event'] <- coltohyper( comb[,'Event'], 
                                  'EAS', 
                                  mybaseurl = getcururl(), 
                                  append= paste0( "&v1=", input$v1, "&quarter=", getquarter() ) )
      names <- c('v1', 't1', 't2')
      values <- c( input$v1, getterm1())
      tmp <- getdynprr_as_links(sourcedf[,'Event'],
                          names=names,
                          values=values, 
                          mybaseurl=getcururl()
                          )
      comb <- data.frame(comb, tmp[[1]])
      names( comb ) <- c('Preferred Term',	paste('Counts for', s), 	'Counts for All Reports', 	
                         'PRR', 'Lower 95% CI for PRR', 'Upper 95% CI for PRR', 'Dynamic PRR')
      }
    colname <- 'Preferred Term'
    
    return( list(comb=comb, eventcounts=unique(eventcounts), 
                 alleventcounts=unique(alleventcounts), 
                 colname=colname, prrtab=prrtab, mydf=mydf, sourcedf=sourcedf ) )
  })
  
  getprrE <- reactive({
    geturlquery() 
    curquarter <- getquarter()
    if ( !exists( 'detable' ) ){
    }
    load( paste0( DATADIR, 'quarters/', curquarter, '.RData') )
    prrtab <- detable
    s <- getterm1()
    mydf <- prrtab
    if ( s == '')
    {
      mydf <- mydf[order(mydf$AB, decreasing = TRUE ),]
      drugcounts <- data.frame( Event=mydf[, 'd'], Count=mydf[, 'AB'] )
    } else {
      curinds <-  which( prrtab$e==s & prrtab$A >= getlimit( session ) )
      mydf <- mydf[ curinds ,]
      mydf <- mydf[order(mydf$PRR, decreasing = TRUE ),]
      drugcounts <- data.frame( Event=mydf[, 'd'], Count=mydf[, 'A'] )
   #   mydf <- mydf[order(mydf$A, decreasing = TRUE ),]
    }
    alldrugcounts <- data.frame( Event=mydf[, 'd'], Count=mydf[, 'AB'] )
    
    sourcedf <- data.frame(Drug=mydf$d, drugcounts= as.integer(mydf$A), allcounts=mydf$AB, 
                       PRR= round(mydf$PRR, digits=2), LB=round(mydf$lb, digits=2), UB=round(mydf$ub, digits=2), row.names=NULL)
    comb <- sourcedf
    if ( s != '')
    {
      comb[,'Drug'] <- coltohyper( comb[,'Drug'], 
                                  'DAS', 
                                  mybaseurl = getcururl(), 
                                  append= paste0( "&v1=", input$v1, "&quarter=", getquarter() ) )
      names <- c('v1', 't2', 't1')
      values <- c( input$v1, getterm1())
      tmp <- getdynprr_as_links(sourcedf[,'Drug'],
                                names=names,
                                values=values, 
                                mybaseurl=getcururl()
                            )
      comb <- data.frame(comb, tmp[[1]])
      }
    names( comb ) <- c('Drug',	paste('Counts for', s), 	'Counts for All Reports', 	
                       'PRR', 'Lower 95% CI for PRR', 'Upper 95% CI for PRR', 'Dynamic PRR')
    colname <- 'Drug'
    
    return( list(comb=comb, drugcounts=unique(drugcounts), alldrugcounts=unique(alldrugcounts), colname=colname, prrtab=prrtab, mydf=mydf, sourcedf=sourcedf ) )
  })
  
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

# tabPanel("PRR and ROR Results"

output$prrtitle <- renderText({ 
  geturlquery()
  return('<h4>Reporting Ratios: Results sorted by PRR</h4>')
})

prrd <- reactive({  
  if (getterm1( session )=="") {
    return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, Count=0, PRR=0))
  } else {
    tableout(mydf = getprr()$comb,  
             mynames = NULL,
             error = paste('No records for', getterm1( session ))
    )
  }
} )

prre <- reactive({  
  if (getterm1( session )=="") {
    return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, Count=0, PRR=0))
  } else {
    tableout(mydf = getprrE()$comb,  
             mynames = NULL,
             error = paste('No records for', getterm1( session ))
    )
  }
} )


output$prr2 <- renderDataTable({  
  
  if ( getwhich() == 'D')
  {
    prrd()
    }
  else
  {
    prre()
    }
}, escape = FALSE )

cloudprr <- reactive({ 
  mydf <- getprr()$sourcedf 
  if( getterm1() != '' )
  {
    if( getwhich()=='D')
    {
      mydf <- data.frame(mydf[,1], mydf[,'PRR']*10)
    } else {
      mydf <- data.frame(mydf[,1], mydf[,'PRR']*10)
    }
  } else {
    mydf <- data.frame( unique(mydf[,1]), PRR=1 )
  }
  cloudout(unique(mydf), paste('PRR for Events in Reports That Contain', getterm1( session ) ) )  
})
output$cloudprr <- renderPlot({  
  cloudprr()  
}, height=900, width=900)

textplot <- reactive({ 
  if (getterm1( session )!="") {
    mylist <- getprr()
    mydf <- mylist$comb
#    browser()
    y <- mydf[,'PRR']
    x <- mydf[, 2]
    w <- getvalvectfromlink( mydf[, mylist$colname ] )
  } else {
    w <- NULL
    y <-NULL
    x <- NULL
  }
  #  browser()
  #plot with no overlap and all words visible
  return ( mytp(x, y, w, myylab='PRR') )
  #cloudout(mydf, paste('PRR for Events in Reports That Contain', getterm1( session ) ) )  
})
output$textplot <- renderPlot({ 
 textplot()
}, height=400, width=900)

output$info <- renderTable({
  mylist <- getprr()
  mydf <- mylist$comb
  mydf2 <- mylist$sourcedf
  #   mydf <- data.frame( Event = mydf[,'term'],
  #                       Count = mydf[,'count.x'],
  #                       PRR = mydf[,'prr'] )
  # With base graphics, need to tell it what the x and y variables are.
  brushedPoints(mydf, input$plot_brush, yvar = "PRR", xvar = 2 )
},  sanitize.text.function = function(x) x)




##
# tabPanel("Analyzed Drug/Event Counts for Specified Event/Drug"  
output$alldrugtext <- renderText({ 
  l <- gettotals()
  return( 
    paste( '<b>Total reports with', getsearchtype(), getterm1( session ) , 'in database:</b>', prettyNum( l['totaldrug'], big.mark=',' ), '<br>') )
})

cloudquery <- reactive({  
  if ( getwhich() == 'D')
{
    cloudout( getprr()$eventcounts, paste('Terms in Reports That Contain', getterm1( session ) ))
}
  else
  {
    cloudout( getprr()$drugcounts, paste('Terms in Reports That Contain', getterm1( session ) ))
  }
  
})
output$cloudquery <- renderPlot({  
  cloudquery()
}, height=900, width=900 )


output$specifieddrug2 <- renderDataTable({ 
  tableout(mydf = getprr()$eventcounts,  
           mynames = c('Term', paste( 'Counts for', getterm1( session ) ) ),
           error = paste( 'No results for', getterm1( session ) ) )
})

output$specifiedevent2 <- renderDataTable({ 
  tableout(mydf = getprr()$drugcounts,  
           mynames = c('Term', paste( 'Counts for', getterm1( session ) ) ),
           error = paste( 'No results for', getterm1( session ) ) )
})

# tabPanel("Event Counts for All Drugs" 'alltext' 'queryalltext' 
#'alltitle'  'allquerytext' ,'cloudall', 'all'
output$alltext <- renderText({ 
  l <- gettotals()
  paste( '<b>Total reports in database:</b>', prettyNum(l['total'], big.mark=',' ) )
})
# output$queryalltext <- renderText({ 
#   l <- gettotals()
#   paste( '<b>Query:</b>', removekey( makelink(l['totalurl'] ) ), '<br>')
# })

output$alltitle <- renderText({ 
  return( ('<h4>Counts for Entire Database</h4><br>') )
})

cloudall <- reactive({  
  if ( getwhich() == 'D')
  {
    cloudout(getprr()$alleventcounts, 
             paste('Events in Reports That Contain', getterm1( session ) ) ) 
  }
  else
  {
    cloudout(getprr()$alldrugcounts, 
             paste('Events in Reports That Contain', getterm1( session ) ) ) 
  }
})
output$cloudall <- renderPlot({  
  cloudall()
}, height=900, width=900)

all <- renderTable({  
  
  if ( getwhich() == 'D')
{
  prr()
}
  else
  {
    prre()
  }
  tableout(mydf = geteventtotalstable()$mydf, 
           mynames = c('Term', paste( 'Counts for All Reports'), 'Query' ),
           error = paste( 'No events for', getsearchtype(), getterm1( session ) ) 
  )
})


output$all2 <- renderDataTable({  
  if ( getwhich() == 'D')
  {
    tableout(mydf = getprr()$alleventcounts, 
             mynames = c('Term', paste( 'Counts for All Reports') ),
             error = paste( 'No events for', getsearchtype(), getterm1( session ) ) ) 
  }
  else
  {
    tableout(mydf = getprr()$alldrugcounts, 
             mynames = c('Term', paste( 'Counts for All Reports') ),
             error = paste( 'No events for', getsearchtype(), getterm1( session ) ) ) 
  } 
}, escape = FALSE)


output$maptable <- renderDataTable({  
  tableout(mydf = getmaptable()$outmap, 
           error = paste( 'No events for', getsearchtype(), getterm1( session ) ) 
  )
})

output$fulltable <- renderDataTable({  
  mytable <- getprr()$prrtab
  mynames <- names(mytable)
  mynames[1] <- 'Event'
  mynames[2] <- 'Drug'
  tableout(mydf = mytable, mynames=mynames,
           error = paste( 'No events for', getsearchtype(), getterm1( session ) ) 
  )
})
# tabPanel("Ranked Drug/Event Counts for Event/Drug 'cotextE' 'querycotextE'  'cotitleE',
# 'coquerytextE' ,'cloudcoqueryE', 'coqueryE'

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
    s <- makeapplinks(  getcururl(), getqueryvars( 1 ) ) 
#    write(s, file='')
    return( makeapplinks(  getcururl(), getqueryvars( 1 ) )  )
  }) 
})