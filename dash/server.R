require(shiny)
require(shinyBS)

if (!require('openfda') ) {
  devtools::install_github("ropenhealth/openfda")
  library(openfda)
  print('loaded open FDA')
}

source('sourcedir.R')

# wait <- function(x) {
#   Sys.sleep(x)
# }

#*****************************************************
shinyServer(function(input, output, session) {
  
#  print(session$clientData$url_hostname)
#   mywait <- .0
#   
#   getwaittime <- reactive({ 
#     if(session$clientData$url_hostname == '10.12.207.87')
#     {
#       return( 0.0)
#     } else if(session$clientData$url_hostname == '127.0.0.1') {
#       return (0.5)
#     }
#     return(0.0)
#   })
  
  getqueryvars <- function( num = 1 ) {
    s <- vector(mode = "character", length = 7)
      #Dashboard
      s[1] <- paste0( input$t1, '&v1=', input$v1 )
      
      #PRR for a Drug
      s[2] <- paste0( input$t1, '&v1=', input$v1 )
      
      #PRR for an Event
      s[3] <- paste0( '', '&v1=', input$v1 )
      
      #Dynamic PRR
      s[4] <- paste0( input$t1 , '&v1=', input$v1 )
      
      #CPA
      s[5] <- paste0( input$t1 , '&v1=', input$v1 )
      
      #Reportview
      s[6] <- paste0( input$t1, '&v1=', input$v1 )
      
      #labelview
      s[7] <- paste0( input$t1, '&v1=', input$v1 )
      
      #LRTest
      s[8] <- paste0( input$t1, '&v1=', input$v1 )
      return(s)
  }
  getdrugname <- reactive({ 
#    print(input$usepopcb)
    s <- toupper(input$t1)
    if  (is.null(s) | s=="" ) {
      return("")
    }
    names <- s
    names <- paste0(names, collapse=' ')
    return(names)
  })
  
  gett1v1 <- reactive({ 
    return( c(t1=input$term1, v1=input$var1) )
  })
  
  getquoteddrugname <- reactive({ 
    s <- getdrugname()
    if  (is.null( s ) | s=="" ) {
      return("")
    }
    names <- paste0('%22', s, '%22')
    names <- paste0(names, collapse=' ')
    return(names)
  })
  
  output$t1 <- renderPrint({ 
    return( getdrugname() )
  })
  
  getdrugvarname <- reactive({ 
    return(input$v1)
  })
  
  getexactdrugvarname <- reactive({ 
    return( paste0(input$v1, '.exact') )
  })
  
  getbestdrugvarname <- function(){
    anychanged()
    exact <-   ( getdrugcounts999()$exact)
    if (exact){
      return( getexactdrugvarname() )
    } else {
      return( getdrugvarname() )
    }
  }
  
  getbestdrugname <- function(quote=TRUE){
    exact <-   ( getdrugcounts999()$exact)
    if (exact)
    {
      return( getquoteddrugname() )
    } else {
      return( getdrugname() )
    }
  }
  
  geteventvarname <- reactive({ 
      return(   "patient.reaction.reactionmeddrapt.exact" )
  })
  
  
  fixInput <- reactive({
    
    updateTextInput(session, "t1", value= (input$t1) )
  })
  
  updatevars <- reactive({
    input$update
    isolate( {
      updateTextInput(session, "t1", value=( input$drugname ) )
    })
  })
  
  anychanged <- reactive({
    a <- input$t1
    b <- input$v1
    c <- input$useexact
    closeAlert(session, 'erroralert')
  })
  #************************************
  # Get Seriuosness Query
  #*********************
  
  getseriouscounts <- reactive({
    
#     "seriousnesscongenitalanomali": "1",
#     "seriousnessdeath": "1",
#     "seriousnessdisabling": "1"
#     "seriousnesshospitalization": "1",
#     "seriousnesslifethreatening": "1",
#     "seriousnessother": "1",
    geturlquery()
    mydf <- data.frame(Serious=0, Count=0)
    myurl <- buildURL(v= getbestdrugvarname(), t=getbestdrugname(), 
                      count='seriousnesscongenitalanomali' )
    conganom <-  fda_fetch_p(session, myurl)$result
    if (length(conganom)==0)
      {
      conganom <- c(1,0)
      }
    myurl <- buildURL(v= getbestdrugvarname(), t=getbestdrugname(),
                      count='seriousnessdeath' )
    death <-  fda_fetch_p( session, myurl)$result
    if (length(death)==0)
      {
      death <-  c(1,0)
      }
    
    myurl <- buildURL(v= getbestdrugvarname(), t=getbestdrugname(),
                      count='seriousnessdisabling' )
    disable <- fda_fetch_p( session, myurl)$result
    
    if (length(disable)==0)
    {
      disable <- c(1,0)
    }
    
    myurl <- buildURL(v= getbestdrugvarname(), t=getbestdrugname(),
                      count='seriousnesshospitalization' )
    hosp <- fda_fetch_p( session, myurl)$result
    
    if (length(hosp)==0)
    {
      hosp <- c(1,0)
    }
    
    myurl <- buildURL(v= getbestdrugvarname(), t=getbestdrugname(),
                      count='seriousnesslifethreatening' )
    lifethreat <- fda_fetch_p( session, myurl)$result
    
    if (length(lifethreat)==0)
    {
      lifethreat  <- c(1,0)
    }

    myurl <- buildURL(v= getbestdrugvarname(), t=getbestdrugname(),
                      count='seriousnessother' )
    other <- fda_fetch_p( session, myurl)$result
    
    if (length(other)==0)
    {
      other <- c(1,0)
    }
    
    mydf <- rbind(conganom, death, disable, hosp, lifethreat, other)
   mydf[,'term'] <- c('Congenital Anomaly', 'Death', 'Disability', 'Hospitalization',
                      'Life Threatening', 'Other')
    
    mydf <- mydf[order(mydf[,2]), ]
    return( mydf )
  })    
  
#************************************
# Get Sex Query
#*********************

getsexcounts <- reactive({
  
  
  geturlquery()
  
  myurl <- buildURL(v= getbestdrugvarname(), t=getbestdrugname(),
                    count="patient.patientsex" )
  mydf <- fda_fetch_p( session, myurl)$result
  mydf[,3] <- mydf[,1]
  mydf[ mydf[,1]==2 , 1] <- 'Female' 
  mydf[ mydf[,1]==1 , 1] <- 'Male' 
  mydf[ mydf[,1]==0 , 1] <- 'Unknown' 
  mydf <- mydf[order(mydf[,2]), ]
  
  return( mydf )
})    
#************************************
# Get source Query
#*********************

getsourcecounts <- reactive({
#   1 = Physician
#   2 = Pharmacist
#   3 = Other Health Professional
#   4 = Lawyer
#   5 = Consumer or non-health professional
  
  geturlquery()
  myurl <- buildURL(v= getbestdrugvarname(), t=getbestdrugname(),
                    count="primarysource.qualification" )
  mydf <- fda_fetch_p( session, myurl)$result
  mydf[,3] <- mydf[,1]
  mydf[ mydf[,1]==1 , 1] <- 'Physician' 
  mydf[ mydf[,1]==2 , 1] <- 'Pharmacist' 
  mydf[ mydf[,1]==3 , 1] <- 'Other Health Professional' 
  mydf[ mydf[,1]==4 , 1] <- 'Lawyer' 
  mydf[ mydf[,1]==5 , 1] <- 'Consumer or non-health...' 
  mydf <- mydf[order(mydf[,2]), ]
  
  return( mydf )
})    

  #************************************
  # Get Drug-Event Query
  #*********************
  
  getdrugcounts999 <- reactive({
    
    geturlquery()
    mylist <- getcounts999 ( session, v= getexactdrugvarname(), t= getterm1( session, quote = FALSE ), 
                             count=geteventvarname(), limit=999, exactrad=input$useexact, counter=1 )
    return( list(mydf=mylist$mydf, myurl=(mylist$myurl), exact = mylist$exact  ) )
  })    
  
  # Only use the first value of limit rows
  getdrugcounts <- reactive({
    mylist <-  getdrugcounts999()
    mydf <-  mylist$mydf
    totdf <- gettotals()
    percents <- 100*mydf[,2]/totdf$totaldrug
    mydf <- data.frame( mydf[], percents)
    return( list(mydf=mydf, myurl=mylist$myurl  ) )
  })  
  
  
  #Build table containing drug-event pairs
  getdrugcountstable <- reactive({
    geturlquery()
    mydf <- getdrugcounts()
    myurl <- mydf$myurl
    mydf <- mydf$mydf
    sourcedf <- mydf
    mydf <- data.frame( rep('M', nrow(mydf) ), mydf )
    mydf[,1] <- makemedlinelink(mydf[,2], mydf[,1])
    names <- c('v1','t1' ,'v2', 't2')
    values <- c(getbestdrugvarname(), getbestdrugname(), geteventvarname() )
    mydf[,3] <- numcoltohyper(mydf[ , 3], sourcedf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydf[,2] <- coltohyper(mydf[,2], 'E', mybaseurl = getcururl(),
                           append= paste0( "&v1=", input$v1) )
    return( list(mydf=mydf, myurl=(myurl),  sourcedf=sourcedf  ) )
  })  
  
#**************************
# Concomitant drug table
getcocounts <- reactive({
  geturlquery()
  if ( is.null( getdrugname() ) ){
    return(data.frame( c(paste('Please enter a drug name'), '') ) )
  }
  myurl <- buildURL( v= getbestdrugvarname(), t=getbestdrugname(), 
                     count= getexactdrugvarname(), limit=999 )
  mydf <- fda_fetch_p( session, myurl)
  mydf <- mydf$result
  myrows <- min(nrow(mydf), 999)
  mydf <- mydf[1:myrows,]
#   print(myrows)
#   print(mydf)
  sourcedf<- mydf
  mydf <- data.frame( rep('L', myrows) , mydf )
  mydf <- mydf[!is.na(mydf[,3]), ]

  if (input$v1 != 'patient.drug.medicinalproduct')
    {
      drugvar <- gsub( "patient.drug.","" ,input$v1, fixed=TRUE)
      drugvar <- paste0( "&v1=",URLencode(drugvar, reserved = TRUE )   )
      medlinelinks <- coltohyper( paste0( '%22' , sourcedf[,1], '%22' ), 'L', 
                                  mybaseurl = getcururl(), 
                                  display= rep('L', nrow( sourcedf ) ), 
                                  append= drugvar )
      mydf[,1] <- medlinelinks
    }
  names <- c('v1','t1', 'v2', 't2')
  values <- c(getbestdrugvarname(), getbestdrugname(), getexactdrugvarname() ) 
  mydf[,3] <- numcoltohyper(mydf[ , 3], mydf[ , 2], names, values, mybaseurl = getcururl(), addquotes=TRUE )
  mydf[,2] <- coltohyper(mydf[,2], 'D', mybaseurl = getcururl(), 
                         append= paste0( "&v1=", input$v1) )
  return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
})     

#Indication table
getindcounts <- reactive({
  geturlquery()
  if ( is.null( getdrugname() ) ){
    return(data.frame( c(paste('Please enter a', getsearchtype(), 'name'), '') ) )
  }
  myurl <- buildURL( v= getbestdrugvarname(), t=getbestdrugname(), 
                     count= paste0( 'patient.drug.drugindication', '.exact'), limit=999)
  mydf <- fda_fetch_p( session, myurl)
  mydf <- mydf$result
  myrows <- min(nrow(mydf), 999)
  mydf <- mydf[1:myrows,]
  mydf <- mydf[!is.na(mydf[,2]), ]
  sourcedf <- mydf
  medlinelinks <- makemedlinelink(sourcedf[,1], 'M')
  names <- c('v1','t1', 'v2', 't2')
  values <- c( getbestdrugvarname(), getbestdrugname(), paste0( 'patient.drug.drugindication', '.exact') )
  mydf[,2] <- numcoltohyper(mydf[ , 2], mydf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
  mydf[,1] <- makemedlinelink(sourcedf[,1], mydf[,1])
  
  return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
})   

#  
  #Get total counts in database for each event and Total reports in database
  gettotals<- reactive({
    geturlquery()
    
    
    v <- c( '_exists_', '_exists_' )
    t <- c( geteventvarname(), getexactdrugvarname() )
    totalurl <- buildURL(v, t,  count='', limit=1)
    totalreports <- fda_fetch_p( session, totalurl)    
    total <- totalreports$meta$results$total
    v <- c( '_exists_', getbestdrugvarname() )
    t <- c( geteventvarname(), getbestdrugname() )
    totaldrugurl <- buildURL( v, t, count='', limit=1)
    totaldrugreports <- fda_fetch_p( session, totaldrugurl)    
    if ( length( totaldrugreports )==0 )
    {
      totaldrugurl <- buildURL( v= getdrugvarname(), t=getdrugname(), count='', limit=1)
      totaldrugreports <- fda_fetch_p( session, totaldrugurl)
    }
    
    totaldrug <- totaldrugreports$meta$results$total
    
    adjust <- total/totaldrug
    out <- list(total=total, totaldrug=totaldrug, adjust=adjust, 
                totalurl=(totalurl), totaldrugurl=(totaldrugurl) )
  }) 
  
  
  output$mymodal <- renderText({
    if (input$update > 0)
    {
      updatevars()    
      toggleModal(session, 'modalExample', 'close')
    }
    return('')
  })
#
#Setters ==============
#
  output$drugname <- renderText({
    s <- getdrugname()
    if(s == '') {
      s <- 'None'
    }
    out <- paste( '<br><b>Drug Name:<i>', s, '</i></b><br>' )
    return(out)
  })
  
output$serious <- renderTable({ 
  mydf <- getseriouscounts()
  if ( is.data.frame(mydf) )
  {
    names(mydf) <- c('Serious', 'Case Counts' )
    mysum <- sum( mydf[,'Case Counts'] )
#    browser()
    mydf <- data.frame(mydf, percent =  100*mydf[,'Case Counts']/mysum )
    names(mydf) <- c('Serious', 'Case Counts', '%' )
    mydf[,'Case Counts'] <- prettyNum( mydf[,'Case Counts'], big.mark=',' )
    mydf[,'%'] <- paste0( format( mydf[,'%'], big.mark=',', digits=2, width=4 ), '%' )
    return(mydf) 
  } else  {return(data.frame(Term=paste( 'No results for', getdrugname() ), Count=0))}
}, height=300, align=c("rllr"), sanitize.text.function = function(x) x)  


output$seriousplot <- renderPlot({ 
  mydf <- getseriouscounts()
  if ( is.data.frame(mydf) )
  {
    names(mydf) <- c('Serious', 'Case Counts' )
    return( dotchart(mydf[,2], labels=mydf[,1], main='Seriousness') ) 
  } else  {return(data.frame(Term=paste( 'No results for', getdrugname() ), Count=0))}
}, height=300)  

output$seriouspie <- renderPlot({ 
  mydf <- getseriouscounts()
  if ( is.data.frame(mydf) )
  {
    names(mydf) <- c('Serious', 'Case Counts' )
    return( pie(mydf[,2], labels=mydf[,1], main='Seriousness') ) 
  } else  {return(data.frame(Term=paste( 'No results for', getdrugname() ), Count=0))}
})  
output$sex <- renderTable({ 
  mydf <- getsexcounts()
  if ( is.data.frame(mydf) )
  {
    names(mydf) <- c('Gender', 'Case Counts', 'Code' )
    mysum <- sum( mydf[,'Case Counts'] )
    #    browser()
    mydf <- data.frame(mydf, percent =  100*mydf[,'Case Counts']/mysum )
    names(mydf) <- c('Serious', 'Case Counts', 'Code', '%' )
    mydf[,'Case Counts'] <- prettyNum( mydf[,'Case Counts'], big.mark=',' )
    mydf[,'%'] <- paste0( format( mydf[,'%'], big.mark=',', digits=2, width=4 ), '%' )
    return(mydf) 
  } else  {return(data.frame(Term=paste( 'No results for', getdrugname() ), Count=0))}
}, height=300, align=c("rlllr"), sanitize.text.function = function(x) x)  

output$sexplot <- renderPlot({ 
  mydf <- getsexcounts()
  if ( is.data.frame(mydf) )
  {
    names(mydf) <- c('Gender', 'Case Counts', 'Code' )
    return( dotchart(mydf[,2], labels=mydf[,1], main='Gender') ) 
  } else  {return(data.frame(Term=paste( 'No results for', getdrugname() ), Count=0))}
} , height=300) 

output$sexpie <- renderPlot({ 
  mydf <- getsexcounts()
  if ( is.data.frame(mydf) )
  {
    names(mydf) <- c('Gender', 'Case Counts', 'Code' )
    return( pie(mydf[,2], labels=mydf[,1], main='Gender') ) 
  } else  {return(data.frame(Term=paste( 'No results for', getdrugname() ), Count=0))}
}) 
output$sourceplot <- renderPlot({
  mydf <- getsourcecounts()
  return(dotchart(mydf[,2], labels=mydf[,1], main='Primary Source Qualifications') )
}, height=300)

output$sourcepie <- renderPlot({
  mydf <- getsourcecounts()
  return(pie(mydf[,2], labels=mydf[,1], main='Primary Source Qualifications') )
})

output$source <- renderTable({ 
  mydf <- getsourcecounts()
  if ( is.data.frame(mydf) )
  {
    names(mydf) <- c('Qualifications', 'Case Counts', 'Code' )
    mysum <- sum( mydf[,'Case Counts'] )
    #    browser()
    mydf <- data.frame(mydf, percent =  100*mydf[,'Case Counts']/mysum )
    names(mydf) <- c('Serious', 'Case Counts', 'Code', '%' )
    mydf[,'Case Counts'] <- prettyNum( mydf[,'Case Counts'], big.mark=',' )
    mydf[,'%'] <- paste0( format( mydf[,'%'], big.mark=',', digits=2, width=4 ), '%' )
    return(mydf) 
  } else  {return(data.frame(Term=paste( 'No results for', getdrugname() ), Count=0))}
}, height=300, align=c("rlllr"), sanitize.text.function = function(x) x)  


output$query <- renderTable({  
  mydf <- getdrugcountstable()$mydf
  if ( is.data.frame(mydf) )
  {
    names(mydf) <- c( 'M', 'Preferred Term', paste( 'Case Counts for', getdrugname()), '% Count' )
    return(mydf) 
  } else  {return(data.frame(Term=paste( 'No results for', getdrugname() ), Count=0))}
},  sanitize.text.function = function(x) x)

output$eventcloud <- renderPlot({  
  mydf <- getdrugcountstable()$sourcedf
  if ( is.data.frame(mydf) )
  {
    mytitle <- paste('Events in Reports That Contain', getdrugname() )
    return( getcloud(mydf, title=mytitle ) ) 
  } else  {
    return( data.frame(Term=paste( 'No events for', getdrugname() ) ) )
  }  
  
}, height=800, width=800)

output$querytext <- renderText({ 
  l <- getdrugcounts()
  return( 
    paste( '<b>Query:</b>', removekey( makelink(l['myurl']) ) , '<br>') ) 
})

output$querytitle <- renderText({ 
  return( paste('<h4>Counts for', getdrugname(), '</h4><br>') )
})



output$alltitle <- renderText({ 
  return( ('<h4>Counts for Entire Database</h4><br>') )
})

output$queryalldrugtext <- renderText({ 
  l <- gettotals()
  return( 
    paste( '<b>Query:</b>', removekey( makelink(l['totaldrugurl']) ) , '<br>') ) 
})


output$queryalltext <- renderText({ 
  l <- gettotals()
  paste( '<b>Query:</b>', removekey( makelink(l['totalurl'] ) ), '<br>')
})

output$alldrugtext <- renderText({ 
  l <- gettotals()
  return( 
    paste( '<b>Total reports with', getdrugname() , 'in database:</b>', prettyNum( l['totaldrug'], big.mark=','  ), '<br>') )
})

output$alltext <- renderText({ 
  l <- gettotals()
  paste( '<b>Total reports with drug name in database:</b>', l['total'], '(meta.results.total)<br>')
})

#**********Drugs in reports

output$cotitle <- renderText({ 
  return( ( paste0('<h4>Most Common Events In Selected Reports</h4><br>') ) )
})

output$querycotext <- renderText({ 
  l <- getcocounts()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})

output$coquery <- renderTable({  
  codrugs <- getcocounts()$mydf
  if ( is.data.frame(codrugs) )
  { 
    names(codrugs) <- c('L', 'Drug',  'Counts' )
    return(codrugs) 
  } else  {
    return( data.frame(Term=paste( 'No events for', getdrugname() ) ) )
  }  
  
}, sanitize.text.function = function(x) x)


#addTooltip(session, 'cocloud', tt('cocloud'), placement='top')
output$cocloud <- renderPlot({  
  codrugs <- getcocounts()$sourcedf
  if ( is.data.frame(codrugs) )
  { 
    names(codrugs) <- c('Drug',  'Counts' )
    mytitle <- paste('Medications in Reports That Contain', getdrugname() )
    return( getcloud(codrugs, title=mytitle ) ) 
  } else  {
    return( data.frame(Term=paste( 'No events for', getdrugname() ) ) )
  }  
  
}, height=900, width=900)

output$indquery <- renderTable({  
  # if ( getdrugname() =='') {return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, URL=''))}
  codinds <- getindcounts()$mydf
  if ( is.data.frame(codinds) )
  { 
    names(codinds) <- c('Indication',  'Counts' )
    return(codinds) 
  } else  {
    return( data.frame(Term=paste( 'No', getsearchtype(), 'for', getdrugname() ) ) )
  }  
  
}, sanitize.text.function = function(x) x)


output$indcloud <- renderPlot({ 
  
  withProgress( message = 'Progress', {
  codinds <- getindcounts()$sourcedf  } )
  if ( is.data.frame(codinds) )
  { 
    names(codinds) <- c('Indication',  'Counts' )
    mytitle <- paste('Indications in Reports That Contain', getdrugname() )
    return( getcloud(codinds, title=mytitle ) ) 
  } else  {
    return( data.frame(Term=paste( 'No events for', getdrugname() ) ) )
    }  

}, height=900, width=900)

# output$usepop <- renderUI({
#   p( input$usepopcb )
# })

geturlquery <- reactive({
  q <- parseQueryString(session$clientData$url_search)
#  browser()
  updateSelectizeInput(session, inputId = "v1", selected = q$v1)
  updateNumericInput(session, "limit", value = q$limit)
  updateSelectizeInput(session, 't1', selected= q$drug) 
  updateSelectizeInput(session, 't1', selected= q$t1) 
  updateSelectizeInput(session, 'drugname', selected= q$t1) 
  return(q)
})
createinputs <- reactive({
  q <- parseQueryString(session$clientData$url_search)
#  browser()
  v1 <- 
  t1 <- textInput_p("t1", "Name of Drug", '', 
                     HTML( tt('drugname1') ), tt('drugname2'),
                     placement='bottom')
  useexact <- radioButtons('useexact', 'Match drug name:', c('Exactly'='exact', 'Any Term'='any'),
                    selected='any', inline=TRUE)
  drugname <- textInput_p("drugname", "Name of Drug", '', 
                            HTML( tt('drugname1') ), tt('drugname2'),
                            placement='left')
  return( list(v1=v1, t1=t1, useexact=useexact, drugname=drugname))
})
output$v1_in <- renderUI( {
  
  s <- selectInput_p("v1", 'Drug Variable' , getdrugvarchoices(), 
                HTML( tt('drugvar1') ), tt('drugvar2'),
                placement='top')
})
output$t1_in <- renderUI( {
  
  s <- textInput_p("t1", "Name of Drug", 'aspirin', 
                   HTML( tt('drugname1') ), tt('drugname2'),
                   placement='bottom')
})
output$drugname_in <- renderText( {
  
#   s <- textInput_p("drugname", "Name of Drug", 'aspirin', 
#                    HTML( tt('drugname1') ), tt('drugname2'),
#                    placement='left')
  s <- 'aspirin'
})
output$useexact_in <- renderUI( {
  
  s <- radioButtons('useexact', 'Match drug name:', c('Exactly'='exact', 'Any Term'='any'),
                    selected='any', inline=TRUE)
})


output$date1 <- renderText({ 
  l <- getdaterange()
  paste( '<b>', l[3] , 'from', as.Date(l[1],  "%Y%m%d")  ,'to', as.Date(l[2],  "%Y%m%d"), '</b>')
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
#addPopover(session, 'applinks', "", tt('applinks'), placement='top')
})
