
#*****************************************************
shinyServer(function(input, output, session) {
#Getters ===============================================================
  getqueryvars <- function( num = 1 ) {
   s <- vector(mode = "character", length = 7)
   if (getwhich() == 'DEV')
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
     s[8] <- paste0( input$t1, '&v1=', input$v1 )
   } else {
     #Dashboard
     s[1] <- paste0( '', '&v1=', input$v1 )
     
     #PRR for a Drug
     s[2] <- paste0( '', '&v1=', input$v1 )
     
     #PRR for an Event
     s[3] <- paste0( input$t1, '&v1=', input$v1 )
     
     #Dynamic PRR
     s[4] <- paste0( '' , '&v1=', input$v1, '&v2=', getbestvar1(), '&t2=', input$t1 )
     
     #CPA
     s[5] <- paste0( '' , '&v1=', input$v1, '&v2=', getbestvar1(), '&t2=', input$t1 )
     
     #Reportview
     s[6] <- paste0( '', '&v1=', input$v1, '&v2=', getbestvar1() , '&t2=', input$t1 )
     
     #labelview
     s[7] <- paste0( '', '&v1=', input$v1, '&v2=', getbestvar1() , '&t2=', input$t1)
     
     #LRTest
     s[8] <- paste0( '', '&v1=', input$v1 )  
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
  
  getvar1 <- reactive({ 
    anychanged()
    q <- geturlquery()
    if (getwhichprogram() == 'E'){
      return(   "patient.reaction.reactionmeddrapt" )
    } else {
      return(input$v1)
    }
  })
  
  getprrvarname <- reactive({ 
    q <- geturlquery()
    if (getwhichprogram() != 'E'){
      #PRR table of reactions
      return(   "mdr_text.text" )
    } else {
      #PRR table of drugs
      return( paste0(input$v1, '.exact') )
    }
  })
  
  getexactvar1 <- reactive({ 
    q <- geturlquery()
    s <- getvar1()
    return(   paste0(s, ".exact") )
  })
  
  getbestvar1 <- function(){
    exact <-   ( getdrugcounts()$exact)
    if (exact){
      return( getexactvar1() )
    } else {
      return( getvar1() )
    }
  }
  
  getbestterm1 <- function(quote=TRUE){
    exact <-   ( getdrugcounts()$exact)
    if (exact)
    { 
      s <- getterm1( session, quote = TRUE, upper=FALSE )
      s <- gsub(' ', '%20', s, fixed=TRUE)
      return( s )
    } else {
      return( getterm1( session , upper=FALSE) )
    }
  }
# Input SETTERS ====================================================================
  updatevars <- reactive({
    input$update
    closeAlert(session, 'erroralert')
    isolate( {
      updateTextInput(session, "t1", value=( input$drugname ) )
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
    updateNumericInput(session, "limit", value = q$limit)
    updateNumericInput(session, "limit2", value = q$limit)
    if( getwhich()== 'DEV'){
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
    updateRadioButtons( session, 'useexact', selected = q$useexact )
    return(q)
  })
  
  output$drugname <- renderText({ renderterm( getterm1( session, upper=FALSE ), 'Drug Name:') } )
  output$eventname <- renderText({ renderterm( getterm1( session, upper=FALSE ), 'Event Term:') } )
  output$limit <- renderText({ renderterm( getlimit( session ), 'Max # of Events:') } )
  output$start <- renderText({ renderterm( getstart( session ), 'Rank of first event:',
                              label2=paste( 'Analyzing counts with ranks from',
                                     getstart( session ) , 'to', getstart( session )+getlimit( session )-1  )) } )

  
# General Reactives ============================================================
    #************************************
    # Get Drug-Event Query
    #*********************

  
  # Only use the first value of limit rows
  getdrugcounts <- reactive({
    geturlquery()
    mylist <-  getcounts999( session, v= getexactvar1(), t= getterm1( session, quote = FALSE, upper=FALSE ), 
                                 count=getprrvarname(), exactrad = input$useexact, db='/device/' )
    mydfAll <- mylist$mydf
    mystopwords <- tm::stopwords()
    mydfAll <- mydfAll[which( !(mydfAll[,1] %in% mystopwords) ), ]
    start <- getstart( session )
    last <- min(getlimit( session ) + start - 1, nrow(  mydfAll ) )
    #If Not enough event terms to start at start, look at last limit values
    if( last < start )
    {
      start <- last - getlimit( session )
    }
    mydf <- mydfAll[ start:last,]
    return( list(mydf=mydf, mydfAll= mydfAll, myurl=mylist$myurl, excludeddf = mylist$excludeddf, exact = mylist$exact   ) )
  })  
  
  
  #Build table containing drug-event pairs
  getdrugcountstable <- reactive({
    geturlquery()
    mylist <- getdrugcounts()
    myurl <- mylist$myurl
    #mydf for limit terms
    mydf <- mylist$mydf
    #mydf for all terms
    mydfAll <- mylist$mydfAll
    mydfsource <- mydf
    mydfallsource <- mydfAll
    names <- c('v1','t1' ,'v2', 't2')
    values <- c(getbestvar1(), getbestterm1(), getprrvarname() )
    mydf[,2] <- numcoltohyper(mydf[ , 2], mydf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydfAll[,2] <- numcoltohyper(mydfAll[ , 2], mydfAll[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    if (getwhich()=='DEV')
      {
      mydf[,1] <- coltohyper(mydf[,1],  'E',
                           mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact') )
      mydfAll[,1] <- coltohyper(mydfAll[,1],  'E',
                             mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact') )
      } else {
        mydf[,1] <- coltohyper(mydf[,1],  'DEV',
                               mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact' ) )
        mydfAll[,1] <- coltohyper(mydfAll[,1],  'DEV',
                               mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact') )
      }
    return( list(mydf=mydf, myurl=(myurl), mydfsource = mydfsource, mydfAll=mydfAll, mydfallsource = mydfallsource  ) )
  })  
  
  
  #**************************
  # Concomitant drug table
  getcocounts <- reactive({
    geturlquery()
#     if ( is.null( getterm1( session ) ) ){
#       return(data.frame( c(paste('Please enter a', getsearchtype(), 'name'), '') ) )
#     }
    mylist <- getcounts999( session, v= getexactvar1(), t= getterm1( session, quote = FALSE, upper=FALSE ), 
                       count=getexactvar1(), exactrad = input$useexact, db='/device/' )
    myurl <- mylist$myurl
#    browser() 
    mydf <- mylist$mydf
    mydf <- mydf[!is.na(mydf[,2]), ]
    mydf <- mydf[which( !(mydf[,1] %in% tm::stopwords()) ), ]
    sourcedf <- mydf
#     if (getwhich() =='DEV'){
#       colname <- 'Drug Name'
#       if (input$v1 != 'patient.drug.medicinalproduct')
#         {
#         drugvar <- gsub( "patient.drug.","" , input$v1, fixed=TRUE)
#         drugvar <- paste0( "&v1=", drugvar )
#         medlinelinks <- coltohyper( paste0( '%22' , sourcedf[,1], '%22' ), 'L', 
#                                   mybaseurl = getcururl(), 
#                                   display= rep('L', nrow( sourcedf ) ), 
#                                   append= drugvar )
#         
#         drugvar <- paste0( "&v1=", input$v1 )
#         dashlinks <- coltohyper( paste0( '%22' , sourcedf[, 1 ], '%22' ), 'DA', 
#                                  mybaseurl = getcururl(), 
#                                  display= rep('DEV', nrow( sourcedf ) ), 
#                                  append= drugvar )
# #         mydf <- data.frame(D=dashlinks, L=medlinelinks, mydf)
# #         mynames <- c( 'D', 'L', colname, 'Count') 
#         }
#       else {
#         medlinelinks <- rep(' ', nrow( sourcedf ) )
#         mynames <- c('-', colname, 'Count') 
#       }
#     } else {
#       colname <- 'Preferred Term'
#       mynames <- c('M', colname, 'Count') 
#       medlinelinks <- makemedlinelink(sourcedf[,1], 'M')          
#       mydf <- data.frame(M=medlinelinks, mydf) 
#     }
#     names <- c('v1','t1', 'v2', 't2')
#     values <- c(getbestvar1(), getbestterm1(), getexactvar1() ) 
#     mydf[,'count'] <- numcoltohyper(mydf[ , 'count' ], mydf[ , 'term'], names, values, mybaseurl = getcururl(), addquotes=TRUE )
#     mydf[,'term'] <- coltohyper(mydf[,'term'], getwhich() , mybaseurl = getcururl(), 
#                           append= paste0( "&v1=", input$v1, "&useexact=", 'exact') )
 # names(mydf) <- mynames
   return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
  })    
  
  #Indication table
  getindcounts <- reactive({
    geturlquery()
    if ( is.null( getterm1( session, upper=FALSE ) ) ){
      
      return(data.frame( c(paste('Please enter a', getsearchtype(), 'name'), '') ) )
    }
    mylist <- getcounts999( session, v= getbestvar1(), t=getbestterm1(), 
                            count= paste0( 'patient.drug.drugindication', '.exact'), exactrad = input$useexact, db='/device/' )
    mydf <- mylist$mydf
    mydf <- mydf[!is.na(mydf[,2]), ]
    sourcedf <- mydf
    myurl <- mylist$myurl
    names <- c('v1','t1', 'v2', 't2')
    values <- c( getbestvar1(), getbestterm1(), paste0( 'patient.drug.drugindication', '.exact') )
    mydf[,2] <- numcoltohyper(mydf[ , 2], mydf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydf[,1] <- makemedlinelink(sourcedf[,1], mydf[,1])
    return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
  })   
  

#Get total counts in database for each event and Total reports in database
  gettotals<- reactive({
    geturlquery()
    v <- c( '_exists_', '_exists_' )
    t <- c( getprrvarname(), getbestvar1() )
    totalurl <- buildURL(v, t,  count='', limit=1, db= '/device/')
    totalreports <- fda_fetch_p( session, totalurl, flag=NULL) 
    total <- totalreports$meta$results$total
    v <- c( '_exists_', getbestvar1() )
    t <- c( getprrvarname(), getbestterm1() )
    totaldrugurl <- buildURL( v, t, count='', limit=1, db= '/device/')
    totaldrugreports <- fda_fetch_p( session, totaldrugurl, flag=paste( 'No Reports for',
                                    ifelse(getwhich()=='DEV', 'drug', 'event' ), getterm1( session, upper=FALSE ), '<br>' ) ) 
#     if ( length( totaldrugreports )==0 )
#       {
#       totaldrugurl <- buildURL( v= getvar1(), t=getterm1( session ), count='', limit=1)
# 
#       totaldrugreports <- fda_fetch_p( session, totaldrugurl, flag= paste( 'No Reports of Drug', getterm1( session ) ) )
#       }
    
    totaldrug <- totaldrugreports$meta$results$total
    
    adjust <- total/totaldrug
    out <- list(total=total, totaldrug=totaldrug, adjust=adjust, 
                totalurl=(totalurl), totaldrugurl=(totaldrugurl) )
  }) 

  #Calculate PRR and put in merged table
  getprr<- reactive({
    geturlquery()
    #    totals <- gettotals()
#    browser()
    comblist <- makecomb(session, getdrugcounts()$mydf, geteventtotals(), gettotals(), getsearchtype())
    comb <- comblist$comb
    if (length(comb) < 1)
    {
      tmp <- data.frame( Error=paste('No results for', input$useexact, getterm1(session, upper=FALSE), '.'),
                         count=0 )
      return( list( comb=tmp, sourcedf=tmp) )
    }
    ror <- comblist$ror
    if (getwhich() =='DEV'){ 
      names <- c('exactD', 'exactE','v1', 'term1','term2')
      values <- c(input$useexact , 'exact', getvar1(), gsub( '"', '', getbestterm1(), fixed=TRUE  ) )
#      browser()
      exacttext <- paste0(  '&exactD=', input$useexact , '&exactE=exact' )
      links <-getcpalinks(comb[ , 1], names, values, getcururl() )
      comb <- data.frame( M='M' , comb, links$dynprr, links$cpa,  ror)
#      print( names(comb) )
      sourcedf <- comb
      colname <- 'Preferred Term'
      iname <- 'M'
      medlinelinks <- makemedlinelink(sourcedf[,2], iname)
    } else { 
      names <- c('exactD', 'exactE','v2','term2', 'v1','term1')
      values <- c('exact', input$useexact, getvar1(), gsub( '"', '', getbestterm1(), fixed=TRUE  ), input$v1 )
      exacttext <- paste0(  '&exactD=exact', '&exactE=', input$useexact )
      links <-getcpalinks(comb[ , 1], names, values, getcururl(), appendtext =  exacttext )
      comb <- data.frame(D='D', M='L' , comb, links$dynprr, links$cpa,  ror)
      sourcedf <- comb
      colname <- 'Drug Name'
      iname <- c( 'D', 'L')
      if (input$v1 != 'patient.drug.medicinalproduct')
      {
        drugvarname <- gsub( "patient.drug.","" , input$v1 , fixed=TRUE)
        drugvar <- paste0( "&v1=", drugvarname)
        medlinelinks <- coltohyper( paste0( '%22' , sourcedf[, 'term' ], '%22' ), 'L', 
                                    mybaseurl = getcururl(), 
                                    display= rep(iname[2], nrow( sourcedf ) ), 
                                    append= drugvar )
        drugvar <- paste0( "&v1=", input$v1 )
        dashlinks <- coltohyper( paste0( '%22' , sourcedf[, 'term' ], '%22' ), 'DA', 
                                    mybaseurl = getcururl(), 
                                    display= rep(iname[1], nrow( sourcedf ) ), 
                                    append= drugvar )
       comb[,'D'] <- dashlinks
      }
      else {
        medlinelinks <- rep(' ', nrow( sourcedf ) )
      }
    }
    comb[,'M'] <- medlinelinks
    names <- c('v1','t1' ,'v2', 't2')
    values <- c(getbestvar1(), getbestterm1(), getprrvarname() )
    comb[,'count.x'] <- numcoltohyper(comb[ , 'count.x'], comb[ , 'term'], names, values, mybaseurl =getcururl(), addquotes=TRUE )
    names <- c('v1','t1' ,'v2', 't2')
    values <- c( '_exists_', getvar1()  , getprrvarname() )
    comb[, 'count.y' ] <- numcoltohyper(comb[ , 'count.y' ], comb[ , 'term'], names, values , mybaseurl = getcururl(), addquotes=TRUE)
    comb[,'term'] <- coltohyper( comb[,'term'], ifelse(getwhich()=='DEV', 'E', 'DEV' ), 
                            mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact') )
    comb <- comb[order(comb$prr, decreasing = TRUE),]
    row.names(comb)<- seq(1:nrow(comb))
   
    countname <- paste( 'Counts for', getterm1( session, upper=FALSE ))
    names(comb) <-  c( iname, colname,countname, 
                       'Counts for All Reports','PRR', 'RRR',  'a', 'b', 'c', 'd', 'Dynamic PRR', 'Change Point Analysis', 'ROR')
    keptcols <-  c( iname, colname,countname, 
                                    'Counts for All Reports', 'PRR', 'RRR', 'Dynamic PRR', 'Change Point Analysis', 'ROR')

    #    mydf <- mydf[, c(1:4, 7,8,9)]
    return( list( comb=comb[, keptcols], sourcedf=sourcedf) )
  })
  
  geteventtotalstable <- reactive({
    geturlquery()
    mydf <- geteventtotals()
    sourcedf <- mydf
    names <- c('v1','t1' ,'v2', 't2')
    values <- c('_exists_', getvar1( )  , getprrvarname() )
    mydf[,2] <- numcoltohyper(mydf[ , 2], mydf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydf[,1] <- coltohyper(mydf[,1], ifelse( getwhich()=='DEV', 'E', 'DEV'), 
                           mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact' ) )
#    print(head(mydf))
    return( list(mydf=mydf, sourcedf=sourcedf) )
  })  
  
geteventtotals <- reactive(
  {
  geturlquery()
  mydf <- getdrugcounts()$mydf
  if ( !is.data.frame(mydf) ) 
  {
    return(NA)
    }
  realterms <- mydf[,1]
  foundtermslist <- mydf[,1]
  foundtermslist <- paste('"', foundtermslist, '"', sep='')
  foundtermslist <- gsub(' ', '%20',foundtermslist, fixed=TRUE )
  
  all <- data.frame(term=rep(URL='u', 'a', length(foundtermslist)), count=0L,  stringsAsFactors = FALSE)
  for (i in seq_along(foundtermslist))
    {
    eventvar <- gsub('.exact', '', getprrvarname(), fixed=TRUE)
#    myv <- c('_exists_', eventvar)
    myv <- c('_exists_', getprrvarname() )
    myt <- c( getvar1(),  foundtermslist[[i]]  )
#    cururl <- buildURL(v= myv, t=myt, count= getprrvarname(), limit=1)
    cururl <- buildURL(v= myv, t=myt, limit=1, whichkey=i%%2, db= '/device/')
 #   print(cururl)
#    all_events2 <- getcounts999( session, v= myv, t=myt, count= getprrvarname(), limit=1, counter=i )      
    all_events2 <- fda_fetch_p( session, cururl, message= i )
#    Sys.sleep( .25 )
    all[i, 'URL'] <- removekey( makelink( cururl ) )
    all[i, 'term'] <- realterms[[i]]
    curcount <- all_events2$meta$results$total
    if( is.null( curcount ) )
    {
      curcount <- NA
    }
    all[i, 'count'] <- curcount
    }
  return(all) 
} )
 #end calculations


##Tables ================================
output$specifieddrug <- renderTable({ 
  tableout(mydf = getdrugcountstable()$mydf,  
           mynames = c('Term', paste( 'Counts for', getterm1( session, upper=FALSE ) ) ),
           error = paste( 'No results for', getterm1( session, upper=FALSE ) ) )
},  sanitize.text.function = function(x) x)

output$indquery <- renderTable({ 
  tableout(mydf = getindcounts()$mydf, mynames = c('Indication',  'Counts' ),
           error = paste( 'No results for', getterm1( session, upper=FALSE ) ) )
}, sanitize.text.function = function(x) x)

output$coquery <- renderTable({  
  tableout(mydf = getcocounts()$mydf,  mynames = NULL,
           error = paste( 'No', getsearchtype(), 'for', getterm1( session, upper=FALSE ) ))
}, sanitize.text.function = function(x) x)

output$all <- renderTable({  
  tableout(mydf = geteventtotalstable()$mydf, 
           mynames = c('Term', paste( 'Counts for All Reports'), 'Query' ),
           error = paste( 'No events for', getsearchtype(), getterm1( session, upper=FALSE ) ) 
           )
}, sanitize.text.function = function(x) x)

output$prr <- renderTable({  
  getcururl()
  if (getterm1( session, upper=FALSE )=="") {
    return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, Count=0, PRR=0, ROR=0))
  } else {
    tableout(mydf = getprr()$comb,  
             mynames = NULL,
             error = paste('No records for', getterm1( session, upper=FALSE ))
    )
  }
},  sanitize.text.function = function(x) x)

output$coqueryE <- renderTable({  
  tableout(mydf = getdrugcountstable()$mydfAll,  
           mynames = c('Term', paste( 'Counts for', getterm1( session, upper=FALSE ) ) ),
           error = paste( 'No Events for', getterm1( session, upper=FALSE ) )
  )
}, sanitize.text.function = function(x) x)

output$coqueryEex <- renderTable({  
  tableout(mydf = getdrugcounts()$excludeddf,  
#           mynames = c( "Terms that contain '^' or ' ' ' can't be analyzed and are excluded", 'count' ),
           error = paste( 'No Events for', getterm1( session, upper=FALSE ) )
  )
}, sanitize.text.function = function(x) x)

#Plots========================================================
output$cloudquery <- renderPlot({  
  cloudout(getdrugcountstable()$mydfsource, paste('Terms in Reports That Contain', getterm1( session, upper=FALSE ) ))
}, height=900, width=900 )

output$cloudcoquery <- renderPlot({  
  cloudout( getcocounts()$sourcedf, 
            paste('Events in Reports That Contain', getterm1( session, upper=FALSE ) ) )
  
}, height=900, width=900 )
output$cloudcoqueryE <- renderPlot({ 
  cloudout( getdrugcountstable()$mydfallsource, 
            paste('Events in Reports That Contain', getterm1( session, upper=FALSE ) ) )
  
}, height=900, width=900 )
output$cloudindquery <- renderPlot({  
  cloudout( getindcounts()$sourcedf, 
            paste('Events in Reports That Contain', getterm1( session, upper=FALSE ) ) )
}, height=1000, width=1000)

output$cloudall <- renderPlot({  
  cloudout(geteventtotalstable()$sourcedf, 
           paste('Events in Reports That Contain', getterm1( sessio, upper=FALSEn ) ) ) 
}, height=900, width=900)

output$cloudprr <- renderPlot({  
  mydf <- getprr()$sourcedf
if( getwhich()=='DEV')
  {
  mydf <- data.frame(mydf[,2], mydf[,'prr']*10)
} else {
  mydf <- data.frame(mydf[,3], mydf[,'prr']*10)
}
  cloudout(mydf, paste('PRR for Events in Reports That Contain', getterm1( session, upper=FALSE ) ) )  
}, height=900, width=900)

# Text================================================================

output$prrtitle <- renderText({ 
  return('<h4>Reporting Ratios</h4><br>')
})


output$querytitle <- renderText({ 
  return( paste('<h4>Counts for', getterm1( session, upper=FALSE ), '</h4><br>') )
})

output$cotitleE <- renderText({ 
  return( paste('<h4>Most common events for', getterm1( session, upper=FALSE ), '</h4><br>') )
})

output$alltitle <- renderText({ 
  return( ('<h4>Counts for Entire Database</h4><br>') )
})


output$cotitle <- renderText({ 
  return( ( paste0('<h4>Most Common ', getsearchtype() , 's In Selected Reports</h4><br>') ) )
})
# Query outputs =======================================================
output$querycotext <- renderText({ 
  l <- getcocounts()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})

output$querycotextE <- renderText({ 
  l <- getdrugcountstable()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})
output$querytext <- renderText({ 
  l <- getdrugcounts()
  return( 
    paste( '<b>Query:</b>', removekey( makelink(l['myurl']) ) , 
           '<br>' ) )
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
    paste( '<b>Total reports with', getsearchtype(), getterm1( session, upper=FALSE ) , 'in database:</b>', prettyNum( l['totaldrug'], big.mark=',' ), '<br>') )
})

output$alltext <- renderText({ 
  l <- gettotals()
  paste( '<b>Total reports with value for', getbestvar1() ,'in database:</b>', prettyNum(l['total'], big.mark=',' ), '(meta.results.total)<br>')
})

output$indtitle <- renderText({ 
  return( ( paste0('<h4>Most Common Indications In Selected Reports</h4><br>') ) )
})

output$queryindtext <- renderText({ 
  l <- getindcounts()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})

output$date1 <- renderText({ 
  l <- getdaterangedeviceAE()
  paste( '<b>', l[3] ,'from', as.Date(l[1],  "%Y%m%d")  ,'to', as.Date(l[2],  "%Y%m%d"), '</b>')
})
# URL Stuff =====
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
    return( makeapplinks(  getcururl() )  )
  }) 
})