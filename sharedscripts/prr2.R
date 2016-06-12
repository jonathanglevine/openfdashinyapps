# popprr <- function()
# {
#   text <- "Some prr stuff"
#   head <- "prr head"
#   return( c(head=head, text=text) )
# }
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
     s[4] <- paste0( '' , '&v1=', input$v1, '&v2=', getbestvar1(), '&t2=', input$t1 )
     
     #CPA
     s[5] <- paste0( '' , '&v1=', input$v1, '&v2=', getbestvar1(), '&t2=', input$t1 )
     
     #Reportview
     s[6] <- paste0( '', '&v1=', input$v1, '&v2=', getbestvar1() , '&t2=', input$t1 )
     
     #labelview
     s[7] <- paste0( '', '&v1=', input$v1, '&v2=', getbestvar1() , '&t2=', input$t1)
     
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
      return(   "patient.reaction.reactionmeddrapt.exact" )
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
      s <- getterm1( session, quote = TRUE )
      s <- gsub(' ', '%20', s, fixed=TRUE)
      return( s )
    } else {
      return( getterm1( session ) )
    }
  }
  
  gettimevar <- function(){
    return ('receiptdate')
  }
  
  gettimerange <- reactive({
    geturlquery()
    mydates <- getstartend()
    start <- mydates[1]
    end <-  mydates[2]
    timerange <- paste0('[', start, '+TO+', end, ']')
    return(timerange)
  })
  
  getstartend <- reactive({
    geturlquery()
    start <- input$daterange[1]
    end <- input$daterange[2]
    return( c(start, end))
  })
  
  gettimeappend <- reactive({
    geturlquery()
    mytime <- getstartend()
   s <- paste0('&start=', mytime[1] , '&end=', mytime[2] )
    return( s )
  })
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
    updateRadioButtons( session, 'useexact', selected = q$useexact )
    updateDateRangeInput(session, 'daterange', start = q$start, end = q$end)
    updateTabsetPanel(session, 'maintabs', selected=q$curtab)
    return(q)
  })
  
  
  output$drugname <- renderText({ 
    s <- getterm1description( input$useexact, getterm1( session ) )
    renderterm( s, 'Drug Name:') 
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
    #************************************
    # Get Drug-Event Query
    #*********************

  
  # Only use the first value of limit rows
  getdrugcounts <- reactive({
    geturlquery()
    v <- c('_exists_' , getexactvar1(), gettimevar() )
    t <- c(  getexactvar1() ,getterm1( session, quote = TRUE ), gettimerange() )
    mylist <-  getcounts999( session, v= v, t= t, 
                                 count=getprrvarname(), exactrad = input$useexact )
    mydfAll <- mylist$mydf
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
    names <- c('v1','t1' ,'v3', 't3', 'v2', 't2' )
    values <- c(getbestvar1(), getbestterm1(), gettimevar(), gettimerange(),  getprrvarname() )
    mydf[,2] <- numcoltohyper(mydf[ , 2], mydf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydfAll[,2] <- numcoltohyper(mydfAll[ , 2], mydfAll[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
#    browser()
    if (getwhich()=='D')
      {
      mydf[,1] <- coltohyper(mydf[,1],  'E',
                               mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend() )  )
      mydfAll[,1] <- coltohyper(mydfAll[,1],  'E',
                               mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend() ) )
      } else {
      mydf[,1] <- coltohyper(mydf[,1],  'D',
                               mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend() ) )
      mydfAll[,1] <- coltohyper(mydfAll[,1],  'D',
                               mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend() ) )
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
    
    v <- c( getbestvar1(), gettimevar() )
    t <- c(  getbestterm1(), gettimerange() )
#     mylist <- getcounts999( session, v= getexactvar1(), t= getterm1( session, quote = FALSE ), 
#                             count=getexactvar1(), exactrad = input$useexact )
    mylist <- getcounts999( session, v= v, t= t, count=getexactvar1(), exactrad = input$useexact )
    if (length(mylist)==0)
      {
      return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
      }
    myurl <- mylist$myurl
    mydf <- mylist$mydf
    mydf <- mydf[!is.na(mydf[,2]), ]
    sourcedf <- mydf
    if (length( mydf )==0)
    {
      return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
    }
    if (getwhich() =='D'){
      colname <- 'Drug Name'
      if (input$v1 != 'patient.drug.medicinalproduct')
        {
        drugvar <- gsub( "patient.drug.","" , input$v1, fixed=TRUE)
        drugvar <- paste0( "&v1=", drugvar )
        medlinelinks <- coltohyper( paste0( '%22' , sourcedf[,1], '%22' ), 'L', 
                                  mybaseurl = getcururl(), 
                                  display= rep('Label', nrow( sourcedf ) ), 
                                  append= drugvar )
        
        drugvar <- paste0( "&v1=", input$v1 )
        dashlinks <- coltohyper( paste0( '%22' , sourcedf[, 1 ], '%22' ), 'DA', 
                                 mybaseurl = getcururl(), 
                                 display= rep('Dashboard', nrow( sourcedf ) ), 
                                 append= drugvar )
        mydf <- data.frame(D=dashlinks, L=medlinelinks, mydf)
        mynames <- c( 'D', 'L', colname, 'Count') 
        }
      else {
        medlinelinks <- rep(' ', nrow( sourcedf ) )
        mydf <- data.frame(L=medlinelinks, mydf)
        mynames <- c('-', colname, 'Count') 
      }
    } else {
      colname <- 'Preferred Term'
      mynames <- c('M', colname, 'Count') 
      medlinelinks <- makemedlinelink(sourcedf[,1], 'Definition')          
      mydf <- data.frame(M=medlinelinks, mydf) 
    }
    names <- c('v1','t1','v3', 't3', 'v2', 't2')
    values <- c(getbestvar1(), getbestterm1(), gettimevar(), gettimerange(), getexactvar1() ) 
    mydf[,'count'] <- numcoltohyper(mydf[ , 'count' ], mydf[ , 'term'], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydf[,'term'] <- coltohyper(mydf[,'term'], getwhich() , mybaseurl = getcururl(), 
                           append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend()) )
  names(mydf) <- mynames
   return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
  })    
  
  #Indication table
  getindcounts <- reactive({
    geturlquery()
    if ( is.null( getterm1( session ) ) ){
      
      return(data.frame( c(paste('Please enter a', getsearchtype(), 'name'), '') ) )
    }
    v <- c( getbestvar1(), gettimevar() )
    t <- c( getbestterm1(), gettimerange() )
#     mylist <- getcounts999( session, v= getbestvar1(), t=getbestterm1(), 
#                             count= paste0( 'patient.drug.drugindication', '.exact'), exactrad = input$useexact )
    mylist <- getcounts999( session, v= v, t=t, count= paste0( 'patient.drug.drugindication', '.exact'), exactrad = input$useexact )
    mydf <- mylist$mydf
    mydf <- mydf[!is.na(mydf[,2]), ]
    sourcedf <- mydf
    myurl <- mylist$myurl
    if (length( mydf )==0)
    {
      return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
    }
    names <- c('v1','t1','v3', 't3', 'v2', 't2')
    values <- c( getbestvar1(), getbestterm1(), gettimevar(), gettimerange(), paste0( 'patient.drug.drugindication', '.exact') )
    mydf[,2] <- numcoltohyper(mydf[ , 2], mydf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydf[,1] <- makemedlinelink(sourcedf[,1], mydf[,1])
    return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
  })   
  

#Get total counts in database for each event and Total reports in database
  gettotals<- reactive({
    geturlquery()
    v <- c( '_exists_', '_exists_', gettimevar() )
    t <- c( getprrvarname(), getbestvar1(), gettimerange() )
    totalurl <- buildURL(v, t,  count='', limit=1)
    totalreports <- fda_fetch_p( session, totalurl, flag=NULL) 
    total <- totalreports$meta$results$total
    v <- c( '_exists_', '_exists_', getbestvar1(), gettimevar() )
    t <- c( getbestvar1(), getprrvarname(), getbestterm1(), gettimerange() )
    totaldrugurl <- buildURL( v, t, count='', limit=1)
    totaldrugreports <- fda_fetch_p( session, totaldrugurl, flag=paste( 'No Reports for',
                                    ifelse(getwhich()=='D', 'drug', 'event' ), getterm1( session ), '<br>' ) ) 
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
  getprr <- reactive({
    geturlquery()
    #    totals <- gettotals()
#    browser()
    comblist <- makecomb(session, getdrugcounts()$mydf, geteventtotals(), gettotals(), getsearchtype())
    comb <- comblist$comb
    if (length(comb) < 1)
    {
      tmp <- data.frame( Error=paste('No results for', input$useexact, getterm1(session), '.'),
                         count=0 )
      return( list( comb=tmp, sourcedf=tmp) )
    }
#    ror <- comblist$ror
    if (getwhich() =='D'){ 
      names <- c('exactD', 'exactE','v1', 'term1','term2')
      values <- c(input$useexact , 'exact', getvar1(), gsub( '"', '', getbestterm1(), fixed=TRUE  ) )
#      browser()
      exacttext <- paste0(  '&exactD=', input$useexact , '&exactE=exact' )
      links <-getcpalinks(comb[ , 1], names, values, getcururl() )
      comb <- data.frame( M='M' , comb, links$dynprr, links$cpa,  comb$ror, comb$nij)
#      print( names(comb) )
      sourcedf <- comb
      colname <- 'Preferred Term'
      iname <- 'Definition'
      medlinelinks <- makemedlinelink(sourcedf[,2], iname)
    } else { 
      names <- c('exactD', 'exactE','v2','term2', 'v1','term1')
      values <- c('exact', input$useexact, getvar1(), gsub( '"', '', getbestterm1(), fixed=TRUE  ), input$v1 )
      exacttext <- paste0(  '&exactD=exact', '&exactE=', input$useexact )
      links <-getcpalinks(comb[ , 1], names, values, getcururl(), appendtext =  exacttext )
      comb <- data.frame(D='D', M='L' , comb, links$dynprr, links$cpa,  comb$ror, comb$nij)
      sourcedf <- comb
      colname <- 'Drug Name'
      iname <- c( 'Dashboard', 'Label')
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
    names <- c('v1','t1','v3', 't3' ,'v2', 't2')
    values <- c(getbestvar1(), getbestterm1(), gettimevar(), gettimerange(), getprrvarname() )
    comb[,'count.x'] <- numcoltohyper(comb[ , 'count.x'], comb[ , 'term'], names, values, mybaseurl =getcururl(), addquotes=TRUE )
    names <- c('v1','t1','v3', 't3' ,'v2', 't2')
    values <- c( '_exists_', getvar1(), gettimevar(), gettimerange(), getprrvarname() )
    comb[, 'count.y' ] <- numcoltohyper(comb[ , 'count.y' ], comb[ , 'term'], names, values , mybaseurl = getcururl(), addquotes=TRUE)
    comb[,'term'] <- coltohyper( comb[,'term'], ifelse(getwhich()=='D', 'E', 'D' ), 
                            mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend()) )
#     comb <- comb[order(comb$prr, decreasing = TRUE),]
#     sourcedf <- sourcedf[order(sourcedf$prr, decreasing = TRUE),]
#     row.names(comb)<- seq(1:nrow(comb))
   
    countname <- paste( 'Counts for', getterm1( session ))
    names(comb) <-  c( iname, colname,countname, 
                       'Counts for All Reports','PRR', 'RRR',  'a', 'b', 'c', 'd', 'Dynamic PRR', 'Change Point Analysis', 'ROR', 'nij')
    keptcols <-  c( iname, colname,countname, 
                                    'Counts for All Reports', 'PRR',  'Dynamic PRR', 'Change Point Analysis', 'ROR', 'nij')

    #    mydf <- mydf[, c(1:4, 7,8,9)]
    return( list( comb=comb[, keptcols], sourcedf=sourcedf, countname=countname, colname=colname) )
  })
  
  geteventtotalstable <- reactive({
    geturlquery()
    mydf <- geteventtotals()
    sourcedf <- mydf
    names <- c('v1','t1','v3', 't3' ,'v2', 't2')
    values <- c('_exists_', getvar1( ), gettimevar(), gettimerange()  , getprrvarname() )
    mydf[,2] <- numcoltohyper(mydf[ , 2], mydf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydf[,1] <- coltohyper(mydf[,1], ifelse( getwhich()=='D', 'E', 'D'), 
                           mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend() ) )
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
    myv <- c('_exists_', getprrvarname(), '_exists_', gettimevar() )
    myt <- c( getbestvar1(),  foundtermslist[[i]], getprrvarname(), gettimerange()  )
#    cururl <- buildURL(v= myv, t=myt, count= getprrvarname(), limit=1)
    cururl <- buildURL(v= myv, t=myt, limit=1, whichkey=i%%2)
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

##
# tabPanel("PRR and ROR Results"

output$prrtitle <- renderText({ 
  geturlquery()
  return('<h4>Reporting Ratios: Results sorted by PRR</h4>')
})

prr <- reactive({  
  if (getterm1( session )=="") {
    return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, Count=0, PRR=0, ROR=0))
  } else {
    tableout(mydf = getprr()$comb,  
             mynames = NULL,
             error = paste('No records for', getterm1( session ))
    )
  }
} )
output$prr <- renderTable({  
 prr()
},  sanitize.text.function = function(x) x)

output$prr2 <- renderDataTable({  
  prr()
}, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '50', targets = c(1, 2) ) ) ),  escape = FALSE )


cloudprr <- reactive({  
  mydf <- getprr()$sourcedf
  if( getwhich()=='D')
  {
    mydf <- data.frame(mydf[,2], mydf[,'prr']*10)
  } else {
    mydf <- data.frame(mydf[,3], mydf[,'prr']*10)
  }
  cloudout(mydf, paste('PRR for Events in Reports That Contain', getterm1( session ) ) )  
})
output$cloudprr <- renderPlot({  
  cloudprr()  
}, height=900, width=900)

textplot <- reactive({ 
  if (getterm1( session )!="") {
    mylist <- getprr()
    mydf <- mylist$comb
    y <- mydf[,'PRR']
    x <- mydf[, 'nij']
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
  brushedPoints(mydf, input$plot_brush, yvar = "PRR", xvar = 'nij' )
},  sanitize.text.function = function(x) x)




##
# tabPanel("Analyzed Drug/Event Counts for Specified Event/Drug"  
output$alldrugtext <- renderText({ 
  l <- gettotals()
  return( 
    paste( '<b>Total reports with', getsearchtype(), getterm1( session ) , 'in database:</b>', prettyNum( l['totaldrug'], big.mark=',' ), '<br>') )
})
output$queryalldrugtext <- renderText({ 
  l <- gettotals()
  return( 
    paste( '<b>Query:</b>', removekey( makelink(l['totaldrugurl']) ) , '<br>') ) 
})

output$querytitle <- renderText({ 
  return( paste('<h4>Counts for', getterm1( session ), '</h4><br>') )
})

cloudquery <- reactive({  
  cloudout(getdrugcountstable()$mydfsource, paste('Terms in Reports That Contain', getterm1( session ) ))
})
output$cloudquery <- renderPlot({  
  cloudquery()
}, height=900, width=900 )

specifieddrug <- reactive({ 
  tableout(mydf = getdrugcountstable()$mydf,  
           mynames = c('Term', paste( 'Counts for', getterm1( session ) ) ),
           error = paste( 'No results for', getterm1( session ) ) )
})
output$specifieddrug <- renderTable({ 
  tableout(mydf = getdrugcountstable()$mydf,  
           mynames = c('Term', paste( 'Counts for', getterm1( session ) ) ),
           error = paste( 'No results for', getterm1( session ) ) )
},  height=120, sanitize.text.function = function(x) x)

output$specifieddrug2 <- renderDataTable({ 
  tableout(mydf = getdrugcountstable()$mydf,  
           mynames = c('Term', paste( 'Counts for', getterm1( session ) ) ),
           error = paste( 'No results for', getterm1( session ) ) )
},  escape=FALSE )

# tabPanel("Event Counts for All Drugs" 'alltext' 'queryalltext' 
#'alltitle'  'allquerytext' ,'cloudall', 'all'
output$alltext <- renderText({ 
  l <- gettotals()
  paste( '<b>Total reports with value for', getbestvar1() ,'in database:</b>', prettyNum(l['total'], big.mark=',' ), '(meta.results.total)<br>')
})
output$queryalltext <- renderText({ 
  l <- gettotals()
  paste( '<b>Query:</b>', removekey( makelink(l['totalurl'] ) ), '<br>')
})

output$alltitle <- renderText({ 
  return( ('<h4>Counts for Entire Database</h4><br>') )
})

cloudall <- reactive({  
  cloudout(geteventtotalstable()$sourcedf, 
           paste('Events in Reports That Contain', getterm1( session ) ) ) 
})
output$cloudall <- renderPlot({  
  cloudall()
}, height=900, width=900)

all <- renderTable({  
  tableout(mydf = geteventtotalstable()$mydf, 
           mynames = c('Term', paste( 'Counts for All Reports'), 'Query' ),
           error = paste( 'No events for', getsearchtype(), getterm1( session ) ) 
  )
})
output$all <- renderTable({  
  tableout(mydf = geteventtotalstable()$mydf, 
           mynames = c('Term', paste( 'Counts for All Reports'), 'Query' ),
           error = paste( 'No events for', getsearchtype(), getterm1( session ) ) 
  )
}, sanitize.text.function = function(x) x)

output$all2 <- renderDataTable({  
  tableout(mydf = geteventtotalstable()$mydf, 
           mynames = c('Term', paste( 'Counts for All Reports'), 'Query' ),
           error = paste( 'No events for', getsearchtype(), getterm1( session ) ) 
  )
}, escape=FALSE)


# tabPanel("Ranked Drug/Event Counts for Event/Drug 'cotextE' 'querycotextE'  'cotitleE',
# 'coquerytextE' ,'cloudcoqueryE', 'coqueryE'


output$querycotextE <- renderText({ 
  l <- getdrugcountstable()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})
output$cotitleE <- renderText({ 
  return( paste('<h4>Most common events for', getterm1( session ), '</h4><br>') )
})
output$cotitleD <- renderText({ 
  return( paste('<h4>Most common drugs for', getterm1( session ), '</h4><br>') )
})

cloudcoqueryE <- reactive({ 
  cloudout( getdrugcountstable()$mydfallsource, 
            paste('Events in Reports That Contain', getterm1( session ) ) )
  
})
output$cloudcoqueryE <- renderPlot({ 
  cloudcoqueryE()
  
}, height=900, width=900 )

coqueryE <- reactive({  
  out <- tableout(mydf = getdrugcountstable()$mydfAll,  
           mynames = c('Term', paste( 'Counts for', getterm1( session ) ) ),
           error = paste( 'No Events for', getterm1( session ) )
            )
  
#  browser()
  return(out)
})
output$coqueryE <- renderTable({  
  coqueryE()
}, sanitize.text.function = function(x) x)

output$coqueryE2 <- renderDataTable({  
  coqueryE()
}, escape=FALSE)

# tabPanel("Counts For Drugs In Selected Reports"
#            htmlOutput( 'cotext' ),
#            htmlOutput_p( 'querycotext' ,
#                          tt('gquery1'), tt('gquery2'),
#                          placement='bottom' )
#          ),
#          wellPanel(
#            htmlOutput( 'cotitle' )
#          ),
#          htmlOutput_p( 'coquerytext' ,
#                        tt('gquery1'), tt('gquery2'),
#                        placement='bottom' ),
#          wordcloudtabset('cloudcoquery', 'coquery'


output$querycotext <- renderText({ 
  l <- getcocounts()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})
output$cotitle <- renderText({ 
  return( ( paste0('<h4>Most Common ', getsearchtype() , 's In Selected Reports</h4><br>') ) )
})

output$coquery <- renderTable({  
  tableout(mydf = getcocounts()$mydf,  mynames = NULL,
           error = paste( 'No', getsearchtype(), 'for', getterm1( session ) ))
}, sanitize.text.function = function(x) x)


coquery2 <- reactive({  
  
  out <- tableout(mydf = getcocounts()$mydf,  mynames = NULL,
           error = paste( 'No', getsearchtype(), 'for', getterm1( session ) ))
  return(out)
} )

output$coquery2 <- renderDataTable({  
  coquery2()
},  escape=FALSE )

# tabPanel("Counts For Indications In Selected Reports"
#            htmlOutput( 'indtext' ),
#            htmlOutput_p( 'queryindtext' ,
#                          tt('gquery1'), tt('gquery2'),
#                          placement='bottom' )
#          ),
#          wellPanel(
#            htmlOutput( 'indtitle' )
#          ),
#          wordcloudtabset('cloudindquery', 'indquery'

##Tables ================================


output$indquery <- renderTable({ 
  tableout(mydf = getindcounts()$mydf, mynames = c('Indication',  'Counts' ),
           error = paste( 'No results for', getterm1( session ) ) )
}, sanitize.text.function = function(x) x)

output$indquery2 <- renderDataTable({ 
  tableout(mydf = getindcounts()$mydf, mynames = c('Indication',  'Counts' ),
           error = paste( 'No results for', getterm1( session ) ) )
},  escape=FALSE )




output$coqueryEex <- renderTable({  
  tableout(mydf = getdrugcounts()$excludeddf,  
#           mynames = c( "Terms that contain '^' or ' ' ' can't be analyzed and are excluded", 'count' ),
           error = paste( 'No Events for', getterm1( session ) )
  )
}, sanitize.text.function = function(x) x)


output$coqueryEex2 <- renderDataTable({  
  tableout(mydf = getdrugcounts()$excludeddf,  
           #           mynames = c( "Terms that contain '^' or ' ' ' can't be analyzed and are excluded", 'count' ),
           error = paste( 'No Events for', getterm1( session ) )
  )
}, escape=FALSE )

#Plots========================================================
# output$cloudquery <- renderPlot({  
#   cloudout(getdrugcountstable()$mydfsource, paste('Terms in Reports That Contain', getterm1( session ) ))
# }, height=900, width=900 )

output$cloudcoquery <- renderPlot({  
  cloudout( getcocounts()$sourcedf, 
            paste('Events in Reports That Contain', getterm1( session ) ) )
  
}, height=900, width=900 )

output$cloudindquery <- renderPlot({  
  cloudout( getindcounts()$sourcedf, 
            paste('Events in Reports That Contain', getterm1( session ) ) )
}, height=1000, width=1000)




# Text================================================================











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







output$indtitle <- renderText({ 
  return( ( paste0('<h4>Most Common Indications In Selected Reports</h4><br>') ) )
})

output$queryindtext <- renderText({ 
  l <- getindcounts()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})

output$date1 <- renderText({ 
  l <- getdaterange()
  paste( '<b>', l[3], 'from', as.Date(l[1],  "%Y%m%d")  ,'to', as.Date(l[2],  "%Y%m%d"), '</b>')
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
    s <- makeapplinks(  getcururl(), getqueryvars( 1 ) ) 
#    write(s, file='')
    return( makeapplinks(  getcururl(), getqueryvars( 1 ) )  )
  }) 
})