
popcoquery <- function()
{
  text <- 'Frequency table for drugs found in selected reports. Drug name is linked to LRT results for drug. \"L\" is linked to SPL labels for drug in openFDA. \"D\" is linked to a dashboard display for the drug.'
  head <- 'Concomitant Medications' 
  return( c(head=head, text=text) )
}
#*****************************************************
shinyServer(function(input, output, session) {
  
  mywait <- 0.5
  
# Getters ======  

  
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
     
   } else {
     #Dashboard
     s[1] <- paste0( '', '&v1=', input$v1 )
     
     #PRR for a Drug
     s[2] <- paste0( '', '&v1=', input$v1 )
     
     #PRR for an Event
     s[3] <- paste0( input$t1, '&v1=', input$v1 )
     
     #Dynamic PRR
     s[4] <- paste0( '' , '&v1=', input$v1, '&v2=', getbestterm1var(), '&t2=', input$t1 )
     
     #CPA
     s[5] <- paste0( '' , '&v1=', input$v1, '&v2=', getbestterm1var(), '&t2=', input$t1 )
     
     #Reportview
     s[6] <- paste0( '', '&v1=', input$v1, '&v2=', getbestterm1var() , '&t2=', input$t1 )
     
     #labelview
     s[7] <- paste0( '', '&v1=', input$v1, '&v2=', getbestterm1var() , '&t2=', input$t1)
     
     #LRTest
     s[8] <- paste0( input$t1, '&v1=', input$v1, gettimeappend() )
     
     #LRTestE
     s[9] <- paste0( '', '&v1=', input$v1 , gettimeappend())  
     }
   return(s)
 }
#   getterm1 <- reactive({ 
#     q <- geturlquery()
#     s <- toupper( input$t1 )
#     if  (is.null(s) | s=="" ) {
#       return("")
#     }
#     names <- s
#     names <- paste0(names, collapse=' ')
#     return(names)
#   })
  
  getquotedterm1 <- reactive({ 
    q <- geturlquery()
    s <- toupper( input$t1 )
    if  (is.null(s) | s =="" ) {
      return("")
    }
    s <- gsub(' ', '%20', s, fixed=TRUE)
    s <- paste0('%22', s, '%22')
    s <- paste0(s, collapse=' ')
    return(s)
  })

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
  
  getterm1var <- reactive({ 
    q <- geturlquery()
    anychanged()
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
  
  gettimeappend <- reactive({
    geturlquery()
    mytime <- getstartend()
    s <- paste0('&start=', mytime[1] , '&end=', mytime[2] )
    return( s )
  })
  
  getexactterm1var <- reactive({ 
    q <- geturlquery()
    s <- getterm1var()
    return(   paste0(s, ".exact") )
  })
  
  getbestterm1var <- function(){
    exact <-   ( getdrugcounts999()$exact)
    if (exact){
      return( getexactterm1var() )
    } else {
      return( getterm1var() )
    }
  }
  
  getbestterm1 <- function(quote=TRUE){
    exact <-   ( getdrugcounts999()$exact)
    if (exact)
    {
      return( getquotedterm1() )
    } else {
      return( getterm1(session) )
    }
  }
  
  gettimevar <- function(){
    return ('receivedate')
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
  
#   getlimit <- reactive({ 
#     q <- geturlquery()
#     return(input$limit)
#   })
#   
#   getstart <- reactive({ 
#     q <- geturlquery()
#     return(input$start)
#   })
  
  updatevars <- reactive({
    input$update
    isolate( {
      updateTextInput(session, "t1", value=( input$drugname ) )
      updateNumericInput(session, "limit", value= ( input$limit2 ) )
      updateNumericInput(session, "start", value= ( input$start2 ) )
      updateNumericInput(session, "numsims", value= ( input$numsims2 ) )
    })
  })
  
  
  anychanged <- reactive({
    a <- input$t1
    b <- input$v1
    c <- input$useexact
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
  
  #end getters
# Queries =======
  
  output$drugname <- renderText({
    s <- getterm1(session)
    if(s == '') {
      s <- 'None'
    }
    out <- paste( '<br><b>Drug Name:<i>', s, '</i></b><br><br>' )
    return(out)
  })
  
  output$eventname <- renderText({
    s <- getterm1(session)
    if(s == '') {
      s <- 'None'
    }
    out <- paste( '<b>Event Term:<i>', s, '</i></b><br><br>' )
    return(out)
  })
  
  
  output$limit <- renderText({ renderterm( getlimit( session ), 'Limit Analysis to', 'most frequent events.') } )
  output$start <- renderText({ 
    startfont <- '<i><font color="dodgerblue" size="4">'
    endfont <- '</font></i>'
    renderterm( getstart( session ), 'Start analysis at ranked frequency count # ',
                label2=paste( '<br>Analyzing counts with ranked frequencies from',
                              startfont, getstart( session ) , endfont,
                              'to', 
                              startfont, getstart( session )+getlimit( session )-1, endfont  ) ) 
  } )
  
  output$numsims <- renderText({ renderterm( getnumsims( session ), 'Number of simulations:', '' ) } )
  
#   output$limit <- renderText({
#     s <- getlimit( session )
#     if(s == '') {
#       s <- 'None'
#     }
#     out <- paste( '<b>Max # of Events:<i>', s, '</i></b><br><br>' )
#     return(out)
#   })
#   
#   output$start <- renderText({
#     s <- getstart( session )
#     mylist <-  getdrugcounts999( )
#     mydf <- mylist$mydf
#     counts <- mydf$count
#     if(s == '') {
#       s <- 'None'
#     }
#     out <- paste( '<b>Rank of first event:<i>', s, '</i></b><br>
#                   Analyzing counts with ranks from', s , 'to',s+getlimit( session )-1 ,'<br>' )
#     return(out)
#   })
#************************************
# Get Drug-Event Query
#*********************
  
  getdrugcounts999 <- reactive({

    if ( is.null( getterm1(session) ) ){
      return(data.frame( c( paste('Please enter a', getsearchtype(), 'name') , '') ) )
    }
    geturlquery()
    v <- c( '_exists_', getexactterm1var(), gettimevar() )
    t <- c( input$v1, getterm1( session, quote = TRUE ), gettimerange() )
    mylist <-  getcounts999( session, v= v, t= t , 
                             count=getprrvarname(), exactrad = input$useexact )
    
    return( list(mydf=mylist$mydf, myurl=mylist$myurl, excludeddf = mylist$excludeddf, exact = mylist$exact   ) )
  })    
  
#============================  
#getdrugcounts
# Get counts of events or reports for a specified drug  
  getdrugcounts <- reactive({
    #
    geturlquery()
#    mylist <-  getdrugcounts999()
   mylist <-  getdrugcounts999(  )
#How many events in first 999?
    totalevents <- sum(mylist$mydf$count )
    #    print(totalevents)
# How many are we evaluting? 
    start <- getstart( session )
    last <- min(getlimit( session ) + start - 1, nrow(  mylist$mydf ) )
    #If Not enough event terms to start at start, look at last limit values
    if( last < start )
    {
      start <- last - getlimit( session )
    }
    #Create two copies of events to study that only contain terms to analyze
    #mydfE will have counts based on events
    #mydfR will have counts based on reports
    mydfE <- mylist$mydf[start:last,]
    mydfR <- mylist$mydf[start:last,]
      s <- mydfR$term
      s<- paste0('%22', s, '%22', collapse='+')
      v <- c( '_exists_', getbestterm1var() , getprrvarname(), gettimevar() )
      t <- c( input$v1, getbestterm1() , s,  gettimerange() )
      myurl2 <- buildURL(v= v, t=t, addplus=FALSE )
#     myquery <- fda_fetch_p( session, myurl2)
#      mydfR <- rbind( mydfR, c('Other', as.numeric( gettotals()$totaldrug  - myquery$meta$results$total ) ) )
      mydfR <- rbind( mydfR, c('Other', as.numeric( totalevents  - sum(mydfE$count ) ) ) )
      mydfR[,2] <- as.numeric(mydfR[,2])
      mydfE <- rbind(mydfE, c('Other', as.numeric( totalevents  - sum(mydfE$count) ) ) )
      mydfE[,2] <- as.numeric(mydfE[,2])
    return( list(mydfE=mydfE, mydfR = mydfR, myurl=mylist$myurl  ) )
  })  
  
  getdownload <- function(tabname, skip=1, limit=100)
  {  
    mydf <- prrsource()
    outdf <- prr()
    outdf[[2]] <- mydf[['term']]
    return( outdf[, 2:5 ] ) 
  }
  
  #Build table containing drug-event pairs
  getdrugcountstable <- reactive({
    geturlquery()
    mydf <- getdrugcounts()
    myurl <- mydf$myurl
    mydf <- mydf$mydfE
    mydfsource <- mydf
#     names <- c('v1','t1' ,'v2', 't2')
#     values <- c(getbestterm1var(), getbestterm1(), getprrvarname() )
    names <- c('v1','t1' ,'v3', 't3', 'v2', 't2' )
    values <- c(getbestterm1var(), getbestterm1(), gettimevar(), gettimerange(),  getprrvarname() )
    mydf[,2] <- numcoltohyper(mydf[ , 2], mydf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    if (getwhich()=='D')
      {
      mydf[,1] <- coltohyper(mydf[,1],  'LRE',
                           mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact',  gettimeappend()) )
      } else {
        mydf[,1] <- coltohyper(mydf[,1],  'LR',
                               mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend() ) )
      }
    return( list(mydfE=mydf, myurl=(myurl), mydfsource = mydfsource  ) )
  })  
  
  
  getcodruglist <- reactive({
    v <- c('_exists_', getbestterm1var(), gettimevar() )
    t <- c( input$v1, getbestterm1(), gettimerange())
    myurl <- buildURL( v, t, 
                       count= getexactterm1var(), limit=1000 )
    mydf <- fda_fetch_p( session, myurl)
    mydf <- mydf$result[1:1000,]
    mydf <- mydf[!is.na(mydf[,2]), ]
    mydf <- data.frame(mydf, cumsum= cumsum(mydf[,2]))
    return( list( mydf=mydf, myurl=myurl) )
  })
  
  getcoeventlist <- reactive({
    v <- c('_exists_', getbestterm1var(), gettimevar() )
    t <- c( input$v1, getbestterm1(), gettimerange())
    myurl <- buildURL( v, t, 
                       count= getprrvarname(), limit=1000 )
    mydf <- fda_fetch_p( session, myurl)
    mydf <- mydf$result[1:1000,]
    mydf <- mydf[!is.na(mydf[,2]), ]
    mydf <- data.frame(mydf, cumsum= cumsum(mydf[,2]))
    return( list( mydf=mydf, myurl=myurl) )
  })
  
  
  getalleventlist <- reactive({
    v <- c('_exists_', '_exists_', '_exists_', gettimevar())
    t <- c( input$v1, getterm1var(),"patient.reaction.reactionmeddrapt", gettimerange())
    myurl <- buildURL( v, t, 
                       count= "patient.reaction.reactionmeddrapt.exact", limit=1000 )
    mydf <- fda_fetch_p( session, myurl)
    mydf <- mydf$result[1:1000,]
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
    if ( is.null( getterm1(session) ) ){
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
      linkapp <- 'LR'
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
        mynames <- c( 'D', 'L', colname, 'Count', 'Cum Sum') 
      }
      else {
        medlinelinks <- rep(' ', nrow( sourcedf ) )
        mynames <- c('-', colname, 'Count', 'Cum Sum') 
      }
      #Event Table
    } else {
      linkapp <- 'LRE'
      colname <- 'Preferred Term'
      mynames <- c('M', colname, 'Count', 'Cum Sum') 
      medlinelinks <- makemedlinelink(sourcedf[,1], 'M')          
      mydf <- data.frame(M=medlinelinks, mydf) 
    }
#     names <- c('v1','t1')
#     values <- c(getbestterm1var() ) 
    names <- c('v1','v3', 't3','t1'  )
    values <- c(getbestterm1var(), gettimevar(), gettimerange() )
    mydf[,'count'] <- numcoltohyper(mydf[ , 'count' ], mydf[ , 'term'], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydf[,'term'] <- coltohyper(mydf[,'term'], linkapp , mybaseurl = getcururl(), 
                                append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend()) )
    names(mydf) <- mynames
    return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
  }   
getindcounts <- reactive({
    geturlquery()
    if ( is.null( getterm1(session) ) ){
      return(data.frame( c(paste('Please enter a', getsearchtype(), 'name'), '') ) )
    }
  
  v <- c('_exists_', getbestterm1var(), gettimevar())
  t <- c( input$v1, getbestterm1(), gettimerange() )
    myurl <- buildURL( v= v, t=t, 
                       count= paste0( 'patient.drug.drugindication', '.exact'), limit=1000)
    mydf <- fda_fetch_p( session, myurl)
    mydf <- mydf$result[1:1000,]
    mydf <- mydf[!is.na(mydf[,2]), ]
    mydf <- data.frame(mydf, cumsum= cumsum(mydf[,2]))
    sourcedf <- mydf
#    medlinelinks <- makemedlinelink(sourcedf[,1], '?')
#     names <- c('v1','t1', 'v2', 't2')
#     values <- c( getbestterm1var(), getbestterm1(), paste0( 'patient.drug.drugindication', '.exact') )
    names <- c('v1','t1' ,'v3', 't3', 'v2', 't2'  )
    values <- c(getbestterm1var(), getbestterm1(), gettimevar(), gettimerange(), paste0( 'patient.drug.drugindication', '.exact') )
    mydf[,2] <- numcoltohyper(mydf[ , 2], mydf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydf[,1] <- makemedlinelink(sourcedf[,1], mydf[,1])
    
    return( list( mydf=mydf, myurl=(myurl), sourcedf=sourcedf ) )
  })   
  

#Get total counts in database for each event and Total reports in database
  gettotals<- reactive({
    geturlquery()
    
    
    v <- c('_exists_', '_exists_', '_exists_', gettimevar() )
    t <- c( input$v1, getprrvarname(), getbestterm1var(), gettimerange() )
    totalurl <- buildURL(v, t,  count='', limit=1)
    totalreports <- fda_fetch_p( session, totalurl)    
    total <- totalreports$meta$results$total
    
    v <- c( '_exists_', '_exists_', getbestterm1var(), gettimevar() )
    t <- c( input$v1, getprrvarname(), getbestterm1(), gettimerange() )
    totaldrugurl <- buildURL( v, t, count='', limit=1)
    totaldrugreports <- fda_fetch_p( session, totaldrugurl)    
#     if ( length( totaldrugreports )==0 )
#       {
#       totaldrugurl <- buildURL( v= getterm1var(), t=getterm1(session), count='', limit=1)
# 
#       totaldrugreports <- fda_fetch_p( session, totaldrugurl)
#       }
    
    totaldrug <- totaldrugreports$meta$results$total
    
    adjust <- total/totaldrug
    out <- list(total=total, totaldrug=totaldrug, adjust=adjust, 
                totalurl=(totalurl), totaldrugurl=(totaldrugurl) )
  }) 
  #end queries
# Calculations =========
  #Calculate PRR and put in merged table
  getprr <- reactive({
    geturlquery()
    drug1_event <- getdrugcounts()$mydfE
#    drug2_event <- getdrugcounts()$mydfR
    allevent <- geteventtotals()$alleventsdf
    alldrug <- geteventtotals()$allreportsdf
#    print( alldrug)
    if ( !is.data.frame(drug1_event) )  {
      return(data.frame(Term=paste( 'No events for', getsearchtype(),  getterm1(session) ), Count=0 , Count=0,Count2=0, PRR='prr'))
      }
    totals <- gettotals()
    mytitle <- 'Calculating Statistics'
    createAlert(session, 'alert', 'simalert',
                title=mytitle, content = 'This may take a while.', dismiss = FALSE)
    comb <- merge(drug1_event, allevent[, c('term', 'count')], by.x='term', by.y='term')
#     save(comb, file=paste0(getterm1( session ), '_LRT.RData'))
#     save(totals, file=paste0(getterm1( session ), '_LRT_totals.RData'))
#    comb2 <- merge(drug2_event, alldrug[, c('term', 'count')], by.x='term', by.y='term')
#    print(totals)
    if (getwhich()=='D')
      {
#       #Total number of reports
#       rn.. <- totals$total
#       #Total reporst for drug j
#       rn.j <- totals$totaldrug
#       #Total reports for DE combination
#       rnij <-  comb2$count.x
#       #Total report forevent i
#       rni. <- comb2$count.y
#      print(rn..)
      
      
      #Total number of Events
      alleventdf <- getalleventlist()$mydf
      allevents <- sum(alleventdf$count)
      n.. <- sum(alleventdf$count)
      #Total reporst for drug j
      n.j <- sum(comb$count.x)
      n.. <- sum(alleventdf$count)
      #Total reports for DE combination
      nij <-  comb$count.x
      #Total report for event i
      ni. <- comb$count.y
      

      pi. <- ni./n..
#      rpi. <- rni./rn..
      a <- nij
      b <- ni. - nij
      c <- n.j - nij
      d <- n.. - ni. - n.j + nij
#       PRRE <- prre( n.., ni., n.j, nij )
#       PRRD <- prrd( rn.., rni., rn.j, rnij )
#       LLR <- LLR( n.., ni., n.j, nij )
#       LLR[PRRE < 1] <- 0       
#Total number of Events
      alleventdf <- getalleventlist()$mydf
      allevents <- sum(alleventdf$count)
      mystats <- calcLRTstats(totals, comb, NULL, allevents)
      comb <- data.frame(comb, sigcol="NS",  LLR=mystats$LLRE, rrr=mystats$RR, stringsAsFactors = FALSE)
#      comb2 <- data.frame(comb2, sigcol="NS",  LLR=mystats$LLRR, rrr=mystats$PRRD, stringsAsFactors = FALSE)
      
      } 
    comb <- data.frame(comb, a, b, c, d, pi., nij,  n.j, ni.,  n..)
#    comb2 <- data.frame(comb2, a, b, c, d, rpi., rnij,  rn.j, rni.,  rn..)
#    print(  comb2[, 'rn..'])
    if (getwhich() =='D'){ 
      names <- c('v1', 'term1','term2')
      values <- c(getterm1var(), gsub( '"', '', getbestterm1(), fixed=TRUE  ) )
      cpa <- numcoltohyper( paste(comb[ , 1], 'CPA'), comb[ , 1], names, values, type='C', 
                            mybaseurl =getcururl() )
      
      dynprr <- numcoltohyper( paste(comb[ , 1], 'PRR'), comb[ , 1], names, values, type='P',
                               mybaseurl =getcururl() )
      comb <- data.frame( M='M' , comb, dynprr, cpa,  mystats$RR)
#      comb2 <- data.frame( M='M' , comb2, dynprr, cpa,  mystats$RR)
#      print(  comb2[, 'rn..'])
      comb<- data.frame(comb,
                        LLR= mystats$LLRE,
                        RR= mystats$RR,
                        PRRD= mystats$PRRD
      )
#       comb2 <- data.frame(comb2,
#                         LLR= mystats$LLRE,
#                         RR= mystats$RR,
#                         PRRD= mystats$PRRD
#      )
#      print(  comb2[, 'rn..'])
#      print( names(comb) )
      sourcedf <- comb
#      sourcedf2 <- comb2
      colname <- 'Preferred Term'
      iname <- 'M'
      medlinelinks <- makemedlinelink(sourcedf[,2], iname)
    }
    comb[,'M'] <- medlinelinks
#    comb2[,'M'] <- medlinelinks
#     names <- c('v1','t1' ,'v2', 't2')
#     values <- c(getbestterm1var(), getbestterm1(), getprrvarname() )
    names <- c('v1','t1' ,'v3', 't3', 'v2', 't2' )
    values <- c(getbestterm1var(), getbestterm1(), gettimevar(), gettimerange(),  getprrvarname() )
#    comb[,'count.x'] <- numcoltohyper(comb[ , 'count.x'], comb[ , 'term'], names, values, mybaseurl =getcururl(), addquotes=TRUE )
#     names <- c('v1','t1' ,'v2', 't2')
#     values <- c('_exists_', getterm1var()  , getprrvarname() )
    names <- c('v1','t1' ,'v3', 't3', 'v2', 't2' )
    values <- c( '_exists_' , getterm1var(), gettimevar(), gettimerange(),  getprrvarname() )
#    comb[, 'count.y' ] <- numcoltohyper(comb[ , 'count.y' ], comb[ , 'term'], names, values , mybaseurl = getcururl(), addquotes=TRUE)
    comb[,'term'] <- coltohyper( comb[,'term'], 'LRE', 
                            mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend()) )
    
    comb <- comb[order(comb$LLR, decreasing = TRUE),]
    sourcedf <- sourcedf[order(sourcedf$LLR, decreasing = TRUE),]
    row.names(comb)<- seq(1:nrow(comb))
#    row.names(comb2)<- seq(1:nrow(comb2))
    countname <- paste( 'nij for', getterm1(session))
#    print(head(comb2))
    names(comb) <-  c( iname, colname, countname, 
                       'ni.', "Significant?", 'LLR',
                       'RR',  'a', 'b', 'c', 'd', 'pi.',
                       'nij',  'n.j', 'ni.',  'n..',
                       'Dynamic PRR', 'Change Point Analysis', 'PRR2', 'LLR', 'PRRE', 'PRRD' )
#     names(comb2) <-  c( iname, colname, countname, 
#                        'ni.', "Significant?", 'LLR',
#                        'PRR',  'a', 'b', 'c', 'd', 'pi.',
#                        'nij',  'n.j', 'ni.',  'n..',
#                        'Dynamic PRR', 'Change Point Analysis', 'PRR2', 'LLR', 'PRRE', 'PRRD' )
#    print(  comb2[, 'n..'])
 #   print((comb2$pri.))
    keptcols1 <-  c( iname, colname, "Significant?", 'LLR',
                    'RR', 'nij' )
#     keptcols2 <-  c( iname, colname, "Significant?", 'LLR',
#                      'PRR' )
    #    mydf <- mydf[, c(1:4, 7,8,9)]
#      print(comb[, keptcols])
#      print(comb2[, keptcols])
    
    closeAlert(session, 'simalert')
    
    numsims <- getnumsims(session)
    mycritval <- getCritVal2(session, numsims, comb$n.j[1], comb$ni., comb$n..[1], comb$pi., .95)
    critval05 <- mycritval$critval
#     mycritval2 <- getCritVal2(numsims, comb2$n.j[1], comb2$ni., comb2$n..[1], comb2$pi., .95)
#     critval052 <- mycritval2$critval
    comb[ comb$LLR > critval05  , "Significant?"] <- "p < 0.05"
 #   comb2[ comb2$LLR > critval052  , "Significant?"] <- "p < 0.05"
#    comb[, 'nij'] <- prettyNum(comb[, 'nij'] , big.mark=',', drop0trailing = TRUE)
    comb[, 'n.j'] <- prettyNum(comb[, 'n.j'] , big.mark=',', drop0trailing = TRUE)
    comb[, 'ni.'] <- prettyNum(comb[, 'ni.'] , big.mark=',', drop0trailing = TRUE)
    comb[, 'n..'] <- prettyNum(comb[, 'n..'] , big.mark=',', drop0trailing = TRUE)
#     comb2[, 'nij'] <- prettyNum(comb2[, 'nij'] , big.mark=',', drop0trailing = TRUE)
#     comb2[, 'n.j'] <- prettyNum(comb2[, 'n.j'] , big.mark=',', drop0trailing = TRUE)
#     comb2[, 'ni.'] <- prettyNum(comb2[, 'ni.'] , big.mark=',', drop0trailing = TRUE)
#     comb2[, 'n..'] <- prettyNum(comb2[, 'n..'] , big.mark=',', drop0trailing = TRUE)
 #   print(comb2[, keptcols])
    return( list( comb=comb[, keptcols1], sourcedf=sourcedf, 
                  maxLRT = max(comb$LLR), critval=mycritval, numsims=numsims, colname=colname ) )
  })
  
  geteventtotalstable <- reactive({
    geturlquery()
    mydf <- geteventtotals()$alleventsdf
    mydf2 <- geteventtotals()$allreportsdf
    sourcedf <- mydf
    names <- c('v1','t1' ,'v3', 't3', 'v2', 't2'  )
    values <- c('_exists_', getterm1var() , gettimevar(), gettimerange(), getprrvarname() )
    mydf[,2] <- numcoltohyper(mydf[ , 2], mydf[ , 1], names, values, mybaseurl = getcururl(), addquotes=TRUE )
    mydf[,1] <- coltohyper(mydf[,1], ifelse( getwhich()=='D', 'LRE', 'LR'), 
                           mybaseurl = getcururl(), append= paste0( "&v1=", input$v1, "&useexact=", 'exact', gettimeappend()) )
    return( list(mydf=mydf, mydf2= mydf2 , sourcedf=sourcedf) )
  })  
  
geteventtotals <- reactive(
  {
  geturlquery()
  starttime <- Sys.time()
  mydf <- getdrugcounts()$mydfE
  if ( !is.data.frame(mydf) ) {return(NA)}
  realterms <- mydf[,1]
  foundtermslist <- mydf[,1]
  foundtermslist <- paste('"', foundtermslist, '"', sep='')
  foundtermslist <- gsub(' ', '%20',foundtermslist, fixed=TRUE )
  myrows <- length(foundtermslist)
#   if (getlimit( session ) < 35)
#     {
#      myrows <- myrows +1 
#     }
  allevent <- data.frame(term=rep(URL='u', 'a', myrows ), count=0L,  stringsAsFactors = FALSE)
  allreport <- data.frame(term=rep(URL='u', 'a', myrows ), count=0L,  stringsAsFactors = FALSE)
  for (i in seq_along(foundtermslist))
    {
    if ( realterms[[i]] =='Other')
    {
      
      #Find exact # without any of the events
#       s <- foundtermslist
#       s<- paste0( s,  collapse='+')
#       v <- c( getbestterm1var() , getprrvarname() )
#       t <- c( getbestterm1() ,s)
#       myurl2 <- buildURL(v= v, t=t, addplus=FALSE )
#       myquery2 <- fda_fetch_p( session, myurl2, message=paste(i, 'Fetching Data'))
#      print( myquery2$meta$results$total )
#       myurl3 <- buildURL(v= v[2], t=t[2], addplus=FALSE )
#       myquery3 <- fda_fetch_p( session, myurl3)
#      print( myquery3$meta$results$total )
#      allreport[i, 'URL'] <- removekey( makelink( myurl3 ) )
      allreport[i, 'URL'] <- 'removekey( makelink( myurl3 )'
      allreport[i, 'term'] <- 'Other'
#      allreport[i, 'count'] <- as.numeric(gettotals()$total - myquery3$meta$results$total) 
      allreport[i, 'count'] <- as.numeric(gettotals()$total - sum(allevent$count) ) 
      # End find exact      
      allevent[i, 'URL'] <- '-' 
      allevent[i, 'term'] <- 'Other'
      mydf <- getalleventlist()$mydf
      allevent[i, 'count'] <- as.numeric( sum(mydf$count) - sum(allevent$count) )
    } else {
      eventvar <- gsub('.exact', '', getprrvarname(), fixed=TRUE)
#      myv <- c('_exists_', eventvar)
      myv <- c('_exists_', '_exists_', getprrvarname(), gettimevar() )
      myt <- c( input$v1, getterm1var(),  foundtermslist[[i]], gettimerange()  )
#      cururl <- buildURL(v= myv, t=myt, count= getprrvarname(), limit=1)
      cururl <- buildURL(v= myv, t=myt, limit=1)
#Sys.sleep( .25 )
      all_events2 <- fda_fetch_p( session, cururl, message= i )
      allevent[i, 'URL'] <- removekey( makelink( cururl ) )
      allevent[i, 'term'] <- realterms[[i]]
      allevent[i, 'count'] <- all_events2$meta$results$total
#       allreport[i, 'URL'] <- removekey( makelink( cururl ) )
#       allreport[i, 'term'] <- realterms[[i]]
#       allreport[i, 'count'] <- all_events2$meta$results$total
      }
    }
  
  print( as.double(Sys.time()-starttime ) )
  return( list( alleventsdf = allevent, allreportsdf = allreport )  )
} )
 #end calculations

# setters ======
#Tab 1: LRT Results based on Total Events
prr <- reactive({  
  if (getterm1(session)=="") {
    return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, Count=0, PRR=0, ROR=0))
  }
  return( checkdf( getprr()[['comb']], getsearchtype() ) )
})

prrnohyper <- reactive({  
  myprr <- prr()
  mysource <- prrsource()
  myprr[,2] <- mysource[,2]
  out <- myprr[, -1]
  return(out)
})

prrsource <- reactive({  
  if (getterm1(session)=="") {
    return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, Count=0, PRR=0, ROR=0))
  }
  return( checkdf( getprr()[['sourcedf']], getsearchtype() ) )
})
output$prr <- renderTable({   
  prr()
},  sanitize.text.function = function(x) x)

getcloudprrscale <- reactive({
  scale <- getcloud_try(getprr()$sourcedf, name=2, freq='LLR',  scale1=9 )
  return(scale)
})
cloudprr <- function(){ 
  scale <- getcloudprrscale()
  cloudplot( mydf = getprr()$sourcedf, session, scale1=9, 
             name=2, freq = 'LLR', stattext='LLR' )
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
    x <- mydf[, 'nij']
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
  maxLRT <- getprr()$maxLRT
  critval <- getprr()$critval$critval
  return( paste( '<h4>Reporting Ratios</h4>',
                 'Critical Value =',  round( critval, 2),
                 '<br># of Simulations =',  getprr()$numsims
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
    truehist(vals , breaks=mybreaks, 
             main="Histogram of Simulated Distribution of LLR", 
             xlab='Loglikelihood Ratio', xaxt='n' )
    text(mycrit,.3, paste('Rejection Region, LLR >', round(mycrit, 2) ), pos=4, col='red')
    smallbreaks <- seq(0, max(mybreaks), 1)
    
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

#Tab 3 AnalyzedEventCountsforDrug

AnalyzedEventCountsforDrug <- reactive(
  {
    mydf <- getdrugcountstable()$mydfE
    checkdf(mydf, getterm1(session),
            names=c('Term', paste( 'Counts for', getterm1(session)) ),
            changecell = c( row=nrow(mydf), column='Term', val='Other (# of Events)' ) )
  }
)

AnalyzedEventCountsforDrugnohyper <- reactive(
  {
    mydf <- AnalyzedEventCountsforDrug()
    mysource <- getdrugcountstable()$mydfsource
    mydf[, 1] <- mysource[,1]
    mydf[, 2] <- mysource[,2]
    return(mydf)
  }
)

output$AnalyzedEventCountsforDrug <- renderTable({  
  AnalyzedEventCountsforDrug()
},  sanitize.text.function = function(x) x)

cloudAnalyzedEventCountsforDrug <- function(){
  scale <- getcloudprrscale()
    cloudplot( mydf = getdrugcountstable()$mydfsource, session)
  }
output$cloudAnalyzedEventCountsforDrug <- renderPlot({  
  cloudAnalyzedEventCountsforDrug()
}, height=900, width=900 )

queryAnalyzedEventCountsforDrug <- reactive(
  { 
    l <- getdrugcounts()
    return( 
      paste( '<b>Query:</b>', removekey( makelink(l['myurl']) ) , 
             '<br>' ) )
  }
)
output$queryAnalyzedEventCountsforDrug <- renderText({ 
  queryAnalyzedEventCountsforDrug()
}
)

titleAnalyzedEventCountsforDrug <- reactive(
  { 
    return( paste('<h4>Counts for', getterm1(session), '</h4>') )
  }
)
output$titleAnalyzedEventCountsforDrug <- renderText({ 
  titleAnalyzedEventCountsforDrug()
})

alldrugtextAnalyzedEventCountsforDrug  <- reactive({ 
  l <- gettotals()
  return( 
    paste( '<b>Total reports with', getsearchtype(), getterm1(session) , 'in database:</b>', prettyNum( l['totaldrug'], big.mark=',' ), '<br>') )
})
output$alldrugtextAnalyzedEventCountsforDrug  <- renderText({ 
  alldrugtextAnalyzedEventCountsforDrug()
})


alldrugqueryAnalyzedEventCountsforDrug <- reactive({ 
  l <- gettotals()
  mysum <- sum( getalleventlist( )$mydf$count )
  return( 
    paste( '<b>Query:</b>', removekey( makelink(l['totaldrugurl']) ) ) ) 
})
output$alldrugqueryAnalyzedEventCountsforDrug <- renderText({ 
  alldrugqueryAnalyzedEventCountsforDrug()
})

##
#Tab 4 Analyzed Event Counts for All Drugs

queryalltext <- function(){ 
  l <- gettotals()
  paste( '<b>Query:</b>', removekey( makelink(l['totalurl'] ) ), '<br>')
}
output$queryalltext <- renderText({ 
  queryalltext()
})



alltext <- function(){ 
  l <- gettotals()
  paste( '<b>Total reports with value for', getbestterm1var() ,'in database:</b>', prettyNum(l['total'], big.mark=',' ), '(meta.results.total)<br>')
}
output$alltext <- renderText({ 
  alltext()
})

alltitle <- function(){ 
  return( ('<h4>Counts for Entire Database</h4><br>') )
}
output$alltitle <- renderText({ 
  alltitle()
})

all <- function(){  
  all <- geteventtotalstable()$mydf
  checkdf(all, paste(getsearchtype(), getterm1(session)), 
          names=c('Term', paste( 'Counts for All Reports'), 'Query' ), 
          changecell=c( row=nrow(all), column='Term', val='Other (# of Events)' ) )
}

allnohyper <- function(){  
  all <- geteventtotalstable()$mydf
  mysource <- geteventtotalstable()$sourcedf
  all[, 1] <- mysource[, 1]
  all[, 2] <- mysource[, 2]
  all[, 3] <- mysource[, 3]
  return (all)
}

output$all <- renderTable({  
  all()
}, sanitize.text.function = function(x) x)



cloudall <- function(){  
  scale <- getcloudprrscale()
  cloudplot( mydf = geteventtotalstable()$sourcedf, session,
             termtype='Events', intype='Drug' )
}
output$cloudall <- renderPlot({  
  cloudall()
}, height=900, width=900)

#End Tab 4 Analyzed Event Counts for All Drugs
##


##
# Tab 5: Counts For Drugs In Selected Reports
output$cotitle <- renderText({ 
  return( ( paste0('<h4>Most Common Drugs In Selected Reports</h4><br>') ) )
})


coquery <- function(){  
  #if ( getterm1() =='') {return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, URL=''))}
  codrugs <- getcocountsD()$mydf
  checkdf(codrugs, getterm1(session))
} 
output$coquery <- renderTable({  
  coquery()
}, sanitize.text.function = function(x) x)  

querycotext <- function(){ 
  l <- getcocounts()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
}
output$querycotext <- renderText({ 
  querycotext()
})

cloudcoquery <- function(){  
  cloudplot( mydf = getcocountsD()$sourcedf, session,termtype='Drugs', intype='Events' )
}
output$cloudcoquery <- renderPlot({  
  cloudcoquery()
}, height=900, width=900 )

##
# End Tab 5: Counts For Drugs In Selected Reports

##
# Tab 6 Event Counts for Drug

cotitleE <- function(){ 
  return( ( paste0('<h4>Most Common Events In Selected Reports</h4><br>') ) )
}
output$cotitleE <- renderText({ 
  return( ( paste0('<h4>Most Common Events In Selected Reports</h4><br>') ) )
})

querycotextE <- function(){ 
  l <- getcocountsE()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
}
output$querycotextE <- renderText({ 
  l <- getcocountsE()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})

coqueryE <- function(){  
  #if ( getterm1() =='') {return(data.frame(Term=paste('Please enter a', getsearchtype(), 'name'), Count=0, URL=''))}
  codrugs <- getcocountsE()$mydf
  checkdf(codrugs, getterm1(session))
}
output$coqueryE <- renderTable({  
  coqueryE()
}, sanitize.text.function = function(x) x)

coqueryEex <- function(){  
  codrugs <- getdrugcounts999()$excludeddf
  checkdf(codrugs, getterm1(session))
}
output$coqueryEex <- renderTable({  
  coqueryEex()
}, sanitize.text.function = function(x) x)

cloudcoqueryE <- function(){  
  cloudplot( mydf = getcocountsE()$sourcedf, session,termtype='Events', intype='Drug' )
}
output$cloudcoqueryE <- renderPlot({  
  cloudcoqueryE()
}, height=900, width=900 )
##
# End Tab 6 Event Counts for Drug

output$cotitleA <- renderText({ 
  return( ( paste0('<h4>Most Common Events In All Reports</h4><br>') ) )
})





output$querycotextA <- renderText({ 
  l <- getalleventlist()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})




output$coqueryA <- renderTable({  
  codrugs <-  getalleventlist( )$mydf
  checkdf(codrugs, getterm1(session))
}, sanitize.text.function = function(x) x)




output$cloudcoqueryA <- renderPlot({  
  cloudplot( mydf = getalleventlist()$mydf, session, termtype='Events', intype='Drug' )
}, height=900, width=900 )
#***************Events in report
output$indtitle <- renderText({ 
  return( ( paste0('<h4>Most Common Indications In Selected Reports</h4><br>') ) )
})

output$queryindtext <- renderText({ 
  l <- getindcounts()
  paste( '<b>Query:</b>', removekey( makelink( l['myurl'] ) ), '<br>')
})

output$indquery <- renderTable({  
  codinds <- getindcounts()$mydf
  checkdf(codinds, getterm1(session), names=c('Indication',  'Counts', 'Cum Sum' ))
}, sanitize.text.function = function(x) x)

output$cloudindquery <- renderPlot({  
  cloudplot( mydf = getindcounts()$sourcedf, session, termtype='Indication', intype='Drug' )
}, height=1000, width=1000)






output$date1 <- renderText({ 
  l <- getdaterange()
  paste( '<b>Reports from', as.Date(l[1],  "%Y%m%d")  ,'to', as.Date(l[2],  "%Y%m%d"), '</b>')
})

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
  updateDateRangeInput(session, 'daterange', start = q$start, end = q$end)
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
  
})