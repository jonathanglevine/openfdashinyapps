

require(RColorBrewer)
require(wordcloud)
require('curl')
require('httr')
require(stringi)
if (!require('openfda') ) {
  devtools::install_github("ropenhealth/openfda")
  library(openfda)
  print('loaded open FDA')
}

#Temporary fix for peer certificate error
#httr::set_config( config( ssl_verifypeer = 0L ) )

#require(GOsummaries)

SOURCEDIR <- '../sharedscripts/'
if (!file.exists( paste0( SOURCEDIR, 'key.r') ))
{
  SOURCEDIR <- 'sharedscripts/'
} else {
  #Temporary fix for peer certificate error
  httr::set_config( config( ssl_verifypeer = 0L ) )
#  print(SOURCEDIR)
}
source( paste0( SOURCEDIR, 'key.r') )

getcloud_try <- function(mydf, scale1=9, name=1, freq=2, title='Terms') {
#  mydf[, name] <- abbreviate(mydf[,name], 20)
#  layout(matrix(c(1,2), 2, 1, byrow = FALSE), heights=c(1,12 ) )
  par(mar=c(0,0,2,0))
  
#  plot(c(0,1),c(0,1),xlab='',type='n',  xaxt='n', yaxt='n',frame.plot=FALSE)
 # plot.new()
         
  
  #****************************************
  #* maketiff
#  if ( !is.null(mydf) & (1==2) )
#   {
#     filename <- paste0( mydf[2,1], mydf[1,1] ,'.tif')
#     print(filename)
#     filename <- gsub('%22', '', filename)
#     if ( nrow(mydf) >0 )
#     {
#       filename <- paste0( mydf[2,1], mydf[1,1] ,'.png')
#       print(filename)
# #      tiff(file = filename, res=600, compression='lzw',height=8, width=8, units="in",  bg = "transparent" )
#       png(file = filename, width = 4000, height = 4000, res=600, bg = "transparent" )
#     plotWordcloud(mydf[ , 1], mydf[,2],
#                 color=brewer.pal(8, "Dark2"))
#     dev.off()
#     }
#   }
  #end maketiff
mydf[ which( is.nan(mydf[,freq] ) ), freq] <- 1
options(warn=5)
curscale <- scale1
for (i in 1:(2*scale1-1))
{ 
#  print(curscale)
  out <-  try(  ( wordcloud(mydf[, name], mydf[,freq], max.words = 200 ,
                         color=rep('white', 8), random.order=FALSE, 
                         scale=c(curscale, .5) ) ) )
  if( class(out) == "try-error" )
  {   
    curscale <- curscale-.5
  } else {
    break()
  }
}
options(warn=0)
return( c(curscale, .5) )
}

getcloud <- function(mydf, scale1=9, name=1, freq=2, title='Terms', scale=NULL) {

  if ( is.null(scale) & nrow(mydf) > 1)
  {
    scale <- invisible(getcloud_try(mydf, scale1, name, freq, title))
  }
  par(mar=c(0,0,2,0))
  
  mylabel <- paste('Word Cloud for', title)
  mylabel <- title
  #text(.5, .5, labels=mylabel, cex=2)
  mydf[ which( is.nan(mydf[,freq] ) ), freq] <- 1
  if( nrow(mydf) > 1 )
    {
    wordcloud(mydf[, name], mydf[,freq], max.words = 200 ,
                color=brewer.pal(8, "Dark2"), random.order=FALSE, 
                scale=scale )
    }
  text(.5, 1, labels=mylabel, cex=1.5)

}
gettables <- function( tmp )
{
  mycols <- grep('_table', names(tmp), fixed=TRUE )
  outdf <- tmp[[1]]
  s <- ""
  for (i in seq_along(mycols))
  {
    for (j in seq_along( tmp[[ mycols[i] ]] ) )
    {
      if ( tmp[[ mycols[i] ]][[j]]!='None' )
      {
        s <- paste(s, '<br>', tmp[[ mycols[i] ]][[j]] )
      }
    }
  }
  if (nchar(s)==0)
  {
    return('<i>No tables in this section</i>')
  }
  return(  HTML(s) )
}

getwaittime <- function(session){ 
  if(session$clientData$url_hostname == '10.12.207.87')
  {
    return( 0)
  } else if(session$clientData$url_hostname == '127.0.0.1') {
    return (0.0)
  }
  return(0.0)
}

fda_fetch_p <- function(session, myurl, 
                        message = '', 
                        wait=0,
                        reps=0,
                        flag=NULL)
{
  prog <- ''
  dispurl <- removekey(myurl)
  starttime <- Sys.time()
  mytitle <- paste('Fetching Data', message )
  
  closeAlert(session, 'alert2')
  createAlert(session, 'alert', 'fetchalert',title=mytitle, content = substr(dispurl, 37,100), dismiss = FALSE)
#   if ( length(flag) < 1 )
#   { 
#     flag <- paste( 'No Reports for selected term<br>' )
#   }
  iter <- 1
  for (i in 1:10)
   { 
    out <-  try( fda_fetch( myurl ) )
    if( class(out) == "try-error" )
      {        
      closeAlert(session, 'erroralert')
      err <- out
      createAlert(session, 'alert2', 'erroralert', title=paste( '<font color="red">Error:', err,' </font>' ), 
                  content= flag )
      Sys.sleep(1)
    } else if( length(out$results)<1 )
        {  
        s <- GET(myurl)
        if( !is.null( httr::content(s)$error) )
        {
          closeAlert(session, 'erroralert')
          err <- ( httr::content(s) )
          createAlert(session, 'alert2', 'erroralert', title=paste( '<font color="red">Error:', err$error$message,' </font>' ), 
                      content= flag )
          break()
        }
        else
        {
          iter <- iter + 1
          print( paste('iteration', i, 'with empty df') )
          if (iter > 25)
          {
            break()
          }
        }
        #    out<- data.frame(term='error', count=NA)
      } else {
        closeAlert(session, 'erroralert')
        break
      }
  }

  #Sys.sleep( max(0, .31 - as.double(Sys.time()-starttime) ) )
  closeAlert(session, 'fetchalert')
  return(out)
}

makecomb <- function(session, mydf1, mydf2, totals, type, sortvar='prr' ){
  if ( !is.data.frame(mydf1) )  {
    return(data.frame(Term=paste( 'No events for', type,  getterm1( session ) ), Count=0 , Count=0,Count2=0, PRR='prr'))
  }
  comb <- merge(mydf1, mydf2[, c('term', 'count')], by.x='term', by.y='term')
  if (type=='Drug')
  {
    #x is # reports for DE, y is # reports for Events
#     num <- comb$count.x/totals$totaldrug
#     denom <- (comb$count.y-comb$count.x)/(totals$total-totals$totaldrug)
#     num2 <- comb$count.x/(totals$totaldrug-comb$count.x)
#     denom2 <- comb$count.y/(totals$total-comb$count.y)#Total reports for drug j
    n.j <- totals$totaldrug
    #Total reports for DE combination
    nij <-  comb$count.x
    num <- nij/n.j
    #Total reports forevent i
    ni. <- comb$count.y
    n.. <- totals$total
    denom <- ( ni.-nij )/(  n.. - n.j )
  } else {
    #x is # reports for DE, y is # reports for Drug
 #   num <- comb$count.x/comb$count.y
#    denom <- (totals$totaldrug-comb$count.x)/(totals$total-comb$count.y)
    
#    num2 <- comb$count.x/(comb$count.y - comb$count.x)
#    denom2 <-(totals$totaldrug)/(totals$total-totals$totaldrug)
    
    n.j <- comb$count.y
    #Total reports for DE combination
    nij <-  comb$count.x
    num <- nij/n.j
    ni. <- totals$totaldrug
    n.. <- totals$total
    denom <- ( ni.-nij )/(  n.. - n.j )
  }
#  ror <- num2/denom2
#  comb <- data.frame(comb, prr=num/denom, num, denom)
  prr <- prre( n.., ni., n.j, nij ) 
  rrr <- prrd( n.., ni., n.j, nij ) 
  ror <- ror( n.., ni., n.j, nij )
  llr <- LLR( n.., ni., n.j, nij )  
  comb <- data.frame(comb, prr=round( prr, digits = 2), ror=ror, nij, ni., n.j, n..)   
  comb <- comb[order(comb[, sortvar], decreasing = TRUE),]
  row.names(comb)<- seq(1:nrow(comb))
  return( list(comb=comb, ror=ror) )
}
getcpalinks <- function(column, names, values, mybaseurl, appendtext='')
  {
  cpa <- numcoltohyper( paste(column, 'CPA'), column, names, values, type='C', 
                        mybaseurl, append = appendtext)
  dynprr <- numcoltohyper( paste(column, 'PRR'), column, names, values, type='P',
                           mybaseurl, append = appendtext )
  return( list( cpa = cpa, dynprr = dynprr ) )
}

getdynprr_as_links <- function(column, names, values, mybaseurl, appendtext='')
{
  dynprr <- numcoltohyper( paste(column, 'PRR'), column, names, values, type='DPS', 
                        mybaseurl, append = appendtext)
  return( list( dynprr = dynprr ) )
}
#buildurl =======================================================
extractbaseurl <- function(myurl){
  myurl <- gsub('//', '/', myurl, fixed=TRUE )
  tmp <- strsplit(myurl,'?', fixed=TRUE)
  tmp1 <- tmp[[1]][1]
  tmp2 <- strsplit(tmp1,'/', fixed=TRUE)
  tmp3 <- tmp2[[1]][1:(length(tmp2[[1]])-1)]
  tmp4 <- paste0(tmp3, collapse='/' )
  tmp4 <- paste0(tmp4, '/') 
  return(tmp4)
}
getemptyapplist <- function()
{
  mynames <- c('DA', 'D', 'E', 'P', 'Z', 'R', 'L', 'LR', 'LRE', 'ENFD',
                  'AEDEV', 'ENFDEV', 'CLSDEV', '510', 'PMA', 'RLDEV', 'RCLDEV', 
                  'ENFFD', 'DAS', 'EAS', 'DPS', 'LRDAS', 'LREAS' )
  s <- vector(mode='character', length = length(mynames))
 names(s) <- mynames
  return(s)
}
makeapplinks <- function( cururl, append = '' )
{
  labels <- getemptyapplist()
  labels['DA'] <- '<h4>Drug Apps</h4><b>Dashboard-</b> Overview of reports for a drug'
  labels['D'] <- '<b>PRR for a Drug-</b> Calculate Proportional Reporting Rates for Common Events for a drug'
  labels['E'] <-  '<b>PRR for an Event-</b> Calculate Proportional Reporting Rates for Common Drugs that have a specified event'
  labels['P'] <-  '<b>Dynamic PRR-</b> Calculate Proportional Reporting Rates for a drug-event pair over time'
  labels['Z'] <- '<b>Change Point Analysis-</b> Change point analysis for a drug-event pair over time'
  labels['R'] <-  '<b>Adverse Event Browser-</b> View reports that meet search criteria'
  labels['L'] <-           '<b>Label Browser-</b> View labels that meet search criteria'
  labels['LR'] <-            '<b>Likelihood Ratio Test for Drug-</b> Calculate Likelihood Ratio Tests for Common Events for a drug'
  labels['LRE'] <-           '<b>Likelihood Ratio Test for Event-</b> Calculate Likelihood Ratio Tests for Common Drugs for an event'
  labels['ENFD'] <-           '<b>Drug Enforcement Report Browser-</b> View enforcement reports that meet search criteria'
              
  labels['AEDEV'] <-           '<hr><h4>Device Apps</h4><b>Adverse Event Browser-</b> View reports that meet search criteria'
  labels['ENFDEV'] <-          '<b>Device Enforcement Report Browser-</b> View labels that meet search criteria'
  labels['CLSDEV'] <-           '<b>Device Classification</b> View labels that meet search criteria'
  labels['510'] <-          '<b>510(k) Viewer-</b> View 510(k) Data'
  labels['PMA'] <-           '<b>PMA Viewer-</b> View PMA Data'
  labels['RLDEV'] <-           '<b>Registration and Listing Browser-</b> View Registration and Listing Data'
  labels['RCLDEV'] <-           '<b>Recall Browser-</b> View Recall Reports'
  labels['ENFFD'] <-            '<hr><h4>Food Apps</h4><b>Food Enforement Report Browser-</b> View reports that meet search criteria'
  
  labels['DAS'] <- '<h4>Drug Apps Using Suspect Drugs Only</h4><b>PRR for a Drug-</b> Calculate Proportional Reporting Rates for Common Events for a drug'
  labels['EAS'] <-  '<b>PRR for an Event-</b> Calculate Proportional Reporting Rates for Common Drugs that have a specified event'
  labels['DPS'] <-  '<b>Dynamic PRR-</b> Calculate Proportional Reporting Rates for a drug-event pair over time'
  labels['LRDAS'] <-            '<b>Likelihood Ratio Test for Drug-</b> Calculate Likelihood Ratio Tests for Common Events for a drug'
  labels['LREAS'] <-           '<b>Likelihood Ratio Test for Event-</b> Calculate Likelihood Ratio Tests for Common Drugs for an event'
  s <- names(labels)
  #s <- getbaseurl(s)
  out <- ''
  if ( max(append) == '')
  {
    addquery <- FALSE
  } else {
    addquery <- TRUE
  }
  for (i in seq_along(s))
  {
    out <- paste0(out, coltohyper(append[i], s[i] , mybaseurl = cururl, display=labels[i], makequery=addquery ), '<br>' )
  }
  return( out  )
}

getappname <- function(myurl){
  tmp <- strsplit(myurl,'?', fixed=TRUE)
  tmp1 <- tmp[[1]][1]
  tmp2 <- strsplit(tmp1,'/', fixed=TRUE)
  tmp3 <- tmp2[[1]][ length(tmp2[[1]]) ]
  return(tmp3)
}


removekey <- function(url) {
  mykey <- paste0('api_key=', getkey(),'&')
  s <-gsub(mykey, '', url, fixed=TRUE)
  return(s)
}

getkey <- function(){
  if( exists('getmykey'))
  {
   return( getmykey() ) 
  }
  else
  {
    return( '' )
  }
  
}

makelink <- function( s, s2 = NULL ) {
  if (is.null(s2))
  {
    s2 <- removekey( s[[1]] )
  }
  s <- gsub('"', '%22', s, fixed=TRUE)
  s <- gsub(' ', '%20', s, fixed=TRUE)
  s2 <- gsub('"', '%22', s2, fixed=TRUE)
  link <- paste0('<a href="', s, '" target="_blank">', s2, '</a>')
  return (link)
}

getbaseurl <- function( which, mycururl=NULL, appname= 'cpa' ){
  apps1 <-c('cpa', 'dynprr', 'RR_D', 'RR_E', 'reportview', 'dash', 'LRTest', '510kview')
  if (is.null(mycururl))
  {
    out <- getcururl()
  } else{
    out <- mycururl
  }
  if (which=='D'){
    s <- 'RR_D'
  } else if (which=='DAS'){
    s <- 'RR_D_Activesubstance'
  } else if(which == 'E' ) {
    s <- 'RR_E'
  } else if(which == 'EAS' ) {
    s <- 'RR_E_Activesubstance'
  }  else if(which == 'R' ){
    s <- 'reportview'
  } else if(which == 'P' ){
    s <- 'dynprr'
  } else if(which == 'DPS' ){
    s <- 'dynprr_Activesubstance'
  } else if(which == 'DA' ){
    s <- 'dash'
  } else if(which == 'L' ){
    s <- 'labelview'
  } else if(which == 'LR' ){
    s <- 'LRTest' 
  } else if (which=='LRDAS'){
    s <- 'LR_D_Activesubstancename'
  } else if(which == 'LRE' ){
    s <- 'LRTest_E' 
  } else if (which=='LREAS'){
    s <- 'LR_E_Activesubstancename'
  } else if(which == 'ENFD' ){
    s <- 'drugenforceview'
  }  else if(which == 'AEDEV' ){
      s <- 'devicereports'
  }  else if(which == 'ENFDEV' ){
    s <- 'deviceenforceview'
  }  else if(which == 'CLSDEV' ){
    s <- 'deviceclassview'
  }  else if(which == '510' ){
    s <- '510kview'
  }  else if(which == 'PMA' ){
    s <- 'PMAview'
  }  else if(which == 'RLDEV' ){
    s <- 'devicereglist'
  }  else if(which == 'RCLDEV' ){
    s <- 'devicerecallview'
  }  else if(which == 'ENFFD' ){
    s <- 'foodrecallview'
  } else {
    s <- 'ChangePoint'
  }
  if ( !( appname %in% apps1) )
  {
    s <- paste0(s, 2)
  }
  out <-paste0( out, s, '/' )
  return(out)
}


makeexturl <- function(base, names, values){
#  values <- gsub('NA', "", values, fixed=TRUE)
  values[ which(is.na(values))] <- ''
  names2 <- URLencode(names, reserved = TRUE)
  values2 <- URLencode(values, reserved = TRUE)
  s1 <- paste0( '&', names2)
  s2 <- paste0( '=', values2)
  # print(s2)
  allpairs2 <- paste0(s1, s2, collapse='')
  allpairs2 <- sub('&', "?", allpairs2, fixed=TRUE)
  allpairs2 <- paste0(base, allpairs2)
  
  
  s1 <- paste0( '&', names)
  s2 <- paste0( '=', values)
  allpairs <- paste0(s1, s2, collapse='')
  allpairs <- sub('&', "?", allpairs, fixed=TRUE)
  allpairs <- paste0(base, allpairs)
  return( allpairs )
}

makehyper <- function(base , names, values, display, makequery=TRUE){
  if (makequery)
  {
    url <- makeexturl(base, names, values)
  }
  else {
    url <- base
  }
  
  url <- gsub('"','%22', url, fixed=TRUE)
  hyper <- paste0('<a href="', url, '" target="_blank">', display, '</a>')
  return(hyper)
}

makemedlinelink <- function(s, s2) {
  medstring <- paste0("http://www.merriam-webster.com/medlineplus/", s )
  out <- makelink(medstring, s2)
  return(out)
}

coltohyper <- function(col, which, mybaseurl=NULL, display=NULL, makequery=TRUE, append=''){
  baseurl <- getbaseurl(which, mybaseurl)
  name <- 't1'
  if (is.null(display)){
    display <- col
  }
  out <- vector(mode='character', length=length(col))
  for (i in seq_along(col))
  {
    col[i] <- gsub('"', '', col[i])
    out[i] <- makehyper(baseurl , name, paste0('' ,col[i] , append  ), display[i], makequery=makequery)
  }
  
  return(out)
}
numcoltohyper <- function( dispcol, valcol, names, values, type='R', mybaseurl=NULL, addquotes=FALSE, append='' ){
  baseurl <- getbaseurl( type, mybaseurl )
  out <- vector(mode='character', length=length(dispcol))
  #   if (addquotes)
  #     {
  #     values <-  gsub('"', '', values , fixed=TRUE)
  #     values <- paste0('%22' ,values , '%22' )
  #     }
  for (i in seq_along(dispcol))
  {
    if (addquotes)
    {
      valcol[i] <-  gsub('"', '', valcol[i] , fixed=TRUE)
      valcol[i] <- paste0('%22' ,valcol[i] , '%22' )
    }
    curvals <- c(  paste0('' ,values , '' ), paste0('' ,valcol[i] , '' ))
    curvals <-  gsub('%22%22', '', curvals , fixed=TRUE)
    #     print(paste('baseurl=',baseurl))
    #     print(paste('names=',names))
    #     print(paste('curvals=',curvals))
    #     print(paste('dispcol[i]=',dispcol[i]))
    out[i] <- makehyper( baseurl , names, paste0('' ,curvals , append  ), prettyNum(dispcol[i], big.mark=',') )
  }
  return(out)
}

buildURL <- function (v, t, count='' ,limit=NULL, skip=0,  usekey=TRUE, 
                      type='event', db= '/drug/', addplus=TRUE, whichkey=1){
#  browser()
  if(whichkey == 1)
    { 
    mykey <- getkey()
    } else { 
      mykey <- getkey()
    }
  baseurl <- paste0(db, type ,".json")
   qq <- fda_query(baseurl)
#     fda_api_key(mykey)
  for (i in seq_along(v) )
    if ( v[i]!='')
    { 
 #     print(t[i])
      if (t[i]!=''){ 
        if(addplus)
        {
          t[i] <- gsub( ' ', '+', t[i],fixed=TRUE)
        } else {
          t[i] <- gsub( ' ', '%20', t[i],fixed=TRUE)
        }
        t[i] <- gsub( '%22', '"', t[i],fixed=TRUE)
        t[i] <- gsub( ',', '', t[i],fixed=TRUE)
        t[i] <- gsub( '^', '', t[i],fixed=TRUE)
        t[i] <- gsub( "\\", '%5C' , t[i], fixed=TRUE)
        t[i] <- gsub( '""', '%22' , t[i], fixed=TRUE)
        t[i] <- gsub( '""', '%22' , t[i], fixed=TRUE)
        t[i] <- paste0('(',t[i] ,')')
        qq <- qq %>% 
          fda_filter(v[i], paste0(t[i], collapse='+') )
      }
    }
  if (count!=''){ 
    qq <- qq %>% 
      fda_count(count)
  }
  if (!is.null(limit)){
    qq$limit <- limit
  }
#   qq <- qq %>% 
#     fda_api_key(mykey)
  myurl <- (fda_url(qq)) 
  if(usekey == TRUE)
    {
    myurl <- gsub('?', paste0('?api_key=', mykey, '&'), myurl , fixed=TRUE)
    }
  
  #temp
  #https://openfda-old.ngrok.io
#  myurl <- gsub('api.fda.gov', 'openfda-old.ngrok.io', myurl , fixed=TRUE)
#   myurl <- gsub('api.fda.gov', 'openfda.ngrok.io', myurl , fixed=TRUE)
#  #temp
  
  myurl <-paste0(myurl, '&skip=', skip)
  if(!usekey) {
    myurl <- removekey( myurl )
  }
  myurl <- gsub('Any Variable:', "", myurl, fixed=TRUE)
  return( myurl )
}

quotestr <- function(s) {
  return( paste0( '%22', s, '%22'))
}

getdaterangeFUN <- function( var, db, type){
  v <- c( '_exists_')
  t <- c( var )  
  myurl <- buildURL(v, t, count= var, db=db, type=type )
  mydf <- fda_fetch( myurl)
  meta <- mydf$meta
  tmp <- mydf$result
#  print( head(tmp))
  myrange <- c(  min(tmp$time, na.rm = TRUE), max(tmp$time, na.rm = TRUE))
  return( myrange )
}

getdaterange <- reactive({
  var <- 'receivedate'
  db <- '/drug/'
  type <- 'event'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
})    

getdaterangedeviceAE <- reactive({
  var <- 'date_received'
  db <- '/device/'
  type <- 'event'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
})    

getdaterangelabel <- reactive({
  var <- 'effective_time'
  db <- '/drug/'
  type <- 'label'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
})  

getdaterange510K <- reactive({
  var <- 'decision_date'
  db <- '/device/'
  type <- '510K'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
})   


getdaterangePMA <- reactive({
  var <- 'decision_date'
  db <- '/device/'
  type <- 'pma'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
})   

getdaterangeenforce <- reactive({
  var <- 'report_date'
  db <- '/drug/'
  type <- 'enforcement'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
}) 

getdaterangedeviceclass <- reactive({
  var <- 'report_date'
  db <- '/device/'
  type <- 'classification'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
}) 

getdaterangeenforce_food <- reactive({
  var <- 'report_date'
  db <- '/food/'
  type <- 'enforcement'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
}) 


getdaterangeenforce_device <- reactive({
  var <- 'report_date'
  db <- '/device/'
  type <- 'enforcement'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
}) 

getdaterangerecall_device <- reactive({
  var <- 'event_date_terminated'
  db <- '/device/'
  type <- 'recall'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
}) 

getdaterangereglist_device <- reactive({
  var <- 'created_date'
  db <- '/device/'
  type <- 'registrationlisting'
  myrange <- getdaterangeFUN(var, db, type)
  return( c(myrange, var ) )
}) 

listtostring <- function(s, delim=';')
{
  myevents <- gsub('\"', '', s, fixed=TRUE) 
  myevents <- gsub('c(', '', myevents, fixed=TRUE)
  myevents <- gsub('list(', '', myevents, fixed=TRUE)
  myevents <- gsub(')', '', myevents, fixed=TRUE)
  myevents <- gsub(',', delim, myevents, fixed=TRUE)
  return(myevents)
}

extractcols <- function( mydf, myvars )
{  
  myvars <- myvars[ myvars %in% names(mydf) ]
  mydf <- mydf[, myvars]   
  types <- (sapply(mydf, class))
  #list of strings, make one string
  for (i in seq_along(types))
  {
    if(types[i] == 'list')
    {
      mydf[,i] <- listtostring( mydf[ , i ])
    }
  }
  types <- (sapply(mydf, class))
#  print(types)
  typesval <- types[types!='data.frame' & types!='list']
  mydf <- as.data.frame(mydf, stringsAsFactors = FALSE)
  if(ncol(mydf)==1)
    {
#     print(mydf)
#     browser()
    names(mydf) <- myvars[1]
#    print(mydf)
  } else {
  mydf <- mydf[ , names(typesval) ]
  }
  if ( is.data.frame( mydf ) ){
    return( mydf ) 
  } else {
    return( data.frame(Note='No patient data'))
  }
  
}

getjson <- function(myurl)
{
  tmp <- tempfile()
  curl_download(myurl, tmp, quiet=TRUE)
  s <- (scan(tmp, what='character', quote='', sep='?'))
  unlink(tmp)
  s <- paste0(s, collapse='<BR>')
  return(s[1])
}

getopenfdamaxrecords <- function( maxlim=5000 )
{
  maxlim <- min(5000, maxlim)
  return(maxlim)
}

getvalfromlink <- function(instr)
{
  s <- strsplit(instr, '=' ) 
  s <- s[[1]][3]
  s <- strsplit( s, '&' )
  s <- s[[1]][1]
  s <- sub('%20', ' ', s[1])
  return(s)
}
getvalvectfromlink <- function(instr)
{
  s <- ( sapply(instr, FUN = getvalfromlink ))
  names(s) <- NULL
  return(s)
}

mytp <- function(x, y, w, refline=1, 
                 mytitle="Text Plot for Terms.  Draw a box around terms to see more details",
                 myylab='LLR') { 
  # browser()
  mycex=1
  if (length(w) > 0)
  {
    #    w <- gsub(' ', '_', w, fixed = TRUE)
    #     xlim <-  c( min(x, na.rm = TRUE), max(x, na.rm = TRUE))
    #     ylim <-  c( min(y, na.rm = TRUE), max(y, na.rm = TRUE))
    #     lay <- wordlayout(x,y,w, xlim=xlim, ylim=ylim)
    xlim <-  c( min(x, na.rm = TRUE), max(x, na.rm = TRUE))
    xlim[2] <- xlim[2] + 0.3*(xlim[2]-xlim[1] ) 
    plot(x,y,type="p", 
         xlim = xlim,
         ylim = c( 1.0, max(y, na.rm = TRUE) ),
         log='y',
         xlab= 'Number of Events',
         ylab= myylab,
         col='red',
         main=mytitle,
         cex=mycex)
    text(x, y, w, pos=4, cex=mycex)
  } else {
    plot(1,1,type="n", 
         log='y',
         xlab= 'Number of Events',
         ylab= myylab,
         col='red',
         main='Please enter a term',
         cex=mycex)
  }
  #  text(x,y,w, pos=4, cex=.75)
  grid()
  abline(h=refline, col='red')
}


renderterm <-  function( term, label, label2='' ){ 
  if(term == '') {
    term <- 'None'
  }
  if (label2 != '')
  { 
    out <- paste( '<br><b>', label, '<i><font color="dodgerblue" size="4">', term, '</font></i><br>', label2,'</b><br>' )
  } else {
    out <- paste( '<br><b>', label, '<i><font color="dodgerblue" size="4" >', term, '</font></i></b><br><br>' )
  }
  return(out)
}
renderterm2 <-  function( term, label, label2='' ){ 
  if(term == '') {
    term <- 'None'
  }
  if (label2 != '')
  { 
    out <- paste( '<br><b>', label, '<i><font color="dodgerblue" size="4">', term, '</font></i><br>', label2,'</b>' )
  } else {
    out <- paste( '<br><b>', label, '<i><font color="dodgerblue" size="4" >', term, '</font></i></b>' )
  }
  return(out)
}
getterm1description <- function(exact, term)
  {
  s <- term
  if ( exact!= 'exact' )
    {
    s <- gsub(' ', ' or ', term, fixed=TRUE)
    s <- gsub('or AND or', ' AND ', s, fixed=TRUE)
    s <- gsub('or OR or', ' OR ', s, fixed=TRUE)
    } else { 
    s <- paste0( "'", term, "'")
    }
  return( s )
}

updateviewerinputs <- function(session)
  {
  updateTextInput(session, "v1", value=( session$input$v1_2 ) )
  updateTextInput(session, "t1", value= ( session$input$t1_2 ) )
  updateTextInput(session, "v2", value=( session$input$v2_2 ) )
  updateTextInput(session, "t2", value= ( session$input$t2_2 ) )
  updateTextInput(session, "v3", value=( session$input$v3_2 ) )
  updateTextInput(session, "t3", value= ( session$input$t3_2 ) )
  updateSliderInput( session, 'skip', value=1)
}


getallvars <- function( cols, mytype = 'text', section= c('all') )
{
  # tables <- which(  stri_sub(cols, -6, -1) == '_table' )
  # text <- which( stri_sub(cols, -6, -1) != '_table' )
  if (section[1] !='all')
  {
    text <- which( stri_sub(cols, 1, 2) %in% section )
    cols <- cols[text]
  }
  cols <- substr(cols, 4, 100)
  if (mytype == 'text')
  {
    return (cols)
  } else {
    return(paste0(cols, '_table'))
  }
}

gettablenames <- function(cols)
{
  tables <- which(  stri_sub(cols, -6, -1) == '_table' )
  return(cols[tables])
}

gettextnames <- function(cols)
{
  textvar <- which(  stri_sub(cols, -6, -1) != '_table' )
  return(cols[textvar])
}

getselectedcols <- function(tmp, type, sections)
{
  realcols <- names(tmp)
  knowncols <- getallvars( allvars(), type, section=sections)
  mycols <- intersect (knowncols, realcols)
  outdf <- extractdfcols(tmp, mycols)
  return( outdf )
}

simpleCap <- function(x) {
  s <- tolower(x)
  s <- strsplit(s, " ")[[1]]
  first <- paste(toupper(substring(s[1], 1, 1)), substring(s[1], 2),
                 sep = "", collapse = " ")
  out <-  paste( s[-1], sep = "", collapse = " ")
  out <- paste(first, out)
}



listtocvect <- function(s, delim=', ', trunc=25){
  if (is.null( s) ) { 
    return('')
  }
  out <- paste0( s, sep='', collapse=delim)  
  out <- strtrim(out, trunc)
  return(out)
}

listtodf <- function(lis, delim=', ', trunc=100){
  if ( is.null(lis) )
  {
    return(NULL)
  }
  out <- data.frame(rownames=1:length(lis[[1]]), stringsAsFactors =FALSE )
  for (i in seq_along(lis) )
  {
    if (is.list(lis[[i]]))
    {
      tmp <- sapply(lis[[i]], function(x) listtocvect(x, delim, trunc) )
      out[[i]] <-   tmp
    } else {
      out[[i]] <-   ''
    }
  }
  # print(lis[[i]])
  out <- data.frame(out, stringsAsFactors =FALSE)
  names(out) <- names(lis)
  return(out)
}

listtostring <- function(s, delim=';')
{
  myevents <- gsub('\"', '', s, fixed=TRUE) 
  myevents <- gsub('c(', '', myevents, fixed=TRUE)
  myevents <- gsub(')', '', myevents, fixed=TRUE)
  myevents <- gsub(',', delim, myevents, fixed=TRUE)
  return(myevents)
}

getdf <- function(mydf, name, message='Empty Table')
{
  #  print(name)
  #  print(head(mydf) )
  err <- data.frame( Note=message, stringsAsFactors = FALSE )
  if ( is.data.frame(mydf)  ) {
    if (name %in% names(mydf) ) {
      tmp <- mydf[, name]
      if ( is.data.frame(tmp)  ) {
        return(tmp) 
      } else if ( is.data.frame(tmp[[1]])  ) {
        return(tmp[[1]]) 
      } else {
        return(err)
      }
    }
  }
  return( err ) 
  
}

extractdfcols <- function(tmp, mycols, numrows=1)
{ 
 # browser()
  numcols <- length(mycols)
 # browser()
  outdf <- data.frame( matrix( ncol=numcols, nrow=0 ), stringsAsFactors = FALSE )
  mynames <- names(tmp)
  for ( i in 1:numrows)
  {
    outdf <- rbind(outdf, as.character( rep('None', numcols) ) )
  }
  names(outdf) <- mycols
  cols <- mynames %in% mycols 
  for (i in seq_along(cols))
  {
    if (cols[i])
    { 
      if ( length(tmp[[i]]) > 0 )
      {
        outdf[ mynames[i] ] <- listtostring(tmp[[i]], ';')
      } else {
        outdf[ mynames[i] ] <- ''
      }
    }
  }
  if ( length(outdf)==0 )
  {
    outdf <- data.frame( Variable ='None')
  }
  return(outdf)
}

getvarprefixs <- function( cols=allvars() )
{
  s <- substr( cols, 1, 2 )
  return( unique(s))
}

getsimplecols <- function(mydf)
  {
  types <- (sapply(mydf, class))
  typesval <- types[types!='data.frame' & types!='list']
  mynames <-  names(typesval)
  mydf <- as.data.frame( mydf[ , mynames ] )
  names(mydf) <- mynames
  return (mydf)
}

buildtable <- function(flat, keyvals, keyname, myvars)
  {
  mynames <- c( keyname, getallvars( allvars(), mytype = 'text', section= myvars ) )
  blank <- matrix(nrow=1, ncol=length(mynames))
  blankdf <- as.data.frame( blank, stringsAsFactors=FALSE )
  names(blankdf) <- mynames
  tmp <- blankdf
#   browser()
  for (i in 1:nrow(flat) )
  {
    curid <- keyvals[i]
    curdata <- flat[ i, ]
    curdf <- data.frame( id=curid , curdata)
    curnames <- names(curdf)
 #   browser()
    newdf <- matrix(nrow=nrow(curdf), ncol=length(mynames))
    newdf <- as.data.frame( newdf, stringsAsFactors=FALSE )
    names(newdf) <- mynames
    newdf[ keyname ] <- curdf['id' ]
    for (j in 2:length(mynames) )
    {
      if ( mynames[j] %in% curnames )
      {
        newdf[ mynames[j] ] <- curdf[ mynames[j] ]
      } 
    }
    tmp <- rbind(tmp, newdf)
  }
#  browser()
  tmp <- tmp[ which(!is.na( tmp[ keyname ] ) ), ]
  return( tmp )
}

buildtablerow <- function(flat, keyval, keyname, myvars)
{
 # browser()
  mynames <- c( keyname, getallvars( allvars(), mytype = 'text', section= myvars ) )
  blank <- matrix(nrow=1, ncol=length(mynames))
  blankdf <- as.data.frame( blank, stringsAsFactors=FALSE )
  names(blankdf) <- mynames
  tmp <- blankdf
  #   browser()
  for (i in 1:nrow(flat) )
  {
    curid <- keyval
    curdata <- as.data.frame( flat[ i, ] )
    curdf <- data.frame(curdata)
    curnames <- names(curdf)
    #   browser()
    newdf <- matrix(nrow=nrow(curdf), ncol=length(mynames))
    newdf <- as.data.frame( newdf, stringsAsFactors=FALSE )
    names(newdf) <- mynames
#    newdf[ keyname ] <- curdf[keyname ]
    for (j in 1:length(mynames) )
    {
      if ( mynames[j] %in% curnames )
      {
        newdf[ mynames[j] ] <- curdf[ mynames[j] ]
      } 
    }
    tmp <- rbind(tmp, newdf)
  }
  #  browser()
  tmp <- tmp[ which(!is.na( tmp[ keyname ] ) ), ]
  return( tmp )
}