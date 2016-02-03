

getterm1 <- function( session, quote=FALSE, upper=TRUE){
  s <- session$input$t1
  if( upper )
    {
    s <- toupper( s )
    }
  if (s == '')
  {
    return( '' )
  }
  if (quote){
    s <-  gsub('"', '', s , fixed=TRUE)
    return(paste0('"', s, '"'))
    return(paste0('%22', s, '%22'))
  } else {
    return( s )
  }
  names <- s
  names <- paste0( names, collapse=' ')
  return( names )
}

getterm2 <- function( session, quote=FALSE){
  s <- toupper( session$input$t2 )
  if (s == '')
  {
    return( '' )
  }
  if (quote){
    s <-  gsub('"', '', s , fixed=TRUE)
    return(paste0('%22', s, '%22'))
  } else {
    return( s )
  }
  names <- s
  names <- paste0( names, collapse=' ')
  return( names )
}

getlimit <- function( session ){ 
  return(session$input$limit)
}

getnumsims <- function( session ){ 
  return(session$input$numsims)
}

getstart <- function( session ){
  return(session$input$start)
}

tableout <- function( mydf, mynames=NULL, error)
  { 
  if ( length(mydf) > 0 )
  {
    if (!is.null(mynames))
    {
      names(mydf) <- mynames
      }
    return(mydf) 
  } else  {return(data.frame(Term=error, Count=0))}
}

cloudout <- function(mydf, title)
  {
  if ( is.data.frame(mydf) )
  {
    mydf <- mydf[ 1:min( 100, nrow(mydf) ), ]
 #   print(mydf)
    return( getcloud(mydf,  title = title ) )  
  } else  {
    return( data.frame( error) )
  }  
  
}
getcounts999 <- function( session, v, t, count, limit=1000, 
                          exactrad='exact', counter=1, db= '/drug/' )
  {
  if ( is.null( t ) ){
    return(data.frame( c( paste('Please enter a', getsearchtype(), 'name') , '') ) )
  }
#  browser()
  #Can we find exact name?
  if ( exactrad=='exact' )
  {
    exact <- TRUE
#    t <- paste0('%22', t, '%22')
    myurl <- buildURL(v = v, t= t, 
                      count = count, limit=limit, db= db, addplus = FALSE  )
    mylist <- fda_fetch_p( session, myurl,  message = counter, flag=paste( 'No Reports for', t, '<br>' ) )
  
  } else {
    #No, can we find close name?
    exact <- FALSE
    v <- sub('.exact', '', v, fixed=TRUE)
    myurl <- buildURL(v= v, t=t, 
                      count=count,limit=limit, db= db)
    mylist <- fda_fetch_p( session, myurl,  message = counter, flag=paste( 'No Reports for', t, '<br>' ) )
  }
  mydf <- mylist$result
  excludeddf <- data.frame()
  if( length(mydf)>0 )
    {
    mydfsource <- mylist$result
#    browser()
    caretrow <- which(grepl('^', mydfsource[,'term'], fixed=TRUE) )
    if (length(caretrow) > 0)
    {
      excludeddf <- mydfsource[ caretrow, ] 
      mydf <- mydfsource[-caretrow, ]
    }
    aposrow <- which(grepl("'", mydf[,'term'], fixed=TRUE) )
    if (length(aposrow) > 0)
    {
      excludeddf <- rbind( excludeddf, mydf[ aposrow, ] )
      mydf <- mydf[ -aposrow, ]
    }
    slashrow <- which(grepl("/", mydf[,'term'], fixed=TRUE) )
    if (length(slashrow) > 0)
    {
      excludeddf <- rbind( excludeddf, mydf[ slashrow, ] )
      mydf <- mydf[ -slashrow, ]
    }
    commarow <- which(grepl(",", mydf[,'term'], fixed=TRUE) )
    if (length(commarow) > 0)
    {
      excludeddf <- rbind( excludeddf, mydf[ commarow, ] )
      mydf <- mydf[ -commarow, ]
    }
    if (length(excludeddf) > 0 )
      {
      names(excludeddf) <- c( "Terms that contain '^',  '/',  ','  or ' ' ' can't be analyzed and are excluded", 'count' )
      }
  } else {
    excludeddf <- mydf
  }
  max <- min(900, nrow(mydf) )
  return( list(mydf=mydf[1:max,], myurl=myurl, exact = exact, excludeddf = excludeddf   ) )
  }