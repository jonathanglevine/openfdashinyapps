

calcLRTstats <- function(totals, combe, combr=NULL, allevents)
  #Total number of reports
{
  #Total reports for drug j
  rn.j <- totals$totaldrug
  #Total reports for DE combination
  rnij <-  combe$count.x
  #Total report forevent i
  rni. <- combe$count.y
  rn.. <- totals$total
  #   print(combe)
  #   print(combr)
  #   print(totals)
  
  #Total number of Events
  #Total events for drug j
  n.j <- sum(combe$count.x)
  #Total reports for DE combination
  nij <-  combe$count.x
  #Total reports for event i
  ni. <- combe$count.y
  #Total events
  n.. <- sum(allevents)
  
  
  pi. <- ni./n..
  a <- nij
  b <- ni. - nij
  c <- n.j - nij
  d <- n.. - ni. - n.j + nij
  RR <- prrd( n.., ni., n.j, nij )
  PRRD <- prrd( rn.., rni., rn.j, rnij )
  LLRE <- LLR( n.., ni., n.j, nij )
  LLRE[is.nan(LLRE)] <- 0
  LLRE[RR < 1] <- 0
  LLRR <- LLR( rn.., rni., rn.j, rnij )
  LLRR[is.nan(LLRR)] <- 0
  return( list( RR=RR, PRRD=PRRD, LLRE=LLRE, LLRR=LLRR ) )
}

cloudplot <- function( mydf, session, scale1=6, name=1, freq=2, mymult=1,
                       stattext="Frequencies",termtype='Events', intype='Drug', scale=NULL  ){ 

  mydf <- data.frame(mydf[,name], mydf[, freq]*mymult)
  if ( is.data.frame(mydf) & getterm1(session)!="" )
  {    
    mytitle <- paste( stattext, 'for',  termtype , 'in Reports That Contain\n', getterm1(session) )
  } else  {
    mytitle <- paste('Please Enter', intype, 'Term' )
    mydf <- data.frame(term=c('No Drug Given'), LLR=1)
  }  
  return( getcloud(mydf, scale1=scale1, title=mytitle, scale=scale ) )  
}

checkdf <- function(mydf, myinput, names=NULL, changecell=NULL)
{
  if ( is.data.frame(mydf) & length(mydf) > 0 )
  {
    if( !is.null(names))
    {
      names(mydf) <- names
    }
    if( !is.null(changecell))
    {
      mydf[changecell['row'], changecell['column']] <- changecell['val']
    }
    return(mydf) 
  } else  {return(data.frame(Term=paste( 'No results for', myinput ), Count=0))}
}

