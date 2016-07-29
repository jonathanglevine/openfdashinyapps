require(shinyBS)

# HTML <- function(s1, s2=NULL)
# {
#   # browser()
#   out1 <- as.character( s1 )
#   out2 <- s2
#   out <- shiny::HTML(out1, out2)
#   return(out)
# }

getquarters <- function() {
  #  browser()
  curdir <- paste0( DATADIR, 'quarters')
  flist <- list.files( curdir, all.files = TRUE, 
                       ignore.case = TRUE, full.names = FALSE, no..=TRUE)
  myquarters <- substr( flist, 1, 8)
  myquarters <- sort( myquarters, decreasing = TRUE)
  mydates <- as.Date(myquarters, '%Y%m%d')
  s <- quarters(mydates)
  names(myquarters) <- paste( s, substr(myquarters,1,4 ) )
  return( as.list( myquarters ) )
}

getdrugvarchoices <- function(){
  openfdavars <- c( 
    'generic_name',
    'substance_name',
    'brand_name',
    'pharm_class_moa',
    'pharm_class_cs',
    'pharm_class_pe',
    'pharm_class_epc')
  openfdavars <-  paste0( 'patient.drug.openfda.', openfdavars )
  s <- c( openfdavars, 'patient.drug.medicinalproduct',
          'patient.drug.drugindication', 
          'patient.drug.activesubstance.activesubstancename')
  return(s)
}

getdevicevarchoices <- function(){
  openfdavars <- c( 
    'device_name')
  openfdavars <-  paste0( 'device.openfda.', openfdavars )
  s <- c( openfdavars)
  return(s)
}

wordcloudtabset <- function(cloud, table, 
                            types= c('table', 'plot'),
                            names=c( "Tables","Word Cloud" ), 
                            popheads = c('Frequency Table',tt('word1') ), 
                            poptext = c('Counts', tt('word2') ) ) { 

  tabsetPanel(
    tabPanel(names[1],
             wellPanel(
               if (types[1] == 'table'){
               htmlOutput_p(table,
                            HTML( popheads[1] ), HTML(poptext[1]),
                            placement='top')
               } else if (types[1] == 'datatable') {
                 dataTableOutput( table )
               }
             )        
    ),
  tabPanel( names[2],
            if (types[2] == 'table'){
              htmlOutput_p(cloud,
                           HTML( popheads[2] ), HTML(poptext[2]),
                           placement='top')
            } else {
             plotOutput_p(cloud,
                        HTML( popheads[2] ), poptext[2],
                        placement='top')
            }
    ), selected = names[1]
  )
}
maketabset <- function( outputs, types=c('html', 'plot'), 
                        names=c( "Table2","Word Cloud2" )
                        , 
                        popheads = c(NULL, NULL, NULL) , 
                        poptext = c(NULL, NULL, NULL ) 
                        ) { 
  
  tabsetPanel(
    tabPanel(names[1],
             wellPanel(
               if (types[1] == 'html'){
                 htmlOutput_p(outputs[1],
                              HTML( popheads[1] ), HTML(poptext[1]),
                              placement='top' )
               } else if ( types[1] == 'datatable' )
               {
               dataTableOutput( outputs[1] )
               }
               else {
                 plotOutput(outputs[1])
               }
             )
    ),
    tabPanel( names[2],
              if (types[2] == 'html'){
                htmlOutput_p(outputs[2],
                             HTML( popheads[2] ), HTML(poptext[2]),
                             placement='top')
              } else {
                plotOutput_p(outputs[2],
                             HTML( popheads[2] ), HTML(poptext[2]),
                             placement='top')
              }
    ),
      tabPanel( names[3],
                
                wellPanel(
                  tableOutput("hoverinfo")
                ),
                         if (types[3] == 'html'){
                           htmlOutput_p(outputs[3],
                                        HTML( popheads[3] ), HTML(poptext[3]),
                                        placement='top')
                           } else if (types[3] == 'html'){
                             htmlOutput_p(outputs[3],
                                          HTML( popheads[3] ), HTML(poptext[3]),
                                          placement='top')
                         } else {
                           plotOutput_p(outputs[3],
                                        HTML( popheads[3] ), HTML(poptext[3]),
                                        placement='left')
                           
                         },
                
                wellPanel(
                tableOutput("info")
                )
      ), selected = names[1]
  )
}

maketabset2 <- function( outputs, types=c('html', 'plot'), 
                        names=c( "Table2","Word Cloud2" )
                        , 
                        popheads = c(NULL, NULL, NULL) , 
                        poptext = c(NULL, NULL, NULL ) 
) { 
  
  tabsetPanel(
    tabPanel(names[1],
             wellPanel(
               if (types[1] == 'html'){
                 htmlOutput_p(outputs[1],
                              HTML( popheads[1] ), HTML(poptext[1]),
                              placement='top' )
               } else if ( types[1] == 'datatable' )
               {
                 dataTableOutput( outputs[1] )
               }
               else {
                 plotOutput(outputs[1])
               }
             )
    ),
    tabPanel( names[2],
              if (types[2] == 'html'){
                htmlOutput_p(outputs[2],
                             HTML( popheads[2] ), HTML(poptext[2]),
                             placement='top')
              } else {
                plotOutput_p(outputs[2],
                             HTML( popheads[2] ), HTML(poptext[2]),
                             placement='top')
              }
    )
    , selected = names[1]
  )
}
getpopstrings <- function( myname, pophead, poptext )
  {
  helpfunname <- paste0('pop', myname )
#  browser()
# if function called popmyname exists, call it to get pop heads
# otherwise if pophead or poptext are null get tt(mynametext) or tt(mynamehead)
  if ( exists( helpfunname ) )
    {
      helpfun <- get( helpfunname)
      s <- helpfun()
      pophead <- s['head']
      poptext <- s['text']
    } else {
      if (is.null(pophead))
      {
        pophead <- tt( paste0(myname, 'head' ) )
#        print(pophead)
      }
      if (is.null(poptext))
      {
        poptext <- tt( paste0(myname, 'text' ) )
      }
    }
  return ( c( pophead=pophead[[1]], poptext=poptext[[1]] ) )
}

htmlOutput_p <- function(table, pophead=NULL, poptext=NULL, placement='top')
  {
  s <- getpopstrings( table, pophead, poptext)
  pophead <- s['pophead']
  poptext <- s['poptext']
  if( !is.null(pophead) )
      {
      popify(
        htmlOutput(table),
      HTML(  paste('<b>', pophead,'</b>') ), poptext,
      placement=placement)
    }
  else
    {
    tableOutput(table)
    }
  }     

dataTableOutput_p <- function(table, pophead=NULL, poptext=NULL, placement='top')
{
  s <- getpopstrings( table, pophead, poptext)
  pophead <- s['pophead']
  poptext <- s['poptext']
  if( !is.null(pophead) )
  {
    popify(
      dataTableOutput(table),
      HTML(  paste('<b>', pophead,'</b>') ), poptext,
      placement=placement)
  }
  else
  {
    dataTableOutput(table)
  }
}     

plotOutput_p <- function(plot, pophead=NULL, poptext=NULL, placement='top', ...)
{
  s <- getpopstrings( plot, pophead, poptext)
  pophead <- s['pophead']
  poptext <- s['poptext']
  if( !is.null(pophead) )
  {
    popify(
      plotOutput(plot, brush = "plot_brush", hover=hoverOpts(id='plot_hover'), ...),
      HTML( pophead ), HTML(poptext),
      placement=placement)
  }
  else
  {
    plotOutput(plot, brush = "plot_brush", hover=hoverOpts(id='plot_hover'), ...)
  }
}  

selectInput_p <- function( name, label, values, pophead=NULL, poptext=NULL, 
                           placement='bottom', usepop=TRUE, selected=NULL,  ...)
{
#  browser()
  s <- getpopstrings( name, pophead, poptext)
  pophead <- s['pophead']
  poptext <- s['poptext']
  if( !is.null( pophead ) & usepop )
  {
    popify( 
      selectInput(name, label , values, selected), 
      HTML( pophead ), poptext,
      placement=placement)
  }
  else
  {
    selectInput(name, label , values, selected)
  }
} 

textInput_p <- function( name, label, value, pophead=NULL, poptext=NULL, 
                           placement='bottom', ...)
{
  s <- getpopstrings( name, pophead, poptext)
  pophead <- s['pophead']
  poptext <- s['poptext']
  if( !is.null(pophead) )
  {
    popify( 
      textInput(name, label , value), 
      HTML( pophead ), poptext,
      placement=placement)
  }
  else
  {
    textInput(name, label , value)
  }
} 

numericInput_p <- function( name, label, value, min=NA, max=NA, step=NA, pophead=NULL, poptext=NULL, 
                         placement='bottom', ...)
{
  s <- getpopstrings( name, pophead, poptext)
  pophead <- s['pophead']
  poptext <- s['poptext']
  if( !is.null(pophead) )
  {
    popify( 
      numericInput(name, label , value, min, max, step), 
      HTML( pophead ), poptext,
      placement=placement)
  }
  else
  {
    numericInput(name, label , value, min, max, step)
  }
} 



usepopup <- function()
{
   usepop <- uiOutput('usepop') 
#   print(usepop)
  if (is.null(usepop))
    (
      usepop <- TRUE
    )
 return( paste(usepop, 'Hello' ) )
}


renderDates <- function() { 
  
  ( htmlOutput('date1') )
  
}  

renderiframe <- function( s )
{
  out <- paste0('<iframe src="', s, '" width=100% height=600 ></iframe>')
  return(out)
}