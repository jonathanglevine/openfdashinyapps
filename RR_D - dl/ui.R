library(shiny)


source('sourcedir.R')

getdrugnames <- function() {
  if ( !exists( 'asprrdf' ) ){
    load( 'data/asprrdf.RData')
  }
  drugnames <- c( '', unique( asprrdf$d ))
  return( as.list( sort(drugnames) ) )
}

renderDrugName <- function() { 
  
  ( htmlOutput('drugname') )
  
} 
renderLimit <- function() { 
  
  ( htmlOutput('limit') )
  
}  

renderStart <- function() { 
  

  ( htmlOutput('start') )
  
}  

renderStart2 <- function() { 
  ( htmlOutput('start2') )
  
}  

getcurtab <- function() { 
#  browser()
#  print( textOutput('curtab') )
  
#  browser()
   s<-( textOutput('limit') )
   print(s)
#   ss <- strsplit( as.character(s), ">" , fixed=TRUE)
#   ss <- strsplit( as.character(ss[[1]][2]), "<" , fixed=TRUE)
#   print(ss[[1]][1])
  return(  "PRR and ROR Results" )
  
}  

shinyUI(fluidPage(
                  fluidRow(
                    column(width=4,
                           a(href='https://open.fda.gov/', 
                             img(src='l_openFDA.png', align='bottom') ),
                           renderDates()
                    ),
                    column(width=8,
                           titlePanel("RR-Drug" ) )
                  ),
#   img(src='l_openFDA.png'),
#   titlePanel("RR-Drug"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Select Drug',
#                  if ( !exists( 'drugnames' ) ){
#                    load( 'data/drugnames.RData')
#                  },
#                  selectInput_p("v1", 'Drug Variable' , c('Active Substance', 'Medicinal Product'), 
#                                HTML( tt('drugvar1') ), tt('drugvar2'),
#                                placement='top'), 
                 conditionalPanel(
                   condition = "1 == 1",
#                    selectInput_p( 't1' , "Drug Name", getdrugnames(), 
#                                HTML( tt('drugname1') ), tt('drugname2'),
#                                placement='top'), 
                   selectInput( 't1' , "Drug Name (activesubstancename)", getdrugnames(), multiple = FALSE)
, 
#                    
                   numericInput_p('limit', 'Minimum event count', 5,
                                  1, 100, step=1, 
                                  HTML( tt('limit1') ), tt('limit2'),
                                  placement='bottom')
#                    
#                    numericInput_p('start', 'Rank of first event', 1,
#                                   1, 999, step=1, 
#                                   HTML( tt('limit1') ), tt('limit2'),
#                                   placement='bottom')
                 ),
#                  wellPanel(
#                    bsButton("tabBut", "Select Drug and # of Events...", 
#                             style='primary'),
#                    br(),
#                    renderDrugName()
# #                    ,
# #                    radioButtons('useexact', 'Match drug name:', c('Exactly'='exact', 'Any Term'='any'), selected='any'),
# #                    renderLimit(),
# #                    renderStart()
#                  ), 
#                 dateRangeInput('daterange', 'Use Reports Between: ', start = '1989-6-30', end = Sys.Date()),
                 
                 bsModal( 'modalExample', "Enter Variables", "tabBut", size = "small",
                          htmlOutput('mymodal'),
                          #          dateRangeInput('daterange2', 'Date Report Was First Received by FDA.', start = '1989-6-30', end = Sys.Date() ),
                          bsButton("update", "Update Variables", style='primary'),
#                           selectInput_p( 'drugname', "Name of Drug", getdrugnames(), 
#                                       HTML( tt('drugname1') ), tt('drugname2'),
#                                       placement='left'),  
                          selectInput( 'drugname', "Name of Drug (activesubstancename)", getdrugnames(), multiple = FALSE)
# , 
#                           numericInput_p('limit2', 'Number of most frequent events to analyze:', 50,
#                                          1, 100, step=1, 
#                                          HTML( tt('limit1') ), tt('limit2'),
#                                          placement='left'), 
#                           
#                           numericInput_p('start2', 'Start with ranked frquency #', 1,
#                                          1, 999, step=1, 
#                                          HTML( tt('limit1') ), tt('limit2'),
#                                          placement='left')
                          ),
                 bsAlert("alert"),
                 HTML( (loadhelp('overviewside') ) )  )
        ,
    id='sidetabs', selected='Select Drug')
    ),
    mainPanel(
      bsAlert("alert2"),
      tabsetPanel(
                tabPanel("PRR and ROR Results",
                         wellPanel(
                         htmlOutput( 'prrtitle' )
                         ),
#                          wordcloudtabset('cloudprr', 'prr', 
#                                          popheads=c( tt('prr1'), tt('word1') ), 
#                                          poptext=c( tt('prr5'), tt('word2') ) )
                         maketabset( c('prr2', 'cloudprr', 'textplot'), 
                                     types=c('datatable', "plot", 'plot'),
                                     names=c("Table","Word Cloud", "text Plot"), 
                                     popheads = c(tt('prr1'), tt('word1'), tt('textplot1') ), 
                                     poptext = c( tt('prr5'), tt('wordPRR'), tt('textplot2') ) )
                ),
                tabPanel("Analyzed Event Counts for Specified Drug"   ,
#                          wellPanel( 
#                            htmlOutput( 'alldrugtext' ),
#                            htmlOutput_p( 'queryalldrugtext' ,
#                                          tt('gquery1'), tt('gquery2'),
#                                          placement='bottom' )
#                          ), 
#                          wellPanel( 
#                            htmlOutput( 'querytitle' ), 
#  #                          tableOutput("query"),
#                            htmlOutput_p( 'querytext' ,
#                                        tt('gquery1'), tt('gquery2'),
#                                        placement='bottom' )
#                          ),
                    wordcloudtabset('cloudquery', 'specifieddrug2', 
                                    names=c('datatable', "plot"),
                                    popheads=c( tt('event1'), tt('word1') ), 
                                    poptext=c( tt('event2'), tt('word2') )
                    )
                ),
                tabPanel("Analyzed Event Counts for All Drugs",
#                          wellPanel( 
#                            htmlOutput( 'alltext' ),
#                            htmlOutput_p( 'queryalltext' ,
#                                        tt('gquery1'), tt('gquery2'),
#                                        placement='bottom' )
#                          ),
#                          wellPanel(
#                            htmlOutput( 'alltitle' ), 
#                          htmlOutput_p( 'allquerytext' ,
#                                      tt('gquery1'), tt('gquery2'),
#                                      placement='bottom' ) ),
                         wordcloudtabset('cloudall', 'all2', 
                                         names=c('datatable', "plot"),
                                         popheads=c( tt('event1'), tt('word1') ), 
                                         poptext=c( tt('event2'), tt('word2') ))
                ),
                tabPanel("PRRs for all Drug-Event COmbinations",
                         wellPanel( 
                           dataTableOutput('fulltable')
                         )
                ),
                tabPanel("Activesubstancename-Medicinalproduct mapping",
                        wellPanel( 
                          htmlOutput( 'mpmaptext' )
                        ),
                         wellPanel( 
                           dataTableOutput('maptable')
                         )
                 ),
#                 tabPanel("Counts For Indications In Selected Reports",
#                          wellPanel( 
#                            htmlOutput( 'indtext' ),
#                            htmlOutput_p( 'queryindtext' ,
#                                        tt('gquery1'), tt('gquery2'),
#                                        placement='bottom' )
#                          ),
#                          wellPanel(
#                            htmlOutput( 'indtitle' )
#                          ),
#                          wordcloudtabset('cloudindquery', 'indquery',
#                                          popheads=c( tt('indication1'), tt('word1') ),
#                                          poptext=c( tt('indication2'), tt('word2') ) )
#                 ),
                tabPanel("Other Apps",  
                         wellPanel( 
                           htmlOutput( 'applinks' )
                         )
                ),
                tabPanel('Data Reference', HTML( renderiframe('https://open.fda.gov/drug/event/') )  ),
                tabPanel('About', 
                         img(src='l_openFDA.png'),
                         HTML( (loadhelp('about') ) )  ),
#                 tabPanel("session",  
#                          wellPanel( 
#                            verbatimTextOutput( 'urlquery' )
#                          )
#                 ),
              id='maintabs', selected=  "PRR and ROR Results" 
            )
          )
        )
      )
    )
