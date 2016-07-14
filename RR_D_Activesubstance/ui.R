library(shiny)


source('sourcedir.R')


getdrugnames <- function() {
  # browser()
  myquarters <- getquarters()
  load( paste0( DATADIR, 'quarters/', myquarters[[1]], '.RData' ) )
  drugnames <- c( '', unique( as.character(detable$d) ))
  return( as.list( sort(drugnames) ) )
}
geteventnames <- function() {
  # browser()
  myquarters <- getquarters()
  load( paste0( DATADIR, 'quarters/', myquarters[[1]], '.RData' ) )
  eventnames <- c( '', unique( as.character(detable$e) ))
  return( as.list( sort(eventnames) ) )
}
renderterm1 <- function() { 
  ( htmlOutput('term1') )
  
}   
getcurtab <- function() { 
  return(  "PRR Results" )
  
}  

shinyUI(fluidPage(
                  fluidRow(
                    column(width=4,
                           a(href='https://open.fda.gov/', 
                             img(src='l_openFDA.png', align='bottom') )
                    ),
                    column(width=8,
                           titlePanel("RR-Drug-Activesubstance" ) )
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
                 selectInput_p("v1", 'Drug Variable' , c('Active Substance- Suspect Only'), 
                               HTML( tt('drugvar1') ), tt('drugvar2'),
                               placement='top'), 
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
                                  placement='bottom'),
                    selectInput( 'quarter' , "From Q1 2004 through", getquarters(), multiple = FALSE)
                 ),
                 bsAlert("alert"),
                 HTML( (loadhelp('overviewsidedl') ) )  )
        ,
    id='sidetabs', selected='Select Drug')
    ),
    mainPanel(
      bsAlert("alert2"),
      tabsetPanel(
                tabPanel("PRR Results",
#                          wellPanel(
#                          htmlOutput( 'prrtitle' )
#                          ), 
                        wellPanel( 
                          htmlOutput( 'prrtitle' ),
                          shiny::tags$head(shiny::tags$style(shiny::HTML(
                            "#text2 { font-size: 12px; height: 75px; overflow: auto; }"
                          ))),
                          
                          h5('FAERS Medicinal Product Terms Mapped to '),
                          h4(renderterm1()),
                          div(id = "text2", pre( textOutput( 'mpvalues' ) ) ) ),
                         maketabset( c('prr2', 'cloudprr', 'textplot'), 
                                     types=c('datatable', "plot", 'plot'),
                                     names=c("Table","Word Cloud", "text Plot"), 
                                     popheads = c(tt('prr1'), tt('word1'), tt('textplot1') ), 
                                     poptext = c( tt('prr5'), tt('wordPRR'), tt('textplot2') ) )
                ),
                tabPanel("Analyzed Event Counts for Specified Drug"   ,
                         wellPanel( 
                           htmlOutput( 'alldrugtext' )
                         ), 
#                         
                    wordcloudtabset('cloudquery', 'specifieddrug2', 
                                    types=c('datatable', 'plot'),
                                    popheads=c( tt('event1'), tt('word1') ), 
                                    poptext=c( tt('event2'), tt('word2') )
                    )
                ),
                tabPanel("Analyzed Event Counts for All Drugs",
                         wellPanel( 
                           htmlOutput( 'alltext' )
                         ),
                         wordcloudtabset('cloudall', 'all2', 
                                         types=c('datatable', 'plot'),
                                         popheads=c( tt('event1'), tt('word1') ), 
                                         poptext=c( tt('event2'), tt('word2') ))
                ),
                tabPanel("PRRs for all Drug-Event Combinations",
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
                tabPanel("Other Apps",  
                         wellPanel( 
                           htmlOutput( 'applinks' )
                         )
                ),
                tabPanel('Data Reference', HTML( renderiframe('https://open.fda.gov/drug/event/') )  ),
                tabPanel('About', 
                         img(src='l_openFDA.png'),
                         HTML( (loadhelp('about') ) )  ),
              id='maintabs', selected=  "PRR Results" 
            )
          )
        )
      )
    )
