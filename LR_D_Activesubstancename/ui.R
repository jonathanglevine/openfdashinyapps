library(shiny)
source('sourcedir.R')

getdrugnames <- function() {
  myquarters <- getquarters()
  load( paste0( DATADIR, 'quarters/', myquarters[[1]], '.RData' ) )
  drugnames <- c( '', unique( as.character(detable$d) ))
  return( as.list( sort(drugnames) ) )
}
geteventnames <- function() {
  myquarters <- getquarters()
  load( paste0( DATADIR, 'quarters/', myquarters[[1]], '.RData' ) )
  eventnames <- c( '', unique( as.character(detable$e) ))
  return( as.list( sort(eventnames) ) )
}

renderDrugName <- function() { 
  
  ( htmlOutput('drugname') )
  
} 
shinyUI(fluidPage(
                  fluidRow(
                    column(width=4,
                           a(href='https://open.fda.gov/', 
                             img(src='l_openFDA.png', align='bottom') )
                    ),
                    column(width=8,
                           titlePanel("LRT Signal Analysis for a Drug" ) )
                  ),

  sidebarLayout(
    sidebarPanel(
                 selectInput_p("v1", 'Drug Variable' , c('Active Substance- Suspect Only'), 
                               HTML( tt('drugvar1') ), tt('drugvar2'),
                               placement='top'), 
                 conditionalPanel(
                   condition = "1 == 1",
                   selectInput( 't1' , "Drug Name (activesubstancename)", getdrugnames(), multiple = FALSE)
                   ,
                   
                   selectInput( 'startquarter' , "From", getquarters(), multiple = FALSE, selected = min(unlist(getquarters())) ), 
                
                   selectInput( 'quarter' , "through", getquarters(), multiple = FALSE),
                   
            
                   numericInput_p('numsims', 'Number of Simulations', 1000,
                                  1000, 50000, step=1, 
                                  HTML( tt('numsims1') ), tt('numsims2'),
                                  placement='bottom')
                 ),

                 wellPanel(
                   helpText( HTML('<b>Down Load Report</b>') ),
                   radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                inline = TRUE),
                   downloadButton('downloadReport', 'Download LRT Report')
                 ),
                 
                 bsAlert("alert")
                 ,
                 HTML( (loadhelp('LRT') ) )  
    ),
    mainPanel(
      bsAlert("alert2"),
      tabsetPanel(
                tabPanel("LRT Results based on Total Events",
                         wellPanel(
                         htmlOutput( 'prrtitle' ), 
                         helpText('Results sorted by LRR'),
                         shiny::tags$head(shiny::tags$style(shiny::HTML(
                           "#text2 { font-size: 12px; height: 75px; overflow: auto; }"
                         ))),
                         
                         h5('FAERS Medicinal Product Terms Mapped to '),
                         h4(renderDrugName()),
                         div(id = "text2", pre( textOutput( 'mpvalues' ) ) )
                         ),
                         maketabset( c('prr', 'cloudprr', 'textplot'), 
                                     types=c('datatable', "plot", 'plot'),
                                     names=c("Table","Word Cloud", "Text Plot") )
                ),
              tabPanel("Simulation Results for Event Based LRT",
                       wellPanel( 
                         plotOutput( 'simplot')
                        )
              ),
              tabPanel("Counts for all Drug-Event Combinations",
                       wellPanel( 
                         dataTableOutput('fulltable')
                       )
              ),

                tabPanel("Other Apps",  
                         wellPanel( 
                           htmlOutput( 'applinks' )
                         )
                ),
                tabPanel('Data Reference', HTML( renderiframe('https://open.fda.gov/drug/event/') ) ),
                tabPanel('About', 
                         img(src='l_openFDA.png'),
                         HTML( (loadhelp('about') ) )  ),

              id='maintabs'
            )
          )
        )
      )
    )
