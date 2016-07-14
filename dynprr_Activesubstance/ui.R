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
renderDrugName <- function() { 
  
  ( htmlOutput('drugname') )
  
} 
renderEventName <- function() { 
  
  ( htmlOutput('eventname') )
  
} 
renderEventText <- function() { 
  
  return( verbatimTextOutput('geteventtext') )
  
}    

renderterm1 <- function() { 
  ( htmlOutput('term1') )
}  
shinyUI(fluidPage(
                   fluidRow(
                     column(width=4,
                            a(href='https://open.fda.gov', 
                              img(src='l_openFDA.png', align='bottom'))
                     ),
                     column(width=8,
                            titlePanel("Dynamic PRR" ) )
                   ),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Select Inputs',
                 
                 selectInput_p("v1", 'Drug Variable' , c('Active Substance- Suspect Only'), 
                               HTML( tt('drugvar1') ), tt('drugvar2'),
                               placement='top'), 
                   selectInput( 't1' , "Drug Name", getdrugnames(), multiple = FALSE),                  
                   selectInput( 't2' , "Event Name", geteventnames(), multiple = FALSE), 
                 conditionalPanel(
                   condition = "1 == 2",  
                   selectInput( 'quarter' , "From Q1 2004 through", getquarters(), multiple = FALSE)
                 ),
                 bsAlert("alert"),
    id='sidetabs', selected='Select Inputs')
    ) ),
    mainPanel(
      bsAlert("alert2"),
      tabsetPanel(
        tabPanel("PRR Over Time",  
                 wellPanel( 
                   plotOutput_p( 'prrplot',
                                 tt('prr1'), tt('prr5'),
                                 placement='left', height='600px' )
                 )
        ),
      tabPanel("Report Counts and PRR",  
               wellPanel( 
                  htmlOutput( 'prrtitle' ),
                  shiny::tags$head(shiny::tags$style(shiny::HTML(
                    "#text2 { font-size: 12px; height: 75px; overflow: auto; }"
                    ))),
                  h5('FAERS Medicinal Product Terms Mapped to '),
                  h4(renderterm1()),
                  div(id = "text2", pre( textOutput( 'mpvalues' ) ) ) ),
               wellPanel( 
                 htmlOutput_p( 'querytitle' ), 
                 dataTableOutput_p("query_counts2",
                              tt('ts1'), tt('ts2'),
                              placement='top' )
               )
              ),
      tabPanel("Drug-Event Counts Over Time",  
               wellPanel( 
                 plotOutput_p( 'countplot',
                               tt('prr1'), tt('prr5'),
                               placement='left', height='600px' )
               )
      ),
      tabPanel("Other Apps",  
               wellPanel( 
                 htmlOutput( 'applinks' )
               )
      ),
      tabPanel('Data Reference', HTML( renderiframe( "https://open.fda.gov/drug/event/") ) 
      ),
      tabPanel('About', 
               img(src='l_openFDA.png'),
               HTML( (loadhelp('about') ) )  ),
              id='maintabs'
            )
          )
        )
      )
    )
