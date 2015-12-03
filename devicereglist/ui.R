library(shiny)

source('sourcedir.R')
rendercurrec <- function() { 
  
  uiOutput('currec') 
  
} 
renderrepportid <- function() { 
  
  uiOutput('reportid') 
  
} 

renderv1 <- function() { 
  
  ( htmlOutput('v1') )
  
} 
renderv2 <- function() { 
  
  ( htmlOutput('v2') )
  
}  
renderv3 <- function() { 
  
  ( htmlOutput( 'v3' ) )
  
} 
rendert1 <- function() { 
  
  ( htmlOutput('t1') )
  
} 
rendert2 <- function() { 
  
  ( htmlOutput('t2') )
  
}  
rendert3 <- function() { 
  
  ( htmlOutput( 't3' ) )
  
} 
shinyUI(fluidPage(
                  fluidRow(
                    column(width=4,
                           a(href='https://open.fda.gov', 
                             img(src='l_openFDA.png', align='bottom')),
                           renderDates()
                    ),
                    column(width=8,
                           titlePanel("Registration and Listing Browser" ) )
                  ),
  fluidRow(
    column(width=2, bsButton( 'prevrow', '< Previous Report', style = 'primary') ),
    column(width=2, htmlOutput("ptext") ),
    column(width=6, 
           strong( rendercurrec() ) ),
    column(width=2, htmlOutput("ntext") ),
    column(width=2, bsButton( 'nextrow', 'Next Report>', style = 'primary') )
  ),
  fluidRow(
    column(width=12, 
           wellPanel( 
             sliderInput('skip', 'Report #', value=1, min=1, step= 1, max=100, width='100%')
           )
    )
  ),
fluidRow(
  column(width=3,
         wellPanel( 
           bsButton("tabBut", "Filter by...", style='primary'),
           br(),
           renderv1(),
           rendert1(),
           conditionalPanel(
             condition = "1 == 2",
             selectizeInput('v1', 'Variable 1', getchoicevars() , width='100%', 
                            selected=getchoicevars()[1], options=list(create=TRUE, maxOptions=1000) ),
             textInput("t1", "Terms", '')
           )
           ,
           bsModal( 'modalExample1', "Enter Variables", "tabBut", size = "small",
                    htmlOutput('mymodal'), 
                    selectizeInput('v1_2', 'Variable 1', getchoicevars() , width='100%', 
                                   selected=getchoicevars()[1], options=list(create=TRUE, maxOptions=1000) ),
                    textInput("t1_2", "Terms", ''),
                    selectizeInput('v2_2', 'Variable 2', getchoicevars() , width='100%', 
                                   selected=getchoicevars()[2], options=list(create=TRUE, maxOptions=1000) ),
                    textInput("t2_2", "Terms", ''),
                    selectizeInput("v3_2", "Variable 3", c('products.created_date', getchoicevars() ) , width='100%', 
                                   selected='products.created_date' , options=list(create=TRUE, maxOptions=1000) ), 
                    textInput("t3_2", "Terms", '[20000101+TO+20170101]'),
                    bsButton("update", "Update Variables", style='primary') )
         )
         ,
         wellPanel( 
           renderv2(),
           rendert2(),
           conditionalPanel(
             condition = "1 == 2",
             selectizeInput('v2', 'Variable 2', getchoicevars() , width='100%', 
                            selected=getchoicevars()[1], options=list(create=TRUE, maxOptions=1000) ),
             textInput("t2", "Terms", '')
           )
         ),
         wellPanel( 
           renderv3(),
           rendert3(),
           conditionalPanel(
             condition = "1 == 2",
             selectizeInput("v3", "Variable 3", c( 'products.created_date', getchoicevars() ) , width='100%', 
                            selected='products.created_date' , options=list(create=TRUE, maxOptions=1000) ), 
             textInput("t3", "Terms", paste0('[19060630+TO+', format(Sys.Date(), '%Y%m%d'), ']') )
           )
         ),
         bsAlert("alert")
  ),
  column(width=9, 
         bsAlert("alert2"),  
      tabsetPanel(
                tabPanel("OpenFDA",  
                         
                         wellPanel(  
                           htmlOutput('openfdatabletitle'),
                           tableOutput( 'openfda' )
                         )
                ),
                tabPanel("Meta Data",  
                        
                         wellPanel( 
                           htmlOutput( 'querytitle' ), 
                           htmlOutput( 'metatext' ), 
                           htmlOutput( 'querytext' ), 
                           htmlOutput( 'json' )
                         )
                ),
                tabPanel("Product", 
                         
                         wellPanel(  
                           htmlOutput( 'overviewtabletitle' ), 
                           tableOutput("overviewtable")
#                            hr(),
#                            htmlOutput('receivertabletitle'),
#                            tableOutput("receiver"),
#                            hr(),
#                            
#                            htmlOutput('reportduplicatetabletitle'),
#                            tableOutput("reportduplicate"),
#                            hr(),
#                            
#                            htmlOutput('sendertabletitle'),
#                            tableOutput("sender"),
#                            hr(),
#                            
#                            htmlOutput('primarysourcetabletitle'),
#                            tableOutput("primarysource")
                         ),
                          wellPanel( 
                           htmlOutput('ownertabletitle'),
                           htmlOutput( 'owner')
                         )
                ),
                tabPanel("Registration",  
                         
                         wellPanel( 
                           htmlOutput('registrationtabletitle'),
                           htmlOutput( 'registration')
                         )
                ),
                tabPanel("Other Apps",  
                         wellPanel( 
                           htmlOutput( 'applinks' )
                         )
                ),
                tabPanel('Data Reference', HTML( renderiframe( "https://open.fda.gov/device/registrationlisting/") ) 
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