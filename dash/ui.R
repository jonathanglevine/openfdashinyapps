library(shiny)
require(shinyBS)

source('sourcedir.R')

# SOURCEDIR <- 'sharedscripts/'
# tipify(
#   selectInput("drugvar", 'Drug Variable' ,getdrugvarchoices()),
#   'openFDA Content'
# )



# getdrugvarchoices <- function(){
#   openfdavars <- c( 
#     'generic_name',
#     'substance_name',
#     'brand_name')
#   openfdavars <-  paste0( 'patient.drug.openfda.', openfdavars )
#   s <- c( openfdavars, 'patient.drug.medicinalproduct')
#   return(s)
# }



openfdavars <- getdrugvarchoices()

renderDrugName <- function() { 
  
  ( htmlOutput('drugname') )
  
} 
renderv1 <- function() { 
  
  ( uiOutput('v1_in') )
  
}  
renderdrugname <- function() { 
  browser()
  s <- ( ('drugname_in') )
  s <- strsplit(s, '<', fixed=TRUE)
  s <- strsplit(s[1], '>', fixed=TRUE)
  return( s[2] )
} 
rendert1 <- function() { 
  
  ( uiOutput('t1_in') )
  
} 
renderuseexact <- function() { 
  
  ( uiOutput('useexact_in') )
  
} 
shinyUI(fluidPage( 
                   fluidRow(
                     column(width=4,
                            a(href='https://open.fda.gov/', 
                              img(src='l_openFDA.png', align='bottom')),
                            renderDates()
                     ),
                     column(width=8,
                            titlePanel("Dashboard" ) )
                   ),
#                    img(src='l_openFDA.png'),
#                    titlePanel( 'Dashboard' ),
                    hr(),
  fluidRow(
   column(width=12,
          bsAlert("alert2"),
          HTML('<h4>Product</h4>'))
   ),
  fluidRow(
    column(width=4,
           selectInput_p("v1", 'Drug Variable' , openfdavars, 
                   HTML( tt('drugvar1') ), tt('drugvar2'),
                   placement='top')
           ), 
    column(width=3 ,
           conditionalPanel(
              condition = "1 == 2",
              textInput_p("t1", "Name of Drug", '', 
                   HTML( tt('drugname1') ), tt('drugname2'),
                    placement='bottom')
                  ),
#           wellPanel(
             bsButton("tabBut", "Select Drug...", style='primary'),
              renderDrugName(),
             radioButtons('useexact', 'Match drug name:', c('Exactly'='exact', 'Any Term'='any'),
                          selected='any', inline=TRUE)
#                 )
,  
           bsModal( 'modalExample', "Enter Variables", "tabBut", size = "small",
                              htmlOutput('mymodal'), 
                              textInput_p("drugname", "Name of Drug", '', 
                                          HTML( tt('drugname1') ), tt('drugname2'),
                                          placement='left'),
                              bsButton("update", "Update Variable", style='primary')
                    )
           ),
    column(width=5, 
          htmlOutput_p( 'alldrugtext' ,
          HTML( tt('gcount1') ), tt('gcount2'),
          placement='bottom'),
          htmlOutput_p( 'queryalldrugtext' ,
          HTML( tt('gquery1') ), tt('gquery2'),
          placement='bottom')
      )
    ),
  fluidRow(
    column(width=12,
           HTML('<h4>Product Summary</h4>'))),
  fluidRow(
    column(width=4,
           tabsetPanel(
             tabPanel("Table",
                        htmlOutput_p("source", 
                                      tt( 'freqtab1'), 
                                      tt( 'freqtab2') )
             ),
             tabPanel("Dotchart",
                        plotOutput_p("sourceplot", 
                                     HTML( tt('dot1') ), tt('dot2'), 
                                     height = "250px")
             ),
             tabPanel("Piechart",
                        plotOutput_p("sourcepie", 
                                     HTML( tt('pie1') ), tt('pie2'), height = "250px")
             ),
             id='maintabs', selected="Dotchart"
           )),
    column(width=4,
      tabsetPanel(
                tabPanel("Table",
                           htmlOutput_p("serious", 
                                        tt( 'freqtab1'), 
                                        tt( 'freqtab2'))
                  ),
                tabPanel("Dotchart",
                           plotOutput_p("seriousplot", 
                                        HTML( tt('dot1') ), tt('dot2'), 
                                        height = "250px")
                ),
                tabPanel("Piechart",
                           plotOutput_p("seriouspie", 
                                        HTML( tt('pie1') ), tt('pie2'), height = "250px")
                ),
                id='maintabs', selected="Dotchart"
                )
            ),
    column(width=4,
           tabsetPanel(
             tabPanel("Table",
                        htmlOutput_p("sex", 
                                     tt( 'freqtab1'), 
                                     tt( 'freqtab2'))
             ),
             tabPanel("Dotchart",
                        plotOutput_p("sexplot", 
                                   HTML( tt('dot1') ), tt('dot2'),
                                   height = "250px")
             ),
             tabPanel("Piechart",
                        plotOutput_p("sexpie", 
                                     HTML( tt('pie1') ), tt('pie2'), height = "250px")
             ),
             id='maintabs', selected="Dotchart"
           )
           )
  ),
  fluidRow(
    column(width=12,
           HTML('<h4>Adverse Events and Concomitant Medications</h4>'))
    ),
  fluidRow(
    column(width=9,
      tabsetPanel(


        tabPanel("Events",
                 wordcloudtabset('eventcloud', 'query', 
                    popheads=c( tt('event1'), tt('word1') ), poptext=c( tt('event2'), tt('word2') )
                 )
                ),
        tabPanel("Concomitant Medications",
                 wordcloudtabset('cocloud', 'coquery', 
                 popheads=c( tt('codrug1'), tt('word1') ), poptext=c( tt('codrug2'), tt('word2') )
                 )
            ),
        tabPanel("Indications",
                 wordcloudtabset('indcloud', 'indquery', 
                                 popheads=c( tt('indication1'), tt('word1') ), poptext=c( tt('indication2'), tt('word2') )
                 )
        ),
        tabPanel("Other Apps",  
                 wellPanel( 
                   htmlOutput_p( 'applinks' )
                 )
        ),
        tabPanel('Data Reference', HTML( renderiframe( "https://open.fda.gov/drug/event/") ) 
        ),
        tabPanel('About', 
                 img(src='l_openFDA.png'),
                 HTML( (loadhelp('about') ) )  )
        
      )
    ),
    column(width=3,
           bsAlert("alert") )
  )
)
)
