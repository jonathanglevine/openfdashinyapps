library(shiny)


source( 'sourcedir.R')

# getdrugvarchoices <- function(){
#   openfdavars <- c( 
#     'generic_name',
#     'substance_name',
#     'brand_name')
#   openfdavars <-  paste0( 'patient.drug.openfda.', openfdavars )
#   s <- c( openfdavars, 'patient.drug.medicinalproduct')
#   return(s)
# }

renderEventName <- function() { 
  
  ( htmlOutput('eventname') )
  
} 
renderLimit <- function() { 
  
  ( htmlOutput('limit') )
  
}  
renderStart <- function() { 
  
  ( htmlOutput('start') )
  
}  
shinyUI(fluidPage(
                  fluidRow(
                    column(width=4, 
                           a(href='https://open.fda.gov', 
                             img(src='l_openFDA.png', align='bottom')),
                           renderDates()
                           ),
                    column(width=8,
                          titlePanel("RR-Event"))
                    ),
#                  HTML( paste( "<img src='l_openFDA.png'/>" ,  "<h4>RR-Event</h4>" ) ),
                  sidebarLayout(
                    sidebarPanel(
                      tabsetPanel(
                        tabPanel('Select Reaction',
                                 selectInput_p("v1", 'Drug Variable' ,getdrugvarchoices(), 
#                                               HTML( tt('drugvar1') ), tt('drugvar2'),
                                               placement='top'),
                                 conditionalPanel(
                                   condition = "1 == 2", 
                                   textInput_p("t1", "Adverse Reaction", '', 
                                               HTML( tt('eventname1') ), tt('eventname2'),
                                               placement='bottom'), 
                                   
                                   numericInput_p('limit', 'Maximum number of drugs', 50,
                                                  1, 100, step=1,
                                                  HTML( tt('limit1') ), tt('limit2'),
                                                  placement='bottom'), 
                                   
                                   numericInput_p('start', 'Rank of first event', 1,
                                                  1, 999, step=1, 
                                                  HTML( tt('limit1') ), tt('limit2'),
                                                  placement='bottom')
                                 ),
                                 wellPanel(
                                   bsButton("tabBut", "Select Event and # of Drugs", 
                                            style='primary'),
                                   br(),
                                   renderEventName(),
                                   radioButtons('useexact', 'Match Event Term:', c('Exactly'='exact', 'Any Term'='any'), selected='any'),
                                   renderLimit(),
                                   renderStart()
                                 ), 
                                 dateRangeInput('daterange', 'Use Reports Between: ', start = '1989-6-30', end = Sys.Date()),
                                 bsModal( 'modalExample', "Enter Variables", "tabBut", size = "small",
                                          htmlOutput('mymodal'), 
                                          textInput_p("drugname", "Adverse Reaction", '', 
                                                      HTML( tt('eventname1') ), tt('eventname2'),
                                                      placement='left'), 
                                          
                                          numericInput_p('limit2', 'Maximum number of drugs', 50,
                                                         1, 100, step=1,
                                                         HTML( tt('limit1') ), tt('limit2'),
                                                         placement='left'),
                                          
                                          numericInput_p('start2', 'Rank of first event', 1,
                                                         1, 999, step=1, 
                                                         HTML( tt('limit1') ), tt('limit2'),
                                                         placement='bottom'),
                                          #          dateRangeInput('daterange2', 'Date Report Was First Received by FDA.', start = '1989-6-30', end = Sys.Date() ),
                                          bsButton("update", "Update Variables", style='primary')),
                                 bsAlert("alert"),
                                 HTML( (loadhelp('overviewside') ) )  )
                        ,
                        id='sidetabs', selected='Select Reaction')
                    ),
                    mainPanel(
                      tags$head(
                        tags$style(type="text/css", "select { max-width: 200px; }"),
                        tags$style(type="text/css", "textarea { max-width: 185px; }"),
                        tags$style(type="text/css", ".jslider { max-width: 200px; }"),
                        tags$style(type='text/css', ".well { max-width: 900px; }"),
                        tags$style(type='text/css', ".span4 { max-width: 310px; }")
                      ),
                      bsAlert("alert2"),
                      tabsetPanel(
                        tabPanel("PRR and ROR Results",
#                                  wellPanel(
#  #                                  helpText('Results sorted by PRR')
#                                  )
                                  htmlOutput( 'prrtitle' ),
                                # wordcloudtabset('cloudprr', 'prr')
#                                 wordcloudtabset('cloudprr', 'prr', 
#                                 popheads=c( tt('prr1'), tt('word1') ), 
#                                 poptext=c( tt('prr_E'), tt('word2') )
#                               )
                                 maketabset( c('prr2', 'cloudprr', 'textplot'), 
                                             types=c('datatable', "plot", 'plot'),
                                             names=c("Table","Word Cloud", "Text Plot")
                                             , 
                                             popheads = c(tt('prr1'), tt('word1'), tt('textplot1') ), 
                                             poptext = c( tt('prr5'), tt('wordPRR'), tt('textplot2') ) 
                                             )
                        ),
                        tabPanel("Analyzed Drug Counts for Specified Events"   ,
                                 wellPanel( 
                                   htmlOutput( 'alldrugtext' ),
                                   htmlOutput_p( 'queryalldrugtext' ,
                                                 tt('gquery1'), tt('gquery2'),
                                                 placement='bottom' )
                                 ), 
                                 wellPanel( 
                                   htmlOutput( 'querytitle' ), 
                                   htmlOutput_p( 'querytext' ,
                                               tt('gquery1'), tt('gquery2'),
                                               placement='bottom' )
                                 ),
                                 wordcloudtabset('cloudquery', 'specifieddrug2',
                                                 types= c('datatable', 'plot'), 
                                                 popheads=c( tt('drug1'), tt('word1') ), 
                                                 poptext=c( tt('codrug1a'), tt('word2') ))
                        ),
                        tabPanel("Analyzed Drug Counts for All Events",
                                 wellPanel( 
                                   htmlOutput( 'alltext' ),
                                   htmlOutput_p( 'queryalltext' ,
                                               tt('gquery1'), tt('gquery2'),
                                               placement='bottom' )
                                 ),
                                 wellPanel(
                                   htmlOutput( 'alltitle' )
                                 ), 
                                 htmlOutput_p( 'allquerytext' ,
                                             tt('gquery1'), tt('gquery2'),
                                             placement='bottom' ),
                                 wordcloudtabset('cloudall', 'all2',
                                                 types= c('datatable', 'plot'), 
                                                 popheads=c( tt('drug1'), tt('word1') ), 
                                                 poptext=c( tt('codrug1a'), tt('word2') ))
                        ),
                        tabPanel("Ranked Drug Counts for Event",
                                 wellPanel( 
                                   htmlOutput( 'cotextE' ),
                                   htmlOutput_p( 'querycotextE' ,
                                                 tt('gquery1'), tt('gquery2'),
                                                 placement='bottom' )
                                 ),
                                 wellPanel(
                                   htmlOutput( 'cotitleD' )
                                 ),
#                                  htmlOutput_p( 'coquerytextE' ,
#                                                tt('gquery1'), tt('gquery2'),
#                                                placement='bottom' ),
                                 wordcloudtabset('cloudcoqueryE', 'coqueryE2',
                                                 types= c('datatable', 'plot'), 
                                                 popheads=c( tt('codrug1'), tt('word1') ), 
                                                 poptext=c( tt('codrug3'), tt('word2') ))
                        ),
                        tabPanel("Counts For Events In Selected Reports",
                                 wellPanel( 
                                   htmlOutput( 'cotext' ),
                                   htmlOutput_p( 'querycotext' ,
                                               tt('gquery1'), tt('gquery2'),
                                               placement='bottom' )
                                 ),
                                 wellPanel(
                                   htmlOutput( 'cotitle' ), 
                                   htmlOutput_p( 'coquerytext'  ,
                                               tt('gquery1'), tt('gquery2'),
                                               placement='bottom') ) ,
                                 wordcloudtabset('cloudcoquery', 'coquery2', 
                                                 types= c('datatable', 'plot'), 
                                                 popheads=c( tt('event1'), tt('word1') ), 
                                                 poptext=c( tt('event2'), tt('word2') ))
                        ),
                        tabPanel("Counts For Indications In Selected Reports",
                                 wellPanel( 
                                   htmlOutput( 'indtext' ),
                                   htmlOutput_p( 'queryindtext'  ,
                                               tt('gquery1'), tt('gquery2'),
                                               placement='bottom')
                                 ),
                                 wellPanel(
                                   htmlOutput( 'indtitle' ) 
                                 ),
                                 wordcloudtabset('cloudindquery', 'indquery2',
                                                 types= c('datatable', 'plot'), 
                                                 popheads=c( tt('indication1'), tt('word1') ),
                                                 poptext=c( tt('indication2'), tt('word2') ) )
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
                        id='maintabs', selected="PRR and ROR Results"
                      )
                    )
                  )
)
)
