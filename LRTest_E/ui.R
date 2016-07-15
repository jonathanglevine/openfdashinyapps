library(shiny)

source('sourcedir.R')


renderDrugName <- function() { 
  
  ( htmlOutput('drugname') )
  
} 
renderLimit <- function() { 
  ( htmlOutput('limit') )
  
}  

renderStart <- function() { 
  
  ( htmlOutput('start') )
  
}  

renderNumsims <- function() { 
  
  ( htmlOutput('numsims') )
  
}  

shinyUI(fluidPage(
                  fluidRow(
                    column(width=4,
                           a(href='https://open.fda.gov/', 
                             img(src='l_openFDA.png', align='bottom') ),
                           renderDates()
                    ),
                    column(width=8,
                           titlePanel("LRT Signal Analysis for an Event" ) )
                  ),
  sidebarLayout(
    sidebarPanel(
                 selectInput_p("v1", 'Drug Variable' ,getdrugvarchoices(), 
                               HTML( tt('drugvar1') ), tt('drugvar2'),
                               placement='top'), 
                 conditionalPanel(
                   condition = "1 == 2",
                   textInput_p("t1", "Event Name", 'NEPHROGENIC SYSTEMIC FIBROSIS', 
                               HTML( tt('drugname1') ), tt('drugname2'),
                               placement='bottom'), 
                   
                   numericInput_p('limit', 'Maximum number of event terms', 50,
                                  1, 100, step=1, 
                                  HTML( tt('limit1') ), tt('limit2'),
                                  placement='bottom'), 
                   
                   numericInput_p('start', 'Rank of first drug', 1,
                                  1, 999, step=1, 
                                  HTML( tt('limit1') ), tt('limit2'),
                                  placement='bottom'),
                   
                   numericInput_p('numsims', 'Number of Simulations', 1000,
                                  1000, 50000, step=1, 
                                  HTML( tt('numsims1') ), tt('numsims2'),
                                  placement='bottom')
                 ),
                 wellPanel(
                   bsButton("tabBut", "Select Drug, # of Events, and # of simulations...", 
                            style='primary'),
                   br(),
                   renderDrugName(),
                   radioButtons('useexact', 'Match event name:', c('Exactly'='exact', 'Any Term'='any'), selected='any'),
                   renderLimit(),
                   renderStart(),
                   renderNumsims()
                 ), 
                 dateRangeInput('daterange', 'Use Reports Between: ', start = '1989-6-30', end = Sys.Date()),
                 bsModal( 'modalExample', "Enter Variables", "tabBut", size = "small",
                          htmlOutput('mymodal'), 
                          textInput_p("drugname", "Name of Event", 'NEPHROGENIC SYSTEMIC FIBROSIS', 
                                      HTML( tt('drugname1') ), tt('drugname2'),
                                      placement='left'), 
                          numericInput_p('limit2', 'Maximum number of event terms', 50,
                                         1, 100, step=1, 
                                         HTML( tt('limit1') ), tt('limit2'),
                                         placement='left'),
                          
                           numericInput_p('start2', 'Rank of first drug', 1,
                                         1, 999, step=1, 
                                         HTML( tt('limit1') ), tt('limit2'),
                                         placement='left'),
                          
                          numericInput_p('numsims2', 'Number of Simulations', 1000,
                                         1000, 50000, step=1, 
                                         HTML( tt('numsims1') ), tt('numsims2'),
                                         placement='bottom'),
                          bsButton("update", "Update Variables", style='primary')
                          ),
                          #          dateRangeInput('daterange2', 'Date Report Was First Received by FDA.', start = '1989-6-30', end = Sys.Date() ),
                          
                 wellPanel(
                   helpText( HTML('<b>Down Load Report</b>') ),
                   radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                inline = TRUE),
                   downloadButton('downloadReport', 'Download LRT Report')
                   ),
                 
                 bsAlert("alert")
                 ,
                 HTML( (loadhelp('LRT') ) )  
)
        ,
    mainPanel(
      
      bsAlert("alert2"),
      tabsetPanel(
                tabPanel("LRT Results based on Total Drugs",
                         wellPanel(
                         htmlOutput( 'prrtitle' ), 
                         helpText('Results sorted by LRR')
                         ),
#                          wordcloudtabset('cloudprr', 'prr', 
#                                          popheads=c( tt('prr1'), tt('word1') ), 
#                                          poptext=c( tt('prr5'), tt('word2') ) )
                         maketabset( c('prr', 'cloudprr', 'textplot'), 
                                     names=c("Table","Word Cloud", "Text Plot"),
                                     types=c('html', "plot", 'plot') )
                ),
              tabPanel("Simulation Results for Drug Based LRT",
                       wellPanel( 
                         plotOutput( 'simplot')
                        )
              ),
              tabPanel("Analyzed Event Counts for Drug"   ,
                       wellPanel( 
                         htmlOutput( 'alldrugtextAnalyzedEventCountsforDrug' ),
                         htmlOutput_p( 'alldrugqueryAnalyzedEventCountsforDrug' ,
                                       tt('gquery1'), tt('gquery2'),
                                       placement='bottom' )
                       ), 
                       wellPanel( 
                         htmlOutput( 'titleAnalyzedEventCountsforDrug' ), 
                         #                          tableOutput("query"),
                         htmlOutput_p( 'queryAnalyzedEventCountsforDrug' ,
                                       tt('gquery1'), tt('gquery2'),
                                       placement='bottom' )
                       ),
                       wordcloudtabset('cloudAnalyzedEventCountsforDrug', 'AnalyzedEventCountsforDrug'
                       )
              ),
                tabPanel("Analyzed Drug Counts for All Events",
                         wellPanel( 
                           htmlOutput( 'alltext' ),
                           htmlOutput_p( 'queryalltext' ,
                                       tt('gquery1'), tt('gquery2'),
                                       placement='bottom' )
                         ),
                         wellPanel(
                           htmlOutput( 'alltitle' ), 
                         htmlOutput_p( 'allquerytext' ,
                                     tt('gquery1'), tt('gquery2'),
                                     placement='bottom' ) ),
                         wordcloudtabset('cloudall', 'all')
                ),
                tabPanel("Counts For Events In Selected Reports",
                         wellPanel( 
                           htmlOutput( 'cotext' ),
                           htmlOutput_p( 'querycotext' ,
                                       tt('gquery1'), tt('gquery2'),
                                       placement='bottom' )
                         ),
                         wellPanel(
                           htmlOutput( 'cotitle' )
                           ),
                         htmlOutput_p( 'coquerytext' ,
                                     tt('gquery1'), tt('gquery2'),
                                     placement='bottom' ),
                         wordcloudtabset('cloudcoquery', 'coquery')
                 ),
                tabPanel("Drug Counts for Event",
                         wellPanel( 
                           htmlOutput( 'cotextE' ),
                           htmlOutput_p( 'querycotextE' ,
                                         tt('gquery1'), tt('gquery2'),
                                         placement='bottom' )
                         ),
                         wellPanel(
                           htmlOutput( 'cotitleD' )
                         ),
                         wellPanel(
                           htmlOutput( 'cotitleEex' )
 #                          htmlOutput( 'coqueryEex' )
                         ),
#                          htmlOutput_p( 'coquerytextE' ,
#                                        tt('gquery1'), tt('gquery2'),
#                                        placement='bottom' ),
                         wordcloudtabset('coqueryEex', 'coqueryE', names=c('Tables', 'Tables'),
                                         popheads=c( tt('codrug1'), tt('word1') ), 
                                         poptext=c( tt('codrug3'), tt('word2') ))
                ),
                tabPanel("Counts For All Drugs",
                         wellPanel( 
                           htmlOutput( 'cotextA' ),
                           htmlOutput_p( 'querycotextA' ,
                                         tt('gquery1'), tt('gquery2'),
                                         placement='bottom' )
                         ),
                         wellPanel(
                           htmlOutput( 'cotitleA' )
                         ),
                         htmlOutput_p( 'coquerytextA' ,
                                       tt('gquery1'), tt('gquery2'),
                                       placement='bottom' ),
                         wordcloudtabset('cloudcoqueryA', 'coqueryA',
                                         popheads=c( tt('codrug1'), tt('word1') ), 
                                         poptext=c( tt('codrug3'), tt('word2') ))
                ),
                tabPanel("Counts For Indications In Selected Reports",
                         wellPanel( 
                           htmlOutput( 'indtext' ),
                           htmlOutput_p( 'queryindtext' ,
                                       tt('gquery1'), tt('gquery2'),
                                       placement='bottom' )
                         ),
                         wellPanel(
                           htmlOutput( 'indtitle' )
                         ),
                         wordcloudtabset('cloudindquery', 'indquery',
                                         popheads=c( tt('indication1'), tt('word1') ),
                                         poptext=c( tt('indication2'), tt('word2') ) )
                ),
                tabPanel("Other Apps",  
                         wellPanel( 
                           htmlOutput( 'applinks' )
                         )
                ),
                tabPanel('Data Reference', HTML( renderiframe('https://open.fda.gov/drug/event/')   )  ),
                tabPanel('About', 
                         img(src='l_openFDA.png'),
                         HTML( (loadhelp('about') ) )  ),
#                 tabPanel("session",  
#                          wellPanel( 
#                            verbatimTextOutput( 'urlquery' )
#                          )
#                 ),
              id='maintabs'
            )
          )
        )
      )
    )
