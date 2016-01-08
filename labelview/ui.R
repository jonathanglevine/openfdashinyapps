library(shiny)

source('sourcedir.R')



getchoices <- function(){
  openfdavars <- getallvars(allvars(), section = c('of') ) 
  openfdavars <-  paste0( 'openfda.', openfdavars )
  s <- c(openfdavars, 'effective_time' )
  selected <- getvarprefixs()
  s <- c( s, getallvars(allvars(), section = selected ) )
  s <- c('Any Variable', s, '_exists_' ) 
  return(s)
}

getdatechoices <- function(){
  s <- c( 'receivedate' )
  return(s)
}

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
                           titlePanel("Label Browser" ) )
                  ),
  fluidRow(
    column(width=2, bsButton( 'prevrow', '< Previous Label', style='primary') ),
    column(width=2, htmlOutput("ptext") ),
    column(width=6, 
           strong( rendercurrec() ) ),
    column(width=2, htmlOutput("ntext") ),
    column(width=2, bsButton( 'nextrow', 'Next Label>', style='primary') )
  ),
  fluidRow(
    column(width=12, 
             sliderInput('skip', 'Label #', value=1, min=1, step= 1, max=100, width='100%')
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
         selectizeInput('v1', 'Variable 1', getchoices() , width='100%', 
                        selected=getchoices()[1], options=list(create=TRUE, maxOptions=1000) ),
         textInput("t1", "Terms", '')
         )
        ,
         bsModal( 'modalExample1', "Enter Variables", "tabBut", size = "small",
                  htmlOutput('mymodal'), 
                  selectizeInput('v1_2', 'Variable 1', getchoices() , width='100%', 
                                 selected=getchoices()[1], options=list(create=TRUE, maxOptions=1000) ),
                  textInput("t1_2", "Terms", ''), 
                  selectizeInput('v2_2', 'Variable 2', getchoices() , width='100%', 
                                 selected=getchoices()[53], options=list(create=TRUE, maxOptions=1000) ),
                  textInput("t2_2", "Terms", ''),
                  selectizeInput("v3_2", "Variable 3", c( getdatechoices(), getchoices() ) , width='100%', 
                                 selected='effective_time' , options=list(create=TRUE, maxOptions=1000) ), 
                  textInput("t3_2", "Terms", '[20000101+TO+20170101]'),
                  bsButton("update", "Update Variables", style='primary') )
         )
       ,
       wellPanel( 
#         bsButton("tabBut2", "Change Values...", style='primary'),
         renderv2(),
         rendert2(),
         conditionalPanel(
           condition = "1 == 2",
         selectizeInput('v2', 'Variable 2', getchoices() , width='100%', 
                        selected=getchoices()[53], options=list(create=TRUE, maxOptions=1000) ),
         textInput("t2", "Terms", '')
         )
#          bsModal( 'modalExample2', "Enter Variables", "tabBut2", size = "small",
#                   htmlOutput('mymodal2'), 
#                   selectizeInput('v2_2', 'Variable 2', getchoices() , width='100%', 
#                                  selected=getchoices()[1], options=list(create=TRUE, maxOptions=1000) ),
#                   textInput("t2_2", "Terms", ''),
#                               bsButton("update2", "Update Variables", style='primary') )
         ),
       wellPanel( 
#         bsButton("tabBut3", "Change Values...", style='primary'),
         renderv3(),
         rendert3(),
         conditionalPanel(
           condition = "1 == 2",
         selectizeInput("v3", "Variable 3", c( getdatechoices(), getchoices() ) , width='100%', 
                        selected='effective_time' , options=list(create=TRUE, maxOptions=1000) ), 
         textInput("t3", "Terms", '[20000101+TO+20170101]')
          )
#        bsModal( 'modalExample3', "Enter Variables", "tabBut3", size = "small",
#                 htmlOutput('mymodal3'), 
#                 selectizeInput("v3_2", "Variable 3", c( getdatechoices(), getchoices() ) , width='100%', 
#                                selected='effective_time' , options=list(create=TRUE, maxOptions=1000) ), 
#                 textInput("t3_2", "Terms", '[20000101+TO+20170101]'),
#                 bsButton("update3", "Update Variables", style='primary') )
       ),
bsAlert("alert")
),
column(width=9,
       bsAlert("alert2"),                 
      tabsetPanel(
                tabPanel("Overview",  
                         
                         wellPanel( 
                           htmlOutput( 'overviewtitle' ), 
                           tableOutput( 'overviewtable' )
                         )
                ),
                tabPanel("Meta Data",  
                        
                         wellPanel( 
                           htmlOutput( 'querytitle' ), 
                           htmlOutput( 'metatext' ), 
                           htmlOutput( 'json' )
                         )
                ),
                tabPanel("ID and version", 
                         
                         wellPanel(  
                           htmlOutput( 'headertabletitle' ), 
                           tableOutput("headertable"),
                           hr()
                         )
                ),
                tabPanel("1. Indications, usage, and 2,3.dosage",  
                         
                         wellPanel( 
                           htmlOutput('indtitle'),
                           wordcloudtabset('indtabs', 'ind', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("4., 5. Warnings and precautions", 
                         
                         wellPanel(  
                           htmlOutput( 'warntabletitle' ), 
                           wordcloudtabset('warntabs', 'warn', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("6. Adverse effects and 7. interactions",  
                         
                         wellPanel(  
                           tableOutput('aeinttabletitle'),
                           wordcloudtabset('aetabs', 'aeint', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("8. Special populations", 
                         
                         wellPanel(  
                           htmlOutput('specialtabletitle'),
                           wordcloudtabset('specialtabs', 'special', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("10. Abuse and 9. overdosage",  
                         
                         wellPanel( 
                           htmlOutput('abuseodtabletitle'),
                           wordcloudtabset('abuseodtabs', 'abuseod', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("12. Clinical pharmacology",  
                         
                         wellPanel(  
                           htmlOutput('clinpharmtabletitle'),
                           wordcloudtabset('clinpharmtabs', 'clinpharm', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("13. Nonclinical toxicology", 
                         
                         wellPanel( 
                           
                           htmlOutput('nonclintoxtabletitle'),
                           wordcloudtabset('nonclintoxtabs', 'nonclintox', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("14. Clinical Studies and 15. References", 
                         
                         wellPanel(  
                           htmlOutput( 'referencetabletitle' ), 
                           wordcloudtabset('referencetabs', 'reference', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("16. Supply, storage, and handling", 
                         
                         wellPanel(  
                           htmlOutput( 'supplytabletitle' ), 
                           wordcloudtabset('supplytabs', 'supply', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("Patient information", 
                         
                         wellPanel(  
                           htmlOutput('patinfotabletitle'),
                           wordcloudtabset('patinfotabs', 'patinfo', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("Other",  
                         wellPanel( 
                           htmlOutput( 'othertabletitle' ),
                           wordcloudtabset('othertabs', 'other', names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("OpenFDA",  
                         
                         wellPanel(  
                           htmlOutput('patientdrugopenfdatabletitle'),
                           tableOutput( 'openfda' )
                         )
                ),
                tabPanel("Other Apps",  
                         wellPanel( 
                           htmlOutput_p( 'applinks' )
                         )
                ),
                tabPanel('Data Reference', HTML( renderiframe( "https://open.fda.gov/drug/label/") ) 
                ),
                tabPanel('About', 
                         img(src='l_openFDA.png'),
                         HTML( (loadhelp('about') ) )  )
                , type='pill',
              id='maintabs', position='right', selected="Overview"
            )
      )
    )
  )
)