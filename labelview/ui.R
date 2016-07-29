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

myselected <- "Overview"

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
         textInput("t1", "Terms", ''),
         selectizeInput('v2', 'Variable 2', getchoices() , width='100%', 
                        
                        selected='product_type', options=list(create=TRUE, maxOptions=1000) ),
         textInput("t2", "Terms", value = 'prescription'),
         
         selectizeInput("v3", "Variable 3", c( getdatechoices(), getchoices() ) , width='100%', 
                        selected='is_original_packager' , options=list(create=TRUE, maxOptions=1000) ), 
         textInput("t3", "Terms", value = 'yes')
         )
        ,
         bsModal( 'modalExample1', "Enter Variables", "tabBut", size = "small",
                  htmlOutput('mymodal'), 
                  selectizeInput('v1_2', 'Variable 1', getchoices() , width='100%', 
                                 selected=getchoices()[1], options=list(create=TRUE, maxOptions=1000) ),
                  textInput("t1_2", "Terms", ''), 
                  
                  selectizeInput('v2_2', 'Variable 2', getchoices() , width='100%', 
                                 selected='product_type', options=list(create=TRUE, maxOptions=1000) ),
                  textInput("t2_2", "Terms", value = 'prescription'),
                  
                  selectizeInput("v3_2", "Variable 3", c( getdatechoices(), getchoices() ) , width='100%', 
                                 selected='is_original_packager' , options=list(create=TRUE, maxOptions=1000) ), 
                  textInput("t3_2", "Terms", 'yes'),
                  bsButton("update", "Update Variables", style='primary') )
         )
       , 
       wellPanel( 
         renderv2(),
         rendert2()
       )
       , 
       wellPanel( 
         renderv3(),
         rendert3()
       )
       ,
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
                           wordcloudtabset('indtabs', 'ind', types=c('table', 'table'), names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("4., 5. Warnings and precautions", 
                         
                         wellPanel(  
                           htmlOutput( 'warntabletitle' ), 
                           wordcloudtabset('warntabs', 'warn', types=c('table', 'table'), names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("6. Adverse effects and 7. interactions",  
                         
                         wellPanel(  
                           tableOutput('aeinttabletitle'),
                           wordcloudtabset('aetabs', 'aeint', types=c('table', 'table'), names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("8. Special populations", 
                         
                         wellPanel(  
                           htmlOutput('specialtabletitle'),
                           wordcloudtabset('specialtabs', 'special', types=c('table', 'table'), names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("10. Abuse and 9. overdosage",  
                         
                         wellPanel( 
                           htmlOutput('abuseodtabletitle'),
                           wordcloudtabset('abuseodtabs', 'abuseod', types=c('table', 'table'), names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("12. Clinical pharmacology",  
                         
                         wellPanel(  
                           htmlOutput('clinpharmtabletitle'),
                           wordcloudtabset('clinpharmtabs', 'clinpharm', types=c('table', 'table'), names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("13. Nonclinical toxicology", 
                         
                         wellPanel( 
                           
                           htmlOutput('nonclintoxtabletitle'),
                           wordcloudtabset('nonclintoxtabs', 'nonclintox', types=c('table', 'table'), names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("14. Clinical Studies and 15. References", 
                         
                         wellPanel(  
                           htmlOutput( 'referencetabletitle' ), 
                           wordcloudtabset('referencetabs', 'reference', types=c('table', 'table'), names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("16. Supply, storage, and handling", 
                         
                         wellPanel(  
                           htmlOutput( 'supplytabletitle' ), 
                           wordcloudtabset('supplytabs', 'supply', types=c('table', 'table'), names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("Patient information", 
                         
                         wellPanel(  
                           htmlOutput('patinfotabletitle'),
                           wordcloudtabset('patinfotabs', 'patinfo', types=c('table', 'table'), names=c('Text', 'Tables') )
                         )
                ),
                tabPanel("Other",  
                         wellPanel( 
                           htmlOutput( 'othertabletitle' ),
                           wordcloudtabset('othertabs', 'other', types=c('table', 'table'), names=c('Text', 'Tables') )
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
                         HTML( (loadhelp('about') ) )  ),
                tabPanel(tabnames[ 19 ],
                         numericInput("downloadstart", "Start Record", value = 1, min=0, step=1),
                         downloadButton('downloadData', 'Download 100 records starting at "Start Record"'),
                        radioButtons('download', 'Select Section to Download', tabnames[c (1, 3:15) ], selected = 'Overview')
                )
                , type='tabs',
              id='maintabs', position='right', selected=myselected
            )
      )
    )
  )
)