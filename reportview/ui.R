library(shiny)

source('sourcedir.R')

getchoices <- function(){
  othervars <- c( 'Any Variable',  '_exists_')
  
  openfdavars <-  getallvars( allvars(), mytype = 'text', section= c('of', 'o2'))
  openfdavars <-  paste0( 'patient.drug.openfda.', openfdavars )
  
  headervars <- getallvars( allvars(), mytype = 'text', section= c('rh', 'se' ))
  
  patientvars <- getallvars( allvars(), mytype = 'text', section= c('pt', 'na'))
  patientvars <-  paste0( 'patient.', patientvars )
  
  drugvars <- getallvars( allvars(), mytype = 'text', section= c('dr', 'as'))
  drugvars <-  paste0( 'patient.drug.', drugvars )
  
  reactionvars <- getallvars( allvars(), mytype = 'text', section= c('re'))
  reactionvars <-  paste0( 'patient.reaction.', reactionvars )

  exactvars <- paste0( c( getdrugvarchoices(), 'patient.reaction.reactionmeddrapt' ), '.exact')
  
  s <- c(othervars, openfdavars, headervars,  patientvars, drugvars, reactionvars, exactvars)
  return(s)
}

getdatechoices <- function(){
  s <- c( 'receiptdate','receivedate' )
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
                           a(href='https://open.fda.gov/', 
                             img(src='l_openFDA.png', align='bottom')),
                           renderDates()
                    ),
                    column(width=8,
                           titlePanel("Drug Adverse Event Report Browser" ) )
                  ),
#                   img(src='l_openFDA.png'),
#   titlePanel("Report Browser"),
#   fluidRow(
#     column(width=4,
#            wellPanel( 
#              selectizeInput('v1', 'Variable 1', getchoices() , width='100%', 
#                             selected=getchoices()[2], options=list(create=TRUE, maxOptions=1000) ),
#              textInput("t1", "Terms", '')
#            )
#     ),
#     
#     column(width=4,
#            wellPanel( 
#              selectizeInput('v2', 'Variable 2', getchoices() , width='100%', 
#                             selected=getchoices()[1], options=list(create=TRUE, maxOptions=1000) ),
#              textInput("t2", "Terms", '')
#              #Haemoglobinuria
#            )
#     ),
#     column(width=4,
#            wellPanel( 
#              selectizeInput("v3", "Variable 3", c( getdatechoices(), getchoices() ) , width='100%', 
#                             selected=getdatechoices()[1] , options=list(create=TRUE, maxOptions=1000) ), 
#              textInput("t3", "Terms", paste0('[19060630+TO+', format(Sys.Date(), '%Y%m%d'), ']') )
#            )
#     )
#   ),
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
             selectizeInput('v1', 'Variable 1', getchoices() , width='100%', 
                            selected=getchoices()[1], options=list(create=TRUE, maxOptions=1000) ),
             textInput("t1", "Terms", '')
           )
           ,
           bsModal( 'modalExample1', "Enter Variables", "tabBut", size = "large",
                    htmlOutput('mymodal'), 
                    selectizeInput('v1_2', 'Variable 1', getchoices() , width='100%', 
                                   selected=getchoices()[1], options=list(create=TRUE, maxOptions=1000) ),
                    textInput("t1_2", "Terms", ''),
                    selectizeInput('v2_2', 'Variable 2', getchoices() , width='100%', 
                                   selected=getchoices()[1], options=list(create=TRUE, maxOptions=1000) ),
                    textInput("t2_2", "Terms", ''),
                    selectizeInput("v3_2", "Variable 3", c( getdatechoices(), getchoices() ) , width='100%', 
                                   selected='effective_time' , options=list(create=TRUE, maxOptions=1000) ), 
                    textInput("t3_2", "Terms", '[20000101+TO+20170101]'),
                    bsButton("update", "Update Variables", style='primary') )
         )
         ,
         wellPanel( 
           renderv2(),
           rendert2(),
           conditionalPanel(
             condition = "1 == 2",
             selectizeInput('v2', 'Variable 2', getchoices() , width='100%', 
                            selected=getchoices()[1], options=list(create=TRUE, maxOptions=1000) ),
             textInput("t2", "Terms", '')
           )
         ),
         wellPanel( 
           renderv3(),
           rendert3(),
           conditionalPanel(
             condition = "1 == 2",
             selectizeInput("v3", "Variable 3", c( getdatechoices(), getchoices() ) , width='100%', 
                            selected='effective_time' , options=list(create=TRUE, maxOptions=1000) ), 
             textInput("t3", "Terms", paste0('[19060630+TO+', format(Sys.Date(), '%Y%m%d'), ']') )
           )
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
                tabPanel("Report Header", 
                         
                         wellPanel(  
                           htmlOutput( 'headertabletitle' ), 
                           tableOutput("headertable"),
                           hr(),
                           htmlOutput('receivertabletitle'),
                           tableOutput("receiver"),
                           hr(),
                           
                           htmlOutput('reportduplicatetabletitle'),
                           tableOutput("reportduplicate"),
                           hr(),
                           
                           htmlOutput('sendertabletitle'),
                           tableOutput("sender"),
                           hr(),
                           
                           htmlOutput('primarysourcetabletitle'),
                           tableOutput("primarysource")
                         )
                ),
                tabPanel("Patient",  
                         
                         wellPanel( 
                           htmlOutput('patienttabletitle'),
                           htmlOutput( 'patient' )
                         )
                ),
                tabPanel("Patient.Reaction",  
                         
                         wellPanel( 
                           htmlOutput('patientreactiontabletitle'),
                           htmlOutput( 'patientreaction')
                         )
                ),
                tabPanel("Patient.Drug",  
                         
                         wellPanel(  
                           htmlOutput('patientdrugtabletitle'),
                           htmlOutput( 'drug' )
                         )
                ),
                tabPanel("Patient.Drug.OpenFDA",  
                         
                         wellPanel(  
                                     htmlOutput('patientdrugopenfdatabletitle'),
                                     tableOutput( 'openfda' ),
                                     htmlOutput('patientdrugopenfda2tabletitle'),
                                     tableOutput( 'openfda2' )
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