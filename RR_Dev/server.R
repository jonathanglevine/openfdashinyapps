require(shiny)
if (!require('openfda') ) {
  devtools::install_github("ropenhealth/openfda")
  library(openfda)
  print('loaded open FDA')
}
require(RColorBrewer)
require(wordcloud)
require(tm)

#source('helperfunctions.r')

source('sourcedir.R')


