require(shinyapps)
base <- 'C:/Users/levinej/Documents/R Script projects/openFDA/deployshiny/'
dirlist <- list.dirs(full.names=FALSE, recursive=FALSE)
exclude<- c(
  ".git",
  ".Rproj.user",
  "016-knitr-pdf",
  "RR_D - dep",
  'tmp',
  "sharedscripts"
)
app <- setdiff(dirlist, exclude)
app <- c(
# "510kview", 
#  "ChangePoint", 
# "dash" ,  
#  "deviceclassview",
# "deviceenforceview",
# "devicerecallview" ,
# "devicereglist",  
# "devicereports" ,   
#  "drugenforceview", 
# "dynprr" ,
# "foodrecallview" , 
# "labelview"  ,   
"LRTest",   
# "LR_D_Activesubstancename" ,   
#  "LR_E_Activesubstancename" ,          
"LRTest_E" ,   
# "PMAview" ,         
#"reportview" ,    
#   "RR_D" ,      
#    "RR_D_Activesubstance" ,     
#    "RR_E_Activesubstance" ,      
#    "dynprr_Activesubstance" ,         
# "RR_Dev"  ,     
# "RR_E",
NULL
)
for ( i in seq_along(app))
  {
  deployApp(app[i])
}
# for ( i in seq_along(app))
# {
#   deployApp(app[i], lint=FALSE)
# }