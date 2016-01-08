require(shinyapps)
base <- 'C:/Users/levinej/Documents/R Script projects/openfdadev2105/deployshiny/'
dirlist <- list.dirs(full.names=FALSE, recursive=FALSE)
exclude<- c(
  ".git",
  ".Rproj.user",
  "016-knitr-pdf",
  "RR_D - dep",
  'tmp',
  "sharedscripts",
  "downloads"
)
app <- setdiff(dirlist, exclude)
# print(app)
# app <- c('RR_D', 
#          'RR_E'
#          )
for (i in seq_along( app ))
  {
  mypath <- app[i]
  destpath1 <-paste0( base,  app[i])
  if (!dir.exists(destpath1))
  {
    dir.create(destpath1)
  }
  # print(mypath)
  # print(destpath1)
  # print(flist)
  flist <- list.files( mypath, all.files = TRUE, 
                       ignore.case = TRUE, full.names = TRUE, no..=TRUE)
  file.copy(from=flist, to= destpath1, overwrite = TRUE)
  
  mypath <- paste0(app[i], '/www')
  destpath1 <-paste0( base, mypath)
  if (!dir.exists(destpath1))
  {
    dir.create(destpath1)
  }
  flist <- list.files( mypath, all.files = TRUE, 
                       ignore.case = TRUE, full.names = TRUE, no..=TRUE)
  file.copy(from=flist, to= destpath1, overwrite = TRUE)
  
  mypath <- 'sharedscripts'
  destpath1 <-paste0( base,   app[i], '/', mypath)
  if (!dir.exists(destpath1))
  {
    dir.create(destpath1)
  }
  flist <- list.files( mypath, all.files = TRUE, 
                       ignore.case = TRUE, full.names = TRUE, no..=TRUE)
  file.copy(from=flist, to= destpath1, overwrite = TRUE)
}