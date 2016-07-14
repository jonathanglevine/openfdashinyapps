
SOURCEDIR <- '../sharedscripts/'
if (!file.exists( paste0( SOURCEDIR, 'serverhelpers.R') ))
{
  SOURCEDIR <- 'sharedscripts/'
}
DATADIR <- '../activesubstancedata/'
if (!file.exists( paste0( DATADIR, 'cleanmpmap.RData') ))
{
  DATADIR <- 'activesubstancedata/'
}
source( paste0( SOURCEDIR, 'prr_dl.R') )
source( paste0( SOURCEDIR, 'helpfunctions.r') )
#source( paste0( SOURCEDIR, 'buildurlfun2.R') )
source( paste0( SOURCEDIR, 'serverhelpers.R') )
source( paste0( SOURCEDIR, 'uihelpers.R') )
source( paste0( SOURCEDIR, 'getters.R') )
source( paste0( SOURCEDIR, 'jstats.R') )

getwhich <- function() {
  return('E')
}
getappname <- function() {
  return('RR_E_Activesubstance')
}