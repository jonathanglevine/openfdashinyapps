SOURCEDIR <- '../sharedscripts/'
if (!dir.exists(SOURCEDIR))
{
  SOURCEDIR <- 'sharedscripts/'
}
DATADIR <- '../activesubstancedata/'
if (!file.exists( paste0( DATADIR, 'cleanmpmap.RData') ))
{
  DATADIR <- 'activesubstancedata/'
}
source( paste0( SOURCEDIR, 'helpfunctions.r') )
source( paste0( SOURCEDIR, 'serverhelpers.R') )
source( paste0( SOURCEDIR, 'outputhelpers.R') )
source( paste0( SOURCEDIR, 'uihelpers.R') )
source( paste0( SOURCEDIR, 'jstats.R') )
source( paste0( SOURCEDIR, 'getters.R') )
source( paste0( SOURCEDIR, 'LRTShare.R') )
source( paste0( '', 'LRT.R') )