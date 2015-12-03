SOURCEDIR <- '../sharedscripts/'
if (!dir.exists(SOURCEDIR))
{
  SOURCEDIR <- 'sharedscripts/'
}
source( paste0( SOURCEDIR, 'helpfunctions.r') )
source( paste0( SOURCEDIR, 'serverhelpers.R') )
source( paste0( SOURCEDIR, 'uihelpers.R') )
source( paste0( SOURCEDIR, 'jstats.R') )
source( paste0( SOURCEDIR, 'getters.R') )
source( paste0( SOURCEDIR, 'LRTShare.R') )
source( paste0( '', 'LRTE.R') )