SOURCEDIR <- '../sharedscripts/'
if (!dir.exists(SOURCEDIR))
{
  SOURCEDIR <- 'sharedscripts/'
}
browser()
source( paste0( SOURCEDIR, 'helpfunctions.r') )
source( paste0( SOURCEDIR, 'serverhelpers.R') )
source( paste0( SOURCEDIR, 'uihelpers.R') )
source(  'helperfunctions.R' )