###########################################################################
#######          This file (test.R) contains test function          #######
###########################################################################

Test <- function(classifier, dat){
  
  p <- predict(classifier, dat)
  
  return(p)
}