###########################################################################
#######          This file (test.R) contains test function          #######
###########################################################################

test <- function(classifier, dat){
  
  p <- predict(classifier, dat)
  
  return(p)
}
