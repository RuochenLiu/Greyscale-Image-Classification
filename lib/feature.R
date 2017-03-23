#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Authors: Yue Gao
### Project 7
### ADS Spring 2017

feature <- function(img_dir, export=T){
  
  ### Construct process features for training/testing images
  
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  ### load libraries
  library("EBImage")
  library("OpenImageR")
  
  n_files <- length(list.files(img_dir))
  dir_names <- list.files(img_dir)
  
  ### store HOG values of images
  H <- matrix(NA, n_files,54) 
  
  for(i in 1:n_files){
    img <- readImage(paste0(img_dir, dir_names[i]))
    h <- HOG(img)
    H[i,] <- h
  }
  export <- TRUE
  ### output constructed features
  if(export){
    save(H, file=paste0("../output/HOG.RData"))
  }
  return(H)
}
