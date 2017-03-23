library("EBImage")
library("OpenImageR")

img_dir <- "../data/raw_images/"
dir_names <- list.files(img_dir)
n_files <- length(list.files(img_dir))

img1 <- readImage(paste0(img_dir,  dir_names[1]))
img2 <- readImage(paste0(img_dir,  dir_names[2]))
h1 <- HOG(img1)
p1 <- phash(img2, MODE = "binary")
p2 <- phash(img2, MODE = "binary")


### store HOG and HASH values of images
H <- matrix(NA, n_files, 54) 
P <- matrix(NA, n_files, 64)
A <- matrix(NA, n_files, 64)
for(i in 1:n_files){
  img <- readImage(paste0(img_dir, dir_names[i]))
  h <- HOG(img)
  H[i,] <- h
  p <- phash(img, MODE = "binary")
  P[i,] <- p
}
export <- TRUE
### output constructed features
if(export){
  save(H, file=paste0("../output/HOG.RData"))
}

