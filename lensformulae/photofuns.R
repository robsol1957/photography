
library(readxl)
camera_info <- read_xlsx("cameras.xlsx",sheet="cameras")
print_sizes <- read_xlsx("cameras.xlsx",sheet="print_size")
# camera_info <- read_xlsx(paste0(dir,"cameras.xlsx"),sheet="cameras")
# print_sizes <- read_xlsx(paste0(dir,"cameras.xlsx"),sheet="print_size")
# camera_info <- data.frame(
#   camera = c("Nikon D5","Nikon D750","Nikon D7100","Moravian Astro","Takahari 5X4"),
#   pixel_size= c(6.4,5.95,3.9,9,15)
# )


balancedf <- function(pixelsize,n=1,lambda = 500){
  #Reyleigh Criteria for lens
  # R= 1.22*wavelength/D --1
  # where D = Diameter of lense
  # D=L/f --2
  # where L =focal length and f=focal ratio
  # from 1 and 2
  # R =1.22*wavelength*f/L ---3
  #
  # Pixel Res
  # Rp = n*P/L ---4
  # where P = Pixel size
  # L = Focal Length
  # n = number of pixels 
  # set 3 = 4
  #  1.22*wavelength*f/L = n*P/L
  # 1.22*wavelength*f = n*P
  # f= n*P/(1.22*wavelength)
  # wavelength in nm
  # pixel size in micron
  f <- n * pixelsize/(1.22 * lambda / 1000)
  f
}


balanced_camera <- function(camera){
  camera=toupper(camera)
  if(camera=="D5"){
    ret <- balancedf(6.41)
  } else if (camera=="D7100"){
    ret <- balancedf(3.9)
  } else if (camera=="MORAVIAN"){
    ret <- balancedf(9)
  } else {
    ret <- paste0("camera ",camera," not found")
  }
  ret
}

Hyperfocal <- function(focal_length,f,ZofC){
  # L focal length of lens
  # f f-number
  # c zone of confusion
  #  Near = Hu/(U+u)
  #  Far = Hu/(H-u)
  # 
  focal_length * focal_length/(f*ZofC/1000)/1000
}

focal_range <- function(focal_dist,H){
  t <- H*focal_dist
  list(focal_dist=focal_dist,near=t/(H+focal_dist),far=t/max((H-focal_dist),0),H=H)
}
depth_of_field <- function(focal_length,f,ZofC,focal_dist){
  H <- Hyperfocal(focal_length,f,ZofC)
  f <- focal_range(focal_dist,H)
  list(focal_length=focal_length,focusdata=f)
}
H <- Hyperfocal(focal_length=100,f=11,ZofC=6.4)
f <- focal_range(10,H)
f <- depth_of_field(focal_length=100,f=11,ZofC=6.4,focal_dist=H)

DofF <- function(focal_length,fstop,pixel_size,focal_dist,pos,pixels=1){
  ZofC=pixel_size*pixels
 f <-  depth_of_field(focal_length,fstop,ZofC,focal_dist)
 if(pos=="near"){
   ret=f$focusdata$near
 } else if(pos=="far"){
   ret=f$focusdata$far
 } else if(pos=="H"){
   ret=f$focusdata$H
 } else {
   ret=-100
 }
 f
}
DofF(focal_length = 100,fstop=11,pixel_size = 6.4,focal_dist = 142.0455,pos="near",pixels = 1)
