


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
balanced_camera("D5")
