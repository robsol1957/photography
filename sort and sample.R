library(jpeg)

dir <- "D:\\PSAutoRecover\\Rob\\bu\\iCloud Photos\\iCloud Photos"
land <- paste0(dir,"\\land\\")
port <- paste0(dir,"\\port\\")

clear_dir <- function(dir){
  for(file in dir(dir)){
    file.remove(paste0(dir,file))
  }  
}

sample_Dir <- function(fromdir, todir, n = 50) {
  clear_dir(paste0(dir,"\\samp\\"))
  filelist <- dir(fromdir)
  sample <- sample(filelist, n, FALSE)
  count = 0
  for (file in sample) {
    from <- paste0(fromdir, file)
    to <- paste0(todir, as.character(count), ".jpg")
    file.copy(from, to)
    count = count + 1
  }
}

sort_img <- function() {
  filelist <- dir(dir)
  filelist <- grep(".JP", toupper(filelist), value = T)
  d <- as.character(as.integer(runif(1, 1000000, 99999999)))
  file <- filelist[1]
  for (file in filelist) {
    img <- readJPEG(paste0(dir, "\\", file))
    n <- paste0(substr(file, 1, nchar(file) - 4), "_", d, ".JPG")
    if (dim(img)[1] > dim(img)[2]) {
      t <- file.rename(paste0(dir, "\\", file), paste0(port, "\\", n))
    } else {
      t <- file.rename(paste0(dir, "\\", file), paste0(land, "\\", n))
    }
  }
}


sample_Dir(fromdir=port,todir=paste0(dir,"\\samp\\"),n=100)

