library(downloader)
library(utils)
library(pryr)
library(plyr)
library(dplyr)

setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Pluto")
#downloading v1
for(i in 5:16){ 
  if (i == 5) {
    download(paste("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_0",as.character(i),"d.zip",sep=""),paste("pl_",as.character(i),".zip",sep=""),mode = "wb")
  }
  else if (i < 8) {
    download(paste("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_0",as.character(i),"c.zip",sep=""),paste("pl_",as.character(i),".zip",sep=""),mode = "wb")
  } else if ( i == 9) { #Note there is no 8 zip file
    download(paste("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_0",as.character(i),"v1.zip",sep=""),paste("pl_",as.character(i),".zip",sep=""),mode = "wb")
  } else if (i > 9) {
    download(paste("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_",as.character(i),"v1.zip",sep=""),paste("pl_",as.character(i),".zip",sep=""),mode = "wb")    
  }
}
#downloading v2
for(i in 9:16){ 
  if ( i == 9) { #Note there is no 8 zip file
    download(paste("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_0",as.character(i),"v2.zip",sep=""),paste("pl_",as.character(i),"v2.zip",sep=""),mode = "wb")
  } else if ( i!= 15) { #There is no v2 15 file
    download(paste("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_",as.character(i),"v2.zip",sep=""),paste("pl_",as.character(i),"v2.zip",sep=""),mode = "wb")    
  }
}

#Unzipping All Pluto Files. After 13 the unzipped files stop having names with numbers  
for(i in 5:13) {
  unzip(paste("pl_",as.character(i),".zip",sep=""))
}
#Version 2 files
for(i in 9:13) {
  unzip(paste("pl_",as.character(i),"v2.zip",sep=""))
}
#Dealing with zips 14:16
for(i in 14:16) {
  unzip(paste("pl_",as.character(i),".zip",sep=""))
  file.rename("BK.csv", paste("BK", as.character(i),"v1.csv",sep=""))
  file.rename("BX.csv", paste("BX", as.character(i),"v1.csv",sep=""))
  file.rename("MN.csv", paste("MN", as.character(i),"v1.csv",sep=""))
  file.rename("SI.csv", paste("SI", as.character(i),"v1.csv",sep=""))
  file.rename("QN.csv", paste("QN", as.character(i),"v1.csv",sep=""))
}
#v2 files
for(i in 14:16) {
  if (i !=15 ) {
    unzip(paste("pl_",as.character(i),"v2.zip",sep=""))
    file.rename("BK.csv", paste("BK", as.character(i),"v2.csv",sep=""))
    file.rename("BX.csv", paste("BX", as.character(i),"v2.csv",sep=""))
      file.rename("MN.csv", paste("MN", as.character(i),"v2.csv",sep=""))
    file.rename("SI.csv", paste("SI", as.character(i),"v2.csv",sep=""))
    file.rename("QN.csv", paste("QN", as.character(i),"v2.csv",sep=""))
  }
}  
list.files()
#Read text. Until 13 the files are all text formated
#Changing default setting of read.delim for ease

#Loading just 9+, earlier pluto data avaliable but not neceesary
.read.delim <- pryr::partial(read.delim, sep=",", dec=",")
.read.csv <- pryr::partial(read.csv, sep=",", dec=",")

#Loading csv and text files 
#Should have 40
pl <-  lapply(list.files(pattern="*v1.txt"), .read.delim)
pl_ <-  lapply(list.files(pattern="*v2.txt"), .read.delim)
pl  <-  c(pl,pl_)
#Should have 25
pl2 <-  lapply(list.files(pattern="*.csv"), .read.csv)

#selecting only necessary varibales
cols <- c("Borough","Block","Lot","XCoord","YCoord","Address")
pl3 <-  list(1)
for(i in 1:40) {
  pl3[[i]] <- pl[[i]][,cols]
}

pl4 <-  list(1)
for(i in 1:20) {
  pl4[[i]] <- pl2[[i]][,cols]
}
#Computer can't handle joining pl3 and 4 at once with unique 
pl3 <- ldply(pl3)
pl3 <- unique(pl3)
pl4 <-  unique(ldply(pl4))
prop <-  rbind(pl3,pl4)
prop2 <-  unique(prop)
#Intial save including duplicates
saveRDS(prop2, "pluto_all.rds")
#Dropping any missing addresses or xy coordinates with function
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
p_f <-  completeFun(prop2, c("XCoord", "YCoord","Address"))
p_f <- arrange(p_f, Borough, Block, Lot,Address)
p_f$Address <-  trimws(as.character(p_f$Address))

#XY Coordinates differ slightly by year of Pluto data so duplicates by bourough, block and lot removed for now can probably add address too which  may be removed in future
p_f2 <-  p_f[!duplicated(p_f[c("Borough", "Block","Lot")]),]
p_f3 <-  p_f[!duplicated(p_f[c("Borough", "Block","Lot","Address")]),]
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
saveRDS(p_f2, "p_f5.rds")
