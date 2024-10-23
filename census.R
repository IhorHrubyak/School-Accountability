library(dplyr)
library(plyr)

#Loads 2010 census data files that were extracted from zip into directory
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Census")
fun <- 
  function(x) {
    read.csv(x,skip=1)
  }

myfiles <- lapply(list.files(pattern="*with_ann.csv"), fun)

#Make sure same census data types chosen otherwise rows will differ and cbind won't work
ce <- NA
for(i in 1:12){ 
  if (i == 1) {
    ce <- cbind(myfiles[[i]], myfiles[[i+1]])
    
  } else if (i > 3) {
    ce <- cbind(ce, myfiles[[i-1]])
  }
}

#Selecting only necessary varibales from above
#could maybe create function to skip first columns except for first one if needed to automated for future
ce <- ce[c(1:6,10:17,21:23,27:30,34:41,45,51,57:58,63,69:76,80,86:92,94,99)]

ce <- mutate(ce,
  per_1household = ce[,8]/ce[,7],
  per_2household = ce[,9]/ce[,7],
  per_2household = ce[,9]/ce[,7],
  per_3household = ce[,10]/ce[,7],
  per_4household = ce[,11]/ce[,7],
  per_5household = ce[,12]/ce[,7],
  per_6household = ce[,13]/ce[,7],
  per_7household = ce[,14]/ce[,7],
  per_occupied = ce[,16]/ce[,15],
  per_vacant = ce[,17]/ce[,15],
  per_owened_mortloan = ce[,19]/ce[,18],
  per_ow_freeclear = ce[,20]/ce[,18],
  per_ow_renter = ce[,21]/ce[,18],
  per_p_forrent = ce[,23]/ce[,22],
  per_p_fr_notocc = ce[,24]/ce[,22],
  per_p_forsale = ce[,25]/ce[,22],
  per_p_fs_no = ce[,26]/ce[,22],
  per_famhous = ce[,33]/ce[,32],
  per_nonfamhous = ce[,34]/ce[,32],
  per_white_c = ce[,36]/ce[,35],
  per_black_c = ce[,37]/ce[,35],
  per_native_c = ce[,38]/ce[,35],
  per_asian_c = ce[,39]/ce[,35],
  per_other_c = ce[,40]/ce[,35],
  per_mixed_c = ce[,41]/ce[,35]
)

#Mostly picking up just created varibales above
ce <- ce[c(1:6,30,43,52:76)]

#Creating census tract, census block and bourough identifiers
ce[,3] <-  as.character(ce[,3])
ce$CB2010 <- as.numeric(trimws(gsub("[A-z]","",sapply(strsplit(ce[,3], ","), "[", 1))))
ce$CT2010 <- as.numeric(trimws(gsub("[A-z]","",sapply(strsplit(ce[,3], ","), "[", 2))))


ce$Borough <- trimws(sapply(strsplit(ce[,3], ","), "[", 3))

#Renaming Bouroughs to align
ce$Borough[ce$Borough=="New York County"] <- "MN"
ce$Borough[ce$Borough=="Bronx County"] <- "BX"
ce$Borough[ce$Borough=="Kings County"] <- "BK"
ce$Borough[ce$Borough=="Queens County"] <- "QN"
ce$Borough[ce$Borough=="Richmond County"] <- "SI"

#Saving census data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
saveRDS(ce, "ce.rds")
