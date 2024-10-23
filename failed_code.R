#Weird code

#Loading property data to add additonal census varibales
pl_ct <-  readRDS("pl_ct.rds")
pl_ct <- pl_ct[c(1:3,5:6)]
names(pl_ct)[1:3] <- c("BOROUGH","BLOCK","LOT")
#Census Tract seems to loaded oddly
#Dropping Values that are missing census blocks
pl_ct <-  unique(pl_ct)
#apparently there were a lot of duplicates

pl_ct <-  pl_ct[!duplicated(pl_ct[c("BOROUGH","BLOCK","LOT")]),]
pl_ct <-  filter(pl_ct, CB2010 > 0)
pl_ct$CB2010 <-  as.character(pl_ct$CB2010)
#Fixing pluto census blocks.
for(i in 1:nrow(pl_ct)){ 
  if (nchar(pl_ct$CB2010[i]) > 1) {
    pl_ct$CB2010[i] <- substring(pl_ct$CB2010[i], 1, 1)
  } else {
    pl_ct$CB2010[i] <- pl_ct$CB2010[i]
  }
}

pl_ct$CB2010 <- as.numeric(pl_ct$CB2010)

#Saving cleaned pluto data
saveRDS(pl_ct, "pl_ct2.rds")

#Loading data for merges
#pl_ct= pluto data with census tracts, za2=property and school data,ce=census
za <-  readRDS("za2.rds")
ce <-  readRDS("census.rds")
za2 <-  inner_join(za,pl_ct, by=c("BOROUGH","BLOCK","LOT"))

#Joing with census data
names(ce)[36] <-  "BOROUGH"
za2$CT2010 <- as.numeric(as.character(za2$CT2010))
za2$CB2010 <- as.numeric(za2$CB2010)
za2$BOROUGH <- as.character(za2$BOROUGH)

za_c <- inner_join(za2, ce, by=c("CT2010","CB2010", "BOROUGH"))
#Saving data with census
saveRDS(za_c, "za_c.rds")


#Adding additional property varibales

#Problem of differntitating v1 and v2 if differences for now just going to use v2 
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Pluto")


.read.delim <- pryr::partial(read.delim, sep=",", dec=",")
.read.csv <- pryr::partial(read.csv, sep=",", dec=",")

#could simplify with for loop
pl9 <-  lapply(list.files(pattern="*09v1.txt"), .read.delim)
pl9a <-  lapply(list.files(pattern="*09v2.txt"), .read.delim)
pl10 <-  lapply(list.files(pattern="*10v1.txt"), .read.delim)
pl10a <-  lapply(list.files(pattern="*10v2.txt"), .read.delim)
pl11 <-  lapply(list.files(pattern="*11v1.txt"), .read.delim)
pl11a <-  lapply(list.files(pattern="*11v2.txt"), .read.delim)
pl12 <-  lapply(list.files(pattern="*12v1.txt"), .read.delim)
pl12a <-  lapply(list.files(pattern="*12v2.txt"), .read.delim)

pl1 <- c(pl9,pl9a,pl10,pl10a,pl11)
pl1a <- c(pl11a,pl12,pl12a)
pl <-  lapply(list.files(pattern="*.csv"), .read.csv)

pl2 <- c(pl1a,pl)

#Starting in 11v2 ct2010 used
cols <- c("Borough","Block","Lot","Address", "CT2000","CB2000")
cols2 <- c("Borough","Block","Lot","Address", "CT2010","CB2010")

pl3 <-  list(1)
for(i in 1:25) {
  pl3[[i]] <- pl1[[i]][,cols]
}
pl4 <-  list(1)
for(i in 1:40) {
  pl4[[i]] <- pl2[[i]][,cols2]
}

pl3 <- ldply(pl3)
pl3 <- unique(pl3)
pl4 <-  unique(ldply(pl4))
pl3$CT2010 <- NA
pl3$CB2010 <- NA
pl4$CT2000 <- NA
pl4$CB2000 <- NA

pl3$Address <-  trimws(as.character(pl3$Address))
pl4$Address <-  trimws(as.character(pl4$Address))
pl4 <- unique(pl4)

ct <-  readRDS("pl_ct.rds")
prop <- merge(pl3, pl4,c("Borough","Block","Lot","Address"), all = TRUE)

setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
saveRDS(pl4, "pl_ct.rds")


prop2 <-  unique(prop)
prop$Address <-  trimws(as.character(prop$Address))
prop3 <-  unique(prop)
#Intial save including duplicates
saveRDS(prop2, "ct.rds")

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
p_f <-  completeFun(prop2, c("XCoord", "YCoord","Address"))
p_f <- arrange(p_f, Borough, Block, Lot,Address)
p_f$Address <-  trimws(as.character(p_f$Address))
#Should addresses be included here, test 
p_f3 <-  p_f[!duplicated(p_f[c("Borough", "Block","Lot","Address")]),]

#Now joining p_f3 with complete pluto data to pick up extra necessary varibales

#Some of the cols may vary by year though like property owner definelty would. So can probably exclude but add with join on specific year based on sale of the propety. 
cols <- c("Borough","Block","Lot","OwnerType","LandUse","OwnerName","LotArea","BldgArea","ResArea","NumFloors","AssessLand","AssessTot","YearAlter1","YearAlter2","HistDist","Landmark")

pl3 <-  list(1)
for(i in 1:40) {
  pl3[[i]] <- pl1[[i]][,cols]
}
pl4 <-  list(1)
for(i in 1:20) {
  pl4[[i]] <- pl2[[i]][,cols2]
}

pl3 <- ldply(pl3)
pl3 <- unique(pl3)
pl4 <-  unique(ldply(pl4))
pl3$CT2010 <- NA
pl3$CT2010 <- NA
pl4$CT2000 <- NA
pl4$CT2000 <- NA
prop <-  rbind(pl3,pl4)
prop2 <-  unique(prop)

