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
.read.delim <- pryr::partial(read.delim, sep=",", dec=",")
.read.csv <- pryr::partial(read.csv, sep=",", dec=",")

#Function to read data from all bouroughs into one clean dataset
#Reading text files...this one takes a while
pl = lapply(list.files(pattern="*.txt"), .read.delim)
pl_ = lapply(list.files(pattern="*.csv"), .read.csv)
pl_all <- pl

cols <- c("Borough","Block","Lot","XCoord","YCoord","Address")
cols2 <- c("Borough","Block","Lot","XCoord","YCoord","CT","CB","OwnerType","LandUse","OwnerName","LotArea","BldgArea","ResArea","NumFloors","AssessLand","AssessTot","YearAlter1","YearAlter2","HistDist","Landmark")

pl2 <-  list(1)
for(i in 1:45) {
  pl2[[i]] <- pl_all[[i]][,cols]
}

pl3 <-  list(1)
for(i in 1:45) {
  pl3[[i]] <- pl[[i]][,cols]
}
prop <-  unique(ldply(pl3))

#na.omit to remove a few cases of missing coordinates
#XY Coordinates differ slightly by year of Pluto data. This could be fixed better, but for now just removing any duplicates
p_f <- arrange(prop, Borough, Block, Lot)
p_f2 <-  p_f[!duplicated(p_f[c("Borough", "Block","Lot")]),]

#Writes the Full clean pluto data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
saveRDS(p_f, "p_f.rds")
saveRDS(p_f2, "p_f2.rds")
write.csv(p_f,"p_f.csv",row.names=FALSE)

#Newest version with address
saveRDS(p_f2, "p_f3.rds")


#Intial join with Property Data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
prop_f <- readRDS("all_prop.rds")
p_f2 <- readRDS("p_f3.rds")

pl <- p_f2
rm(p_f)
rm(p_f2)
#Changing Names and cleaning data
names(pl)[1:6] <- c("BOROUGH","BLOCK","LOT","XCoord","YCoord","ADDRESS")
names(prop_f)[3] <- c("bu_class")
names(prop_f)[21] <- c("s_date")
prop_f$bu_class <- trimws(prop_f$bu_class,"r")
pl$ADDRESS <-  trimws(as.character(pl$ADDRESS))

#Renaming Bouroughs in datasets to align
prop_f$BOROUGH[prop_f$BOROUGH==1] <- "MN"
prop_f$BOROUGH[prop_f$BOROUGH==2] <- "BX"
prop_f$BOROUGH[prop_f$BOROUGH==3] <- "BK"
prop_f$BOROUGH[prop_f$BOROUGH==4] <- "QN"
prop_f$BOROUGH[prop_f$BOROUGH==5] <- "SI"
#Renaming Building Class to Align...may need to do more
prop_f$bu_class[prop_f$bu_class=="01 ONE FAMILY DWELLINGS"] <- "01 ONE FAMILY HOMES"
prop_f$bu_class[prop_f$bu_class=="02 TWO FAMILY DWELLINGS"] <- "02 TWO FAMILY HOMES"
prop_f$bu_class[prop_f$bu_class=="03 THREE FAMILY DWELLINGS"] <- "03 THREE FAMILY HOMES"

#Intial join that excludes condos and a few other properties
prop_pl <- inner_join(prop_f, pl, by=c("BLOCK","LOT","BOROUGH"))
#still seem to be duplicates...which seems sorta odd
prop_pl2 <-  unique(prop_pl)
#The intial join still misses a good bit of property
nrow(prop_pl2)/nrow(prop_f)

#Another join including address and block. Picks up less but address isn't exactly matched in a number of instances
prop_pl3 <- inner_join(prop_f, pl, by=c("BLOCK","BOROUGH","ADDRESS"))
prop_pl4 <-  unique(prop_pl3)
#Dropping and renaming varibales from the two datasets above to enable bind
prop_pl2 <- prop_pl2[,1:24]
drop <- "LOT.y"
prop_pl4 <-  prop_pl4[ , !(names(prop_pl4) %in% drop)]
names(prop_pl2)[9] <- "ADDRESS"
names(prop_pl4)[6] <- "LOT"
#Combining the two different joins above and removing duplicates
prop_pl5 <-  unique(rbind(prop_pl4,prop_pl2))
#This is still imperfect but now at 89% of properties identified. Around half of condos picked up
#See if possible to get proportion comparsion by building class too
nrow(prop_pl5)/nrow(prop_f)
table(prop_pl5$bu_class)
table(prop_pl2$bu_class)


#Mergining with Pluto again to get back necessary varibales that weren't picked up intially
#Will need to do address and block join again, maybe just do this above



#Imperfect but saving as a sample for now
#Date for 2011-2012 School ZOne
p11_12 <-  filter(prop_pl2, s_date > "2011-10-01", s_date < "2012-10-01")

saveRDS(p11_12, "p11_12.rds")
write.csv(p11_12, "p11_12.csv",row.names = FALSE)

#Filtering for just residental units 
#Vector of building class categories
#Still seems to be issue with spacing...
bu <- c("01 ONE FAMILY HOMES","02 TWO FAMILY HOMES","03 THREE FAMILY HOMES")
prop_f2 <- filter(prop_f, bu_class %in% bu)

#Filtering one year as a test, but will need to filter Sale Date to coincide with the release of the grades
prop8 <- filter(prop_f, Year == 2008)
prop8_r <- filter(prop_f2, Year == 2008)

head(subset(prop_f, s_date < "2003-05-03" ))


#A decent bit is lost but around 70% is picked up
t <- inner_join(prop8, pl, by=c("BLOCK","LOT","BOROUGH"))
nrow(t)/nrow(prop8)
#But looking at just residental the pick up rate is a lot better at 99%
t2 <- inner_join(prop8_r, pl, by=c("BLOCK","LOT","BOROUGH"))
nrow(t2)/nrow(prop8_r)

#Not Filtering by year looking at everything
t3 <- inner_join(prop_f, pl, by=c("BLOCK","LOT","BOROUGH"))
nrow(t3)/nrow(prop_f)

t4 <- inner_join(prop_f2, pl, by=c("BLOCK","LOT","BOROUGH"))
nrow(t4)/nrow(prop_f2)


#Looking at which Property Types are missing from join
ta <- left_join(prop_f, pl, by=c("BLOCK","LOT","BOROUGH"))
mi <- filter(ta, !complete.cases(YearBuilt))
