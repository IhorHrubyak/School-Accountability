# Input coordinates.
#2263=long island crs
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
za <- readRDS("za3.rds")
library(rgdal)
library(sp)
library(dplyr)
library(geosphere)
#Removing any properites missing coordinates
#longitude=x latidue=y  
data <- za[ , 93:94]

coordinates(data) <- ~ XCoord+YCoord
  proj4string(data) <- CRS("+init=epsg:2263")
data.proj <- spTransform(data, CRS("+init=epsg:4326"))
a <- as.data.frame((data.proj))
#combining with old data and removing old xy coordinates
za2 <- select(za,1:92,95:154)
za3 <- cbind(za2,a)
#Rearranging order of coordinates to align with distance later
#writing for export
saveRDS(za3, "za3.rds")

#Loading Zone data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Zone2")
z <- read.csv("11-12.csv")

#Only using data with complete school schools
z$DBN <- as.character(z$DBN)
z$SCH_ZONE <- gsub(".*,.*","",z$SCH_ZONE)
z$SCH_ZONE <- as.numeric(z$SCH_ZONE)
z <- z[complete.cases(z[ , 6]),]

#FIND ONE OR TWO LINE CODE TO REMOVE ) AND ()
#Gets rid of characters
z$the_geom2 <- gsub("[A-z]","",z$the_geom)
#Get rid of ((
z$the_geom2 <- gsub("\\((","",z$the_geom2)
#Get rid of )))
z$the_geom2 <- gsub("\\)))","",z$the_geom2)
#And also there seem to be random ))
z$the_geom2 <- gsub("\\))","",z$the_geom2)
#And also there seem to be random ))
z$the_geom2 <- gsub("\\)","",z$the_geom2)
z$the_geom2 <- gsub("\\(","",z$the_geom2)

#Columns for coordinates, turns them into dataframes and into a list
t <- strsplit(as.vector(z$the_geom2), split = ",")
t2 <- lapply(t,as.data.frame)


#Creates two columns for xy coordinates in a list
l <- list(1)
for (i in 1:nrow(z)) {
  l[[i]] <- as.data.frame(cbind(t[[i]],do.call(rbind, strsplit(as.vector(t2[[i]][,1]), split = " "))))
}

#Adding school district varibale to identify with property
for (i in 1:nrow(z)) {
  l[[i]]$SchoolDist = z$SCHOOLDIST[i] 
}
for (i in 1:nrow(z)) {
  l[[i]]$DBN = z$DBN[i] 
}

#Making the coordinates numeric...apparently this creates some nas which may be an issue
for (i in 1:length(l)) {
  l[[i]][,3] <- as.numeric(as.character(l[[i]][,3]))
}

for (i in 1:length(l)) {
  l[[i]][,4] <- as.numeric(as.character(l[[i]][,4]))
}


#Loading property data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
za <- readRDS("za3.rds")


#Calcuates the distance of property for the school district it's in
#Werid issue where 454.2 is way over represented

#up to 18 it seems ok
#23 doesn't work...maybe it's the only issue. Maybe it's becuase of one of the lines for it being weird and failing
#For the first instance 23 it works then it fails
#It seems with 23 there are some nas



#Probably should exclude zone dataframe that I know won't be used for effiencey. This would need to be at the start though
#It seems 23 isn't recognized by either za or in the list so 454 is the default value for some reason
#Filtering for only relevant year

#SHOULD I MERGE BY ZONE OR DBN? MAYBE CHECK IF THERE'S A DIFFERNECE
za2$DBN <-  as.character(za2$DBN)
za2 <-   filter(za, Y == 12)
for (i in 1:nrow(za2)) {
  for (j in 1:length(l))
    if (za2$DBN[i] == l[[j]]$DBN){
      za2$dist[i] = dist2Line(za2[i,153:154], l[[j]][,3:4])[1,1]
    }
}
Mode(za$dist)

#76 first instance of district 23
dist2Line(za[2,83:84], l[[78]][,3:4])[1,1]

#So it seems like only some are calculated then it poops out for some reason and doesn't calcuate
#If filtering by just one school zone it works though
#maybe just one of the school dist is odd and throwing it off
c <- NA
for (i in 1:nrow(za)) {
  for (j in 1:length(l))
    if (za$SchoolDist[i] == l[[j]]$SchoolDist){
      c[i] = dist2Line(za[i,83:84], l[[j]][,3:4])[1,1]
    } 
}

dist2Line(za[1,83:84], l[[600]][,3:4])[1,1]

a <- filter(za, dist > 454.1770, dist < 454.1772)
b <- filter(za, dist > 454.1772 | dist < 454.1708)

