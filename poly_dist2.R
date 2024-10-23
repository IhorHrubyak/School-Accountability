library(rgdal)
library(sp)
library(dplyr)
library(geosphere)

#All are probably structured slightly differenetly
#Maybe can try to load in qgis and export...or something
#2012-2013 has no csv...maybe can export with qgis
#Two earliest years don't have DBN, but they were created in School_zone code can load those versions

#ISSUE WIH 13 LATITUDE AND LONGITUDE ARE REVERED
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Zone2")
z10 <- read.csv("09-10.csv")
z11 <- read.csv("10-11.csv")
z12 <- read.csv("11-12.csv")
z13 <- read.csv("12-13.csv")
z14 <- read.csv("13-14.csv")
z15 <- read.csv("14-15.csv")


#Adding DBNs to 10 and 11 
#Creating DBN fo School Zone Data
z10$SCH_ZONE <- gsub(".*,.*","",z10$SCH_ZONE)
z10$SCH_ZONE <- as.numeric(z10$SCH_ZONE)
z10 <- z10[complete.cases(z10[ , 4]),]
#Fixing Schoot dist for join
a <- filter(z10,SCHOOLDIST >= 10 )
b <- filter(z10,SCHOOLDIST < 10 )
b$SCHOOLDIST <- gsub("^","0",b$SCHOOLDIST)
z10 <- rbind(a,b)

#Creating DBN fo School Zone Data
for(i in 1:nrow(z10)){ 
  if(z10$SCH_ZONE[i] < 10) {
    if (z10$BORO[i] == "BK") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"K00",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "MN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"M00",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "QN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"Q00",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "BX") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"X00",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "SI") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"R00",sep=""),z10$SCH_ZONE[i]) 
    } 
  } else if(z10$SCH_ZONE[i] < 100 ) {
    if (z10$BORO[i] == "BK") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"K0",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "MN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"M0",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "QN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"Q0",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "BX") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"X0",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "SI") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"R0",sep=""),z10$SCH_ZONE[i]) 
    } 
  } else {
    if (z10$BORO[i] == "BK") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"K",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "MN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"M",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "QN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"Q",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "BX") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"X",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BORO[i] == "SI") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$SCHOOLDIST[i]),"R",sep=""),z10$SCH_ZONE[i]) 
    }
  }
}

#Replicating above for z11
a <- filter(z11,SCHOOLDIST >= 10 )
b <- filter(z11,SCHOOLDIST < 10 )
b$SCHOOLDIST <- gsub("^","0",b$SCHOOLDIST)
z11 <- rbind(a,b)

z11$SCH_ZONE <- gsub(".*,.*","",z11$SCH_ZONE)
z11$SCH_ZONE <- as.numeric(z11$SCH_ZONE)
z11 <- z11[complete.cases(z11[ , 4]),]
for(i in 1:nrow(z11)){ 
  if(z11$SCH_ZONE[i] < 10) {
    if (z11$BORO[i] == "BK") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"K00",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "MN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"M00",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "QN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"Q00",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "BX") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"X00",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "SI") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"R00",sep=""),z11$SCH_ZONE[i]) 
    } 
  } else if(z11$SCH_ZONE[i] < 100 ) {
    if (z11$BORO[i] == "BK") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"K0",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "MN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"M0",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "QN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"Q0",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "BX") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"X0",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "SI") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"R0",sep=""),z11$SCH_ZONE[i]) 
    } 
  } else {
    if (z11$BORO[i] == "BK") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"K",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "MN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"M",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "QN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"Q",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "BX") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"X",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BORO[i] == "SI") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$SCHOOLDIST[i]),"R",sep=""),z11$SCH_ZONE[i]) 
    }
  }
}


l <<- NA
fun <- function(z) {
  z <<- z
  z$the_geom2 <- gsub("[A-z]","",z$the_geom)
  z$the_geom2 <- gsub("[^[:alnum:][:blank:],.-]", "", z$the_geom2)
  
  t <- strsplit(as.vector(z$the_geom2), split = ",")
  t2 <<- lapply(t,as.data.frame)
  
  l <- list(1)
  for (i in 1:nrow(z)) {
    l[[i]] <- as.data.frame(cbind(t[[i]],do.call(rbind, strsplit(as.vector(t2[[i]][,1]), split = " "))))
  } 
  for (i in 1:nrow(z)) {
    l[[i]]$DBN = z$DBN[i] 
  }
  
  #Making the coordinates numeric
  for (i in 1:length(l)) {
    l[[i]][,3] <- as.numeric(as.character(l[[i]][,3]))
  }
  for (i in 1:length(l)) {
    l[[i]][,4] <- as.numeric(as.character(l[[i]][,4]))
  }
  l <<- l
}
#Runing for each of the datasets and renaming lists

fun(z10)
l10 <- l
fun(z11)
l11 <- l
fun(z12)
l12 <- l
fun(z14)
l14 <- l
fun(z15)
l15 <- l

#13 slightly weird needs different
z <- z13
z$the_geom2 <- gsub("[A-z]","",z$the_geom)
z$the_geom2 <- gsub("[^[:alnum:][:blank:],.-]", "", z$the_geom2)
z$the_geom2 <-  trimws(z$the_geom2)
t <- strsplit(as.vector(z$the_geom2), split = ",")
t2 <<- lapply(t,as.data.frame)

l <- list(1)
for (i in 1:nrow(z)) {
  l[[i]] <- as.data.frame(cbind(t[[i]],do.call(rbind, strsplit(as.vector(t2[[i]][,1]), split = " "))))
} 
for (i in 1:nrow(z)) {
  l[[i]]$DBN = z$DBN[i] 
}

#Making the coordinates numeric
for (i in 1:length(l)) {
  l[[i]][,2] <- as.numeric(as.character(l[[i]][,2]))
}
for (i in 1:length(l)) {
  l[[i]][,3] <- as.numeric(as.character(l[[i]][,3]))
}
l13 <- l
#Function to calculate distance

fd <- function(l,d) for (i in 1:nrow(d)) {
  for (j in 1:length(l))
    if (d$DBN[i] == l[[j]]$DBN){
      d$dist[i] <- dist2Line(d[i,153:154], l[[j]][,3:4])[1,1]
    }
  #Converts distance to miles
  d$dist_m <- d$dist/1609.34
  d <<- d 
}



#Calculates distance for each school zone
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
za <- readRDS("za3.rds")
za10 <- filter(za, Y == 10)
za11 <- filter(za, Y == 11)
za12 <- filter(za, Y == 12)
za13 <- filter(za, Y == 13)
za14 <- filter(za, Y == 14)




fd(l10,za10)
d10 <- d
saveRDS(d10,"d10.rds")
fd(l11,za11)
d11 <- d
saveRDS(d11,"d11.rds")
fd(l12,za12)
d12 <- d
saveRDS(d12,"d12.rds")
fd(l13,za13)
d13 <- d
saveRDS(d13,"d13.rds")
fd(l14,za14)
d14 <- d
saveRDS(d14,"d14.rds")

#Again 13 slightly different
fd2 <- function(l,d) for (i in 1:nrow(d)) {
  for (j in 1:length(l))
    if (d$DBN[i] == l[[j]]$DBN){
      d$dist[i] <- dist2Line(d[i,153:154], l[[j]][,2:3])[1,1]
    }
  #Converts distance to miles
  d$dist_m <- d$dist/1609.34
  d <<- d 
}
fd2(l13,za13)
d13 <- d
saveRDS(d13,"d13.rds")

setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Zone2")
d <-  rbind(d10,d11,d12,d13,d14)
d13 <-  readRDS("d13.rds")
d <- rbind(readRDS("d10.rds"),readRDS("d11.rds"),readRDS("d12.rds"),readRDS("d13.rds"),readRDS("d14.rds"))
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
saveRDS(d,"zab.rds")
#The large outliers seem to all be in manhattan beach
t <-  filter(d10, dist_m > 5)

#Can use the below to show distance to boundaries on a ...or qgis looks a lot nicer
t <- filter(bk3, dist >= 454.1 & dist <= 454.2)
#only 13 and 15 seems unaffacted
table(bk3$SchoolDist)
table(t$SchoolDist)
#Test to look at boundaries
a <- filter(bk3, XCoord >= -73.95 & XCoord <= -73.94 & YCoord >= 40.69 & YCoord <= 40.70)[,83:85]
plot(makeLine(l[[76]][,3:4]), type='l')

te <- makeLine(l[[76]][,3:4])

points(te)
points(a, col='blue', pch=50)
points(a[,1], a[,2], col='red', pch='x')
for (i in 1:nrow(d)) lines(gcIntermediate(pnts[i,], d[i,2:3], 10), lwd=2)

te <- as.data.frame((d))
str(te)
