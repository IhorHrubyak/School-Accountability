library(dplyr)
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Zone_prop3")
#Differences in school zone data sets
#09-10.10-11 No DBN SCH_ZONE has number without numbers or BOROUGHugh identifier
#11-12 has DBN, 12-13 only as shape file, the rest have csvs with DBNs

z10 <- read.csv("z_p9-10.csv",sep=",")
z11 <- read.csv("z_p10-11.csv",sep=",")
z12 <- read.csv("z_p11-12.csv",sep=",")
z13 <- read.csv("z_p12-13.csv",sep=",")
z14 <- read.csv("z_p13-14.csv",sep=",")
#Note the below matched to 14-15 school zone data
z15 <- read.csv("z_p13-.csv",sep=",")

#Need to make sure the databases have the same names, mostly similar but need to account for exceptions
#10 and 11 are the same
names(z10)[25] <- "SCH_ZONE"
names(z11)[25] <- "SCH_ZONE"
names(z12)[25] <- "DBN"
names(z14)[25] <- "SCH_ZONE"
names(z14)[25] <- "DBN"
names(z15)[25] <- "DBN"


#For loop to create DBN to correspond with accountability data
#M = Manhattan X= Bronx Q=Queens K=Brooklyn R=Staten Island. 
#Some articafal DBNs in the case where there were no or multiple school zones were created they were dropped to ease the join

#Converting any rows with multiple school zones into blanks to ease conversion to numeric and drop them
z10$SCH_ZONE <- gsub(".*,.*","",z10$SCH_ZONE)
z10$SCH_ZONE <- as.numeric(z10$SCH_ZONE)
z10 <- z10[complete.cases(z10[ , 25]),]
#Fixing Schoot dist for join
a <- filter(z10,schooldist >= 10 )
b <- filter(z10,schooldist < 10 )
b$schooldist <- gsub("^","0",b$schooldist)
z10 <- rbind(a,b)

#Creating DBN fo School Zone Data
for(i in 1:nrow(z10)){ 
  if(z10$SCH_ZONE[i] < 10) {
    if (z10$BOROUGH[i] == "BK") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"K00",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "MN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"M00",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "QN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"Q00",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "BX") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"X00",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "SI") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"R00",sep=""),z10$SCH_ZONE[i]) 
    } 
  } else if(z10$SCH_ZONE[i] < 100 ) {
    if (z10$BOROUGH[i] == "BK") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"K0",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "MN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"M0",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "QN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"Q0",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "BX") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"X0",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "SI") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"R0",sep=""),z10$SCH_ZONE[i]) 
    } 
  } else {
    if (z10$BOROUGH[i] == "BK") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"K",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "MN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"M",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "QN") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"Q",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "BX") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"X",sep=""),z10$SCH_ZONE[i]) 
    } else if (z10$BOROUGH[i] == "SI") {
      z10$DBN[i] <- sub("^",paste(as.character(z10$schooldist[i]),"R",sep=""),z10$SCH_ZONE[i]) 
    }
  }
}

#Replicating above for z11
a <- filter(z11,schooldist >= 10 )
b <- filter(z11,schooldist < 10 )
b$schooldist <- gsub("^","0",b$schooldist)
z11 <- rbind(a,b)

z11$SCH_ZONE <- gsub(".*,.*","",z11$SCH_ZONE)
z11$SCH_ZONE <- as.numeric(z11$SCH_ZONE)
z11 <- z11[complete.cases(z11[ , 25]),]
for(i in 1:nrow(z11)){ 
  if(z11$SCH_ZONE[i] < 10) {
    if (z11$BOROUGH[i] == "BK") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"K00",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "MN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"M00",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "QN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"Q00",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "BX") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"X00",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "SI") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"R00",sep=""),z11$SCH_ZONE[i]) 
    } 
  } else if(z11$SCH_ZONE[i] < 100 ) {
    if (z11$BOROUGH[i] == "BK") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"K0",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "MN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"M0",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "QN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"Q0",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "BX") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"X0",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "SI") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"R0",sep=""),z11$SCH_ZONE[i]) 
    } 
  } else {
    if (z11$BOROUGH[i] == "BK") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"K",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "MN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"M",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "QN") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"Q",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "BX") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"X",sep=""),z11$SCH_ZONE[i]) 
    } else if (z11$BOROUGH[i] == "SI") {
      z11$DBN[i] <- sub("^",paste(as.character(z11$schooldist[i]),"R",sep=""),z11$SCH_ZONE[i]) 
    }
  }
}
  

#Dropping column that's no longer necessary
z10 <- select(z10, -25,-26)
z11 <- select(z11, -25,-26)
#Loading Accountability data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
ac <- readRDS("p_all.rds")

#Keeping only all students and relevant school year. Can also create separte datasets for subsets of students if needed
#09-10 School zone data
ac$Year <-  as.character(ac$Year)
ac9 <- filter(ac, Year == "2009", Category == "All Students", Grade == 9, Score_9 > 0)
ac10 <- filter(ac, Year == "2010", Category == "All Students", Grade == 9, Score_10 > 0)
ac11 <- filter(ac, Year == "2011", Category == "All Students", Grade == 9, Score_11 > 0)
ac12 <- filter(ac, Year == "2012", Category == "All Students", Grade == 9, Score_12 > 0)
ac13 <- filter(ac, Year == "2013", Category == "All Students", Grade == 9, Score_13 > 0)
ac14 <- filter(ac, Year == "2014", Category == "All Students", Grade == 9, Score_13 > 0)


#Joining accountability with property data
za10 <- inner_join(ac9, z10, by="DBN")
za11 <- inner_join(ac10, z11, by="DBN")
za12 <- inner_join(ac11, z12, by="DBN")
za13 <- inner_join(ac12, z13, by="DBN")
za14 <- inner_join(ac13, z14, by="DBN")
za15 <- inner_join(ac14, z15, by="DBN")

#Creating unique identifier for each of the data set to distinguish them
za10$Y <- 10 
za11$Y <- 11
za12$Y <- 12
za13$Y <- 13
za14$Y <- 14
za15$Y <- 15
names(za15) <- names(za14)

#Joining the above into one data set from list
za_l <- list(za10,za11,za12,za13,za14,za15)

for(i in 1:7){
  if (i ==1){
  za <- rbind(za_l[[i]],za_l[[i+1]])
   
  } else if (i > 3) {
    za <- rbind(za,za_l[[i-1]])    
  }
}

#Saving the dataset
saveRDS(za, "za2.rds")
