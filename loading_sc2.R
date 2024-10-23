#This File is simply for loading the various data sets and libraries 
library(knitr)
library(readxl)
library(tidyr)
library(pander)
library(dplyr)
library(plyr)
library(stargazer)
library(rdd)
library(rddtools)
library(ggplot2)
library(lattice)

#Public school English and Math Test scores
#Data sets imported for different divisions of data i.e All Students, ELL, by Ethnicity, Gender, swd
#For simplicty % at level scores are excluded
#All Students
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Test_Score")
z <- list()
fun <- function(x,q){
  for(i in 1:5) {
    z[[i+q]] <<- as.data.frame(read_excel(paste(as.character(x)),sheet=i, na = "NA", skip=6))[,1:6]
    
  } 
}
#Loading function for new test score data slighly different
fun2 <- function(x,q){
  for(i in 2:7) {
    z[[i+q]] <<- select(as.data.frame(read_excel(paste(as.character(x)),sheet=i, na = "NA", skip=7))[,1:8],-1,-3)
    
  } 
}


fun("eng6-12.xlsx",0)
fun2("eng13-17.xlsx",4)
fun("math6-12.xlsx",11)
fun2("math13-17.xlsx",15)

#Odd issue with z[[19]] so reloading it manually and adding on
tem <- as.data.frame(read_excel("math13-17.xlsx",sheet=4, na = "NA", skip=7)[,1:8])
z[[19]] <- select(tem,1,3:7)

#Renaming columns
for(i in 1:length(z)){ 
  if (i < 12) {
    names(z[[i]])[5:6] <- c("eng_n","eng_sco")
  } else {
    names(z[[i]])[5:6] <- c("math_n","math_sco")
  }
}


#Joining dataframes from list into math and english dataframes
#1:11 eng 12:22 math
for(i in 1:22){ 
  if (i == 1) {
    p <- rbind(z[[i]], z[[i+1]])
  } else if (i < 12 & i > 3) {
    p <- rbind(p, z[[i-1]])
  } else if (i == 12) {
    p2 <- rbind(z[[i]], z[[i+1]])
  } else if (i > 14) {
    p2 <- rbind(p2, z[[i-1]])
  }
}

#Joining Math and English dataframes from above
p3 <- full_join(p, p2, by=c("DBN","Grade","Year","Category"))
p3$math_sco <- as.numeric(p3$math_sco)
p3$eng_sco <- as.numeric(p3$eng_sco)
#Demographic data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Demo")
de <- read.csv("demo6-12.csv",stringsAsFactors = FALSE)
de2 <- as.data.frame(read_excel("demo13-17.xlsx", sheet=5, na = "NA"))

#Data Corrections
de$schoolyear[de$schoolyear==20052006] <- 2006
de$schoolyear[de$schoolyear==20062007] <- 2007
de$schoolyear[de$schoolyear==20072008] <- 2008
de$schoolyear[de$schoolyear==20082009] <- 2009
de$schoolyear[de$schoolyear==20092010] <- 2010
de$schoolyear[de$schoolyear==20102011] <- 2011
de$schoolyear[de$schoolyear==20112012] <- 2012

de2$Year[de2$Year=="2012-13"] <- 2013
de2$Year[de2$Year=="2013-14"] <- 2014
de2$Year[de2$Year=="2014-15"] <- 2015
de2$Year[de2$Year=="2015-16"] <- 2016
de2$Year[de2$Year=="2016-17"] <- 2017
names(de)[3] <- "Year"

de$fl_percent <- as.numeric(de$fl_percent)
de$Year <- as.factor(de$Year)
de$DBN <- as.factor(de$DBN)

#Aligining de and de2 to have equilvant varibale names. Free lunch and poverty varibales similar not joining
cols <- c(1:3,6:20,37:38,35:36,27:32)
names(de2)[1:28] <- names(de[,cols])
cols2 <- c(33:34,23:24,21:22)
names(de2)[31:36] <- names(de[,cols2])
names(de2)[37:39] <- c("pov_num","pov_per","econ_need_in")
names(de2)[29:30] <- c("mr_num","mr_per")

#Converting newer demographic data percentages to align with old
de2$female_per <- de2$female_per * 100
de2$male_per <- de2$male_per * 100
de2$asian_per <- de2$asian_per * 100 
de2$black_per <- de2$black_per * 100 
de2$white_per <- de2$white_per * 100 
de2$hispanic_per <- de2$hispanic_per * 100 
de2$sped_percent <- de2$sped_percent * 100 
de2$mr_per <- de2$mr_per * 100 
de2$ell_percent <- de2$ell_percent * 100 
de2$pov_per <- de2$pov_per * 100 
de2$econ_need_in <- de2$econ_need_in * 100 

#Joined old and new demographic data
de3 <- arrange(rbind.fill(de,de2), DBN,Year)

#Saving without accountability data
de3$Year <- as.numeric(de3$Year)
full <- full_join(de3, p3, by=c("DBN","Year"))
saveRDS(full, "full_na.rds")

#Accountability data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Account_data")

#10 and 12, 13 have percentile rank. 10 and 12 renamed in excel to ease import
#Loop to load and prepare accountability data
acc <- list(1)
for(i in 7:13){ 
  if (i < 11 ) {
    x <- as.data.frame(read_excel(paste("", as.character(i), ".xlsx", sep=""),sheet=1, na = "NA", skip=1))
    valid_column_names <- make.names(names=names(x), unique=TRUE, allow_ = TRUE)
    names(x) <- valid_column_names
    xa <- select(x, DBN,Peer.Index,Grade,Overall.Score,Percentile,School.Type)
    xa$School.Type <- toupper(xa$School.Type) 
    names(xa)[2:5] <- c(paste("Peer_", as.character(i),sep = ""),paste("Grade_", as.character(i),sep = ""),paste("Score_", as.character(i),sep = ""),paste("Percentile_", as.character(i),sep = ""))
    acc[[i]] <- xa
  } else if (i < 13) {
    x <- as.data.frame(read_excel(paste("", as.character(i), ".xlsx", sep=""),sheet=1, na = "NA", skip=1))
    valid_column_names <- make.names(names=names(x), unique=TRUE, allow_ = TRUE)
    names(x) <- valid_column_names
    xa <- select(x, DBN,Peer.Index,Overall.Grade,Overall.Score,Percentile,School.Type)
    xa$School.Type <- toupper(xa$School.Type)
    names(xa)[2:5] <- c(paste("Peer_", as.character(i),sep = ""),paste("Grade_", as.character(i),sep = ""),paste("Score_", as.character(i),sep = ""),paste("Percentile_", as.character(i),sep = ""))
    acc[[i]] <- xa
  } else {
    x <- as.data.frame(read_excel(paste("", as.character(i), ".xlsx", sep=""),sheet=1, na = "NA", skip=1))
    valid_column_names <- make.names(names=names(x), unique=TRUE, allow_ = TRUE)
    names(x) <- valid_column_names
    xa <- select(x, DBN,Overall.Grade,Overall.Score,Percentile.Rank,School.Type)
    xa$School.Type <- toupper(xa$School.Type)
    names(xa)[2:4] <- c(paste("Grade_", as.character(13),sep = ""),paste("Score_", as.character(13),sep = ""),paste("Percentile_", as.character(i),sep = ""))
    acc[[i]] <- xa
  }
}

#Loop to Fix School Type names
acc3 <- acc
for(i in 7:12){ 
  acc[[i]][6] <- gsub("MIDDLE SCHOOL","MIDDLE",acc[[i]]$School.Type )
}


#Loop to join datasets
#seems like there are a few duplicates from this still...but there shouldn't be any for particular years it's just in certain years school types change
#So good to keep two versions of the data when school type is required and when it's not
for(i in 7:14){ 
  if (i == 7) {
    acc2 <- full_join(acc[[i]], acc[[i+1]], by=c("DBN","School.Type"))
    
  } else if (i > 9) {
    acc2 <- full_join(acc2, acc[[i-1]], by=c("DBN","School.Type"))
  }
}


#1250 if not joining by school type

#Joining Accountability data with demographic and test score data
full <- full_join(de3, acc2, by=c("DBN"))
full$Year <- as.numeric(full$Year)
full2 <- full_join(full, p3, by=c("DBN","Year"))
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
saveRDS(full2, "full.rds")

#Fixing missing test scores for all students




#De Blasio era data. INCOMPLETE
#Only extracting relevant scores
bl <- list(1)
for(i in 1:5){ 
  bl[[i]] <- as.data.frame(read_excel("14.xlsx", sheet=i, na = "NA", skip=1))[,1:3]
}

#Joining into one dataset
#NOT ALL VARIBALES PICKED UP NEED TO CHECK WHAT'S OF
for(i in 2:6){ 
  if (i == 2) {
    bl2 <- full_join(bl[[i]], bl[[i+1]], by=c("DBN","School"))
    
  } else if (i > 4 & i <= 6) {
    bl2 <- full_join(bl2, bl[[i-1]], by=c("DBN","School"))
  }
}

#For the last one, closing the achievement score, that has to be separetly calculated. Below probably simpfiles
bl2$Score_14 <- .6*bl2[,3] + .25*bl2[,4]+.15*bl2[,5]+.16*bl2[,6]

bl3 <- na.omit(bl2)
quantile(bl3$Over, c(.75,.4,.1,.03)) 

bl3$Grade_14 <- NA

for(i in 1:nrow(bl3)) {
  if (bl3[i,]$Score_14 >= 69.7360) {
    bl3[i,]$Grade_14 <- "A"
  } else if (bl3[i,]$Score_14 >= 55.1280) {
    bl3[i,]$Grade_14 <- "B"
  } else if (bl3[i,]$Score_14 >= 39.8175) {
    bl3[i,]$Grade_14 <- "C"
  }else if (bl3[i,]$Score_14 >= 30.0915) { 
    bl3[i,]$Grade_14 <- "D"
  } else {
    bl3[i,]$Grade_14 <- "F"
  }
}
#Joining with the rest of the data and removing unnecessary variables
names(bl3)
bl3 <- select(bl3, DBN, Grade_14,Score_14)
p2 <- full_join(p,bl3,by=c("DBN"))


#Creating New Accountability Grades based on percentiles. Additional Credit missing...but still similar
#The additional score may also be calculated by reading how it was created
#Closing the Achievment gap is also a bit different than in previous year 
#Also will need to create percentiles by school type to be completly accurate
str(bl2)
15*prog$Environment + 55*prog$Progress + 30*prog$Performance + prog$`Additional Credit Total`

quantile(duration, c(.75,.4,.1,.3)) 

#Post 2014 data still has Student Achievement score allocated. Perhaphs could be used? See how the score correlates with past overall scores
