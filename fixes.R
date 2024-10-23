#Loads relevant pluto data to align with sales data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Pluto2")
.read.delim <- pryr::partial(read.delim, sep=",", dec=",")


#Need 09b to 15b
pl11a <-  lapply(list.files(pattern="*11v2.txt"), .read.delim)
pl12 <-  lapply(list.files(pattern="*12v1.txt"), .read.delim)
pl12a <-  lapply(list.files(pattern="*12v2.txt"), .read.delim)

pl11a <-  ldply(pl11a)
pl12 <- ldply(pl12)
pl12a <- ldply(pl12a)

#Picking only neccesry columns
cols <- c("Borough","Block","Lot","CT2010","CB2010","LtdHeight","OwnerType","LandUse","OwnerName","LotArea","BldgArea","ResArea","NumFloors","UnitsRes","UnitsTotal","AssessLand","AssessTot","YearAlter1","YearAlter2","HistDist","Landmark")
pl11a <- pl11a[,cols]
pl12 <- pl12[,cols]
pl12a <- pl12a[,cols]

#Fixing Pluto census blocks to align with census data and saving
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
pl11a <-  filter(pl11a, CB2010 > 0)
for(i in 1:nrow(pl11a)){ 
  if (nchar(pl11a$CB2010[i]) > 1) {
    pl11a$CB2010[i] <- substring(pl11a$CB2010[i], 1, 1)
  } else {
    pl11a$CB2010[i] <- pl11a$CB2010[i]
  }
}
saveRDS(pl11a,"pl11a.rds")
pl12 <-  filter(pl12, CB2010 > 0)
for(i in 1:nrow(pl12)){ 
  if (nchar(pl12$CB2010[i]) > 1) {
    pl12$CB2010[i] <- substring(pl12$CB2010[i], 1, 1)
  } else {
    pl12$CB2010[i] <- pl12$CB2010[i]
  }
}
saveRDS(pl12,"pl12.rds")
pl12a <-  filter(pl12a, CB2010 > 0)
for(i in 1:nrow(pl12a)){ 
  if (nchar(pl12a$CB2010[i]) > 1) {
    pl12a$CB2010[i] <- substring(pl12a$CB2010[i], 1, 1)
  } else {
    pl12a$CB2010[i] <- pl12a$CB2010[i]
  }
}
saveRDS(pl12a,"pl12a.rds")

#Joining Pluto with census varibales
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
ce <-  readRDS("ce.rds")

pl11a$CT2010 <-  as.numeric(as.character(pl11a$CT2010))
pl11a$CB2010 <-  as.numeric(pl11a$CB2010)
pl11a3 <- inner_join(pl11a, ce, by=c("CT2010","CB2010", "Borough"))
pl12$CT2010 <-  as.numeric(as.character(pl12$CT2010))
pl12_2 <- inner_join(pl12, ce, by=c("CT2010","CB2010", "Borough"))
pl12a$CT2010 <-  as.numeric(as.character(pl12a$CT2010))
pl12a2 <- inner_join(pl12a, ce, by=c("CT2010","CB2010", "Borough"))

#Loading sales data
za <-  readRDS("za_c.rds")
#Dates formatted slightly differently need to be fixed if using later years
za10 <-  filter(za,Y == 10)
za <-  filter(za,Y > 10)

za10$s_date <- as.Date(as.character(za10$s_date),"%m/%d/%Y")
za$s_date <-  as.Date(as.character(za$s_date,"%Y-%m-%d"))
za <-  rbind(za,za10)

za1 <-filter(za, s_date >= "2011-10-01", s_date < "2012-01-01")
za2 <-filter(za, s_date >= "2011-02-01", s_date < "2012-06-01")
za3 <-filter(za, s_date >= "2012-06-01", s_date < "2012-10-01")
names(pl11a)[1:3] <- c("BOROUGH","BLOCK","LOT")
names(pl12)[1:3] <- c("BOROUGH","BLOCK","LOT")
names(pl12a)[1:3] <- c("BOROUGH","BLOCK","LOT")
#A very small amount lost
za11 <-  inner_join(za1,pl11a, by=c("BOROUGH","BLOCK","LOT"))
za12 <-  inner_join(za2,pl12, by=c("BOROUGH","BLOCK","LOT"))
za12a <-  inner_join(za3,pl12a, by=c("BOROUGH","BLOCK","LOT"))

za_f <- rbind(za11,za12,za12a)
#Saving Clean final version
saveRDS(za_f,"za_f.rds")


#Redoing above for the rest of the data

#Loads relevant pluto data to align with sales data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Pluto2")
.read.delim <- pryr::partial(read.delim, sep=",", dec=",")
.read.csv <- pryr::partial(read.csv, sep=",", dec=",")

#up to 13 is text need up to 14-15
pl13 <-  ldply(list.files(pattern="*13v1.csv"), .read.csv)
#no 13 v2 data
pl14 <-  ldply(list.files(pattern="*14v1.csv"), .read.csv)
pl14a <-  ldply(list.files(pattern="*14v2.csv"), .read.csv)


#Puting the above in a list to pick up necessary colums. Older pluto needs to account for earlier census tracks
#Starting in 11v2 ct2010 used. So for now 09-10 and 10-11 merges with 11v2 for the relevant census varibales. 
cols <- c("Borough","Block","Lot","CT2010","CB2010","LtdHeight","OwnerType","LandUse","OwnerName","LotArea","BldgArea","ResArea","NumFloors","UnitsRes","UnitsTotal","AssessLand","AssessTot","YearAlter1","YearAlter2","HistDist","Landmark")

pl12a <-  pl12a[,cols]
pl13 <-  pl13[,cols]
pl14 <-  pl14[,cols]
pl14a <-  pl14a[,cols]

#Fixing Pluto census blocks to align with census data and saving
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")

pl13 <-  filter(pl13, CB2010 > 0)
for(i in 1:nrow(pl13)){ 
  if (nchar(pl13$CB2010[i]) > 1) {
    pl13$CB2010[i] <- substring(pl13$CB2010[i], 1, 1)
  } else {
    pl13$CB2010[i] <- pl13$CB2010[i]
  }
}
saveRDS(pl13,"pl13.rds")

pl13a <-  filter(pl13a, CB2010 > 0)
for(i in 1:nrow(pl13a)){ 
  if (nchar(pl13a$CB2010[i]) > 1) {
    pl13a$CB2010[i] <- substring(pl13a$CB2010[i], 1, 1)
  } else {
    pl13a$CB2010[i] <- pl13a$CB2010[i]
  }
}
saveRDS(pl13a,"pl13a.rds")

pl14 <-  filter(pl14, CB2010 > 0)
for(i in 1:nrow(pl14)){ 
  if (nchar(pl14$CB2010[i]) > 1) {
    pl14$CB2010[i] <- substring(pl14$CB2010[i], 1, 1)
  } else {
    pl14$CB2010[i] <- pl14$CB2010[i]
  }
}
saveRDS(pl14,"pl14.rds")

pl14a <-  filter(pl14a, CB2010 > 0)
for(i in 1:nrow(pl14a)){ 
  if (nchar(pl14a$CB2010[i]) > 1) {
    pl14a$CB2010[i] <- substring(pl14a$CB2010[i], 1, 1)
  } else {
    pl14a$CB2010[i] <- pl14a$CB2010[i]
  }
}
saveRDS(pl14a,"pl14a.rds")


#Joining Pluto with census varibales and loading other necessary datasets
#Reading it if used later and loading census data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
pl11a <-  readRDS("pl11a.rds")
pl12 <-  readRDS("pl12.rds")
pl12a <-  readRDS("pl12a.rds")
pl13 <-  readRDS("pl13.rds")
pl14 <-  readRDS("pl14.rds")
pl14a <-  readRDS("pl14a.rds")
ce <-  readRDS("ce.rds")


pl11a$CT2010 <-  as.numeric(as.character(pl11a$CT2010))
pl11a$CB2010 <-  as.numeric(pl11a$CB2010)
pl11a <- inner_join(pl11a, ce, by=c("CT2010","CB2010", "Borough"))
pl12$CT2010 <-  as.numeric(as.character(pl12$CT2010))
pl12$CB2010 <-  as.numeric(pl12$CB2010)
pl12 <- inner_join(pl12, ce, by=c("CT2010","CB2010", "Borough"))
pl12a$CT2010 <-  as.numeric(as.character(pl12a$CT2010))
pl12a$CB2010 <-  as.numeric(pl12a$CB2010)
pl12a <- inner_join(pl12a, ce, by=c("CT2010","CB2010", "Borough"))
pl13$CT2010 <-  as.numeric(as.character(pl13$CT2010))
pl13$CB2010 <-  as.numeric(pl13$CB2010)
pl13 <- inner_join(pl13, ce, by=c("CT2010","CB2010", "Borough"))
pl14$CT2010 <-  as.numeric(as.character(pl14$CT2010))
pl14$CB2010 <-  as.numeric(pl14$CB2010)
pl14 <- inner_join(pl14, ce, by=c("CT2010","CB2010", "Borough"))
pl14a$CT2010 <-  as.numeric(as.character(pl14a$CT2010))
pl14a$CB2010 <-  as.numeric(pl14a$CB2010)
pl14a <- inner_join(pl14a, ce, by=c("CT2010","CB2010", "Borough"))


#Loading sales data and fixing dates
za <-  readRDS("za2.rds")
za10 <-  filter(za,Y == 15)
za <-  filter(za, Y <15 )

za10$s_date <- as.Date(as.character(za10$s_date),"%m/%d/%Y")
za$s_date <-  as.Date(as.character(za$s_date,"%Y-%m-%d"))
za <-  rbind(za,za10)

#09-10
za10 <- filter(za, s_date >= "2009-10-01", s_date < "2010-01-01")
za10a <- filter(za, s_date >= "2010-01-01", s_date < "2010-06-01")
za10b <- filter(za, s_date >= "2010-06-01", s_date < "2010-10-01")

#10-11
za11 <- filter(za, s_date >= "2010-10-01", s_date < "2011-01-01")
za11a <- filter(za, s_date >= "2011-01-01", s_date < "2011-06-01")
za11b <- filter(za, s_date >= "2011-06-01", s_date < "2011-10-01")

#11-12
za12 <- filter(za, s_date >= "2011-10-01", s_date < "2012-01-01")
za12a <- filter(za, s_date >= "2012-01-01", s_date < "2012-06-01")
za12b <- filter(za, s_date >= "2012-06-01", s_date < "2012-10-01")

#12-13
za13 <- filter(za, s_date >= "2012-10-01", s_date < "2013-01-01")
za13a <- filter(za, s_date >= "2013-01-01", s_date < "2013-06-01")
za13b <- filter(za, s_date >= "2013-06-01", s_date < "2013-10-01")

#13-14
za14 <- filter(za, s_date >= "2013-10-01", s_date < "2014-01-01")
za14a <- filter(za, s_date >= "2014-01-01", s_date < "2014-06-01")
za14b <- filter(za, s_date >= "2014-06-01", s_date < "2014-10-01")

#Added on if needed later
#14-15
za15 <- filter(za, s_date >= "2014-10-01", s_date < "2015-01-01")
za15a <- filter(za, s_date >= "2015-01-01", s_date < "2015-06-01")
za15b <- filter(za, s_date >= "2015-06-01", s_date < "2015-10-01")

#15-16
za16 <- filter(za, s_date >= "2015-10-01", s_date < "2016-01-01")
za16a <- filter(za, s_date >= "2016-01-01", s_date < "2016-06-01")
za16b <- filter(za, s_date >= "2016-06-01", s_date < "2016-10-01")

#16-17
za17 <- filter(za, s_date >= "2016-10-01", s_date < "2017-01-01")
za17a <- filter(za, s_date >= "2017-01-01", s_date < "2017-06-01")
za17b <- filter(za, s_date >= "2017-06-01", s_date < "2017-10-01")


#Renaming necessary varibales in list

names(pl11a)[1:3] <- c("BOROUGH","BLOCK","LOT")
names(pl12)[1:3] <- c("BOROUGH","BLOCK","LOT")
names(pl12a)[1:3] <- c("BOROUGH","BLOCK","LOT")
names(pl13)[1:3] <- c("BOROUGH","BLOCK","LOT")
names(pl14)[1:3] <- c("BOROUGH","BLOCK","LOT")
names(pl14a)[1:3] <- c("BOROUGH","BLOCK","LOT")


#Putting above in list
pl <- list(pl11a,pl12,pl12a,pl13,pl14,pl14a)


#Joins and save. Check how much picked up too

#09-10
#Joins with 11a
za10c <-  inner_join(za10,pl[[1]], by=c("BOROUGH","BLOCK","LOT"))
za10d <-  inner_join(za10a,pl[[1]], by=c("BOROUGH","BLOCK","LOT"))
za10e <-  inner_join(za10b,pl[[1]], by=c("BOROUGH","BLOCK","LOT"))
za_f10 <- rbind(za10c,za10d,za10e)
saveRDS(za_f10,"za_f10.rds")

#10-11
#Joins with 11a
za11c <-  inner_join(za11,pl[[1]], by=c("BOROUGH","BLOCK","LOT"))
za11d <-  inner_join(za11a,pl[[1]], by=c("BOROUGH","BLOCK","LOT"))
za11e <-  inner_join(za11b,pl[[1]], by=c("BOROUGH","BLOCK","LOT"))
za_f11 <- rbind(za11c,za11d,za11e)
saveRDS(za_f11,"za_f11.rds")

#11-12

za12c <-  inner_join(za12,pl[[1]], by=c("BOROUGH","BLOCK","LOT"))
za12d <-  inner_join(za12a,pl[[2]], by=c("BOROUGH","BLOCK","LOT"))
za12e <-  inner_join(za12b,pl[[3]], by=c("BOROUGH","BLOCK","LOT"))
za_f12 <- rbind(za12c,za12d,za12e)
saveRDS(za_f12,"za_f12.rds")

#12-13
za13c <-  inner_join(za13,pl[[3]], by=c("BOROUGH","BLOCK","LOT"))
za13d <-  inner_join(za13a,pl[[4]], by=c("BOROUGH","BLOCK","LOT"))
za13e <-  inner_join(za13b,pl[[4]], by=c("BOROUGH","BLOCK","LOT"))
za_f13 <- rbind(za13c,za13d,za13e)
saveRDS(za_f13,"za_f13.rds")

#13-14 Need to fix sales data for this and this is the last year as accountability ended for 2012-2013 school year
za14c <-  inner_join(za14,pl[[4]], by=c("BOROUGH","BLOCK","LOT"))
za14d <-  inner_join(za14a,pl[[5]], by=c("BOROUGH","BLOCK","LOT"))
za14e <-  inner_join(za14b,pl[[6]], by=c("BOROUGH","BLOCK","LOT"))
za_f14 <- rbind(za14c,za14d,za14e)
saveRDS(za_f14,"za_f14.rds")

