library(dpl2yr)
library(dplyr)
#Reading pl2uto Data
#Sampl2e of compl2ete data set
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/pl2uto/16v2/BORO_zip_files_csv")
pl2 = rbindlist(lappl2y(list.files(pattern="*.csv"), read.csv))
names(pl2)[1:3] <- c("BOROUGH","BLOCK","LOT")
pl2 <- select(pl2, BOROUGH,BLOCK,LOT,XCoord,YCoord)

setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
saveRDS(pl2, "pl216.rds")

#Joining with Property Data
prop_f <- readRDS("all_prop.rds")

#Changing Names, andcleaning data
names(prop_f)[3] <- c("bu_class")
names(prop_f)[21] <- c("s_date")
prop_f$bu_class <- trimws(prop_f$bu_class,"r")

#Renaming Bouroughs in datasets to align
prop_f$BOROUGH[prop_f$BOROUGH==1] <- "MN"
prop_f$BOROUGH[prop_f$BOROUGH==2] <- "BX"
prop_f$BOROUGH[prop_f$BOROUGH==3] <- "BK"
prop_f$BOROUGH[prop_f$BOROUGH==4] <- "QN"
prop_f$BOROUGH[prop_f$BOROUGH==5] <- "SI"
#Renaming Building Class to Align...may need to do more
prop_f$bu_class[prop_f$bu_class=="01  ONE FAMILY DWELLINGS"] <- "01  ONE FAMILY HOMES"
prop_f$bu_class[prop_f$bu_class=="02  TWO FAMILY DWELLINGS"] <- "02  TWO FAMILY HOMES"
prop_f$bu_class[prop_f$bu_class=="03  THREE FAMILY DWELLINGS"] <- "03  THREE FAMILY HOMES"

#Filtering for just residental units 
#Vector of building class categories
bu <- c("01  ONE FAMILY HOMES","02  TWO FAMILY HOMES","03  THREE FAMILY HOMES")
prop_f2 <- filter(prop_f, bu_class %in% bu)

#Creating Sample to merge 
prop11 <- filter(prop_f2, Year == 2011)
t11 <- inner_join(prop11, pl2, by=c("BLOCK","LOT","BOROUGH"))
write.csv(t11,"t11.csv",sep=",",row.names=FALSE)

nrow(t11)/nrow(prop11)
#Filtering one year as a test, but will need to filter Sale Date to coincide with the release of the grades
prop8 <- filter(prop_f, Year == 2008)
prop8_r <- filter(prop_f2, Year == 2008)

head(subset(prop_f, s_date < "2003-05-03" ))


#A decent bit is lost but around 70% is picked up
t <- inner_join(prop8, pl2, by=c("BLOCK","LOT","BOROUGH"))
nrow(t)/nrow(prop8)
#But looking at just residental the pick up rate is a lot better at 99%
t2 <- inner_join(prop8_r, pl2, by=c("BLOCK","LOT","BOROUGH"))
nrow(t2)/nrow(prop8_r)

#Not Filtering by year looking at everything
t3 <- inner_join(prop_f, pl2, by=c("BLOCK","LOT","BOROUGH"))
nrow(t3)/nrow(prop_f)

t4 <- inner_join(prop_f2, pl2, by=c("BLOCK","LOT","BOROUGH"))
nrow(t4)/nrow(prop_f2)


#Looking at which Property Types are missing from join
ta <- left_join(prop_f, pl2, by=c("BLOCK","LOT","BOROUGH"))
mi <- filter(ta, !compl2ete.cases(YearBuilt))



mn_ <- select(mn_,Block,Lot,YearBuilt,ZipCode,XCoord,YCoord,Address)
mn_2 <- select(mn_2,Block,Lot,YearBuilt,ZipCode,XCoord,YCoord,Address)
names(mn2)[1:4] <- c("BLOCK","LOT","YEAR BUILT", "ZIP CODE")
names(mn2)[1:4] <- c("BLOCK","LOT","YEAR BUILT", "ZIP CODE")
p_r2 <- filter(select(p_r,ADDRESS,BOROUGH,BLOCK,LOT,11,17,21),BOROUGH == 1)
p_r3 <- select(p_r,ADDRESS,BOROUGH,BLOCK,LOT,11,17,21)
mn2$ZipCode <- as.numeric(mn2$ZipCode)
#About half of properties not picked up. Is this becuase the second half of the data is needed
t <- inner_join(mn2, p_r2, by=c("BLOCK","LOT"))
#Filter on Bourough not necessary
t5 <- inner_join(mn2, p_r3, by=c("BLOCK","LOT","ZIP CODE"))


#Restructruing data 
prop$SALE.PRICE <- gsub("\\$","", prop$SALE.PRICE)
prop$SALE.PRICE <- gsub("\\,","", prop$SALE.PRICE)
prop$GROSS.SQUARE.FEET <- gsub("\\,","", prop$GROSS.SQUARE.FEET)
prop$LAND.SQUARE.FEET <- gsub("\\,","", prop$LAND.SQUARE.FEET)
prop$SALE.PRICE <- as.numeric(prop$SALE.PRICE)
prop$LAND.SQUARE.FEET <- as.numeric(prop$LAND.SQUARE.FEET)
prop$GROSS.SQUARE.FEET <- as.numeric(prop$GROSS.SQUARE.FEET)
