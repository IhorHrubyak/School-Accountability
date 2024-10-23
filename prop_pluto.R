#The below merges Pluto Data with Property data and breaks down year by year
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
pl <- readRDS("p_f5.rds")
prop_f <- readRDS("all_prop.rds")
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
#Renaming Building Class to Align
prop_f$bu_class[prop_f$bu_class=="01 ONE FAMILY DWELLINGS"] <- "01 ONE FAMILY HOMES"
prop_f$bu_class[prop_f$bu_class=="02 TWO FAMILY DWELLINGS"] <- "02 TWO FAMILY HOMES"
prop_f$bu_class[prop_f$bu_class=="03 THREE FAMILY DWELLINGS"] <- "03 THREE FAMILY HOMES"

#Only selecting property sales from relevant years
prop_f <-  filter(prop_f, s_date > "2009-10-01")

#Intial join
prop_pl <- inner_join(prop_f, pl, by=c("BLOCK","LOT","BOROUGH"))

#Duplicates likely from different address spellings so making sure unique property transactions are picked up
prop_pl2 <-   unique(prop_pl)
#The intial join still misses a good bit of property
nrow(prop_pl2)/nrow(prop_f)


#Another join including address and block. Picks up less but address isn't exactly matched in a number of instances
prop_pl3 <- inner_join(prop_f, pl, by=c("BLOCK","BOROUGH","ADDRESS"))
prop_pl4 <-  unique(prop_pl3)
pl_a <- pl$ADDRESS
pf_a <- prop_f$ADDRESS
pf_a <- as.data.frame(pf_a)
pl_a <- as.data.frame(pl_a)
names(pl_a) <- "a"
names(pf_a) <- "a"
pp <-  stringdist_inner_join(pf_a,pl_a, by = c("a"))

#Dropping and renaming varibales from the two datasets above to enable bind
#MAKE SURE TO INCLUDE LOT DATA FROM PLUTO NOT FINANCE AS NEEDED FOR LATER
drop <- "ADDRESS.y"
prop_pl2 <-  prop_pl2[ , !(names(prop_pl2) %in% drop)]
prop_pl4 <-  arrange(prop_pl4,-LOT.y, -LOT.x )
drop2 <- "LOT.x"
prop_pl4 <-  prop_pl4[ , !(names(prop_pl4) %in% drop2)]
names(prop_pl2)[9] <- "ADDRESS"
names(prop_pl4)[22] <- "LOT"
#Combining the two different joins above and removing duplicates
prop_pl5 <-  unique(rbind(prop_pl4,prop_pl2))
#Again making sure no duplicates of specific property
prop_pl6 <-  prop_pl5[!duplicated(prop_pl5[c("BOROUGH","BLOCK","LOT","XCoord","YCoord","ADDRESS","s_date")]),]

#OVerall amount of sales picked up and picked up out of years of relevance
#87% overall and 86% for relevant years
nrow(prop_pl6)/nrow(prop_f)

nrow(filter(prop_pl6, s_date > "2009-10-01", s_date < "2015-10-01"))/nrow(filter(prop_f, s_date > "2009-10-01", s_date < "2015-10-01"))

#Amount picked up by property type and relevant years

#over 99% of single family homes pretty good
nrow(filter(prop_pl6, s_date > "2009-10-01", s_date < "2015-10-01", bu_class == "01  ONE FAMILY HOMES"))/nrow(filter(prop_f, s_date > "2009-10-01", s_date < "2015-10-01",bu_class == "01  ONE FAMILY HOMES"))
#only 62% conods
nrow(filter(prop_pl6, s_date > "2009-10-01", s_date < "2015-10-01", bu_class == "13  CONDOS - ELEVATOR APARTMENTS"))/nrow(filter(prop_f, s_date > "2009-10-01", s_date < "2015-10-01",bu_class == "13  CONDOS - ELEVATOR APARTMENTS"))

write.csv(prop_pl6,"p_full.csv",row.names = FALSE)
write.csv(filter(prop_pl6, s_date > "2009-10-01", s_date < "2010-10-01"),"p9_10.csv",row.names = FALSE)
write.csv(filter(prop_pl6, s_date > "2010-10-01", s_date < "2011-10-01"),"p10_11.csv",row.names = FALSE)
write.csv(filter(prop_pl6, s_date > "2011-10-01", s_date < "2012-10-01"),"p11_12.csv",row.names = FALSE)
write.csv(filter(prop_pl6, s_date > "2012-10-01", s_date < "2013-10-01"),"p12_13.csv",row.names = FALSE)
#Ex-post correction no need to breakup post 2014 data can just filter later if needed
write.csv(filter(prop_pl6, s_date > "2013-10-01", s_date < "2014-10-01"),"p13_14.csv",row.names = FALSE)
write.csv(filter(prop_pl6, s_date > "2014-10-01", s_date < "2015-10-01"),"p14_15.csv",row.names = FALSE)
write.csv(filter(prop_pl6, s_date > "2015-10-01"),"p15-.csv",row.names = FALSE)
write.csv(rbind(read.csv("p13_14.csv"),read.csv("p14_15.csv"),read.csv("p15-.csv")),"p13-.csv")
