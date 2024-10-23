#Results
#Loading necessary datasets
library(dplyr)
library(stargazer)
library(lubridate)
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
#Combine all datasets into one and having them separte depending on what's needed
za10 <-  readRDS("za_f10.rds")
za11 <-  readRDS("za_f11.rds")
za12 <-  readRDS("za_f12.rds")
za13 <-  readRDS("za_f13.rds")
za14 <-  readRDS("za_f14.rds")


za10$Y <- 10
za11$Y <- 11
za12$Y <- 12
za13$Y <- 13
za14$Y <- 14

#Fixing grade names
names(za10)[54:55] <- c("Grade","Score")
names(za11)[58:59] <- c("Grade","Score")
names(za12)[62:63] <- c("Grade","Score")
names(za13)[66:67] <- c("Grade","Score")
names(za14)[69:70] <- c("Grade","Score")

drops <- c("Grade_7","Grade_8","Grade_9","Grade_10","Grade_11","Grade_12","Grade_13","Score_7","Score_8","Score_9","Score_10","Score_11","Score_12","Score_13","Peer_7","Peer_8","Peer_9","Peer_10","Peer_11","Peer_12","Peer_13","Percentile_7","Percentile_8","Percentile_9","Percentile_10","Percentile_11","Percentile_12","Percentile_13")
za10 <-  za10[ , !(names(za10) %in% drops)]
za11 <-  za11[ , !(names(za11) %in% drops)]
za12 <-  za12[ , !(names(za12) %in% drops)]
za13 <-  za13[ , !(names(za13) %in% drops)]
za14 <-  za14[ , !(names(za14) %in% drops)]


za <-  rbind(za10,za11,za12,za13,za14)

#Intial filters to select neceesary spans
#Removing extreme highs and lows, and selecting only relevant propert types
#No Condos in the below for now
#Renaming property types to align, fixing varibales, and selecting only necessary varibales
#Removing any duplicates...a small amount found not sure why
za <-  za[!duplicated(za[c("ADDRESS","s_date","OwnerName")]),]

za$bu_class[za$bu_class=="01  ONE FAMILY DWELLINGS"] <- "01  ONE FAMILY HOMES"
za$bu_class[za$bu_class=="02  TWO FAMILY DWELLINGS"] <- "02  TWO FAMILY HOMES"
za$bu_class[za$bu_class=="03  THREE FAMILY DWELLINGS"] <- "03  THREE FAMILY HOMES"
za$Score <- as.numeric(za$Score)
za$NumFloors <- as.numeric(za$NumFloors)
za$YEAR.BUILT <- as.numeric(za$YEAR.BUILT >= 1950)
za$yb <- za$YEAR.BUILT - 1950
za$yb2 <- za$yb^2
za$yb3 <- za$yb^3
za$ya1 <-  as.numeric(za$YearAlter1 == 0)
za$ya2 <-  as.numeric(za$YearAlter2 == 0)
za$ya1a <-  as.numeric(za$YearAlter1 >= 1990)
za$ya2a <-  as.numeric(za$YearAlter2 >= 1980)
za$m <-  as.factor(month(za$s_date))

saveRDS(za, "za3.rds")

za2 <-  filter(za, SALE.PRICE > 10000, SALE.PRICE < 3000000)
za2 <- filter(za2, bu_class == "01  ONE FAMILY HOMES" | bu_class == "02  TWO FAMILY HOMES" | bu_class == "03  THREE FAMILY HOMES"
              | bu_class == "09  COOPS - WALKUP APARTMENTS"
              | bu_class == "10  COOPS - ELEVATOR APARTMENTS" | bu_class == "12  CONDOS - WALKUP APARTMENTS" | bu_class == "13  CONDOS - ELEVATOR APARTMENTS" 
              | bu_class == "15  CONDOS - 2-10 UNIT RESIDENTIAL" |bu_class == "04  TAX CLASS 1 CONDOS")

za2 <- filter(za2, bu_class == "01  ONE FAMILY HOMES" | bu_class == "02  TWO FAMILY HOMES" | bu_class == "03  THREE FAMILY HOMES")
za2 <- filter(za2, bu_class == "21  OFFICE BUILDINGS" | bu_class == "22  STORE BUILDINGS" 
              | bu_class == "29  COMMERCIAL GARAGES" | bu_class ==  "27  FACTORIES" | bu_class == "30  WAREHOUSES" )
za2 <- select(za2, 1,45,46,61,62,71,73,84:85,89,117,120:121,123:128,130,139,141,142,144,147:149,152:154)
#If using old db that keeps grades by year
za2 <- select(za2, 1,44:71,86,87,96,98,109:110,114,142,145,146,148:153,155,164,166:167,169,172:174,177:178,179,120)

names(za2)[4:30] <- c("Math Score", "English Score","Borough","Building Class","Square Feet Land", "Gross Square Feet","Sale Price", "Average Household Size",
                       "Median Age", "Average Family Size","Percentage 1 person household","Percentage 2 person household","Percentage 3 person household",
                       "Percentage 4 person household","Percentage 5 person household","Percentage 6 person household","Percentage Vacant Housing Units",
                       "Percentage Family Households","Percentage White","Percentage Black","Percentage Asian","Year Built - 1950",
                       "(Year Built - 1950)^2","(Year Built - 1950)^3","Year Alterted 1 > 1990","Year Alterted 2 > 1980","Month")


#Just test score for all years
p <- select(za2, 31:48,50:56)

r <-  lm(log(p$`Sale Price`) ~  .,data=p)
stargazer(r, type = "text", keep = c("English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"))

#For math scores
p <- select(za2, 30,32:48,50:56)

r2 <-  lm(log(p$`Sale Price`) ~  .,data=p)
stargazer(r2, type = "text", keep = c("Math Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"))


#Intial Regressions by Year
#10. 2008-2009 grades on 2009-2010 sales
#10 weird as almost 90% of sales were associated with A school and there are no F schools 
#Just Grades
pl10 <- select(za2, 12,32:48,50:57)
#Grade and Score
pl10 <- select(za2, 12:13,32:48,50:57)
reg10 <- lm(log(pl10$`Sale Price`) ~  .,data=pl10,subset=(Y=="10"))
stargazer(reg10, type = "text", keep = c("Grade_9","eng_sco","math_sco"))

pl10 <- select(za2, 12:13,31,32:48,50:57)

pl3 <- filter(pl10, Grade_9 == "A" | Grade_9 == "B" )
pl4 <- filter(pl10, Grade_9 == "B" | Grade_9 == "C" )
pl5 <- filter(pl10, Grade_9 == "C" | Grade_9 == "D" )
reg10a <- lm(log(pl3$`Sale Price`) ~  .,data=pl3,subset=(Y=="10"))
reg10b <- lm(log(pl4$`Sale Price`) ~  .,data=pl4,subset=(Y=="10"))
reg10c <- lm(log(pl5$`Sale Price`) ~  .,data=pl5,subset=(Y=="10"))
reg10d <- lm(log(pl6$`Sale Price`) ~  .,data=pl6,subset=(Y=="10"))

stargazer(reg10a,reg10b, type = "text", keep = c("Grade_9","English"))

#11
#11 distriubtion of grades is more normal not much signifigance unless I use y=10 for some reason
pl11 <- select(pl2, 15:18,30:92)
nrow(filter(pl11, Grade_10 == "A"))/nrow(pl11)
table(pl11$Grade_10)
reg11 <- lm(log(pl11$SALE.PRICE) ~  .,data=pl11,subset=(Y=="11"))
stargazer(reg11, type = "text", keep = c("Grade_10","eng_sco","math_sco"))

pl3 <- filter(pl11, Grade_10 == "A" | Grade_10 == "B" )
pl4 <- filter(pl11, Grade_10 == "B" | Grade_10 == "C" )
pl5 <- filter(pl11, Grade_10 == "C" | Grade_10 == "D" )
pl6 <- filter(pl11, Grade_10 == "D" | Grade_10 == "F" )
reg11a <- lm(log(pl3$SALE.PRICE) ~  .,data=pl3,subset=(Y=="11"))
reg11b <- lm(log(pl4$SALE.PRICE) ~  .,data=pl4,subset=(Y=="11"))
reg11c <- lm(log(pl5$SALE.PRICE) ~  .,data=pl5,subset=(Y=="11"))
reg11d <- lm(log(pl6$SALE.PRICE) ~  .,data=pl6,subset=(Y=="11"))
stargazer(reg11a,reg11b,reg11c, type = "text", keep = c("Grade_10","eng_sco","math_sco"))

#12...very signifigant for all years except F unless done along the grades
pl12 <- select(pl2, 20:21,30:84)
pl122 <- select(pl12, 1:2,8:11,12,14,16:17,19,28:57,21)
pl12$Score_11 <-  as.numeric(pl12$Score_11)
pl122$Score_11 <-  as.numeric(pl122$Score_11)
nrow(filter(pl12, Grade_11 == "A"))/nrow(pl12)
table(pl12$Grade_11)
reg12 <- lm(log(pl12$SALE.PRICE) ~  .,data=pl12,subset=(Y=="12"))
summary(lm(log(pl122$SALE.PRICE) ~  .,data=pl122,subset=(Y=="12")))
reg12 <- lm(log(pl122$SALE.PRICE) ~  .,data=pl122,subset=(Y=="12"))
stargazer(reg12, type = "text", keep = c("Grade_11","eng_sco","math_sco"))

pl3 <- filter(pl12, Grade_11 == "A" | Grade_11 == "B" )
pl4 <- filter(pl12, Grade_11 == "B" | Grade_11 == "C" )
pl5 <- filter(pl12, Grade_11 == "C" | Grade_11 == "D" )
pl6 <- filter(pl12, Grade_11 == "D" | Grade_11 == "F" )
reg12a <- lm(log(pl3$SALE.PRICE) ~  .,data=pl3,subset=(Y=="12"))
reg12b <- lm(log(pl4$SALE.PRICE) ~  .,data=pl4,subset=(Y=="12"))
reg12c <- lm(log(pl5$SALE.PRICE) ~  .,data=pl5,subset=(Y=="12"))
reg12d <- lm(log(pl6$SALE.PRICE) ~  .,data=pl6,subset=(Y=="12"))
stargazer(reg12a,reg12b,reg12c, type = "text", keep = c("Grade_11","eng_sco","math_sco"))

#13
pl13 <- select(pl2, 23:26,30:92)
pl13$Score_12 <-  as.numeric(pl13$Score_12)
nrow(filter(pl13, Grade_12 == "A"))/nrow(pl13)
table(pl13$Grade_12)
reg13 <- lm(log(pl13$SALE.PRICE) ~  .,data=pl13,subset=(Y=="13"))
stargazer(reg13, type = "text", keep = c("Grade_12","eng_sco","math_sco"))

stargazer(reg13, type = "text")

pl3 <- filter(pl13, Grade_12 == "A" | Grade_12 == "B" )
pl4 <- filter(pl13, Grade_12 == "B" | Grade_12 == "C" )
pl5 <- filter(pl13, Grade_12 == "C" | Grade_12 == "D" )
pl6 <- filter(pl13, Grade_12 == "D" | Grade_12 == "F" )
reg13a <- lm(log(pl3$SALE.PRICE) ~  .,data=pl3,subset=(Y=="13"))
reg13b <- lm(log(pl4$SALE.PRICE) ~  .,data=pl4,subset=(Y=="13"))
reg13c <- lm(log(pl5$SALE.PRICE) ~  .,data=pl5,subset=(Y=="13"))
reg13d <- lm(log(pl6$SALE.PRICE) ~  .,data=pl6,subset=(Y=="13"))
stargazer(reg13a,reg13b,reg13c, type = "text", keep = c("Grade_12","eng_sco","math_sco"))

#14...only 150 something properties so can't use now
pl14 <- select(pl2, 26:29,30:92)
pl14$Score_13 <-  as.numeric(pl14$Score_13)
nrow(filter(pl14, Grade_13 == "A"))/nrow(pl14)
nrow(filter(pl14, Y=="14"))
table(pl14$Grade_13)
reg14 <- lm(log(pl14$SALE.PRICE) ~  .,data=pl14,subset=(Y=="14"))
stargazer(reg14, type = "text", keep = c("Grade_13","eng_sco","math_sco"))

pl3 <- filter(pl14, Grade_13 == "A" | Grade_13 == "B" )
pl4 <- filter(pl14, Grade_13 == "B" | Grade_13 == "C" )
pl5 <- filter(pl14, Grade_13 == "C" | Grade_13 == "D" )
pl6 <- filter(pl14, Grade_13 == "D" | Grade_13 == "F" )
reg14a <- lm(log(pl3$SALE.PRICE) ~  .,data=pl3,subset=(Y=="14"))
reg14b <- lm(log(pl4$SALE.PRICE) ~  .,data=pl4,subset=(Y=="14"))
reg14c <- lm(log(pl5$SALE.PRICE) ~  .,data=pl5,subset=(Y=="14"))
reg14d <- lm(log(pl6$SALE.PRICE) ~  .,data=pl6,subset=(Y=="14"))
stargazer(reg14a,reg14b,reg14c, type = "text", keep = c("Grade_13","eng_sco","math_sco"))

#Alternative specifications

#Just Grades and test scores

#Narrowing band of sale date
pl2 <- filter(pl, bu_class == "01  ONE FAMILY HOMES" | bu_class == "02  TWO FAMILY HOMES" | bu_class == "03  THREE FAMILY HOMES" 
              | bu_class == "07  RENTALS - WALKUP APARTMENTS" | bu_class == "08  RENTALS - ELEVATOR APARTMENTS" | bu_class == "09  COOPS - WALKUP APARTMENTS"
              | bu_class == "10  COOPS - ELEVATOR APARTMENTS" | bu_class == "12  CONDOS - WALKUP APARTMENTS" | bu_class == "13  CONDOS - ELEVATOR APARTMENTS")

pl2$Score_11 <- as.numeric(pl2$Score_11)
pl2$Score_12 <- as.numeric(pl2$Score_12)
pl2$Score_13 <- as.numeric(pl2$Score_13)
#No test scores
pl_n <- select(pl2, 1:29,41,43)

pl10 <- select(pl_n, 12,13,30:31)
pl11 <- select(pl_n, 16,17,30:31)
pl12 <- select(pl_n, 20,21,30:31)
pl13 <- select(pl_n, 24,25,30:31)
reg10 <- lm(log(pl10$SALE.PRICE) ~  .,data=pl10,subset=(Y=="10"))
reg11 <- lm(log(pl11$SALE.PRICE) ~  .,data=pl11,subset=(Y=="11"))
reg12 <- lm(log(pl12$SALE.PRICE) ~  .,data=pl12,subset=(Y=="12"))
reg13 <- lm(log(pl12$SALE.PRICE) ~  .,data=pl13,subset=(Y=="13"))
stargazer(reg10,reg11,reg12,reg13, type = "text",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013"),style="qje")

#With Test Scores
pl_n <- select(pl2, 1:29,31,33,41,43)
pl10 <- select(pl_n, 12,13,30:33)
pl11 <- select(pl_n, 16,17,30:33)
pl12 <- select(pl_n, 20,21,30:33)
pl13 <- select(pl_n, 24,25,30:33)
pl14 <- select(pl_n, 27,28,30:33)
reg10 <- lm(log(pl10$SALE.PRICE) ~  .,data=pl10,subset=(Y=="10"))
reg11 <- lm(log(pl11$SALE.PRICE) ~  .,data=pl11,subset=(Y=="11"))
reg12 <- lm(log(pl12$SALE.PRICE) ~  .,data=pl12,subset=(Y=="12"))
reg13 <- lm(log(pl13$SALE.PRICE) ~  .,data=pl13,subset=(Y=="13"))
stargazer(reg10,reg11,reg12,reg13, type = "text")


#With property controls
pl_n <- select(pl2, 1:29,31,33,36:39,41,43,45:52,87:91)
pl_n$bu_class <-  as.character(pl_n$bu_class)
pl10 <- select(pl_n, 12,13,30:47)
pl11 <- select(pl_n, 16,17,30:47)
pl12 <- select(pl_n, 20,21,30:47)
pl13 <- select(pl_n, 24,25,30:47)
reg10 <- lm(log(pl10$SALE.PRICE) ~  .,data=pl10,subset=(Y=="10"))
reg11 <- lm(log(pl11$SALE.PRICE) ~  .,data=pl11,subset=(Y=="11"))
reg12 <- lm(log(pl12$SALE.PRICE) ~  .,data=pl12,subset=(Y=="12"))
reg13 <- lm(log(pl12$SALE.PRICE) ~  .,data=pl13,subset=(Y=="13"))
stargazer(reg10,reg11,reg12,reg13, type = "text",keep = c("Grade_9","Grade_10","Grade_11","Grade_12","math_sco","eng_sco"))

#With Demographic Controls and everything else
pl_n <- select(pl2, 1:29,31,33,36:39,41,43,45:52,55:91)
pl_n$bu_class <-  as.character(pl_n$bu_class)
pl10 <- select(pl_n, 12,13,30:80)
pl11 <- select(pl_n, 16,14,30:80)
pl12 <- select(pl_n, 20,21,30:80)
pl13 <- select(pl_n, 24,25,30:80)
reg10 <- lm(log(pl10$SALE.PRICE) ~  .,data=pl10,subset=(Y=="10"))
reg11 <- lm(log(pl11$SALE.PRICE) ~  .,data=pl11,subset=(Y=="11"))
reg12 <- lm(log(pl12$SALE.PRICE) ~  .,data=pl12,subset=(Y=="12"))
reg13 <- lm(log(pl12$SALE.PRICE) ~  .,data=pl13,subset=(Y=="13"))
stargazer(reg10,reg11,reg12,reg13, type = "text",keep = c("Grade_9","Grade_10","Grade_11","Grade_12","math_sco","eng_sco"))

stargazer(reg10,reg11,reg12,reg13, type = "text")


#By Grade
pl3 <- filter(pl10, Grade_9 == "A" | Grade_9 == "B" )
pl4 <- filter(pl10, Grade_9 == "B" | Grade_9 == "C" )
pl5 <- filter(pl10, Grade_9 == "C" | Grade_9 == "D" )
reg10a <- lm(log(pl3$SALE.PRICE) ~  .,data=pl3,subset=(Y=="10"))
reg10b <- lm(log(pl4$SALE.PRICE) ~  .,data=pl4,subset=(Y=="10"))
reg10c <- lm(log(pl5$SALE.PRICE) ~  .,data=pl5,subset=(Y=="10"))
reg10d <- lm(log(pl6$SALE.PRICE) ~  .,data=pl6,subset=(Y=="10"))

stargazer(reg10a,reg10b,reg10c,type = "text", keep = c("Grade_9","eng_sco","math_sco"))
#11
#11 distriubtion of grades is more normal not much signifigance unless I use y=10 for some reason
pl3 <- filter(pl11, Grade_10 == "A" | Grade_10 == "B" )
pl4 <- filter(pl11, Grade_10 == "B" | Grade_10 == "C" )
pl5 <- filter(pl11, Grade_10 == "C" | Grade_10 == "D" )
pl6 <- filter(pl11, Grade_10 == "D" | Grade_10 == "F" )
reg11a <- lm(log(pl3$SALE.PRICE) ~  .,data=pl3,subset=(Y=="11"))
reg11b <- lm(log(pl4$SALE.PRICE) ~  .,data=pl4,subset=(Y=="11"))
reg11c <- lm(log(pl5$SALE.PRICE) ~  .,data=pl5,subset=(Y=="11"))
reg11d <- lm(log(pl6$SALE.PRICE) ~  .,data=pl6,subset=(Y=="11"))
stargazer(reg11a,reg11b,reg11c, type = "text", keep = c("Grade_10","eng_sco","math_sco"))

#12...very signifigant for all years except F unless done along the grades
pl3 <- filter(pl12, Grade_11 == "A" | Grade_11 == "B" )
pl4 <- filter(pl12, Grade_11 == "B" | Grade_11 == "C" )
pl5 <- filter(pl12, Grade_11 == "C" | Grade_11 == "D" )
pl6 <- filter(pl12, Grade_11 == "D" | Grade_11 == "F" )
reg12a <- lm(log(pl3$SALE.PRICE) ~  .,data=pl3,subset=(Y=="12"))
reg12b <- lm(log(pl4$SALE.PRICE) ~  .,data=pl4,subset=(Y=="12"))
reg12c <- lm(log(pl5$SALE.PRICE) ~  .,data=pl5,subset=(Y=="12"))
reg12d <- lm(log(pl6$SALE.PRICE) ~  .,data=pl6,subset=(Y=="12"))
stargazer(reg12a,reg12b,reg12c, type = "text", keep = c("Grade_11","eng_sco","math_sco"))
#13

pl3 <- filter(pl13, Grade_12 == "A" | Grade_12 == "B" )
pl4 <- filter(pl13, Grade_12 == "B" | Grade_12 == "C" )
pl5 <- filter(pl13, Grade_12 == "C" | Grade_12 == "D" )
pl6 <- filter(pl13, Grade_12 == "D" | Grade_12 == "F" )
reg13a <- lm(log(pl3$SALE.PRICE) ~  .,data=pl3,subset=(Y=="13"))
reg13b <- lm(log(pl4$SALE.PRICE) ~  .,data=pl4,subset=(Y=="13"))
reg13c <- lm(log(pl5$SALE.PRICE) ~  .,data=pl5,subset=(Y=="13"))
reg13d <- lm(log(pl6$SALE.PRICE) ~  .,data=pl6,subset=(Y=="13"))
stargazer(reg13a,reg13b,reg13c, type = "text", keep = c("Grade_12","eng_sco","math_sco"))



#Could make a nice table showing the grade effects for each of the years. Just need to rename the grades for each reg then throw it in stargazer
#Could also include previous year grades to see what effect they had


reg <-  lm(log(t2$SALE.PRICE) ~ .,data = t)

#Checks on how many properties by property type kept. For family homes make sure dwellings not renmaed in za
za2 <-  readRDS("za2.rds")
nrow(filter(za, bu_class == "01  ONE FAMILY HOMES"))/nrow(filter(za2, bu_class == "01  ONE FAMILY HOMES"))
nrow(filter(za, bu_class == "07  RENTALS - WALKUP APARTMENTS"))/nrow(filter(za2, bu_class == "07  RENTALS - WALKUP APARTMENTS"))
nrow(filter(za, bu_class == "09  COOPS - WALKUP APARTMENTS"))/nrow(filter(za2, bu_class == "09  COOPS - WALKUP APARTMENTS"))
nrow(filter(za, bu_class == "17  CONDOPS "))/nrow(filter(za2, bu_class == "17  CONDOPS"))




###FIXED EFFECTS

#Fixed effects...this has to have have the grades collapse into one varibale
#selecting only relevant for grade for each year, dropping the rest then renaming
#NEED TO CHECK THAT DUPLICATES GONE FROM THESE!
za10 <-  select(za10,1,54,55,78,86,96,98,109,111,129,135,136,114)
names(za10)[2:3] <- c("Grade","Score")
za11 <-  select(za11,1,58,59,78,86,96,98,109,111,129,135,136,114)
names(za11)[2:3] <- c("Grade","Score")
za12 <-  select(za12,1,62,53,78,86,96,98,109,111,129,135,136,114)
names(za12)[2:3] <- c("Grade","Score")
za13 <-  select(za13,1,66,67,78,86,96,98,109,111,129,135,136,114)
names(za13)[2:3] <- c("Grade","Score")
za14 <-  select(za14,1,69,70,78,86,96,98,109,111,129,135,136,114)
names(za14)[2:3] <- c("Grade","Score")

za3 <-  rbind(za10,za11,za12,za13,za14)

za3$bu_class[za3$bu_class=="01  ONE FAMILY DWELLINGS"] <- "01  ONE FAMILY HOMES"
za3$bu_class[za3$bu_class=="02  TWO FAMILY DWELLINGS"] <- "02  TWO FAMILY HOMES"
za3$bu_class[za3$bu_class=="03  THREE FAMILY DWELLINGS"] <- "03  THREE FAMILY HOMES"

za$bu_class[za$bu_class=="01  ONE FAMILY DWELLINGS"] <- "01  ONE FAMILY HOMES"
za$bu_class[za$bu_class=="02  TWO FAMILY DWELLINGS"] <- "02  TWO FAMILY HOMES"
za$bu_class[za$bu_class=="03  THREE FAMILY DWELLINGS"] <- "03  THREE FAMILY HOMES"

za$NumFloors <- as.numeric(za$NumFloors)
za$yb <- as.numeric(za$YEAR.BUILT >= 1950)
za$ya1 <-  as.numeric(za$YearAlter1 == 0)
za$ya2 <-  as.numeric(za$YearAlter2 == 0)
za$ya1a <-  as.numeric(za$YearAlter1 >= 1990)
za$ya2a <-  as.numeric(za$YearAlter2 >= 1990)
za$m <-  as.factor(month(za$s_date))
pl <-  filter(za3, SALE.PRICE > 10000, SALE.PRICE < 3000000)
pl2 <- filter(pl, bu_class == "01  ONE FAMILY HOMES" | bu_class == "02  TWO FAMILY HOMES" | bu_class == "03  THREE FAMILY HOMES" 
              | bu_class == "09  COOPS - WALKUP APARTMENTS" | bu_class == "10  COOPS - ELEVATOR APARTMENTS" | bu_class == "12  CONDOS - WALKUP APARTMENTS" 
              | bu_class == "13  CONDOS - ELEVATOR APARTMENTS")
pl3 <- filter(pl, bu_class == "01  ONE FAMILY HOMES" | bu_class == "02  TWO FAMILY HOMES" | bu_class == "03  THREE FAMILY HOMES")
summary(lm(log(SALE.PRICE)~Grade,data=pl2))
pl2$Score <-  as.numeric(pl2$Score)
r <- lm(log(SALE.PRICE)~Grade+DBN+Score+math_sco_s+BOROUGH+bu_class+LAND.SQUAR+ResArea,data=pl2)
summary(lm(log(SALE.PRICE)~Grade+DBN+Score+math_sco_s+BOROUGH+bu_class+LAND.SQUAR+ResArea,data=pl2))
summary(lm(log(SALE.PRICE)~Grade+Score+LAND.SQUAR+BOROUGH+bu_class+DBN,data=pl2))
#For just family homes we get an effect
summary(lm(log(SALE.PRICE)~Grade+Score+math_sco_s+LAND.SQUAR+BOROUGH+bu_class+DBN,data=pl3))
summary(lm(log(SALE.PRICE)~Grade+math_sco_s+LAND.SQUAR+BOROUGH+bu_class+DBN,data=pl3))
r <-  lm(log(SALE.PRICE)~Grade_9+Grade_10+Grade_11+Grade_12+Grade_13+DBN+math_sco_s_so+eng_sco_s_av+bu_class+LAND.SQUAR+GROSS.SQUA+NumFloors,data=pl)
r2 <-  lm(log(SALE.PRICE)~Grade_9+Grade_10+Grade_11+Grade_12+Grade_13+DBN+math_sco_s_so+eng_sco_s_av+bu_class+LAND.SQUAR+GROSS.SQUA+NumFloors,data=pl)
