#Loading and selecting necessary subets
library(dplyr)
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
za <- readRDS("za3.rds")
za <- readRDS("zab.rds")
za <- select(za, 1,45,46,61,62,71,73,84:85,89,115,118,119,121:126,128,137,139,140,142,145:147,150:151,152,95,156,93,50,52)
names(za)[4:35] <- c("Math Score", "English Score","Borough","Building Class","Square Feet Land", "Gross Square Feet","Sale Price", "Average Household Size",
                      "Median Age", "Average Family Size","Percentage 1 person household","Percentage 2 person household","Percentage 3 person household",
                      "Percentage 4 person household","Percentage 5 person household","Percentage 6 person household","Percentage Occupied Housing Units",
                      "Percentage Family Households","Percentage White","Percentage Black","Percentage Asian","Year Built - 1950",
                      "(Year Built - 1950)^2","(Year Built - 1950)^3","Year Alterted 1 > 1990","Year Alterted 2 > 1980","Month","CB2010","Distance","Y","Math Score Raw","English Score Raw")
za$`Test Score` <- (za$`Math Score`+za$`English Score`)/2
za$`Test Score Raw` <- (za$`Math Score Raw`+za$`English Score Raw`)/2
za2 <- filter(za,`Sale Price` > 10000, `Sale Price` < 3000000, `Building Class` == "01  ONE FAMILY HOMES" | `Building Class` == "02  TWO FAMILY HOMES" | `Building Class` == "03  THREE FAMILY HOMES"
              | `Building Class` == "09  COOPS - WALKUP APARTMENTS"
              | `Building Class` == "10  COOPS - ELEVATOR APARTMENTS" | `Building Class` == "12  CONDOS - WALKUP APARTMENTS" | `Building Class` == "13  CONDOS - ELEVATOR APARTMENTS" 
              | `Building Class` == "15  CONDOS - 2-10 UNIT RESIDENTIAL" |`Building Class` == "04  TAX CLASS 1 CONDOS") 
#Saving for paper
saveRDS(za2, "za4.rds")

#Saving for map
zam <-  filter(select(za,1,45,153:154,73,89,93,156), Y == 12)
names(zam)[5:8] <- c("Building Class","Sale Price","Y","Miles")

zam <- filter(zam,`Sale Price` > 10000, `Sale Price` < 3000000, `Building Class` == "01  ONE FAMILY HOMES" | `Building Class` == "02  TWO FAMILY HOMES" | `Building Class` == "03  THREE FAMILY HOMES"
       | `Building Class` == "09  COOPS - WALKUP APARTMENTS"
       | `Building Class` == "10  COOPS - ELEVATOR APARTMENTS" | `Building Class` == "12  CONDOS - WALKUP APARTMENTS" | `Building Class` == "13  CONDOS - ELEVATOR APARTMENTS" 
       | `Building Class` == "15  CONDOS - 2-10 UNIT RESIDENTIAL" |`Building Class` == "04  TAX CLASS 1 CONDOS") 
zam2 <- filter(zam, Miles < .025)

write.csv(zam, "zam.csv",row.names = FALSE)
write.csv(zam2, "zam2.csv",row.names = FALSE)

#Just Family Homes
za3 <- filter(za,`Sale Price` > 10000, `Sale Price` < 3000000, `Building Class` == "01  ONE FAMILY HOMES" | `Building Class` == "02  TWO FAMILY HOMES" | `Building Class` == "03  THREE FAMILY HOMES")
#Placebo
za4 <- filter(za,`Sale Price` > 10000, `Sale Price` < 3000000, `Building Class` == "21  OFFICE BUILDINGS" | `Building Class` == "22  STORE BUILDINGS" 
              | `Building Class` == "29  COMMERCIAL GARAGES" | `Building Class` ==  "27  FACTORIES" | `Building Class` == "30  WAREHOUSES" )
saveRDS(za4, "za6.rds")


#Just test score for all years
p <- select(za2, 4,6:22,24:31)
p2 <- select(za2, 5,6:22,24:31)
r <-  lm(log(p$`Sale Price`) ~  .,data=p)
r2 <-  lm(log(p$`Sale Price`) ~  .,data=p2)
r10 <-  lm(log(p$`Sale Price`) ~  .,data=p,subset=(Y=="10"))
r11 <-  lm(log(p$`Sale Price`) ~  .,data=p,subset=(Y=="11"))
r12 <-  lm(log(p$`Sale Price`) ~  .,data=p,subset=(Y=="12"))
r13 <-  lm(log(p$`Sale Price`) ~  .,data=p,subset=(Y=="13"))
r14 <-  lm(log(p$`Sale Price`) ~  .,data=p,subset=(Y=="14"))



stargazer(r,r2, type = "text",title="Just Test Scores",style="qje",column.labels = c("English","Math"),keep = c("Math Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"))
stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Math Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"))

#For appendix
stargazer(r,r2, type = "text",title="Just Test Scores",style="qje",column.labels = c("English","Math"))
stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"))

#Fixed effect  for all years...this makes no sense
za2$Comb <- (za2$`Math Score`+za2$`English Score`)/2
p <- select(za2, 1,2,3,6,7,10,25,26,32)
p$Comb2 <- p$Comb^2
p$Comb3 <- p$Comb^3
r <-  lm(log(`Sale Price`) ~  .,data=p)
stargazer(r, type = "text",title="Just Test Scores",style="qje",keep = c("Grade","Comb"))

#
stargazer(r, type = "text",title="Just Test Scores",style="qje",column.labels = c("English","Math"))

#Just Grade
pl <- select(za2, 2,3,6:22,24:31)
reg10 <- lm(log(`Sale Price`) ~  .,data=pl,subset=(Y=="10"))
reg11 <- lm(log(`Sale Price`) ~  .,data=pl,subset=(Y=="11"))
reg12 <- lm(log(`Sale Price`) ~  .,data=pl,subset=(Y=="12"))
reg13 <- lm(log(`Sale Price`) ~  .,data=pl,subset=(Y=="13"))
reg14 <- lm(log(`Sale Price`) ~  .,data=pl,subset=(Y=="14"))
stargazer(reg10,reg11,reg12,reg13,reg14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade"))



#Grade and Test Score
  pl10 <- select(za2, 12:13,31,32:48,50:57)
  pl11 <- select(za2, 16:17,31,32:48,50:57)
  pl12 <- select(za2, 20:21,31,32:48,50:57)
  pl13 <- select(za2, 24:25,31,32:48,50:57)
  pl14 <- select(za2, 27:28,31,32:48,50:57)
  reg10 <- lm(log(pl10$`Sale Price`) ~  .,data=pl10,subset=(Y=="10"))
  reg11 <- lm(log(pl11$`Sale Price`) ~  .,data=pl11,subset=(Y=="11"))
  reg12 <- lm(log(pl12$`Sale Price`) ~  .,data=pl12,subset=(Y=="12"))
  reg13 <- lm(log(pl13$`Sale Price`) ~  .,data=pl13,subset=(Y=="13"))
  reg14 <- lm(log(pl14$`Sale Price`) ~  .,data=pl14,subset=(Y=="14"))
  stargazer(reg10,reg11,reg12,reg13,reg14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","English"))

#Just A/B Reg
pl <- select(za2, 2,3,6:22,24:31)
pl3 <- filter(pl, Grade == "A" | Grade == "B" )
pl4 <- filter(pl, Grade == "B" | Grade == "C" )
pl5 <- filter(pl, Grade == "C" | Grade == "D" )
pl6 <- filter(pl, Grade == "F" | Grade == "D" )
reg10 <- lm(log(`Sale Price`) ~  .,data=pl3,subset=(Y=="10"))
reg11 <- lm(log(`Sale Price`) ~  .,data=pl3,subset=(Y=="11"))
reg12 <- lm(log(`Sale Price`) ~  .,data=pl3,subset=(Y=="12"))
reg13 <- lm(log(`Sale Price`) ~  .,data=pl3,subset=(Y=="13"))
reg14 <- lm(log(`Sale Price`) ~  .,data=pl3,subset=(Y=="14"))

stargazer(reg10,reg11,reg12,reg13,reg14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","English"))
  
#B/C
reg10 <- lm(log(`Sale Price`) ~  .,data=pl4,subset=(Y=="10"))
reg11 <- lm(log(`Sale Price`) ~  .,data=pl4,subset=(Y=="11"))
reg12 <- lm(log(`Sale Price`) ~  .,data=pl4,subset=(Y=="12"))
reg13 <- lm(log(`Sale Price`) ~  .,data=pl4,subset=(Y=="13"))
reg14 <- lm(log(`Sale Price`) ~  .,data=pl4,subset=(Y=="14"))
stargazer(reg10,reg11,reg12,reg13,reg14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","English"))
#c/d
reg10 <- lm(log(`Sale Price`) ~  .,data=pl5,subset=(Y=="10"))
reg11 <- lm(log(`Sale Price`) ~  .,data=pl5,subset=(Y=="11"))
reg12 <- lm(log(`Sale Price`) ~  .,data=pl5,subset=(Y=="12"))
reg13 <- lm(log(`Sale Price`) ~  .,data=pl5,subset=(Y=="13"))
reg14 <- lm(log(`Sale Price`) ~  .,data=pl5,subset=(Y=="14"))

stargazer(reg10,reg11,reg12,reg13,reg14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","English"))

#d/F
reg10 <- lm(log(`Sale Price`) ~  .,data=pl6,subset=(Y=="10"))
reg11 <- lm(log(`Sale Price`) ~  .,data=pl6,subset=(Y=="11"))
reg12 <- lm(log(`Sale Price`) ~  .,data=pl6,subset=(Y=="12"))
reg13 <- lm(log(`Sale Price`) ~  .,data=pl6,subset=(Y=="13"))
reg14 <- lm(log(`Sale Price`) ~  .,data=pl6,subset=(Y=="14"))


stargazer(reg11,reg12,reg13,reg14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","English"))

#Grade from Previous Year and no test score...need to get old test score I think
pl10 <- select(za2, 8:9,32:48,50:57)
pl11 <- select(za2, 12:13,32:48,50:57)
pl12 <- select(za2, 16:17,32:48,50:57)
pl13 <- select(za2, 20:21,32:48,50:57)
pl14 <- select(za2, 24:25,32:48,50:57)
reg10 <- lm(log(pl10$`Sale Price`) ~  .,data=pl10,subset=(Y=="10"))
reg11 <- lm(log(pl11$`Sale Price`) ~  .,data=pl11,subset=(Y=="11"))
reg12 <- lm(log(pl12$`Sale Price`) ~  .,data=pl12,subset=(Y=="12"))
reg13 <- lm(log(pl13$`Sale Price`) ~  .,data=pl13,subset=(Y=="13"))
reg14 <- lm(log(pl14$`Sale Price`) ~  .,data=pl14,subset=(Y=="14"))
stargazer(reg10,reg11,reg12,reg13,reg14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","English"))




#For Appendix 
#excluding Score
pl10 <- select(za2, 12,32:48,50:57)
pl11 <- select(za2, 16,32:48,50:57)
pl12 <- select(za2, 20,32:48,50:57)
pl13 <- select(za2, 24,32:48,50:57)
pl14 <- select(za2, 27,32:48,50:57)
reg10 <- lm(log(pl10$`Sale Price`) ~  .,data=pl10,subset=(Y=="10"))
reg11 <- lm(log(pl11$`Sale Price`) ~  .,data=pl11,subset=(Y=="11"))
reg12 <- lm(log(pl12$`Sale Price`) ~  .,data=pl12,subset=(Y=="12"))
reg13 <- lm(log(pl13$`Sale Price`) ~  .,data=pl13,subset=(Y=="13"))
reg14 <- lm(log(pl14$`Sale Price`) ~  .,data=pl14,subset=(Y=="14"))
stargazer(reg10,reg11,reg12,reg13,reg14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade"))

#Grade and Test Score no other score
pl <- select(za2, 2,5,6:22,24:31)
reg10 <- lm(log(`Sale Price`) ~  .,data=pl,subset=(Y=="10"))
reg11 <- lm(log(`Sale Price`) ~  .,data=pl,subset=(Y=="11"))
reg12 <- lm(log(`Sale Price`) ~  .,data=pl,subset=(Y=="12"))
reg13 <- lm(log(`Sale Price`) ~  .,data=pl,subset=(Y=="13"))
reg14 <- lm(log(`Sale Price`) ~  .,data=pl,subset=(Y=="14"))
stargazer(reg10,reg11,reg12,reg13,reg14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","English"))

#NEW STUFF BEYOND PAPER
#Test Score quadratic
za2 <- readRDS("za4.rds")
p <- select(za2, 4,6:22,24:31)
p2 <- select(za2, 5,6:22,24:31)
p$`Math Score^2` <- (p$`Math Score`)^2
p$`Math Score^3` <- (p$`Math Score`)^3
r <- lm(log(p$`Sale Price`) ~ .,data=p)
r2 <- lm(log(p$`Sale Price`) ~ .,data=p2)
r10 <- lm(log(p$`Sale Price`) ~ .,data=p,subset=(Y=="10"))
r11 <- lm(log(p$`Sale Price`) ~ .,data=p,subset=(Y=="11"))
r12 <- lm(log(p$`Sale Price`) ~ .,data=p,subset=(Y=="12"))
r13 <- lm(log(p$`Sale Price`) ~ .,data=p,subset=(Y=="13"))
r14 <- lm(log(p$`Sale Price`) ~ .,data=p,subset=(Y=="14"))

stargazer(r,r2,type="text",title="Test Scores For All Years",column.labels = c("English","Math"),keep = c("Math Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"),style="qje",omit.stat = c("f","ser"),header=FALSE)


#Probs add school type interaction
