#Graphs
install.packages("sampleSelection")
library(sampleSelection)
library(dplyr)
library(stargazer)
library(ggplot2)
library(checkmate)
library(htmlTable)
library(grid)
library(gridExtra)
library(cowplot)

#Loading Accountability Data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
#setwd("F:/stuff/NYC Schools/Pluto")

ac <- readRDS("full3.rds")
za <- readRDS("za.rds")
ap <- readRDS("all_prop.rds")
nrow(ap)
ac$Grade = as.numeric((gsub('All Grades', '9', ac$Grade)))

#REMEMBER GRADE CHANGED TO 9 INSTEAD OF ALL STUDENTS HERE AT WORK
#Subsets used

#Will probably need to drop outlier...maybe need to ask
#2010 Property Sales data removing zero sale prices
#Spacing can create weird issues if not careful
za12 <- filter(za, Y == 12) %>% select(bu_class,BOROUGH) 

tab <- filter(za,bu_class == "01  ONE FAMILY HOMES", Y == 12)
cols <- c(75,77,22,24,28,30,32,34,62,97,78)
tab <- tab[,cols]
tab2 <- filter(tab, SALE.PRICE < 10000000)
tab3 <- filter(tab, SALE.PRICE < 5000000)
tab4 <- filter(tab, SALE.PRICE < 3000000)
za$Score_11 <- as.numeric(za$Score_11)
pl <- filter(za,bu_class == "01  ONE FAMILY HOMES", Y == 12, Score_11 > -25)


za12 <- filter(za, Y == 12) %>% select(bu_class,BOROUGH) 
kable(t(as.matrix(za12)))

#This is a big concern
nrow(filter(tab, `Sale Price` == 0 ))
nrow(filter(tab, `Sale Price` <= 10000 ))


pl <- filter(za,bu_class == "01 ONE FAMILY HOMES", Y == 12, Grade_11 != "NA", za$SALE.PRICE > 10000, za$SALE.PRICE < 1000000)
pl$Score_11 <- as.numeric(pl$Score_11)
pl <- filter(za,bu_class == "01  ONE FAMILY HOMES", Y == 12, Score_11 > -25)
pl2 <- filter(pl, SALE.PRICE < 3000000)
pl3 <- filter(pl, SALE.PRICE < 3000000, SALE.PRICE > 0)
pl$l <- log(pl$SALE.PRICE)

za_f <- filter(za,bu_class == "01 ONE FAMILY HOMES")


a <-  ggplot(tab, aes(y = SALE.PRICE, x=BOROUGH)) +
  geom_boxplot() + theme(axis.text.x=element_blank()) +labs(title = "2011-2012 One Family Homes Sales", x = "Bourough", y="Sale Price")
b <- ggplot(tab4, aes(y = SALE.PRICE, x=BOROUGH)) +
  geom_boxplot() + theme(axis.text.x=element_blank()) +labs(title = "2011-2012 One Family Homes Sales Under $3 Milion", x = "Bourough", y="Sale Price")

c <-  ggplot(tab, aes(y = l, x=BOROUGH)) +
  geom_boxplot() + + theme(axis.text.x=element_blank()) +labs(title = "2011-2012 One Family Homes Sales", x = "Bourough", y="Log Sale Price")


plot_grid(a,b,c, ncol = 1, rel_heights = c(2, 2))

#Note this looks at elementary and middle schools. Need to drop nas
ac$Score_7 <- as.numeric(ac$Score_7)
ac$Score_8 <- as.numeric(ac$Score_8)
ac$Score_9 <- as.numeric(ac$Score_9)
ac$Score_10 <- as.numeric(ac$Score_10)
ac$Score_11 <- as.numeric(ac$Score_11)
ac$Score_12 <- as.numeric(ac$Score_12)
ac$Score_13 <- as.numeric(ac$Score_13)

ac_7 <- filter(ac, Year == "2007", Category == "All Students", Grade == 9, Score_7 > -25)
ac_8 <- filter(ac, Year == "2008", Category == "All Students", Grade == 9, Score_8 > -25)
ac_9 <- filter(ac, Year == "2009", Category == "All Students", Grade == 9, Score_9 > -25)
ac_10 <- filter(ac, Year == "2010", Category == "All Students", Grade == 9, Score_10 > -25)
ac_11 <- filter(ac, Year == "2011", Category == "All Students", Grade == 9, Score_11 > -25)
ac_12 <- filter(ac, Year == "2012", Category == "All Students", Grade == 9, Score_12 > -25)
ac_13 <- filter(ac, Year == "2013", Category == "All Students", Grade == 9, Score_13 > -25)

MN <- select(filter(tab, Bourough == "MN"), `Sale Price`)
BX <- select(filter(tab, Bourough == "BX"), `Sale Price`)
SI <- select(filter(tab, Bourough == "SI"), `Sale Price`)
BK <- select(filter(tab, Bourough == "BK"), `Sale Price`)
QN <- select(filter(tab, Bourough == "QN"), `Sale Price`)


#StarGazer plots
stargazer(MN,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "Manhattan Property Statistics")
stargazer(BK,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "Brooklyn Property Statistics")
stargazer(QN,type="html",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "Queens Property Statistics")
stargazer(BX,type="html",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "Brooklyn Property Statistics")
stargazer(SI,type="html",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "Staten Island Property Statistics")


#Accountability plots
a <- ggplot(ac_11, aes(x=Score_11, y=eng_sco)) + geom_point(aes(color = Grade_11))+ labs(title ="2011 English Scores", x = "Overall Score", y="English score") + theme(legend.position = "bottom")+ scale_colour_discrete(name ="Accountability Grade")+ geom_smooth(method = "lm", se = FALSE,color="red") 
b <-  ggplot(ac_11, aes(x=Score_11, y=math_sco)) + geom_point(aes(color = Grade_11))+ labs(title ="2011 Math Scores", x = "Overall Score", y="Math score") + theme(legend.position = "bottom")+ scale_colour_discrete(name ="Accountability Grade")+ geom_smooth(method = "lm", se = FALSE,color="red") 

plot_grid(a,b,ncol = 2, rel_heights = c(2, 2))

#Alternatively if only looking at elementary schools...may also need to add K-8 Schools...
ac_11 <- filter(ac, Year == "2011", Category == "All Students", Grade == 9, Grade_11 != "NA",School.Type == "ELEMENTARY")
ac_11$Score_11 <- as.numeric(ac_11$Score_11)

ggplot(ac_11, aes(x=Score_11, y=eng_sco)) + geom_point(aes(color = Grade_11))+ labs(title ="2011 English Scores", x = "Overall Score", y="English score") + theme(legend.position = "bottom")+ scale_colour_discrete(name ="Accountability Grade")+ geom_smooth(method = "lm", se = FALSE,color="red") 
ggplot(ac_11, aes(x=Score_11, y=math_sco)) + geom_point(aes(color = Grade_11))+ labs(title ="2011 Math Scores", x = "Overall Score", y="Math score") + theme(legend.position = "bottom")+ scale_colour_discrete(name ="Accountability Grade")+ geom_smooth(method = "lm", se = FALSE,color="red") 


#Correlation Matrix 2011 Base year of Grade
#Probably don't really need just mention strong correlation over time and show persistence of grades
ma <- ac_11[,50:79]
ma2 <- select(ma, 2,6,10,14,18,21,26,28,29,30)
a <- rbind(ma2,ac_11$eng_sco)
ma2$Score_13 <- as.numeric(ma2$Score_13)
cor(ma2,ma2,use = "pairwise.complete.obs")



#For Y12 need Grade_11
#NOTE NEED TO FIX TO INCLUDE PREVIOUS YEAR TEST SCORE 
names(tab) <- c("English Test Score","Math Test Score","English Language Learners Percent","Special Education Percent","Asian Percent","Black Percent","Hispanic Percent","White Percent","Accountability Grade","Sale Price", "Bourough")
a <- filter(tab, `Accountability Grade` =="A")
b <- filter(tab, `Accountability Grade` =="B")
c <- filter(tab, `Accountability Grade` =="C")
d <- filter(tab, `Accountability Grade` =="D")
f <- filter(tab, `Accountability Grade` =="F")

stargazer(a,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "A School Descriptive Statistics")
stargazer(b,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "B School Descriptive Statistics")
stargazer(c,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "C School Descriptive Statistics")
stargazer(d,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "D School Descriptive Statistics")
stargazer(f,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "F School Descriptive Statistics")

#By Borough
MN <- filter(tab, Bourough == "MN")
BX <- filter(tab, Bourough == "BX")
SI <- filter(tab, Bourough == "SI")
BK <- filter(tab, Bourough == "BK")
QN <- filter(tab, Bourough == "QN")

stargazer(MN,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "Manhattan School Descriptive Statistics")
stargazer(BK,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "Brooklyn School Descriptive Statistics")
stargazer(QN,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "Queens School Descriptive Statistics")
stargazer(BX,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "Brooklyn School Descriptive Statistics")
stargazer(SI,type="text",header=FALSE,digits=0,notes="Note: The mean and SD are rounded",title= "Staten Island School Descriptive Statistics")


#Naive Regression

stargazer(r3,r4,r5,r6,r7,r8,r9,type="latex",covariate.labels = c("F Grade"),dep.var.labels = "Grades",column.labels = c("3", "4", "5","6", "7", "8","All Grades")
          ,style="qje",dep.var.caption="Math Score",keep.stat=c("n"),notes.align = "l",header=FALSE,omit = c("Constant","Overall_Score",":"), notes="Regression weighed by number of math or English test takers all regressions",omit.table.layout="#",title = "Impact On All Students Math Scores Linear Model")

#
pl <- filter(za,bu_class == "01  ONE FAMILY HOMES", Y == 12, Score_11 > -25)

#Exluding Extreme highs
pl <-  filter(pl, SALE.PRICE > 10000, SALE.PRICE < 3000000)
pl$yb <- pl$YEAR.BUILT - 1950 
pl2 <- filter(pl, Grade_11 == "A" | Grade_11 == "B" )
pl3 <- filter(pl, Grade_11 == "B" | Grade_11 == "C" )
pl4 <- filter(pl, Grade_11 == "C" | Grade_11 == "D" )
pl5 <- filter(pl, Grade_11 == "D" | Grade_11 == "F" )

r1 <- lm(log(pl2$SALE.PRICE)~pl2$Grade_11+pl2$Score_11+pl2$black_per+pl2$white_per+pl2$asian_per+pl2$ell_percent+pl2$BOROUGH+poly(pl2$LAND.SQUAR,3)+poly(pl2$GROSS.SQUA,3)+pl2$TOTAL.UNIT+poly(pl2$yb,3))
r2 <- lm(log(pl3$SALE.PRICE)~pl3$Grade_11+pl3$black_per+pl3$white_per+pl3$asian_per+pl3$ell_percent+pl3$BOROUGH+pl3$LAND.SQUAR+pl3$GROSS.SQUA+pl3$TOTAL.UNIT+poly(pl3$yb,3))
r3 <- lm(log(pl4$SALE.PRICE)~pl4$Grade_11+pl4$black_per+pl4$white_per+pl4$asian_per+pl4$ell_percent+pl4$BOROUGH+pl4$LAND.SQUAR+pl4$GROSS.SQUA+pl4$TOTAL.UNIT+poly(pl4$yb,3))
r4 <- lm(log(pl5$SALE.PRICE)~pl5$Grade_11+pl5$black_per+pl5$white_per+pl5$asian_per+pl5$ell_percent+pl5$BOROUGH+pl5$LAND.SQUAR+pl5$GROSS.SQUA+pl5$TOTAL.UNIT+poly(pl5$yb,3))

#With Test Score controls
r5 <- lm(log(pl2$SALE.PRICE)~pl2$Grade_11+pl2$eng_sco+pl2$black_per+pl2$white_per+pl2$asian_per+pl2$ell_percent+pl2$BOROUGH+poly(pl2$LAND.SQUAR,3)+poly(pl2$GROSS.SQUA,3)+pl2$TOTAL.UNIT+poly(pl2$yb,3))
r6 <- lm(log(pl3$SALE.PRICE)~pl3$Grade_11+pl3$eng_sco+pl3$black_per+pl3$white_per+pl3$asian_per+pl3$ell_percent+pl3$BOROUGH+pl3$LAND.SQUAR+pl3$GROSS.SQUA+pl3$TOTAL.UNIT+poly(pl3$yb,3))
r7 <- lm(log(pl4$SALE.PRICE)~pl4$Grade_11+pl4$eng_sco+pl4$black_per+pl4$white_per+pl4$asian_per+pl4$ell_percent+pl4$BOROUGH+pl4$LAND.SQUAR+pl4$GROSS.SQUA+pl4$TOTAL.UNIT+poly(pl4$yb,3))
r8 <- lm(log(pl5$SALE.PRICE)~pl5$Grade_11+pl5$eng_sco+pl5$black_per+pl5$white_per+pl5$asian_per+pl5$ell_percent+pl5$BOROUGH+pl5$LAND.SQUAR+pl5$GROSS.SQUA+pl5$TOTAL.UNIT+poly(pl5$yb,3))

#With Test Scores
r9 <- lm(log(pl$SALE.PRICE)~pl$eng_sco+pl$black_per+pl$white_per+pl$asian_per+pl$ell_percent+pl$BOROUGH+pl$LAND.SQUAR+pl$GROSS.SQUA+pl$TOTAL.UNIT+poly(pl$yb,3))
r10 <- lm(log(pl$SALE.PRICE)~pl$math_sco+pl$black_per+pl$white_per+pl$asian_per+pl$ell_percent+pl$BOROUGH+pl$LAND.SQUAR+pl$GROSS.SQUA+pl$TOTAL.UNIT+poly(pl$yb,3))

#On Diff property types

pl <- filter(za,bu_class == "22  STORE BUILDINGS", Y == 12, Score_11 > -25)

#Exluding Extreme highs
pl <-  filter(pl, SALE.PRICE > 10000, SALE.PRICE < 3000000)
pl$yb <- pl$YEAR.BUILT - 1950 
pl2 <- filter(pl, Grade_11 == "A" | Grade_11 == "B" )
pl3 <- filter(pl, Grade_11 == "B" | Grade_11 == "C" )
pl4 <- filter(pl, Grade_11 == "C" | Grade_11 == "D" )
pl5 <- filter(pl, Grade_11 == "D" | Grade_11 == "F" )

#On Test Scores
r <- lm(log(SALE.PRICE)~eng_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl2)
r1 <- lm(log(SALE.PRICE)~math_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl3)
stargazer(r,r1,type="text",column.labels = c("English Scores","Math Scores"),style="qje",notes.align = "l")

#Assuming A/B schools on boundary are similar which has been shown so no need for demographic controls
r2 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl2)
r3 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl3)
r4 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl4)
r5 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl5)

stargazer(r2,r3,r4,r5,type="text",column.labels = c("A/B","B/C","C/D","D/F"),style="qje",notes.align = "l")

#Table of Property Types by Bouroughs
pr <- filter(za, bu_class == "01 ONE FAMILY HOMES")

htmlTable(za$bu_class,za$BOROUGH)

za12$bu_class <-  as.character(za12$bu_class) 
za12$BOROUGH <-  as.character(za12$BOROUGH) 

library(plyr)
library(dplyr)

a <- za %>%
  group_by(bu_class) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

kable(a)

#Table of persistnece of accountability grades

a <- round(prop.table(table(ac_11$Grade_12,ac_11$Grade_11),1)*100,2)


htmlTable(a)

za12 <- filter(za, Y == 12) %>% select(bu_class,BOROUGH) 

table(za12$bu_class,za12$BOROUGH)
htmltable(za12$bu_class,za12$BOROUGH)

res <- za12 %>%
  group_by(Grade_7) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

#Number of schools
ac_11$Grade_11 <- as.factor(ac_11$Grade_11)

ac_11 <- filter(ac, Year == "2011", Category == "All Students", Grade == 9, Grade_11 != "NA")

prop.table(table(ac_7$Grade_7))*100
prop.table(table(ac_8$Grade_8))*100
prop.table(table(ac_9$Grade_9))*100
prop.table(table(ac_10$Grade_10))*100
prop.table(table(ac_11$Grade_11))*100
prop.table(table(ac_12$Grade_12))*100
prop.table(table(ac_13$Grade_13))*100



ggplot(tab, aes(x = Bourough, y = `Sale Price`)) +
  geom_boxplot()

a <-  ggplot(tab, aes(y = `Sale Price`, x=Bourough)) +
  geom_boxplot() +labs(title = "2011-2012 One Family Homes Sales", y="Sale Price") 
a


a <- round(prop.table(table(ac_11$Grade_12,ac_11$Grade_11),2)*100,2)
b <- round(prop.table(table(ac_11$Grade_12,ac_11$Grade_13),2)*100,2)

kable(a,caption="2012 Vs 2011 Grades")

kable(b,"html",caption="2012 Vs 2013 Grades")

za_f <- filter(za,bu_class == "01  ONE FAMILY HOMES")




za_f %>% group_by(DBN) %>% summarize(count=n())

za12 <- filter(za, Y == 12) %>% select(bu_class,BOROUGH) 
names(za12)[1] <- "Building Class"



# At previous years accountability grades
pl <- filter(za,bu_class == "01  ONE FAMILY HOMES", Y == 12, Score_10 > -25)

#Exluding Extreme highs
pl <-  filter(pl, SALE.PRICE > 10000, SALE.PRICE < 3000000)
pl$yb <- pl$YEAR.BUILT - 1950 
pl2 <- filter(pl, Grade_10 == "A" | Grade_10 == "B" )
pl3 <- filter(pl, Grade_10 == "B" | Grade_10 == "C" )
pl4 <- filter(pl, Grade_10 == "C" | Grade_10 == "D" )
pl5 <- filter(pl, Grade_10 == "D" | Grade_10 == "F" )

#On Test Scores
r <- lm(log(SALE.PRICE)~eng_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl2)
r1 <- lm(log(SALE.PRICE)~math_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl3)
stargazer(r,r1,type="text",column.labels = c("English Scores","Math Scores"),style="qje",notes.align = "l")

#Assuming A/B schools on boundary are similar which has been shown so no need for demographic controls
r2 <- lm(log(SALE.PRICE)~Grade_10+Score_10+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl2)
r3 <- lm(log(SALE.PRICE)~Grade_10+Score_10+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl3)
r4 <- lm(log(SALE.PRICE)~Grade_10+Score_10+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl4)
r5 <- lm(log(SALE.PRICE)~Grade_10+Score_10+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl5)

stargazer(r2,r3,r4,r5,type="text",column.labels = c("A/B","B/C","C/D","D/F"),style="qje",notes.align = "l")

setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Property")
man <- as.data.frame(read_excel("2012_manhattan.xls",sheet=1, na = "NA", skip=4))
prop.table(table(man$`BUILDING CLASS CATEGORY`))


#Heckman
http://www.polsci.ucsb.edu/faculty/glasgow/ps207/ps207_class6.r
https://www.r-bloggers.com/parallel-simulation-of-heckman-selection-model/
  https://cran.r-project.org/web/packages/sampleSelection/vignettes/selection.pdf