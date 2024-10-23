#Add test score quintles with f-test add another column 

#Adding the two scores together
#NEW STUFF BEYOND Rough draft
#Test Score quadratic...seems to make a lot more sense in reducing the effect of scores...although the scores are standardized which might make things weird
#Results get weird beyond the third quadratic especially for the raw scores
za2 <- readRDS("za4.rds")
za2 <-  za2[!(is.na(za2$Grade)),]
za2$`Test Score^2` <- za2$`Test Score`^2
za2$`Test Score^3` <- za2$`Test Score`^3

za2$`Test Score Raw^2` <- za2$`Test Score Raw`^2
za2$`Test Score Raw^3` <- za2$`Test Score Raw`^3

#Quartiles and raw scores
za2 <- za2 %>% mutate(quartile = ntile(za2$`Test Score`, 4))
za2$quartile <-  as.factor(za2$quartile)

p <- select(za2,37, 6:30,33)
p2 <- select(za2,37,40, 6:30,33)
p3 <- select(za2,37,40,41, 6:30,33)
p4 <- select(za2,42, 6:30,33)
p5 <- select(za2,37,40,41,42, 6:30,33)


r <- lm(log(p$`Sale Price`) ~ .,data=p)
r2 <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3 <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r4 <- lm(log(p4$`Sale Price`) ~ .,data=p4)
r5 <- lm(log(p5$`Sale Price`) ~ .,data=p5)
r10 <- lm(log(p4$`Sale Price`) ~ .,data=p4,subset=(Y=="10"))
r11 <- lm(log(p4$`Sale Price`) ~ .,data=p4,subset=(Y=="11"))
r12 <- lm(log(p4$`Sale Price`) ~ .,data=p4,subset=(Y=="12"))
r13 <- lm(log(p4$`Sale Price`) ~ .,data=p4,subset=(Y=="13"))
r14 <- lm(log(p4$`Sale Price`) ~ .,data=p4,subset=(Y=="14"))

stargazer(r,r2,r3,r4,r5,type="text",title="Test Scores For All Years",column.labels = c(".1 miles from boundary",".05 miles from boundary",".025 miles from boundary"),keep = c("Test Score","Borough","Percentage Asian","Percentage White","Percentage Black","Percentage Vacant Housing Units","quartile"),style="qje",omit.stat = c("f","ser"),header=FALSE)
stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Test Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Vacant Housing Units","quartile"),style="qje",omit.stat = c("f","ser"),header=FALSE)

#Standardized test scores +5 
#Quartiles and raw scores
za2$`Test Score` <- za2$`Test Score`+6
za2$`Test Score^2` <- za2$`Test Score`^2
za2$`Test Score^3` <- za2$`Test Score`^3

p <- select(za2,36, 6:30,33)
p2 <- select(za2,36,38, 6:30,33)
p3 <- select(za2,36,38,39, 6:30,33)
p4 <- select(za2,42, 6:30,33)

r <- lm(log(p$`Sale Price`) ~ .,data=p)
r2 <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3 <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r4 <- lm(log(p4$`Sale Price`) ~ .,data=p4)
r5 <- lm(log(p5$`Sale Price`) ~ .,data=p5)
r10 <- lm(log(p4$`Sale Price`) ~ .,data=p4,subset=(Y=="10"))
r11 <- lm(log(p4$`Sale Price`) ~ .,data=p4,subset=(Y=="11"))
r12 <- lm(log(p4$`Sale Price`) ~ .,data=p4,subset=(Y=="12"))
r13 <- lm(log(p4$`Sale Price`) ~ .,data=p4,subset=(Y=="13"))
r14 <- lm(log(p4$`Sale Price`) ~ .,data=p4,subset=(Y=="14"))

stargazer(r,r2,r3,r4,type="text",title="Test Scores For All Years",column.labels = c(".1 miles from boundary",".05 miles from boundary",".025 miles from boundary"),keep = c("Test Score","Borough","Percentage Asian","Percentage White","Percentage Black","Percentage Vacant Housing Units","quartile"),style="qje",omit.stat = c("f","ser"),header=FALSE)
stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Test Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Vacant Housing Units","quartile"),style="qje",omit.stat = c("f","ser"),header=FALSE)



#.13 mean .08 median with quartile
p <- select(filter(za2, Distance < .1),42, 6:30,33)
p2 <- select(filter(za2, Distance < .05),42, 6:30,33)
p3 <- select(filter(za2, Distance < .025),42, 6:30,33)


r <- lm(log(p$`Sale Price`) ~ .,data=p)
r2 <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3 <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r10 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))

stargazer(r,r2,r3,type="text",title="BDD Regression Results All Years",column.labels = c(".1 miles from boundary",".05 miles from boundary",".025 miles from boundary"),keep = c("Test Score","Borough","Percentage Asian","Percentage White","Percentage Black","Percentage Vacant Housing Units","quartile"),style="qje",omit.stat = c("f","ser"),header=FALSE)
stargazer(r10,r11,r12,r13,r14, type = "text",title="BDD Regression Results Along each year 0.25 miles from Boundary",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Test Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Vacant Housing Units","quartile"),style="qje",omit.stat = c("f","ser"),header=FALSE)

#Grades
p3 <- select(za2, 2:3,6:30,33,42)
r3 <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r10 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))

stargazer(r3,type="text",title="Test Scores For All Years",column.labels = c("All Years"),keep = c("Grade","quartile"),style="qje",omit.stat = c("f","ser"),header=FALSE)
stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","quartile"),style="qje",omit.stat = c("f","ser"),header=FALSE)

#Grades with Test scores
#one missing value removed in order to do anvoa

p <- select(za2,2:3,34, 6:30,33)
p2 <- select(za2,2:3,34:35, 6:30,33)
p3 <- select(za2,2:3,34:36, 6:30,33)
ra <- lm(log(p$`Sale Price`) ~ .,data=p)
r2a <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3a <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r10a <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11a <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12a <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13a <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14a <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))

stargazer(ra,r2a,r3a,type="text",title="Test Scores For All Years",column.labels = c(".1 miles from boundary",".05 miles from boundary",".025 miles from boundary"),keep = c("Grade","Test Score"),style="qje",omit.stat = c("f","ser"),header=FALSE)
stargazer(r10a,r11a,r12a,r13a,r14a, type = "text",title="Just Test Scores",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","Test Score"),style="qje",omit.stat = c("f","ser"),header=FALSE)

#Of Just test scores and adding grades
p <- select(za2,34, 6:30,33)
p2 <- select(za2,34:35, 6:30,33)
p3 <- select(za2,34:36, 6:30,33)
r <- lm(log(p$`Sale Price`) ~ .,data=p)
r2 <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3 <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r10 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))
stargazer(anova(r3,r3a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test All Years")
stargazer(anova(r10,r10a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test 2009-2010")
stargazer(anova(r11,r11a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test 2010-2011")
stargazer(anova(r12,r12a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test 2011-2012")
stargazer(anova(r13,r13a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test 2012-2013")
stargazer(anova(r14,r14a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test 2013-2014")

anova(r11,r11a)

anova(r12,r12a)
anova(r13,r13a)
anova(r14,r14a)

#Distance
p <- select(filter(za2, Distance < .1),2:3,34:36, 6:30,33)
p2 <- select(filter(za2, Distance < .05),2:3,34:36, 6:30,33)
p3 <- select(filter(za2, Distance < .025),2:3,34:36, 6:30,33)


ra <- lm(log(p$`Sale Price`) ~ .,data=p)
r2a <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3a <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r10a <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11a <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12a <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13a <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14a <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))

stargazer(ra,r2a,r3a,type="text",title="BDD Regression Results Grades and Test Scores All Years",column.labels = c(".1 miles from boundary",".05 miles from boundary",".025 miles from boundary"),keep = c("Grade","Test Score"),style="qje",omit.stat = c("f","ser"),header=FALSE)
stargazer(r10a,r11a,r12a,r13a,r14a, type = "text",title="BDD Regression Results Test Scores Along each year",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","Test Score"),style="qje",omit.stat = c("f","ser"),header=FALSE)

#Anova distance for .025
p <- select(filter(za2, Distance < .1),34:36, 6:30,33)
p2 <- select(filter(za2, Distance < .05),34:36, 6:30,33)
p3 <- select(filter(za2, Distance < .025),34:36, 6:30,33)


r <- lm(log(p$`Sale Price`) ~ .,data=p)
r2 <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3 <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r10 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))

stargazer(anova(r3,r3a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test All Years")
stargazer(anova(r10,r10a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test 2009-2010")
stargazer(anova(r11,r11a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test 2010-2011")
stargazer(anova(r12,r12a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test 2011-2012")
stargazer(anova(r13,r13a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test 2012-2013")
stargazer(anova(r14,r14a),type = "text",omit.summary.stat = c("Min","Max","sd"),title="F-Test 2013-2014")



#MOre Rdd stuff
za2 <- readRDS("za4.rds")
za2$Comb <- (za2$`Math Score`+za2$`English Score`)/2
p3 <- select(za2, 2,3,6:22,24:32)
p3$Comb2 <- p3$Comb^2
p3$Comb3 <- p3$Comb^3
r10 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))

stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","Score","Comb","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"))
#With linear interaction on ab
p3 <- filter(p3, Grade == "A" | Grade == "B" )
p3$Score_int <- (p3$Grade)*p3$Score
r10 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))

stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","Score","Comb","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"))

#with quadratic
p3 <- select(za2, 2,3,6:22,24:32)
p3$Score2 <- p3$Score^2
p3$Comb2 <- p3$Comb^2
p3$Comb3 <- p3$Comb^3
r10 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))
stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Grade","Score","Comb","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"))

#Anova of adding grades to test scores along each year
p3 <- select(za2, 6:22,24:32)
p3$Comb2 <- p3$Comb^2
p3$Comb3 <- p3$Comb^3
r10 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))


p3 <- select(za2, 2,3,6:22,24:32)
p3$Comb2 <- p3$Comb^2
p3$Comb3 <- p3$Comb^3

r102 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r112 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r122 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r132 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r142 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))

#anova along each of the above. All very signifignat except 2 is odd.
anova(r10,r102)
anova(r11,r112)
anova(r12,r122)
anova(r13,r132)
anova(r13,r142)

#On non standardized test scores
za <- readRDS("za3.rds")
za <- select(za, 1,45,46,61,62,71,73,84:85,89,117,120:121,123:128,130,139,141,142,144,147:149,152:154,95,50,52)
names(za)[4:33] <- c("Math Score", "English Score","Borough","Building Class","Square Feet Land", "Gross Square Feet","Sale Price", "Average Household Size",
                     "Median Age", "Average Family Size","Percentage 1 person household","Percentage 2 person household","Percentage 3 person household",
                     "Percentage 4 person household","Percentage 5 person household","Percentage 6 person household","Percentage Vacant Housing Units",
                     "Percentage Family Households","Percentage White","Percentage Black","Percentage Asian","Year Built - 1950",
                     "(Year Built - 1950)^2","(Year Built - 1950)^3","Year Alterted 1 > 1990","Year Alterted 2 > 1980","Month","Y","Raw English Score", "Raw Math Score")
za <- filter(za,`Sale Price` > 10000, `Sale Price` < 3000000, `Building Class` == "01  ONE FAMILY HOMES" | `Building Class` == "02  TWO FAMILY HOMES" | `Building Class` == "03  THREE FAMILY HOMES"
              | `Building Class` == "09  COOPS - WALKUP APARTMENTS"
              | `Building Class` == "10  COOPS - ELEVATOR APARTMENTS" | `Building Class` == "12  CONDOS - WALKUP APARTMENTS" | `Building Class` == "13  CONDOS - ELEVATOR APARTMENTS" 
              | `Building Class` == "15  CONDOS - 2-10 UNIT RESIDENTIAL" |`Building Class` == "04  TAX CLASS 1 CONDOS") 

p <- select(za, 6:22,24:31,33)
p2 <- select(za ,6:22,24:32)

p$`Raw Math Score^2` <- (p$`Raw Math Score`)^2
p$`Raw Math Score^3` <- (p$`Raw Math Score`)^3
p$`Raw Math Score^4` <- (p$`Raw Math Score`)^4
p$`Raw Math Score^5` <- (p$`Raw Math Score`)^5
p$`Raw Math Score^6` <- (p$`Raw Math Score`)^6
r <- lm(log(p$`Sale Price`) ~ .,data=p)
r2 <- lm(log(p$`Sale Price`) ~ .,data=p2)
r10 <- lm(log(p$`Sale Price`) ~ .,data=p,subset=(Y=="10"))
r11 <- lm(log(p$`Sale Price`) ~ .,data=p,subset=(Y=="11"))
r12 <- lm(log(p$`Sale Price`) ~ .,data=p,subset=(Y=="12"))
r13 <- lm(log(p$`Sale Price`) ~ .,data=p,subset=(Y=="13"))
r14 <- lm(log(p$`Sale Price`) ~ .,data=p,subset=(Y=="14"))
stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Math Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"))
#Probs add school type interaction

stargazer(r,r2,type="text",title="Test Scores For All Years",column.labels = c("English","Math"),keep = c("Math Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"),style="qje",omit.stat = c("f","ser"),header=FALSE)
stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Math Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"))
#Probs add school type interaction

#Distance
za <- za2
#Just the mean
za2 <-  filter(za, Distance < .22)
filter()
za2$Comb <- (za2$`Math Score`+za2$`English Score`)/2
p <- select(za2, 4,6:22,24:31)
p2 <- select(za2, 5,6:22,24:31)
p3 <- select(za2, 6:22,24:33)
p$`Math Score^2` <- (p$`Math Score`)^2
p$`Math Score^3` <- (p$`Math Score`)^3
p2$`English Score^2` <- (p2$`English Score`)^2
p2$`English Score^3` <- (p2$`English Score`)^3
p3$Comb2 <- p3$Comb^2
p3$Comb3 <- p3$Comb^3

r <- lm(log(p$`Sale Price`) ~ .,data=p)
r2 <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3 <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r10 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="10"))
r11 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="11"))
r12 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="12"))
r13 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="13"))
r14 <- lm(log(p3$`Sale Price`) ~ .,data=p3,subset=(Y=="14"))

stargazer(r,r2,r3,type="text",title="Test Scores For All Years",column.labels = c("English","Math","Combined"),keep = c("Math Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households","Comb"),style="qje",omit.stat = c("f","ser"),header=FALSE)
stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Comb","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households"))

#Using Combined score for distance bands
za <- za2
za$Comb <- (za$`Math Score`+za$`English Score`)/2
za$Comb2 <- za$Comb^2
za$Comb3 <- za$Comb^3

za3 <-  filter(za, Distance < .22)
za4 <-  filter(za, Distance < .10)
za5 <-  filter(za, Distance < .05)
za6 <-  filter(za, Distance < .025)
za7 <-  filter(za, Distance < .0125)
p2 <- select(za, 6:22,24:36)
p3 <- select(za3, 6:22,24:36)
p4 <- select(za4, 6:22,24:36)
p5 <- select(za5, 6:22,24:36)
p6 <- select(za6, 6:22,24:36)
p7 <- select(za7, 6:22,24:36)

p2 <- select(za2, 6:10,25:30,33)
p3 <- select(za3, 6:10,25:30,33)
p4 <- select(za4, 6:10,25:30,33)
p5 <- select(za5, 6:10,25:30,33)
p6 <- select(za6, 6:10,25:30,33)
r2 <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3 <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r4 <- lm(log(p4$`Sale Price`) ~ .,data=p4)
r5 <- lm(log(p5$`Sale Price`) ~ .,data=p5)
r6 <- lm(log(p6$`Sale Price`) ~ .,data=p6)
r7 <- lm(log(p7$`Sale Price`) ~ .,data=p7)

stargazer(r2,r3,r4,r5,r6,r7,type="text",title="Test Scores For All Years",column.labels = c("English","Math","Combined"),keep = c("Math Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households","Comb"),style="qje",omit.stat = c("f","ser"),header=FALSE)
r10 <- lm(log(p7$`Sale Price`) ~ .,data=p7,subset=(Y=="10"))
r11 <- lm(log(p7$`Sale Price`) ~ .,data=p7,subset=(Y=="11"))
r12 <- lm(log(p7$`Sale Price`) ~ .,data=p7,subset=(Y=="12"))
r13 <- lm(log(p7$`Sale Price`) ~ .,data=p7,subset=(Y=="13"))
r14 <- lm(log(p7$`Sale Price`) ~ .,data=p7,subset=(Y=="14"))

stargazer(r10,r11,r12,r13,r14, type = "text",title="Just Test Scores",style="qje",column.labels = c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014"),keep = c("Comb","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households","Grade"))



#All years again with distance from above
p2 <- select(za, 2,3,6:22,25:30,33:35)
p3 <- select(za3, 2,3,6:22,25:30,33:35)
p4 <- select(za4, 2,3,6:22,25:30,33:35)
p5 <- select(za5, 2,3,6:22,25:30,33:35)
p6 <- select(za6, 2,3,6:22,25:30,33:35)
r2 <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3 <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r4 <- lm(log(p4$`Sale Price`) ~ .,data=p4)
r5 <- lm(log(p5$`Sale Price`) ~ .,data=p5)
r6 <- lm(log(p6$`Sale Price`) ~ .,data=p6)
r7 <- lm(log(p7$`Sale Price`) ~ .,data=p7)


stargazer(r2,r3,r4,r5,r6,r7,type="text",title="Test Scores For All Years",column.labels = c("English","Math","Combined"),keep = c("Math Score","English Score","Borough","Median Age","Percentage Asian","Percentage White","Percentage Family Households","Comb","Grade"),style="qje",omit.stat = c("f","ser"),header=FALSE)


#A/B

p2 <- select(za, 2,3,6:22,25:30,33:35)
p3 <- select(za3, 2,3,6:22,25:30,33:35)
p4 <- select(za4, 2,3,6:22,25:30,33:35)
p5 <- select(za5, 2,3,6:22,25:30,33:35)
p6 <- select(za6, 2,3,6:22,25:30,33:35)
r2 <- lm(log(p2$`Sale Price`) ~ .,data=p2)
r3 <- lm(log(p3$`Sale Price`) ~ .,data=p3)
r4 <- lm(log(p4$`Sale Price`) ~ .,data=p4)
r5 <- lm(log(p5$`Sale Price`) ~ .,data=p5)
r6 <- lm(log(p6$`Sale Price`) ~ .,data=p6)
r7 <- lm(log(p7$`Sale Price`) ~ .,data=p7)


#For map


za2 <- readRDS("za4.rds")
za2 <-  za2[!(is.na(za2$Grade)),]