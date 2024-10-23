library(stargazer)
#Regresssion Analysis
za <- readRDS("za_f.rds")

pl <- filter(za,bu_class == "01  ONE FAMILY HOMES", Y == 12, Score_11 > -25)
pl <- filter(za,bu_class == "13  CONDOS - ELEVATOR APARTMENTS", Y == 12, Score_11 > -25)
#Excluding extreme highs
pl <-  filter(pl, SALE.PRICE > 10000, SALE.PRICE < 3000000)
pl <-  filter(pl, SALE.PRICE > 10000, SALE.PRICE < 3000000, BOROUGH == "MN")
pl$yb <- pl$YEAR.BUILT - 1950 
pl2 <- filter(pl, Grade_11 == "A" | Grade_11 == "B" )
pl3 <- filter(pl, Grade_11 == "B" | Grade_11 == "C" )
pl4 <- filter(pl, Grade_11 == "C" | Grade_11 == "D" )
pl5 <- filter(pl, Grade_11 == "D" | Grade_11 == "F" )


r2 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl2)
r3 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl3)
r4 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl4)
r5 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+poly(LAND.SQUAR,3)+poly(GROSS.SQUA,3)+TOTAL.UNIT+poly(yb,3),data=pl5)


stargazer(r2,r3,r4,r5,type="text",column.labels = c("A/B","B/C","C/D","D/F"),style="qje",notes.align = "l")

#for condos getting rid of land
r2 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+TOTAL.UNIT+poly(yb,3),data=pl2)
r3 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+TOTAL.UNIT+poly(yb,3),data=pl3)
r4 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH++TOTAL.UNIT+poly(yb,3),data=pl4)
r5 <- lm(log(SALE.PRICE)~Grade_11+Score_11+math_sco+BOROUGH+TOTAL.UNIT+poly(yb,3),data=pl5)

#well...condos seem to give teh wrong result...but this maybe seems because land is excluded 
stargazer(r2,r3,r4,r5,type="text",column.labels = c("A/B","B/C","C/D","D/F"),style="qje",notes.align = "l")
