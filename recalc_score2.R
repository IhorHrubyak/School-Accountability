
#Score correction2. Just adding on 16-17 and econ disadvantaged data
#Newer data
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Demo")
p <- readRDS("full_na.rds")
#older data with corrected old scores
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
p2 <- select(readRDS("full3.rds"),1:43,72:77)

#2017 data
p_n <- filter(p, Year == 2017)

#Not Econ Disadv and Econ Disadv subgroups. Could also add never ell and current ell
p_g <- filter(p, Category == "Econ Disadv" | Category == "Not Econ Disadv" & Year != 2017)

#rbinding with older data
#Dropping old math and english scores to enable join in p2. Need to recalculate in score_correction
p3 <- rbind(p_n,p_g,p2)

#saving to reuse in score_correction
saveRDS(p3,"full17.rds")


