library(dplyr)
setwd("C:/Users/maxim/Documents/R/Old Computer Code/Schools2/rdata")

# Load dataset
ac <- readRDS("full2.rds")

#After 2012 All students calculated. Any grade or group with less than 5 students reports no test score so they're removed
p <- filter(ac,math_n > 5,eng_n > 5,Year < 2012)
p <- arrange(p, DBN, Year,Category, Grade)

p$math_n1 = 0
p$eng_n1 = 0
for (i in 2:nrow(p)) {
  if (p$Year[i] == p$Year[i-1] && p$Category[i] == p$Category[i-1]) {
    p$math_n1[i] = p$math_n[i-1] + p$math_n1[i-1] 
    p$eng_n1[i] = p$eng_n[i-1] + p$eng_n1[i-1] 
  } else {
    p$math_n1[i] = 0
    p$eng_n1[i] = 0
  } 
}

for (i in 1:nrow(p)) { 
  if (p$Grade[i] == "All Grades") {
    p$math_n[i] =p$math_n1[i] 
    p$eng_n[i] = p$eng_n1[i] 
  }
}


#Average scores by year calculated
p <- mutate(p,all_m= p$math_n * p$math_sco, all_e= p$eng_n * p$eng_sco)
p$av_diff_e = NA
p$av_diff_e[1] = 0
p$av_diff_m = NA
p$av_diff_m[1] = 0
for (i in 2:nrow(p)) {
  if (p$Year[i] == p$Year[i-1] && p$Category[i] == p$Category[i-1]) {
    p$av_diff_e[i] = p$all_e[i-1] + p$av_diff_e[i-1] 
  } else {
    p$av_diff_e[i] = 0
  }
}

for (i in 2:nrow(p)) {
  if (p$Year[i] == p$Year[i-1] && p$Category[i] == p$Category[i-1]) {
    p$av_diff_m[i] = p$all_m[i-1] + p$av_diff_m[i-1] 
  } else {
    p$av_diff_m[i] = 0
  }
}

p <- mutate(p, avg_all_m = av_diff_m / math_n)
p <- mutate(p, avg_all_e = av_diff_e / eng_n)

for (i in 1:nrow(p)) {
  if (is.na(p$math_sco[i]) == TRUE && p$Grade[i] == "All Grades") {
    p$math_sco[i] = p$avg_all_m[i]
  }
}

for (i in 1:nrow(p)) {
  if (is.na(p$eng_sco[i]) == TRUE && p$Grade[i] == "All Grades") {
    p$eng_sco[i] = p$avg_all_e[i]
  }
}
p$math_sco <- round(p$math_sco)
p$eng_sco <- round(p$eng_sco)

#Dropping uneccesary varibales created above and joining with newer data
p2 <- p
p3 <- p2[,1:77]

p4 <- filter(ac,math_n > 5,eng_n > 5,Year >= 2012)
p <- rbind(p3,p4)


#Creating variblae for scores in previous years

p <- arrange(p, DBN,Category, Grade, Year)
p$eng_sco_o=NA
p$eng_sco_o2=NA
p$eng_sco_o3=NA

#Already ordered by year so that identifer not necessary
for (i in 2:nrow(p)) {
  if (p$Grade[i] == p$Grade[i-1] && p$DBN[i] == p$DBN[i-1] && p$Category[i] == p$Category[i-1] ){
    p$eng_sco_o[i] = p$eng_sco[i-1]
  } else 
    p$eng_sco_o[i] = NA
  
}

for (i in 3:nrow(p)) {
  if (p$Grade[i] == p$Grade[i-2] && p$DBN[i] == p$DBN[i-2] && p$Category[i] == p$Category[i-2] ){
    p$eng_sco_o2[i] = p$eng_sco[i-2]
  } else 
    p$eng_sco_o2[i] = NA
  
}

for (i in 4:nrow(p)) {
  if (p$Grade[i] == p$Grade[i-3] && p$DBN[i] == p$DBN[i-3] && p$Category[i] == p$Category[i-3] ){
    p$eng_sco_o3[i] = p$eng_sco[i-3]
  } else 
    p$eng_sco_o3[i] = NA
  
}

p$math_sco_o=NA
p$math_sco_o2=NA
p$math_sco_o3=NA
for (i in 2:nrow(p)) {
  if (p$Grade[i] == p$Grade[i-1] && p$DBN[i] == p$DBN[i-1] && p$Category[i] == p$Category[i-1] ){
    p$math_sco_o[i] = p$math_sco[i-1]
  } else 
    p$math_sco_o[i] = NA
}

for (i in 3:nrow(p)) {
  if (p$Grade[i] == p$Grade[i-2] && p$DBN[i] == p$DBN[i-2] && p$Category[i] == p$Category[i-2] ){
    p$math_sco_o2[i] = p$math_sco[i-2]
  } else 
    p$math_sco_o2[i] = NA
  
}

for (i in 4:nrow(p)) {
  if (p$Grade[i] == p$Grade[i-3] && p$DBN[i] == p$DBN[i-3] && p$Category[i] == p$Category[i-3] ){
    p$math_sco_o3[i] = p$math_sco[i-3]
  } else 
    p$math_sco_o3[i] = NA
  
}

#Note scale of scores changes in 2013 so the average score will be meaningless for later years need to correct for this
#Calculate Average Score from current year and two previous years

#Some of the avearge don't make sense though, if possible need a way to exclude nas and deal with score maybe standaridze scores by years group etc
p <- mutate(p, math_sco_av = (math_sco+math_sco_o+math_sco_o2)/3)
p <- mutate(p, eng_sco_av = (eng_sco+eng_sco_o+eng_sco_o2)/3)

#Saving
prop_f$bu_class[prop_f$bu_class=="01 ONE FAMILY DWELLINGS"] <- "01 ONE FAMILY HOMES"
p$Grade[p$Grade == "All Grades"] <- "9"
p$Grade <- as.numeric(p$Grade)
saveRDS(p, "full3.rds")

p <- readRDS("full17.rds")
#Standardizing test scores by category...maybe a pain as it has to be done by year and category
#Can maybe can create separate datasets by category then have a loop over each of the years to create a standardized score

#Or can just standardize for all students for now can do more if needed. Can also redo above for 
p_a6 <- unique(filter(p, Category =="All Students", Year == "2006", Grade == 9))
p_a7 <- unique(filter(p, Category =="All Students", Year == "2007", Grade == 9))
p_a8 <- unique(filter(p, Category =="All Students", Year == "2008", Grade == 9))
p_a9 <- unique(filter(p, Category =="All Students", Year == "2009", Grade == 9))
p_a10 <- unique(filter(p, Category =="All Students", Year == "2010", Grade == 9))
p_a11 <- unique(filter(p, Category =="All Students", Year == "2011", Grade == 9))
p_a12 <- unique(filter(p, Category =="All Students", Year == "2012", Grade == 9))
p_a13 <- unique(filter(p, Category =="All Students", Year == "2013", Grade == 9))
p_a14 <- unique(filter(p, Category =="All Students", Year == "2014", Grade == 9))
p_a15 <- unique(filter(p, Category =="All Students", Year == "2015", Grade == 9))
p_a16 <- unique(filter(p, Category =="All Students", Year == "2016", Grade == 9))

p_a17 <- filter(p, Category =="All Students", Year == "2017", Grade == "All Grades")
p_a17$Grade <-  ifelse(p_a17$Grade == "All Grades", 9, NA)

#Standardizing Math
p_a6$math_sco_s <- as.numeric(scale(p_a6$math_sco))
p_a7$math_sco_s <- as.numeric(scale(p_a7$math_sco))
p_a8$math_sco_s <- as.numeric(scale(p_a8$math_sco))
p_a9$math_sco_s <- as.numeric(scale(p_a9$math_sco))
p_a10$math_sco_s <- as.numeric(scale(p_a10$math_sco))
p_a11$math_sco_s <- as.numeric(scale(p_a11$math_sco))
p_a12$math_sco_s <- as.numeric(scale(p_a12$math_sco))
p_a13$math_sco_s <- as.numeric(scale(p_a13$math_sco))
p_a14$math_sco_s <- as.numeric(scale(p_a14$math_sco))
p_a15$math_sco_s <- as.numeric(scale(p_a15$math_sco))
p_a16$math_sco_s <- as.numeric(scale(p_a16$math_sco))
p_a17$math_sco_s <- as.numeric(scale(p_a17$math_sco))

#Standardizing English
p_a6$eng_sco_s <- as.numeric(scale(p_a6$eng_sco))
p_a7$eng_sco_s <- as.numeric(scale(p_a7$eng_sco))
p_a8$eng_sco_s <- as.numeric(scale(p_a8$eng_sco))
p_a9$eng_sco_s <- as.numeric(scale(p_a9$eng_sco))
p_a10$eng_sco_s <- as.numeric(scale(p_a10$eng_sco))
p_a11$eng_sco_s <- as.numeric(scale(p_a11$eng_sco))
p_a12$eng_sco_s <- as.numeric(scale(p_a12$eng_sco))
p_a13$eng_sco_s <- as.numeric(scale(p_a13$eng_sco))
p_a14$eng_sco_s <- as.numeric(scale(p_a14$eng_sco))
p_a15$eng_sco_s <- as.numeric(scale(p_a15$eng_sco))
p_a16$eng_sco_s <- as.numeric(scale(p_a16$eng_sco))
p_a17$eng_sco_s <- as.numeric(scale(p_a17$eng_sco))

#Combining the datasets into one again
p_all <- rbind(p_a6,p_a7,p_a8,p_a9,p_a10,p_a11,p_a12,p_a13,p_a14,p_a15,p_a16,p_a17)

#Creating old test scores and averages for the above
p_all <- arrange(p_all, DBN, Grade, Year)
p_all$eng_sco_so=NA
p_all$eng_sco_so2=NA
p_all$eng_sco_so3=NA

for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1]){
    p_all$eng_sco_so[i] = p_all$eng_sco_s[i-1]
  } else 
    p_all$eng_sco_so[i] = NA
  
}

for (i in 3:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-2] && p_all$DBN[i] == p_all$DBN[i-2] && p_all$Category[i] == p_all$Category[i-2] ){
    p_all$eng_sco_so2[i] = p_all$eng_sco_s[i-2]
  } else 
    p_all$eng_sco_so2[i] = NA
  
}

for (i in 4:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-3] && p_all$DBN[i] == p_all$DBN[i-3] && p_all$Category[i] == p_all$Category[i-3] ){
    p_all$eng_sco_so3[i] = p_all$eng_sco_s[i-3]
  } else 
    p_all$eng_sco_so3[i] = NA
  
}

p_all$math_sco_s_so=NA
p_all$math_sco_s_so2=NA
p_all$math_sco_s_so3=NA
for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1] && p_all$Category[i] == p_all$Category[i-1] ){
    p_all$math_sco_s_so[i] = p_all$math_sco_s[i-1]
  } else 
    p_all$math_sco_s_so[i] = NA
}

for (i in 3:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-2] && p_all$DBN[i] == p_all$DBN[i-2] && p_all$Category[i] == p_all$Category[i-2] ){
    p_all$math_sco_s_so2[i] = p_all$math_sco_s[i-2]
  } else 
    p_all$math_sco_s_so2[i] = NA
  
}

for (i in 4:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-3] && p_all$DBN[i] == p_all$DBN[i-3] && p_all$Category[i] == p_all$Category[i-3] ){
    p_all$math_sco_s_so3[i] = p_all$math_sco_s[i-3]
  } else 
    p_all$math_sco_s_so3[i] = NA
  
}

#Average scores
p_all <- mutate(p_all, math_sco_s_av = (math_sco_s+math_sco_s_so+math_sco_s_so2)/3)
p_all <- mutate(p_all, eng_sco_s_av = (eng_sco_s+eng_sco_so+eng_sco_so2)/3)

#Next year's scores added too
#p_all <- readRDS("p_all.rds")

p_all$math_sco_s_n=NA
p_all$math_sco_s_n2=NA
for (i in 1:nrow(p_all)-1) {
  if (p_all$Grade[i] == p_all$Grade[i+1] && p_all$DBN[i] == p_all$DBN[i+1]){
    p_all$math_sco_s_n[i] = p_all$math_sco_s[i+1]
  } else 
    p_all$math_sco_s_n[i] = NA
}

for (i in 1:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i+2] && p_all$DBN[i] == p_all$DBN[i+2] && p_all$Category[i] == p_all$Category[i+2] ){
    p_all$math_sco_s_n2[i] = p_all$math_sco_s[i+2]
  } else 
    p_all$math_sco_s_n2[i] = NA
  
}

p_all$eng_sco_n=NA
p_all$eng_sco_n2=NA

for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i+1] && p_all$DBN[i] == p_all$DBN[i+1]){
    p_all$eng_sco_n[i] = p_all$eng_sco_s[i+1]
  } else 
    p_all$eng_sco_n[i] = NA
  
}

for (i in 3:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i+2] && p_all$DBN[i] == p_all$DBN[i+2] && p_all$Category[i] == p_all$Category[i+2] ){
    p_all$eng_sco_n2[i] = p_all$eng_sco_s[i+2]
  } else 
    p_all$eng_sco_n2[i] = NA
  
}

p_all$total_enrollment_o=NA
p_all$total_enrollment_n=NA

for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1]){
    p_all$total_enrollment_o[i] = p_all$total_enrollment[i-1]
  } else 
    p_all$total_enrollment_o[i] = NA
  
}

for (i in 1:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i+1] && p_all$DBN[i] == p_all$DBN[i+1]){
    p_all$total_enrollment_n[i] = p_all$total_enrollment[i+1]
  } else 
    p_all$total_enrollment_n[i] = NA
  
}


p_all$sped_percent_o=NA
p_all$black_per_o=NA
p_all$white_per_o=NA
p_all$sped_percent_o=NA
p_all$pov_per_o=NA
p_all$hispanic_per_o=NA
p_all$asian_per_o=NA
p_all$ell_percent_o=NA
for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1]){
    p_all$sped_percent_o[i] = p_all$sped_percent[i-1]
  } else 
    p_all$sped_percent_o[i] = NA
  
}
for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1]){
    p_all$black_per_o[i] = p_all$black_per[i-1]
  } else 
    p_all$black_per_o[i] = NA
  
}
for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1]){
    p_all$white_per_o[i] = p_all$white_per[i-1]
  } else 
    p_all$white_per_o[i] = NA
  
}
for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1]){
    p_all$sped_percent_o[i] = p_all$sped_percent[i-1]
  } else 
    p_all$sped_percent_o[i] = NA
  
}
for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1]){
    p_all$pov_per_o[i] = p_all$pov_per[i-1]
  } else 
    p_all$pov_per_o[i] = NA
  
}
for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1]){
    p_all$hispanic_per_o[i] = p_all$hispanic_per[i-1]
  } else 
    p_all$hispanic_per_o[i] = NA
  
}
for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1]){
    p_all$asian_per_o[i] = p_all$asian_per[i-1]
  } else 
    p_all$asian_per_o[i] = NA
  
}
for (i in 2:nrow(p_all)) {
  if (p_all$Grade[i] == p_all$Grade[i-1] && p_all$DBN[i] == p_all$DBN[i-1]){
    p_all$ell_percent_o[i] = p_all$ell_percent[i-1]
  } else 
    p_all$ell_percent_o[i] = NA
  
}
#next and previous year's enrolment


saveRDS(p_all, "p_all2.rds")
