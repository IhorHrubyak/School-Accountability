#############################################
TRANSFORM
#############################################

#Maybe make the below into a function to transform necessary datasets
#Joing Math and English into one dataset function. Whichever specific data used for subsequent analysis decided here

#Joining All the datasets to each approiate one by test
pa <- inner_join(pe,pm, by=c("DBN","Grade","Year","Category"))
pl <- inner_join(pe_ell,pm_ell, by=c("DBN","Grade","Year","Category"))
peth <- inner_join(pe_eth,pm_eth, by=c("DBN","Grade","Year","Category"))
pg <- inner_join(pe_g,pm_g, by=c("DBN","Grade","Year","Category"))
ps <- inner_join(pe_swd,pm_swd, by=c("DBN","Grade","Year","Category"))

#Combining all of them togheter and Rows with less than 5 test takers are not reported so removed for convience. Unnecceary years also removed
p <- filter(rbind(pa,pl,peth,pg,ps),math_n > 5,eng_n > 5,Year == "2006"|Year == "2007"|Year == "2008")
p$Year=as.numeric(as.character(p$Year))

#The below loops can a while and are likely inefficnet filtering by Catgeory subgroups can quicken the proccess if only interested in certain subgroups
#Correcting for number of test takers after above removal
p <- arrange(p, DBN, Year,Category, Grade)
p$math_n1 = 0
p$eng_n1 = 0
for (i in 2:nrow(p)) {
  if (p$Year[i] == p$Year[i-1] && p$Category[i] == p$Category[i-1]) {
    p$math_n1[i] = p$math_n[i-1] + p$math_n1[i-1] 
    p$eng_n1[i] = p$eng_n[i-1] +   p$eng_n1[i-1] 
  } else  {
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
p <-  mutate(p,all_m= p$math_n * p$math_sco, all_e= p$eng_n * p$eng_sco)
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


p <- arrange(p, DBN,Category, Grade, Year)
p$eng_sco_o=NA
p$math_sco_o=NA
for (i in 2:nrow(p)) {
  if (p$Grade[i] == p$Grade[i-1] && p$Year[i] == p$Year[i-1]+1 && p$DBN[i] == p$DBN[i-1] && p$Category[i] == p$Category[i-1] ){
    p$eng_sco_o[i] =  p$eng_sco[i-1]
  } else 
    p$eng_sco_o[i] = NA
  
}

for (i in 2:nrow(p)) {
  if (p$Grade[i] == p$Grade[i-1] && p$Year[i] == p$Year[i-1]+1 && p$DBN[i] == p$DBN[i-1] && p$Category[i] == p$Category[i-1] ){
    p$math_sco_o[i] =  p$math_sco[i-1]
  } else 
    p$math_sco_o[i] = NA
}

p <- arrange(p,DBN, Grade, Year)

for (i in 2:nrow(p)) {
  if (p$Grade[i] == p$Grade[i-1] && p$Year[i] == p$Year[i-1]+1 && p$DBN[i] == p$DBN[i-1]){
    p$eng_sco_o[i] =  p$eng_sco[i-1]
  } else 
    p$eng_sco_o[i] = NA
}

for (i in 2:nrow(p)) {
  if (p$Grade[i] == p$Grade[i-1] && p$Year[i] == p$Year[i-1]+1 && p$DBN[i] == p$DBN[i-1]){
    p$math_sco_o[i] =  p$math_sco[i-1]
  } else 
    p$math_sco_o[i] = NA
}

p$Year <- as.factor(p$Year)

#Dropping uneccesary varibales created above
p <- p[,1:8]



p$Year <- as.factor(p$Year)

#Joining with demographic and accountability data
prd <- inner_join(p,de, by=c("DBN","Year"))
prde <- inner_join(prd,prog, by=c("DBN"))

#Further Data Corrections making All Grades = 9 to ease analysis
prde$Grade = as.numeric((gsub('All Grades', '9', prde$Grade)))


#Writing as csv for export
write.csv(prde,"prde.csv",sep=",",row.names=FALSE)



