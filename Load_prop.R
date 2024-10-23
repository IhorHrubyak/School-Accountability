setwd("C:/Users/Sarah Blair/Documents/R/Schools2/Property")
list.files()

bronx <- list(1)
man <- list(1)
queen <- list(1)
si <- list(1)
broo <- list(1)
#2014=skip 4
#Could make the below into a function to make it smaller so just done once for each borough
#Maybe could add a unique year column to each of this if needed to filter by year easier
for(i in 3:16){ 
  if (i < 7 ) {
    bronx[[i]] <- as.data.frame(read_excel(paste("sales_bronx_0", as.character(i), ".xls", sep=""),sheet=1, na = "NA", skip=3))
    man[[i]] <- as.data.frame(read_excel(paste("sales_manhattan_0", as.character(i), ".xls", sep=""),sheet=1, na = "NA", skip=3))
    queen[[i]] <- as.data.frame(read_excel(paste("sales_queens_0", as.character(i), ".xls", sep=""),sheet=1, na = "NA", skip=3))
    si[[i]] <- as.data.frame(read_excel(paste("sales_si_0", as.character(i), ".xls", sep=""),sheet=1, na = "NA", skip=3))
    broo[[i]] <- as.data.frame(read_excel(paste("sales_brooklyn_0", as.character(i), ".xls", sep=""),sheet=1, na = "NA", skip=3))
  } 
  else if ( i < 9 ) {
    bronx[[i]] <- as.data.frame(read_excel(paste("sales_200", as.character(i), "_bronx.xls", sep=""),sheet=1, na = "NA", skip=3))
    man[[i]] <- as.data.frame(read_excel(paste("sales_200", as.character(i), "_manhattan.xls", sep=""),sheet=1, na = "NA", skip=3))
    queen[[i]] <- as.data.frame(read_excel(paste("sales_200", as.character(i), "_queens.xls", sep=""),sheet=1, na = "NA", skip=3))
    si[[i]] <- as.data.frame(read_excel(paste("sales_200", as.character(i), "_statenisland.xls", sep=""),sheet=1, na = "NA", skip=3))
    broo[[i]] <- as.data.frame(read_excel(paste("sales_200", as.character(i), "_brooklyn.xls", sep=""),sheet=1, na = "NA", skip=3))
  } else if ( i == 9 ) {
    bronx[[i]] <- as.data.frame(read_excel(paste("200", as.character(i), "_bronx.xls", sep=""),sheet=1, na = "NA", skip=3))
    man[[i]] <- as.data.frame(read_excel(paste("200", as.character(i), "_manhattan.xls", sep=""),sheet=1, na = "NA", skip=3))
    queen[[i]] <- as.data.frame(read_excel(paste("200", as.character(i), "_queens.xls", sep=""),sheet=1, na = "NA", skip=3))
    si[[i]] <- as.data.frame(read_excel(paste("200", as.character(i), "_statenisland.xls", sep=""),sheet=1, na = "NA", skip=3))
    broo[[i]] <- as.data.frame(read_excel(paste("200", as.character(i), "_brooklyn.xls", sep=""),sheet=1, na = "NA", skip=3)) 
  } else if ( i == 10 ) {
    bronx[[i]] <- as.data.frame(read_excel(paste("20", as.character(i), "_bronx.xls", sep=""),sheet=1, na = "NA", skip=3))
    man[[i]] <- as.data.frame(read_excel(paste("20", as.character(i), "_manhattan.xls", sep=""),sheet=1, na = "NA", skip=3))
    queen[[i]] <- as.data.frame(read_excel(paste("20", as.character(i), "_queens.xls", sep=""),sheet=1, na = "NA", skip=3))
    si[[i]] <- as.data.frame(read_excel(paste("20", as.character(i), "_statenisland.xls", sep=""),sheet=1, na = "NA", skip=3))
    broo[[i]] <- as.data.frame(read_excel(paste("20", as.character(i), "_brooklyn.xls", sep=""),sheet=1, na = "NA", skip=3)) 
    
  }
  else if ( i > 10 ) {
    bronx[[i]] <- as.data.frame(read_excel(paste("20", as.character(i), "_bronx.xls", sep=""),sheet=1, na = "NA", skip=4))
    man[[i]] <- as.data.frame(read_excel(paste("20", as.character(i), "_manhattan.xls", sep=""),sheet=1, na = "NA", skip=4))
    queen[[i]] <- as.data.frame(read_excel(paste("20", as.character(i), "_queens.xls", sep=""),sheet=1, na = "NA", skip=4))
    si[[i]] <- as.data.frame(read_excel(paste("20", as.character(i), "_statenisland.xls", sep=""),sheet=1, na = "NA", skip=4))
    broo[[i]] <- as.data.frame(read_excel(paste("20", as.character(i), "_brooklyn.xls", sep=""),sheet=1, na = "NA", skip=4)) 
    
  }
}


#Fixing issues in column names above
for(i in 4:16){
  names(bronx[[i]]) <- names(man[[3]])
  names(man[[i]]) <- names(man[[3]])
  names(queen[[i]]) <- names(man[[3]])
  names(si[[i]]) <- names(man[[3]])
  names(broo[[i]]) <- names(man[[3]])
}

#Function to add year column 
fun <- function(x){
  for(i in 3:16){
    if (i < 10) {
      x[[i]]$Year <<- paste("200", as.character(i)) 
    }
    else {
      x[[i]]$Year <<- paste("20", as.character(i)) 
    }
  }
}

#For some reason the funciton doesn't work doing for loop over for now
for(i in 3:16){
  if (i < 10) {
    man[[i]]$Year <- paste("200", as.character(i),sep="") 
  }
  else {
    man[[i]]$Year <- paste("20", as.character(i),sep="") 
  }
}
#broo
for(i in 3:16){
  if (i < 10) {
    broo[[i]]$Year <- paste("200", as.character(i),sep="") 
  }
  else {
    broo[[i]]$Year <- paste("20", as.character(i),sep="") 
  }
}
for(i in 3:16){
  if (i < 10) {
    si[[i]]$Year <- paste("200", as.character(i),sep="") 
  }
  else {
    si[[i]]$Year <- paste("20", as.character(i),sep="") 
  }
}
for(i in 3:16){
  if (i < 10) {
    queen[[i]]$Year <- paste("200", as.character(i),sep="") 
  }
  else {
    queen[[i]]$Year <- paste("20", as.character(i),sep="") 
  }
}
for(i in 3:16){
  if (i < 10) {
    bronx[[i]]$Year <- paste("200", as.character(i),sep="") 
  }
  else {
    bronx[[i]]$Year <- paste("20", as.character(i),sep="") 
  }
}

#Function for loop to to join datasets by bourough
m_b <- list(1)
fun2 <- function(x,q){
  for(i in 3:17){ 
    if (i == 3) {
      m_b[[q]] <<- rbind(x[[i]], x[[i+1]])
      
    } else if (i > 5) {
      m_b[[q]] <<- rbind(m_b[[q]], x[[i-1]])
    }
  }
}

fun2(man,1)
fun2(broo,2)
fun2(bronx,3)
fun2(queen,4)
fun2(si,5)

#Loop to Join bouroughs into one nyc dataset
for(i in 1:6){ 
  if (i == 1) {
    prop_f <- rbind(m_b[[i]], m_b[[i+1]])
    
  } else if (i > 3) {
    prop_f <- rbind(prop_f, m_b[[i-1]])
  }
}

table(prop_f$Year)
#Eliminating trailing white space from above
prop_f$`BUILDING CLASS CATEGORY` <- trimws(prop_f$`BUILDING CLASS CATEGORY`,"r")
prop_f$ADDRESS <- trimws(prop_f$ADDRESS,"r")
setwd("C:/Users/Sarah Blair/Documents/R/Schools2/rdata")
saveRDS(prop_f, "all_prop.rds")
rm(bronx,broo,m_b,man,queen,si)
