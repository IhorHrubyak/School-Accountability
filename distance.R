#Final dataset has to have the school zone, the year of the zone and the polygon coordinates of zone
#This can be done in the end possibely or while joining properties into the school zones


#https://cran.r-project.org/web/packages/geosphere/geosphere.pdf
#Distance between points and lines
dist2Line


#Loading School Zone line data and converting gemometric coordinates into compatible format to convert to line
#Actually ideal data set should combine data set should have the DBN and the coordinates so they can be matched
#A for loop can be run using a unique identifer, such as sch_zone which should be in both datasets, so for i in sch_zone

#Something like this would ultimatley need to done
#for(i in 1:nrow(prop)) {
# for (j in elements in list)
# if(zone[[i]$zone] == prop$zone[i] ){
# prop$dist[i] <- dist2Line(a, line)
# } else {
# do nothing/skip
# }
#}



#Can maybe create separte datasets for each of the zones and then combine them into a list. 

setwd("F:/stuff/NYC Schools/Zones")
z <- read.csv("11-12.csv")

sc$Location.1 <- gsub("^.*\\(","", sc$Location.1)
sc$Location.1 <- gsub("\\)","", sc$Location.1)
sc <- cbind(sc,do.call(rbind, strsplit(as.vector(sc$Location.1), split = ",")))
names(sc)[40:41] <- c("latitude","longitude")
sc$latitude <- as.numeric(as.character(sc$latitude))
sc$longitude <- as.numeric(as.character(sc$longitude))
#84X497 coordinates wrong as they're 0,0

#Gets rid of characters
z$the_geom2 <- gsub("[A-z]","",z$the_geom)
#Get rid of ((
z$the_geom2 <- gsub("\\(","",z$the_geom2)
#Can now take each of the first 2 whole numbers up to , and place in a new dataset
sc <- cbind(do.call(rbind, strsplit(as.vector(z$the_geom2), split = ",")))
#this kinda seems right but it's messy

#This more or less works...as convulted as it is. Just need a for loop to automate put back into another list,varibale names and unique identifer...cake
t <- strsplit(as.vector(z$the_geom2), split = ",")
t2 <- lapply(t,as.data.frame)
a <- as.data.frame(cbind(t[[1]],do.call(rbind, strsplit(as.vector(t2[[1]][,1]), split = " "))))

#Names of zones or dbns...for loop through each of the rows with the first element becoming an entire column for the list
db <- z$DBN

for(i in 1:737) {
  a[[i]]$dbn == z$DBN[i]
} 

t <- strsplit(z$the_geom2, split = ",")
p <- cbind(t[1,1],do.call(rbind, strsplit(as.vector,t[1,1]), split = " "))
str(as.data.frame(t[1,1]))
t2 <- lapply(t,as.data.frame)
t3 <- sapply(t,strsplit,split = " ")
#Maybe something like below for loop
#For each row in zone pull the two characters into a new varibale in separte dataset i
# when number of charters in row = 0 then move on to next row for new dataset


#Can use the below to split if if I can get into one string
sc <- cbind(sc,do.call(rbind, strsplit(as.vector(sc$Location.1), split = ",")))

line <- rbind(c(-73.95473514788019, 40.59502825807329),
              c(-73.95469070045796, 40.594791169807415),
              c(-73.9546216767152, 40.594422188568025), 
              c(-73.95365951350364, 40.59453048027025),
              c(-73.95269927339858, 40.59463749382784),
              c(-73.95175465258309, 40.59473791457866),
              c(-73.95055414747199, 40.594870555598405),
              c(-73.95015202405753, 40.592753189583156),
              c(-73.94911583959598, 40.592867209762446), 
              c(-73.9487173318018, 40.59074670588315), 
              c(-73.94778690895299, 40.590850567875144),
              c(-73.94686188485302, 40.59095070480282),
              c(-73.94645886587605, 40.58882931718442),
              c(-73.94734643726757, 40.588507051836295), 
              c(-73.94696109898847, 40.58647896844264),
              c(-73.94679297199575, 40.58559430552463), 
              c(-73.94674843389734, 40.58535117066841),
              c(-73.94669113294844, 40.58507277426171), 
              c(-73.94641529496758, 40.58356953186315),
              c(-73.94705571385379, 40.58354343710388), 
              c(-73.94787786442133, 40.5835174213614),
              c(-73.94800540721678, 40.58350592633),
              c(-73.94942711530874, 40.583377777898974), 
              c(-73.95027742142764, 40.58330112542327), 
              c(-73.95038530058602, 40.58344942787315), 
              c(-73.95090326054572, 40.5843552967048),
              c(-73.95106514588784, 40.58463088120334),
              c(-73.95153721352244, 40.584677294801374),
              c(-73.9547417720299, 40.585040253933116),
              c(-73.95485543096397, 40.58562477741643),
              c(-73.95522691064438, 40.58768491242727),
              c(-73.95526485038181, 40.58786124943329), 
              c(-73.95567342198058, 40.589984137880094), 
              c(-73.95607720420888, 40.592101431905654),
              c(-73.95655540402991, 40.594647428977154), 
              c(-73.95564781738834, 40.594849970285324), 
              c(-73.95511439406377, 40.59494760296652),
              c(-73.95473514788019, 40.59502825807329))





line <- rbind(c(-180,-20), c(-150,-10), c(-140,55), c(10, 0), c(-140,-60))
pnts <- rbind(c(-73.95230060,40.58828))

#distHaversine distance in meters is the default. Can be converted to miles by dividing by 1600 or so
d = dist2Line(a, line)
d2 <- as.data.frame(d)

head(a)
plot(makeLine(line), type='l')

te <- makeLine(line)

points(line)
points(d, col='blue', pch=50)
points(d[,2], d[,3], col='red', pch='x')
for (i in 1:nrow(d)) lines(gcIntermediate(pnts[i,], d[i,2:3], 10), lwd=2)

te <- as.data.frame((d))
str(te)