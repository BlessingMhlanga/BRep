#Read the data
setwd("C:/Users/BMHLANGA/OneDrive - CIMMYT/Documents/Spatial.Data.Analysis/Day1/lab1_data")
d<-read.csv("Operating-Health-Facilities-with-location-2014.csv")
d
View(d)
# 2. how many rows and columns are there?
dim(d)
nrow(d)
ncol(d)

# 3. what are the variable names?
colnames(d)

# 4. What is the number of facilities for each zone?
table(d$ZONE) 


# 5. Which region has most facilities (and how many), and which has least (and how many)?

x <- table(d$REGION)
names(which.min(x))
# names(x)[which.min(x)]
min(x)

names(which.max(x))
max(x)

# 6. What is the average elevation of all clinics?

mean(d$ALTITUDE, na.rm=TRUE)


# 7. Create a data.frame with four columns: ZONE, REGION, the number of facilities, and their average elevation (by REGION)

# we want something like this

#     ZONE       REGION          elevation  n
#1 Central       Dodoma          1059.4     369
#2 Central       Singida         1355.4     210
#3 Eastern       Dar es Salaam   54.4       599
# etc

# This is difficult. I would suggest using the aggregate function. 
# If you struggle divide and conquer. Do what you can do first. 
# Split the task into parts, and then combine them again later.

# one approach
a1 <- aggregate(d$ALTITUDE, d[, c('ZONE', 'REGION')], FUN=mean, na.rm=TRUE) 
a2 <- aggregate(d$ALTITUDE, d[, c('ZONE', 'REGION')], FUN=length)
a <- merge(a1, a2, by=1:2)

# another approach
a <- aggregate(d$ALTITUDE, d[, c('ZONE', 'REGION')], FUN=function(x) c(mean(x, na.rm=TRUE), length(x))) 
a <- data.frame(a[,1:2], a[,3])

# 7b. provide appropriate column names 
colnames(a)[3:4] <- c('elevation', 'n')

# 7c. sort by ZONE, REGION
a <- a[order(a$ZONE, a$REGION), ]

# 7d. elevation should be rounded to the nearest 10 cm
a$elevation <- round(a$elevation, 1)

# 7e. show the first five lines of the data.frame
head(a, 5)
a[1:5, ]

# 7f. Save the file to disk (comma separated by values format)
write.csv(a, 'C:/Users/BMHLANGA/OneDrive - CIMMYT/Documents/Spatial.Data.Analysis/Day1/lab1_data/test.csv', row.names=FALSE)



# 8. Also read fdile "Tanzania-All-Waterpoints-July-2015.csv"
w <- read.csv("Tanzania-All-Waterpoints-July-2015.csv", stringsAsFactors=FALSE)

# 9. For each region, compute the total number of water points, and the total number of people served (POPULATION.SERVED), 
# and the most common quantity ("WATER_QUAN") and type ("WATERPOINT") (either use the "modal" function from the raster package; or 
# use the table function


wa <- aggregate(w[, 'POPULATION.SERVED'], w[, 'REGION', drop=FALSE], length) 
colnames(wa)[2] <- 'nwp'
wb <- aggregate(w[, 'POPULATION.SERVED', drop=FALSE], w[, 'REGION', drop=FALSE], sum, na.rm=TRUE) 

library(raster)
wc <- aggregate(w[, c('WATER_QUAN', 'WATERPOINT')], w[, 'REGION', drop=FALSE], modal, na.rm=TRUE) 
# or
f <- function(x) names(which.max(table(x)))
wc <- aggregate(w[, c('WATER_QUAN', 'WATERPOINT')], w[, 'REGION', drop=FALSE], f) 

#combine:
water <- merge(wa, wb, by='REGION')
water <- merge(water, wc, by='REGION')


# 10. Make a table that shows counts of water quality types ("WATER_QUAL") by region
qual <- table(w$REGION, w$WATER_QUAL)

# 11. Make a new data.frame that combines the REGION level totals/averages for the  Health facilities and for the water points, and for the water quality
# to get something like:

#  REGION               ZONE elevation   n  nwp POPULATION.SERVED   WATER_QUAN         WATERPOINT coloured fluoride fluoride abandoned good milky salty salty abandoned soft unknown
#1 Arusha           Northern    1542.9 334 4564           1776454 insufficient communal standpipe       18      161                  1    0     5    20               1 4227     131
#2 Dodoma            Central    1059.4 369 3532           4239573       enough communal standpipe       38        0                  0    0    21   594              22 2694     163
#3  Geita               Lake    1721.7 143 1944               384       enough          hand pump       32        1                  0    0    33    51               1 1561     265
#4 Iringa Southern Highlands    1541.1 241 2292            108092       enough communal standpipe        3        0                  0    0     0    27               4 2191      67
#5 Kagera               Lake    1426.5 298 4531            572969       enough communal standpipe       64        5                  0    0    76   117               2 4066     201
#6 Katavi Southern Highlands    1162.5  77 1069               217       enough          hand pump        6        0                  0    0     6   127               2  907      21


m <- merge(a, water, by='REGION')
q <- as.data.frame.matrix(qual)
m <- merge(m, q, by.x='REGION', by.y=0)

# or
m <- merge(a, water, by='REGION')
qq <- as.data.frame(qual)
qq <- reshape(qq, direction='wide', timevar='Var2', idvar='Var1')
m <- merge(m, qq, by.x='REGION', by.y=1)
