# 1. Apply your knowledge!

# example data
zones <- c('Central', 'Eastern', 'Lake', 'Northern', 'Southern', 'Southern Highlands', 'Western') 
set.seed(2016)
v1 <- sample(100, length(zones))
v2 <- sample(100, length(zones))
m <- cbind(v1, v2)


# we want to compute a new variable "v3" that is the sum of v1 and v2 capped at 100 (values larger than 100 are set to 100). 
# Show how you can do that in three ways (!)

# 1a. with vector algebra
v3 <- v1 + v2
v3[v3 > 100] <- 100
v3

# 1b. with a loop and and a branch
v3 <- rep(NA, length(v1))
for (i in 1:length(v1)) {
  v3[i] <- v1[i] + v2[i]
  if (v3[i] > 100) {
    v3[i] <- 100
  }
}
v3

# 1c. With apply
# first write a function
fun <- function(x) {
  z <- sum(x)
  z[z > 100] <- 100
  return(z)
}

# then use apply
v3 <- apply(m, 1, fun)
v3



### More data processing

# Read FAO data
d <- read.csv("Production_Crops_E_All_Data.csv", stringsAsFactors=FALSE)

# what are the variable names?
colnames(d)

# Which variable names end on an F (for flag)?
f <- grep('F$', colnames(d))

# remove these flag variables from the data.frame
d <- d[, -f]

# select the values for Maize and Yield
y <- d[d$Element == 'Yield', ]
y <- y[y$Item == 'Maize', ]


# Remove all "Countries" that are not really countries (see Country.Code)
y <- y[y$Country.Code < 500, ]


# Which fields have the values of interest (start with "Y")
# Either the names or the indices
f <- grep('^Y', colnames(d))

# Change the units to Mg (ton) / ha
y[, f] <- y[, f] / 10000

#for (i in f) {
#	y[, i] <- y[, i] / 10000
#}

unique(y$Unit)
y$Unit <- "Mg"


# Use "apply" to compute the mean maize yields for all countries for the years 2010 to 2014. 
ynow <- y[, c('Y2010', 'Y2011', 'Y2012', 'Y2013', 'Y2014')]
a <- apply(ynow, 1, mean, na.rm=TRUE)

# Make a new data.frame with the countries and the average yield computed above.
m <- data.frame(country=y$Country, yield=a)

# Sort this data.frame
m <- m[order(m$yield), ]

# Remove missing values
m <- na.omit(m)

# Plot the data and comment on data quality
barplot(m[,2], horiz=TRUE)

p <- barplot(m[150:169,2], horiz=TRUE)
text(1, p, m[150:169,1], adj=0)
# some of these are not credible (impossible). 

p <- barplot(m[1:14,2], horiz=TRUE)
text(0.25, p, m[1:14,1], adj=0)
# some of these are not credible (but possible). 


# Extract the time series for Tanzania 
tza <- y[y$Country == 'United Republic of Tanzania', ]


# plot time against yield

plot(1961:2014, tza[, 8:61], ylab='Yield', xlab='Year')

# more formal:
f <- grep('^Y', colnames(d))
year <- as.integer(gsub('Y', '', colnames(tza)[f]))
plot(year, tza[,f], ylab='Yield')

