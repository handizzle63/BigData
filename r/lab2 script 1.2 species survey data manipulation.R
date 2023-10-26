#####################################################################
#####################################################################
## this script challenges you to discover the basics of data manipulation in R

## the data set we'll work with is species count data
## results from a survey of the frequency of different plant species
## at different sites going up the side of a hill
## ie different altitudes

spdata <- read.csv('data/species survey.csv') 
spdata
# columns 3:23 are species
# rows are samples (sites)
# column X = site - 5x samples taken per site
# elevation = altitude
# the number is the number of quadrats in which the species was found at each site
# max of 5 quadrats done at each site


## Q: how many species in total?
# 20 species

## Q: how many sites in total?

dim(spdata) # prints the number of rows and columns
tail(spdata)
# 200 sites

## Q:  how many sites are above 400m?

sum(spdata$elevation>400) #42 sites

## Q: now try to get the same answer using the subset and length functions
high = subset(spdata, subset = spdata$elevation>400)
length(high$X) #choose any row, length(df$x) will return the number of rows

## Q: what is the average frequency of each species?
## (hint, use the colMeans function)
library(dplyr)
justsp <- spdata %>%
  select(-c(X, elevation))%>%
  glimpse()

spmean <-  colMeans(justsp) 
spmean

## Q: what is the average frequency of each species at sites above 400m?

colMeans(high)

## Q: which species has the highest average frequency?

which.max(spmean) #sp20

## Q: how many species were found at each site? (ie what is the species richness of each site?)
## (hint, use indexing and rowSums - lots of other ways too)

rich = rowSums(justsp>0) #number of rows that have a value = 0 which is also the number of species that appear per site or species richness
rich

## Q: which site had the least number of species?
which.min(rich) #at site 18 (or row 18 tbf)

rich[which.min(rich)] #returns 6 species

## plot species richness against elevation - does it look like there is a relationship?
spdata <- cbind(spdata, rich)

plot(rich~elevation,
     xlab = "Elevation (m)",
     ylab = 'Species Richness',
     data = spdata)


## fit a model to test this relationship
library(mgcv)

fit <- gam(rich ~ s(elevation, k = 5, bs = 'cr'), data = spdata)

## plot the model predictions over the data points
plot(fit)

plot(rich~elevation,
     xlab = "Elevation (m)",
     ylab = 'Species Richness',
     data = spdata)

## answer from Michael
# x = elevation
# y = richness
plot(x,y)
fm=smooth.spline(x,y)
lines(x,predict(fm)$y,col='green')
library(mgcv)
fm1 = gam(y~s(x))
AIC(fm1)
lines(x,predict(fm1),col='red')
fm2 = lm(y~x+I(x^2))
AIC(fm2)
lines(x,predict(fm2),col='blue')
fm3 = lm(y~x)
AIC(fm3)
lines(x,predict(fm3),col='purple')



## which site had the most species found in at least three quadrats at that site?
a <- rowSums(justsp>=3) #equal to or greater than

which(a==max(a)) #should print all the sites max if more than 1 is equal to another
a[which(a == max(a))] 


## which site had the most species found in at least two quadrats at that site?
a <- rowSums(justsp>=2)
which(a==max(a)) #site 102
a[which(a == max(a))] #13 species in at least 2 quadrats

## Q: which species was found at the most sites?
a <- colSums(justsp>0)

a[which(a==max(a))] #164 sites - species 5

## Q: which species was found over the greatest range of elevations? 

elevdata = spdata%>%
  select(-c(X, rich))

elevbysp <- sapply(justsp, function (x) ifelse (x>0,elevdata$elevation,x))

df<-data.frame(elevbysp)

elevrange <- sapply(df, function(x) max(x) - min(x[x>0]))

print(elevrange)
elevrange[which(elevrange == max(elevrange))]

#sp 4 & sp20








