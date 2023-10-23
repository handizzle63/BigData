#####################################################################
#####################################################################
## this script challenges you to discover the basics of data manipulation in R

## the data set we'll work with is species count data
## results from a survey of the frequency of different plant species
## at different sites going up the side of a hill
## ie different altitudes

spdata <- read.csv('C:\\Users\\Hannah\\OneDrive\\Documents\\Statistical Analyses\\species survey.csv') 

## have a good look at the data - check you understand it
## each row is a different site
## the first column is site ID number, the second is elevation of the site
## each other columnn is frequency of a different species
## (the number of quadrats in which the species was found, out of 5, at each site

## how many species in total?
# 20 species
## how many sites in total?
## (hint, you could do these easily enough by looking at row numbers, but try the dim function)
dim(spdata) # prints the number of rows and columns

## how many sites are above 400m?
spdata$elevation
spdata$elevation>400
sum(spdata$elevation>400)

## now try to get the same answer using the subset and length functions
high = subset(spdata, subset = spdata$elevation>400)
##help with length function
length(high$sp1) #choose any row to use length function as it will return the number of rows in the object which have been subsetted for  > 400m  

## what is the average frequency of each species?
## (hint, use the colMeans function)
colMeans(spdata)

## what is the average frequency of each species at sites above 400m?

colMeans(high)

## which species has the highest average frequency?
## (hint, use the which and max functions)

justsp = spdata[,-1:-2] #remove first two columns from data frame
justsp
colSums(justsp)
which.max(colSums(justsp))

## how many species were found at each site? (ie what is the species richness of each site?)
## (hint, use indexing and rowSums - lots of other ways too)

site = rowSums(justsp>0) #number of rows that have a value = 0 which is also the number of species that appear per site or species richness
site

## which site had the least number of species?
which.min(site)
#site 18

## plot species richness against elevation - does it look like there is a relationship?

plot(site~spdata$elevation,
     xlab = "Elevation (m)",
     ylab = 'Species Richness')


## fit a model to test this relationship

fit <- smooth.spline(spdata$elevation, site)

## plot the model predictions over the data points

lines(fit, col = "red", lwd = 2)


  ## which site had the most species found in at least three quadrats at that site?
d = rowSums(justsp>3) # making an object with the number of quadrats per row that are > 3
which.max(d)
#site 100

## which site had the most species found in at least two quadrats at that site?
which.max(rowSums(justsp>2)) #site 71

## which species was found at the most sites?
freq = colSums(justsp)
freq[which.max(freq)] #species 20,  414 quadrats across 200 sites

## which species was found over the greatest range of elevations? 
justsp[justsp > 1] <- 1 # changed all values >1 to 1 to make presence/absence matrix
colSums(justsp)
which.max(colSums(justsp))
#site sp5 present across greatest number of sites - but not necessarily greatest range

low = subset(spdata, subset = spdata$elevation<40)
low = low[,-1:-2]
low[low > 1] <- 1
low

high
high[high>1] <- 1
high = high[,-1:-2]
high
a = colSums(high)
which.max(colSums(high))

#minimum elevation for each species
low

#max elevation for each species
high

sp6 = 200 - 6
sp6
sp5 = 196 - 1
sp5
sp4 = 200 - 1
sp4
sp20 = 200 - 1

#sp20 and sp4 have greatest range

# get the smooth curve for the plot 






