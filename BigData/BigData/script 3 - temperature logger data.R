####################################################################
####################################################################
## temperature logger data

# Clear memory ----
rm(list=ls())

## notice all the 'buttonX.txt' files
## these are outputs from ibutton temperature loggers 
## placed at different points within a forest, 
## including within the soil, on the soil and on tree branches

## have a look at the 'button1.txt' data file using a text editor such as notepad
## notice the two columns
## the first column shows a date-time stamp in the format day:hour:minute
## the second column shows the temperature recorded at that time

## read it in and have a look
## notice we have to use 'read.table' instead of 'read.csv' because it is a text file
tempdata <- read.table('button1.txt',header=TRUE)
head(tempdata)

## we could try plotting time against temp
plot(tempdata$time,tempdata$temp,t='l')
## it kind of works, but it looks a bit strange
## this is because time is a character string, not a number
## so it has been read in as a factor
tempdata$time

## we could just plot by index and get a fairly good plot
plot(tempdata$temp,t='l')


## and now the second button
tempdata2 <- read.table('button2.txt',header=TRUE)
head(tempdata2)
## we use lines instead of plot, so that we keep the first plot
lines(tempdata2$temp,t='l',col='red')

## looks easy, but notice the logger is starting from a different time, 
## so the x-axis is not comparable

## we need to process the time stamp to make it numerically meaningful

## see if you can follow what each of these lines is doing - ask if in any doubt
tempdata$time[1]
strsplit(as.character(tempdata$time[1]),':')
strsplit(as.character(tempdata$time[1]),':')[[1]]
strsplit(as.character(tempdata$time[1]),':')[[1]][1]
strsplit(as.character(tempdata$time[1]),':')[[1]][2]

strsplit(as.character(tempdata$time),':')
splitlist <- strsplit(as.character(tempdata$time),':')

## so we want to pull the day out of each element of the list
## we can do that with the the sapply function

sapply(1:length(splitlist) , function(i) splitlist[[i]][1]) 
day <- as.numeric(sapply(1:length(splitlist) , function(i) splitlist[[i]][1]) )
day

## now we pull out hour
hr <- as.numeric(sapply(1:length(splitlist) , function(i) splitlist[[i]][2]) )

## and now minute
min <- as.numeric(sapply(1:length(splitlist) , function(i) splitlist[[i]][3]) )

## and now we can calculate the number of minutes from a 0:0:0 reference
totmin <- ((day*24)+hr)*60+min

## and add those to the dataframe
tempdata$day <- day
tempdata$hr <- hr
tempdata$min <- min
tempdata$totmin <- totmin
head(tempdata)

## now we could do that seperately for each button file, but that would be a bit tedious
## so lets use a loop to automate it

## notice there are 22 button data files 

## see what this does...
paste('button',1,'.txt',sep='')

## so we can loop like this
## it might look complex, but notice how each step is just what we did before
allbuttondata <- list()    ## create an empty list to hold all the data
for (but in 1:22){
	filename <- paste('button',but,'.txt',sep='')
	print(filename)
	tempdata <- read.table(filename,header=TRUE)
	splitlist <- strsplit(as.character(tempdata$time),':')
	day <- as.numeric(sapply(1:length(splitlist) , function(i) splitlist[[i]][1]) )
	hr <- as.numeric(sapply(1:length(splitlist) , function(i) splitlist[[i]][2]) )
	min <- as.numeric(sapply(1:length(splitlist) , function(i) splitlist[[i]][3]) )
	totmin <- ((day*24)+hr)*60+min
	tempdata$day <- day
	tempdata$hr <- hr
	tempdata$min <- min
	tempdata$totmin <- totmin
	print(head(tempdata))
	allbuttondata[[but]] <- tempdata 	## this adds the finished data frame to the list
}

## now we can loop back through the list and plot them
## first plot the first one
plot(allbuttondata[[1]]$totmin,allbuttondata[[1]]$temp,t='l',xlim=c(0,16000),ylim=c(3,30),xlab='time',ylab='temp')
for (but in 1:22){
	lines(allbuttondata[[but]]$totmin,allbuttondata[[but]]$temp,col=but)
}
##or just focussing on the first day
## only difference is the xlims
plot(allbuttondata[[1]]$totmin,allbuttondata[[1]]$temp,t='l',xlim=c(0,24*60),ylim=c(3,30),xlab='time',ylab='temp')
for (but in 1:22){
	lines(allbuttondata[[but]]$totmin,allbuttondata[[but]]$temp,col=but)
}

## now we might want to know the maximum temp recorded for each button
(maxs <- sapply(1:22, function(but) max(allbuttondata[[but]]$temp) ) )

## or the times they occured
(maxtimes <- sapply(1:22, function(but) allbuttondata[[but]]$time[which(allbuttondata[[but]]$temp==maxs[but])]  ) )
 
## or the hours of day they occured
(maxtimes <- sapply(1:22, function(but) allbuttondata[[but]]$hr[which(allbuttondata[[but]]$temp==maxs[but])]  ) )

## or the first time that button 1 is hotter than button 2
plot(allbuttondata[[1]]$totmin,allbuttondata[[1]]$temp,t='l',xlim=c(0,24*60),ylim=c(3,30),xlab='time',ylab='temp')
lines(allbuttondata[[2]]$totmin,allbuttondata[[2]]$temp,col=2)
## first need to line them up
allbuttondata[[1]]$totmin[1:10]
allbuttondata[[2]]$totmin[1:10]
allbuttondata[[1]]$totmin[(1:10)+12]

which(allbuttondata[[2]]$temp[1:100]<allbuttondata[[1]]$temp[(1:100)+12])
allbuttondata[[2]]$time[30]
allbuttondata[[2]]$totmin[30]
abline(v=470,lty=2)

## or the minimum temperature on each day for button 1
tapply(allbuttondata[[1]]$temp, allbuttondata[[1]]$day, min)

## or the proportion of time on each day that the temperature was below 20 degrees for button 1
abline(h=20,lty=3)
nunder20 <- tapply( allbuttondata[[1]]$temp<20, allbuttondata[[1]]$day, sum) 
ntotal <- tapply( allbuttondata[[1]]$temp, allbuttondata[[1]]$day, length)
nunder20 / ntotal


## then as a challenge, you could try to...
## 1. compile a table of daily mins and maxs for all buttons
tapply(allbuttondata[[1]]$temp, allbuttondata[[1]]$day, min)

alltemp <- list()

for (but in 1:22){
min <- tapply(allbuttondata[[but]]$temp, allbuttondata[[but]]$day, min)
max <- tapply(allbuttondata[[but]]$temp, allbuttondata[[but]]$day, max)
temp <- data.frame(min = min,max = max)
temp$but <- but
temp$day <- 0:(length(temp$but)-1)  #need to tell it that the first value is day = 0
alltemp = rbind(alltemp,temp)
}


## 2. determine which button had the highest proportion of time above 25 degrees

allprop <- list()

for (but in 1:22){
    nover25 <- tapply( allbuttondata[[but]]$temp>25, allbuttondata[[but]]$time, sum) 
    ntotal <- tapply( allbuttondata[[but]]$temp, allbuttondata[[but]]$time, length)
    nprop <- tapply(nover25(but)/ntotal(but))
    prop <- data.frame(nover25 = nover25, ntotal = ntotal, nprop = nprop)
    prop$but <- but
    allprop = rbind(allprop, prop)
}

## 3. determine which button reached its daily maximum earliest on each day

## 4. extract all times at which button 2 was hotter then button 1

## 5. compile a list of which button was coldest at every point in time where at least ten buttons were recording

