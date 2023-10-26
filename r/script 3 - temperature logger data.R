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
tempdata <- read.table('C://Users//Hannah//OneDrive//Documents//Statistical Analyses//BigData//button1.txt', header=TRUE)

head(tempdata)

## we could try plotting time against temp
plot(tempdata$time,tempdata$temp,t='l')#error
plot(time~temp, 
     t = 'l',
     data = tempdata) # not working time = chr


tempdata$time

## plot by index (not specifying an x variable)
plot(tempdata$temp,t='l')

## and now the second button
tempdata2 <- read.table('button2.txt',header=TRUE)
head(tempdata2)

## add lines to overlay on previous plot
lines(tempdata2$temp,t='l',col='red')

## BUT - start at different times - so this isn't correct

## altering time vector
tempdata$time[1]
strsplit(as.character(tempdata$time[1]),':') #split the first time by the : so now 3 separate values at position one
strsplit(as.character(tempdata$time[1]),':')[[1]]
strsplit(as.character(tempdata$time[1]),':')[[1]][1]
strsplit(as.character(tempdata$time[1]),':')[[1]][2]

strsplit(as.character(tempdata$time),':') #split all of the times by :
splitlist <- strsplit(as.character(tempdata$time),':') #created a vector of all the split times


## so we want to pull the day out of each element of the list
## we can do that with the the sapply function

sapply(1:length(splitlist) , function(i) splitlist[[i]][1]) 
day <- as.numeric(sapply(1:length(splitlist) , function(i) splitlist[[i]][1]) ) #day is the first value from each 'split' trio 
day 

## now we pull out hour - second value from each trio
hr <- as.numeric(sapply(1:length(splitlist) , function(i) splitlist[[i]][2]) )

## and now minute - third value from each trio
min <- as.numeric(sapply(1:length(splitlist) , function(i)
  splitlist[[i]][3]) )

## and now we can calculate the number of minutes from a 0:0:0 reference
totmin <- ((day*24)+hr)*60+min

## and add those to the dataframe
tempdata$day <- day
tempdata$hr <- hr
tempdata$min <- min
tempdata$totmin <- totmin
head(tempdata)

#### Creating a loop for each button file

## see what this does...
paste('button',1,'.txt',sep='')


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

## PLOTS using Loops
## Plot first, overlay rest with lines
plot(allbuttondata[[1]]$totmin,allbuttondata[[1]]$temp,t='l',xlim=c(0,16000),ylim=c(3,30),xlab='time',ylab='temp')
for (but in 1:22){
	lines(allbuttondata[[but]]$totmin,allbuttondata[[but]]$temp,col=but)
}


##or just focussing on the first day
## only difference is the xlims
plot(allbuttondata[[1]]$totmin,allbuttondata[[1]]$temp,t='l',xlim=c(0,24*60),ylim=c(3,30),xlab='time (mins)',ylab='temp')
for (but in 1:22){
	lines(allbuttondata[[but]]$totmin,allbuttondata[[but]]$temp,col=but)
}

## Maximum Temp per Button
(maxs <- sapply(1:22, function(but) max(allbuttondata[[but]]$temp) ) )

## Time at Max Temp per Button
(maxtimes <- sapply(1:22, function(but) allbuttondata[[but]]$time[which(allbuttondata[[but]]$temp==maxs[but])]  ) )
 
## Hour at Max Temp per Button
(maxtimes <- sapply(1:22, function(but) allbuttondata[[but]]$hr[which(allbuttondata[[but]]$temp==maxs[but])]  ) )

## or the first time that button 1 is hotter than button 2
plot(allbuttondata[[1]]$totmin,allbuttondata[[1]]$temp,t='l',xlim=c(0,24*60),ylim=c(3,30),xlab='time',ylab='temp')
lines(allbuttondata[[2]]$totmin,allbuttondata[[2]]$temp,col=2)
## first need to line them up
allbuttondata[[1]]$totmin[1:10]
allbuttondata[[2]]$totmin[1:10]
allbuttondata[[1]]$totmin[(1:10)+12] #aligning

which(allbuttondata[[2]]$temp[1:100]<allbuttondata[[1]]$temp[(1:100)+12]) 
#prints positions where temp is greater in button 1 than 2 (aligned +12)
allbuttondata[[2]]$time[30] #time at position 30
allbuttondata[[2]]$totmin[30] #total minutes at position 30
abline(v=470,lty=2) #adding vertical line

## or the minimum temperature on each day for button 1
tapply(allbuttondata[[1]]$temp, allbuttondata[[1]]$day, min)

## or the proportion of time on each day that the temperature was below 20 degrees for button 1
abline(h=20,lty=3) #horizontal line from temp = 20
nunder20 <- tapply( allbuttondata[[1]]$temp<20, allbuttondata[[1]]$day, sum) 
ntotal <- tapply( allbuttondata[[1]]$temp, allbuttondata[[1]]$day, length)
nunder20 / ntotal

## then as a challenge, you could try to...
## 1. compile a table of daily mins and maxs for all buttons



## 2. determine which button had the highest proportion of time above 25 degrees
## 3. determine which button reached its daily maximum earliest on each day
## 4. extract all times at which button 2 was hotter then button 1
## 5. compile a list of which button was coldest at every point in time where at least ten buttons were recording

