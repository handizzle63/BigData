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
tempdata <- read.table('data/button1.txt', header=TRUE)

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

fdf <- list()    #creating empty list
b=1 # button
for(b in 1:22){
  dmin<-tapply(allbuttondata[[b]]$temp, allbuttondata[[b]]$day, min)     #min temp
  dmax<-tapply(allbuttondata[[b]]$temp, allbuttondata[[b]]$day, max)   #max temp
  ndf <- data.frame(mins=dmin, maxs=dmax)
  ndf$but <- b     #adding button column
  ndf$day <- names(dmax)      #adding day column
  fdf <- rbind(fdf,ndf)       # adding results to list
}
fdf

## 2. determine which button had the highest proportion of time above 25 degrees
allbuttondatadf=list()
for (but in 1:22){
  tempdf = allbuttondata[[but]]
  tempdf$but = but
  allbuttondatadf = rbind(allbuttondatadf,tempdf)
}

Then, 

nover25 <- tapply( allbuttondatadf$temp>25, allbuttondatadf$but, sum) 
ntotal <- tapply( allbuttondatadf$temp, allbuttondatadf$but, length)
perc_over25 <- nover25/ntotal


perc_over25[which(perc_over25==max(perc_over25))]


## 3. determine which button reached its daily maximum earliest on each day
max_time_per_button_per_day <- vector("list", length = 22)

for (but in 1:22) {  
  
  button_data <- allbuttondata[[but]]  
  
  unique_days <- unique(button_data$day)     
  
  max_time_per_button <- vector("character", length = length(unique_days))     
  
  for (i in seq_along(unique_days)) {    
    
    day <- unique_days[i]    
    
    day_data <- button_data[button_data$day == day, ]         
    
    if (nrow(day_data) > 0) {      
      
      max_temp <- which.max(day_data$temp)      
      
      max_time_per_button[i] <- day_data$time[max_temp]    
      
    }  
    
  }     
  
  max_time_per_button_per_day[[but]] <- max_time_per_button }

print(max_time_per_button_per_day)




max_length <- max(sapply(max_time_per_button_per_day, length))

max_time_matrix <- matrix(NA, nrow = 22, ncol = max_length)

for (but in 1:22) {  

max_time_matrix[but, 1:length(max_time_per_button_per_day[[but]])] <- max_time_per_button_per_day[[but]]

}

max_time_table <- as.data.frame(max_time_matrix)

colnames(max_time_table) <- paste("Day", 0:10, sep = "_")

print(max_time_table)

# OR

earliest_maxs <- data.frame(button1=c(rep(0, 11))) #Initialise empty df
temp_list <- list() #Initialise empty list

for(button in 1:22){
  
  maxs <- tapply(allbuttondata[[button]]$temp, allbuttondata[[button]]$day, max) #Finds button max by day
  
  #Need a nested for loop to iterate through each value in maxs
  for(i in 1:length(maxs)){ 
    
    ## need to subset for each day
    day_data <- subset(allbuttondata[[button]], allbuttondata[[button]]$day==i-1) 
    
    ## get the index of the maxs
    times <- day_data$totmin[which.max(day_data$temp==maxs[[i]])] 
    temp_list[[i]] <- times #Totmin for max n added to list
  }
  
  #Back into main loop. Add Full list to df
  earliest_maxs[button] <- data.frame(c(unlist(temp_list), rep(NA, 11-length(temp_list)))) 
  
  #Column names 
  colnames(earliest_maxs)[button] <- paste("Button",i,sep="") 
  
  temp_list <- list(rep(NA,11)) #Empty out the list
}

#present results
results_table <-list() 

#Gets column index ie button number
results_table <- rbind(apply(earliest_maxs, 1, which.min)) 

rownames(results_table)[1] <- "Button with highest temp"

coloumnnames = sapply(1:11, function(day) paste('Day', day, sep=' ')) #just builds vector with Day 1, Day 2 etc
colnames(results_table) = coloumnnames #renames columns 

results_table
## 4. extract all times at which button 2 was hotter then button 1

## First check the data alignment
allbuttondata[[1]]$totmin[1] #60
allbuttondata[[2]]$totmin[1] #180

## Need to move Button 1 up 12 more to align with Button 2

## Next check the data is the same length
length(allbuttondata[[1]]$totmin[1+12:length(allbuttondata[[1]]$totmin)]) #1471
length(allbuttondata[[2]]$totmin) #1538

## Button 1 data stops way before Button 2 does

## Next check all times match - remember to adjust list indices on Button 1 to match Button 2
all(allbuttondata[[1]]$totmin[13:(1471+12)] == allbuttondata[[2]]$totmin[1:1471], na.rm=T) 

## Cool - returns True

## Get all the times Button2 hotter than Button 1
times_butt2_hotter <- allbuttondata[[2]]$time[which(allbuttondata[[2]]$temp[1:1471]>allbuttondata[[1]]$temp[13:(1471+12)])]

length(times_butt2_hotter)
## 1335 times Button 2 was hotter

## 5. compile a list of which button was coldest at every point in time where at least ten buttons were recording
allbuttondatadf=list()

for (but in 1:22){  
  
  tempdf = allbuttondata[[but]]  
  
  tempdf$but = but  
  
  allbuttondatadf = rbind(allbuttondatadf,tempdf)
  
}

#Next we want to add another column to this data set, which is a sum of the number of buttons active at each time point (I called it 'activebuttons'):
  
  button_count <- numeric()

for (i in 1:nrow(allbuttondatadf)) {  
  
  time <- allbuttondatadf$time[i]      
  
  if (time %in% names(button_count)) {    
    
    button_count[as.character(time)] <- button_count[as.character(time)] + 1  
    
  } else {    
    
    button_count[as.character(time)] <- 1  
    
  }
  
} 

allbuttondatadf$activebuttons <- sapply(allbuttondatadf$time, function(time) {  
  
  button_count[as.character(time)]
  
})

#Next we create a new data frame and write a loop with an 'if' statement to give us the button with the lowest temperature when the number of active buttons is at least 10:
  
  coldestbuttondf <- data.frame(time = numeric(0), coldestbutton = numeric(0))

activebuttons10 <- unique(allbuttondatadf$time[allbuttondatadf$activebuttons >= 10])

for (time_point in activebuttons10) {  
  
  subset_data <- allbuttondatadf[allbuttondatadf$time == time_point & allbuttondatadf$activebuttons >= 10, ]  
  
  mintempbutton <- subset_data$but[which.min(subset_data$temp)]     
  
  coldestbuttondf <- rbind(coldestbuttondf, data.frame(time = time_point, coldest_button = mintempbutton))
  
}

print(coldestbuttondf)


