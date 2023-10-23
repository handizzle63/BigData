#### this is the starting script for intro to ecological modelling lab

### first some revision of looping basics and indexing
### we covered this in the intro lab, so this should just be a very quick check that 
### you are fully up to speed with loops and indexing





### look at this block of code
### what will x be at the end? check that you are correct - and make sure you understand if you were not
x=3:5 #vector with 3 numbers = 3, 4, 5
x[6]=6 #sixth integer should be 6
#no fourth and fifth integers = 3 4 5 NA NA 6
x 

### look at this block of code
### what will x be at the end? check that you are correct - and make sure you understand if you were not
x=4 #object with one value = 4
x[3]=x[2]+4 #third value is equal to the second value plus 4, but 2 isn't defined 
# so 4 NA NA
x

### look at this block of code
### what will x be at the end? check that you are correct - and make sure you understand if you were not
x=c(1,2) #object with two values: 1, 2
i=3 #creating object i equal to 3
x[i+1]=x[i]+3 #fourth value (3+1) is equal to the third integer plus 3
# but three isn't defined so will be 1 2 NA NA
x

### run this block of code ***
### why do I get an error?

for (i in 1:10){
    y[i+1]=y[i]+3
}
# error because y has not been created as an object

### look at this block of code? what will y be at the end? 

y=5

for (i in 1:10){
	y[i+2]=y[i+1]+y[i]
}

# should loop through 1:10 in y and do the formula but there's lots of NAs because
# y is an object with only one value


## now run it and check that you are correct - and make sure you understand if you were not

### now rerun the the block of code above, with the ***
### why do I NOT get an error now!? 
# because y has been defined as an object


### look at this block of code? what will x and y be at the end? 
y=5
x=3
for (i in 1:10){
	y[i+1]=y[i]+x+2
}
# x stays the same as there's no code to change that object
# y increases by 5 each run of the loops because x(3) + 2 = 5

## now run it and check that you are correct - and make sure you understand if you were not

### look at this block of code? what will x and y be at the end? 
y=10
x=c(3,4)
for (i in 1:10){
	y[i+1]=y[i]+x[i]+2
}
#only gives values for first two loops because x is two values big and i has been asked to loop
# to the tenth value 


##################################################################################
##################################################################################

## now we create a simple model of unbounded growth

##################################################################################
######## UNBOUNDED GROWTH MODEL

## first asign the parameter values and initial population
growthrate = 0.1
pop = 5
maxtime = 5
## now loop through the time steps
for (time in 2:maxtime){
	pop[time] = pop[time-1] + growthrate * pop[time-1]
}
## now look at the values of population density we generated
pop
## now plot the time against population density
plot(1:maxtime , pop)
## and make the plot look a bit nicer - see below for more explanation
plot(1:maxtime , pop, 
     type = 'l' , #line graph
     xlab='time', #xlabel
     ylab='population density',
     ylim = c(5,8),#y label
     lwd=2) #line width

################################################################

## do you understand all of that code?
## note how we define some variables at the beginning, like the growthrate, maxtime and the initial population (pop)
## some of these variables never change ie growthrate, maxtime
## and others (pop) are added to at each time step (each time thru the loop) to create a vector of values
## this vector tracks how the value changes over time so we can look at it or plot it later 


### there are a few questions in the lab instructions aboutt his model

## editing the model

growthrate = 0.3
pop = 5
maxtime = 5 
for (time in 2:maxtime){
  pop[time] = pop[time-1] + growthrate * pop[time-1]
}

pop

plot(1:maxtime , pop,
     type = 'l',
     xlab = 'Time',
     ylab = 'Population Density',
     lwd = 2,
     col = 'blue')

### Adding Death Rate 

growthrate = 0.1
deathrate = 0.05
pop = 5
maxtime = 100

for (time in 2:maxtime){
  pop[time] = pop[time-1] + growthrate * pop[time-1] - deathrate * pop[time-1]
}

plot(1:maxtime , pop,
     type = 'l',
     xlab = 'Time',
     ylab = 'Population Density',
     lwd = 2,
     col = 'red')

##can also have pop[time] = pop[time-1] + (growthrate - deathrate) * pop[time-1]
## but have added as extension to above equation because will be fiddling with it later



##################################################################################
######## BOUNDED GROWTH MODEL

## now we want to create a bounded growth model 
## copy the code from above into the space below these comments
## add a parameter for 'density dependent death rate parameter' 
## set it to 0.0001
## this will not change with time, so you should set it at the start of the loop
## there are different ways to do it
## one would be to change the existing line of code inside the loop
## another would be to define a new parameter in a separate line
## when you're done, check that your output plot makes sense to you
## and then have a look at the solutions below
## note you may have found a different and equally correct way to do it!
## if you really get stuck then you can scroll down to see the solutions
## but try to do it yourself first

growthrate = 0.1
ddd = 0.0001 #replaces previous death rate
pop = 5
maxtime = 250

for (time in 2:maxtime){
  pop[time] = pop[time-1] + growthrate * pop[time-1] - (ddd * pop[time-1]) * pop[time-1]
}

plot(1:maxtime , pop,
     type = 'l',
     xlab = 'Time',
     ylab = 'Population Density',
     lwd = 2,
     col = 'red',
     main = "Density Dependent Death Rate 0.0001")

which.max(pop)
pop[130]

tail(pop, 50)

## Bounded Model for editing
growthrate = 0.1
ddd = 0.0002
pop = 50
maxtime = 200

for (time in 2:maxtime){
  pop[time] = pop[time-1] + growthrate * pop[time-1] - (ddd * pop[time-1]) * pop[time-1]
}

plot(1:maxtime , pop,
     type = 'l',
     xlab = 'Time',
     ylab = 'Population Density',
     lwd = 2,
     col = 'red',
     main = "DDD Rate at 0.0001")

pop[50]
pop[100]

############################################
##        RESOURCE LIMITING MODEL         ##

pop = 5
maxtime = 365 #days
food = 100 #biomass of edible seeds, kg/km2

for (time in 2:maxtime){
  foodneeded = pop[time-1] * 0.01
  foodeaten = food[time-1] * pop[time-1] * 0.001
  food[time] = food[time-1] - foodeaten
  growthrate = 0.05 * (foodeaten - foodneeded)
  pop[time] = pop[time-1] + growthrate * pop[time-1]
}


plot(1:maxtime, pop,
     type = 'l',
     xlab = 'Days',
     ylab = 'Density',
     xlim = c(0,400),
     lwd = 2,
     col = 'blue',
     main = "Resource Limited Model")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2) #adds in food line
legend("topright",
       title= "",
       legend= c("Mice","Food (kg/km2)"), #levels
       col= c("blue","red"), #specify colours in order
       pch= 19, 
       bty="n")

pop[60:90]
food[365]
food[50]
pop[365]
pop[50]

## Allee Effect

pop = 5 #mice
maxtime = 800 #days
food = 100 #biomass of edible seeds, kg/km2

for (time in 2:maxtime){
  foodneeded = pop[time-1] * 0.01
  foodeaten = food[time-1] * pop[time-1] * 0.001
  food[time] = food[time-1] - foodeaten
  growthrate = 0.05 * (foodeaten - foodneeded) 
  pop[time] = pop[time-1] + growthrate * pop[time-1]
  if (pop[time]<3) pop[time]=0 #allee effect
}

plot(1:maxtime, pop,
     type = 'l',
     xlab = 'Days',
     ylab = 'Density',
     xlim = c(0,800),
     lwd = 2,
     col = 'blue',
     main = "Time to Total Local Extinction")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2)
legend("topright",
       title= "",
       legend= c("Mice","Food (kg/km2)"), 
       col= c("blue","red"),
       pch= 19, 
       bty="n")

pop[720:740]
pop[728:730]

### Model to Edit 

pop = 5 #mice
maxtime = 800 #days
food = 100 #biomass of edible seeds, kg/km2

for (time in 2:maxtime){
  foodneeded = pop[time-1] * 0.05
  foodeaten = food[time-1] * pop[time-1] * 0.001
  food[time] = food[time-1] - foodeaten
  growthrate = 0.05 * (foodeaten - foodneeded)  
  #to reproduce
  pop[time] = pop[time-1] + growthrate * pop[time-1]
  if (pop[time]<3) pop[time]=0 
}

plot(1:maxtime, pop,
     type = 'l',
     xlab = 'Days',
     ylab = 'Population Density',
     xlim = c(0,800),
     lwd = 2,
     col = 'blue',
     main = "Increased Food Demand 0.05")


## Better at finding food

pop = 5 #mice
maxtime = 1000 #days
food = 100 #biomass of edible seeds, kg/km2

for (time in 2:maxtime){
  foodneeded = pop[time-1] * 0.01
  foodeaten = food[time-1] * pop[time-1] * 0.005 
  food[time] = food[time-1] - foodeaten
  growthrate = 0.05 * (foodeaten - foodneeded) 
  pop[time] = pop[time-1] + growthrate * pop[time-1]
  if (pop[time]<3) pop[time]=0 #allee effect
}

plot(1:maxtime, pop,
     type = 'l',
     xlab = 'Days',
     ylab = 'Population Density',
     xlim = c(0,800),
     lwd = 2,
     col = 'blue',
     main = "Better Foraging Success")
pop[670:680]

## Smaller growth rate


pop = 5 #mice
maxtime = 1000 #days
food = 100 #biomass of edible seeds, kg/km2

for (time in 2:maxtime){
  foodneeded = pop[time-1] * 0.01
  foodeaten = food[time-1] * pop[time-1] * 0.001 
  food[time] = food[time-1] - foodeaten
  growthrate = 0.02 * (foodeaten - foodneeded) #reduced from 0.05 to 0.02
  pop[time] = pop[time-1] + growthrate * pop[time-1]
  if (pop[time]<3) pop[time]=0 #allee effect
}

plot(1:maxtime, pop,
     type = 'l',
     xlab = 'Days',
     ylab = 'Population',
     xlim = c(0,1000),
     lwd = 2,
     col = 'blue',
     main = "Smaller Growth Rate")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2) #adds in food line
pop[1000]

## Less initial food available

pop = 5 #mice
maxtime = 1000 #days
food = 95 #biomass of edible seeds, kg/km2

for (time in 2:maxtime){
  foodneeded = pop[time-1] * 0.01
  foodeaten = food[time-1] * pop[time-1] * 0.001
  food[time] = food[time-1] - foodeaten
  growthrate = 0.05 * (foodeaten - foodneeded) 
  pop[time] = pop[time-1] + growthrate * pop[time-1]
  if (pop[time]<3) pop[time]=0 
}

plot(1:maxtime, pop,
     type = 'l',
     xlab = 'Days',
     ylab = 'Population',
     xlim = c(0,1000),
     lwd = 2,
     col = 'blue',
     main = "Less Resources")

pop[730:760]

## Half Initial Food => Total Local Extinction

pop = 5 #mice
maxtime = 1000 #days
food = 50 #biomass of edible seeds, kg/km2

for (time in 2:maxtime){
  foodneeded = pop[time-1] * 0.01
  foodeaten = food[time-1] * pop[time-1] * 0.001 
  food[time] = food[time-1] - foodeaten
  growthrate = 0.05 * (foodeaten - foodneeded) 
  pop[time] = pop[time-1] + growthrate * pop[time-1]
  if (pop[time]<3) pop[time]=0 
}

plot(1:maxtime, pop,
     type = 'l',
     xlab = 'Days',
     ylab = 'Population',
     xlim = c(0,1000),
     lwd = 2,
     col = 'blue',
     main = "Half Food")

which.min(pop) #771 days
pop[770:780]

## Double initial invaders

pop = 10 #mice
maxtime = 1000 #days
food = 100#biomass of edible seeds, kg/km2

for (time in 2:maxtime){
  foodneeded = pop[time-1] * 0.01
  foodeaten = food[time-1] * pop[time-1] * 0.001 
  food[time] = food[time-1] - foodeaten
  growthrate = 0.05 * (foodeaten - foodneeded) 
  pop[time] = pop[time-1] + growthrate * pop[time-1]
  if (pop[time]<3) pop[time]=0 
}

which.min(pop)
plot(1:maxtime, pop,
     type = 'l',
     xlab = 'Days',
     ylab = 'Population',
     xlim = c(0,1000),
     lwd = 2,
     col = 'blue',
     main = "Double Population")

729-697
771-729


### Regenerating seed 
pop = 5 
maxtime = 1000 
food = 100 

for (time in 2:maxtime){
  foodneeded = pop[time-1] * 0.01
  foodeaten = food[time-1] * pop[time-1] * 0.001 
  food[time] = food[time-1] - foodeaten + 0.5 #0.5kg extra per day
  growthrate = 0.05 * (foodeaten - foodneeded) 
  pop[time] = pop[time-1] + growthrate * pop[time-1]
  if (pop[time]<3) pop[time]=0 
}

plot(1:maxtime, pop,
     type = 'l',
     xlab = 'Days',
     ylab = 'Density',
     xlim = c(0,800),
     lwd = 2,
     col = 'blue',
     main = "Regenerating Seed")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2)
legend("topright",
       title= "",
       legend= c("Mice","Food (kg/km2)"), 
       col= c("blue","red"),
       pch= 19, 
       bty="n")

which.min(food)
which.min(pop)
pop[400:600]
pop[215:245]
food[215:245]
food[300:500]
pop[360:370]
######################################
### Adding in second invader

pop1 = 5 
maxtime = 1000 
food = 100 
pop2 = 0

for (time in 2:maxtime){
  foodneeded = pop1[time-1] * 0.01 
  foodneeded2 = pop2[time-1] * 0.01 
  foodeaten = food[time-1] * pop1[time-1] * 0.001
  foodeaten2 = food[time-1] * pop2[time-1] * 0.001
  food[time] = food[time-1] - (foodeaten + foodeaten2) + 0.5 
  growthrate1 = 0.05 * (foodeaten - foodneeded) 
  growthrate2 = 0.05 * (foodeaten2 - foodneeded2)
  pop1[time] = pop1[time-1] + growthrate1 * pop1[time-1]
  pop2[time] = pop2[time-1] + growthrate2 * pop2[time-1]
  if (time == 365) {pop2[time] = 6
  }
  if (time <= 364) {pop2[time] = 0}  
  if (pop1[time]<3) pop1[time]=0 #keep in as a check
  if (pop2[time]<3) pop2[time]=0
}
  

plot(1:maxtime, pop1,
     main = "Two Invaders",
     ylab = "Density",
     xlab = "Days",
     type = "l",
     lwd = 2,
     col = "blue")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2)
lines(1:maxtime, pop2, col = "green", lwd = 2, lty = 2)
legend("topright",
       title= "",
       legend= c("Mouse 1","Mouse 2", "Food (kg/km2)"), 
       col= c("blue","green", "red"),
       pch= 19, 
       bty="n")

pop1[365:800]
pop2[365:800]
pop2[1000]
food [364:600]

### New Mouse is bigger
pop1 = 5 
maxtime = 2000 
food = 100 
pop2 = 0

for (time in 2:maxtime){
  foodneeded = pop1[time-1] * 0.01 
  foodneeded2 = pop2[time-1] * 0.015 #increase
  foodeaten = food[time-1] * pop1[time-1] * 0.001
  foodeaten2 = food[time-1] * pop2[time-1] * 0.001
  food[time] = food[time-1] - (foodeaten + foodeaten2) + 0.5
  growthrate1 = 0.05 * (foodeaten - foodneeded) 
  growthrate2 = 0.05 * (foodeaten2 - foodneeded2)
  pop1[time] = pop1[time-1] + growthrate1 * pop1[time-1]
  pop2[time] = pop2[time-1] + growthrate2 * pop2[time-1]
  if (time == 365) {pop2[time] = 6
  }
  if (time <= 364) {pop2[time] = 0}  
  if (pop1[time]<3) pop1[time]=0 
  if (pop2[time]<3) pop2[time]=0
}


plot(1:maxtime, pop1,
     main = "Mouse 2 ^ food requirements",
     type = "l",
     lwd = 2,
     col = "blue")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2)
lines(1:maxtime, pop2, col = "green", lwd = 2, lty = 2)
legend("topright",
       title= "",
       legend= c("Mouse 1","Mouse 2", "Food (kg/km2)"), 
       col= c("blue","green", "red"),
       pch= 19, 
       bty="n")

pop1[365:1000]
pop2[1000:1100]
food [364:1000]

### Same size, new mouse better at finding food
pop1 = 5 
maxtime = 2000
food = 100 
pop2 = 0

for (time in 2:maxtime){
  foodneeded = pop1[time-1] * 0.01 
  foodneeded2 = pop2[time-1] * 0.01 
  foodeaten = food[time-1] * pop1[time-1] * 0.001
  foodeaten2 = food[time-1] * pop2[time-1] * 0.0015
  food[time] = food[time-1] - (foodeaten + foodeaten2) + 0.5 
  growthrate1 = 0.05 * (foodeaten - foodneeded) 
  growthrate2 = 0.05 * (foodeaten2 - foodneeded2)
  pop1[time] = pop1[time-1] + growthrate1 * pop1[time-1]
  pop2[time] = pop2[time-1] + growthrate2 * pop2[time-1]
  if (time == 365) {pop2[time] = 6
  }
  if (time <= 364) {pop2[time] = 0} 
  if (pop1[time]<3) pop1[time]=0 
  if (pop2[time]<3) pop2[time]=0 
}


plot(1:maxtime, pop1,
     main = "Mouse 2 better forager",
     type = "l",
     lwd = 2,
     col = "blue")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2)
lines(1:maxtime, pop2, col = "green", lwd = 2, lty = 2)
legend("topright",
       title= "",
       legend= c("Mouse 1","Mouse 2", "Food (kg/km2)"), 
       col= c("blue","green", "red"),
       pch= 19, 
       bty="n")
pop1
pop2
food [300:600]
food [364:1000]
food[2000]
############
## Food to Coexist

pop1 = 5 
maxtime = 3000
food = 10 
pop2 = 0

for (time in 2:maxtime){
  foodneeded = pop1[time-1] * 0.01 
  foodneeded2 = pop2[time-1] * 0.01 
  foodeaten = food[time-1] * pop1[time-1] * 0.001
  foodeaten2 = food[time-1] * pop2[time-1] * 0.0015
  food[time] = food[time-1] - (foodeaten + foodeaten2) + 0.5 
  growthrate1 = 0.05 * (foodeaten - foodneeded) 
  growthrate2 = 0.05 * (foodeaten2 - foodneeded2)
  pop1[time] = pop1[time-1] + growthrate1 * pop1[time-1]
  pop2[time] = pop2[time-1] + growthrate2 * pop2[time-1]
  if (time == 365) {pop2[time] = 6
  }
  if (time <= 364) {pop2[time] = 0} 
  if (pop1[time]<3) pop1[time]=0 
  if (pop2[time]<3) pop2[time]=0 
}


plot(1:maxtime, pop1,
     main = "Coexisting?",
     type = "l",
     ylim = c(0,100),
     lwd = 2,
     col = "blue")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2)
lines(1:maxtime, pop2, col = "green", lwd = 2, lty = 2)
legend("topright",
       title= "",
       legend= c("Mouse 1","Mouse 2", "Food (kg/km2)"), 
       col= c("blue","green", "red"),
       pch= 19, 
       bty="n")
pop1[2000]

### Environmental variation over 6 years
pop1 = 5 
maxtime = (365*6)
food = 100 
pop2 = 0


for (time in 2:maxtime){
  foodneeded = pop1[time-1] * 0.01 
  foodneeded2 = pop2[time-1] * 0.01 
  foodeaten = food[time-1] * pop1[time-1] * 0.001
  foodeaten2 = food[time-1] * pop2[time-1] * 0.001
  food[time] = food[time-1] - (foodeaten + foodeaten2) + (1+sin(time/365*pi*2)) #added variation here
  growthrate1 = 0.05 * (foodeaten - foodneeded) 
  growthrate2 = 0.05 * (foodeaten2 - foodneeded2)
  pop1[time] = pop1[time-1] + growthrate1 * pop1[time-1]
  pop2[time] = pop2[time-1] + growthrate2 * pop2[time-1]
  if (time == 365) {pop2[time] = 6
  }
  if (time <= 364) {pop2[time] = 0} 
}

plot(1:maxtime, pop1,
     main = "Environmental Variation",
     type = "l",
     ylab = "Density",
     xlab = "Days",
     lwd = 2,
     col = "blue")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2)
lines(1:maxtime, pop2, col = "green", lwd = 2, lty = 2)
legend("topright",
       title= "",
       legend= c("Mouse 1","Mouse 2", "Food (kg/km2)"), 
       col= c("blue","green", "red"),
       pch= 19, 
       bty="n")

## Food Production
time = 1:365
foodprod = (1+sin(time/365*pi*2))
plot(foodprod)
which.max(foodprod)

## Food Availability after colonisation
a = food[365:maxtime]
p = pop1[365:maxtime]
q = pop2[365:maxtime]

plot(a,
     main = "Food Availablity Post Colonisaton (365+)",
     xlab = "Year",
     ylab = "Food (kg/km2)",
     ylim = c(0,220),
     type = "l",
     lwd = 2,
     xaxt = "n", #removes label from xaxis
     col = "red")
axis(1, labels=c('1','2', '3', '4', '5', '6'), 
         at=c(0,365,730,1095,1460,1825), 
         cex.axis=1)

lines(p, col = "blue", lwd = 2, lty = 2)
lines(q, col = "green", lwd = 2, lty = 2)

a[1:100]
p[1:100]
q[1:100]
### Pop2 over hundreds of years with food variation

pop1 = 5 
maxtime = (365*200)
food = 100 
pop2 = 0


for (time in 2:maxtime){
  foodneeded = pop1[time-1] * 0.01 
  foodneeded2 = pop2[time-1] * 0.01 
  foodeaten = food[time-1] * pop1[time-1] * 0.001
  foodeaten2 = food[time-1] * pop2[time-1] * 0.001
  food[time] = food[time-1] - (foodeaten + foodeaten2) + (1+sin(time/365*pi*2)) #added variation here
  growthrate1 = 0.05 * (foodeaten - foodneeded) 
  growthrate2 = 0.05 * (foodeaten2 - foodneeded2)
  pop1[time] = pop1[time-1] + growthrate1 * pop1[time-1]
  pop2[time] = pop2[time-1] + growthrate2 * pop2[time-1]
  if (time == 365) {pop2[time] = 6
  }
  if (time <= 364) {pop2[time] = 0} 
}


plot(1:maxtime, pop1,
     main = "Hundreds of Years",
     type = "l",
     ylab = "Density",
     xlab = "Days",
     lwd = 2,
     col = "blue")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2)
lines(1:maxtime, pop2, col = "green", lwd = 2, lty = 2)
legend("topright",
       title= "",
       legend= c("Mouse 1","Mouse 2", "Food (kg/km2)"), 
       col= c("blue","green", "red"),
       pch= 19, 
       bty="n")

#### without variation

pop1 = 5 
maxtime = (365*200)
food = 100 
pop2 = 0


for (time in 2:maxtime){
  foodneeded = pop1[time-1] * 0.01 
  foodneeded2 = pop2[time-1] * 0.01 
  foodeaten = food[time-1] * pop1[time-1] * 0.001
  foodeaten2 = food[time-1] * pop2[time-1] * 0.001
  food[time] = food[time-1] - (foodeaten + foodeaten2) + 0.5
  growthrate1 = 0.05 * (foodeaten - foodneeded) 
  growthrate2 = 0.05 * (foodeaten2 - foodneeded2)
  pop1[time] = pop1[time-1] + growthrate1 * pop1[time-1]
  pop2[time] = pop2[time-1] + growthrate2 * pop2[time-1]
  if (time == 365) {pop2[time] = 6
  }
  if (time <= 364) {pop2[time] = 0} 
}


plot(1:maxtime, pop1,
     main = "Environmental Variation",
     type = "l",
     ylab = "Density",
     xlab = "Days",
     lwd = 2,
     col = "blue")
lines(1:maxtime, food, col = "red", lwd = 2, lty = 2)
lines(1:maxtime, pop2, col = "green", lwd = 2, lty = 2)
legend("topright",
       title= "",
       legend= c("Mouse 1","Mouse 2", "Food (kg/km2)"), 
       col= c("blue","green", "red"),
       pch= 19, 
       bty="n")
pop2[73000]
