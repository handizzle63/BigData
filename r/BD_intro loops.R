######################################################
### an introduction to looping basics and indexing ###
######################################################

x = 5
b = 3
x = b * 5

x

y = c(1,3,6) #vector of three numbers in order specified
y


y = 3:10 #vector with numbers from 3 to 10
y

y[9]=16 #indexing: changes 9th number in vector to 16. If no 9th number will add to end
y

y[11] = 55 #no 10th sample in vector so adds an NA then 55 in position 11
y

x[3]=4
x

## note that the NA means that the value is not defined
## in this case the second value in x has not been defined yet
x[2]=99
x

### make a vector of numbers that goes from 23 to 86, but with 50 replaced by 99 
z = 23:86
z[28]
z[28] = 99
z

## the code below loops through the numbers 1 to 7 ie 1,2,3,4,5,6,7
## for the first time thru the loop, the variable 'i' is set to the first value, ie 1
## with each further time thru the loop the variable 'i' is set equal to the next value
## the bit inside the {} is performed, or executed, each time through the loop

for (i in 1:7) { 
	print(i)
}

## I can have more than one line of code within the loop, ie within the {}
## and those lines inside the {} will be executed each time thru the loop

for (i in 1:7){
	a = 2*i #first loop a = 2, at this step, then
	print(a+i) #a will equal 3 on first loop. 
}

## I can use a loop to build up a list/vector of values

x=1
for (i in 1:10){ #running the loop 10 times for values 1-10
	x[i+1]=x[i]*2 
	print(x)
}

### to help understand this loop, it might help to try to write it out without the loop, understanding that at each step of the loop, the value of i is set to a new value, and the code inside the {} is repeated, like this:

x=1
i=1
x[i+1]=x[i]*2 ## [index] so x[i+1] is x[2] or second position and is equal to [i] (which in first loop is 1)*2. we've specified a second position here so second loop will be i=2
print(x)
i=2
x[i+1]=x[i]*2 ## and since i=2 this creates x[3] as x[2]*2
print(x)
i=3
x[i+1]=x[i]*2 ## and since i=3 this creates x[4] as x[3]*2
print(x)
i=4
x[i+1]=x[i]*2
print(x)

## ... and so on... 10 times in total... at each step an extra value is added to x so at the end, so at the end, x will contain 10 values


## note that the order of lines inside the loop matters
## make sure you understand why these two loops give different output 

x=1
for (i in 1:10){ #i = 1 first loop and ascends by 1 each time over 10 loops
	x[i+1]=x[i]*2 #as above
	print(x) #then prints 
}

x=1
for (i in 1:10){ # 10 loops, first loop i
	print(x) #then prints i=1
	x[i+1]=x[i]*2 #then adds formula -> see object in global enviromment
}
x #so it's done the same thing just printed in console at the prior step 

## now before you execute it, look carefully at the code below
## see if you can predict what it will output BEFORE you execute/run it

x=c(1,2) #object with 2 numbers (1,2)
x
for (i in 3:8){ #loops from positions 3:8 so 1,2 will remain the same
	x[i]=x[i-2]+x[i-1] #first loop x[i] is = x[3-2]+x[3-1] = x[3]
	print(x)
}

#### if you find this one tricky, then it might help to again write it out step by step without the loop... like this...

x=c(1,2)
x
i=3
x[i]=x[i-2]+x[i-1]  ## ie x[3]=x[1]+x[2]
print(x)
i=4
x[i]=x[i-2]+x[i-1]  ## ie x[4]=x[2]+x[3]
print(x)
i=5
x[i]=x[i-2]+x[i-1]
print(x)
#... etc



################# can you see why these two loops below give different outputs?? 
x = 1
y = 2
for (i in 1:5){ #5 loops
	x=x+y # 3=1+2. x now = 3
	y=y+x #  5=2+3
	print(paste(x,y))
}


x = 1
y = 2
for (i in 1:5){ #5 loops, first loop
	y=y+x # 3=1+2, y=3
	x=x+y # 4=1+3, x = 4
	print(paste(x,y)) # first loop = "4","3" (remember order specified in paste)
}

### now try to write a loop that outputs the following lines and goes 20 times:
#Hint: you will need to use 'paste'
#eg.
#x=1
#paste(x,"+",x,"=",x+x,sep='')
1+1=2
2+2=4
4+4=8
8+8=16

x=1
i=1
y=x[i]+x[i] #don't need to have the i just need to define x
y
print(paste(x, "+", x, "=",y, sep='')) #before we update x for the next loop
x=x*2 # update for next loop

#now below with the loop function

x=1
for (i in 1:20){
  y=x+x
  print(paste(x, "+", x, "=",y, sep=''))
  x=x*2}
  

##################################################################################
##################################################################################

## now we create a simple model of unbounded growth - this is the first model in the lab next week

##################################################################################
######## UNBOUNDED GROWTH MODEL

## first assign the parameter values and initial population
growthrate = 0.1
pop = 5
maxtime = 5 

## now loop through the time steps

for (time in 2:maxtime){ #from 2 to maxtime (which has been defined as 5)
	pop[time] = pop[time-1] + growthrate * pop[time-1] #5[time]=5[time-1]+0.1*5[time-1]
}

## R is creating a vector called time for each loop from 2 to the maxtime of 5
## it knows to do this because we have predefined maxtime so we don't need to specify time as a 
## vector in a new line

## now look at the values of population density we generated
pop
## now plot the time against population density
plot(1:maxtime , pop)
## and make the plot look a bit nicer - see below for more explanation
plot(1:maxtime , pop, type = 'l' ,xlab='time',ylab='population density',lwd=2)

################################################################

## do you understand all of that code?
## note how we define some variables at the beginning, like the growthrate, maxtime and the initial population (pop)
## some of these variables never change ie growthrate, maxtime
## and others (pop) are added to at each time step (each time thru the loop) to create a vector of values
## this vector tracks how the value changes over time so we can look at it or plot it later 

## if you havent used R for plotting much before, also note the optional arguments to the plot function
## xlab and ylab should be obvious
## type = 'l' makes it a line whereas t='p' would make it points
## lwd=2 sets the line width... you could try changing it to lwd=5...
## we could also change the line type and colour with lty and col if we wanted
plot(1:maxtime , pop, type = 'l' ,xlab='time',ylab='population density',lwd=2,lty=3,col='purple')
