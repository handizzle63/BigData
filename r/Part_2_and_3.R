#####################################
########                        #####
########  More Population Models ####
########                        #####
#####################################

##equation given was nt+1 = g*nt*(1-nt/1000)
#n = pop
#t = time
#g = growth

## base model
fly = 7
months = 100
growth = 2


for (time in 2:months){
  fly[time] = growth * fly[time-1] * (1-fly[time-1]/1000)
}

plot(1:months, fly,
     type = "l",
     lwd = 2)



## Model to edit
fly = 349.097
months = 100
growth = 4

for (time in 2:months){
  fly[time] = growth * fly[time-1] * (1-fly[time-1]/1000)
}

plot(1:months, fly,
     type = "l",
     lwd = 2)


fly[25:35]
fly[90:100]


#######################################################
#Bacteria Populations

x <- 2 #autotrophic bacteria (kg) 
y <- 2 #heterotrophic bacteria (kg)
timestep=0.1 #each minute broken into 10 smaller timesteps
t <- seq(0,24*60,by=timestep) #total simulation is one day (24*60)

for (i in 1:length(t)){
  growthrate_x = 0.02*(1-y[i]) #growth rate is 0.02% per minute. The autotroph is eaten by the heterotroph so as the heterotroph grows the x reduces (1-y[i])
  dx=timestep*growthrate_x*x[i] #must be density increase of autotroph
  growthrate_y = 0.02*(x[i]-1) #heterotroph depends on autotroph for food, so as the autotroph pop increases so too does the heterotroph (x[i]-1)
  dy=timestep*growthrate_y*y[i] #dy must be density increase of heterotroph
  x[i+1]=x[i]+dx #biomass of x at time+1 = biomass at time + growth?
  y[i+1]=y[i]+dy
}
plot(t,x[-length(x)])
points(t,y[-length(y)],col='red')

#plot autotroph (x) against heterotroph (y)
plot(x,y)

## Heterotroph density starting at 0

x <- 2 #autotroph
y <- 0 #heterotroph
timestep=0.1 
t <- seq(0,24*60,by=timestep) 

for (i in 1:length(t)){
  growthrate_x = 0.02*(1-y[i]) 
  dx=timestep*growthrate_x*x[i] 
  growthrate_y = 0.02*(x[i]-1) 
  dy=timestep*growthrate_y*y[i] 
  x[i+1]=x[i]+dx 
  y[i+1]=y[i]+dy
}
plot(x,y)


## adding in growth rate modifier for autotroph to limit own growth

x <- 2 #autotroph
y <- 0 #heterotroph
timestep=0.1 
t <- seq(0,24*60,by=timestep) 

for (i in 1:length(t)){
  growthrate_x = 0.02*(1-x[i]/10-y[i]) #changed here
  dx=timestep*growthrate_x*x[i] 
  growthrate_y = 0.02*(x[i]-1) 
  dy=timestep*growthrate_y*y[i] 
  x[i+1]=x[i]+dx 
  y[i+1]=y[i]+dy
}
plot(x,y)

##Heterotroph initial density = 0.01


x <- 2 #autotroph
y <- 0.01 #heterotroph
timestep=0.1 
t <- seq(0,24*60,by=timestep) 

for (i in 1:length(t)){
  growthrate_x = 0.02*(1-x[i]/10-y[i]) #changed here
  dx=timestep*growthrate_x*x[i] 
  growthrate_y = 0.02*(x[i]-1) 
  dy=timestep*growthrate_y*y[i] 
  x[i+1]=x[i]+dx 
  y[i+1]=y[i]+dy
}
plot(x,y)

##two stable populations
x <- 10 #autotroph
y <- 100  #heterotroph
timestep=0.1 
t <- seq(0,24*60,by=timestep) 

for (i in 1:length(t)){
  growthrate_x = 0.02*(1-x[i]/10-y[i]) #changed here
  dx=timestep*growthrate_x*x[i] 
  growthrate_y = 0.02*(x[i]-1) 
  dy=timestep*growthrate_y*y[i] 
  x[i+1]=x[i]+dx 
  y[i+1]=y[i]+dy
}
plot(x,y)

##### Adding Species Z
x <- 2 #autotroph
y <- 2  #heterotroph
z <- 2 #eats y
timestep=0.1 
t <- seq(0,24*120,by=timestep) 

for (i in 1:length(t)){
  growthrate_x = 0.02*(1-x[i]/10-y[i]) #represents self limiting
  dx=timestep*growthrate_x*x[i] 
  growthrate_y = 0.02*(x[i]-1-z[i]) #represents z eating it, and eating x
  dy=timestep*growthrate_y*y[i] 
  growthrate_z = 0.02*(y[i]-1) #represents eating y
  dz=timestep*growthrate_z*z[i]
  x[i+1]=x[i]+dx 
  y[i+1]=y[i]+dy
  z[i+1]=z[i]+dz
}
plot(x)
which.min(x)
tail(x)
plot(y)
which.min(y)
y[24000:24500]
plot(z)
which.min(z)
tail(z)

plot(x,y,cex=z/max(z)) #points represent biomass of third species - gives 3D effect

######################## same at t & t+1
x <- 2 
y <- 2  
z <- 2 
timestep=0.1 
t <- seq(0,24*120,by=timestep) 
tolerance <- 1e-6

for (i in 1:length(t)){
  growthrate_x = 0.02*(1-x[i]/10-y[i]) 
  dx=timestep*growthrate_x*x[i] 
  growthrate_y = 0.02*(x[i]-1-z[i]) 
  dy=timestep*growthrate_y*y[i] 
  growthrate_z = 0.02*(y[i]-1) 
  dz=timestep*growthrate_z*z[i]
  x[i+1]=x[i]+dx 
  y[i+1]=y[i]+dy
  z[i+1]=z[i]+dz
  if (abs(x[i] - x[i + 1]) < tolerance &&
      abs(y[i] - y[i + 1]) < tolerance &&
      abs(z[i] - z[i + 1]) < tolerance) {
    break  # Exit the loop if the condition is met
  }
}

### Species Z doesn't need as much food

x <- 2 
y <- 2 
z <- 2 
timestep=0.1 
t <- seq(0,24*60,by=timestep) 

for (i in 1:length(t)){
  growthrate_x = 0.02*(1-x[i]/10-y[i]) 
  dx=timestep*growthrate_x*x[i] 
  growthrate_y = 0.02*(x[i]-1-z[i]) 
  dy=timestep*growthrate_y*y[i] 
  growthrate_z = 0.02*(y[i]-0.5) #changed 1 to 0.5
  dz=timestep*growthrate_z*z[i]
  x[i+1]=x[i]+dx 
  y[i+1]=y[i]+dy
  z[i+1]=z[i]+dz
}
plot(z)
plot(x)
plot(y)
plot(x,y,cex=z/max(z))

