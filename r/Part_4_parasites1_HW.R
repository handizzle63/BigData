### parasites

#### parameter values
r <- 0.4
a <- 0.1
Hmr <- 0.1
Pmr <- 0.9
totaltime <- 400
worldsize <- 100

pauseTime <- 0

### set up 'world'
N <- matrix(rep(0,worldsize*worldsize ),nrow=worldsize )
P <- matrix(rep(0,worldsize*worldsize ),nrow=worldsize )

### starting population
N[6,6] <- 200  ##200 host individuals at location 6,6 in world map
N[60,50] <- 2 # 2 individuals at location 60,50
P[60,50] <- 10 ## 10 parasite individuals at location 50,60 in world map

### growth functions
host <- function(N,P) N*exp(r-a*P)
parasite <- function(N,P) N*(1-exp(-a*P))
#host <- function(N,P) N*exp(r*exp(rnorm(1,0,rvar))-a*P)

## edge functions
worldsize2 <- worldsize+2
worldsize1 <- worldsize+1

host.edges <- function(N){
Hedges<-matrix(rep(0,worldsize2^2),nrow=worldsize2)
Hedges[2:worldsize1,2:worldsize1]<-N
Hedges[1,2:worldsize1]<-N[worldsize,]
Hedges[worldsize2 ,2:worldsize1]<-N[1,]
Hedges[2:worldsize1,1]<-N[,worldsize]
Hedges[2:worldsize1,worldsize2 ]<-N[,1]
Hedges[1,1]<-N[worldsize,worldsize]
Hedges[worldsize2 ,worldsize2 ]<-N[1,1]
Hedges[1,worldsize2 ]<-N[worldsize,1]
Hedges[worldsize2 ,1]<-N[1,worldsize]
Hedges}

parasite.edges <- function(P){
Pedges<-matrix(rep(0,worldsize2^2),nrow=worldsize2)
Pedges[2:worldsize1,2:worldsize1]<-P
Pedges[1,2:worldsize1]<-P[worldsize,]
Pedges[worldsize2 ,2:worldsize1]<-P[1,]
Pedges[2:worldsize1,1]<-P[,worldsize]
Pedges[2:worldsize1,worldsize2 ]<-P[,1]
Pedges[1,1]<-P[worldsize,worldsize]
Pedges[worldsize2 ,worldsize2 ]<-P[1,1]
Pedges[1,worldsize2 ]<-P[worldsize,1]
Pedges[worldsize2 ,1]<-P[1,worldsize]
Pedges}

## define the neighbourhood
nhood<-function(X,j,i) sum(X[(j-1):(j+1),(i-1):(i+1)])

## migration functions
h.migration<-function(Hedges){
Hmigs<-matrix(rep(0,worldsize^2),nrow=worldsize)
for(a in 2:worldsize1){
for(b in 2:worldsize1){
Hmigs[a-1,b-1]<-nhood(Hedges,a,b)
}}
Hmigs}

p.migration<-function(Pedges){
Pmigs<-matrix(rep(0,worldsize^2),nrow=worldsize)
for(a in 2:worldsize1){
for(b in 2:worldsize1){
Pmigs[a-1,b-1]<-nhood(Pedges,a,b)
}}
Pmigs}

mousedown <- function(buttons, x, y) {
        NULL
    }


record <- c("time","H","P")
## run the simulation
for (t in 1:totaltime){
print(paste(t,sum(N)))

he <- host.edges(N)
pe <- parasite.edges(P)

Hmigs <- h.migration(he)
Pmigs <- p.migration(pe)

N<-N-Hmr*N+Hmr*Hmigs/9
P<-P-Pmr*P+Pmr*Pmigs/9

Ni<-host(N,P)
P<-parasite(N,P)
N<-Ni

thisrecord <- c(t,N[60,50],P[60,50])
record <- rbind(record,thisrecord)
view <- N/(50+N)
image(1:worldsize,1:worldsize,view,xlab=paste('time:',t),ylab="",zlim=c(0,1))
for (i in 1:pauseTime){ff<-10}
}
write.csv(record,"results.csv",row.names=FALSE)

plot(as.numeric(record[2:dim(record)[1],1]),
     as.numeric(record[2:dim(record)[1],2]),
     t='l',
     xlab='time',
     ylab='density')
lines(as.numeric(record[2:dim(record)[1],1]),
      as.numeric(record[2:dim(record)[1],3]),
      col='blue')

legend("topleft",
       title= "Species",
       legend= c("Host","Parasite"), #levels
       col= c("black","blue"), #specify colours in order
       pch= 19, 
       bty="n")


