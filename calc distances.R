## Calculating Distances between trees in town
## for simulations later on

load('BigData/BigData/model w males 2020/ntrees')
tinds = which(ntrees>0,arr.ind=TRUE)
tsize = ntrees[tinds]
table(tsize)
plot(tinds,cex=0.1,col=rgb(0,0,0,alpha=0.1*tsize),pch=16)

ntreecells = dim(tinds)[1]
tinds2 = floor(tinds/50)
plot(tinds2)

alltrees = data.frame(x=tinds[,1],y=tinds[,2],x2=tinds2[,1],y2=tinds2[,2],id=1:ntreecells,size=tsize)
head(alltrees)
save(alltrees,file='alltrees')

#### record the 400 closest for each tree
allcloseones=list()
for (i in 1:ntreecells){
	print(i)
	dists = with(alltrees,sqrt( (x-x[i])^2 + (y-y[i])^2 )*10)
	dists[i] = 9999999999
	closeones = data.frame(id=order(dists)[1:400],dist=dists[order(dists)][1:400])
	allcloseones[[i]] = closeones 
}
save(allcloseones,file='allcloseones')
load('allcloseones')
##check
i=49000
i=900
aco=allcloseones[[i]]
plot(alltrees[aco$id,]$x, alltrees[aco$id,]$y,cex=0.01)
text(alltrees[aco$id,]$x, alltrees[aco$id,]$y,1:400,cex=0.6)
text(alltrees[aco$id,]$x[1:200], alltrees[aco$id,]$y[1:200],1:200,cex=0.6)
points(alltrees[i,]$x, alltrees[i,]$y,col='red')
points(alltrees[aco$id,]$x[1:3], alltrees[aco$id,]$y[1:3],col='green')

































