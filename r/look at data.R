list.files()

#### just one population
all = read.csv("all e1 gen1.csv")
head(all,20)
dim(all)
table(all$pop)
all
allag = aggregate(all[-1:-4],list(all$pop),mean)
allag
boxplot(ft2~pop,data=all)

#### now try using this code to look at other traits and other generations

#### ordination on all the genetic markers for that population
head(all[18:37])
samp = all[sample(1:nrow(all),400),]
dd = dist(samp[,18:37],method="manhattan")
### need to install vegan library if not already installed
library(vegan)
mymds=metaMDS(dd)
names(mymds)
plot(mymds$points,col=as.numeric(samp$pop),pch=as.numeric(samp$pop))
plot(mymds$points,col='white')
text(mymds$points[,1],mymds$points[,2],samp$pop,cex=0.5,col=as.numeric(samp$pop))

#### now try using this code to look at other generations


### plot trait changing over generations
rec=NULL
for (gen in 1:10){
	print(gen)
	dat = read.csv(paste("all e1 gen",gen,".csv",sep=""))
	thistraitbypop = tapply(dat$ft1,dat$pop,mean)
	rec=cbind(rec,thistraitbypop )
}
matplot(t(rec),t='l',col='black')

### plot another trait changing over generations
rec=NULL
for (gen in 1:10){
	print(gen)
	dat = read.csv(paste("all e1 gen",gen,".csv",sep=""))
	thistraitbypop = tapply(dat$ft3,dat$pop,mean)
	rec=cbind(rec,thistraitbypop )
}
matplot(t(rec),t='l',col='red',add=TRUE)

#### now try using this code to look at more generations and other traits
### eventually you probbaly need to look at all the traits and the complete time series of data
### but you can see this will be time consuming
### so you might want to sample generations across the full time series, rather than look at every generation

