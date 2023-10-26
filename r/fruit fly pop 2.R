## estimation on the number of female flies likely to leave an infested fruit
## tree on any given day after the first female fly arrives at the tree


allpleave=list()
for (ntrees in c(1:10)){

pop=data.frame(ls=4,ttg=5) #four stages of life cycle, time to grow = 5 - between each stage?
# 1 egg, 2 grub, 3 pupa, 4 fly
meandaysinls = c(4,15,13,20) 
femeggsperday = 2

arec=NULL
leaverrec=NULL
resources = 250*ntrees
for (t in 1:120){
	pop$ttg=pop$ttg-1
	pop$ls[pop$ttg<1]=pop$ls[pop$ttg<1]+1
	pop$ttg[pop$ttg<1]=meandaysinls[pop$ls[pop$ttg<1]]
	nnew = sum(pop$ls==4)*femeggsperday 
	if (nnew>resources){
		nnew=resources
	}
	resources = resources - nnew
	newpop = data.frame(ls=rep(1,nnew), ttg = rep(meandaysinls[1],nnew) )
	pop=rbind(pop,newpop)
	pop=subset(pop,ls<5)
	nleavers = 0
	if (resources==0) {
		nleavers = length(pop$ls[pop$ls==4])
		pop=subset(pop,ls<4)
		}
	leaverrec=c(leaverrec,nleavers)
	arec=c(arec,dim(subset(pop,ls==4))[1])
}

plot(leaverrec,ylab='N female flies leaving',xlab='days',type='b')
points(arec,col='red')

pleave = leaverrec+arec*0.05
pleave[1:10]=0
#pleave[pleave==0]=0.001

allpleave[[ntrees]]=pleave
}
save(allpleave,file='allpleave')



##########################################
## plot spread predictions
## ps=dcauchy(0:1000,0,100)
## plot(ps,t='l',xlab='distance (m)',ylab='probability',col='red',lwd=2)

###### plot population predictions
ntrees=10

plot(allpleave[[ntrees]]+0.0001,log='y',ylab='Expected Number',xlab='days',type='b',ylim=c(0.1,500))















