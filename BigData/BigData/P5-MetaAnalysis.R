###################################
###                             ###
### Assignment 1: Meta-Analysis ###
###                             ###
###################################




# Clear memory ----
rm(list=ls())

library(dplyr)
library(nlme)
library(meta)

meta = read.csv("systematic review data for assignment.csv",stringsAsFactors = TRUE)

names(meta)

#number of studies = 60

unique(meta$study)

#max number of species in any one study = 5

tapply(meta$spn, meta$study, length)

test = meta%>%
  filter(study == "36")
test = meta%>%
  filter(study == "7")

#how many studies included max number of species = 11

test = tapply(meta$spn, meta$study, length)
length(which(test == 5))

# how many total results has my assistant recorded? - 181
length(unique(meta$record))

# how many different authors? - 23
length(unique(meta$author))

#which author wrote the most studies? - Y (20 studies)
which.max(tapply(meta$study, meta$author, length)) 
#note the number in console represents the position of Y and not the number of studies

#which environment had most studies? - grassland (54 studies)
which.max(tapply(meta$study, meta$environment, length))

#what method was used in most studies? - a (64 studies)
which.max(tapply(meta$study, meta$method, length))

#which author studied the fewest species - G (2 species)
which.min(tapply(meta$spn, meta$author, length))

# were the same number of reps used in each study? No
unique(meta$nreps)

#were the same number of reps used for each species within each study - yes
reps <- meta %>%
  group_by(study, spn) %>%
  summarize(nreps = n())

unique(reps$nreps)

####### STANDARDISING DATA
nrec = 181

#Forest Plot of data prior to standardisation
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
  lines(c(meta$lb[i],meta$ub[i]),c(i,i),col=meta$sig[i]+1)
}
axis(1)
points(meta$effsz,1:nrec,pch=16,cex=0.5,col=meta$sig+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)

summary(stand$sig)
summary(meta$sig)

#Next standardise the data by dividing all the columns from ‘meanbase’ to ‘ub’ (ie six columns) by the meanbase growth rate (ie: make everything relative to the mean base relative growth rate for that species)
stand = meta %>%
  mutate(s_meanbase = meanbase/meanbase)%>%
  mutate(meancon = meancon/meanbase)%>%
  mutate(effsz = effsz/meanbase)%>%
  mutate(stderr = stderr/meanbase)%>%
  mutate(lb = lb/meanbase)%>%
  mutate(ub = ub/meanbase)%>%
  glimpse()

#standardised data forest plot
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
  lines(c(stand$lb[i],stand$ub[i]),c(i,i),col=stand$sig[i]+1)
}
axis(1)
points(stand$effsz,1:nrec,pch=16,cex=0.5,col=stand$sig+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)

# really tightened the effect size


#For how many records/species was a significant difference found? - Sig (106), Non-sig (75)

(tapply(stand$study, stand$sig, length))

#For what proportion of records/species was a significant difference found?

106/181 #0.5856354

#What is the mean unweighted (but standardised) effect size across the studies? - 0.183038

mean(stand$effsz)

#What is the mean standardised effect size across the studies, when we weight by the number of reps used for that species in the study? - 0.1849566

weighted.mean(stand$effsz,stand$nreps)

#Make some forest plots using colouring and ordering to get a visual impression of whether effect
#size seems to vary with environment, plant types, method, author and/or study. 

#environment
stand = stand[order(as.numeric(stand$environment),decreasing=TRUE),]
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
  lines(c(stand$lb[i],stand$ub[i]),c(i,i),col=as.numeric(stand$environment)[i]+1)
}
axis(1)
points(stand$effsz,1:nrec,pch=16,cex=0.5,col=as.numeric(stand$environment)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(stand$environment),col=2:5,pch=16)

#plant types
stand = stand[order(as.numeric(stand$planttype),decreasing=TRUE),]
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
  lines(c(stand$lb[i],stand$ub[i]),c(i,i),col=as.numeric(stand$planttype)[i]+1)
}
axis(1)
points(stand$effsz,1:nrec,pch=16,cex=0.5,col=as.numeric(stand$planttype)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(stand$planttype),col=2:5,pch=16)

#method
stand = stand[order(as.numeric(stand$method),decreasing=TRUE),]
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
  lines(c(stand$lb[i],stand$ub[i]),c(i,i),col=as.numeric(stand$method)[i]+1)
}
axis(1)
points(stand$effsz,1:nrec,pch=16,cex=0.5,col=as.numeric(stand$method)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(stand$method),col=2:5,pch=16)

#Just from the forest plot, which environment looks like it has the smallest effect size overall (ie closest to zero)? 
# forest?

#Estimate the effect sizes for each environment by calculating the mean standardised effect for each
#environment, weighted by the number of replicates. 
for (env in levels(stand$environment)){
  print(env)
  thisss = subset(stand,environment==env)
  print(weighted.mean(thisss$effsz,thisss$nreps))
}
#"arid"  0.230221

##Now estimate the effect sizes for each environment by calculating the mean standardised effect for
#each environment, weighted by the inverse standard error. 
#Does the relative ranking of the effect sizes for the four environments change?

for (env in levels(stand$environment)){
  print(env)
  thisss = subset(stand,environment==env)
  print(weighted.mean(thisss$effsz,thisss$stderr))
}
#increased effect size except for arid which has dropped by 0.003
#forest increased by a tenth
# grassland and heath increased by 0.02

#forest smallest effect size overall = what I thought

######## Subset Significant only
with(stand,plot(stderr,effsz,col=sig+1,pch=16))

#subset
sig = subset(stand,sig)
with(sig,plot(stderr,effsz,col=sig+1,pch=16))


#Now what is the mean unweighted (but standardised) effect size across the studies?
# 0.2749598
mean(sig$effsz)

#Why is this estimate bigger than the previous estimate based on the non-significant studies as well?
mean(stand$effsz)
## effect size has been inflated by only significant studies aka. publication bias

#Estimate the effect sizes for each environment by calculating the mean standardised effect for each
#environment, weighted by the number of replicates. 
for (env in levels(sig$environment)){
  print(env)
  thisss = subset(sig,environment==env)
  print(weighted.mean(thisss$effsz,thisss$nreps))
}

#What is the estimated effect size for species in
#arid environments? 0.2921152

#For which environment has estimated effect size changed the most by excluding the non-significant
#results?
#arid
0.2921152 - 0.230221 #0.0618942
#forest
0.02366456 - 0.007307282 #0.01635728
#grassland
0.2788012 - 0.1919517 #0.0868495
#heath
0.3874327 - 0.2848183 #0.1026144 ## most changed (increased) 

##META with significant only df.
#Now try looking at the effects of environment, plant type, and method using the metareg function,
#using the data set with the non-significant results excluded.

m = metagen(effsz,stderr,data=sig)
m
forest(m)
m1 <- metareg(m,~environment+planttype+method)

m2 <- metareg(m,~environment*planttype*method)
anova(m1,m2)
#m1 better fit
m1

m3 <- metareg(m, ~environment+planttype+method+author)
m3
forest(m3)
m4 <- metareg(m, ~environment*planttype+method+author)
m4

#environment
sig = sig[order(as.numeric(sig$environment),decreasing=TRUE),]
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
  lines(c(sig$lb[i],sig$ub[i]),c(i,i),col=as.numeric(sig$environment)[i]+1)
}
axis(1)
points(sig$effsz,1:nrec,pch=16,cex=0.5,col=as.numeric(sig$environment)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(sig$environment),col=2:5,pch=16)

#plant types
sig = sig[order(as.numeric(sig$planttype),decreasing=TRUE),]
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
  lines(c(sig$lb[i],sig$ub[i]),c(i,i),col=as.numeric(sig$planttype)[i]+1)
}
axis(1)
points(sig$effsz,1:nrec,pch=16,cex=0.5,col=as.numeric(sig$planttype)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(sig$planttype),col=2:5,pch=16)

#method
sig = sig[order(as.numeric(sig$method),decreasing=TRUE),]
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
  lines(c(sig$lb[i],sig$ub[i]),c(i,i),col=as.numeric(sig$method)[i]+1)
}
axis(1)
points(sig$effsz,1:nrec,pch=16,cex=0.5,col=as.numeric(sig$method)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(sig$method),col=2:5,pch=16)

#author

sig = sig[order(as.numeric(sig$author),decreasing=TRUE),]
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
  lines(c(sig$lb[i],sig$ub[i]),c(i,i),col=as.numeric(sig$author)[i]+1)
}
axis(1)
points(sig$effsz,1:nrec,pch=16,cex=0.5,col=as.numeric(sig$author)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(sig$author),col=2:5,pch=16)

### using meta package
heath = subset(sig,environment=="heath")
m.heath = metagen(effsz, stderr, data = heath)
forest(m.heath)
