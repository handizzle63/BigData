# your trusty assistant has completed a systematic review of studies
# looking at the effect of growing a plant in con-specific conditioned soil

# they have compiled a data base...  
# in the data base, there is a row or record for each experiment
# where an experiment represents one species in each paper they found
# (there were often more than one species considered in a paper/study) 
# for every species/record they have recorded the study/paper ID, the main author, the environment, 
# the method used (there are several possible methods commonly used)
# whether the species was a monocot or a dicot
# the number of reps used in the experiment
# the p-value, 
# the mean growth rate in sterile soil
# the mean growth rate in con-specific conditioned soil 
# the estimated effect size
# the standard error of this effect size
# the lower and upper bound of a 95% confidence interval on the effect size
# and whether the difference between sterile and conditioned soil was found to be significant at p<0.05

# Clear memory ----
rm(list=ls())

library(dplyr)
library(nlme)
library(meta)

# first read in the data and have a look at it
metdat = read.csv("systematic review data.csv",stringsAsFactors = TRUE)
head(metdat)
metdat
nrec=nrow(metdat) # number of rows
str(metdat)

##### for the following questions you can just look carefully at the Excel file
## or the data frame in R
## or you can try to write some R code to get the answers quickly - there are hints just below
## but don't spend too much time on these
## the idea is just to make sure you've had a good look at the data

## how many studies/papers did they find?

unique(metdat$study) # 40 studies

## how many species were studied in each study? # see spp df. 
spp <- metdat %>%
  group_by(study) %>%
  summarize(spn = n_distinct(spn))

#or

tapply(metdat$spn, metdat$study, length)

## how many total results/records/species have they recorded? 

unique(metdat$spn) #6 species

## how many different authors?
nlevels(metdat$author) #14

## how many studies by each author?
auth <- metdat %>%
  group_by(author) %>%
  summarize(study = n_distinct(study))%>%
  glimpse()

tapply(metdat$study, metdat$author, length)

## how many studies in each environment?
test <- metdat %>%
  group_by(environment) %>%
  summarize(study = n_distinct(study))%>%
  glimpse()

tapply(metdat$study, metdat$environment, length)

## how many studies with each method?
test <- metdat %>%
  group_by(method) %>%
  summarize(study = n_distinct(study))%>%
  glimpse()

tapply(metdat$study, metdat$method, length)

## how many species were studied by each author? 
test <- metdat %>%
  group_by(author) %>%
  summarize(spn = n_distinct(spn))%>%
  glimpse()

tapply(metdat$spn, metdat$author, length)

## were the same number of reps used in each study?
unique(metdat$nreps)


## were the same number of reps used for each species within each study? 

reps <- metdat %>%
  group_by(study, spn) %>%
  summarize(nreps = n())

# Check if all replicate counts are the same within each study
test <- reps %>%
  group_by(study) %>%
  summarize(sreps = all(nreps == first(nreps)))
unique(test$sreps)


### now some example R code hints to help 
##############################################
## to calculate how many studies/papers did they find:
length(unique(metdat$study))
## to calculate the number of species in each study:
tapply(metdat$spn, metdat$study, length)
## to caclulate how many studies by each author:
tapply( metdat$study,  metdat$author, function(x) length(unique(x)) )

## note that there are other correct ways to get these answers
################################################


## for how many records/species was a significant difference found?
sum(metdat$sig)
## what proportion is that?
sum(metdat$sig)/nrec

## what is the mean effect size across the studies? 
mean(metdat$effsz)

## but usually we would want to weight this to account for different confidence in different studies, eg
weighted.mean(metdat$effsz,metdat$nreps)
weighted.mean(metdat$effsz,1/metdat$stderr)
weighted.mean(metdat$effsz,1/metdat$stderr^2)


## make a 'forest plot' and check you understand what it shows? 
#lines represent each study
#studies that overlap zero and are balck have greater confidence than red lines
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
	lines(c(metdat$lb[i],metdat$ub[i]),c(i,i),col=metdat$sig[i]+1)
}
axis(1)
points(metdat$effsz,1:nrec,pch=16,cex=0.5,col=metdat$sig+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)

## make another 'forest plot' and check you understand what it shows? 
##coloured by environment 

plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
	lines(c(metdat$lb[i],metdat$ub[i]),c(i,i),col=as.numeric(metdat$environment)[i]+1)
}
axis(1)
points(metdat$effsz,1:nrec,pch=16,cex=0.5,col=as.numeric(metdat$environment)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(metdat$environment),col=2:5,pch=16)

## make another 'forest plot' and check you understand what it shows? 
## ordered the data by envronment -> grouped on the plot

metdat = metdat[order(as.numeric(metdat$environment),decreasing=TRUE),]
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
	lines(c(metdat$lb[i],metdat$ub[i]),c(i,i),col=as.numeric(metdat$environment)[i]+1)
}
axis(1)
points(metdat$effsz,1:nrec,pch=16,cex=0.5,col=as.numeric(metdat$environment)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(metdat$environment),col=2:5,pch=16)

##### what do you notice??? 

#### now try making a forest plot where species/records are ordered by the effect size, but coloured by the plant type
## what do you notice? 

metdat = metdat[order(as.numeric(metdat$effsz),decreasing=TRUE),]
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
	lines(c(metdat$lb[i],metdat$ub[i]),c(i,i),col=as.numeric(metdat$planttype)[i]+1)
}
axis(1)
points(metdat$effsz,1:nrec,pch=16,cex=0.5,col=as.numeric(metdat$planttype)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(metdat$planttype),col=2:3,pch=16)


### so we can estimate the effect size for different environments
tapply(metdat$effsz,metdat$environment,mean)

## or weight
for (env in levels(metdat$environment)){
	print(env)
	thisss = subset(metdat,environment==env)
	print(weighted.mean(thisss$effsz,thisss$nreps))
}


## or weight another way ## by stderr
for (env in levels(metdat$environment)){
	print(env)
	thisss = subset(metdat,environment==env)
	print(weighted.mean(thisss$effsz,(1/thisss$stderr)))
}

## do the weightings make a big difference here? 
# bit of a difference

##################### ASK MICHAEL
## in what environment does it seem that con-specific soil has the greatest effect on plant growth? 

## group by environment and species? 
# visualising by plot (unsure if this is necessary & couldn't get legend to work)
metdat = metdat[order(as.numeric(metdat$effsz),decreasing=TRUE),]
plot.new()
plot.window(xlim=c(-2,2),ylim=c(0,nrec))
for (i in 1:nrec){
  lines(c(metdat$lb[i],metdat$ub[i]),c(i,i),col=as.numeric(metdat$environment)[i]+1)
}
axis(1)
points(metdat$effsz,1:nrec,pch=as.numeric(metdat$spn),cex=0.5,col=as.numeric(metdat$environment)+1)
mtext('effect size',side=1,line=2)
abline(v=0,lty=3)
legend('topright',levels(metdat$environment),col=2:5,pch=16)


# printing via code without weighting
metdat = metdat[order(as.numeric(metdat$effsz),decreasing=TRUE),]
head(metdat, 1)
#sp 1, forest, efs = 0.845

## and in which environment the least? 
tail(metdat, 1)
# arid, sp 1, efz -1.059

## weighted by stderr
for (env in levels(metdat$environment)){
  print(env)
  thisss = subset(metdat,environment==env)
  print(weighted.mean(thisss$effsz,(1/thisss$stderr)))
  print(metdat, spn == env)
}

wmetdat = metdat %>%
  mutate(w.mean = weighted.mean(metdat$effsz,(1/metdat$stderr)))%>%
  glimpse()
wmetdat = wmetdat[order(as.numeric(wmetdat$w.mean),decreasing=TRUE),]
head(wmetdat, 1)
tail(wmetdat, 1)



############## publication bias...
with(metdat,plot(nreps,stderr))
### why do you see this relationship? 
# as reps increase the variance decreases (std. dev) which in turn decreases the std error


## let's plot standard error against estimated effect size # red is significant

with(metdat,plot(stderr,effsz,col=sig+1,pch=16))
## or
with(metdat,plot(nreps,effsz,col=sig+1,pch=16))

### now what would happen if none of the non-significant ones has been published?? 

## now just for a subset
with(subset(metdat,environment=="grassland"),plot(stderr,effsz,col=sig+1,pch=16))
with(subset(metdat,environment=="grassland"),plot(nreps,effsz,col=sig+1,pch=16))

### now what would happen if none of the non-significant ones has been published?? 
metdat1 = subset(metdat,sig)
with(metdat1,plot(stderr,effsz,col=sig+1,pch=16))
with(subset(metdat1,environment=="grassland"),plot(stderr,effsz,col=sig+1,pch=16))


### effect size for different environments
tapply(metdat1$effsz,metdat1$environment,mean)
## or weight
for (env in levels(metdat1$environment)){
	print(env)
	sig = subset(metdat1,environment==env)
	print(weighted.mean(sig$effsz,sig$nreps))
}

## or weight with stderr
for (env in levels(metdat1$environment)){
	print(env)
	sig = subset(metdat1,environment==env)
	print(weighted.mean(sig$effsz,1/sig$stderr))
}

#### compare with the estimates from above, where the non-significant studies were included
## or weight
for (env in levels(metdat$environment)){
  print(env)
  thisss = subset(metdat,environment==env)
  print(weighted.mean(thisss$effsz,thisss$nreps))
}

for (env in levels(metdat1$environment)){
  print(env)
  sig = subset(metdat1,environment==env)
  print(weighted.mean(sig$effsz,sig$nreps))
}

## or weight another way ## by stderr
for (env in levels(metdat$environment)){
  print(env)
  thisss = subset(metdat,environment==env)
  print(weighted.mean(thisss$effsz,(1/thisss$stderr)))
}

### are the estimated effect sizes bigger or smaller now?
# bigger

### do you understand why?? 
# estimates are bigger with just significant outcomes because effect size indicates a practical significance. Non-sig outcomes would reduce the estimated effect size 

## now look at the effect sizes for different plant types
for (env in levels(metdat$planttype)){
  print(env)
  thisss = subset(metdat,planttype==env)
  print(weighted.mean(thisss$effsz,thisss$nreps))
}

for (env in levels(metdat1$planttype)){
  print(env)
  sig = subset(metdat1,planttype==env)
  print(weighted.mean(sig$effsz,sig$nreps))
}

## or weight another way ## by stderr
for (env in levels(metdat$planttype)){
  print(env)
  thisss = subset(metdat,planttype==env)
  print(weighted.mean(thisss$effsz,thisss$stderr))
}

for (env in levels(metdat1$planttype)){
  print(env)
  sig = subset(metdat1,planttype==env)
  print(weighted.mean(sig$effsz,sig$stderr))
}

## and different methods
for (env in levels(metdat$method)){
  print(env)
  thisss = subset(metdat,method==env)
  print(weighted.mean(thisss$effsz,thisss$nreps))
}

for (env in levels(metdat1$method)){
  print(env)
  sig = subset(metdat1,method==env)
  print(weighted.mean(sig$effsz,sig$nreps))
}

## or weight another way ## by stderr
for (env in levels(metdat$method)){
  print(env)
  thisss = subset(metdat,method==env)
  print(weighted.mean(thisss$effsz,thisss$stderr))
}

for (env in levels(metdat1$method)){
  print(env)
  sig = subset(metdat1,method==env)
  print(weighted.mean(sig$effsz,sig$stderr))
}

###### to look for significance of differences among environments etc we can use the lme function in the nlme library
## you'll need to load the library before you can use it
## but I don't think you'll need to install it, as it's a standard R library that usually installs when you install R itself (?) 


### see if you can understand what each line is doing (and not doing)

fm = lm(effsz~environment,data=metdat) #linear model, y = effsz, x = env
summary(fm) #difference in calculations between summary & anova
# arid is intercept, each env is sig. different from each other
anova(fm) # sig different


fm = lm(effsz~environment,data=metdat,weights=metdat$nreps) #lm with effsz weighted by nreps
summary(fm) # all significant still, some more and some less
anova(fm) # less significant than previous - would posthoc be relevant here? 

metdat$environment
fm = lme(effsz~environment,random=~1|study,data=metdat,method="ML")
## linear mixed effects, study as RE, ML is log likelihood maximised
fm = lme(effsz~environment,random=~1|study,data=metdat)
summary(fm)
anova(fm)
## some difference between default and ML method

fm = lme(effsz~environment,data=metdat,random=~1|study,weights=~nreps,method="ML") # with RE and weights + log likelihood maximisation
summary(fm)
anova(fm)

fm = lme(effsz~environment,data=metdat,random=~1|study,weights=~1/stderr,method="ML")
summary(fm)
anova(fm)

fm = lme(effsz~planttype,data=metdat,random=~1|study,weights=~nreps,method="ML")
summary(fm)
anova(fm)

fm = lme(effsz~environment*planttype,data=metdat,random=~1|study,weights=~nreps,method="ML")
summary(fm)
anova(fm)


## there is a strong indication that environment makes a difference to effect size
## and a less strong indication that plant type makes a difference to effect size
## what about method?? 
fm = lme(effsz~method,data=metdat,random=~1|study,weights=~nreps,method="ML")
summary(fm)
anova(fm)
## method doesn't seem to be significant - ### MICHAEL double check I'm looking at the p=value from method and not intercept

## we can use base and standard R packages for meta analysis, but there are also specicialised packages that use the most up-to-date methods and
## (might) make things much easier

## meta is one of the most commonly used packages for meta-analysis
## you'll have to install it if you don't already have it installed 

## a good tutorial on using it is available here https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/

library(meta)  
head(metdat)
m = metagen(effsz,stderr,data=metdat)

m

## the output from the test is explained here https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/heterogeneity.html

## but a detailed understanding of these measures of heterogeneity is beyond the scope of this course

## for now just notice the estimates of the overall effect size are -0.2101 and -0.2325 according to two different types of model

## note that these are quite similar to the estimates we obtained previously using the base R calculations, right?


## we can get a forest plot from this package too, but not so pretty here!
forest(m)

## now try on just a subset
metdat.heath = subset(metdat,environment=="heath")
head(metdat.heath)
m.heath = metagen(effsz,stderr,data=metdat.heath)
m.heath
forest(m.heath)

metdat.arid = subset(metdat,environment=="arid")
head(metdat.arid)
m.arid = metagen(effsz,stderr,data=metdat.arid)
m.arid
forest(m.arid)

## are the estimated effect sizes in these environments similar to what we got previously? 
### try that for the other enviroments... do you get similar estimates for effect sizes in different enviroments as we got previously? 
metdat.forest = subset(metdat, environment == "forest")
head(metdat.forest)
m.forest = metagen(effsz, stderr, data = metdat.forest)
m.forest
forest(m.forest)

unique(metdat$environment)
metdat.grass = subset(metdat, environment == "grassland")
head(metdat.grass)
m.grass = metagen(effsz, stderr, data = metdat.grass)
m.grass
forest(m.grass)

## this metareg function lets us look at and test whether/how effect size depends on other factors...
metareg(m,~environment)

## note that the table at the end gives the estimated effect size for the arid group, and then estimated differences for the other groups
## do these match previous estimates? 
metareg(m,~environment+planttype)
metareg(m,~environment+planttype+method)
metareg(m,~environment*planttype*method)
metareg(m,~environment+planttype+method+author)

### or this function does the estimates for the environments even more easily
update.meta(m, 
            byvar = environment, 
            tau.common = FALSE)

## which is the same as this
metagen(effsz,stderr,byvar = environment,data=metdat)


### there's a lot of detail in the meta functions output that are beyond the scope of this course
### but you should at least be able to see (or work out) the effect size estimates for the different groups, with means and confidence bounds? 
## can you see that we get similar estimates for the effect sizes in the different environments with this function? 

 
############ if you want to know more about meta-analysis with R then check this website 
###  https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/   
### but what's covered in this script is enough for this course


##### now go through and repeat all the analyses using lme and meta on the data set where non-significant results were NOT included
### what differences do you find? 
### do these make sense?? 

lm1 = lme(effsz~method,data=metdat1,random=~1|study,weights=~nreps,method="ML")
summary(lm1)
anova(lm1)

lm2 = lme(effsz~environment, data = metdat1, random =~1|study, weights = ~nreps, method = "ML")
summary(lm2)
anova(lm2)

m2 = metagen(effsz, stderr, data=metdat1)
m2
forest(m2)

