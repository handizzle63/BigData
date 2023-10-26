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

meta = read.csv("C://Users//Hannah//OneDrive//Documents//Statistical Analyses//BigData//systematic review data for assignment.csv",stringsAsFactors = TRUE)

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
#size seems to vary with environment, plant types, method, author and/or study. Just from the forest
#plot, which environment looks like it has the smallest effect size overall (ie closest to zero)? 
