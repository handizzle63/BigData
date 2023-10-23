#########################################################
#   									  #
#  -- DAY ONE SCIE4402  ¸.·´¯`·.´¯`·.¸¸.·´¯`·.¸><(((º>  #
#									  #
#########################################################


##  Save your own copy of this script and work on your copy 
## (and do that for all the example scripts from now on)

##  You can open a new script using File --> New Script 
 
##  Enter each line using control-R, but make sure you understand 
##  what the command is actually doing and the output
##  Remember, ask a lab demonstrator if anything is not clear


##  Do try to write work through the extra things that are asked during the script
##  and try out anything else you like as well 

##  It is a very good idea to add comments in the script to remind yourself what you're doing when you come back later
##  Comments are lines like this - with a hash # (or two or ten) at the start

############################################################
### guided t-test example
### see lecture for detailed explanantion

#  For this example we will type out data directly into R
#  We will call the two groups of measurements we are comparing 'a' and 'b'

#  Enter our first sample and look at what we entered

a <- c(3.6, 4.2, 5.7, 4.0, 2.1, 4.7, 3.3, 5.9)
a

#  Enter our second sample and look at what we entered

b <- c(4.9, 5.7, 6.0, 4.3, 2.3, 5.6, 3.1, 7.1)
b

#  We can perform math operations on the data we enter
#  Look at the what happens when you run the three lines below

b+1  #adds 1 to each sample within object
a+b  ##adds a to b per row in samples
a*b  #multiples a by b per row

##  Do you see what happened in each case?
##  Are there any other operations that you can think of?
##  Try a few different alternatives and see what happens

#  Having typed our data into R we can also get summary information about our sample

mean(a)
## what is the mean of b??
mean(b)
sd(a)
sd(b)

## what is the standard deviation of b??

###########  Visual representation of our data 

#  You may have made some changes to 'a' and 'b'
#  Let's enter the original values back again to make sure we all have the same values

a <- c(3.6, 4.2, 5.7, 4.0, 2.1, 4.7, 3.3, 5.9)
b <- c(4.9, 5.7, 6.0, 4.3, 2.3, 5.6, 3.1, 7.1)


# The simplest plot option we have is 

plot(a)

# Can you match the values in the plot to those in the list of values for 'a'?
# Can you tell what the x-values used are?

# Y values are the sample measurements
# X values are the samples position by row in the dataset

# Can you get a plot of the 'b' data?

plot(b)

#  Can you match the values in the plot for 'b' with those on the list of values
#  for 'b'?

#   Note what R has done for the x- and y-axis labels here 

plot(a,b)
# plot(x axis, yaxis)

#  Look at the plot and look at what other information has been added?

#  Our formal t-test is going to rely on information about the mean and variance 
#  (or SD) of the sample. The classic visual version of the t-test
#  is to look at a box-plot.  A box plot uses the 5-number summary
#  We can get a 5-number summary as follows

summary(a)

#  and we can plot 5-number summary information using 

boxplot(a)

#  Can you see the relationship between the summary values 
#  and different parts of the boxplot?

# whiskers are min and max
# 1st IQR is bottom of box (but slightly diff due to diff formula)
# 3rd IQR is top of box (but slightly diff due to diff formula)
# mean is bar in the centre

#  Note that the inter-quartile range used in the boxplot is NOT the same  
#  as given in the summary - they use a slightly different formula

#  Try and draw a box plot for the 'b' sample 
#  Don't worry about calling the summary information details

boxplot(b)
summary(b)

## A great feature of an R boxplot is that it automatically picks up issues like outliers in the sample

#  Read in the extra sample 'd' below and then create and look at the boxplot 

d <- c(3.6, 4.2, 3.7, 4.0, 2.1, 4.7, 3.3, 7.9)

#  Is it clear to you what is going on with the plot

#  Recall we want to compare our two samples in a picture
#  We have an number of options to do this
#  They get increasingly complex.....

# (i) side-by-side plots

#  We change the plot window so it will show two figures
#  The first line below creates a grid of one row and two columns
#  The two plots are then pasted side-by-side

par(mfrow=c(1, 2))
boxplot(a)
boxplot(b)

#  This is OK, but remember we are relying on visual perspectives of the data
#  Look at the scale in each plot. Do you see any issues in terms of visual comparisons
#  of the samples?

#  Let's reset the plot space to show a single plot
#  We do this by specifying a grid with one row and one column

par(mfrow=c(1, 1))

# (ii) combining the data into a new 'dataframe' 

#  The following line says, take the set of numbers that are sample 'a' and the set of numbers 
#  that are sample 'b' and bind them together in column format

c <-cbind(a,b)
c

boxplot(c)

#  We now have both samples at the same scale
#  With no consideration of statistics -- which after all are always dodgy :) -- do you think
#  these two samples are different 

#  Note: if we wanted to bind the two samples together as rows we would use rbind

e <-rbind(a,b) # wide format
e

boxplot(e)

#  Can you understand what's happended here?

# (iii) The power of R 

#  When reading the above line think Darth Vader.... 

# First, check the maximum value in each sample

max(a) 
max(b)

#  From this we know that our vertical axis needs to go to about eight.  Why?
#   Largest sample is 7.1 and we want a bit of room 

#  For the horizontal axis we need to have the start, the end, plus enough room 
#  to display this information, so we set the x-axis from (0,3)
#  0 and 3 define the ends, with 1 and 2 left for us as spaces we can put out boxplots 

#  We can also add colours to our box plot which we will do

#  We now add a boxplot for sample a.
#  Try and make sense of the details, you can work it out if you think it through a bit

boxplot(a,
        xlim=c(0,3),
        at=1, #at position 1 along x axis
        ylim=c(0,8),
        col='red')

#  Now let's try and add sample b to our box plot
#  If we think about it we just want to add a figure so
#  A logical first guess might be as follows:

boxplot(b,
        at=2,
        col='cyan') 

#  What happened? we didn't x and y lim the axes

#  Now let's set the plot space up again, and then have another try

boxplot(a,xlim=c(0,3),at=1,ylim=c(0,8),col='red')

#  To add to the plot space we need some extra detail in our directions for R
#  We use the below line:

boxplot(b,
        add=TRUE,
        at=2,
        col='cyan')

#  What was different ?

#  Now think about our plot space.

#  Can you add the 'a' sample information at point 0 and the 'b' sample information at 
#  point 3 on the horizontal axis?  
#  Try some different colours, say 'grey' and 'dark grey'  -- useful if printing B&W only

#  Once you have done this, does the way that the plot space was defined now make sense?

#  Let's get our nice base plot back once more

boxplot(a,xlim=c(0,3),at=1,ylim=c(0,8),col='light grey')
boxplot(b,add=T,at=2,col=' grey')

#  We want to add some label information to the sample to make out plot complete

#  To do this we need to define 
#  (i)   what text we want to show
#  (ii)  which axis to display the text on
#  (iii) where, on a given axis we want to put the text
#  (iv)  how big we want the text to be

#  The way we do this is to use

axis(1,labels=c('a','b'),at=1:2,cex.axis=2)

#  Below are some additional options for adding text
#  Try and make sure you understand what each line does


axis(3,labels=c('nope', 'a', 'b', 'bloop'),at=0:3)

axis(1,labels=c('test'),at=0,cex.axis=1.1)
axis(1,labels=c('test 2'),at=3,cex.axis=1.5)

axis(2,labels=c('test 3'),at=3,cex.axis=1.5)
axis(3,labels=c('test 4'),at=2.5,cex.axis=1.3)
axis(4,labels=c('test 5'),at=1.6,cex.axis=1.4)

# OK, that was a fun excursion into R graphics
# to give you just a little taste of what is possible


##  Returning to the issue of data analysis

#  In practical terms we will conduct a t-test to look for differences
#  Based on the outcome of the test we will form a view
#  about whether it is likely or unlikely that the two samples 
#  are drawn from the same underlying population

#  In R nothing could be simpler than conducting a t-test

#  Before actually conducting the test can you write a null hypothesis
#  and an alternate hypothesis?
#  Null = There is no difference in means between a and b

#  To compare the means of the two samples we use:

t.test(a,b)

#  work through all the summary information of the test

#  (i)  what is the test p-value and what does this mean: 
## 0.3527 means there is a 35% chance that these two samples are not different 
#   (ii) what is the 95 percent confidence interval and what does this mean
## the true mean of the population lies between -2.226 and 0.851
#  (iii) in terms of your hypothesis what do you conclude?
# accept null hypothesis. There is no difference between populations

##  note that the degrees of freedom is NOT a whole number as you might expect for a t-test
##  this is because the default R t-test does not assume equality of variance, and this version
##  of the t-test 'loses' some part of a degree of freedom to estimate variances 

#  We can look up the details of the t-test formula in R using

help(t.test)

#  read this material and see how you go

#  In practice no-one is going to complain if you always just use a t-test
#  where you do not assume equal variance,
#  but you can test whether or not this is a reasonable assumption


#  To decide whether the assumption of equal variance is reasonable 
#  we conduct a variance ratio test
#  If the variance of sample 'a' and 'b' are the same then 
#  if we divide one variance by the other the ratio should be close to one

#  The is no rule about which way the division goes
#  So we could use var(a)/var(b)
#  or we could use var(b)/var(a)

var(a)/var(b)
var(b)/var(a)
# unequal variance

#  what values fo you get?

#  to determine whether these values are statistically different from one we use a formal test

#  Note that regardless of the way we run the test it will give the same p-value
#   confidence intervals will be different though,
#  but remember as long as the CI crosses one we do not reject the null of equal variance.
#  So even though we will have different CIs, the decision rule is the same

var.test(a,b)
var.test(b,a)

#  So based on either the CI or the p-value we can conclude that it is OK
#  to use the equal variance assumtion
#  ie the ratio is NOT significantly different to 1 because p>0.05
#  we re-run the t-test as:

t.test(a,b,var.equal=TRUE)
t.test(a,b, var.equal = F)

#  note that the degrees of freedom is now a whole number as this is the 
#  'traditional t-test that assumes equality of variance
#  did anything change...  not really much 

#  the CI shrinks slighly and in most applications the difference is slight
#  you do gain 0.7 of a degree of freedom which means things are just a little more
#  precise, but not that much

####################################################################
#
#               ,@@@@@@@,
#       ,,,.   ,@@@@@@/@@,  .oo8888o.      i said a t-test not a tree-test...... 
#    ,&%%&%&&%,@@@@@/@@@@@@,8888\88/8o
#   ,%&\%&&%&&%,@@@\@@@/@@@88\88888/88'
#   %&&%&%&/%&&%@@\@@/ /@@@88888\88888'
#   %&&%/ %&%%&&@@\ V /@@' `88\8 `/88'
#   `&%\ ` /%&'    |.|        \ '|8'
#       |o|        | |         | |
#       |.|        | |         | |         source:  http://ascii.co.uk/art/tree
#    \\/ ._\//_/__/  ,\_//__\\/.  \_//__/_
#
#####################################################################

#  There are other forms of the t-test

#  Suppose that sample 'a' are all values for the grape yield per ha for the
#  the base trellis system and the 'b' sample is the yield per ha
#  for a new fancy pants trellis system

#  In this case i don't want to know whether the yield is different
#  but whether the yield for sample 'b' is higher than 
#  the yield for sample 'a'.  
#  I only care if the new trellis system is an improvement
  
#  Before we even start our test we check the sample means

mean(a)
mean(b)

#  So yes, the mean yield for the new trellis system is higher,
#  but could this difference be just due to chance?

#  the test could be framed a number of different ways
#  Look at the the two versions of the test below and make sure you understand what they are asking

#  Because we are using the same data as above and i have already established that the
#  equal variance assumption is OK i add this to the formula also

t.test(a,b,alternative = "less", var.equal=TRUE)
t.test(b,a,alternative = "greater", var.equal=TRUE)

#  regardless of the way you set up the test what do you conclude?
# accept null hypothesis of not different

#  Now look back at the original t-test

t.test(a,b,var.equal=TRUE)

#  Can you see the relationship in the p-values?
#  Even though we still conclude there is no difference
#  by thinking through the test structure can you see what has happened to the p-value?

#  This is why we ask you to think through design and approach so much



##########################################################			        
#     __ {_/         _ {_/          
#     \_}\\ _         \_}\\      The grape; a very special fruit.....
#        _\(_)_          _\(_)_
#       (_)_)(_)_       (_)_)(_)_
#      (_)(_)_)(_)     (_)(_)_)(_)
#       (_)(_))_)       (_)(_))_)   
#        (_(_(_)         (_(_(_)
#         (_)_)           (_)_)
#    jgs   (_)             (_)     source:  http://ascii.co.uk/art/grapes      
#
###################################################################


#  The final version of the t-test is the paired t-test
#  If there really is a true difference between the sample means
#  and the experiment has been set up to allow a before and
#  after type test, then a paired t-test will have greater power to detect
#  the existence of true difference if they really exist 


#  Assume sample 'a' is the fat content in grams of salmon
#  prior to being fed a new type of fishmeal.
#  Let us track each individual salmon and say that
#  sample 'b' is the fat content measurement on each matching fish
#  three days later
#  By having repeat measures on matching fish
#  we have reduced the variability in the experiment


#  when we have a paired t-test we might look at the data in a different way
#  a box plot is no longer the best visual representation on the data

#  before moving to a paired t-test our first step is to look at the data

## we plot the data using

plot(a,b)

#  to see if one measurement is higher than the other we 
#  then add a 45 degree line 

abline(0,1)

#  the first value in the abline() formula is the intercept
#  the second value in the abline() formula is the slope
#  so abline(0,1) adds a line that goes through the origin 
#  and has a slope of one


#  We might revise our default plot to be cleaner
#  and more symmetric

plot(a,b,xlim=c(0,8), ylim=c(0,8), pch='x')
abline(0,1,lty=2)


#  does it look like the value for 'a' is consistently bigger 
#  than for b when paired like this?


#  To formally test whether this is the case we use a paired t-test

#  The paired t-test works on the difference series so there
#  is no need to specify equal variances
#  -- there is only one sample for testing, the difference sample!

t.test(a,b,paired=TRUE)

#  what does the test say?
# null is no difference in fish meals
# p=0.01 therefore reject null hypothesis and conclude that fish meal type has an effect
#  Has the fish meal has an effect?

#  If we want, we can conduct a paired t-test as a single sample t-test
#  Working through the steps might help you recall the process of the test
#  First we create a new data series, the difference series

f <- a-b
f

#  As you can see 7 out of 8 values are negative
#  this say in 7 out of 8 cases, the fat content of the fish
#  increased after we changed the fishmeal
#  look back at you plot.  based on the figure is this what you would expect?

#  we then conduct a single sample t-test on this difference series as

t.test(f)

#  Note we get the same result as for the paired t-test

#  try reversing the order of 'a' and 'b' in the t-test formula
#  what do you find ?

g <- b-a
g
# the inverse of f
#####################################################
#
#                        ,--,_
#                 __    _\.---'-.
#                 \ '.-"     // o\
#                 /_.'-._    \\  /  ... have you ever noticed that everything comes with a 
#                        `"--(/"`         a used by date, even tinned pineapple ... bloop...  
#                       _,--,
#
######################################################



#################################################################

##  This next section takes you through some common errors you will come across

##  (i)   for each example below work out why each line gives an error?
##  (ii)  how would you fix them?
##  (iii) note the error message in each case, so you can learn for next time you see a similar one...

a <- c(3.6, 4.2, 5.7, 4.0, 2.1, 4.7, 3.3, 5.9))

a <- c(3.6, 4.2, 5.7, 4.0, 2.1, 4.7, 3.3, 5.9

a <- c(3.6, 4.2, 5.7, 4.0, 2.1. 4.7, 3.3, 5.9)

a <- c(3.6, 4.2, 5.7; 4.0, 2.1, 4.7, 3.3, 5.9)

a <- (3.6, 4.2, 5.7, 4.0, 2.1, 4.7, 3.3, 5.9)

std(a)

Mean(a)

ttest(a,b)

t.test(A,B)

################################################
#
#                  .' '.            __
#         .        .   .           (__\_
#          .         .         . -{{_(|8)  ... i hear footsteps getting louder...
#            ' .  . ' ' .  . '     (__/`                 ... all that money wants...
#
##################################################


