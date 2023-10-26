################################ some basics
## check you understand what is happening in each case here

##loops and indexing

somenames <- c('anne','betty','chris','don')
ages <- c(2,4,9,4)

#loops through the vector 'ages' printing the age from each line
for (age in ages){
print(age)
}


for (i in 1:length(ages)){
print(paste(i,':',somenames[i],"is",ages[i]))
}
#assigns the person to the age as per their location in the column (i.e. first is 2 in ages and
# first in names is anne = anne is 2)


paste(somenames,'is',ages) # prints these associations (anne = 2, etc.) in the console
#with is between them

### sapply
sapply(ages,sqrt) # square roots the ages and prints in console

sapply(1:4, function(i) paste(i,somenames[i],ages[i])) #i will go from 1 to 4
# and then print the name and ages from first to last as defined in the object


### there is another R function lapply that works much like sapply
### sapply converts its output to a vector, which is usually what you want
### lapply does not, so it returns a list, which may occasionally be what you want

lapply(ages,sqrt)
lapply(1:4, function(i) paste(i,somenames[i],ages[i]))

# prints in console as a list (long form) rather than wide form (sapply)

### table
table(ages)
table(c('a','b','c','a','b','b')) # two a's, three b's and a c printed into a table below

## which and max/min
max(ages)
which(ages==max(ages)) # prints 3 as the age 9 is the third value in the object
somenames[which(ages==max(ages))] # chris is the oldest (9)
(oldest <- somenames[which(ages==max(ages))])   ## the parantheses around it just makes it print the result as well as create the vector
print(paste(oldest,'is the oldest')) #chris has been defined as oldest in the vector
#prints 'chris is the oldest'



#################################
##### Pet dataframe
# number of pets owned by different people
petdata = data.frame(
	name = c('max','bob','bobbette','kris','joce','ruthy'),
	dogs = c(1,3,4,0,0,2),
	cats = c(1,1,1,7,8,0),
	rats = c(0,0,0,0,0,18),
	guineapigs = c(1,2,3,0,0,0),
	birds = c(0,0,0,0,0,2),
	stickinsects = c(6,0,0,3,0,0),
	monkeys = c(0,0,1,0,0,0)
)
petdata 
#e.g. max has a dog, cat, guinea pig and 6 x stick insects


justcounts = petdata[,-1]#removes the first column 'name'
justcounts 

rowSums(justcounts) #sums by row

colSums(justcounts) #sums by column

pettots = colSums(justcounts) #created object of the column sums
pettots 

which.max(pettots) #prints the column name and position (2) with the max that occurs first (cats comes before rats)
pettots[which.max(pettots)] #prints max in the df and what that max number is
which(pettots==max(pettots)) #prints all types that are the max and their position
pettots[which(pettots==max(pettots))] #prints the type and occurrence (18 cats, 18 rats)
which.min(pettots) #prints least frequent pet type and position in df (7th column)
pettots[which.min(pettots)] #prints occurrence of least frequent pet type (1 x monkey)

justcounts
rowSums(justcounts)
rowSums(justcounts>0)# prints the number of pet types owned by each owner. e.g.
# max owns 4 different types of pets (or the sum of columns where a value appears that is >0 by row)
petdata

## now can you write a line of code that tells you who has the most different kinds of pets?

owners =  c('max','bob','bobbette','kris','joce','ruthy')
a = rowSums(justcounts>0) 
a
owners[which(a == max(a))] 

## and next can you write a line of code that tells you which kind of pet is owned by the most people?
which.max(colSums(justcounts>0))

# below also works: 
# b = colSums(justcounts>0)
# which.max(b)


colstouse = rainbow(7) #create object specifying colours
b=barplot(t(as.matrix(justcounts)),col=colstouse )
axis(1,at=b,petdata$name)
legend('topleft', colnames(justcounts ),fill=colstouse )

### TO DO
## can you make that graph as pretty and informative as the one above? with pets on the x axis
# and colour per owner with legend
pet <- read.csv('petcsv.csv')
pet

custcol <- rainbow (6)
ggplot(pet, aes(x = pet, fill = name))+
  geom_histogram(stat = 'count')+
  theme_minimal()
