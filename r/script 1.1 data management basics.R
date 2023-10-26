################################ some basics
## check you understand what is happening in each case here

##loops and indexing

somenames <- c('anne','betty','chris','don')
ages <- c(2,4,9,4)

for (age in ages){
print(age)
}

for (i in 1:length(ages)){
print(paste(i,':',somenames[i],"is",ages[i]))
}
#assigns the person to the age as per their location in the column (i.e. first is 2 in ages and
# first in names is anne = anne is 2)

## paste

paste(somenames,'is',ages) # prints these associations (anne = 2, etc.) in the console
#with is between them

### sapply
sapply(ages,sqrt) # square roots the ages and prints in console

sapply(1:4, function(i) paste(i,somenames[i],ages[i])) #i will go from 1 to 4
# and then print the name and ages from first to last as defined in the object

### there is another R function lapply that works much like sapply
### sapply converts its output to a vector, which is usually what you want
### lapply does not, so it returns a list, which may occasionlly be what you want
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
##### now lets make a data frame showing the number of pets of each type owned by some different people
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

#### created a dataframe where 6 individuals and their pet types are tallied

#### now enter each line below, one by one, and check carefully that you fully understand what each line has done (calculated and/or shown)

justcounts = petdata[,-1]#removes the first column 'name'
justcounts 
rowSums(justcounts) #sums the number of pets owned by a person (by row)
colSums(justcounts) #sums the number of each pet type owned
pettots = colSums(justcounts) #created object of the column sums
pettots 
which.max(pettots) #prints which pet type occurs most - that also occurs across the most individuals? 
pettots[which.max(pettots)] #prints the pet type that occurs most frequently across owners?
which(pettots==max(pettots)) #prints the two types that are most abundant 
pettots[which(pettots==max(pettots))] #prints the type and occurrence (18 cats, 18 rats)
which.min(pettots) #prints least frequent pet type
pettots[which.min(pettots)] #prints occurence of least frequent pet type

rowSums(justcounts>0)# prints the number of columns per row that have values >0

## now can you write a line of code that tells you who has the most different kinds of pets?

owners =  c('max','bob','bobbette','kris','joce','ruthy')
a = rowSums(justcounts>0) 
owners[which(a == max(a))] 

## and next can you write a line of code that tells you which kind of pet is owned by the most people?

b = colSums(justcounts>0)
which.max(b)


colstouse = rainbow(7) #create object specifying colours
b=barplot(t(as.matrix(justcounts)),col=colstouse )
axis(1,at=b,petdata$name)
legend('topleft', colnames(justcounts ),fill=colstouse )

### TO DO
## can you make that graph as pretty and informative as the one above? with pets on the x axis
# and colour per owner with legend

