############ CREATING SAMPLING DESIGNS ON A GRID

#1. Grid Design, 100 Traps

df=data.frame(expand.grid(x=seq(50,1000,by=100),y=seq(50,1000,by=100)))
df
write.csv(df,'design1.csv')

# Clumped Design designr1
df=data.frame(x=runif(100)*100+500,y=runif(100)*100+500)
df
write.csv(df,'designr1.csv')


# Random Expert design (designr)
df=data.frame(x=runif(100)*1000,y=runif(100)*1000)
df
write.csv(df,'designr.csv')

# Grid Design, Reduced Density (25)

df=data.frame(expand.grid(x=seq(100,1000,by=200),y=seq(100,1000,by=200)))
df
write.csv(df,'design_rd25.csv')

# Random 1
set.seed(123)
n <- 100
x <- runif (n, min = 0, max = 1000)
y <- runif (n, min = 0, max = 1000)

df=data.frame(x = x, y = y)
df
write.csv(df,'design_rand1.csv')

#Random 2
set.seed(132)
n <- 100
x <- runif (n, min = 0, max = 1000)
y <- runif (n, min = 0, max = 1000)

df=data.frame(x = x, y = y)
df
write.csv(df,'design_rand2.csv')

#Random 3
set.seed(111)
n <- 100
x <- runif (n, min = 0, max = 1000)
y <- runif (n, min = 0, max = 1000)

df=data.frame(x = x, y = y)
df
write.csv(df,'design_rand3.csv')

#efficient?
set.seed(111)
n <- 100
x <- runif (n, min = 100, max = 900)
y <- runif (n, min = 200, max = 700)

df=data.frame(x = x, y = y)
df
write.csv(df,'design_eff.csv')

#inefficient

df=data.frame(expand.grid(x=seq(150,850,by=75),y=seq(200,950,by=80)))
df
write.csv(df,'design_ineff.csv')


