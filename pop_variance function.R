sample<-c(2,3,5,6,9)
mean(sample)
#mean = 5
sd(sample)

#sample sd = 2.74

se<- 2.5/sqrt(5)
se

5+(1.96*se)
5-(1.96*se)

pop.var <- function(x) var(x) * (length(x)-1) / length(x)
pop.sd <- function(x) sqrt(pop.var(x))
pop.sd

pop.var(sample)
pop.sd(sample)

abs(qt(.05/2,32))


x<-c(3,4,5)
y<-c(5,6,7)
t.test(x,y)
