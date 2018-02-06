
# This script models the analysis of two categorical variables.
# I am focusing on a binary response variable in this example.

# I first load the titanic RDATA file. (Note that the Titanic Data
# are available in the R data sets.)

load(file = "titanic.RData")

# Let's prepare the data for a barplot and frequency table for the
# passenger class variable.

pclass.freq = table(titanic$pclass)
pclass.prop = prop.table(pclass.freq)

pclass.freq.dist = cbind(pclass.freq,
                         format(pclass.prop, digits = 2))

# Here is the passenger class composition in a graph.

barplot(pclass.prop,
        xlab = "Passenger Class",
        ylab = "Proportion",
        ylim = c(0, .6),
        col = c("darkblue","orange", "darkgreen"))

# Here are the descriptive statistics.

pclass.freq.dist

# Now let's do the same thing for survival.

# First I'm going to give the survived values better names than just
# 0 and 1.

titanic$survived = factor(titanic$survived,
                          labels = c("Perished", "Survived"))

# Now I'm going to prepare the data.

survived.freq = table(titanic$survived)
survived.prop = prop.table(survived.freq)

survived.freq.dist = cbind(survived.freq,
                           format(survived.prop, digits = 2))

# Here is a barplot for survival.

barplot(survived.prop,
        xlab = "Survival Count",
        ylab = "Proportion",
        ylim = c(0, .8),
        col = c("red","green"))

# Now the survival table

survived.freq.dist

# Now let's look at the relationship of these variables

titanic.table = table(titanic$survived,
                        titanic$pclass)

titanic.percents = 100*prop.table(titanic.table, 2)

# Here's the graph.

barplot(titanic.percents,
        col = c("red", "green"),
        axes = FALSE,
        main = "Survival of Titanic Passengers by Cabin Class",
        ylab = "Percent Per Passenger Class")

axis(side = 2,
     at = c(0,10,20,30,40,50,60,70,80,90,100))        

legend(legend = rownames(titanic.percents),
       "bottomright",
       fill = c("red", "green"))

# Here are the statistics, with conditional proprtions

format(titanic.percents,
       digits = 0)

# I need to draw on frequencies for confidence interval for proportions.
# I'm going to create all three pairwise CIs. Note that I'm not
# adjusting error rate in this example. Also note that this is only for
# illustration purposes. We have the entire population in this data set
# so there really is nothing we need to infer to for these data!

# Here are the totals in each passenger class.

class.totals <- colSums(titanic.table)

# I'm going to focus on the proportion that survived. Note that this
# is in the second row of my table. Here is the differnece in proportions
# and the confidence interval for this difference for 1st class minus
# 2nd class.

prop.test(titanic.table[2,c(1,2)],class.totals[c(1,2)])

# Now first class minus third class.

prop.test(titanic.table[2,c(1,3)],class.totals[c(1,3)])

# Now second class minus third class.

prop.test(titanic.table[2,c(2,3)],class.totals[c(2,3)])
