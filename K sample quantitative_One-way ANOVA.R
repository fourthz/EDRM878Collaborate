
### K sample Quantative Data- One-way ANOVA-Ginger ######

# Read your data first #
# if the file is .csv file, use this
my_data <- read.csv(file.choose())

# Here, we’ll use the built-in R data set named PlantGrowth. 
# It contains the weight of plants obtained under a control 
# and two different treatment conditions.
my_data <- PlantGrowth

# Visualize the data#
# Create side-by-side boxplots
boxplot(my_data$weight~my_data$group, ylab = "Weight", col=2:4,
       xlab="Group")

#Discriptive analysis#
# Here are the means for each group #
library(psych)
describeBy(my_data$weight,group=my_data$group)

#Inference#
# Here's the analysis using my model.
model <- lm(my_data$weight ~ my_data$group)
summary(model)
anova(model)

# or
model1 <- aov(weight ~ group, data = my_data)
# Summary of the analysis
summary(model1)

## Multiple pairwise-comparison between means of groups
## We can use Tuckey HSD
TukeyHSD(model1)

## Check ANOVA Assumptions ###
par(mfrow=c(2,2))
plot(model)

#Also we can use Levene's test 
library(car)
leveneTest(weight ~ group, data = my_data)
