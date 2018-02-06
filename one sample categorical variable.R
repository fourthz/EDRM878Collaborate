#### R Code for one sample categorical variable ####
#Descriptive Statistics
#Summary (frequency/proportion)
summary(mydata)

#Produce frequency table
table(mydata$Gender)
table(mydata$Education)

#Two-way tables
EdGender <- table(mydata$Gender, mydata$Education)
EdGender
#Row proportions
prop.table(EdGender,1)
# Round col prop to 2 digits
round(prop.table(EdGender,1), 2) 
# Round col prop to 2 digits (percents)
round(100*prop.table(EdGender,1), 2) 
# Column proportions
prop.table(EdGender,2)
# Round column prop to 2 digits
round(prop.table(EdGender,2), 2)
# Round column prop to 2 digits (percents)
round(100*prop.table(EdGender,2), 2)

#Inferential Statistics
#chi-square test between Gender & Education
chisq.test(EdGender)
#another method of chi-square test
summary(table(Gender$Education))
#another method of chi-square test
summary(table(EdGender))
#pearson correlation test
cor.test(Gender$Education, method = 'pearson')

#Graphs: bar plot 
#Simple Bar Plot
barplot(matrix, main="title of the graph", 
        xlab="X axis label")
#Simple Horizontal Bar Plot with Added Labels
barplot(matrix, main="title of the graph", horiz=True, names.arg=c(firstcategory, secondcategory, thirdcategory))
#Stacked Bar Plot with Colors and Legend
barplot(matrixes, main="title of the graph", xlab="X axis label", col=c("color1","color2"),
        legend = rownames(matrixes))
# Grouped Bar Plot
barplot(matrixes, main="title of the graph", xlab="X axis label", col=c("color1","color2"),
        legend = rownames(matrixes), beside=True)
