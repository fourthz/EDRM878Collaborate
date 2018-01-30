<<<<<<< HEAD
#ONE SAMPLE QUANTITATIVE VARIABLE - PARAMETRIC METHODS

=======
#ONE SAMPLE QUANTITATIVE VARIABLE
# I am making a note is Dawn's filen from Adam
>>>>>>> 75348b827e8675a8d9d2d39854bca218e40e151f
#Visuals: boxplot or histogram

hist(variable, xlab = "X axis label", ylab = "Y axis label", ylim = c(0, 100),
     main = "Title", col = "blue")
boxplot(variable, ylab = "Y axis label", ylim = c(0, 100),
        main = "Title", col = "blue")

#Descriptives: five number summary plus mean and sd

mean(variable)
sd(variable)
summary(variable)

#Inference: confidence interval for the population mean

#Longer method - find components of moe then the upper and lower bounds
se.mean = sd(variable)/sqrt(n)
t = qt(.975, n)
moe = t*se.mean
ci.lb = mean(variable) - moe
ci.ub = mean(variable) + moe

#Short method
t.test(variable)


