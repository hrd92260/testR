# I first import on R the SPSS dataset which was provided by Utrecht University

# To do that, I need to set the working directory of R in the folder where the file is located
setwd("C:\\Users\\hugor\\OneDrive\\PhD\\Learning material\\R")

# Now I will to use the package "foreign" (because it is a SPSS file, hence a "foreign" file). The function library() allows to active the package
library(foreign)

# Then I can use the function "foreign" to import the dataset "simulated_assumptions". I call the new dataset "exercise"
exercise <- read.spss("simulated_assumptions.sav", use.value.label=TRUE, to.data.frame=TRUE)

# With the function "names" we check what are the variables of our dataset
names(exercise)

# Now, we will run a linear regression analysis with "y" as dependent variable and "x1", "x2", and "x3" as predictors

## For that we will use the lm() function
### When we enter the following command, it tells us that the coefficients for the regression model y=(ax1)+b
lm(y~x1, data=exercise)
###So here it means that the regression model is: y=(-0.02973*x1)+27.08055
#### But how significant is the model?
###### Here I am using lm() as well as the function summary () function. This will allow us to visualise the results.
simple_regression = lm(y~x1, data=exercise)
summary(simple_regression)

############# HOW TO INTERPRET THE REGRESSION OUTPUT? ################
#      Residuals: it summarises the error between the prediction of the model [y=(ax1)+b] and the actual results.  Smaller residuals are better.
#      Coefficients
#                 Estimate: This is the weight given to the variable
#                 Standard Error: Says how precisely was the estimate measured. It's basically useful to calculate the t-value
#                 t-value: if it is close to zero, it means the coefficient isn't adding anything to the model (=not very significant)
#      Performance measures
#                 Residual standard error: Smaller is better (it is the standard variation of the residuals)
#                 R-square: the bigger the better. It is the percentage of the response variable variation that is explained by a linear model. The adusted R-square takes into consideration the number of variable. 
#                 F-Statistic (F-test): the p-value has to be lower than 0.05), if not it means the model is not relevant. This figure checks if least one variable's weight is significiantly different than zero.


#### We can also use lm() for x1, x2 and x3 simultanenously (multiple regression)
multi_regression = lm(y~x1+x2+x3, data=exercise)
summary(multi_regression)

# Alright, so now we now basic statistical characteristics of our dataset. 
## The next step is that we want to improve it by removing outliers/weird values

# Let's check the minimum and maximum values for each variable, and let's compare it with the mean of the variable, to see if it makes any sense
# We can use the functions min() and max()
min(exercise$x1)
max(exercise$x1)

# By the way, we can also locate where the max or min are located (which serial number of the dataset), which the following:
which.min(exercise$x1)

# But it is more convenient to use the function range(), which gives both min and max. We can then compare it with mean()
range(exercise$x1)
mean(exercise$x1)

range(exercise$x2)
mean(exercise$x2)

range(exercise$x3)
mean(exercise$x3)

# In fact, there is even a third method, and it is definitely the best one: summary()
summary(exercise$x1)
summary(exercise$x2)
summary(exercise$x3)

######### We can also generate histograms of the data, to see outliers
# The first way is to use the function hist()
hist(exercise$x1)
# For your information, you can enhance the coding in order to add parameters
hist(exercise$x1,
     main="Histogram for x1 - Where are outliers?",
     xlab="X1",
     col="skyblue",
     freq=FALSE
)
# There is another way to generate an histogram, using the package ggplot2. It just looks fancier (and I feel we can see the outliers better)
library(ggplot2)

ggplot(exercise) +
  aes(x = x1) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()



######### Now we will do the same, but with boxplots
# A boxplot helps to visualize a quantitative variable by displaying five common location summary (minimum, median, first and third quartiles and maximum) and any observation that was classified as a suspected outlier using the interquartile range (IQR) criterion. 
# We use the function boxplot()
boxplot(exercise$x1,
        ylab = "x1"
)
# Or, again, we use the package ggplot2, which looks nicer
ggplot(exercise) +
  aes(x = "", y = x1) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

# We can ask R to tell us which values are outliers according to the boxplot

# With the function which() we can also get the row numbers of the outliers
outliers <- boxplot.stats(exercise$x1)$out
outliers_rows <- which(exercise$x1 %in% c(outliers))
outliers_rows

# We can now verify the content of these rows
exercise[outliers_rows, ]

# By the way, we can also mark the values of the outliers directly on the boxplot. We just add the mtext() function
boxplot(exercise$x1,
        ylab = "x1"
)
mtext(paste("Outliers:",paste(outliers,collapse=",")))

####### So, we have seen that rows 19 and 277 are outliers for x1. We want to remove these tw observations from our sample.
exercise_clean <- exercise[-c(19,277),] 

# We verify that the two outliers have been removed from x1
summary(exercise$x1)
summary(exercise_clean$x1)
# Yes, they have!
# Just in passing, we can also use the function subset() to remove rows with a certain value. For instance (arbitrarily) I remove all the rows which have 0 for x1 and 4 for x2
subset(exercise_clean, x1==0 && x2 == 4)





######### There is another technique to identify outliers: percentiles
####### We use the quantile() function to know the values of the lower and upper percentiles (thus the lower and upper limits of the interval)

x1_lower <- quantile(exercise$x1, 0.025)
x1_lower

x1_upper <- quantile(exercise$x1, 0.975)
x1_upper

# According to this method, all observations below 15.85 and above 95.17 will be considered as potential outliers
# We extract the row numbers of the observations outside of the interval with the function which()
outside_interval_x1 <- which(exercise$x1 < 15.85 | exercise$x1 > 95.17)
outside_interval_x1
exercise[outside_interval_x1, ]

# We can reduce the number of outliers by setting the percentiles to 1 and 99
x1_lower_2 <- quantile(exercise$x1, 0.01)
x1_lower_2

x1_upper_2 <- quantile(exercise$x1, 0.99)
x1_upper_2

outside_interval_x1_2 <- which(exercise$x1 < 3.6 | exercise$x1 > 97.9)
outside_interval_x1_2
exercise[outside_interval_x1_2, ]

# Note: if we set the percentiles to 1 and 99, it gives the same potential outliers as with the IQR criteria (boxplot)
