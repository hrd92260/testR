# I first import on R the SPSS dataset which was provided by Utrecht University

# To do that, I need to set the working directory of R in the folder where the file is located
setwd("C:\\Users\\hugor\\OneDrive\\PhD\\Learning material\\R")

# Now I will to use the package "foreign" (because it is a SPSS file, hence a "foreign" file). The function library() allows to active the package
library(foreign)

# Then I can use the function "foreign" to import the dataset "simulated_assumptions". I call the new dataset "exercise"
alcohol_dataset <- read.spss("hangovercure.sav", use.value.label=TRUE, to.data.frame=TRUE)

# There has been an experiment: people have drunk some alcohol (drunk), and the following morning we check how bad/good they feel (well). In the meanwhile, they also consumed another drink- water, cola or lucozade (drink) - to check if this reduces the negative impact of alcohol
# Here we take "well" as the response variable (dependent variable), "drunk" as the predictor variable (covariate) and "drink" as the categorical variable (fixed factor).
# The categorical variable is also called a factor. The different categories within the factor (men/women, etc.) are called levels or groups

# I check my data
library(dplyr)
dplyr::sample_n(alcohol_dataset, 10)
############ Note: the sample_n() function randomly picks a few of the observations in the data frame to print out:

# Check the levels/groups of one given categorical variable
levels(alcohol_dataset$drink)

# We check the statistical characteristics of each group, using the group_by() function of the package dplyr
group_by(alcohol_dataset, drink) %>%
  summarise(
    count = n(),
    mean = mean(well, na.rm = TRUE),
    sd = sd(well, na.rm = TRUE)
  )

# We visualise the data by using plots
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(alcohol_dataset, x = "drink", y = "well", 
          color = "drink", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Water", "Lucozade", "Cola"),
          ylab = "Wellness", xlab = "Drink")

# We can also do that with boxplot()
boxplot(alcohol_dataset$well ~ alcohol_dataset$drink)
# Or...
boxplot(well ~ drink, data = alcohol_dataset)

# Mean plots
ggline(alcohol_dataset, x = "drink", y = "well", 
       add = c("mean_se", "jitter"), 
       order = c("Water", "Lucozade", "Cola"),
       ylab = "Wellness", xlab = "Drink")

############ One-way ANOVA test
######## An ANOVA (ANalysis Of VAriance) test is a way to find out if survey or experiment results are significant.
###### We want to know if there is any significant difference between the average wellness of patients depending on the 3 drinks
# We use the function aov() which stands for Analysis of Variance
# Compute the analysis of variance
aov_simple <- aov(well ~ drink, data = alcohol_dataset)
# Summary of the analysis
summary(aov_simple)
# Result: the P value is 0.288, which is much more than 0.05, which means that there is no significant statistical variation

###########" Analysis of covariance: ANCOVA
# We use Regression analysis to create models which describe the effect of variation in predictor variables on the response variable. 
# Sometimes, if we have a categorical variable with values like Yes/No or Male/Female etc, the simple regression analysis gives multiple results for each value of the categorical variable. 
# In such scenario, we can study the effect of the categorical variable by using it along with the predictor variable and comparing the regression lines for each level of the categorical variable. 
# Such an analysis is termed as Analysis of Covariance also called as ANCOVA.
#### Here we study the effect of the value of "drink (type of drink)" on the regression between "drunk" and "wellbeing". 
########### It is done by using the aov() function followed by the anova() function to compare the multiple regressions.

# We create a regression model taking "drunk" as the predictor variable and "well" as the response variable taking into account the interaction between "drink" and "drunk".
aov_model1 <- aov(well~drunk*drink,data = alcohol_dataset)
# It is worth noting that your categorical variable in the aov() needs to be a factor. 
# For example, you may have categorical groups labeled 1–10, but of those labels are numeric or integeter in the eyes of R, then they won’t work in aov(). 
# Fortunately, the as.factor() wrapper usually does the trick.
#### Here we create a simpler model where we take both "drunk" and "drink" as predictors
aov_model2 <- aov(well~drunk+drink,data = alcohol_dataset)

#### NOTE: I don't know which model is the correct one. I can only say that results provided by Utrecht Uni (through SPSS) are similar to model 2

summary(aov_model1)
summary(aov_model2)
##### What interests us the most is the P value ('Pr')
#### A p-value <0.05 tells us that at least one group mean differs from another at the α=0.05 level of significance. 
### What does all this mean? It means if you run your ANOVA and skip to the p-value, a p-value >0.05 suggests no group means differ from each other and you may be done with that model. 
## If the p-value <0.05, then you have at least one group that differs from the other(s) and additional steps need to be taken to quantify those differences.

# In our case,this result shows that both "drunk" and "drink" don't have significant effect on "well" as the p value in both cases is more than 0.05. 
# The interaction between these two variables (drunk:drink) is also not significant as the p-value is more than 0.05.

####### THIS IS OF COURSE WRONG! We know at least that drunk should be statistically correlated with well. There must be something wrong. Let's check the outlies

# (((( By the way, We can get details for the different groups of the factor))) Could be useful for later
aov_model1$coefficients
aov_model2$coefficients

########## We need to remove outliers (we can see that there are at least two for Cola)
###### With the function which() we can get the row numbers of the outliers
boxplot(well ~ drink, data = alcohol_dataset)
## We can identify and label the outliers by using the ggbetweenstats function in the ggstatsplot package.
### To label outliers, we are specifying the outlier.tagging argument as "TRUE" and we're specifying which variable to use to label each outlier with the outlier.label argument.
library(ggstatsplot)
ggbetweenstats(data = alcohol_dataset, 
               x = drink,
               y = well,
               outlier.tagging = TRUE,
               outlier.label = id)

# We have identified the outliers: rows 20, 49, 53, 55, 59 [note: the graph is not very clear, some labels are hidden...]
alcohol <- alcohol_dataset[-c(20,49, 53, 55, 59),] 

aov_model1_without_outlier <- aov(well~drunk*drink,data = alcohol)
aov_model2_without_outlier <- aov(well~drunk+drink,data = alcohol)
summary(aov_model1_without_outlier)
summary(aov_model2_without_outlier)
# In the model 2 (as used by Utrech Uni in SPSS), the P value is now inferior to 0.05 (less than 0.001 for drunk, 0.038 for drink). It means 
# In the model 1, # The interaction between these two variables (drunk:drink) is still not significant as the p-value is more than 0.05.

# We can generate a regression line to compare results for different groups (how well varies with drunk depending on drink)
ggscatter(
  alcohol, x = "drunk", y = "well",
  color = "drink", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = drink)
  )
# It shows that if we have lucozade, well is higher for equal drunk 

### I could not finish the exercise
