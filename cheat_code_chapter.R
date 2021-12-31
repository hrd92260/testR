####################################
###########     1     ##############
####################################
####### upload dataset #############
####################################

dataset <- dataset_test2


#### To remove all objects ####
rm(list = ls())



####################################
###########     2     ##############
####################################
############ Explore ###############
####################################

head(dataset)
str(dataset)
range(dataset$year)

mean(dataset$rating)
mean(dataset$rating, na.rm = TRUE)

library(pastecs)
stat.desc(dataset)


#I want to know the sample for each year (making sure it is in ascending order)
sort(table(dataset$year), decreasing = TRUE)

#I want to know the overall gender of directors
sort(table(dataset$director), decreasing = TRUE)

#I want to know the gender of directors for each year 
#This is called contigency table
table(dataset$director, dataset$year)

# Notice order matters: 1st variable is row variable, 2nd variable is column variable
table(dataset$year, dataset$director)

# I want to have the percentage
# Instead of having the frequencies (i.e.. the number of cases) 
# you can also have the relative frequencies (i.e., proportions) 
# in each subgroup by adding the table() function inside the prop.table() function:
prop.table(table(dataset$year, dataset$director))

# I don't find this very helpful
# Note that you can also compute the percentages by row or by column 
# How? By adding a second argument to the prop.table() function: 1 for row, or 2 for column:
round(prop.table(table(dataset$year, dataset$director), 1), 2) # round to 2 digits with round()
round(prop.table(table(dataset$year, dataset$director), 2), 2)


####################################
###########     3     ##############
####################################
############# Tabs #################
####################################

#table1 gives tables with a design similar than stata
library(table1)

# All the variables
table1::table1(~rating + votes + director + setting, data = dataset)

# We can disagregate per gender of the director
table1(~ rating + votes + character | director, dataset)

# The issue: we don't have nice labels for the variables and categories, 
# To improve things, we can create factors with descriptive labels for the categorical variables (sex and ulcer), label each variable the way we want, and specify units for the continuous variables (age and thickness), like this:
### First we label things properly
label(dataset$rating)       <- "Ratings on ImDB"
label(dataset$votes)       <- "Number of votes on ImDB"
label(dataset$character)     <- "Role of the main female character(s)"
label(dataset$director) <- "Gender of the director"
label(dataset$year) <- "Year of release"
label(dataset$setting) <- "Main geographic setting of the narrative"

### Just for information, although not useful here, we can also add units
units(dataset$votes)       <- "n"

### Here is how it looks with labels
table1(~ rating + votes + character + setting  | director, data=dataset, overall="Total")

### We can simplify the labeling process
labels <- list(
  variables=list(rating="Ratings on ImDB",
                 votes="Number of votes on ImDB",
                 character="Role of the main female character(s)",
                 director="Gender of the director"))


# We can customize the contents using custom renderers. 
# A custom render can be a function that take a vector 
# as the first argument and return a (named) character vector. 
# There is also a simpler way to customize the table contents 
# using an abbreviated code syntax instead of a render function, 
# but it allows less control over rounding (see below). 
# Here, for example, we specify render functions for the 
# continuous and categorical variables as follows:

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), 
       c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

# Simple
table1(~ rating + votes + character  | director, data=dataset, overall="Total")
# With rendering
table1(~ rating + votes + character  | director, data=dataset, overall="Total",
       render.continuous=my.render.cont, render.categorical=my.render.cat)
# Same but without total
table1(~ rating + votes + character  | director, data=dataset, overall=F,
       render.continuous=my.render.cont, render.categorical=my.render.cat)

# Table: does the ratings or traction change depending on gender of director 
table1(~ rating + votes  | director, data=dataset, overall=F,
       render.continuous=my.render.cont, render.categorical=my.render.cat)

# Table: does the ratings or traction change depending on gender of main character
table1(~ rating + votes  | character, data=dataset, overall=F,
       render.continuous=my.render.cont, render.categorical=my.render.cat)

####################################
###########     4     ##############
####################################
############# Graphs ###############
####################################

library(ggplot2)

ggplot(dataset) +
  aes(x = year) +
    geom_histogram()

ggplot(data=dataset, aes(x=director))+
  geom_bar()

ggplot(data=dataset, aes(x=setting))+
  geom_bar()

ggplot(data=dataset, 
  aes(x=year, y=rating, size=votes))+
  geom_point()

ggplot(data=dataset,
  aes(x=year, y=rating, color=director))+
  geom_point()

ggplot(data=dataset,
  aes(x=year, y=rating, color=director, size=votes))+
  geom_point()

# Line of best fit: It is often of interest to visualize your data using a line of best fit or some other "smoother" in order to better illustrate patterns between specific variables. This can be done using geom_smooth(). It's arguments are structured just like with geom_point(), including the use of aes(). Note that this structure is the same for all aesthetics, although certain parameters are exclusive to specific aesthetics due to their inherent differences in visualizing the data of interest

ggplot(data=dataset, aes(x=year, y=rating))+
  geom_smooth(method="lm")

ggplot(data=dataset, aes(x=year, y=rating))+
  geom_smooth(method="loess")

ggplot(data=dataset, aes(x=year, y=rating))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(data=dataset, aes(x=year, y=rating))+
  geom_point(aes(color=director))+
  geom_smooth(method="lm")

# Box plots
ggplot(data=dataset, aes(x=director, y=rating))+
  geom_boxplot()

ggplot(data=dataset, aes(x=director, y=rating, fill=director))+
  geom_boxplot()

ggplot(data=dataset, aes(x=director, y=rating, colour=director))+
  geom_boxplot()

ggplot(data=dataset, aes(x=director, y=rating, colour=director))+
  geom_boxplot()+
  labs(title="Female Representation Rate", subtitle="Per Gender of Director", 
       fill="Gender", caption="Created by Hugo Ribadeau Dumas", x="Female Visibility Rate", y="Gender of Director")

ggplot(data=dataset, aes(x=director, y=rating, colour=director))+
  geom_boxplot()+
  theme_bw()+
  labs(title="Female Representation Rate", subtitle="Per Gender of Director", 
       fill="Gender", caption="Created by Hugo Ribadeau Dumas", x="Female Visibility Rate", y="Gender of Director")

ggplot(data=dataset, aes(x=director, y=rating, colour=director))+
  geom_boxplot()+
  theme_classic()+
  labs(title="Female Representation Rate", subtitle="Per Gender of Director", 
       fill="Gender", caption="Created by Hugo Ribadeau Dumas", x="Female Visibility Rate", y="Gender of Director")

ggplot(data=dataset, aes(x=director, y=rating, colour=director))+
  geom_boxplot()+
  theme_classic()+
  theme(legend.position = "bottom", 
        legend.title = element_text(face="bold", size=10))+
  labs(title="Female Representation Rate", subtitle="Per Gender of Director", 
       fill="Gender", caption="Created by Hugo Ribadeau Dumas", x="Female Visibility Rate", y="Gender of Director")

# Inverser le sense
ggplot(data=dataset, aes(x=director, y=rating, colour=director))+
  geom_boxplot()+
  theme_classic()+
  theme(legend.position = "bottom", 
        legend.title = element_text(face="bold", size=5),
        legend.text = element_text(size=5),
        legend.key.size=unit(5,"mm"))+
  labs(title="Female Representation Rate", subtitle="Per Gender of Director", 
       fill="Gender", caption="Created by Hugo Ribadeau Dumas", x="Female Visibility Rate", y="Gender of Director")+ 
  coord_flip()

ggplot(data=dataset, aes(x=director, y=rating, colour=director))+
  geom_boxplot()+
  theme_classic()+
  theme(legend.position = "bottom", 
        legend.title = element_text(face="bold", size=5),
        legend.text = element_text(size=5),
        legend.key.size=unit(5,"mm"))+
  facet_wrap(~director)+
  labs(title="Female Representation Rate", subtitle="Per Gender of Director", 
       fill="Gender", caption="Created by Hugo Ribadeau Dumas", x="Female Visibility Rate", y="Gender of Director")




####################################
###########     5     ##############
####################################
############# Stats ################
####################################

# coefficient of variation (standard deviation divided by the mean)
sd(dataset$rating,na.rm = TRUE) / mean(dataset$rating,na.rm = TRUE) 

# correlation
# Another descriptive statistics is the correlation coefficient. 
# A correlation measures the linear relationship between two variables.
# lire

# In order to check whether gender of director is significantly associated with female representation, 
# we could perform a Chi-square test of independence since both variables are categorical variables. 
# See how to do this test by hand and in R.


# P-value
ctable(
x = dataset$year,
y = dataset$director,
chisq = TRUE, # display results of Chi-square test of independence
headings = FALSE # remove headings
)
# The p-value is close to 0 
# So we reject the null hypothesis of independence between the two variables. 
# In our context, this indicates that species and size are dependent and that there is a significant relationship between the two variables.



library(readr)
library(tidyverse)

hist(dataset$rating, xlab = "Female Visibility rate",
     main="Representation of women")

data_only_women <- dataset %>% filter(director=="Female")

hist(data_only_women$rating, xlab = "Rate",
     main="Representation in movies made by women") 

mean(dataset$rating, na.rm=TRUE)

var(dataset$rating, na.rm=TRUE)

median(dataset$rating, na.rm=TRUE)

summary(dataset$rating)

