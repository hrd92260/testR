history()
history(max.show=Inf)
# To remove all objects #
rm(list = ls())


dataset <- dataset_test

head(dataset)
str(dataset)

range(dataset$year)

mean(dataset$rating)

mean(dataset$rating, na.rm = TRUE)

library(pastecs)
stat.desc(dataset)


# coefficient of variation (standard deviation divided by the mean)
sd(dataset$rating) / mean(dataset$rating) 

sort(table(dataset$year), decreasing = TRUE)
sort(table(dataset$director), decreasing = TRUE)

# correlation
# Another descriptive statistics is the correlation coefficient. A correlation measures the linear relationship between two variables.
# lire

# Contigency table
table(dataset$director, dataset$year)

# In order to check whether size is significantly associated with species, we could perform a Chi-square test of independence since both variables are categorical variables. See how to do this test by hand and in R.

# Instead of having the frequencies (i.e.. the number of cases) you can also have the relative frequencies (i.e., proportions) in each subgroup by adding the table() function inside the prop.table() function:
prop.table(table(dataset$year, dataset$director))

# Note that you can also compute the percentages by row or by column by adding a second argument to the prop.table() function: 1 for row, or 2 for column:
round(prop.table(table(dataset$year, dataset$director), 1), 2) # round to 2 digits with round()

# percentages by column:
round(prop.table(table(dataset$year, dataset$director), 2), 2)

# A mosaic plot allows to visualize a contingency table of two qualitative variables:

mosaicplot(table(dataset$year, dataset$director),
           color = TRUE,
           xlab = "Species", # label for x-axis
           ylab = "Size" # label for y-axis
)

library(ggplot2)
ggplot(dataset) +
  aes(x = year) +
  geom_histogram()

# Très bon graphs
library(summarytools)
ctable(
  x = dataset$year,
  y = dataset$director
)

ctable(
  x = dataset$year,
  y = dataset$character,
  prop = "n", # remove proportions
  totals = FALSE, # remove totals
  headings = FALSE # remove headings
)


ctable(
  x = dataset$year,
  y = dataset$director,
  chisq = TRUE, # display results of Chi-square test of independence
  headings = FALSE # remove headings
)
# The p-value is close to 0 so we reject the null hypothesis of independence between the two variables. In our context, this indicates that species and size are dependent and that there is a significant relationship between the two variables.


stby(list(
  x = dataset$setting, # smoker and diseased
  y = dataset$character
),
INDICES = dataset$director, # for each gender
FUN = ctable # ctable for cross-tabulation
)


aggregate(cbind(votes, rating) ~ director + setting,
          data = dataset,
          mean
)



#qwraps2

library(qwraps2)
options(qwraps2_markup = "markdown")
dataset <- as.data.frame(dataset)
summary_statistics <-
  list(
    "Life Expectancy" =
      list(
        "mean (sd)" = ~qwraps2::mean_sd(lifeExp, na_rm = TRUE),
        "median (Q1, Q3)" = ~qwraps2::median_iqr(lifeExp, na_rm = TRUE),
        "min" = ~min(lifeExp, na.rm = TRUE),
        "max" = ~max(lifeExp, na.rm = TRUE),
        "Missing" = ~sum(is.na(lifeExp))
      ),
    "Population" =
      list(
        "mean (sd)" = ~qwraps2::mean_sd(pop, na_rm = TRUE),
        "median (Q1, Q3)" = ~qwraps2::median_iqr(pop, na_rm = TRUE),
        "min" = ~min(pop, na.rm = TRUE),
        "max" = ~max(pop, na.rm = TRUE),
        "Missing" = ~sum(is.na(pop))
      ),
    "GDP per Capita" =
      list(
        "High GDP per Capita" = ~qwraps2::n_perc(na.omit(gdpPercap) %in% "high"),
        "Low GDP per Capita" = ~qwraps2::n_perc(na.omit(gdpPercap) %in% "low"),
        "Missing" = ~sum(is.na(gdpPercap))
      )
  )
summary_table(gapminder, summary_statistics)

######## table1 (SIMILAR TO STATA!)
library(table1)
table1::table1(~rating + votes + director, data = dataset)

table1(~ rating + votes + character | director, dataset)

# we don't have nice labels for the variables and categories, 
# it doesn't look great. To improve things, we can create factors with descriptive labels for the categorical variables (sex and ulcer), label each variable the way we want, and specify units for the continuous variables (age and thickness), like this:


label(dataset$rating)       <- "Ratings on ImDB"
label(dataset$votes)       <- "Number of votes on ImDB"
label(dataset$character)     <- "Role of the main female character(s)"
label(dataset$director) <- "Gender of the director"
label(dataset$year) <- "Year of release"

units(dataset$votes)       <- "n"

table1(~ rating + votes + character  | director, data=dataset, overall="Total")

# improve it further (and simplify the process)

labels <- list(
  variables=list(rating="Ratings on ImDB",
                 votes="Number of votes on ImDB",
                 character="Role of the main female character(s)",
                 director="Gender of the director"))


# We can customize the contents using custom renderers. 
# A custom render can be a function that take a vector as the first argument and return a (named) character vector. 
# There is also a simpler way to customize the table contents using an abbreviated code syntax instead of a render function, but it allows less control over rounding (see below). 
# Here, for example, we specify render functions for the continuous and categorical variables as follows:

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}



table1(~ rating + votes + character  | director, data=dataset, overall="Total")

table1(~ rating + votes + character  | director, data=dataset, overall="Total",
              render.continuous=my.render.cont, render.categorical=my.render.cat)

table1(~ rating + votes + character  | director, data=dataset, overall=F,
       render.continuous=my.render.cont, render.categorical=my.render.cat)


table1(~ rating + votes | director*character, data=dataset, total=F)

######""# Graphs
boxplot(dataset$year)

plot(dataset$votes, dataset$rating)

plot(dataset$votes,
     type = "l") # "l" for line
