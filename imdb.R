##I set up my directory
setwd("C:\\Users\\hugor\\Documents\\R\\Testing")
##I make sure data.table is activated
library(data.table)
##I upload my dataset using fread
imdb_akas <- fread("data_akas.tsv")
##I check the number of rows
dim(imdb_akas)
##I check the entries in the variable "language"
table(imdb_akas$language)
##I create a new dataset which includes only hindi movies
imdb_hindi <- imdb_akas[imdb_akas$language == "hi", ]
dim(imdb_hindi)
##I check the entries in the variable "types"
table(imdb_hindi$types)
##I upload my second dataset using fread
imdb_basics <- fread("data_basics.tsv")
##I now want to merge imdb_hindi and imdb_basics, but for that we need to make the variables have the same name
names(imdb_hindi)
names(imdb_basics)
#"titleId" has to be renamed into "tconst" to be consistent
#I am not successful in doing that so far
## I active the package dplyr (i'll use it to modify the name of a variable)
library(dplyr)
## I change titleId into tconst 
imdb_hindi <- rename(imdb_hindi, tconst = titleId)
##I merged the two datasets
imdb_merge_1 <- merge(imdb_hindi,imdb_basics)
#I check the variables of the new dataframe
names(imdb_merge_1)
#I remove some variables from the dataset, so that it looks cleaner. 
#KEEP REGION!!!!!!!!!!!!!!!
imdb_slim <- select(imdb_merge_1, tconst, titleType, title, language, originalTitle, startYear, isAdult, genres, runtimeMinutes)
names(imdb_slim)
#Now we want to keep only the (1) movies, (2) which are not for adult and (3) which released after 2010
imdb_sample <- imdb_slim[which(imdb_slim$titleType=='movie'& imdb_slim$isAdult==0 & imdb_slim$startYear>=2010),]
head(imdb_sample, n=2)
#I add the last dataset (with ratings)
imdb_ratings <- fread("data_ratings.tsv")
##I merged the two datasets
imdb_2010s <- merge(imdb_sample,imdb_ratings)
#I check the variables of the new dataframe
names(imdb_2010s)
#I save the variables of the new dataframe
write.csv(imdb_2010s,'imdb2010s.csv')
