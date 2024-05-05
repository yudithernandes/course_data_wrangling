# HarvardX PH125.6x
# Data Science: Wrangling

# Paths and the Working Directory

getwd()


# Download the dslabs packages. This is the example dataset, we are working with

library(dslabs)
library(tidyverse)

# Where is the dataset from dslabs stored in my system? Which files are stored?

system.file("extdata", package = "dslabs")
path <- system.file("extdata", package = "dslabs")
list.files(path)

### Importing the files to my working directory

# 1. getting the right path name

filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# 2 . copying the filename in my working directory

file.copy(fullpath, getwd())

# 3. checking if the file is in my working directory

file.exists(filename)

# Before importing the dataset in R, we want to know wich format it has, in order to choose the right code to import it

read_lines ("murders.csv", n_max = 3)    # read only the first 3 lines, to see if there is a header

# Importing the dataset in R with tidyverse

dat <- read_csv(filename)  # importing data with read_csv from tidyverse

head(dat)

# Importing the dataset in R with R-base functions

dat2 <- read.csv(filename)

head(dat2)

# What is the difference when importing with R-Base comparing to importing with tidyverse?

class(dat)  # when importing with the tidyverse we get a table (tibble)
class(dat2) # when importing with R-base we get a data frame

# Question 1: ok, and? table vs data.frame? 
# see https://megapteraphile.wordpress.com/2020/03/25/data-frame-vs-data-table-vs-tibble-in-r/
# 1. data.frame are the original data structure in R-Base
# 2. data.table is faster than data.frame (use less memory)
# 3. tibble has the class tbl_df and is a perfection of the original data.frame. It belongs to the tidyverse, so it works
# with the tidyverse packages: ggplot2, dplyr, tidyr and readr

###### Start: Downloading the file from Github with url

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()  
tmp_filename <- tempfile()

# tempfile creates a character string that is likely to be a unique filename. 
# tempdir() creates a directory with a name that is very unlikely not to be unique.

# or downloading the file to my pc

download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

###### End: Downloading the file from Github

# Cloning the GitHub Project, so I can also work at home:
# in RStudio Terminal - git clone GitHub-Repo-Path

# Section 1: Data Import - 1.1: Data Import - Assessment Part 2: Data Import

library(tidyverse)

? `readr-package`

# Importing the dataset with readr

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat <- read_csv(url, col_names = FALSE)

head(dat)

# How many rows are in the dataset?
nrow(dat)

# How many columns are in the dataset?
ncol(dat)

### End Assessment

# Section 2: Tidy Data

library(dslabs)
data(gapminder)

# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

# Reshaping data: pivot_longer

# example dataset: fertility data in wide format (from previous video)
library(tidyverse) 
library(dslabs)

path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# snippet of wide data
wide_data %>% select(country, '1960':'1965')

# move the values in the columns 1960 through 2015 into a single column
wide_data %>% pivot_longer(`1960`:`2015`)

# another way to do this - only country isn't being pivoted
wide_data %>% pivot_longer(-country)

# change the default column names
new_tidy_data <- wide_data %>% 
  pivot_longer(-country, names_to = "year", values_to = "fertility")
head(new_tidy_data)

# compare the class from our original tidy data (year is an integer) and in the new version (year is a character)
class(tidy_data$year)
class(new_tidy_data$year)

# use the names_transform argument to change the class of the year values to numeric
new_tidy_data <- wide_data %>% 
  pivot_longer(-country, names_to = "year", values_to = "fertility", 
               names_transform = list(year=as.numeric))

# plot the data as before
new_tidy_data %>% ggplot(aes(year, fertility, color = country)) +
  geom_point()

# Reshaping data: pivot_wider

# still working with the same data as in the previous video
# convert the tidy data to wide data
new_wide_data <- new_tidy_data %>% 
  pivot_wider(names_from = year, values_from = fertility)
select(new_wide_data, country, `1960`:`1967`)

# Separate

# import data
path <- system.file("extdata", package = "dslabs")
fname <-  "life-expectancy-and-fertility-two-countries-example.csv"
filename <- file.path(path, fname)

raw_dat <- read_csv(filename)
head(raw_dat)
select(raw_dat, 1:4)

# pivot all columns except country
dat <- raw_dat %>% pivot_longer(-country)
head(dat)
dat$name[1:5]

# separate on underscores
dat %>% separate(name, c("year", "name"), sep = "_")

# the separator "_" is the default separater, so we can omit this argument



# separate on underscores (the default), convert years to numeric
dat %>% separate(name, c("year", "name"), convert = TRUE)

# because the "life_expectancy" variable has also an underscore "_", it will be drop out. This is a new issue. To solve it, we added a third column for expectancy.
# But there is again a new issue: for fertility, the third column has only NA values


# split on all underscores, pad empty cells with NA
dat %>% separate(name, c("year", "name_1", "name_2"), 
                 fill = "right", convert = TRUE)

# Another way to solve this, is to separate the first underscore, but keep the second merged.

# split on first underscore but keep life_expectancy merged
dat %>% separate(name, c("year", "name"), sep = "_", 
                 extra = "merge", convert = TRUE)

# Now we create two variables from the variable "name" using the pivot_wider ()

# separate then create a new column for each variable using pivot_wider
dat %>% separate(name, c("year", "name"), sep = "_", 
                 extra = "merge", convert = TRUE) %>%
  pivot_wider()


# Unite

# using the data from the previous video
# if we had used this non-optimal approach to separate
dat %>% 
  separate(name, c("year", "name_1", "name_2"), 
           fill = "right", convert = TRUE)

# we could unite the second and third columns using unite()
dat %>% 
  separate(name, c("year", "name_1", "name_2"), 
           fill = "right", convert = TRUE) %>%
  unite(variable_name, name_1, name_2, sep="_")

# spread the columns
dat %>% 
  separate(name, c("year", "name_1", "name_2"), 
           fill = "right", convert = TRUE) %>%
  unite(name, name_1, name_2, sep="_") %>%
  spread(name, value) %>%
  rename(fertlity = fertility_NA)

# Assessment Part 1: Reshaping Data (ok well done)

# Assessment Part 2: Reshaping Data

# Question 11: Examine the built-in dataset co2. This dataset comes with base R, not dslabs - just type co2 to access the dataset.

co2  # no tidy: Month variable is the header

# Question 12: Run the following code to define the co2_wide object:

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

head(co2_wide)  # the previous command did not change the data very much, only change the name of the months. The dataset were saved as a data.frame (39 obs of 13 variables)

co2_tidy <- co2_wide %>%
  pivot_longer(-year, names_to = "month", values_to = "co2")

head(co2_tidy)

# Question 13: Use co2_tidy to plot CO2 versus month with a different curve for each year:

co2_tidy %>% 
  ggplot(aes(as.numeric(month), co2, color = year)) + 
  geom_line()

# Question 14: Load the admissions dataset from dslabs, which contains college admission information for men and women across six majors, and remove the applicants percentage column:

library(dslabs)
data(admissions)
dat <- admissions %>% 
  select(-applicants)

head(dat)

dat_tidy <- pivot_wider(dat, names_from = gender, values_from = admitted)

head(dat_tidy)

# Question 15: Now use the admissions dataset to create the object tmp, which has columns major, gender, key and value:

tmp <- admissions %>%
  pivot_longer(cols = c(admitted, applicants), names_to = "key", values_to = "value")
tmp

# Combine the key and gender and create a new column called column_name to get a variable with the following values: 
# admitted_men, admitted_women, applicants_men, and applicants_women. Save the new data as tmp2.

tmp2 <- tmp %>%
  unite(column_name, key, gender)

tmp2

# Question 16: Which function can reshape tmp2 to a table with six rows and five columns named major, 
# admitted_men, admitted_women, applicants_men, and applicants_women?

### End of Assessment Part 2 (well done - easy easy)


# Section 2: Tidy Data 2.2: Combining Tables
# Info: join function in the dplyr packages is based on the SQL joins

# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)

ds_theme_set()   # what is this command?

data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)


# testing if the column state is equal in both tables - for join we need to have at least one similar column in both tables

identical(results_us_election_2016$state, murders$state)  # FALSE: it means, the order of the column "state" is not the same in both tables. That is why we use the function join!

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% 
  select(state, population)


tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% 
  select(state, electoral_votes)

tab2  # the argument "c(1:3, 5, 7:8)" of the function slice takes the location of the integer in the table
      # for example tab2 has 6 observations, and these correspond with the location 1-2-3-5-7-8 in the main table

# tab1 and tab2 are different

# experiment with different joins
left_join(tab1, tab2)

tab1 %>% 
  left_join(tab2)  # it creates NA for the variable, if the information of tab1 (main table) is not in tab2

tab1 %>% 
  right_join(tab2) # it creates NA for the variable, if the information of tab2 (main table) is not in tab1

inner_join(tab1, tab2) # it combines only the common elements in both tables

semi_join(tab1, tab2) # it does not "paste" a table with another, this is more like a filter, 
# it keep of tab1 only the information which is also in tab2

anti_join(tab1, tab2) # it takes the elements not common in both tables (is the opposite of inner_join)

full_join(tab1, tab2) # it takes all the rows from both tables and fill in the missing parts with NAs

### Binding

# Unlike the join functions, the binding functions do not try to match by a variable, but rather just combine the data sets
# bind_col() produces a tibble. If the datasets not match by the appropriate dimension we will obtain an error
# cbind() is another function from R base (not tidyverse - dplyer) and it produces a dataframe instead a tibble (Remember: R base functions <- data.frame (not so efficient), tidyverse functions <- tibble (better than data.frame)
# and data.table better than data.frame and tibble)

bind_cols(a = 1:3, b = 4:6)

# tab function creates a table

tab1 <- tab[, 1:3]
head(tab1)

tab2 <- tab[, 4:6]
head(tab2)

tab3 <- tab[, 7:9]
head(tab3)

new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

# Section: Set Operators

# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

# Assessment: Combining Tables

install.packages("Lahman")
library(Lahman)

top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10

top %>% 
  as_tibble()

head(top)

People %>% 
  as_tibble()

head(People)

# Question 5 

# Use the correct join or bind function to create a combined table of the names and statistics 
# of the top 10 home run (HR) hitters for 2016. This table should have the player ID, first name, 
# last name, and number of HR for the top 10 players. Name this data frame top_names.

# Identify the join or bind that fills the blank in this code to create the correct table:

top_names <- top %>% 
   left_join(People) %>%
  select(playerID, nameFirst, nameLast, HR)

#  Question 6

# Inspect the Salaries data frame. Filter this data frame to the 2016 salaries, then use the correct bind join function
# to add a salary column to the top_names data frame from the previous question. 
# Name the new data frame top_salary. Use this code framework:

head(Salaries)

top_salary <- Salaries %>% 
  filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

head(top_salary)


#  Question 7

# Inspect the AwardsPlayers table. Filter awards to include only the year 2016.

head(AwardsPlayers)

awards <- AwardsPlayers %>%
  filter(yearID == 2016)

# How many players from the top 10 home run hitters won at least one award in 2016?

head(awards)
head(top_names)

length(intersect(top_names$playerID, awards$playerID))

# How many players won an award in 2016 but were not one of the top 10 home run hitters in 2016?

length(setdiff(awards$playerID, top_names$playerID))

# Section: Web Scraping

# import a webpage into R
library(rvest)  # rvest is part of the tidyverse and allows us to make web scraping

url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"

h <- read_html(url)
class(h)
h

tab <- h %>% 
  html_nodes("table")

tab <- tab[[3]]


tab <- tab %>% 
  html_table

class(tab)

tab <- tab %>% 
  setNames(c("state", "population", "total", "murders", 
                          "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))

head(tab)

# CSS Selectors

# SelectorGadget is piece of software that allows you to interactively determine 
# what CSS selector you need to extract specific components from the webpage. 

# For the guacamole recipe page, we already have done this and determined that 
# we need the following selectors:

h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")

recipe <- h %>% 
  html_node(".o-AssetTitle__a-HeadlineText") %>% 
  html_text()

prep_time <- h %>% 
  html_node(".m-RecipeInfo__a-Description--Total") %>% 
  html_text()

ingredients <- h %>% 
  html_nodes(".o-Ingredients__a-Ingredient") %>% 
  html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

# it does not work and using the SelectorGadget on Chrome did not work either

# Assessment: Web Scraping

library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

# Storing all the html tables objects 
nodes <- html_nodes(h, "table")  # there are 22 table nodes

# how to see the content of the table node nr. 8
html_text(nodes[[8]])

# Converting the content of node nr. 8 in a table/data frame
html_table(nodes[[8]])

# Question 1 
# Convert the first four tables in nodes to data frames and inspect them.

html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

# or with only one single line of code

sapply(nodes[1:4], html_table)

# Question 1 - Answer: Table 2, 3 and 4 - but table 2 has not content at all, it is a tibble 1x2 with not information (content)
# about Rank, Team, Payroll.

# Question 2
# For the last 3 components of nodes, which of the following are true? 

html_table(nodes[[length(nodes)-2]])
html_table(nodes[[length(nodes)-1]])
html_table(nodes[[length(nodes)]])

# Question 3 
# Create a table called tab_1 using entry 10 of nodes. Create a table called tab_2 using entry 19 of nodes.

tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])

tab_1  # tab_1 is right so
tab_2  # tab_2 has the problem: the first row has the column names

# Note that the column names should be c("Team", "Payroll", "Average"). 
# You can see that these column names are actually in the first data row of each table, 
# and that tab_1 has an extra first column No. that should be removed so that the column names for both tables match.

# Remove the extra column in tab_1, remove the first row of each dataset, and change the column names 
# for each table to c("Team", "Payroll", "Average"). Use a full_join() by the Team to combine these two tables.

col_names <- c("Team", "Payroll", "Average")

# tab_1 <- tab_1[-1, -1]  # no need to transform tab_1 removing the first row and the first column because tab_1 is ok
tab_1

tab_2 <- tab_2[-1,]  # removing from tab_2 the first row
tab_2


names(tab_2) <- col_names  # replacing the column names of tab_2


tab_3 <- full_join(tab_1,tab_2, by = "Team")

# How many rows are in the joined data table?
nrow(tab_3)

# Question 4 and 5: Introduction

url <- "https://web.archive.org/web/20210606000626/https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

# Question 4: 
# Assign tab to be the html nodes of the "table" class.
# How many tables are in this Wikipedia page?

h <- read_html(url)  # creating an object h to save the url/website information

# Storing all the html tables objects 
tab <- html_nodes(h, "table")  # there are 40 table nodes
length(tab)

# Question 5

# Inspect the first several html tables using html_table() with the argument fill=TRUE (you can read about 
# this argument in the documentation). Find the first table that has 9 columns with the first column named "Date(s) conducted".

# What is the first table number to have 9 columns where the first column is named "Date(s) conducted"?

tab[[5]] %>%
  html_table(fill = TRUE) %>%
  names()

# Section 3: String Processing

# String parsing

# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

# Defining Strings: Single and Double Quotes and How to Escape

s <- "Hello!"    # double quotes define a string
s <- 'Hello!'    # single quotes define a string
s <- `Hello`    # backquotes do not

# s <- "10""    # error - unclosed quotes
s <- '10"'    # correct

# cat shows what the string actually looks like inside R
cat(s)

s <- "5'"
cat(s)

# to include both single and double quotes in string, escape with \
# s <- '5'10"'    # error
# s <- "5'10""    # error
s <- '5\'10"'    # correct
cat(s)
s <- "5'10\""    # correct
cat(s)

# murders_raw defined in web scraping video

# direct conversion to numeric fails because of commas

murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])  # the usual coercion to convert characters
                                        # to numbers does not work here due to commas

class(murders_raw$population)

# The main types of string processing tasks are detecting, locating, 
# extracting and replacing elements of strings.

# murders_raw was defined in the web scraping section

# detect whether there are commas
commas <- function(x) any(str_detect(x, ","))

murders_raw %>% 
  summarize_all(funs(commas))

# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

class(test_1)

# parse_number() = str_replace_all()

# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

# coercing in one step with parse_number

murders_new <- murders_raw %>% 
  mutate_at(2:3, parse_number)

murders_new %>% 
  head

# Assessment String Processing Part 1

# Question 4

# dat %>% mutate_at(2:3, parse_number)

# and 

# dat %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
#  mutate_at(2:3, as.numeric)

# You can use the parse_number command to remove all non-numeric characters. 
# Combining this with the mutate_at command allows you to reformat column two 
# and three (Sales and Profit).
# You can use the str_replace_all command to replace both the “$” and “,” characters, 
# by specifying these in the “pattern” argument of the command. 
# Combining this function with the mutate_at command allows you to reformat 
# both column two and three (Sales and Profit). You then need to use 
# the “as.numeric” command to convert these columns from character strings to numbers.

# Case Study 2: Reported Heights

# load raw heights data and inspect
library(dslabs)
library(tidyverse)
data(reported_heights)
class(reported_heights$height)

# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)  # error because of special characters

head(x)

sum(is.na(x)) # there are 81 na after coercing character into numeric

# keep only entries that result in NAs
reported_heights %>% 
  mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head(n=81)

sum(reported_heights$new_height)

# calculate cutoffs that cover 99.999% of human population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)  # calculating the max value of heights with 99% of prob - 69.1 and 2.9 are mean and sd
qnorm(alpha/2, 63.7, 2.7)    # calculating the min value of heigths with 99% of prob - 63.7 and 2.7 are mean and sd

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# the previous function, keep only the outliers: smaller than 50 inches and taller than 84 inches and keep the na generated
# after the as.numeric() function

# number of problematic entries (applying the function "not_inches")
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height

head(problems)

length(problems)  #there are 292 values with problems

problems

# after identiying the problems values, we have to look after patterns. There are mostly three patterns.
# define the form of the patterns (see cheatsheet for the nomenclature)
# str_subset() is a function that find matching elements
# cat: concatenate and print

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% 
  head(n=50) %>% 
  cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% 
  head(n=50) %>% 
  cat  


# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) ) %>%
  cat

ind <- ind[!is.na(ind)]

problems[ind] %>% 
  head(n=50) %>% 
  cat

#### Regular Expressions (regex)

# load stringr through tidyverse
library(tidyverse)

# detect whether a comma is present
pattern <- ","
str_detect(reported_heights$height, pattern) 

# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

# Example how to use special characters with regex
# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

# regex special character \\d

yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)

# highlight the first occurrence of a pattern
# str_view(s, pattern)  ### this function was deprecated

# highlight all instances of a pattern
str_view_all(s, pattern)


# Character classes

# s was defined in the previous video
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"

# [56] means 5 or 6
str_view_all(s, "[56]")

  # [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

# ^ means start of string, $ means end of string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)
str_detect(s, pattern)

# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

# Search and replace with regex

# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))   # only 14 entries of vector problems match the defined pattern
str_view(problems, pattern)

# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>%    # why c(2, 10, 11, 12, 15) ?????
  str_view(pattern)

str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()  # now we have 48 matches to our new pattern

# R does not ignore whitespace
identical("Hi", "Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")   # why is "AB" TRUE if the pattern is "A1*B"? - it matches also zero 1 (not 1)
str_detect(no, "A1*B")

# test how *, ? and + differ - the question is: how many spaces there are!
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           none_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()   # now we have 53 matches


# Groups with regex

# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups - there is no effect on the matching process
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% 
  head

str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% 
  head

# Testing and improving

# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)  # from the 292 entries with problems, there are 200 that are neither inches nor cm. These were identified 
# by the function "not_inches_or_cm"

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)  # 61% were matched correctly

converted[!index]    # show problems
length(converted[!index])  # 77 entries have yet problems and need to be fitted

# Assessment: String Processing Part 2

# Question 5:

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

# Question 6:

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"  # search for uppercase letters at the end of the string, that is why "$"
str_detect(animals, pattern)

#  Question 9:
# You are working on some data from different universities. You have the following vector:

schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia", "U California", "California State University")
schools

# Option 1 (don´t - because it does not restrict the beggining with Univ)
schools %>% 
  str_replace("Univ\\.?|U\\.?", "University ") %>% 
  str_replace("^University of |^University ", "University of ")

# Option 2 (\\.? - none or once . is ok)
schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")

# Option 3
schools %>% 
  str_replace("^Univ\\.\\s|^U\\.\\s", "University") %>% 
  str_replace("^University of |^University ", "University of ")

# Option 4
schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University") %>% 
  str_replace("University ", "University of ")

# Separate with regex

# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)
tab

# the separate and extract functions behave similarly
tab %>% 
  separate(x, c("feet", "inches"), sep = "'")

tab %>% 
  extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")  # using extract with groups from regex

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% 
  separate(x, c("feet","inches"), sep = "'", fill = "right")

tab %>% 
  extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# Remaining problems, which were not solved with the previous patterns

# 1- Many students measuring exactly 5 or 6 feet did not enter any inches. For example, 6' - our pattern requires that inches be included.
# 2- Some students measuring exactly 5 or 6 feet entered just that number.
# 3- Some of the inches were entered with decimal points. For example 5'7.5''. Our pattern only looks for two digits.
# 4- Some entires have spaces at the end, for example 5 ' 9.
# 5- Some entries are in meters and some of these use European decimals: 1.6, 1,7.
# 6- Two students added cm.
# 7- One student spelled out the numbers: Five foot eight inches.

# Solutions:

# 1- add '0

yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

# 2 and 4
str_replace(s, "^([56])'?$", "\\1'0")

# 3
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

# 5
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
s
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

# Trimming
# Spaces at the beginng or end of a string is a general enough problem that there 
# is a function dedicated to removing them: str_trim.

str_trim("5 ' 9 ")

# To upper and to lower case

s <- c("Five feet eight inches")
str_to_lower(s)

# Putting it into a function
# We are now ready to define a procedure that handles converting all the problematic cases.

convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

# We can also write a function that converts words to numbers:

words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

# Now we can see which problematic entries remain:

converted <- problems %>% 
  words_to_numbers %>% 
  convert_format

remaining_problems <- converted[not_inches_or_cm(converted)]

pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"

index <- str_detect(remaining_problems, pattern)

remaining_problems[!index]

length(remaining_problems[!index])  # 42 entries have problems

# We are now ready to put everything we've done so far together and wrangle our reported heights data 
# as we try to recover as many heights as possible. The code is complex but we will break it down into parts.

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84

new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% 
           convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

# We can check all the entries we converted using the following code:

new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

# Let's take a look at the shortest students in our dataset using the following code:

new_heights %>% 
  arrange(height) %>% 
  head(n=7)

# String splitting

# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
x <- x[-1]

# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head()

# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)

dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

# Recoding

# life expectancy time series for Caribbean countries
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# display long country names
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 

# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# Assessment Part 1: String Processing Part 3

# Question 1

schedule <- data.frame(day = c("Monday", "Tuesday"), staff = c("Mandy", "Steve", "Chris and Laura", "Ruth and Frank"))
schedule

# Question 2

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest()

tidy

tidy <- separate(schedule, staff, into = c("s1","s2","s3"), sep = ",") %>% 
  gather(key = s, value = staff, s1:s3)

tidy

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% 
  unnest()

tidy

# Question 3

# Using the gapminder data, you want to recode countries longer than 12 letters in the region 
# “Middle Africa” to their abbreviations in a new column, “country_short”. Which code would accomplish this? 

dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))
list(dat)

# Question 4

# Which regex expression can be used with str_detect() to return FALSE for every instance of 19.5?
  
ex <- c(19.5) 
pattern <- "^1\\d*$"
pattern <- "^1\\d+\\.\\d?$"
pattern <- "1\\d*"
pattern <- "[1-9]*\\.5"
str_detect(ex, pattern)

# Assessment Part 2: String Processing Part 3

# Import raw Brexit referendum polling data from Wikipedia:

library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)

