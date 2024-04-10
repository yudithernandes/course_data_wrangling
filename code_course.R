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

tab1

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

semi_join(tab1, tab2) # it does not "paste" a table with another, this is more like a filter, it keep of tab1 only the information which is also in tab2

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


