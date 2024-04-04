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
head(dat)

# separate on underscores (the default), convert years to numeric
dat %>% separate(name, c("year", "name"), convert = TRUE)
head(dat)

# split on all underscores, pad empty cells with NA
dat %>% separate(name, c("year", "name_1", "name_2"), 
                 fill = "right", convert = TRUE)
head(dat)

# split on first underscore but keep life_expectancy merged
dat %>% separate(name, c("year", "name"), sep = "_", 
                 extra = "merge", convert = TRUE)
head(dat)

# separate then create a new column for each variable using pivot_wider
dat %>% separate(name, c("year", "name"), sep = "_", 
                 extra = "merge", convert = TRUE) %>%
  pivot_wider()

head(dat)