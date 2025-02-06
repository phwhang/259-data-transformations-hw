#PSYC 259 Homework 2 - Data Transformation
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: 
# NA; myself only (Priscilla Whang)

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")
  
### Question 1 ---------- 

#Use glimpse to check the type of "Year". 
#Then, convert it to a numeric, saving it back to 'ds'
#Use typeof to check that your conversion succeeded

#ANSWER

# check data type of 'Year'?
glimpse(ds$Year) # chr; character

# make it numeric
ds$Year <- as.numeric(ds$Year)

# check conversion
glimpse(ds$Year) # num; numeric

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

names(ds)

#ANSWER
ds <- ds %>% 
  rename_with(tolower)

names(ds)

### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

head(ds$year)

#ANSWER

# checking doc
 ?floor

floor(5.5) # 5
floor(2007) # 2007

# it looks like floor() just rounds decimals to whole number, so we'll 
# divide years by 10, then run floor before multiplying back with 10
# e.g., 2007/10=200.7; floor(200.7) should give us 200; 200*10=2000

# create decade var, using mutate()
ds <- ds %>% 
  mutate(decade = floor(year/10)*10)

names(ds)
head(ds$decade)

### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

head(ds)

#ANSWER
ds <- ds %>% 
  arrange(ds, rank)

head(ds)

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER
top10 <- ds %>%              # create new tibble 'top10'
  filter(rank <= 10) %>%     # only the top 10
  select(rank, song, artist) # only keep rank, song, artist

top10

### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

#ANSWER
ds_sum <- ds %>% # create new tibble 'ds_sum'
  summarize(earliest = min(year, na.rm = T), # earliest release year
            recent = max(year, na.rm = T),   # latest release year
            average = mean(year, na.rm = T)) # avg. release year

ds_sum

### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year

#ANSWER
ds %>% 
  filter(year %in% c(ds_sum)) %>% # filter those w/ year that appear in ds_sum
  arrange(year)                   # sort (ascending) by year

### Question 8 ---------- 

# There's and error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

ds[ds$song == 'Brass in Pocket', ] # check Brass
ds[ds$song != 'Brass in Pocket', ] # check others

#ANSWER
ds <- ds %>%
  mutate(year = ifelse(song == 'Brass in Pocket', 1979, year),
         decade = ifelse(song == 'Brass in Pocket', floor(year/10)*10, decade)) 

ds[ds$song == 'Brass in Pocket', ] # check Brass
ds[ds$song != 'Brass in Pocket', ] # check others; untouched

# re-data frame ds_sum
ds_sum

ds_sum <- ds %>% # create new tibble 'ds_sum'
  summarize(earliest = min(year, na.rm = T), # earliest release year
            recent = max(year, na.rm = T),   # latest release year
            average = mean(year, na.rm = T)) # avg. release year

ds_sum # earliest and avg. changed

### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

?n # gives curr group size

#ANSWER
ds %>% 
  group_by(decade) %>% 
  summarize(average.rank = mean(rank, na.rm = T), # avg. rank
            songs.count = n())                    # number of songs in each decade

### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

?count
?slice_max

#ANSWER
ds %>% 
  group_by(decade) %>% 
  summarize(songs.count = n()) %>% 
  slice_max(order_by = songs.count)

# 1970 had the max. number of songs (143)
  