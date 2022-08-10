sessionInfo()

# Bring in the tidyverse suite of packages.
# This contains many useful packages.
# We will use dplyr and readr here, as well as ggplot2, dbplyr, tidyr in other notebooks.
library(tidyverse)

#data_file = 'ca_wac_S000_JT00_2015.csv'
getwd()
df <- read_csv(paste(getwd(), "/data/ca_wac_S000_JT00_2015.csv", sep=""))
# df <- read.csv('ca_wac_S000_JT00_2015.csv')

print(df)

data_file = 'https://lehd.ces.census.gov/data/lodes/LODES7/ca/wac/ca_wac_S000_JT00_2015.csv.gz'
df <- read_csv(data_file)

# Can check the column specifications
# This shows that most of the column types are numbers (double)
spec(df)

class(data_file)

class(df)

class(1) # this is a number

class(read_csv) # this is a function

'1' + 2

as.numeric('1') + 2

df_tx <- readr::read_csv('https://lehd.ces.census.gov/data/lodes/LODES7/tx/wac/tx_wac_S000_JT00_2015.csv.gz')

dim(df_tx)

# str(df)
glimpse(df_tx)

head(df_tx)

names(df_tx)

df_tx$w_geocode
# df[,'w_geocode']
# These do the same thing

df_tx[1,]

df_tx[,1]
# df[,'w_geocode']
# These do the same thing

# This is a vector of numbers from 1 to 10
print(1:10)

df_tx[1:10,]

df_tx[1:10,1:5]

# Vector with 1, 3, 6, 9, and 11
c(1,3,6,9,11)

# Accessing just rows 1, 3, 6, 9, 11
df_tx[c(1,3,6,9,11),]

over_100 <- df_tx[df_tx$C000 > 100,]
head(over_100)

df_tx$C000 > 100

between_50_100 <- df_tx[(df_tx$C000 > 50) & (df_tx$C000 < 100),]
head(between_50_100)

between_50_100 <- df_tx[(df_tx$C000 <= 50) | (df_tx$C000 >= 100),]
head(between_50_100)

over_100 <- filter(df_tx, C000 > 100)
head(over_100)

between_50_100 <- filter(df_tx, C000 > 50, C000 < 100)
head(between_50_100)

df_tx %>% head()

df_tx %>% 
    filter(C000 > 100) %>% 
    head()

# This does the same as the previous cell
head(filter(df_tx, C000 > 100))

df_tx %>% filter(C000 > 100) %>%
    select(w_geocode, C000) %>%
    head()

df_tx <- readr::read_csv('https://lehd.ces.census.gov/data/lodes/LODES7/tx/wac/tx_wac_S000_JT00_2015.csv.gz')



summary(df_tx)

# Mean number of jobs per census blocks
mean(df_tx$C000)

# Median number of jobs per census blocks
median(df_tx$C000)

# Minimum number of jobs in a census blocks
min(df_tx$C000)

# Maximum number of jobs in a census blocks
max(df_tx$C000)

# Standard Deviation
sd(df_tx$C000)

# Find the mean of C000 and the number of rows.
df_tx %>%
    summarize(mean_c000 = mean(C000),
              median_ca01 = median(CA01),
              n = n())

ca_xwalk <- read.csv('data/ca_xwalk.csv')

ca_xwalk %>%
    head()

count_by_county <- ca_xwalk %>%
    group_by(ctyname) %>%
    summarize(n = n())

head(count_by_county)

ca_xwalk %>%
  group_by(ctyname) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)

with_CA_sum <- df %>%
  mutate(CA_sum = CA01 + CA02 + CA03)

head(with_CA_sum)

check_CA_sum <- with_CA_sum$C000 != with_CA_sum$CA_sum
sum(check_CA_sum)


