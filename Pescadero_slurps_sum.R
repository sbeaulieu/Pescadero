# script to sum data in tables as submitted to WHOAS to re-generate Tables in Fleming et al. (2022) MEPS
# Stace Beaulieu 2023-01-11
setwd('C:/Users/sbeaulieu/Desktop')
library(readr)
library(dplyr)
# will need to read in either the benthic or the plankton csv file
df <- read_csv('benthic_slurps_counts_20221227.csv')
# df <- read_csv('plankton_slurps_larval_counts_20221227.csv') 
# I want to group by Fleming_etal_Table and then sum the ISS*_Total3qtrs in benthic (ISS*_Total in plankton)
# exclude NA's in Fleming_etal_Table column
df <- filter(df, !is.na(Fleming_etal_Table))
subset <- select(df, Fleming_etal_Table, ends_with("Total3qtrs"))
# subset <- select(df, Fleming_etal_Table, ends_with("Total"))
subsetint <- subset %>% 
  mutate_at(c(2:12),as.integer)
# for plankton:
# subsetint <- subset %>% 
#   mutate_at(c(2:8),as.integer)
# create a new integer column that sums all
subsetintsum <- mutate(subsetint, sum_all = rowSums(across(where(is.integer))))
subsetintsum$sum_all <- as.integer(subsetintsum$sum_all)
# compare the "sum_all" column to Table in Fleming et al. (2022)
subsetintsum %>% 
  group_by(Fleming_etal_Table) %>% 
  summarise(across(everything(), sum))

# also want to check the sum of the other columns (not *_Total3qtrs)
othersubset <- select(df, !ends_with("Total3qtrs"))
# othersubset <- select(df, !ends_with("Total"))
othersubsetint <- othersubset %>% 
  mutate_at(c(8:40),as.integer)
# for plankton:
# othersubsetint <- othersubset %>% 
#   mutate_at(c(8:28),as.integer)
othersubsetintsum <- mutate(othersubsetint, sum_all = rowSums(across(where(is.integer))))
othersubsetintsum$sum_all <- as.integer(othersubsetintsum$sum_all)
# compare the "sum_all" column to Table in Fleming et al. (2022)
othersubsetintsum %>% 
  select(Fleming_etal_Table, sum_all) %>%
  group_by(Fleming_etal_Table) %>% 
  summarise(across(where(is.integer), sum))
