## Tidy data concept

counts_df <- data.frame(
  day = c("Monday", "Tuesday", "Wednesday"),
  wolf = c(2, 1, 3),
  hare = c(20, 25, 30),
  fox = c(4, 4, 4)
)

## Reshaping multiple columns in category/value pairs

library(tidyr)
# Makes the data frame more tidy
counts_gather <- gather(counts_df,
                        key = 'species',
                        value = 'count',
                        wolf:fox)

# Makes the data frame more spread out and less tidy
counts_spread <- spread(counts_gather,
                        key = species,
                        value = count)

## Exercise 1

counts_gather <- counts_gather[-8, ]

# By doing this and re-running spread, this creates a cell with an NA, meaning that an
# observation in gone

solution1 <- spread(counts_gather,
                    key = species,
                    value = count,
                    fill = 0)

## Read comma-separated-value (CSV) files

surveys <- read.csv('data/surveys.csv', na.strings = '')

## Subsetting and sorting

library(dplyr)
surveys_1990_winter <- filter(surveys,
                              year == 1990,
                              month %in% 1:3)

surveys_1990_winter <- select(surveys_1990_winter, -year)

sorted <- arrange(surveys_1990_winter, desc(species_id), weight)

## Exercise 2

surveys_RO <- filter(surveys,
                     species_id == 'RO')

surveys_RO <- select(surveys_RO, record_id, sex, weight)

# ALSO CAN DO A ONE-LINE SOLUTION (e.g. surveys_RO <- select(filter(surveys, species_id == 'RO'), record_id, sex, weight))

## Grouping and aggregation

surveys_1990_winter_gb <- group_by(surveys_1990_winter, species_id)

counts_1990_winter <- summarize(surveys_1990_winter_gb, count = n())

## Exercise 3

surveys_DM <- filter(surveys,
                     species_id == 'DM')

surveys_DM_gb <- group_by(surveys_DM, month)

averages_DM <- summarize(surveys_DM_gb, avg_wt = mean(weight, na.rm = TRUE), avg_hflength = mean(hindfoot_length, na.rm = TRUE))

## Transformation of variables

prop_1990_winter <- mutate(counts_1990_winter, prop = count/sum(count))

## Exercise 4

surveys_1990_winter_min_weight <- filter(surveys_1990_winter_gb, 
                                         weight == min(weight, na.rm = TRUE))

surveys_1990_winter_gb_rankorder <- mutate(surveys_1990_winter_gb, rank = row_number(hindfoot_length))
surveys_1990_winter_gb_rankorder <- arrange(surveys_1990_winter_gb_rankorder, desc(species_id))

## Chaining with pipes

prop_1990_winter_piped <- surveys %>%
  filter(year == 1990, month %in% 1:3) %>%
  select(-year) %>% # select all columns but year
  group_by(species_id)  %>% # group by species_id
  summarize(count = n()) %>% # summarize with counts
  mutate(prop = count / sum(count)) # mutate into proportions
