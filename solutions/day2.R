# Advent of Code
## December 2, 2023
## Day 2: Cube Conundrum

# Import data
dat <- read.delim2("data/day2_input.txt", header = FALSE)


# Load libraries
library(tidyverse)
library(stringr)

# Part 1
dat_p1 <- dat %>%
  mutate(ID = as.numeric(str_extract(V1, "\\d+")),
         red = str_extract_all(V1, "\\d+(?= red)"),
         green = str_extract_all(V1, "\\d+(?= green)"),
         blue = str_extract_all(V1, "\\d+(?= blue)"),
         maxred = sapply(str_extract_all(V1, "\\d+(?= red)"), function(x) max(as.numeric(x))),
         maxgreen = sapply(str_extract_all(V1, "\\d+(?= green)"), function(x) max(as.numeric(x))),
         maxblue = sapply(str_extract_all(V1, "\\d+(?= blue)"), function(x) max(as.numeric(x))),
         valid = ifelse(maxred <=12 & maxgreen <= 13 & maxblue <= 14, 1, 0))
sum(dat_p1$valid*dat_p1$ID) # 2278

# Part 2
dat_p2 <- dat %>%
  mutate(ID = as.numeric(str_extract(V1, "\\d+")),
         red = str_extract_all(V1, "\\d+(?= red)"),
         green = str_extract_all(V1, "\\d+(?= green)"),
         blue = str_extract_all(V1, "\\d+(?= blue)"),
         maxred = sapply(str_extract_all(V1, "\\d+(?= red)"), function(x) max(as.numeric(x))),
         maxgreen = sapply(str_extract_all(V1, "\\d+(?= green)"), function(x) max(as.numeric(x))),
         maxblue = sapply(str_extract_all(V1, "\\d+(?= blue)"), function(x) max(as.numeric(x))),
         power = maxred*maxgreen*maxblue)
sum(dat_p2$power) # 67953

