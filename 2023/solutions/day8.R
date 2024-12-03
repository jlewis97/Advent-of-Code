# Advent of Code
## December 8, 2023
## Day 8: Haunted wasteland

library(tidyverse)
library(stringr)
library(pracma)

# Import data
# First two lines for testing
pattern <- strsplit(readr::read_file("data/day8_input - test.txt"), '\\n\\n')[[1]][1]
dat <- read.delim2("data/day8_input - test.txt", header = FALSE) %>% slice(-1)
pattern <- strsplit(readr::read_file("data/day8_input.txt"), '\\n\\n')[[1]][1]
dat <- read.delim2("data/day8_input.txt", header = FALSE) %>% slice(-1)
dat <- dat %>%
  mutate(V1 = gsub("[^A-Za-z0-9]", "", V1),
         source = substr(V1, 1, 3),
         L = substr(V1, 4, 6),
         R = substr(V1, 7, 9)) %>%
  select(-V1)

day8_fun <- function(dat) {
  itt <- 1
  current_side <- substr(pattern, itt, itt)
  current_source <- "AAA"
  end <- "NA"
  
  while (end != "ZZZ") {

    # Find the matching row in dat
    if(current_side == "L") {
      matching_row <- dat$L[dat$source == current_source]
    } else {
      matching_row <- dat$R[dat$source == current_source]
    }
    
    # Check if the value is "ZZZ"
    end <- matching_row
    
    # If it's not "ZZZ", update current_source and itt
    if (end != "ZZZ") {
      current_source <- matching_row
      itt <- itt + 1
      current_side <- substr(pattern, (itt - 1) %% nchar(pattern) + 1, (itt - 1) %% nchar(pattern) + 1)
    }
  }
  
  return(itt)
}

# Part 1
day8_fun(dat)

# Part 2
dat <- dat %>%
  mutate(start = substr(source, 3, 3),
         index = 1:nrow(dat))
index_list <- as.vector(dat$index[dat$start == "A"])

day8_fun2 <- function(dat, i) {
  itt <- 1
  current_side <- substr(pattern, itt, itt)
  current_source <- dat$source[dat$index == i]
  end <- "NA"
  
  while (end != "Z") {
    
    # Find the matching row in dat
    if(current_side == "L") {
      matching_row <- dat$L[dat$source == current_source]
    } else {
      matching_row <- dat$R[dat$source == current_source]
    }
    
    # Check if the value is "ZZZ"
    end <- substr(matching_row, 3, 3)
    
    # If it's not "ZZZ", update current_source and itt
    if (end != "Z") {
      current_source <- matching_row
      itt <- itt + 1
      current_side <- substr(pattern, (itt - 1) %% nchar(pattern) + 1, (itt - 1) %% nchar(pattern) + 1)
    }
  }
  
  return(itt)
}

day8_fun2(dat, 29) #20513
day8_fun2(dat, 299) #18827
day8_fun2(dat, 378) #17141
day8_fun2(dat, 415) #22199
day8_fun2(dat, 422) #12083
day8_fun2(dat, 494) #13207

# calculate least common multiple
lcm_vector <- function(x) Reduce(Lcm, x)
as.character(lcm_vector(c(20513,18827,17141,22199,12083,13207))) #13385272668829
