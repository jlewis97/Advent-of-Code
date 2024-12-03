# Advent of Code
## December 6, 2023
## Day 6: Wait for it

# Import data
dat <- read.delim("data/day6_input.txt", header = FALSE)

# Load libraries
library(tidyverse)
library(stringr)
library(genpwr)

# Part 1
dat_p1 <- data.frame(
  "time" = as.numeric(unlist(str_extract_all(dat$V1[1], "\\d+"))),
  "distance" = as.numeric(unlist(str_extract_all(dat$V1[2], "\\d+")))
) 
dat_p1 <- dat_p1 %>%
  rowwise() %>%
  mutate(time_min = ceiling(min(quad_roots(a = 1, b = -1*time, c = distance))),
         time_max = floor(max(quad_roots(a = 1, b = -1*time, c = distance))),
         dif = time_max - time_min + 1)
prod(dat_p1$dif) # 275724

# Part 2
dat_p2 <- data.frame(
  "time" = as.numeric(unlist(str_extract_all(str_replace_all(dat$V1, " ", "")[1], "\\d+"))),
  "distance" = as.numeric(unlist(str_extract_all(str_replace_all(dat$V1, " ", "")[2], "\\d+")))
) 
dat_p2 <- dat_p2 %>%
  rowwise() %>%
  mutate(time_min = ceiling(min(quad_roots(a = 1, b = -1*time, c = distance))),
         time_max = floor(max(quad_roots(a = 1, b = -1*time, c = distance))),
         dif = time_max - time_min + 1)
dat_p2$dif # 37286485

