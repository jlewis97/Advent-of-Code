# Advent of Code
## December 1, 2023
## Day 1: Trebuchet?!

# Import data
dat <- read.table("data/day1_input.txt", quote="\"", comment.char="")


# Load libraries
library(tidyverse)
library(stringr)
library(stringi)

# Part 1
dat_p1 <- dat %>%
  mutate(firstnum = str_extract(V1, "\\d"),
         lastnum = str_extract(stri_reverse(V1), "\\d"),
         num = as.numeric(paste0(firstnum, lastnum)))
sum(dat_p1$num) #54667

# Part 2
dat_p2 <- dat %>%
  mutate(V2 = str_replace_all(V1, c("one" = "o1e",
                                    "two" = "t2o",
                                    "three" = "t3e",
                                    "four" = "f4r",
                                    "five" = "f5e",
                                    "six" = "s6x",
                                    "seven" = "s7n",
                                    "eight" = "e8t",
                                    "nine" = "n9e"))) %>%
  mutate(firstnum = str_extract(V2, "\\d"),
         lastnum = str_extract(stri_reverse(V2), "\\d"),
         num = as.numeric(paste0(firstnum, lastnum)))
sum(dat_p2$num) #54203
