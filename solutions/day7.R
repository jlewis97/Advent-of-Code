# Advent of Code
## December 7, 2023
## Day 7: Camel cards

# Import data
dat <- read.table("data/day7_input.txt", quote="\"", comment.char="")

# Load libraries
library(tidyverse)
library(stringr)

# Testing theory
test1 <- "JJJJJ" #five of a kind
test2 <- "JJJJ9" #four of a kind
test3 <- "JJJ99" #full house
test4 <- "JJJ9Q" #three of a kind
test5 <- "JJ99Q" #two pair
test6 <- "JJ9Q8" #one pair
test7 <- "J9Q8K" #high card
length(unique(strsplit(test1, NULL)[[1]])) #1
length(unique(strsplit(test2, NULL)[[1]])) #2
length(unique(strsplit(test3, NULL)[[1]])) #2
length(unique(strsplit(test4, NULL)[[1]])) #3
length(unique(strsplit(test5, NULL)[[1]])) #3
length(unique(strsplit(test6, NULL)[[1]])) #4
length(unique(strsplit(test7, NULL)[[1]])) #5
max(table(str_split(test1, ""))) #5
max(table(str_split(test2, ""))) #4
max(table(str_split(test3, ""))) #3
max(table(str_split(test4, ""))) #3
max(table(str_split(test5, ""))) #2
max(table(str_split(test6, ""))) #2
max(table(str_split(test7, ""))) #1

# Create table to identify hand type
match_table <- data.frame(
  unique = c(length(unique(strsplit(test1, NULL)[[1]])),
             length(unique(strsplit(test2, NULL)[[1]])),
             length(unique(strsplit(test3, NULL)[[1]])),
             length(unique(strsplit(test4, NULL)[[1]])),
             length(unique(strsplit(test5, NULL)[[1]])),
             length(unique(strsplit(test6, NULL)[[1]])),
             length(unique(strsplit(test7, NULL)[[1]]))),
  max = c(max(table(str_split(test1, ""))),
          max(table(str_split(test2, ""))),
          max(table(str_split(test3, ""))),
          max(table(str_split(test4, ""))),
          max(table(str_split(test5, ""))),
          max(table(str_split(test6, ""))),
          max(table(str_split(test7, "")))),
  hand = c("five of a kind", 
           "four of a kind", 
           "full house",
           "three of a kind",
           "two pair",
           "one pair",
           "high card"),
  hand_val = c(7, 6, 5, 4, 3, 2, 1)
)

# Part 1
dat_p1 <- dat %>%
  rename(cards = V1, bid = V2) %>%
  rowwise() %>%
  mutate(unique = length(unique(strsplit(cards, NULL)[[1]])),
         max = max(table(str_split(cards, ""))),
         card1 = as.numeric(str_replace_all(substr(cards, 1, 1), c("A" = "14", "K" = "13", "Q" = "12", "J" = "11", "T" = "10"))),
         card2 = as.numeric(str_replace_all(substr(cards, 2, 2), c("A" = "14", "K" = "13", "Q" = "12", "J" = "11", "T" = "10"))),
         card3 = as.numeric(str_replace_all(substr(cards, 3, 3), c("A" = "14", "K" = "13", "Q" = "12", "J" = "11", "T" = "10"))),
         card4 = as.numeric(str_replace_all(substr(cards, 4, 4), c("A" = "14", "K" = "13", "Q" = "12", "J" = "11", "T" = "10"))),
         card5 = as.numeric(str_replace_all(substr(cards, 5, 5), c("A" = "14", "K" = "13", "Q" = "12", "J" = "11", "T" = "10"))))
dat_p1 <- merge(dat_p1, match_table, by = c("unique", "max"))
dat_p1 <- dat_p1 %>%
  arrange(desc(hand_val), desc(card1), desc(card2), desc(card3), desc(card4), desc(card5)) %>%
  mutate(rank = rev(1:nrow(dat_p1))) %>%
  rowwise() %>%
  mutate(score = rank * bid)
sum(dat_p1$score) #249638405

# Part 2
dat_p2 <- dat %>%
  rename(cards = V1, bid = V2) %>%
  rowwise() %>%
  mutate(cards_noj = str_replace_all(cards, "J", ""),
         num_j = 5 - nchar(cards_noj),
         max_noj = ifelse(cards_noj == "", 0, max(table(str_split(cards_noj, "")))),
         max_wj = max(table(str_split(cards, ""))),
         max = max_noj + num_j,
         unique = ifelse(cards_noj == "", 1, length(unique(strsplit(cards_noj, NULL)[[1]]))),
         # Change J to have value of 1 instead of 11
         card1 = as.numeric(str_replace_all(substr(cards, 1, 1), c("A" = "14", "K" = "13", "Q" = "12", "J" = "1", "T" = "10"))),
         card2 = as.numeric(str_replace_all(substr(cards, 2, 2), c("A" = "14", "K" = "13", "Q" = "12", "J" = "1", "T" = "10"))),
         card3 = as.numeric(str_replace_all(substr(cards, 3, 3), c("A" = "14", "K" = "13", "Q" = "12", "J" = "1", "T" = "10"))),
         card4 = as.numeric(str_replace_all(substr(cards, 4, 4), c("A" = "14", "K" = "13", "Q" = "12", "J" = "1", "T" = "10"))),
         card5 = as.numeric(str_replace_all(substr(cards, 5, 5), c("A" = "14", "K" = "13", "Q" = "12", "J" = "1", "T" = "10"))))
dat_p2 <- merge(dat_p2, match_table, by = c("unique", "max"))
dat_p2 <- dat_p2 %>%
  arrange(desc(hand_val), desc(card1), desc(card2), desc(card3), desc(card4), desc(card5)) %>%
  mutate(rank = rev(1:nrow(dat_p2))) %>%
  rowwise() %>%
  mutate(score = rank * bid)
sum(dat_p2$score) #249776650
