# Advent of Code
## December 4, 2023
## Day 4: Scratchcards

# Import data
dat <- read.delim("data/day4_input.txt", header = FALSE)
dat <- dat %>%
  mutate(score = 0,
         nummatch = 0)

# Load libraries
library(tidyverse)
library(stringr)

# Part 1
for (i in 1:nrow(dat)) {
  tmp <- dat[i, 1]
  winnums_list <- str_extract(tmp, "(?<=: ).*?(?=\\|)")
  winnums <- as.numeric(unlist(str_extract_all(winnums_list, "\\d+")))
  playnums_list <- str_extract(tmp, "(?<=\\| ).*")
  playnums <- as.numeric(unlist(str_extract_all(playnums_list, "\\d+")))
  playnums_df <- data.frame(V1 = unlist(playnums))
  playnums_df <- playnums_df %>%
    mutate(match = ifelse(V1 %in% winnums, 1, 0))
  tmpsum <- sum(playnums_df$match)
  tmpscore <- ifelse(tmpsum < 1, 0, 1*(2^(tmpsum - 1)))
  dat[i, 2] <- tmpscore
  dat[i, 3] <- tmpsum
}
sum(dat$score) #18653

# Part 2
dat_p2 <- dat %>%
  mutate(index = 1:nrow(dat),
         currentcardtotal = 1)

for (i in 1:nrow(dat_p2)) {
  index <- dat_p2$index[i]
  nummatch <- dat_p2$nummatch[i]
  numstartingcards <- dat_p2$currentcardtotal[i]
  
  if (nummatch > 0) {
    for (j in 1:nummatch) {
      dat_p2$currentcardtotal[j + index] <- dat_p2$currentcardtotal[j + index] + numstartingcards
    }
  }
}
