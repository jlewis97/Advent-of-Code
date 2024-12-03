# Advent of Code
## December 3, 2023
## Day 3: Gear Ratios

# Import data
dat <- read.table("data/day3_input.txt", quote="\"", comment.char="")

# Load libraries
library(tidyverse)
library(stringr)

# Part 1
extract_numbers <- function(text, row_number) {
  matches <- str_extract_all(text, "\\d+")
  data.frame(
    row_number = row_number,
    number = as.numeric(unlist(matches)),
    num_start = str_locate_all(text, "\\d+")[[1]][, 1]
  )
}

dat_p1 <- do.call(rbind, Map(extract_numbers, dat$V1, row_number = seq_len(nrow(dat)))) %>%
  rowwise %>% 
  mutate(num_end = num_start + nchar(number) - 1,
         search_row_min = row_number - 1,
         search_row_min = ifelse(search_row_min < 1, 1, search_row_min),
         search_row_max = row_number + 1,
         search_row_max = ifelse(search_row_max > 140, 140, search_row_max),
         search_col_min = num_start - 1,
         search_col_min = ifelse(search_col_min < 1, 1, search_col_min),
         search_col_max = num_end + 1,
         search_col_max = ifelse(search_col_max > 140, 140, search_col_max))

table(strsplit(dat$V1, "") %>% unlist()) # -#$%&*/@+=

dat_mat <- strsplit(dat$V1, "") %>% unlist()

dat_validate <- expand.grid(numrow = 1:140, numcol = 1:140) %>%
  mutate(value = dat_mat[numcol + (140*(numrow - 1))],
         type = ifelse(str_detect(value, "\\d"), "digit",
                       ifelse(str_detect(value, "[-#$%&*/@+=]"), "symbol", "dot")))

dat_p1 <- dat_p1 %>%
  rowwise() %>%
  mutate(valid = nrow(dat_validate %>%
                        filter(numrow >= search_row_min,
                                numrow <= search_row_max,
                                numcol >= search_col_min,
                                numcol <= search_col_max,
                                type == "symbol")))

sum(dat_p1$number[dat_p1$valid >= 1]) # 527364

# Part 2
dat_validate_gear <- dat_validate %>%
  filter(value == "*") %>%
  select(-c(type, value)) %>%
  mutate(index = as.character(1:359))

dat_p2 <- do.call(rbind, Map(extract_numbers, dat$V1, row_number = seq_len(nrow(dat)))) %>%
  rowwise %>% 
  mutate(num_end = num_start + nchar(number) - 1,
         search_row_min = row_number - 1,
         search_row_min = ifelse(search_row_min < 1, 1, search_row_min),
         search_row_max = row_number + 1,
         search_row_max = ifelse(search_row_max > 140, 140, search_row_max),
         search_col_min = num_start - 1,
         search_col_min = ifelse(search_col_min < 1, 1, search_col_min),
         search_col_max = num_end + 1,
         search_col_max = ifelse(search_col_max > 140, 140, search_col_max))

dat_p2 <- dat_p2 %>%
  rowwise() %>%
  mutate(gears = as.character(list(dat_validate_gear$index[
      (dat_validate_gear$numrow >= search_row_min) &
        (dat_validate_gear$numrow <= search_row_max) &
        (dat_validate_gear$numcol >= search_col_min) &
        (dat_validate_gear$numcol <= search_col_max)]))) %>%
  filter(gears != "character(0)")


gear_ct <- dat_p2 %>%
  count(gears) %>%
  filter(n == 2)

dat_p2 <- dat_p2 %>%
  filter(gears %in% gear_ct$gears) %>%
  select(gears, number)

result <- dat_p2 %>%
  arrange(gears) %>%
  group_by(gears) %>%
  summarise(total_product = prod(number))  # Calculate product for each group
sum(result$total_product) # 79026871