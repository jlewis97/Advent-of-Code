# Advent of Code
## December 6, 2023
## Day 5: If you give a seed a fertilizer

# Load libraries
library(tidyverse)
library(stringr)

# Import data
seeds <- as.data.frame(t(read.table("data/day5_input - seeds.txt", quote="\"", comment.char="")))
map1 <- read.table("data/day5_input - seed-soil.txt", quote="\"", comment.char="")
map2 <- read.table("data/day5_input - soil-fert.txt", quote="\"", comment.char="")
map3 <- read.table("data/day5_input - fert-water.txt", quote="\"", comment.char="")
map4 <- read.table("data/day5_input - water-light.txt", quote="\"", comment.char="")
map5 <- read.table("data/day5_input - light-temp.txt", quote="\"", comment.char="")
map6 <- read.table("data/day5_input - temp-humid.txt", quote="\"", comment.char="")
map7 <- read.table("data/day5_input - humid-loc.txt", quote="\"", comment.char="")

maps <- c("map1", "map2", "map3", "map4", "map5", "map6", "map7")
for(i in maps) {
  df <- get(i)
  df <- df %>%
    rename(dest_start = V1, source_start = V2, range = V3) %>%
    mutate(dest_end = dest_start + range - 1,
           source_end = source_start + range - 1)
  assign(i, df)
}

# Define a mapping function
map_function <- function(index, source_start, source_end, dest_start) {
  # Identify if the index is within the source start and end range
  match <- ifelse(index >= source_start & index <= source_end, 1, 0)
  
  # Get the dest number
  if (sum(match == 1)) {
    dif <- index - source_start[match == 1]
    dest <- dest_start[match == 1] + dif
  } else {
    dest <- index
  }
  
  return(dest)
}

# Part 1
for (i in 1:nrow(seeds)) {
  seeds$dest_map1[i] <- map_function(seeds$V1[i], map1$source_start, map1$source_end, map1$dest_start)
  seeds$dest_map2[i] <- map_function(seeds$dest_map1[i], map2$source_start, map2$source_end, map2$dest_start)
  seeds$dest_map3[i] <- map_function(seeds$dest_map2[i], map3$source_start, map3$source_end, map3$dest_start)
  seeds$dest_map4[i] <- map_function(seeds$dest_map3[i], map4$source_start, map4$source_end, map4$dest_start)
  seeds$dest_map5[i] <- map_function(seeds$dest_map4[i], map5$source_start, map5$source_end, map5$dest_start)
  seeds$dest_map6[i] <- map_function(seeds$dest_map5[i], map6$source_start, map6$source_end, map6$dest_start)
  seeds$dest_map7[i] <- map_function(seeds$dest_map6[i], map7$source_start, map7$source_end, map7$dest_start)
}
min(seeds$dest_map7) # 282277027

# Part 2
## Need a new list of seed numbers
seeds_new <- as.data.frame(t(read.table("data/day5_input - seeds.txt", quote="\"", comment.char="")))
seed_start <-  seeds_new$V1[seq(1, nrow(seeds_new), by = 2)]
range <- seeds_new$V1[seq(2, nrow(seeds_new), by = 2)]
seed_end <- seed_start + range - 1
seeds <- data.frame(V1 = numeric())

for (i in 1:10) {
  varname <- "V1"
  seed_list <- as.data.frame(seed_start:seed_end)
  names(seed_list) <- varname
  seeds <- bind_rows(seeds, seed_list)
}

for (i in 1:nrow(seeds)) {
  seeds$dest_map1[i] <- map_function(seeds$V1[i], map1$source_start, map1$source_end, map1$dest_start)
  seeds$dest_map2[i] <- map_function(seeds$dest_map1[i], map2$source_start, map2$source_end, map2$dest_start)
  seeds$dest_map3[i] <- map_function(seeds$dest_map2[i], map3$source_start, map3$source_end, map3$dest_start)
  seeds$dest_map4[i] <- map_function(seeds$dest_map3[i], map4$source_start, map4$source_end, map4$dest_start)
  seeds$dest_map5[i] <- map_function(seeds$dest_map4[i], map5$source_start, map5$source_end, map5$dest_start)
  seeds$dest_map6[i] <- map_function(seeds$dest_map5[i], map6$source_start, map6$source_end, map6$dest_start)
  seeds$dest_map7[i] <- map_function(seeds$dest_map6[i], map7$source_start, map7$source_end, map7$dest_start)
}
min(seeds$dest_map7)

#################################################################################

### Copied from https://github.com/AdroMine/AdventOfCode/blob/main/2023/Day05/solution.R
### My brutefroce code above would work, but it would take too long
input <- strsplit(readr::read_file("data/day5_input.txt"), '\\n\\n')[[1]]

seeds <- strsplit(input[1], ": ")[[1]][2] |> 
  strsplit(" ") |> 
  unlist() |> 
  as.numeric()

seed_maps <- 
  strsplit(input[-1], "\\n") |> 
  lapply(\(x) x[-1]) |> # remove first element (map name)
  lapply(strsplit, ' ') |> # multiple lines of numbers
  purrr::map_depth(2, as.numeric) # convert each to number

s_idx <- seq(1, length(seeds), by = 2)
r_idx <- seq(2, length(seeds), by = 2)

sds <- seeds[s_idx]
rng <- seeds[r_idx]


# Part 2 Non brute force

p2 <- Inf
for(i in seq_along(sds)){
  
  # create a range of seed locations
  cur_ranges <- list(c(sds[i], sds[i] + rng[i]))
  
  # for each mapping 
  for(j in seq_along(seed_maps)) {
    
    sm <- seed_maps[[j]]
    
    # ranges that have been mapped to the next seed map
    mapped_ranges <- list()
    
    # go with each row of current seed map
    for(k in seq_along(sm)) {
      
      sm_row <- sm[[k]]
      start <- sm_row[2]
      end <- sm_row[2] + sm_row[3]
      # new ranges that we will have to create (that are not mapped)
      # due to splits
      new_ranges <- list()
      
      # while we still have ranges to go through
      while(length(cur_ranges) > 0) {
        
        # take one object and remove from list
        cr <- cur_ranges[[length(cur_ranges)]]
        cur_ranges[[length(cur_ranges)]] <- NULL
        
        
        # create ranges (before mapping interval, overlapping interval, after mapping interval)
        part1 <- c(cr[1], min(cr[2], start))
        part2 <- c(max(cr[1], start), min(cr[2], end))
        part3 <- c(max(end, cr[1]), cr[2])
        
        # only part 2 will be part of mapping
        
        # if part1 and part3 are valid ranges, add them to next list of ranges to check/map
        if(part1[1] < part1[2]) new_ranges <- c(new_ranges, list(part1))
        if(part3[1] < part3[2]) new_ranges <- c(new_ranges, list(part3))
        
        # this is the only part that will have some mapping, add this to mapped ranges
        if(part2[1] < part2[2]) {
          mapped_part2 <- c(part2[1] - sm_row[2] + sm_row[1], 
                            part2[2] - sm_row[2] + sm_row[1])
          mapped_ranges <- c(mapped_ranges, list(mapped_part2))
        }
        
      }
      # iterate over any new ranges created
      cur_ranges <- new_ranges
    }
    # if any range still left, that goes unmapped, add the mapped ranges to it
    cur_ranges <- c(cur_ranges, mapped_ranges)
  }
  
  # All ranges are now mapped to location and present in cur_ranges
  
  # the first will always be lower, so take the first from each range and min over all
  p2 <- min(p2, min(sapply(cur_ranges, `[[`, 1)))
  
}

p2


