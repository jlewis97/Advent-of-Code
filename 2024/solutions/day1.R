# Import data
dat <- read.table("2024/data/day1_input.txt", quote="\"", comment.char="")
library(dplyr)

list1 <- sort(dat$V1)
list2 <- sort(dat$V2)
tmp <- data.frame(cbind(list1, list2))
tmp$dif <- abs(tmp$list1 - tmp$list2)
sum(tmp$dif)

counts <- table(factor(list2, levels = list1))
results <- as.numeric(counts) * list1
sum(results)
