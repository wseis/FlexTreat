library(tidyverse)

size = 100

x <- replicate(1000, rbinom(1, size = size, prob = 0.9)/size)
x <- rbinom(1000, size = size, prob = 0.95)/size

hist(x)
sd(x)
