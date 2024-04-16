library(microbenchmark)
library(tidyverse)

vector_grow_c <- function(x) {
  output <- numeric()
  for(i in 1:x) {
    output <- c(output, i^2)
  }
  output
}

vector_grow_br <- function(x) {
  output <- numeric()
  for(i in 1:x) {
    output[i] <- i^2
  }
  output
}

vector_grow_prealloc <- function(x) {
  output <- numeric(x)
  for(i in 1:x) {
    output[i] <- i^2
  }
  output
}

vector_colon <- function(x) {
  (1:x)^2
}

vector_seq <- function(x) {
  (seq(1,x, by = 1))^2
}

vector_sapply <- function(x) {
  sapply(1:x, \(i) i^2)
}

vector_lapply <- function(x) {
  lapply(1:x, \(i) i^2)
}

n <- 1e4

microbenchmark(vector_grow_c(n),times = 10,
               vector_grow_br(n),
               vector_grow_prealloc(n),
               vector_colon(n),
               vector_seq(n),
               vector_sapply(n),
               vector_lapply(n)) %>%
  group_by(expr) %>%
  summarize(median_time = median(time)) %>%
  arrange(-median_time)

