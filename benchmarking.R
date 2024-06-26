library(microbenchmark)
library(tidyverse)
library(Rcpp)

vector_grow_append <- function(x) {
  output <- numeric()
  for(i in 1:x) {
    output <- append(output, i^2)
  }
  output

}
list_grow_append <- function(x) {
  output <- vector(mode = "list")
  for(i in 1:x) {
    output <- append(output, i^2)
  }
  output

}
vector_grow_c <- function(x) {
  output <- numeric()
  for(i in 1:x) {
    output <- c(output, i^2)
  }
  output
}

list_grow_c <- function(x) {
  output <- vector(mode = "list")
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

list_grow_br <- function(x) {
  output <- vector(mode = "list")
  for(i in 1:x) {
    output[i] <- i^2
  }
  output
}

vector_prealloc_sng <- function(x) {
  output <- numeric(x)
  for(i in 1:x) {
    output[i] <- i^2
  }
  output
}

vector_prealloc_dbl <- function(x) {
  output <- numeric(x)
  for(i in 1:x) {
    output[[i]] <- i^2
  }
  output
}

list_prealloc_dbl <- function(x) {
  output <- vector(mode = "list", x)
  for(i in 1:x) {
    output[[i]] <- i^2
  }
  output
}

vector_colon <- function(x) {
  (1:x)^2
}

list_colon <- function(x) {
  as.list((1:x)^2)
}

vector_seq <- function(x) {
  (seq(1,x, by = 1))^2
}

list_seq <- function(x) {
  as.list((seq(1,x, by = 1))^2)
}

vector_sapply <- function(x) {
  sapply(1:x, \(i) i^2)
}

list_lapply <- function(x) {
  lapply(1:x, \(i) i^2)
}

vector_map <- function(x) {
  map_dbl(x, \(i) i^2)
}

list_map <- function(x) {
  map(x, \(i) i^2)
}

vector_magrittr <- function(x) {
  1:x %>% (\(i) i^2)()
}

list_magrittr <- function(x) {
  1:x %>% (\(i) i^2)() %>% as.list()
}

vector_base <- function(x) {
  1:x |> (\(i) i^2)()
}

list_base <- function(x) {
  1:x |> (\(i) i^2)() |> as.list()
}


n <- 1e4
sourceCpp("benchmarking.cpp")

mb_by_n <- function(n) {

  microbenchmark(
    vector_grow_append(n),
    vector_grow_c(n),
    vector_grow_br(n),
    # vector_prealloc_sng(n),
    vector_prealloc_dbl(n),
    vector_colon(n),
    vector_seq(n),
    vector_sapply(n),
    vector_rcpp(n),
    vector_base(n),
    vector_magrittr(n),
    vector_map(n),

    list_grow_append(n),
    list_grow_c(n),
    list_grow_br(n),
    # list_prealloc_sng(n),
    list_prealloc_dbl(n),
    list_colon(n),
    list_seq(n),
    list_sapply(n),
    list_rcpp(n),
    list_base(n),
    list_magrittr(n),
    list_map(n),
    ) %>%
  group_by(expr) %>%
  summarize(median_time = median(time)) %>%
  arrange(-median_time) %>%
  mutate(n = n)

}

ns <- c(1, 10, 100, 1000, 2500, 5000, 7500, 10000, 12500, 15000)
mb_data <- map_dfr(ns, mb_by_n)

mb_data %>%
  mutate(expr = str_replace(expr, "vector_(.*)\\(n\\)", "\\1")) %>%
  ggplot(aes(x = n, y = median_time, color = expr, shape = expr)) +
  geom_line() +
  geom_point() +
  labs(x = "size of vector", y = "median time (ns)") +
  scale_color_manual(values = rep(c("tomato", "dodgerblue", "darkgrey", "orange"), 3)) +
  scale_shape_manual(values = rep(c(15, 17, 19), each = 4)) +
  coord_cartesian(ylim = c(0, 5e6)) +
  theme_classic()

x <- (1:100)^2

x[5]
x[c(5, 10)]

vector_get_one <- function(x) {

  index <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)

  x[5]

}

vector_get_ten <- function(x) {

  index <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)

  x[5]
  x[10]
  x[15]
  x[20]
  x[25]
  x[30]
  x[35]
  x[40]
  x[45]
  x[50]

}

vector_get_c <- function(x) {

  index <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)

  c(x[5], x[10], x[15], x[20], x[25],
    x[30], x[35], x[40], x[45], x[50])

}

vector_get_index <- function(x) {

  index <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  x[index]

}


long <- (1:1e4)^2
short <- (1:1e2)^2

microbenchmark(vector_get_one(long),
               vector_get_ten(long),
               vector_get_c(long),
               vector_get_index(long),
               vector_get_one(short),
               vector_get_ten(short),
               vector_get_c(short),
               vector_get_index(short)) %>%
  group_by(expr) %>%
  summarize(median_time = median(time)) %>%
  arrange(-median_time)


