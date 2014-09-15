# Week 3 Assignment
# Jordan Erickson

# 1. ----
something <- c(1:5, NA, 6:7, NA, 9:10)
countMissing <- function(x) {
  sum(is.na(x))
}
countMissing(something) # number of missing values

# 2. ----
something.df <- data.frame(A = c(1:9, NA), B = c(1:4, NA, NA, 7:10), 
                           C = c(rep(NA, 5), 6:10), D = c(1:10))
something.df
countMissingDF <- function(x) {
  sum(is.na(x))
}
sapply(something.df, countMissingDF) # named vector with the number of missing values in each column of the data frame

# 3. ----
set.seed(1234)
randomness <- c(rnorm(95, mean = 10, sd = 2), rep(NA, 5))
summaryStats <- function(x) {
  x.sorted <- sort(x)
  quarter <- ceiling(sum(!is.na(x.sorted)) / 4)
  half <- ceiling(sum(!is.na(x.sorted)) / 2)
  n <- sum(!is.na(x)) # can't use length(), because it counts NAs
  
  min <- x.sorted[1]
  first.q <- x.sorted[quarter]
  median <- x.sorted[half]
  third.q <- x.sorted[quarter*3]
  max <- x.sorted[sum(!is.na(x))]
  
  mean <- sum(x, na.rm = TRUE) / n
  stdDev <- sqrt(sum((x - mean)^2, na.rm = TRUE) / n)
  missing <- sum(is.na(x))
  
  results <- list(min, first.q, median, third.q, max, mean, stdDev, missing)
  names(results) <- c("minimum", "fist quartile", "median", "third quartile", "maximum",
                      "mean", "standard deviation", "missing")
  return(results)
}
summaryStats(randomness)

# 4. ----
g <- c(LETTERS[1:4], LETTERS[6], rep(NA, 3), rep(LETTERS[2], 4), rep(LETTERS[1], 2), "Z")
h <- c("Joe","Joe", "Jill", "Jill", "Fred")

summaryChar <- function(x) {
  y <- na.omit(x) # omit na
  
  distinct <- length(unique(y)) # number of distinct elements
  
  p <- table(y) # create a table of elements and occurances
  q <- sort(p) # sort table by values
  q1 <- q[length(q)] # most common element
  q2 <- q[length(q) - 1] # 2nd most common element, unless q1 ties with q2
  
  most.common <- names(q1) # name of element that is most common
  most.common.count <- q1[[1]] # number of times most common element occurs
  
  missing <- sum(is.na(x))
  
  results <- list(distinct, most.common, most.common.count, missing)
  names(results) <- c("Number of distinct elements", "Most commonly occuring element",
                      "Number of times most common element occurs",
                      "Number of missing values")
  
  if (q1[[1]] == q2[[1]]) {
    return("There are ties in the vector. There is no most commonly occurring element.")
  } else {
    return(results)
  }  
}
summaryChar(g)
summaryChar(h) # handles ties gracefully

# 5. ----
logical <- c(TRUE, TRUE, FALSE, TRUE, FALSE, NA, NA)

summaryLogic <- function(x) {
  missing <- sum(is.na(x))
  y <- na.omit(x) # omit na
  
  no.True <- sum(y)
  no.False <- length(y) - sum(y)
  prop.True <- no.True / (no.True + no.False)
  
  results <- list(no.True, no.False, prop.True, missing)
  names(results) <- c("Number of TRUE values", "Number of FALSE values",
                      "Proportion of TRUE values", "Number of missing values")
  return(results)  
}
summaryLogic(logical)

# 6. ----
num1 <- rnorm(10, mean = 10, sd = 2)
num2 <- c(runif(8), NA, NA)
char <- c(LETTERS[1:5], "A", "A", "A", "B", "C")
logic1 <- c(rep(TRUE, 8), rep(FALSE, 2))
logic2 <- c(rep(TRUE, 5), rep(FALSE, 5))
df <- data.frame(num1, num2, char, logic1, logic2, stringsAsFactors = FALSE)

summaryDF <- function(x) {
  s <- list(NULL)
  c <- list(NULL)
  l <- list(NULL)
  
  for (i in 1:ncol(x)) {
    if (class(x[, i]) == "numeric") {
      s[[i]] <- summaryStats(x[, i])
    } else if (class(x[, i]) == "character") {
      c[[i]] <- summaryChar(x[, i])
    } else if (class(x[, i]) == "logical") {
      l[[i]] <- summaryLogic(x[, i])
    }
  }
  
  results <- list(s, c, l)
  names(results) <- c("Num", "Char", "Logic")
  return(results)
}
summaryDF(df)
