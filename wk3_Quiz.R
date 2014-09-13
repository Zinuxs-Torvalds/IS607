# Week 3 Quiz
# Jordan Erickson

# 1. ----
m.numbers <- function(x, func = mean) {
  do.call(func, args = list(x))
}
m.numbers(1:10)

# 2. ----
m.numbers <- function(x) {
  return(mean(x, na.rm = TRUE))
}
m.numbers(c(1:10, NA))

# 3. ---- assumes we only want positive factors
gcd <- function(x, y) {
  x <- as.integer(x)
  div.x <- seq(abs(x))
  factors.x <- div.x[x %% div.x == 0] # find factors for x
  
  y <- as.integer(y)
  div.y <- seq(abs(y))
  factors.y <- div.y[y %% div.y == 0] # find factors for y
  
  gcd <- max(factors.x[factors.x %in% factors.y]) # get common factors; find greatest factor
  return(gcd)
}
gcd(12, 8) # 4
gcd(8, 12) # 4
gcd(182664, 154875) # 177

# 4. ----
euclid <- function(m, n) {
  r = m %% n
  while (r != 0) {
    m = n
    n = r
    r = m %% n
  }
  return(n)
}
euclid(8, 12) # 4

# 5. ----
equation <- function(x, y) {
  q <- (x^2 * y) + (2 * x * y) - (x * y^2)
  return(q)
}
equation(3, 7)

# 6. ----
# setwd("~/2_Jordan School/02_IS 607_Data Acquisit Mgmt")
price.data <- read.table("week-3-price-data.csv", header = TRUE, sep = ",")
model.data <- read.table("week-3-make-model-data.csv", header = TRUE, sep = ",")
require(plyr)
cars <- join(x = model.data, y = price.data, by = "ModelNumber")
NROW(cars) # 27 observations
# Is this what I would have expected?
# No. But then I checked the ModelNumbers in each data frame.
# price.data has a ModelNumber (23120) that does not appear in model.data.
# Since we are performing a left join on model.data, and model.data does not have ModelNumber 23120
# then it would make sense that we are left with 27 and not 28 observations.

# 7. ----
cars2 <- join(x = price.data, y = model.data, by = "ModelNumber")
NROW(cars2)

# 8. ----
cars2010 <- cars2[cars2$Year == 2010, ]
table(cars2010$Year)

# 9. ----
cars.red.cost <- cars2[cars2$Price > 10000 & cars2$Color == "Red", ]
cars.red.cost

# 10. ----
cars.red.cost$ModelNumber <- NULL
cars.red.cost$Color <- NULL
names(cars.red.cost)

# 11. ----
char.vector <- c("Jordan", "Ashley", "Erickson")
sapply(char.vector, nchar)

# 12. ----
concate <- function(x, y) {
  if (length(x) == length(y)) {
    paste(x, y, sep = " ")
  } else {
    "Vectors are not the same length"
  }
}

char1 <- c("A", "B", "C")
char2 <- c("Apple", "Banana", "Carrot")
char3 <- c("Bad", "Bad", "Leroy", "Brown")
concate(char1, char2)
concate(char1, char3)

# 13. ----
someChar <- c("facetious", "superfluous")
require(stringr)
str_extract(string = someChar, pattern = "[aeiou]{3}") 
# are we counting "y" as a vowel?

# 14. ----
mo <- c(01, 02, 03, 04, 05, 06)
dy <- c(7, 14, 21, 28, 7, 14)
yr <- c(2001, 2002, 2003, 2004, 2005, 2006)
df.dates <- data.frame(mo, dy, yr)
df.dates$dt <- as.Date(paste(yr, mo, dy, sep = "-"))
df.dates

# 15. ----
as.Date("08-01-2010", format = "%m-%d-%Y")

# 16. ----
require(lubridate)
date <- as.Date("08-01-2010", format = "%m-%d-%Y")
month(date)

# 17. ----
(lotsOfDates <- seq(ymd('2005-01-01'), ymd('2014-12-31'), by = 'days'))
length(lotsOfDates)
