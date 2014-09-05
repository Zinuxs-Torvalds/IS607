# Week 2 Quiz
# Jordan Erickson

# 1. ----
(numbers <- c(1:10, 1:5, c(2, 4, 6, 8, 10)))
class(numbers)
length(numbers)

# 2. ----
(numbers.char <- as.character(numbers))
class(numbers.char)

# 3. ----
(numbers.fac <- as.factor(numbers))
class(numbers.fac)

# 4. ----
numbers.fac
nlevels(numbers.fac) # ten levels

# 5. ----
(solution <- (3 * numbers ^ 2) - (4 * numbers) + 1)

# 6. ----
X <- matrix(c(rep(1,8), c(5,4,6,2,3,2,7,8), c(8,9,4,7,4,9,6,4)), ncol = 3)
y <- matrix(c(45.2,46.9,31.0,35.3,25.0,43.1,41.0,35.1), ncol = 1)
A <- t(X) %*% X
b <- t(X) %*% y
solve(A, b)

# 7. ----
ok <- list(c(1:5), c("puppies", "dogs"), c("cats", "kittens"))
names(ok) <- c("numbers", "canine", "feline")
ok

# 8. ----
c <- as.character(letters[1:10])
f <- as.factor(c(rep(letters[24:26], 3), "x"))
n <- 1:10
d <- as.Date(c(rep("2014-09-01", 5), rep("2014-09-02", 5)))
(df <- data.frame(c, f, n, d, stringsAsFactors = FALSE))
str(df)

# 9. ----
df2 <- data.frame("k", as.factor("q"), 11, as.Date("2014-09-02"), stringsAsFactors = FALSE)
names(df2)  <- c("c", "f", "n", "d")
(df3 <- rbind(df, df2))

# 10. ----
temp <- read.table(temperatures.csv, stringsAsFactors = FALSE)

# 11. ----
setwd("~/2_Jordan School/data")
measure <- read.table(measurements.txt, header=TRUE, sep="\t")

# 12. ----
url <- "http://www.madeupwebsite.com/data/madeupdata.csv"
web <- read.table(file = url, header = TRUE, sep = "|")

# 13. ----
fac <- 1
for(i in 1:12) {
  fac <- fac * ((1:12)[i])
}
(fac)
identical(fac, factorial(12)) # check

# 14. ----
y <- 1500
for (i in 1:(6 * 12)) {
  y <- y + (y * 0.0324)
}
y # $14,898.65

# 15. ----
third <- c(1:20)
sum(third[3], third[6], third[9], third[12], third[15], third[18]) # 63

# 16. ----
something <- 0
for (i in 1:10) {
  something <- something + 2^i
}
something # 2046

# 17. ----
some <- 1
something2 <- 0
while (some <= 10) {
  something2 <- something2 + 2^some
  some <- some + 1
}
something2 # 2046

# 18. ----
sum(2^(1:10)) # 2046

# 19. ----
(sequence <- seq(from = 20, to = 50, by = 5))

# 20. ----
(examples <- rep("example", 10))
length(examples)

# 21. ----
a <- 1
b <- 3
c <- -4
quadratic1 <- (-b + sqrt(b^2 - 4*a*c)) / (2*a)
quadratic2 <- (-b - sqrt(b^2 - 4*a*c)) / (2*a)
(quadratic.solution <- c(quadratic1, quadratic2))
