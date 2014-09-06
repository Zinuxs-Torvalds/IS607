# Week 2 Assignment
# Jordan Erickson

# 1. ----
# a.
(queue <- c("James", "Mary", "Steve", "Alex",  "Patricia"))
# b.
(queue <- append(queue, "Harold")) # "James"    "Mary"     "Steve"    "Alex"     "Patricia" "Harold"
# c.
(queue <- queue[-1]) # "Mary"     "Steve"    "Alex"     "Patricia" "Harold"
# d.
(queue <- append(queue, "Pam", 1)) # "Mary"     "Pam"      "Steve"    "Alex"     "Patricia" "Harold"
# e.
(queue <- queue[-6]) # "Mary"     "Pam"      "Steve"    "Alex"     "Patricia"
# f.
(queue <- queue[-match("Alex", queue)]) # "Mary"     "Pam"      "Steve"    "Patricia"
# g.
match("Patricia", queue) # 4
# h.
length(queue) # 4

# 2. ----
a <- 3 # 1, 9, 3
b <- 4 # 3, 12, 4
c <- 2 # -4, 4, 2
# quadratic formula: (-b + sqrt(b^2 - 4*a*c)) / (2*a)
(discriminant <- b^2 - 4*a*c)
if (discriminant == 0) {
  print("there is only ONE solution")
  (quadratic.solution <- (-b + sqrt(discriminant)) / (2*a))
} else if (discriminant <= -1) {
  print("there is no REAL solution")
  (quadratic.solution <- (-b + sqrt(as.complex(discriminant))) / (2*a))
} else {
  print("there are TWO soutions")
  quadratic1 <- (-b + sqrt(discriminant)) / (2*a)
  quadratic2 <- (-b - sqrt(discriminant)) / (2*a)
  (quadratic.solution <- c(quadratic1, quadratic2))
}

# 3. ----
thousand <- 1:1000
sum(ifelse(thousand %% 3 == 0 | thousand %% 7 == 0| thousand %% 11 == 0, 0, 1))
# 1:1000 NOT divisible by 3, 7, or 11 = 520

# 4. ----
# Pythagorean Triple: input.largest^2 = input.A^2 + input.B^2
f <- 3
g <- 4
h <- 5
inputs <- c(f, g, h)
input.largest <- max(inputs)
input.other <- inputs[-match(input.largest, inputs)]
is.it <- input.largest^2 == input.other[1]^2 + input.other[2]^2
ifelse(is.it == TRUE, "We have a Pythagorean Triple!", "We don't have a Pythagorean Triple... so sad.")
