# Week 5 Assignment
# Jordan Erickson

# 1. ----
# 1) Does age affect preference for Cullen skink?
# 2) Is there a difference in preference between cities--Edinburgh and Glasgow?
# 3) Which age group and city combination yields the highest prefernce for Cullen skink?

# 2. ----
yes <- c(80100, 143000, 99400, 150400)
no <- c(35900, 214800, 43000, 207000)
poll <- as.data.frame(rbind(yes, no))
poll$preference <- rownames(poll)
names(poll) <- c("Edinburgh 16-24", "Edinburgh 25+", "Glasgow 16-24", "Glasgow 25+", "preference")
poll

# 3. ----
require(tidyr)

(poll.long <- gather(poll, var, value, 1:4)) # pivot
(poll.long <- separate(poll.long, var, c("city", "age"), sep = " ")) # sep var into city and age

# 4. ----
require(dplyr)

# Answer to question 1:
poll.long %>%
  group_by(age, preference) %>%
  summarize(
    count = sum(value)
      ) %>%
  mutate(pct = count / sum(count)) %>%
  filter(preference == "yes")

# 69% of Scottish voters ages 16-24 prefered Cullen skink,
# compared to 41% of voters ages 25+. Age does affect preference.

# Answer to question 2:
poll.long %>%
  group_by(city, preference) %>%
  summarize(
    count = sum(value)
  ) %>%
  mutate(pct = count / sum(count)) %>%
  filter(preference == "yes")

# 47% of Scottish voters from Edinburgh prefered Cullen skink,
# compared to 50% of voters from Glasgow. 
# There's not really a big difference in preference between Edinburgh and Glasgow.

# Answer to question 3:
poll.long %>%
  group_by(age, city, preference) %>%
  summarize(
    count = sum(value)
  ) %>%
  mutate(pct = count / sum(count)) %>%
  filter(preference == "yes")

# Scottish voters ages 16-24 from Glasgow had the highest preference 
# proportionally (70%) for Cullen skink.

# 5. ----
# I prefer 'raw' data where each row is a voter and his/her associated variables--
# city, age, preference--and then I calculate the statistics.
# I would also like to know the interaction effect between age and city.
