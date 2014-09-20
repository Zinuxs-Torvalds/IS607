# Week 4 Quiz
# Jordan Erickson

setwd("~/2_Jordan School/02_IS 607_Data Acquisit Mgmt")
m <- read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="")
# internet movie database, IMDB, 58,788 movies
# Variables:
#   title: Title of the movie
#   year: Year of release
#   budget: Total budget (if known) in US dollars
#   length: Length in minutes
#   rating: Average IMDB user rating
#   votes: Number of IMDB users who rated this movie
#   r1-10: Distribution of votes for each rating, to mid point of nearest decile: 0 = no votes, 4.5 = 1-9% v, 14.5 = 11-19% v
#   mpaa: MPAA rating
#   action, animation, comedy, drama, documentary, romance, short -- binary variables representing movie genre

require(ggplot2)
require(scales)
require(reshape2)

# 1. ----
m$decade <- (m$year%/%10)*10 # do integral division to get decade
movies.by.decade <- as.data.frame(table(m$decade), stringsAsFactors = FALSE) # tally movies by decade, make into a df
colnames(movies.by.decade) <- c("decade", "movies") # name vars
movies.by.decade$decade <- as.numeric(movies.by.decade$decade)

ggplot(movies.by.decade, aes(x = decade, y = movies)) + geom_line() + geom_point() +
  scale_y_continuous(labels = comma) + labs(title = "Total number of movies for each decade", 
                                            x = "Decade of release", y = "# of movies") +
  theme(panel.grid.minor.x = element_blank())

# 2. ----
m.genres <- m[, c("title", "year", "decade", "length", "budget", "rating", "votes", "mpaa",
                  "Action","Animation","Comedy","Drama","Documentary","Romance","Short")] # keep vars

m.genres <- melt(m.genres, id.vars = c("title", "year", "decade", "length", "budget", "rating", "votes", "mpaa")) # create df
m.genres <- m.genres[which(m.genres$value == 1), ] # keep rows where "value" == 1
colnames(m.genres)[length(m.genres)-1] <- "genres" # name "variable"
m.genres$value <- NULL # remove "value"

# Warning: genres are not unique to movies.
# As an example, the movie titles "$" is both a Comedy and Drama: m.genres[m.genres$title == "$", ]

aggregate(rating ~ genres, m.genres, mean) # average user rating by genres

m.genres.avg <- aggregate(rating ~ genres + decade, m.genres, mean) # df for average user rating by genres over time
ggplot(m.genres.avg, aes(x = decade, y = rating)) + 
  geom_line(aes(color = m.genres.avg$genres), group = m.genres.avg$genres) +
  labs(title = "Average IMDB user rating by genres", x = "Decade of release", y = "Average user rating") +
  scale_color_discrete(name = "Genres")

# Answer: The average IMDB user rating for different generes has changed over time.

# 3. ----
# boxplot(m$length) # 'length' is highly skewed with many extreme values
quantile(m$length, c(.75, .90, .95, .99)) # 'length' of 170 minutes is the 99th percentile
m.99 <- m[which(m$length <= 170), ] # removed outliers to make graphs easier to read
nrow(m) - nrow(m.99) # removed 567 movies, 1% of total IMDB

# ggplot(m.99) + geom_density(aes(x = length), fill = "grey")
# 'length' is a bimodal distribution, with one mound between 0 and 40 and the other between 40 and 170
# Ideally, we should stratify m.99 into these two (although somewhat arbitrary) distributions

# hist(m.99$rating) # 'rating' seems moderately symmetric-to-slightly left skewed, with a mean = 5.9 and sd = 1.55

# Scatterplot to explore the relationship visually, added linear model regression line
ggplot(m.99, aes(x = length, y = rating)) + geom_point() + geom_smooth(method = "lm")  +
  labs(title = "Scatterplot: relationship between length of movie and rating", x = "Length of movie (in minutes)", y = "Average IMDB user rating")
m.99.SLR <- lm(rating ~ length, data = m.99) # simple linear regression
summary(m.99.SLR)

# Answer: There does not appear to be a substantive relationship between length of movie and movie rating.
# The adj r-sq = 0.004. Although stratifying the data into two two data frames by 'length' may reveal 
# a relationship among the two strata.

# 4. ----
m.genres.99 <- m.genres[which(m.genres$length <= 170), ] # removed outliers to make graphs easier to read
nrow(m.genres) - nrow(m.genres.99) # removed 643 movies

ggplot(m.genres.99, aes(y = length, x = genres)) + geom_boxplot() +
  labs(title = "Boxplot: relationship between length of movie and genre", x = "Genre", y = "Length of movie (in minutes)")
ggplot(m.genres.99, aes(y = length, x = genres)) + geom_violin(fill = "black")  +
  labs(title = "Violin: relationship between length of movie and genre", x = "Genre", y = "Length of movie (in minutes)")

# Answer: There is a relationship between length of movie and genre.
# The majority of Animation and Short genre movies are much shorter than the other genres.
# Action, Drama, and Romance genres seem about the same in length and are longer than the other genres.
# Comedy and Documentary genres are somewhat bimodal in length of movie compared to the other genres.

# 5. ----
# Let's try a multiple regression (linear) model...
m.genres.LM <- lm(votes ~ year + length + budget + rating + mpaa + genres, data = m.genres)
summary(m.genres.LM)
require(coefplot)
coefplot(m.genres.LM)

# Based on a simple linear model (additive, no interactions), the following factors appear to predict # of votes:
#   genres: Action (baseline), Short, Romance, Documentary, Drama, Comedy, Animation; mpaa: R, PG-13; and rating
# The following factors seem to NOT predict # of votes:
#   budget; length; year; and mpaa: PG and NC-17
