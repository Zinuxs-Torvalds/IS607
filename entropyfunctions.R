# Project 1: Entropy
# Jordan Erickson

# 1. ---- entropy() function
# entropy takes a vector and returns the entropy of the vector
entropy <- function(x) {
  freqs <- table(x) / length(x) # find probabilities
  freqs.v <- data.frame(freqs)[, 2] # make a vector of probabilities
  
  # set log2 of zero or negtive values to 0
  freqs.v <- ifelse(freqs.v <= 0, 0, freqs.v)
  
  result <- -sum(freqs.v * log2(freqs.v)) # calculate entropy with probabilities
  return(result)
}

# 2. ---- infogain() function
infogain <- function(x, y) {
  partitions <- names(table(y))
  
  # list of ... for each partition
  child <- list(NULL)
  for (i in 1:length(partitions)) {
    child[[i]] <- dataset[y == partitions[i], ]
  } 
  # is there a way to abstract the name of a dataset if it's given in one of the values in the function call???
  # e.g. infogain(NEW.dataset$answer, NEW.dataset$attr1) >> NEW.dataset
  # this code won't work on a call to a data frame not called "dataset"
  
  # list of entropy values for each answer
  child.entropy <- list(NULL)
  for (j in 1:length(child)) {
    child.entropy[[j]] <- entropy(child[[j]]$answer)
  }
  
  freqs <- table(y) / length(y) # find probabilities
  freqs.v <- data.frame(freqs)[, 2] # make a vector of probabilities
  
  # list of entropy values for each partition
  entropy.after.list <- list(NULL)
  for (k in 1:length(child.entropy)) {
    entropy.after.list[[k]] <- freqs.v[k] * child.entropy[[k]]
  }
  
  entropy.after <- sum(unlist(entropy.after.list)) # unlist converts list to vector
  entropy.before <- entropy(x)
  
  information.gain <- entropy.before - entropy.after # information gain
  return(information.gain)
}

# 3. ---- decide() function
# function to abstract x = dataset$target, y = dataset$attributes
decide <- function(df, target) {
  x <- df[, target]
  
  # create a dataset for each attribute
  attribute <- list(NULL)
  for (i in 1:(length(df)-1) ) {
    if (i != target) {
      attribute[[i]] <- df[, i]
    } else {next}
  }
  
  # create a vector of the information gains for each attribute
  gains <- NULL
  for (j in 1:length(attribute)) {
    gains[j] <- infogain(x, attribute[[j]])
  }
  
  # name the gains
  nameIt <- NULL
  for (k in 1:length(gains)) {
    nameIt[k] <- paste0("attr", k)
  }
  names(gains) <- nameIt
  
  # find column number of the attribute that maximizes the information gain
  max <- which(gains == max(gains))
  names(max) <- NULL
  
  # combine into one list
  solution <- list(max, gains)
  names(solution) <- c("max", "gains")
  return(solution)
}

# ---- answers to sample dataset
setwd("~/2_Jordan School/02_IS 607_Data Acquisit Mgmt")
file <- "entropy-test-file.csv"
dataset <- read.csv(file, stringsAsFactors = FALSE, header = TRUE)

entropy(dataset$answer)
infogain(dataset$answer, dataset$attr1)
infogain(dataset$answer, dataset$attr2)
infogain(dataset$answer, dataset$attr3)
decide(dataset, 4)
