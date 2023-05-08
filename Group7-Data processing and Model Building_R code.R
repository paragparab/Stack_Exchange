library(h2o)
library(pROC)
library(tidytext)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)

# Read the CSV file
post_data <- read.csv("posts.csv")

# Remove rows those have missing values
post_data <- na.omit(post_data)

post_data$is_answered <- ifelse(post_data$AcceptedAnswerId == "", 0, 1)

# splitting the tags into separate columns
posts_new_data <- post_data %>% separate(Tags,c("v1","v2","v3","v4","v5","v6"),"><")

# removing the unwanted characters from the separated columns
posts_new_data$v1 <- gsub('<', '', posts_new_data$v1)
posts_new_data$v1 <- gsub('>', '', posts_new_data$v1)
posts_new_data$v2 <- gsub('<', '', posts_new_data$v2)
posts_new_data$v2 <- gsub('>', '', posts_new_data$v2)
posts_new_data$v3 <- gsub('<', '', posts_new_data$v3)
posts_new_data$v3 <- gsub('>', '', posts_new_data$v3)
posts_new_data$v4 <- gsub('<', '', posts_new_data$v4)
posts_new_data$v4 <- gsub('>', '', posts_new_data$v4)
posts_new_data$v5 <- gsub('<', '', posts_new_data$v5)
posts_new_data$v5 <- gsub('>', '', posts_new_data$v5)
posts_new_data$v6 <- gsub('<', '', posts_new_data$v6)
posts_new_data$v6 <- gsub('>', '', posts_new_data$v6)


# creating label encoding for the separated columns of the tags
posts_new_data$v1 <- as.integer(factor(posts_new_data$v1))
posts_new_data$v2 <- as.integer(factor(posts_new_data$v2))
posts_new_data$v3 <- as.integer(factor(posts_new_data$v3))
posts_new_data$v4 <- as.integer(factor(posts_new_data$v4))
posts_new_data$v5 <- as.integer(factor(posts_new_data$v5))
posts_new_data$v6 <- as.integer(factor(posts_new_data$v6))

# replacing NA with 0 value
posts_new_data$v1[is.na(posts_new_data$v1)] <- 0
posts_new_data$v2[is.na(posts_new_data$v2)] <- 0
posts_new_data$v3[is.na(posts_new_data$v3)] <- 0
posts_new_data$v4[is.na(posts_new_data$v4)] <- 0
posts_new_data$v5[is.na(posts_new_data$v5)] <- 0
posts_new_data$v6[is.na(posts_new_data$v6)] <- 0
posts_new_data$is_answered[is.na(posts_new_data$is_answered)] <- 0
posts_new_data$ViewCount[is.na(posts_new_data$ViewCount)] <- 0


# Save post-processed Posts Data to CSV file
write.csv(posts_new_data, "post_data.csv", row.names = FALSE)

# Confirm that the file was saved successfully
if (file.exists("post_data.csv")) {
  print("File saved successfully!")
}

# Initialize the h2o instance
h2o.init(nthreads = -1)

# set up the h2o HigherEducation frame
posts_new_data <- as.data.frame(as.h2o(posts_new_data))

#Assigning dependent variable
dependent_var <- "is_answered"

# splitting data into training and testing set using 60% as training and 40% as testing
set.seed(1)
train.index <- sample(c(1:dim(posts_new_data)[1]), dim(posts_new_data)[1]*0.6)  
train.df <- posts_new_data[train.index,] %>%
  arrange(train.index)
test.df <- posts_new_data[-train.index,]

# Model name of Random Forest Model
model_id <- "Group7-StackExchange_RF_model.h20"

#Convert the data frames into H2OFrames
train.h2o <- as.h2o(train.df)
test.h2o <- as.h2o(test.df)

# Train the RF model
rf_model <- h2o.randomForest(
  model_id = model_id,
  x = c("ViewCount", "v1", "v2", "v3", "v4","v5","v6"),
  y = dependent_var ,
  training_frame = train.h2o,
  validation_frame = test.h2o,
  ntrees = 500,
  mtries = -1,
  stopping_metric = "mse",
  stopping_rounds = 4,
  stopping_tolerance = 0.03
)

# Save the RF model
h2o.saveModel(rf_model, path = "./", force = TRUE)


# Model name of Deep Learning Model
model_id <- "Group7-StackExchange_DL_model.h20"

# Train the DL model
dl_model <- h2o.deeplearning(
  model_id = model_id,
  x = c("ViewCount", "v1", "v2", "v3", "v4","v5","v6"),
  y = dependent_var ,
  training_frame = train.h2o,
  validation_frame = test.h2o,
  activation = "RectifierWithDropout",
  hidden = c(100,100),
  epochs = 1,
  stopping_rounds = 4,
  stopping_tolerance = 0.03
)

# Save the DL model
h2o.saveModel(dl_model, path = "./", force = TRUE)

# Shutdown h2o cluster
h2o.shutdown()
Y
