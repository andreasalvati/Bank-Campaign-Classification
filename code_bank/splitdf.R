# create data frame with random numbers 1 to 100
data <- as.data.frame(round(runif(1000,1,100),digits = 0))

# simple functio to split datasets and set seed
splitdf <- function(dataframe, seed=NULL, percentage=0.8) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  numTrainingSamples <- round(length(index) * percentage)
  trainindex <- sample(index, numTrainingSamples)
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

data <- splitdf(data)