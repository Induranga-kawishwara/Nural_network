# import libraries
library(readxl)
library(dplyr) 
library(neuralnet)
library(Metrics)
library(grid)
library(gridExtra)

# import dataset
data <- read_excel("F://SEMESTER 02//5DATA001C.2 Machine Learning and Data Mining//CW//MyCW//uow_consumption.xlsx", sheet = 1)

# change column names
names(data)[2] <- 'six_hours'
names(data)[3] <- 'seven_hours'
names(data)[4] <- 'eight_hours'

# change date to numeric
date <-factor(data$date)
date <-as.numeric(date)

# create data frame
uow_dataFrame <- data.frame(date,data$'six_hours',data$'seven_hours',data$'eight_hours')

# plot the eight_hours_column
eight_hours_column <- c(uow_dataFrame$data.eight_hours)
plot(eight_hours_column, type = "l")

# create a I/O  matrix
time_delayed_matrix <- bind_cols(t7 = lag(eight_hours_column,8),
                                 t4 = lag(eight_hours_column,5),
                                 t3 = lag(eight_hours_column,4),
                                 t2 = lag(eight_hours_column,3),
                                 t1 = lag(eight_hours_column,2),
                                 eight_hoursHour = eight_hours_column) 

delayed_matrix <- na.omit(time_delayed_matrix)

# Separating dataset into test and training sets.
train_dataset <- delayed_matrix[1:380,]
test_dataset <- delayed_matrix[381:nrow(delayed_matrix),]

# Determining the minimum and maximum values in the training set
first_min_value <- min(train_dataset)
first_max_value <- max(train_dataset)

# Extracting the output data from the testing dataset
first_data_output <- test_dataset$eight_hoursHour

# normalization function
normalization <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

# Un-normalization function
un_normalization <- function(x, min, max) {
  return( (max - min)*x + min )
}

# The normalization function is used to normalise the data.
time_delayedNormaliz <- as.data.frame(lapply(delayed_matrix[1:ncol(delayed_matrix)], normalization))

# Separating the normalised data into test and train sets
train_datasetNormaliz <- time_delayedNormaliz[1:380,]
test_datasetNormaliz <- time_delayedNormaliz[381:nrow(delayed_matrix),]

# See how the data looks like before and after normalisation with a Boxplot
boxplot(delayed_matrix, main="before normalizing data")
boxplot(time_delayedNormaliz, main="After normalizing data")

# Producing Test Information for Each and EveryDelay
t1_testDataSet <- as.data.frame(test_datasetNormaliz[, c("t1")])
t2_testDataSet <- test_datasetNormaliz[, c("t1", "t2")]
t3_testDataSet <- test_datasetNormaliz[, c("t1", "t2", "t3")]
t4_testDataSet <- test_datasetNormaliz[, c("t1", "t2", "t3", "t4")]
t7_testDataSet <- test_datasetNormaliz[, c("t1", "t2", "t3", "t4", "t7")]

#Function to train an AR model

ModelTrain <- function(formula, hiddenVal, isLinear, actFunc,inputs,hidden){
  
  # Defining a title for the plot
  my_title <- paste(inputs,"inputs and",length(hidden),"hidden layers","(",paste(hidden, collapse=","),") \n")
  
  # Establishing a reproducible seed
  set.seed(1234)
  
  # The neuralnet method is used to train a model of a neural network.
  nural <- neuralnet(formula, data = train_datasetNormaliz, hidden = hiddenVal, act.fct = actFunc, linear.output = isLinear)
  
  # Using a neural network to generate a plot
  plot(nural)
  
  # Keeping the plot in a variable
  plot_panel <- grid.grab(wrap = TRUE)
  
  # Creating a title grob
  plot_title <- textGrob(my_title,
                         x = .5, y = .20,
                         gp = gpar(lineheight_hours = 2,
                                   fontsize = 15, col = 'red',
                                   adj = c(1, 0)
                         )
  )
  
  # Stacking the title and main panel, and plotting
  grid.arrange(
    grobs = list(plot_title,
                 plot_panel),
    height_hourss = unit(c(.15, .85), units = "npc"),
    width = unit(1, "npc")
  )
  dev.new()
  dev.off()
  return(nural)
}

# Define function ModelTest with input arguments nuralModel, testing_df, inputs, hidden
ModelTest <- function(nuralModel, testing_df, inputs, hidden){
  
  # Print the number of inputs and hidden layers specified
  cat("There are", inputs, "inputs and", length(hidden), "hidden layers", "(", paste(hidden, collapse=","), ") \n")
  
  # Create a title for the plots based on the inputs and hidden layers specified
  my_title <- paste(inputs, "inputs and", length(hidden), "hidden layers", "(", paste(hidden, collapse=","), ") \n")
  
  # Use the test data to calculate the output of the neural network.
  nnResults <- compute(nuralModel, testing_df)
  
  # Extract the predicted values and un-normalize them using the min and max values of the original data
  predict <- nnResults$net.result
  unuralormalised_predict <- un_normalization(predict, first_min_value, first_max_value)
  
  # Find the difference between your expected results and the actual ones.
  devia = ((first_data_output - unuralormalised_predict) / first_data_output)
  
  # Calculate the model accuracy as 1 minus the absolute mean deviation
  modelAccu = 1 - abs(mean(devia))
  accuracy = round(modelAccu * 100, digits = 2)
  
  # Plot the predicted vs. actual output values with the un-normalized data
  plot(first_data_output, unuralormalised_predict, col = 'green', main = "Un-normalized Prediction Graph", pch = 18, cex = 0.7)
  mtext(my_title,  side = 3, line = 2, cex = 0.8)
  abline(0,1,lwd=2)
  legend("bottomright", legend = 'neural', pch = 18, col = 'green')
  
  # Create a new plot showing the original output values vs. the predicted values with un-normalized data
  x = 1:length(first_data_output)
  plot(x, first_data_output, col = "red", type = "l", lwd=2, main = "Concrete Strength Prediction")
  mtext(my_title,  side = 3, line = 2, cex = 0.8)
  lines(x, unuralormalised_predict, col = "blue", lwd=2)
  legend("topright", legend = c("original-strength", "predicted-strength"), fill = c("red", "blue"), col = 2:3, adj = c(0, 0.6))
  grid()
  
  # Calculate the RMSE, MAE, MAPE, and sMAPE metrics for the model
  rmse = rmse(first_data_output, unuralormalised_predict)
  mae = mae(first_data_output, unuralormalised_predict)
  mape = mape(first_data_output, unuralormalised_predict)
  smape = smape(first_data_output, unuralormalised_predict)
  
  # Get the model's RMSE, MAE, MAPE, and sMAPE values.
  cat("Model Accuracy:", accuracy, "%\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("MAPE:", mape, "\n")
  cat("sMAPE:", smape, "\n")
  cat("\n\n")
  
  # Return the un-normalized predicted output values
  return(unuralormalised_predict)
}

# t1 with different hidden layer sizes
hidden_layers <- list( c(6),c(5, 3))

# loop through different hidden layer sizes
for (i in seq_along(hidden_layers)) {
  # train model using t1 as input and current hidden layer size
  model <- ModelTrain(eight_hoursHour ~ t1, hidden_layers[[i]], isLinear = FALSE, "tanh",1,hidden_layers[[i]])
  # test model on t1_testDataSet
  pred <- ModelTest(model, t1_testDataSet,1,hidden_layers[[i]])
}



# t2 with different hidden layer sizes
# train model using t1 and t2 as input and hidden layer size of 10
t2_train <- ModelTrain(eight_hoursHour ~ t1 + t2, c(10),isLinear = TRUE, "logistic",2,c(10))
# test model on t2_testDataSet
test_t2_predict <- ModelTest(t2_train, t2_testDataSet,2,c(10))



# t3 with different hidden layer sizes
hidden_layers <- list( c(5),c(10),c(5,4))

# loop through different hidden layer sizes
for (i in seq_along(hidden_layers)) {
  # train model using t1, t2, and t3 as input and current hidden layer size
  model <- ModelTrain(eight_hoursHour ~ t1 + t2 + t3 ,hidden_layers[[i]],isLinear = TRUE, "logistic",3,hidden_layers[[i]])
  # test model on t3_testDataSet
  pred <- ModelTest(model, t3_testDataSet,3,hidden_layers[[i]])
}


# t4 with different hidden layer sizes
hidden_layers <- list( c(5),c(15),c(5,2),c(10,5))

# loop through different hidden layer sizes
for (i in seq_along(hidden_layers)) {
  # train model using t1, t2, t3, and t4 as input and current hidden layer size
  model <- ModelTrain(eight_hoursHour ~ t1 + t2 + t3 + t4,hidden_layers[[i]],isLinear = TRUE, "logistic",4,hidden_layers[[i]])
  # test model on t4_testDataSet
  pred <- ModelTest(model, t4_testDataSet,4,hidden_layers[[i]])
}


# t7 with different hidden layer sizes
hidden_layers <- list( c(5),c(10),c(6,4),c(8,5),c(10,5))

# loop through different hidden layer sizes
for (i in seq_along(hidden_layers)) {
  # train model using t1, t2, t3, t4, and t7 as input and current hidden layer size
  model <- ModelTrain(eight_hoursHour ~ t1 + t2 + t3 + t4 + t7,hidden_layers[[i]],isLinear = TRUE, "logistic",7,hidden_layers[[i]])
  # test model on t7_testDataSet
  pred <- ModelTest(model, t7_testDataSet,7,hidden_layers[[i]])
}


#-------------------------------------------------------------------------------------------------#
# combine six_hours and seven_hours columns
delayed_matrix <- cbind(uow_dataFrame[,2:3], time_delayed_matrix)

# Remove rows with missing values
delayed_matrix <- na.omit(delayed_matrix)

# Separate data into a training set and a testing set.
train_dataset <- delayed_matrix[1:380,]
test_dataset <- delayed_matrix[381:nrow(delayed_matrix),]

# Determine the training dataset's minimum and maximum values.
first_min_value <- min(train_dataset)
first_max_value <- max(train_dataset)

# Get the output data for the test dataset
first_data_output <- test_dataset$eight_hoursHour

# Apply normalization to the dataset
time_delayedNormaliz <- as.data.frame(lapply(delayed_matrix[1:ncol(delayed_matrix)], normalization))

# Separate the normalised data set into a training set and a test set.
train_datasetNormaliz <- time_delayedNormaliz[1:380,]
test_datasetNormaliz <- time_delayedNormaliz[381:nrow(delayed_matrix),]

# Plot boxplots to visualize the data before and after normalization
boxplot(delayed_matrix, main="before normalizating data")
boxplot(time_delayedNormaliz, main="After normalizating data")

# Create testing data for each timeDelay
t1_testDataSet <- test_datasetNormaliz[, c("data.six_hours","data.seven_hours", "t1")]
t2_testDataSet <- test_datasetNormaliz[, c("data.six_hours","data.seven_hours", "t1", "t2")]
t3_testDataSet <- test_datasetNormaliz[, c("data.six_hours","data.seven_hours", "t1", "t2", "t3")]
t4_testDataSet <- test_datasetNormaliz[, c("data.six_hours","data.seven_hours", "t1", "t2", "t3", "t4")]
t7_testDataSet <- test_datasetNormaliz[, c("data.six_hours","data.seven_hours", "t1", "t2", "t3", "t4", "t7")]

# Define the input features and their corresponding test data
inputs <- c("t1", "t2", "t3","t4","t7")
test_dataset <- list(t1_testDataSet, t2_testDataSet, t3_testDataSet, t4_testDataSet, t7_testDataSet)

# Define the different hidden layer configurations to test
h1 <- list(c(5))
h2 <- list(c(10))
h3 <- list(c(15))
h4 <- list(c(10,5))
h5 <- list(c(15,5),c(6,3))
hidden_layers <- list(h1,h2,h3,h4,h5)

# Loop through the different numbers of input variables
for (i in seq_along(inputs)) {
  # Iterate over the possible arrangements of hidden layers given the current number of input variables.
  for(j in seq_along(hidden_layers[[i]])){
    # Get the current input variables
    current_inputs <- inputs[1:i]
    
    # Get the current hidden layer configuration
    currentHidden <- hidden_layers[[i]][[j]]
    
    # Train the model using the current input variables and hidden layer configuration
    formula <- as.formula(paste("eight_hoursHour ~ data.six_hours + data.seven_hours +", paste(current_inputs, collapse="+")))
    model <- ModelTrain(formula, currentHidden, isLinear = TRUE, "logistic", (length(current_inputs)+2), currentHidden)
    
    
    # Test the model using the test dataset and print the predictions
    test_predict <- ModelTest(model, test_dataset[[i]], (length(current_inputs)+2), currentHidden)
  }
}
