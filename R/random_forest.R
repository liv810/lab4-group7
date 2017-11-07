library(randomForest)
library(ROCR)
source("R/classify.R")

# filter to only the labelled data (don't train on unknown labels)
image1_filtered <-  image1 %>% filter(label != 0)
image2_filtered <-  image2 %>% filter(label != 0)
image3_filtered <-  image3 %>% filter(label != 0)

# create testing and training data
train_data_12 <- rbind(image1_filtered, image2_filtered)
train_data_23 <- rbind(image2_filtered, image3_filtered)
train_data_13 <- rbind(image1_filtered, image3_filtered)

trainAndEvaluateRF <- function(train_data, test_data, ntree){
  # Trains and evalutes a random forest model with the inputted
  # number of trees.
  #
  # Arguments:
  #   train_data: the dataframe of training examples
  #   test_data: the dataframe of testing examples
  #   ntree: the number of component trees to follow
  # Returns: dataframe of predicted and true values
  
  rf_model <- randomForest(factor(label) ~ NDAI + SD + CORR,
                           data = train_data, ntree = ntree)
  
  # evaluate the model on the testing data
  test_pred <- predict(rf_model, type = "prob", newdata = test_data)
  
  # translate the test set into a vector of 0,1 labels (instead of -1,1)
  test_labels <- test_data$label
  test_labels[test_labels == -1] <- 0

  # create a dataframe with the predicted versus true labels
  predicted_and_true_labels_df <- cbind(test_pred[,2], test_labels)
  colnames(predicted_and_true_labels_df) <- c("predicted", "true")
  
  return(predicted_and_true_labels_df)
}

calculateROCValues <- function(predicted_and_true_labels_df,
                               image_num) {
  # Calculates the TPR and FPR rates necessary to draw an ROC curve.
  #
  # Arguments:
  #   predicted_and_true_labels_df: a dataframe with the predicted
  #     and true values of a RF model, in the form of the returned
  #     variable from trainAndEvaluateRF
  # Returns: tpr and fpr rates for random Forest
  
  # evaluate the performance of the predictions
  pred = prediction(predicted_and_true_labels_df[,1],
                    predicted_and_true_labels_df[,2])
  perf = performance(pred, "tpr", "fpr")
  
  # return a dataframe with true positive and false positive rates,
  # appended with a column that tells what image number the prediction
  # is for
  return(cbind(perf@x.values[[1]], perf@y.values[[1]],
               rep(image_num, length(perf@y.values[[1]]))))
}

