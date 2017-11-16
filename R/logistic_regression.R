library(glm2)
library(caret)
source("R/preprocessing.R")

# We perform 0-1 classification using logistic regression model.
# Outcome variable is the log odds that a given pixel is cloudy.
# Explanatory variables are standardized NDAI, SD, and CORR. 
# Prediction accuracy is assessed using:  
# 1) 3-fold cross validation
# 2) area under the Receiver Operation Curve

# ---------------------------- # 
# Assess fit by 3-fold CV ROC  #
# ---------------------------- # 

# Because there are 3 images, one image is held out for validation
# while the other 2 images are used to train the model and then 
# used to predict the cloudiness in our testing data. 

# filter to only the labelled data (don't train on unknown labels)
images1_filtered <-  images1[!(images1$label == 0), ]
images2_filtered <-  images2[!(images2$label == 0), ]
images3_filtered <-  images3[!(images3$label == 0), ]

# create 3 sets of training and test data 
train_data_23 <- rbind(images2_filtered, images3_filtered)
train_data_13 <- rbind(images1_filtered, images3_filtered)
train_data_12 <- rbind(images1_filtered, images2_filtered)


trainAndEvaluateLR <- function(train_data, test_data){
  # fit logistic regression model on training data
  model <- train(binary_label ~ zNDAI + zSD + zCORR, 
                data = train_data, method = "glm", family = "binomial")
  
  # predict probability on test data
  test_pred <- predict(model, newdata = test_data, 
                        select = c("zNDAI", "zSD", "zCORR"), 
                        type = "prob")
  
  # translate the test set into a vector of 0,1 labels (instead of -1,1)
  test_labels <- test_data$label
  test_labels[test_labels == -1] <- 0
  
  # create a dataframe with the predicted versus true labels
  predicted_and_true_labels_df <- cbind(test_pred[, 2], test_labels)
  colnames(predicted_and_true_labels_df) <- c("predicted", "true")
  
  return(predicted_and_true_labels_df)
}


calculateROCValues <- function(predicted_and_true_labels_df,
                               image_num) {
  # Calculates the TPR and FPR rates necessary to draw an ROC curve.
  #
  # Arguments:
  #   predicted_and_true_labels_df: a dataframe with the predicted
  #     and true values of a LS model, in the form of the returned
  #     variable from trainAndEvaluateLS
  # Returns: tpr and fpr rates for random Forest
  
  # evaluate the performance of the predictions
  pred = prediction(predicted_and_true_labels_df[, 1],
                    predicted_and_true_labels_df[, 2])
  perf = performance(pred, "tpr", "fpr")
  
  # return a dataframe with true positive and false positive rates,
  # appended with a column that tells what image number the prediction
  # is for
  return(cbind(perf@x.values[[1]], perf@y.values[[1]],
               rep(image_num, length(perf@y.values[[1]]))))
}


# Compute AUC # 
# (this section is commented out because 
# it takes a while to run and is not needed to compile Rnw)

# preds <- pred.prob1
# truth <- truth[[1]]
# roc.jumps <-
#   sapply(preds[truth == 0],
#          FUN = function(threshold) {
#            CalculateTPR(threshold, preds, truth)
#          })
# # calculate the average of these false positive rates
# auc <- sum(roc.jumps) / sum(truth ==0)
