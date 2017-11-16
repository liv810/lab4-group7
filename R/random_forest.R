library(randomForest)
library(ROCR)
source("R/classify.R")

# filter to only the labelled data (don't train on unknown labels)
image1_filtered <- image1 %>% filter(label != 0)
image2_filtered <- image2 %>% filter(label != 0)
image3_filtered <- image3 %>% filter(label != 0)

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
  #     and true values of a RF model, in the form of the returned
  #     variable from trainAndEvaluateRF
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

parameterTuneRF <- function(ntree, maxnodes, mtry) {
  # Takes in a vector of values to try for each below-described
  # parameter of an RF model.
  #
  # Arguments:
  #   ntree: vector of values to try for the number of trees to use
  #   maxnodes: the maximum number of nodes allowed in a component tree
  #   mtry: the number of features to sample at each point of the RF
  # Returns: dataframe of AUC calculations for each parameter
  
  # translate the test set into a vector of 0,1 labels (instead of -1,1)
  test_labels <- image3_filtered$label
  test_labels[test_labels == -1] <- 0
  
  # keep a running vector of parameter values + corresponding AUCs
  parameter_auc_values = c()
  
  # try each combination of feature combinations
  for(ntree_value in ntree){
    for(maxnodes_value in maxnodes){
      for(mtry_value in mtry){
        
        # train an rf_model
        rf_model <- randomForest(factor(label) ~ NDAI + SD + CORR,
                                 data = train_data_12, ntree = ntree_value,
                                 maxnodes = maxnodes_value, mtry = mtry_value)
        # evaluate the model on the testing data
        test_pred <- predict(rf_model, type = "prob",
                             newdata = image3_filtered)
        
        pred = prediction(test_pred[, 2], test_labels)
        perf = performance(pred, "auc")
        
        # append the new parameter values and AUC onto the vector
        parameter_auc_values = c(parameter_auc_values,
                                 ntree_value, maxnodes_value,
                                 mtry_value, perf@y.values)
      }
    }
  }

  # coerce the vector into a data frame
  param_data_frame <- as.data.frame(matrix(parameter_auc_values,
                                           ncol=4, byrow=TRUE))
  # name the columns
  colnames(param_data_frame) <- c("ntree", "maxnodes", "mtry", "AUC")

  return(param_data_frame)
}

plotParameterTuning <- function(parameter) {
  # Plots the average AUC per parameter value of a dataframe
  # returned by the parameterTuneRF() method.
  #
  # Arguments:
  #   parameter: a string of the parameter to plot, e.g. "ntree"
  # Returns: ggplot image of the mean AUC for each value of the parameter
  
  
  # first, find the mean AUC per parameter value
  ggimage <- ggplot(parameter_tuning %>%
                    group_by(param = parameter_tuning[,parameter]) %>%
                    summarize(mean = mean(AUC)),
                    aes(x = param, y = mean)) +
    # add points and lines between the points
    geom_point() +
    geom_line() +
    # add labels
    labs(x = parameter,
         y = "Mean AUC",
         title = paste(parameter, " vs. mean AUC")) +
    # clean up plot a little bit
    theme_bw() +
    # make the title and axes labels smaller
    theme(panel.border = element_blank(),
          legend.position = "right",
          legend.title = element_text(size=9),
          legend.text = element_text(size=9),
          plot.title = element_text(size=9),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9))
  
  return(ggimage)
}