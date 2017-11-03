CalculateTPR <- function(threshhold, preds, truth) {
  # a function to calculate the true positive rate
  # (the number of times we correctly predict 1).
  # Args:
  #   threshold: a value above which the prediction is defined to be 1
  #   preds: a vector of predicted probabilities
  #   truth: a 0, 1 vector of true classes
  # Returns: 
  #   a number between 0 and 1 corresponding to the TPR
  as.numeric(sum(preds[truth] > threshhold) / sum(truth))
}

CalculateFPR <- function(threshold, preds, truth) {
  # a function to calculate the false positive rate 
  # (the number of times we incorrectly predict 1).
  # Args:
  #   threshold: a value above which the prediction is defined to be 1
  #   preds: a vector of predicted probabilities
  #   truth: a 0,1 vector of true classes
  # Returns:
  #   a number between 0 and 1 corresponding to the FPR
  as.numeric(sum(preds[!truth] > threshold) / sum(!truth))
}

# Show summary statistics, ROC curve, and AUC for model
EvaluateModel <- function(preds, truth) {
  # Print an ROC curve and return the AUC for a fitted model.
  # Args:
  #  preds: numeric predictions on the held out data
  #  truth: true classifications of the held out data (same length as preds)
  
  # Plot an ROC curve
  
  cat("TPR at threshold of .5: ", CalculateTPR(.5, preds, truth), "\n")
  cat("FPR at threshold of .5: ", CalculateFPR(.5, preds, truth), "\n")
  
  # Get the TPRs and FPRs for all possible thresholds.  
  # (Note that this can be done more efficiently for large data sets.  
  # See "An introduction to ROC analysis" by Fawcett.)
  
  # the following code calculates the true positive rate for 1000 threshold 
  # values thresholds between 0 and 1
  tprs <- sapply(seq(0, 1, length.out = 1000), 
                 FUN = CalculateTPR, 
                 preds, truth)
  
  # the following code calculates the false positive rate for 1000 threshold 
  # values thresholds between 0 and 1
  fprs <- sapply(seq(0, 1, length.out = 1000), 
                 FUN = CalculateFPR, 
                 preds, truth)
  
  # plot an ROC curve for the model 
  print(
    ggplot(data = data.frame(tprs, fprs)) +
      geom_line(aes(x = fprs, y = tprs), color = "cornflowerblue") +
      geom_abline(aes(slope = 1, intercept = 0)) +
      theme_classic()
  )

  # Calculate the AUC
  
  # first calculate the TPR using each of the true negative (group 0) predicted 
  # probabilities as a threshold
  roc.jumps <-
    sapply(preds[!truth],
           FUN = function(threshold) { 
             CalculateTPR(threshold, preds, truth) 
             })
  # calculate the average of these false positive rates
  auc <- sum(roc.jumps) / sum(!truth)
  cat("AUC: ", auc, "\n")
  
  return(roc.jumps)
}