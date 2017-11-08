library(dplyr)
library(reshape2)
library(gridExtra)
library(MASS)
library(ROCR)
library(mvtnorm)
library(ggplot2)

###############################################################################
#  Split Image Function
#

SplitImage <- function(image, xsplit, ysplit){
  # Function takes an image and divides it with xsplit by ysplit grid.
  #   
  # Input:
  #   image - image data frame to be split
  #   xsplit - number of partitions in the x-dimension
  #   ysplit - number of partitions in the y-dimension
  # Output:
  #   A list containing xsplit*ysplit sub-data frames
  #   To call on a particular subset, use the [[]] operator
  # Example:
  #   For example, to split image1 using a 3x4 grid and save it to foo, the 
  #   syntax would read foo <- SplitImage(image1, xsplit = 3, ysplit = 4).
  #   To call on the bottom-left part of the image, we would write
  #   foo[[1]]    
  
  xmin <- min(image$x)
  ymin <- min(image$y)
  
  xmax <- max(image$x)
  ymax <- max(image$y)
  
  # Declare the list of data frames
  subimages <- replicate(xsplit*ysplit, data.frame())
  
  xstep.size <- ceiling((xmax-xmin)/xsplit)
  ystep.size <- ceiling((ymax-ymin)/ysplit)
  
  for(i in 1:xsplit){
    for(j in 1:ysplit){
      
      n <- (i-1)*ysplit + j  # n is the index within the list
      subimages[[n]] <- filter(image, 
                               x >= xmin + (i-1)*xstep.size, 
                               x < xmin + i*xstep.size,
                               y >= ymin + (j-1)*ystep.size, 
                               y < ymin + j*ystep.size)
      
    }
  }
  return(subimages)
}

###############################################################################
#  Image into labels and features function
#

LabelsFeatures <- function(image){
  # Takes image file and returns two data frames, labels and features
  # labels is single column data frame containing the expert lablels
  # features is a three-column data frame containing NDAI, SD, and CORR
  # The two can be called using $labels and $features
  labelled <- filter(image, label != 0)
  labels <- dplyr::select(labelled, label)
  features <- dplyr::select(labelled, NDAI, SD, CORR)
  
  return(list(labels=labels, features=features))
}

###############################################################################
#  Posterior Probabilities
#

Predicted <- function(newdata, training.object){
  # Uses training data posterior to classify new cases
  # Input:
  #   newdata - data frame of cases to be classified
  #   training.object - an LDA or QDA object using training data
  # Output:
  #   A data frame with posterior probabilities of a case being in class 1
  #   and true classification as its columns
  
  pred <- predict(training.object, type="prob",newdata=newdata)
  pred <- data.frame(x = newdata$x,
                     y = newdata$y,
                     posterior = pred$pos[,2], 
                     label = newdata$label)
  
  return(pred)
}
###############################################################################
#  ROC Function
#

Performance <- function(method.object, data, labels){
  # Generates data for ROC plotting.  Uses the output of lda or qda on 
  #   training data to predict and verify on the set of validation data
  # Input:
  #   method.object - an object of class lda or qda
  #   data - a data frame of the validation data set
  #   labels - a vector containing the true labels for the validation data
  # Output:
  #   roc - data frame with the false positive rates and true positive 
  #     rates, and TP/FP counts as its columns
  #   auc - a numeric value with the auc
  #   either can be called with $ on the output
  
  # Extract the posterior probabilities that a data point has class 1
  image.pr <- predict(method.object, type="prob",newdata=data)$posterior[,2]
  
  # Produce an object of class prediction for ROCR
  image.pr <- prediction(image.pr, labels)
  
  # Use ROCR performance method to find TPR and FPR
  roc <- performance(image.pr, "tpr", "fpr")
  
  # Change from S4 class to S3 data frame
  roc <- data.frame(FPR=unlist(roc@x.values),
                    TPR=unlist(roc@y.values),
                    thresh=unlist(roc@alpha.values))
  auc <- unlist(performance(image.pr, "auc")@y.values)
  return(list(roc=roc, auc=auc))
}

###############################################################################
# Class covariance and mean
#

CloudCovMean <- function(data){
  # Finds within-class covariance and mean for being a cloud
  # Input: 
  #   data - a data frame with NDAI, SD, CORR, and labels as its columns
  # Ouput: 
  #   A list with class covariances and means which can be called on
  #   with cloud.cov, cloud.mean
  
  cloud <- dplyr::select(data, NDAI, SD, CORR,label)
  cloud <- filter(cloud, label==1) %>% dplyr::select(-label)
  
  cloud.cov <- cov(cloud)
  
  cloud.mean <- colMeans(cloud)
  
  return(list(cloud.cov=cloud.cov, cloud.mean=cloud.mean))
}

###############################################################################
# Cross-Validation Function
#

KFoldCV <- function(splitimages, method="qda"){
  # Takes the prepartitioned image data set and performs an
  # n-fold cross-validation
  # Input:
  #   splitimages - output of the SplitImage function, a list of data frames
  #     This can also be a list of data frames to be used for cross-validation
  #     e.g. list(image1,image2,image3)
  #   method - string specifying.  currently tested only for lda and qda
  # Output:
  #   predictions - a data frame with the posterior probabilities
  #     for each fold and the predicted class of each case
  #   roc - a data frame with the roc for each fold
  #   auc - a vector specifying the auc for each roc
  
  if (method=="qda"){
    method=qda
  } else if (method=="lda"){
    method=lda
  } else {
    stop('I told you only LDA or QDA would work.  Why did you try something else?
         kappa')
  }
  
  n <- length(splitimages) #number of folds
  
  predictions <- data.frame()
  roc <- data.frame()
  auc <- c()
  
  #foreach(i = 1:n, .export=ls(envir=globalenv())) %dopar%{
  for(i in 1:n){
    
    # Build the training set
    training.set <- data.frame()
    
    for(j in which(1:n != i)){
      training.set <- rbind(training.set, splitimages[[j]])
    }
    
    fold <- factor(i, levels = 1:n)
    im <- LabelsFeatures(splitimages[[i]])
    
    # Train the method
    train <- method(label ~ NDAI + SD + CORR, 
                      data = filter(training.set, label != 0))
    
    # Append the posterior probabilities
    posterior <- data.frame(Predicted(splitimages[[i]], train), fold=fold)
    predictions <- rbind(predictions, posterior)
    
    # Append the ROC curve
    roc.curve <- data.frame(Performance(train, im$features, im$label)$roc,
                            fold=fold)
    roc <- rbind(roc, roc.curve)
    
    # Add AUC to auc vector
    auc[i] <- Performance(train, im$features, im$labels)$auc
    
  }
  return(list(predictions=predictions, roc=roc, auc=auc))
  
  }

###############################################################################
# Confusion Function 
#

Confusion <- function(prob, true.label, thresh=.11){
  # Given a probability that a pixel is a cloud and its true label,
  # returns a confusion value
  # Input:
  #   prob - a posterior probability that a pixel is a cloud
  #   true.label - the true label for the pixel
  #   threshold - the probability cutoff for a pixel being a cloud
  # Output:
  #   A confusion value, correctly labeled (true positives and negatives
  #   return this value), false positive, false negative, or unknown
  if (prob>=thresh && true.label==1) {
    return("True Positive")
  } else if (prob<thresh && true.label==-1){
    return("True Negative")
  } else if (prob>=thresh && true.label==-1){
    return("False Positive")
  } else if (prob<thresh && true.label==1){
    return("False Negative")
  } else {
    return("Unknown")
  }
}

###############################################################################
# Image Reconstruction Function
#

ImageReconstruct <- function(deconstructed, nimages, xsplit, ysplit){
  # Reconstructs the image after cross-validation
  # Input:
  #   deconstructed - the prediction data frame output of KFoldCV
  #   nimages - the number of images used
  #   xsplit - the number of partitions of the x-dimension
  #   ysplit - the number of partitions of the y-dimension
  # Output:
  #   A list of nimages data frames with the columns
  #   x, y, posterior, label, fold, confusion
  
  partitions.per.image <- xsplit*ysplit
  output.names <- c()
  output <- list()
  
  for(image.num in 1:nimages){
    lower <- image.num - 1
    upper <- image.num * partitions.per.image
    output[[image.num]] <- filter(deconstructed, 
                                  as.numeric(fold) > lower,
                                  as.numeric(fold) <= upper) %>%
      rowwise() %>%
      mutate(confusion=Confusion(prob=posterior,
                                 true.label=label))
    output.names[image.num] <- paste("image",image.num,sep="")
  }
  names(output) <- output.names
  
  return(output)
}

###############################################################################

