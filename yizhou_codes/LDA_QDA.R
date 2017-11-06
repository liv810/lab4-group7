library(dplyr)
library(reshape2)
library(MASS)
library(ROCR)
library(mvtnorm)
library(ggplot2)

#set working directory
setwd("E:/work/stat215a/lab4-group7/data")

#Split the data into training and testing data
SplitImage <- function(image, xnum, ynum){
  # Input:
  #   image - image data frame to be split
  #   xnum - number of partitions in the x-dimension
  #   ynum - number of partitions in the y-dimension
  # Output:
  #   A list containing xnum*ynum sub-data frames
  
  xmin <- min(image$x)
  ymin <- min(image$y)
  
  xmax <- max(image$x)
  ymax <- max(image$y)
  
  # Declare the list of data frames
  subimages <- replicate(xnum*ynum, data.frame())
  
  xstep.size <- ceiling((xmax-xmin)/xnum)
  ystep.size <- ceiling((ymax-ymin)/ynum)
  
  for(i in 1:xnum){
    for(j in 1:ynum){
      
      n <- (i-1)*ynum + j  # n is the index within the list
      subimages[[n]] <- filter(image, 
                               x >= xmin + (i-1)*xstep.size, 
                               x < xmin + i*xstep.size,
                               y >= ymin + (j-1)*ystep.size, 
                               y < ymin + j*ystep.size)
      
    }
  }
  return(subimages)
}

#Split the image dataset into list of labels and features
LabelsFeatures <- function(image){
  # Takes image file and returns two data frames, labels and features

  labelled <- filter(image, label != 0)
  labels <- select(labelled, label)
  features <- select(labelled, NDAI, SD, CORR)
  return(list(labels=labels, features=features))
}

#get predicted data
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

#Get Roc curve
Performance <- function(method.object, data, labels){
  # Generates data for ROC plotting.  Uses the output of lda or qda on 
  
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


##Training LDA and QDA
images.train <- c(SplitImage(image1,1,1),
                SplitImage(image2,1,1),
                SplitImage(image3,1,1))

lda.cv <- KFoldCV(images.train, method="lda")
qda.cv <- KFoldCV(images.train, method="qda")

#K-Fold CV
KFoldCV <- function(splitimages, method="qda"){ 
  if (method=="qda"){
    analysis=qda
  } else if (method=="lda"){
    analysis=lda
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
    train <- analysis(label ~ NDAI + SD + CORR, 
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

# ROC plot
lda.roc.loo <- ggplot(lda.cv$roc) +
  geom_line(aes(x=FPR,y=TPR, group=fold, color=fold)) +
  scale_colour_discrete(name  ="Image Left Out") +
  ggtitle("ROC for One-Image-Left-Out CV of LDA")

qda.roc.loo <- ggplot(qda.cv$roc) +
  geom_line(aes(x=FPR,y=TPR, group=fold, color=fold)) +
  scale_colour_discrete(name  ="Image Left Out") +
  ggtitle("ROC for One-Image-Left-Out CV of QDA")
