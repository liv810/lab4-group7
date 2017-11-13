library(dplyr)
library(reshape2)
library(gridExtra)
library(MASS)
library(ROCR)
library(ggplot2)

qda.lda.info <- function(Qimage1,Qimage2,Qimage3){
  # Trains and evalutes QDA and LDA models with three-fold cross-validation
  #
  # Arguments:
  #   images: traning data with label images
  # Returns: plotting information with lda and qda out put
  # Attachments: auc scores from LDA and QDA
  
  images = rbind(Qimage1, Qimage2, Qimage3)
  
  #QDA with one-image-left-out CV
  qda1 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 1))
  qda2 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 2))
  qda3 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 3))
  
  #predict the left-out image with QDA
  qda1.pred = predict(qda1, type="prob",newdata=Qimage1)
  qda2.pred = predict(qda2, type="prob",newdata=Qimage2)
  qda3.pred = predict(qda3, type="prob",newdata=Qimage3)
  
  #ROC values and ROC curves
  qda.roc1 = performance(prediction(qda1.pred$posterior[,2], Qimage1$label), "tpr", "fpr")
  qda.roc2 = performance(prediction(qda2.pred$posterior[,2], Qimage2$label), "tpr", "fpr")
  qda.roc3 = performance(prediction(qda3.pred$posterior[,2], Qimage3$label), "tpr", "fpr")
  
  #AUC scores
  qda.auc1 = performance(prediction(qda1.pred$posterior[,2], Qimage1$label), "auc")@y.values[[1]]
  qda.auc2 = performance(prediction(qda2.pred$posterior[,2], Qimage2$label), "auc")@y.values[[1]]
  qda.auc3 = performance(prediction(qda3.pred$posterior[,2], Qimage3$label), "auc")@y.values[[1]]
  
  #Plot the ROC curve
  qda.cv = rbind(data.frame(TPR = qda.roc1@y.values[[1]], FPR = qda.roc1@x.values[[1]], fold = factor(1)),
                 data.frame(TPR = qda.roc2@y.values[[1]], FPR = qda.roc2@x.values[[1]], fold = factor(2)),
                 data.frame(TPR = qda.roc3@y.values[[1]], FPR = qda.roc3@x.values[[1]], fold = factor(3)))
  
  qda.roc.info = ggplot(qda.cv) +
    geom_line(aes(x=FPR,y=TPR, group=fold, color=fold)) +
    scale_color_discrete(name = "Predictions on Image Num",
                         labels = c("1 (AUC=0.951/0.959)",
                                    "2 (AUC=0.954/0.97)",
                                    "3 (AUC=0.895/0.887)")) +
    ggtitle("QDA")
  
  #lda part
  lda1 = lda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 1))
  lda2 = lda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 2))
  lda3 = lda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 3))
  
  lda1.pred = predict(lda1, type="prob",newdata=Qimage1)
  lda2.pred = predict(lda2, type="prob",newdata=Qimage2)
  lda3.pred = predict(lda3, type="prob",newdata=Qimage3)
  
  lda.roc1 = performance(prediction(lda1.pred$posterior[,2], Qimage1$label), "tpr", "fpr")
  lda.roc2 = performance(prediction(lda2.pred$posterior[,2], Qimage2$label), "tpr", "fpr")
  lda.roc3 = performance(prediction(lda3.pred$posterior[,2], Qimage3$label), "tpr", "fpr")
  
  lda.auc1 = performance(prediction(lda1.pred$posterior[,2], Qimage1$label), "auc")@y.values[[1]]
  lda.auc2 = performance(prediction(lda2.pred$posterior[,2], Qimage2$label), "auc")@y.values[[1]]
  lda.auc3 = performance(prediction(lda3.pred$posterior[,2], Qimage3$label), "auc")@y.values[[1]]
  
  lda.cv = rbind(data.frame(TPR = lda.roc1@y.values[[1]], FPR = lda.roc1@x.values[[1]], fold = factor(1)),
                 data.frame(TPR = lda.roc2@y.values[[1]], FPR = lda.roc2@x.values[[1]], fold = factor(2)),
                 data.frame(TPR = lda.roc3@y.values[[1]], FPR = lda.roc3@x.values[[1]], fold = factor(3)))
  
  lda.roc.info = ggplot(lda.cv) +
    geom_line(aes(x=FPR,y=TPR, group=fold, color=fold)) +
    scale_color_discrete(name = "Predictions on Image Num",
                         labels = c("1 (AUC=0.951/0.959)",
                                    "2 (AUC=0.954/0.97)",
                                    "3 (AUC=0.895/0.887)")) +
    ggtitle("LDA")
    
  return(list(qda.roc.info,lda.roc.info))
}

qda.confustion <- function(Qimage1,Qimage2,Qimage3,method = "qda"){
  # Get the confusion plot for QDA classification
  # Arguments:
  #   images: traning data with label images
  # Returns: confusion plots in the three-fold cv.
  
  images = rbind(Qimage1, Qimage2, Qimage3)
  
  #QDA with one-image-left-out CV
  qda1 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 1))
  qda2 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 2))
  qda3 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 3))
  
  #predict the left-out image with QDA
  qda1.pred = predict(qda1, type="prob",newdata=Qimage1)
  qda2.pred = predict(qda2, type="prob",newdata=Qimage2)
  qda3.pred = predict(qda3, type="prob",newdata=Qimage3)
  
  #ROC values and ROC curves
  qda.roc1 = performance(prediction(qda1.pred$posterior[,2], Qimage1$label), "tpr", "fpr")
  qda.roc2 = performance(prediction(qda2.pred$posterior[,2], Qimage2$label), "tpr", "fpr")
  qda.roc3 = performance(prediction(qda3.pred$posterior[,2], Qimage3$label), "tpr", "fpr")
  
  #Find the best thresholds in the three cross-validations (alpha-values)
  qda.thresh1 = qda.roc1@alpha.values[[1]][which.max(qda.roc1@y.values[[1]] - qda.roc1@x.values[[1]])]
  qda.thresh2 = qda.roc2@alpha.values[[1]][which.max(qda.roc2@y.values[[1]] - qda.roc2@x.values[[1]])]
  qda.thresh3 = qda.roc3@alpha.values[[1]][which.max(qda.roc3@y.values[[1]] - qda.roc3@x.values[[1]])]
  
  #use the mean of the best thresholdes as our classifier
  thresh = mean(c(qda.thresh1,qda.thresh2,qda.thresh3)) #0.21
  
  #Get the confusion plots
  Qimage1 = Qimage1 %>% mutate(posterior = qda1.pred$posterior[,2]) %>% 
    mutate(confusion = ifelse(label == -1,ifelse(posterior < thresh,"TN","FN"),ifelse(posterior >= thresh,"TP","FP")))
  
  Qimage2 = Qimage2 %>% mutate(posterior = qda2.pred$posterior[,2]) %>% 
    mutate(confusion = ifelse(label == -1,ifelse(posterior < thresh,"TN","FN"),ifelse(posterior >= thresh,"TP","FP")))
  
  Qimage3 = Qimage3 %>% mutate(posterior = qda3.pred$posterior[,2]) %>% 
    mutate(confusion = ifelse(label == -1,ifelse(posterior < thresh,"TN","FN"),ifelse(posterior >= thresh,"TP","FP")))
  
  #confusion plot information
  ggconf <- list(geom_point(aes(x=x, y=y, group=confusion, colour=confusion)),
                 scale_colour_manual(values = c("TP" = "white","FP" = "black","TN" = "#1eee2b","FN" = "#ff9428","Unknown" = "#777777")),
                 theme(legend.position="right", aspect.ratio=1))
  
  # theme to remove axis
  no_axis <- theme(axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.y=element_blank(),
                   axis.title.x=element_blank())
  
  #get the confusion plots
  qda1.conf <- ggplot(Qimage1) + ggconf +
    ggtitle("Image 1") + no_axis
  
  qda2.conf <- ggplot(Qimage2) + ggconf +
    ggtitle("Image 2") + no_axis
  
  qda3.conf <- ggplot(Qimage3) + ggconf +
    ggtitle("Image 3") + no_axis
  
  return(list(qda1.conf,qda2.conf,qda3.conf))
}
