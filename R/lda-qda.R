library(dplyr)
library(reshape2)
library(gridExtra)
library(MASS)
library(ROCR)
library(ggplot2)

qda.lda.info <- function(labeled_image1,labeled_image2,labeled_image3){
  # Trains and evalutes QDA and LDA models with three-fold cross-validation,
  # to get the ROC curves for plotting
  #
  # Arguments:
  #   images: traning data with label images
  # Returns: plotting information with lda and qda out put
  # Attachments: auc scores from LDA and QDA
  
  images = rbind(labeled_image1, labeled_image2, labeled_image3)
  
  #QDA with one-image-left-out CV
  qda1 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 1))
  qda2 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 2))
  qda3 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 3))
  
  #predict the left-out image with QDA
  qda1.pred = predict(qda1, type = "prob",newdata = labeled_image1)
  qda2.pred = predict(qda2, type = "prob",newdata = labeled_image2)
  qda3.pred = predict(qda3, type = "prob",newdata = labeled_image3)
  
  #ROC values and ROC curves
  qda.roc1 = performance(prediction(qda1.pred$posterior[, 2],
                                    labeled_image1$label), "tpr", "fpr")
  qda.roc2 = performance(prediction(qda2.pred$posterior[, 2],
                                    labeled_image2$label), "tpr", "fpr")
  qda.roc3 = performance(prediction(qda3.pred$posterior[, 2],
                                    labeled_image3$label), "tpr", "fpr")
  
  #AUC scores
  qda.auc1 = performance(prediction(qda1.pred$posterior[, 2],
                                    labeled_image1$label), "auc")@y.values[[1]]
  qda.auc2 = performance(prediction(qda2.pred$posterior[, 2],
                                    labeled_image2$label), "auc")@y.values[[1]]
  qda.auc3 = performance(prediction(qda3.pred$posterior[, 2],
                                    labeled_image3$label), "auc")@y.values[[1]]
  
  #Plot the ROC curve
  qda.cv = rbind(data.frame(TPR = qda.roc1@y.values[[1]],
                            FPR = qda.roc1@x.values[[1]], fold = factor(1)),
                 data.frame(TPR = qda.roc2@y.values[[1]],
                            FPR = qda.roc2@x.values[[1]], fold = factor(2)),
                 data.frame(TPR = qda.roc3@y.values[[1]],
                            FPR = qda.roc3@x.values[[1]], fold = factor(3)))
  
  #plotting ROC curves  and AUC scores for QDA model
  qda.roc.info = ggplot(qda.cv) +
    geom_line(aes(x = FPR,y = TPR, group = fold, color = fold)) +
    scale_color_discrete(name = "Predictions on Image Num",
                         labels = c("1 (AUC=0.951/0.959)",
                                    "2 (AUC=0.954/0.97)",
                                    "3 (AUC=0.895/0.887)")) +
    ggtitle("QDA")
  
  #LDA with one-image-left-out CV
  lda1 = lda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 1))
  lda2 = lda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 2))
  lda3 = lda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 3))
  
  #predict the left-out image with QDA
  lda1.pred = predict(lda1, type = "prob",newdata = labeled_image1)
  lda2.pred = predict(lda2, type = "prob",newdata = labeled_image2)
  lda3.pred = predict(lda3, type = "prob",newdata = labeled_image3)
  
  #ROC values and ROC curves
  lda.roc1 = performance(prediction(lda1.pred$posterior[, 2],
                                    labeled_image1$label), "tpr", "fpr")
  lda.roc2 = performance(prediction(lda2.pred$posterior[, 2],
                                    labeled_image2$label), "tpr", "fpr")
  lda.roc3 = performance(prediction(lda3.pred$posterior[, 2],
                                    labeled_image3$label), "tpr", "fpr")
  
  #AUC scores
  lda.auc1 = performance(prediction(lda1.pred$posterior[, 2],
                                    labeled_image1$label), "auc")@y.values[[1]]
  lda.auc2 = performance(prediction(lda2.pred$posterior[, 2],
                                    labeled_image2$label), "auc")@y.values[[1]]
  lda.auc3 = performance(prediction(lda3.pred$posterior[, 2],
                                    labeled_image3$label), "auc")@y.values[[1]]
  
  #plotting ROC curves  and AUC scores for LDA model
  lda.cv = rbind(data.frame(TPR = lda.roc1@y.values[[1]],
                            FPR = lda.roc1@x.values[[1]], fold = factor(1)),
                 data.frame(TPR = lda.roc2@y.values[[1]], 
                            FPR = lda.roc2@x.values[[1]], fold = factor(2)),
                 data.frame(TPR = lda.roc3@y.values[[1]],
                            FPR = lda.roc3@x.values[[1]], fold = factor(3)))
  
  lda.roc.info = ggplot(lda.cv) +
    geom_line(aes(x=FPR,y=TPR, group=fold, color=fold)) +
    scale_color_discrete(name = "Predictions on Image Num",
                         labels = c("1 (AUC=0.951/0.959)",
                                    "2 (AUC=0.954/0.97)",
                                    "3 (AUC=0.895/0.887)")) +
    ggtitle("LDA")
    
  #return the two plots
  return(list(qda.roc.info,lda.roc.info))
}

qda.confustion <- function(labeled_image1, labeled_image2, labeled_image3, 
                           method = "qda"){
  # Get the confusion plot for QDA classification
  # Arguments:
  #   images: traning data with label images
  # Returns: confusion plots in the three-fold cv.
  
  images = rbind(labeled_image1, labeled_image2, labeled_image3)
  
  #QDA with one-image-left-out CV
  qda1 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 1))
  qda2 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 2))
  qda3 = qda(label ~ NDAI + SD + CORR, data = images %>% filter(Image != 3))
  
  #predict the left-out image with QDA
  qda1.pred = predict(qda1, type="prob",newdata=labeled_image1)
  qda2.pred = predict(qda2, type="prob",newdata=labeled_image2)
  qda3.pred = predict(qda3, type="prob",newdata=labeled_image3)
  
  #ROC values and ROC curves
  qda.roc1 = performance(prediction(qda1.pred$posterior[,2],
                                    labeled_image1$label), "tpr", "fpr")
  qda.roc2 = performance(prediction(qda2.pred$posterior[,2],
                                    labeled_image2$label), "tpr", "fpr")
  qda.roc3 = performance(prediction(qda3.pred$posterior[,2],
                                    labeled_image3$label), "tpr", "fpr")
  
  #Find the best thresholds in the three cross-validations (alpha-values)
  qda.thresh1 = qda.roc1@alpha.values[[1]][which.max(qda.roc1@y.values[[1]] -
                                                       qda.roc1@x.values[[1]])]
  qda.thresh2 = qda.roc2@alpha.values[[1]][which.max(qda.roc2@y.values[[1]] -
                                                       qda.roc2@x.values[[1]])]
  qda.thresh3 = qda.roc3@alpha.values[[1]][which.max(qda.roc3@y.values[[1]] -
                                                       qda.roc3@x.values[[1]])]
  
  #use the mean of the best thresholdes as our classifier
  thresh = mean(c(qda.thresh1,qda.thresh2,qda.thresh3)) #0.21
  
  #Get the confusion plots
  labeled_image1 = labeled_image1 %>% mutate(posterior =
                                               qda1.pred$posterior[, 2]) %>% 
    mutate(confusion = ifelse(label == -1, ifelse(posterior < thresh,
                                                  "TN", "FP"),
                              ifelse(posterior >= thresh,"TP","FN")))
  
  labeled_image2 = labeled_image2 %>% mutate(posterior =
                                               qda2.pred$posterior[, 2]) %>% 
    mutate(confusion = ifelse(label == -1, ifelse(posterior < thresh,
                                                 "TN", "FP"),
                              ifelse(posterior >= thresh, "TP", "FN")))
  
  labeled_image3 = labeled_image3 %>% mutate(posterior =
                                               qda3.pred$posterior[, 2]) %>% 
    mutate(confusion = ifelse(label == -1, ifelse(posterior < thresh,
                                                  "TN", "FP"), 
                              ifelse(posterior >= thresh, "TP", "FN")))
  
  #confusion plot information
  ggconf <- list(geom_point(aes(x = x, y = y, group = confusion,
                                colour = confusion), alpha = 0.1),
                 scale_colour_manual(values = c("TP" = "gray70",
                                                "FN" = "turquoise2",
                                                "TN" = "lightgoldenrod1",
                                                "FP" = "deeppink",
                                                "Unknown" = "#777777")), 
                 theme(legend.position = "right", aspect.ratio = 1),
                 guides(color = guide_legend(override.aes=list(size = 2.5,
                                                               alpha=0.5))))
  
  # theme to remove axis
  no_axis <- theme(axis.line = element_blank(),
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank())
  
  #get the confusion plots
  qda1.conf <- ggplot(labeled_image1) + ggconf +
    ggtitle("Image 1") + blank_theme + no_axis   
  qda2.conf <- ggplot(labeled_image2) + ggconf +
    ggtitle("Image 2") + blank_theme + no_axis
  qda3.conf <- ggplot(labeled_image3) + ggconf +
    ggtitle("Image 3") + blank_theme + no_axis  

  #return list of two plots
  return(list(qda1.conf, qda2.conf, qda3.conf))
}
