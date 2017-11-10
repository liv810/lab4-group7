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
#   Assess fit by 3-fold CV    #
# ---------------------------- # 

# Because there are 3 images, one image is held out for validation
# while the other 2 images are used to train the model and then 
# used to predict the cloudiness in our testing data. 

# create 3 sets of training and test data 
train23 <- rbind(images2, images3)
train13 <- rbind(images1, images3)
train12 <- rbind(images1, images2)

# exclude expert unlabeled pixels 
train23 <- train23[!(train23$label == 0), ]
train13 <- train13[!(train13$label == 0), ]
train12 <- train12[!(train12$label == 0), ]
test1 <- image1[!(image1$label == 0), ]
test2 <- image2[!(image2$label == 0), ]
test3 <- image3[!(image3$label == 0), ]

# fit model on training data (train23, train13, train12)
model23 <- train(binary_label ~ zNDAI + zSD + zCORR,
                 data = train23, method = "glm", family = "binomial")
model13 <- train(binary_label ~ zNDAI + zSD + zCORR,
                 data = train13, method = "glm", family = "binomial")
model12 <- train(binary_label ~ zNDAI + zSD + zCORR,
                 data = train12, method = "glm", family = "binomial")


# obtain coefficients
# exp(coef(model23$finalModel))

# predict on testing data
pred1 = predict(model23, newdata = subset(test1,
                                          select =  c("zNDAI", "zSD", "zCORR")))
pred2 = predict(model13, newdata = subset(test2,
                                         select =  c("zNDAI", "zSD", "zCORR")))
pred3 = predict(model12, newdata = subset(test3,
                                          select =  c("zNDAI", "zSD", "zCORR")))

pred.prob1 = predict(model23, newdata = subset(test1,
                                          select =  c("zNDAI", "zSD", "zCORR")),
                                          type = "prob")
pred.prob2 = predict(model13, newdata = subset(test2,
                                          select =  c("zNDAI", "zSD", "zCORR")),
                                          type = "prob")
pred.prob3 = predict(model12, newdata = subset(test3,
                                          select =  c("zNDAI", "zSD", "zCORR")),
                                          type = "prob")


# make cross tabulation to see accuracy of the prediction
confusionMatrix(data = pred1, test1$binary_label)
confusionMatrix(data = pred2, test2$binary_label)
confusionMatrix(data = pred3, test3$binary_label)



# # ---------------------------- # 
# #      Assess fit by AUC       #
# # ---------------------------- # 
 
source("R/classify.R")

# store predicted probabilities and true labels in a list
preds <- truth <- tprs <- fprs <- list()
preds[[1]] <- pred.prob1[,2]
preds[[2]] <- pred.prob2[,2]
preds[[3]] <- pred.prob3[,2]
truth[[1]] <- as.numeric(levels(test1$binary_label))[test1$binary_label]
truth[[2]] <- as.numeric(levels(test2$binary_label))[test2$binary_label]
truth[[3]] <- as.numeric(levels(test3$binary_label))[test3$binary_label]


# for each image, compute true positive and false positive rates
# at 1000 possible threshold values.
for(i in 1:3){

  # store true positive rate
  tprs[[i]] <- sapply(seq(0, 1, length.out = 1000),
                      FUN = CalculateTPR,
                      preds[[i]], truth[[i]])
  # store false positive rate
  fprs[[i]] <- sapply(seq(0, 1, length.out = 1000),
                 FUN = CalculateFPR,
                 preds[[i]], truth[[i]])
}

# for each image, make tprs and fprs into a dataframe
data1 <- as.data.frame(cbind(tprs[[1]], fprs[[1]], 1))
data2 <- as.data.frame(cbind(tprs[[2]], fprs[[2]], 2))
data3 <- as.data.frame(cbind(tprs[[3]], fprs[[3]], 3))
data <- rbind(data1, data2, data3)
colnames(data) <- c("tprs", "fprs", "group")
data$group <- as.factor(data$group)



# first calculate the TPR using each of the true negative (group 0) predicted 
# probabilities as a threshold
preds <- pred.prob1
truth <- truth[[1]]
roc.jumps <-
  sapply(preds[truth == 0],
         FUN = function(threshold) { 
           CalculateTPR(threshold, preds, truth) 
         })
# calculate the average of these false positive rates
auc <- sum(roc.jumps) / sum(truth ==0)








