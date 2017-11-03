library(randomForest)

# create testing and training data
train_data <- rbind(image1, image2)
test_data <- image3

# add a column to indicate whether pixel is cloud
train_data <- add_column(train_data,
                         is_cloud = train_data$label %in% c(-1,0))
test_data <- add_column(test_data,
                         is_cloud = test_data$label %in% c(-1,0))


# train a model on whether a label is a cloud or not
rf_model <- randomForest(is_cloud ~ NDAI + SD + CORR + DF + CF + BF + AF + AN,
                      data = train_data)
pred <- predict(rf_model, newdata = test)