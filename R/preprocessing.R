# data preprocessing

# this function standardizes NDAI, SD, CORR and creates a binary variable
# that takes value 1 if expert label is cloudy and 0 otherwise
func <- function(df) {
  df$zNDAI <- scale(df$NDAI)
  df$zSD <- scale(df$SD)
  df$zCORR <- scale(df$CORR)
  
  df$binary_label <- NA # unknown
  df$binary_label[df$label == -1] <- 0 # clear
  df$binary_label[df$label == 1] <- 1 # cloudy
  df$binary_label <- as.factor(df$binary_label)
  
  df
}

image <- lapply(list(image1, image2, image3), func)
images1 <- image[[1]]
images2 <- image[[2]]
images3 <- image[[3]]

