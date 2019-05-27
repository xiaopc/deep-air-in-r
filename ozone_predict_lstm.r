# read data
library(readr)
df <- read_csv(sprintf("city-2015-2016-chengdu.csv"))
df.train <- df[,2:6]

df$O3[is.na(df$O3)] <- 0

# remove 1st line (all NA)
df.train <- na.omit(df.train)

# series data to supervised
s_to_super <- function(x, n_in = 1){
  DF <- x
  for (i in 1:n_in) {
    prev <- x[1:(nrow(x) - i),]
    for (j in 1:i) {
      prev <- rbind(NA, prev)
    }
    colnames(prev) <- paste0(colnames(prev), "(t-", i, ")")
    DF <- cbind(prev, DF)
  }
  return(tail(DF, -n_in))
}
df.super <- s_to_super(df.train, 24)

# scale data
#feature_range = c(0, 1)
#scaler <- c(min = min(df.super), max = max(df.super))
#df.std <- (df.super - scaler[1]) / (scaler[2] - scaler[1])
df.min <- apply(df.super, 2, min)
df.max <- apply(df.super, 2, max)
center <- sweep(df.super, 2, df.min, '-') #在列的方向上减去最小值，不加‘-’也行
df.scaled<- sweep(center, 2, df.max - df.min, "/") #把减去均值后的矩阵在列的方向上除以极差向量

# split data into train set and test set
train_percent <- 0.7
N <- nrow(df.scaled)
n <- round(N * train_percent, digits = 0)
train <- df.scaled[1:n, ]
test <- df.scaled[(n+1):N, ]

# split x and y 
y_train = train$O3
x_train = as.matrix(train[, !names(train) %in% c("O3")])
y_test = test$O3
x_test = as.matrix(test[, !names(train) %in% c("O3")])

# reshape dataset
dim(x_train) <- c(nrow(x_train), 1, ncol(x_train))
dim(x_test) <- c(nrow(x_test), 1, ncol(x_test))

# define model
library(keras)
#install_keras()
model <- keras_model_sequential() 
model %>%
  layer_lstm(50, input_shape = c(dim(x_train)[2], dim(x_train)[3])) %>%
  layer_dropout(0.3) %>%
  layer_dense(units = 1, kernel_initializer = 'normal', activation = 'sigmoid')
model %>% compile(
  loss = 'mean_absolute_error',
  optimizer = optimizer_adam(),  
  metrics = c('mse')
)

summary(model)

# training
model %>% fit(x_train, y_train, 
              validation_data = list(x_test, y_test),
              epochs = 50, batch_size = 72, 
              verbose = 2, shuffle = FALSE,
              callbacks = callback_tensorboard("logs/"))
# save model
model %>% save_model_hdf5("ozone_predict_model.h5")

# predicting
yhat <- model %>% predict(x_test)
predict.data <- data.frame(1: nrow(yhat), y_test, yhat)
names(predict.data) <- c("time", "observation", "prediction")

plot.data <- head(predict.data, 96)

# plot
library(ggplot2)
library(ggthemes)
ggplot(plot.data, aes(plot.data$time)) +
  geom_line(aes(y = plot.data$observation, colour = "observation"), size = 1) + 
  geom_point(aes(y = plot.data$observation, colour = "observation"), size = 2) +
  geom_line(aes(y = plot.data$prediction, colour = "prediction"), size = 1) +
  geom_point(aes(y = plot.data$prediction, colour = "prediction"), size = 2) +
  theme_bw() +
  labs(title = "Air Ozone Prediction by RNN and LSTM", x = "time/h", y = "normalized data") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

# calculate root mean squard error
error <- (predict.data$prediction - predict.data$observation) * (df.max["O3"] - df.min["O3"])
rmse <- function(error) {
  sqrt(mean(error^2))
}
rmse(error)
