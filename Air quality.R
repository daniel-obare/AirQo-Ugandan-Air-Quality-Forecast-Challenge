require(dplyr)
require(tidyr)
require(magrittr)


# cleaning the train data set

temp_new <- paste("temp", seq(1, 121, 1))
precip_new <- paste("precip", seq(1, 121, 1))
humidity_new <- paste("rel_humidity", seq(1, 121, 1))
wind_dir_new <- paste("wind_dir", seq(1, 121, 1))
wind_spd_new <- paste("wind_spd", seq(1, 121, 1))
atmos_new <- paste("atmos_press", seq(1, 121, 1))

trn <- Train %>% tidyr::separate(col = "temp", into = temp_new, sep = ",", convert = TRUE) %>% 
  tidyr::separate(col = "precip", into = precip_new, sep = ",", convert = TRUE) %>% 
  tidyr::separate(col = "rel_humidity", into = humidity_new, sep = ",", convert = TRUE) %>% 
  tidyr::separate(col = "wind_dir", into = wind_dir_new, sep = ",", convert = TRUE) %>% 
  tidyr::separate(col = "wind_spd", into = wind_spd_new, sep = ",", convert = TRUE) %>% 
  tidyr::separate(col = "atmos_press", into = atmos_new, sep = ",", convert = TRUE) %>%
  mutate(location = factor(location)) %>% 
  janitor::clean_names()

# replace missing values in the train set
trn[] <- lapply(trn, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})

# normalize the train data set
normalize <- function(x) {
  return((x-min(x)) / (max(x)-min(x)))
}

train.norm <- as.data.frame(c(trn[1:2], as.data.frame(lapply(trn[3:729], normalize))))

# Cleaning the test data set
tst <- Test %>% tidyr::separate(col = "temp", into = temp_new, sep = ",", convert = TRUE) %>% 
  tidyr::separate(col = "precip", into = precip_new, sep = ",", convert = TRUE) %>% 
  tidyr::separate(col = "rel_humidity", into = humidity_new, sep = ",", convert = TRUE) %>% 
  tidyr::separate(col = "wind_dir", into = wind_dir_new, sep = ",", convert = TRUE) %>% 
  tidyr::separate(col = "wind_spd", into = wind_spd_new, sep = ",", convert = TRUE) %>% 
  tidyr::separate(col = "atmos_press", into = atmos_new, sep = ",", convert = TRUE) %>%
  mutate(location = factor(location)) %>% 
  janitor::clean_names()

# replace missing values in the test set
tst[] <- lapply(tst, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})


# normalize the test data set
test.norm <- as.data.frame(c(tst[1:2], as.data.frame(lapply(tst[3:728], normalize))))

# fit a linear model on train data
# sample 3/4 of train set
set.seed(1234)
index <- sample(1:nrow(train.norm), 0.75*nrow(train.norm))
x <- train.norm[index, ]
 mod <- lm(target~., data = x)


# fit model into train data
fit <- e1071::svm(target~temp_1+precip_1+rel_humidity_1+wind_dir_1+wind_spd_1+atmos_press_1, data = trn, type = "eps-regression", kernel = "radial")
pred <- predict(fit, newdata = trn)

# check for the RMSE
RMSE <- hydroGOF::rmse(trn$target, pred)
RMSE #root mean square error is 38.69183

tst$target <- predict(fit, newdata = tst)
prediction <- tst %>% dplyr::select(1,729)

# export prediction dataframe to a csv file
write.csv(prediction, "prediction.csv")

require(e1071)
optsvm <- tune(method = svm, target~temp_1+precip_1+rel_humidity_1+wind_dir_1+wind_spd_1+atmos_press_1, 
               data = trn, ranges = list(epsilon = seq(0, 1, 0.1), cost = 20:50))
plot(optsvm)








