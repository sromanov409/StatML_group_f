#install.packages("e1071")
library("e1071")


train_data <- read.csv("proc_data_train.csv")
train_labels <- read.csv("proc_label_train.csv")
test_data <- read.csv("proc_data_test.csv")
test_separation <- read.csv("proc_datasep_test.csv")

set.seed(5)
shuffle <- sample(nrow(train_data))
train_data <- train_data[shuffle,]
train_labels <- train_labels[shuffle,]



model <- svm(x=train_data, y=train_labels[,1], type='C-classification', kernel='radial',cost=10)
summary(model)
predictions <- predict(model, test_data)

actual <- c(0)

for (i in 1:(nrow(test_separation)-1)) {
  vec <- predictions[test_separation[i,1]:test_separation[i+1,1]]
  if (length(which(vec==0))>length(which(vec==1)) & length(which(vec==0))>length(which(vec==2))) {
    actual <- c(actual,0)
  } else if (length(which(vec==1))>=length(which(vec==0)) & length(which(vec==1))>length(which(vec==2))) {
    actual <- c(actual,1)
  } else {
    actual <- c(actual,2)
  }
}

write.csv(actual,"C:/Users/Main/Documents/Work/SML/Term 2/ECG_group_F.csv",row.names= FALSE, col.names = FALSE)