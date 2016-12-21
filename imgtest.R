### Image test
library(magrittr)
library(jpeg)
# library(plyr)     # for rbind.fill()
library(caret)

# load data
files_m <- list.files("~/Desktop/images_df/male", full.names=TRUE)
files_f <- list.files("~/Desktop/images_df/female", full.names=TRUE)

male <- lapply(files_m, readJPEG)
female <- lapply(files_f, readJPEG)

## data wrangling
# male
mpx <- NULL
for (i in 1:length(male)) {
    mpx[i] <- dim(male[[i]]) %>% 
        prod()
}

mdata <- matrix(NA, nrow=length(male), ncol=max(mpx)) %>% 
    as.data.frame()
for (i in 1:length(male)) {
    length(male[[i]]) <- max(mpx)
    mdata[i,] <- unlist(male[[i]])
}
mdata <- cbind(rep("male", length(male)), mdata)

# female
fpx <- NULL
for (i in 1:length(female)) {
    fpx[i] <- dim(female[[i]]) %>% 
        prod()
}

fdata <- matrix(NA, nrow=length(female), ncol=max(fpx)) %>% 
    as.data.frame()
for (i in 1:length(female)) {
    length(female[[i]]) <- max(fpx)
    fdata[i,] <- unlist(female[[i]])
}
fdata <- cbind(as.character(rep("female", length(female))), fdata)

# combine
empty <- matrix(NA, nrow=dim(mdata)[1], ncol=dim(fdata)[2]-dim(mdata)[2]) %>% 
    as.data.frame()
mdata <- cbind(mdata, empty) %>% 
    as.matrix()
mdata[,2:dim(mdata)[2]] <- as.numeric(mdata[,2:dim(mdata)[2]])
names(fdata)[1] <- "gender"
names(mdata) <- names(fdata)
data <- rbind(mdata, fdata)

# data <- rbind.fill(mdata, fdata)
rm(male, female, fpx, mpx, files_f, files_m, i, fdata, mdata, empty)


## machine learning
# split into train/test-set
set.seed(500)
index <- createDataPartition(data[,1], p=.7, list=F)
training <- data[index,]
testing <- data[-index,]

model <- train(y ~ ., data=training, method="nnet")
model$finalModel

predictions <- predict(model, newdata=testing)
confusionMatrix(predictions, testing$gender)

