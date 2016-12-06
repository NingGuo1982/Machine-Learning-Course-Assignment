library(caret); library(ggplot2); library(randomForest)
## Load data
fileURL1<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileURL2<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(!file.exists("training")|!file.exists("testing")){
        # create the placeholder file
        td = tempdir()
        # download into the placeholder file 
        tf1 = tempfile(tmpdir=td)
        download.file(fileURL1, tf1)
        pmltraining<- read.csv(tf1)
       
        tf2 = tempfile(tmpdir=td)
        download.file(fileURL2, tf2)
        pmltesting<- read.csv(tf2)     
}
dim(pmltraining)
dim(pmltesting)

## Data pre-processing
nzv<- nearZeroVar(pmltraining)
pmltrainingtemp<- pmltraining[,-nzv]
threshod<- dim(pmltraining)[1]*0.9
badcolumes<- which(apply(is.na(pmltrainingtemp), 2, sum) > threshod)
pmltrainingtidy<- pmltrainingtemp[,-badcolumes]
pmltestingtemp<- pmltesting[,-nzv]
pmltestingtidy<- pmltestingtemp[, -badcolumes]
## Removing non-measurement data
RemoveInx1 <- grepl("X|timestamp|user_name|problem_id", names(pmltrainingtidy))
RemoveInx2 <- grepl("X|timestamp|user_name|problem_id", names(pmltestingtidy))
pmltrainingtidy<- pmltrainingtidy[, which(RemoveInx1==FALSE)]
pmltestingtidy<- pmltestingtidy[, which(RemoveInx2==FALSE)]

## Data splitting for resampling
set.seed(123)
inTrain<- createDataPartition(y=pmltrainingtidy$classe, p=0.7, list = FALSE)
training<- pmltrainingtidy[inTrain,]
testing<- pmltrainingtidy[-inTrain,]

## random forest
modfit1<- randomForest(classe~., data = training)
prediction1<- predict(modfit1, testing)
confusionMatrix(testing$classe, prediction1)
## decision tree
modfit2<- train(classe~., method="rpart", data = training)
prediction2<- predict(modfit2, testing)
confusionMatrix(testing$classe, prediction2)
## LDA
modfit3<- train(classe~.,  method="lda", data = training)
prediction3<- predict(modfit3, testing)
confusionMatrix(testing$classe,  prediction3)
## gbm
modfit4<- train(classe~.,  method="gbm", data = training)
prediction4<- predict(modfit4, testing)
confusionMatrix(testing$classe,  prediction4)

## important variables
varImpPlot(modfit1, n.var = 20, main = "Top20 important variables in random forest modeling")

predanswer<- predict(modfit2, pmltestingtidy)
predanswer
