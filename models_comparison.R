# install.packages("e1071")
# install.packages("caret")
# install.packages("pROC")
# library(e1071)
# library(caret)
# library(pROC)




# Gestione automatica dependencies
if(!require(rstudioapi)){
    install.packages("rstudioapi")
    library(rstudioapi)
}
library(rstudioapi) 
setwd(dirname(getActiveDocumentContext()$path)) 
source("myFunctions.R")
installer_of_packages()

# Lettura del dataset
dataset = read.csv("heart_mix.csv")

dataset = preprocessing_dataset(dataset) # Rimuovo outliers e converto a factor le variabili categoriche
levels(dataset$target)=c("No","Yes") # Modifica necessaria per far funzionare caret 

# Split del dataset in trainset e testset
ind = sample(2, nrow(dataset), replace =  TRUE, prob=c(0.7,0.3))
trainset = dataset[ind == 1,]
testset = dataset[ind == 2,]

# Training

control = trainControl(method = "cv", number = 10,
                       classProbs = TRUE, summaryFunction = twoClassSummary)

    ## Naive Bayes
cat("Training di Naive Bayes... ")
nb.model = train(target~., data=trainset, method="nb", metric="ROC",trControl = control)
cat("DONE!\n")

    ## CART
cat("Training di CART... ")
tGrid = expand.grid(cp = 0.02)
rpart.model = train(target~., data=trainset, method="rpart", tuneGrid = tGrid, metric="ROC",trControl = control)
cat("DONE!\n")

    ## Random Forest
cat("Training di Random Forest... ")
rf.model = train(target~., data=trainset, method="rf", metric="ROC",trControl = control)
cat("DONE!\n")

# Predictions

nb.predict = predict(nb.model, testset, type="prob")
rpart.predict = predict(rpart.model, testset, type="prob")
rf.predict = predict(rf.model, testset, type="prob")


roc(testset$target ~ nb.predict[,1], plot = TRUE,
    legacy.axes=TRUE, percent = TRUE,
    xlab = "False Positive Percentage", 
    ylab = "True Positive Percentage",
    col = "#377eb8", lwd = 2,
    print.auc = TRUE)

roc(testset$target ~ rpart.predict[,1], plot = TRUE,
   legacy.axes=TRUE, percent = TRUE,
   xlab = "False Positive Percentage", 
   ylab = "True Positive Percentage",
   add = TRUE,
   col = "#4daf4a", lwd = 2,
   print.auc = TRUE, print.auc.y = 40)

roc(testset$target ~ rf.predict[,1], plot = TRUE,
    legacy.axes=TRUE, percent = TRUE,
    xlab = "False Positive Percentage", 
    ylab = "True Positive Percentage",
    add = TRUE,
    col = "#cc3433", lwd = 2,
    print.auc = TRUE, print.auc.y = 30)

legend("bottomright",legend = c("Naive Bayes", "CART", "Random Forest"),
        col = c("#377eb8", "#4daf4a", "#cc3433"), cex=0.6, lwd=4)



cv.values = resamples(list(Naive_Bayes = nb.model,CART = rpart.model, Random_Forest= rf.model))
print(bwplot(cv.values, layout = c(3, 1)))

cat("\n\nTempi:\n")
print(cv.values$timings)

