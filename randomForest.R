#install.packages("caret")
#install.packages("ggplot2")
#install.packages("pROC")
# library(caret)
# library(ggplot2)
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


# Split del dataset in trainset e testset
ind = sample(2, nrow(dataset), replace =  TRUE, prob=c(0.7,0.3))
trainset = dataset[ind == 1,]
testset = dataset[ind == 2,]

# Training del modello con 10-fold-cross-validation e selezione del numero di covariate ideale
train_control = trainControl(method="cv", number=10)
### tunegrid = expand.grid(.mtry=c(1:16))

cat("\n Esecuzione della 10-fold cross validation sull'intero dataset...")
model = train(target~., data=dataset, trControl=train_control, method="rf")
cat("DONE!")

cat("\n\n\n____PERFORMANCE CALCOLATE CON 10 FOLD CROSS VALIDATION____\n\n")
print(model)
print(plot(model))

result= confusionMatrix(model)
print(result)

## Disegno la matrice di confusione
confusion = as.data.frame(result[["table"]])
confusion$Freq = round(confusion$Freq*91.65) # converto da percentuale a confmat cumulativa

print(ggplot(data = confusion, aes(x=Prediction, y=Reference, fill=Freq)) +
        geom_tile()+
        geom_text(label= confusion$Freq, color="white"))

prec_rec_f1(result) # Stampo i valori di Precision, Recall e F-measure


# CURVA ROC
cat("\n\n____CALCOLO DELLA CURVA ROC:____\n\n")

cat("Training di Random Forest sul solo trainset...")
tunegrid = expand.grid(.mtry = 9)
rf = train(target~., data=trainset, tuneGrid=tunegrid, trControl=train_control, method="rf")
cat("DONE!")


rf.predict = predict(rf, testset, type="prob")
par(pty = "s")
print(roc(testset$target ~ rf.predict[,1], plot = TRUE,
          legacy.axes=TRUE, percent = TRUE,
          xlab = "False Positive Percentage", 
          ylab = "True Positive Percentage",
          col = "#377eb8", lwd = 4))




