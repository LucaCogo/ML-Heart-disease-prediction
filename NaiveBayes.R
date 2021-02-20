# install.packages("e1071")
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("pROC")
# library(e1071)
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

dataset = read.csv("heart_mix.csv")

dataset = preprocessing_dataset(dataset) # Rimuovo outliers e converto a factor le variabili categoriche

# Split del dataset in trainset e testset
ind = sample(2, nrow(dataset), replace = TRUE, prob=c(0.7, 0.3))
trainset = dataset[ind == 1,]
testset = dataset[ind == 2,]



# Training del calssificatore bayesiano 
nb.classifier = naiveBayes(target ~  .,data= trainset)
# Analisi di performance
test.predicted = predict(nb.classifier, testset) 
result = confusionMatrix(test.predicted, testset[,c("target")])

cat("\n____RISULTATI DELLE PREDIZIONI SUL TESTSET:____\n\n")
print(result)

prec_rec_f1(result)

## Disegno la matrice di confusione
confusion = as.data.frame(result[["table"]])
print(ggplot(data = confusion, aes(x=Prediction, y=Reference, fill=Freq)) +
    geom_tile()+
    geom_text(label= confusion$Freq, color="white"))

# 10 FOLD CROSS VALIDATION DEL MODELLO
cat("\n\n\n____10 CROSS FOLD VALIDATION____\n\n\n")

train_control = trainControl(method="cv", number=10)
tGrid = expand.grid(fL = 0, usekernel = FALSE, adjust = 1)
model = train(target~., data=dataset, trControl=train_control, method="nb", tuneGrid = tGrid)
print(model)

result= confusionMatrix(model)
print(result)

## Disegno la matrice di confusione
confusion = as.data.frame(result[["table"]])
confusion$Freq = round(confusion$Freq*91.65) # converto da percentuale a confmat cumulativa

print(ggplot(data = confusion, aes(x=Prediction, y=Reference, fill=Freq)) +
        geom_tile()+
        geom_text(label= confusion$Freq, color="white"))

prec_rec_f1(result)

# CURVA ROC

cat("\n\n____CALCOLO DELLA CURVA ROC:____\n\n")
nb.predict = predict(nb.classifier, testset, type="raw")
par(pty = "s")
print(roc(testset$target ~ nb.predict[,1], plot = TRUE,
    legacy.axes=TRUE, percent = TRUE,
    xlab = "False Positive Percentage", 
    ylab = "True Positive Percentage",
    col = "#377eb8", lwd = 4))



