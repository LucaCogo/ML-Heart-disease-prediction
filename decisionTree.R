#install.packages("rpart")
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("pROC")
# library(rpart)
# library(rattle)
# library(rpart.plot)
# library(RColorBrewer)
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

# Train dell'albero e visualizzazione
decisionTree = rpart(target ~ ., data = trainset, method = "class", parms = list(split = 'information'))
fancyRpartPlot(decisionTree, cex = 0.6)

# Analisi di performance
testset$Prediction = predict(decisionTree, testset, type = "class")
result = confusionMatrix(testset$Prediction, testset[,c("target")])

cat("\n____RISULTATI DELLE PREDIZIONI DELL'ALBERO SUL TESTSET:____\n\n")
print(result)

  ## Disegno la matrice di confusione
confusion = as.data.frame(result[["table"]])
print(ggplot(data = confusion, aes(x=Prediction, y=Reference, fill=Freq)) +
  geom_tile()+
  geom_text(label= confusion$Freq, color="white"))



# Pruning
cat("\n\n____PRUNING____\n")


printcp(decisionTree)
plotcp(decisionTree)

## Pruning offettuato con cp = 0.021 fornisce un buon compromesso tra performance e complessità
prunedDecisionTree = prune(decisionTree, cp= 0.021)
fancyRpartPlot(prunedDecisionTree)

# Analisi di performance pruning

testset$Prediction = predict(prunedDecisionTree, testset, type = "class")
result = confusionMatrix(testset$Prediction, testset[,c("target")])

cat("\n____RISULTATI DELLE PREDIZIONI DELL'ALBERO POTATO SUL TESTSET:____\n\n")
print(result)

  ## Disegno la matrice di confusione
confusion = as.data.frame(result[["table"]])
print(ggplot(data = confusion, aes(x=Prediction, y=Reference, fill=Freq)) +
  geom_tile()+
  geom_text(label= confusion$Freq, color="white"))


# 10 FOLD CROSS VALIDATION DEL MODELLO
cat("\n\n____10 FOLD CROSS VALIDATION____\n\n")


train_control = trainControl(method="cv", number=10)
tGrid = expand.grid(cp = 0.02)
model = train(target~., data=dataset, trControl=train_control, method="rpart", tuneGrid = tGrid)
print(model)

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

tree.predict = predict(prunedDecisionTree, testset, type="prob")
par(pty = "s")
print(roc(testset$target ~ tree.predict[,1], plot = TRUE,
                      legacy.axes=TRUE, percent = TRUE,
                      xlab = "False Positive Percentage", 
                      ylab = "True Positive Percentage",
                      col = "#377eb8", lwd = 4))



