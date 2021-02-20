# Stampa a video i risultati di Precision, Recall e F-measure
prec_rec_f1 = function(result) {
  # Precision, recall e f-measure
  confusion = as.data.frame(result[["table"]])
  
  tp = confusion[confusion$Prediction==1 & confusion$Reference==1,]$Freq
  tn = confusion[confusion$Prediction==0 & confusion$Reference==0,]$Freq
  fp = confusion[confusion$Prediction==1 & confusion$Reference==0,]$Freq
  fn = confusion[confusion$Prediction==0 & confusion$Reference==1,]$Freq
  
  precision = tp/(tp+fp)
  recall = tp/(tp+fn)
  f.measure = 2*precision*recall/(precision+recall)
  cat("Precision:", precision, "\nRecall:", recall, "\nF-measure:", f.measure)
}


# Pre processa il dataset rimuovendo gli outliers e 
# convertendo le variabili categoriche
preprocessing_dataset = function(dataset) {
  # Rimozione outliers
  dataset = dataset[dataset$resting.bp.s!=0, ]
  dataset = dataset[dataset$cholesterol!=0,]
  
  # Trasformo il dataset (uso factor sulle variabili categoriche)
  dataset$target = factor(dataset$target)
  dataset$sex = factor(dataset$sex)
  dataset$chest.pain.type = factor(dataset$chest.pain.type)
  dataset$fasting.blood.sugar = factor(dataset$fasting.blood.sugar)
  dataset$resting.ecg = factor(dataset$resting.ecg)
  dataset$exercise.angina = factor(dataset$exercise.angina)
  dataset$ST.slope = factor(dataset$ST.slope)
  
  return(dataset)
}

installer_of_packages = function(){
  # Librerie per "analisi_esplorativa.R"
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require(cowplot)){
    install.packages("cowplot")
    library(cowplot)
  }
  if(!require(scales)){
    install.packages("scales")
    library(scales)
  }
  if(!require(GGally)){
    install.packages("GGally")
    library(GGally)
  }
  
  # Librerie per "NB.R"
  if(!require(e1071)){
    install.packages("e1071")
    library(e1071)
  }
  if(!require(caret)){
    install.packages("caret")
    library(caret)
  }
  if(!require(pROC)){
    install.packages("pROC")
    library(pROC)
  }  
  
  # Librerie per "decisionTree.R"
  if(!require(rpart)){
    install.packages("rpart")
    library(rpart)
  }
  if(!require(rattle)){
    install.packages("rattle")
    library(rattle)
  }
  if(!require(rpart.plot)){
    install.packages("rpart.plot")
    library(rpart.plot)
  }
  if(!require(RColorBrewer)){
    install.packages("RColorBrewer")
    library(RColorBrewer)
  }

}




