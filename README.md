 # **Diagnosi di malattie cardiache tramite tecniche di Machine Learning**
Progetto per esame di Machine Learning, Università degli Studi di Milano-Bicocca, 2021

A cura di Cogo Luca

**ISTRUZIONI:**

La cartella "Codice" contiene il dataset "heart_mix.csv" e una serie di script:

- "analisi_esplorativa.R": esegue tutti i passi dell'analisi esplorativa
- "NaiveBayes.R": effettua il training di un classificatore Naive Bayes e ne effettua i test (test sul testset e 10-fold cross validation + curve ROC)
- "decisionTree.R": effettua il training di un classificatore Decision Tree (CART), ne effettua il pruning e ne effettua i test (test sul testset e 10-fold cross validation + curve ROC)
- "randomForest.R": effettua il training di un classificatore Random Forest e ne effettua i test tramite 10-fold cross validation e curve ROC
- "models_comparison.R": Effettua il training dei tre classificatori (usando gli iperparametri decisi precedentemente) e effettua i test per confrontarli tra loro
- "myFunctions.R" : un semplice file contenente funzioni che vengono riutilizzate dagli script descritti in precedenza, tra cui la funzione che gestisce il caricamento automatico delle librerie

Tutti gl script sono stati realizzati in modo da essere autoconsistenti. In ogni caso, per sicurezza, le librerie utilizzate sono commentate all'inizio di ogni script.

Per ulteriore completezza si riportano qui tutte le dependencies:

- ggplot2
- dplyr
- cowplot
- scales
- GGally
- e1071
- caret
- pROC
- rpart
- rattle
- rpart.plot
- RColorBrewer
