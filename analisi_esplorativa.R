#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("cowplot")
#install.packages("scales")
#install.packages("GGally")
# library(ggplot2)
# library(dplyr)
# library(cowplot)
# library(scales)
# library(GGally)

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

# Calcolo di min, max, quartili, media
basic_statistics = summary(dataset)
cat("STATISTICHE DI BASE:\n\n")
print(basic_statistics)

# Calcolo varianza
var = sapply(dataset, sd)
cat("\n\nVARIANZA:\n\n")
print(var)

# Calcolo frequenza per covariate categoriche

cat("\nCOVARIATE CATEGORICHE:\n\n")
cat("Sesso (0 = Femmina, 1 = Maschio)")
print(table(dataset$sex))

cat("\nChest pain type (1 = dolore anginoso tipico, 2 = dolore anginoso atipico, 3 = dolore non anginoso, 4 = nessun dolore)")
print(table(dataset$chest.pain.type))

cat("\nGlicemia (1 se > 120 mg/dL, 0 altrimenti)")
print(table(dataset$fasting.blood.sugar))

cat("\nElettrocardiogramma (0 = normale, 1 = anomalia ST-T, 2 = ipertrofia ventricolare)")
print(table(dataset$resting.ecg))

cat("\nAngina pectoris dopo esercizi (0 = assente, 1 = presente)")
print(table(dataset$exercise.angina))

cat("\nInclinazione tangente ST durante esercizi (1 = ascendente, 2 =    orizzontale, 3 = discendente)")
print(table(dataset$ST.slope))


# Calcolo della distribuzione delle covariate

dataset.0 = dataset[dataset$target == 0,] #estraggo dal dataset solo gli individui sani
dataset.1 = dataset[dataset$target == 1,] #estraggo dal dataset solo gli individui malati

  ## Distibuzione TARGET
table.target = table(dataset$target)
cat("\n\n DISTRIBUZIONE TARGET:\n")
print(table.target)

lbls = paste(c("Normal", "Heart Disease"), "\n", round(table.target/1190*100, 1), sep="")
colors = c("#1F77B4","#FF7F0E")
pie(table.target, labels = lbls, cex = 1.1, radius = 1, col = colors)

  ## Distribuzione SESSO:
cat("\n\n DISTRIBUZIONE SESSO:\n")
table.sex = table(dataset$sex)
print(table.sex)

lbls = paste(c("Donne", "Uomini"), "\n", round(table.sex/1190*100, 1), sep="")
colors = c("#1F77B4","#FF7F0E")
pie(table.sex, labels = lbls, cex = 1.1, radius = 1, col = colors)


### Bar plot sesso dei soli pazienti sani
hist.sex.0 = ggplot(data=dataset.0, aes(sex)) + 
  geom_bar(col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  labs(title="Distribuzione sesso pazienti sani", x="Sex", y="Count")

### Bar plot sesso dei soli pazienti malati
hist.sex.1 = ggplot(data=dataset.1, aes(sex)) + 
  geom_bar(col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  labs(title="Distribuzione sesso pazienti malati", x="Sex", y="Count")

print(plot_grid(hist.sex.0, hist.sex.1, nrow=2, ncol=1)) # plot delle due distrib.

  ## Distribuzione ETA':

hist.age= ggplot(data=dataset, aes(age)) + 
  geom_histogram(aes(y =..density..), 
                 col="#1F77B4", 
                 fill="#1F77B4", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title="Histogram for Age", x="Age", y="Count")

print(hist.age)

  ### Istogramma eta dei soli pazienti sani 

hist.age.0 = ggplot(data=dataset.0, aes(age)) + 
  geom_histogram(aes(y =..density..), 
                 col="#1F77B4", 
                 fill="#1F77B4", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title="Distribuzione età pazienti sani", x="Age", y="Count")

  ### Istogramma eta dei soli pazienti malati

hist.age.1= ggplot(data=dataset.1, aes(age)) + 
  geom_histogram(aes(y =..density..), 
                 col="#1F77B4", 
                 fill="#1F77B4", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title="Distribuzione età pazienti malati", x="Age", y="Count")

print(plot_grid(hist.age.0, hist.age.1, nrow=2, ncol=1)) # plot delle due distrib.


  ## Distribuzione CHEST PAIN TYPE

    ### Hist di chest pain
hist.chest.pain = ggplot(data=dataset, aes(chest.pain.type)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)),
           col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  scale_y_continuous(labels = percent_format())+
  labs(title="Distribuzione chest pain", x="Chest pain", y="%")
 
    ### Hist di chest pain nei casi di pazienti malati
hist.chest.pain.1 = ggplot(data=dataset.1, aes(chest.pain.type)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)),
           col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  scale_y_continuous(labels = percent_format())+
  labs(title="Distribuzione chest pain pazienti malati", x="Chest pain", y="%")

print(plot_grid(hist.chest.pain, hist.chest.pain.1, nrow=1, ncol=2)) # plot delle due distrib.

  ## Distribuzione FASTING BLOOD SUGAR (GLICEMIA)

    ### Hist di fbs
hist.fbs = ggplot(data=dataset, aes(fasting.blood.sugar)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)),
           col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  scale_y_continuous(labels = percent_format())+
  labs(title="Distribuzione glicemia", x="Glicemia", y="%")

    ### Hist di fbs nei casi di pazienti malati
hist.fbs.1 = ggplot(data=dataset.1, aes(fasting.blood.sugar)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)),
           col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  scale_y_continuous(labels = percent_format())+
  labs(title="Distribuzione glicemia pazienti malati", x="Glicemia", y="%")

print(plot_grid(hist.fbs, hist.fbs.1, nrow=1, ncol=2)) # plot delle due distrib.

  ## Distribuzione resting ecg

    ### Hist di resting ecg
hist.ecg = ggplot(data=dataset, aes(resting.ecg)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)),
           col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  scale_y_continuous(labels = percent_format())+
  labs(title="Distribuzione risultati ECG", x="ECG", y="%")

    ### Hist di resting ecg nei casi di pazienti malati
hist.ecg.1 = ggplot(data=dataset.1, aes(resting.ecg)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)),
           col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  scale_y_continuous(labels = percent_format())+
  labs(title="Distribuzione risultati ECG pazienti malati", x="ECG", y="%")

print(plot_grid(hist.ecg, hist.ecg.1, nrow=1, ncol=2)) # plot delle due distrib.

  ## Distribuzione exercise angina

    ### Hist di exercise angina
hist.angina = ggplot(data=dataset, aes(exercise.angina)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)),
           col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  scale_y_continuous(labels = percent_format())+
  labs(title="Distribuzione casi angina pectoris", x="Angina pectoris", y="%")

    ### Hist di exercise angina nei casi di pazienti malati
hist.angina.1 = ggplot(data=dataset.1, aes(exercise.angina)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)),
           col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  scale_y_continuous(labels = percent_format())+
  labs(title="Distribuzione casi angina pectoris nei pazienti malati", x="Angina pectoris", y="%")

print(plot_grid(hist.angina, hist.angina.1, nrow=1, ncol=2)) # plot delle due distrib.


## Distribuzione ST slope

### Hist di ST slope
hist.ST = ggplot(data=dataset, aes(ST.slope)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)),
           col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  scale_y_continuous(labels = percent_format())+
  labs(title="Distribuzione ST slope", x="ST slope", y="%")

### Hist di ST slope nei casi di pazienti malati
hist.ST.1 = ggplot(data=dataset.1, aes(ST.slope)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)),
           col="#1F77B4", 
           fill="#1F77B4", 
           alpha=.2)+ 
  scale_y_continuous(labels = percent_format())+
  labs(title="Distribuzione ST slope nei pazienti malati", x="ST slope", y="%")

print(plot_grid(hist.ST, hist.ST.1, nrow=1, ncol=2)) # plot delle due distrib.



# SCATTERPLOT MATRIX DELLE VARIABILI NUMERICHE:
  
  ## Creo il subset di covariate numeriche
sub = subset(dataset, select = c("age", "resting.bp.s", "cholesterol", "max.heart.rate", "oldpeak", "target"))
  ## Realizzo lo scatterplot
print(ggpairs(sub[,1:5], mapping = aes(color = factor(sub$target))))



# OUTLIERS DETECTION

boxplot(dataset$age, main="Eta")
boxplot(dataset$resting.bp.s, main="Pressione")
boxplot(dataset$cholesterol, main="Colesterolo")
boxplot(dataset$max.heart.rate, main="Heart Rate")
boxplot(dataset$oldpeak, main="Sottoslivellam. ST")

  ## Rimozione outliers (rimuovo istanze con pressione o colesterolo a 0)
dataset = dataset[dataset$resting.bp.s!=0, ]
dataset = dataset[dataset$cholesterol!=0,]

cat("Il dataset è stato ridotto a", nrow(dataset), "istanze a seguito della rimozione degli outliers")





