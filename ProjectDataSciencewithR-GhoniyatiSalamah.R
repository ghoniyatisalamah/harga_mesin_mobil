## PROJECT DATA SCIENCE WITH R
## Author : Ghoniyati Salamah

## Data yang digunakan adalah mobil_mesin_harga.csv.
## Data tersebut berisi data kekuatan mesin dan harga mobil.
## Akan dilakukan analisis dengan metode CRISP-DM (tanpa deployment).

## 1. Business Understanding :
##    Memprediksi harga mobil berdasarkan kekuatan mesin.
df = read.csv('mobil_mesin_harga.csv')
View(df)

## 2. Data Understanding
## 2.1 Summary
summary(df)

## 2.2 Univariate Analysis
##  a. Analisis Harga Mobil
library(ggplot2)
ggplot(data = df, mapping = aes(x=Harga)) +
  geom_histogram(bins = 10)

##  b. Analisis Kekuatan Mesin
ggplot(data = df, mapping = aes(x=KekuatanMesin)) +
  geom_histogram(bins = 6)

## Insight: - Sebagian besar harga mobil berkisar antara 70-116 juta (bin ke-2)
##          - Sebagian besar kekuatan mesin sekitar 60-110 horsepower (bin ke-2)

## 2.3 Bivariate Analysis
ggplot(data = df, mapping = aes(x=KekuatanMesin, y=Harga)) +
  geom_point() +
  labs(title = "Grafik Relasi Kekuatan Mesin terhadap Harga", 
       x = "Kekuatan Mesin", y = "Harga") +
  theme(plot.title = element_text(hjust = 0.5))

library(corrplot)
corrplot.mixed(cor(df))

## Insight: - Ada relasi kuat antara variabel Kekuatan Mesin dengan Harga
##            (relasi sebesar 0.81)
##          - Semakin kuat mesin suatu mobil, besar kemungkinan 
##            harganya juga semakin meningkat (relasi positif)

## 3. Data Preparation
dim(df) # data memiliki 200 baris dan 2 kolom
## Check Missing Value
sapply(df, function(x)sum(is.na(x))) # tidak ada NULL value

## 4. Modeling : Regression Analysis
## 4.1 Training set and Testing set
## Data frame dibagi menjadi 70% training set dan
## 30% testing set
library(caTools)
set.seed(123)
split = sample.split(df$Harga, SplitRatio = 0.7)
training_set = subset(df,split=="TRUE")
testing_set = subset(df,split=="FALSE")


## 4.2 Model Shaping : Regression Analysis
##     Dibentuk model untuk memprediksi Harga mobil
##     dengan feature Kekuatan Mesin
model_lr <- lm(formula=Harga~KekuatanMesin,
               data=training_set)

## 5. Evaluation
## 5.1 Model Evaluation
prediksi_harga <- predict(model_lr,testing_set)
harga_aktual = testing_set$Harga
table_evaluasi <- data.frame(harga_aktual,prediksi_harga)
View(table_evaluasi)

## 5.2 Model Performance
performance <- function(prediction, actual, method){
  n_data <- length(prediction)
  
  e.mlr <- prediction - actual
  se.mlr <- e.mlr^2 #Squared Error
  sse.mlr <- sum(se.mlr) #Sum squared error
  mse.mlr <- mean(se.mlr) #Mean squared error
  rmse.mlr <- sqrt(mse.mlr) #Root mean squared error
  
  r.mlr <- cor(prediction, actual) 
  result <- paste("Method = ", method,
                  "\nRMSE = ", round(rmse.mlr, 3)/n_data,
                  "\nR    = ", round(r.mlr,3),
                  "\n")
  cat(result)
  #return(result)
}

performance(prediksi_harga,harga_aktual,"Linear Regression")

## Interpretasi:
## 1. RMSE = 0.899416666666667 menunjukkan nilai besarnya variansi
##    model regresi yaitu sebesar 0.899416666666667.
## 2. R = 0.848 menunjukkan derajat hubungan antara variabel
##    Kekuatan Mesin terhadap Harga mobil.
