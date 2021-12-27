library(caret)
library(tidyverse)
library(recipes)
library(C50)



set.seed(123)
train<-sample(1:nrow(data),round(4*nrow(data)/5))

data_train<-data[train,]
data_test<-data[-train,]


prop.table(table(data_train$specie))

prop.table(table(data_test$specie))


any(!complete.cases(data))

### Varianza proxima a 0

objeto_recipe <- recipe(formula = specie ~ .,
                        data =  data_train)

data %>%
  nearZeroVar(saveMetrics = TRUE)

objeto_recipe <- objeto_recipe %>% step_nzv(all_predictors())

### Estandarización y escalado


objeto_recipe <- objeto_recipe %>% step_center(all_numeric())
objeto_recipe <- objeto_recipe %>% step_scale(all_numeric())


### Binarización variables cualitativas


objeto_recipe <- objeto_recipe %>% step_dummy(all_nominal(), -all_outcomes())


trained_recipe <- prep(objeto_recipe, training = data_train)
trained_recipe


datos_train_prep <- bake(trained_recipe, new_data = data_train)
datos_test_prep  <- bake(trained_recipe, new_data = data_test)

glimpse(datos_train_prep)


##### Selección predictores


registerDoMC(cores = 2)

# Tamaño de los conjuntos de predictores analizados

## Metodo wrapper

subsets <- c(1:26)

# Número de resamples para el proceso de bootstrapping

repeticiones <- 30

set.seed(123)

seeds <- vector(mode = "list", length = repeticiones + 1)

for (i in 1:repeticiones) {
  seeds[[i]] <- sample.int(1000, length(subsets))
} 

seeds[[repeticiones + 1]] <- sample.int(1000, 1)

ctrl_rfe <- rfeControl(functions = rfFuncs, method = "boot", number = repeticiones,
                       returnResamp = "all", allowParallel = TRUE, verbose = FALSE,
                       seeds = seeds)

set.seed(123)
rf_rfe <- rfe(specie ~ ., data = datos_train_prep,
              sizes = subsets,
              metric = "Accuracy",
              # El accuracy es la proporción de clasificaciones correctas
              rfeControl = ctrl_rfe,
              ntree = 500)


rf_rfe
rf_rfe$optVariables


#[1] "hd"              "freq_min"        "fc"              "snr"            
#[5] "freq_end"        "bin_max_amp"     "freq_max_amp"    "freq_center"    
#[9] "kalman_slope"    "freq_knee"       "freq_max"        "freq_start"     
#[13] "bandwidth"       "duration"        "slope"           "pc_freq_min"    
#[17] "mid_offset"      "freq_bw_knee_fc" "pc_freq_max_amp"


datos_test_prep<-datos_test_prep[,c(1,2,3,4,5,6,7,8,9,11,12,14,15,18,19,24,25,27)]
datos_train_prep<-datos_train_prep[,c(1,2,3,4,5,6,7,8,9,11,12,14,15,18,19,24,25,27)]


## K-Nearest Neighbor (kNN)



particiones  <- 10
repeticiones <- 5

# Hiperparámetros

hiperparametros <- data.frame(k = c(1, 2, 5, 10, 15, 20, 30))

set.seed(123)

seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)

for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================

set.seed(123)
modelo_knn <- train(specie ~ ., data = datos_train_prep,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train)
modelo_knn

#k   Accuracy   Kappa    
#1  0.5372500  0.4874654
#2  0.4516711  0.3928062
#5  0.3904087  0.3241464
#10  0.3487068  0.2776278
#15  0.3343121  0.2615836
#20  0.3191545  0.2449168
#30  0.3170813  0.2421261


ggplot(modelo_knn, highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evolución del accuracy del modelo KNN", x = "K") +
  theme_bw()



####### ARBOL DE CLASIFICACION #########

# Hiperparámetros

hiperparametros <- data.frame(parameter = "none")

set.seed(123)

seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)

for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}

seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================

set.seed(123)

modelo_C50Tree <- train(specie ~ ., data = datos_train_prep,
                        method = "C5.0Tree",
                        tuneGrid = hiperparametros,
                        metric = "Accuracy",
                        trControl = control_train)
modelo_C50Tree
summary(modelo_C50Tree$finalModel)

#Accuracy   Kappa    
#0.6188995  0.5779444

########## RANDOM FOREST #############

# Hiperparámetros

hiperparametros <- expand.grid(mtry = c(3, 4, 5, 7),
                               min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
                               splitrule = "gini")

set.seed(123)

seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)

for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(100, nrow(hiperparametros))
}

seeds[[(particiones * repeticiones) + 1]] <- sample.int(100, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================

set.seed(123)

modelo_rf <- train(specie ~ ., data = datos_train_prep,
                   method = "ranger",
                   tuneGrid = hiperparametros,
                   metric = "Accuracy",
                   trControl = control_train,
                   # Número de árboles ajustados
                   num.trees = 500)
modelo_rf

modelo_rf$finalModel

#mtry  min.node.size  Accuracy   Kappa    
#3      2             0.7211864  0.6909112
#3      3             0.7192504  0.6887305
#3      4             0.7175234  0.6867834
#3      5             0.7176054  0.6868753
#3     10             0.7066672  0.6746724
#3     15             0.6948568  0.6614984
#3     20             0.6786773  0.6435223
#3     30             0.6483586  0.6098603
#4      2             0.7236143  0.6936001
#4      3             0.7231205  0.6930047
#4      4             0.7199627  0.6895263
#4      5             0.7194002  0.6888729
#4     10             0.7115321  0.6800666
#4     15             0.6989373  0.6660293
#4     20             0.6841341  0.6496000
#4     30             0.6515103  0.6133213
#5      2             0.7239843  0.6940107
#5      3             0.7223234  0.6921579
#5      4             0.7217623  0.6914815
#5      5             0.7211895  0.6908656
#5     10             0.7119578  0.6804997
#5     15             0.6987917  0.6658856
#5     20             0.6857770  0.6514474
#5     30             0.6530182  0.6150127
#7      2             0.7237636  0.6937552
#7      3             0.7204701  0.6900866
#7      4             0.7221951  0.6920031
#7      5             0.7199119  0.6894018
#7     10             0.7118739  0.6804296
#7     15             0.7006494  0.6679855
#7     20             0.6870085  0.6528055
#7     30             0.6538752  0.6160803

# REPRESENTACIÓN GRÁFICA
# ==============================================================================

ggplot(modelo_rf, highlight = TRUE) +
  scale_x_continuous(breaks = 1:30) +
  labs(title = "Evolución del accuracy del modelo Random Forest") +
  guides(color = guide_legend(title = "mtry"),
         shape = guide_legend(title = "mtry")) +
  theme_bw()



########## NNET ##########

# Hiperparámetros

hiperparametros <- expand.grid(size = c(10, 20, 50, 80, 100, 120),
                               decay = c(0.0001, 0.1, 0.5))

set.seed(123)

seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)

for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}

seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================

set.seed(123)

modelo_nnet <- train(specie ~ ., data = datos_train_prep,
                     method = "nnet",
                     tuneGrid = hiperparametros,
                     metric = "Accuracy",
                     trControl = control_train,
                     # Rango de inicialización de los pesos
                     rang = c(-0.7, 0.7),
                     # Número máximo de pesos
                     # se aumenta para poder incluir más meuronas
                     MaxNWts = 2000,
                     # Para que no se muestre cada iteración por pantalla
                     trace = FALSE)
modelo_nnet

# REPRESENTACIÓN GRÁFICA
# ==============================================================================

ggplot(modelo_nnet, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo NNET") +
  theme_bw()

#size  decay  Accuracy   Kappa    
#10   1e-04  0.3028994  0.2256195
#10   1e-01  0.3150053  0.2392570
#10   5e-01  0.3165119  0.2405037
#20   1e-04  0.3649218  0.2955022
#20   1e-01  0.3691349  0.3004884
#20   5e-01  0.3717767  0.3028989
#50   1e-04  0.3934136  0.3281104
#50   1e-01  0.4073862  0.3434546
#50   5e-01  0.4146973  0.3509512



########## Validación

modelos <- list(KNN = modelo_knn, LDA = modelo_lda, rf = modelo_rf,
                arbol = modelo_C50Tree, NNET = modelo_nnet)

predicciones <- extractPrediction(
  models = modelos,
  testX = datos_test_prep[,-18],
  testY = datos_test_prep$specie
)

predicciones %>% head()

metricas_predicciones <- predicciones %>%
  mutate(acierto = ifelse(obs == pred, TRUE, FALSE)) %>%
  group_by(object, dataType) %>%
  summarise(accuracy = mean(acierto))

metricas_predicciones %>%
  spread(key = dataType, value = accuracy) %>%
  arrange(desc(Test))


#object  Test Training
#  <chr>  <dbl>    <dbl>
#1 rf     0.741    1    
#2 arbol  0.622    0.910
#3 KNN    0.548    1    
#4 NNET   0.412    0.507



####### NAIVE BAYES ###############

# Hiperparámetros

hiperparametros <- data.frame(usekernel = FALSE, fL = 0 , adjust = 0)

set.seed(123)

seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)

for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}

seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================


set.seed(123)
modelo_nb <- train(specie ~ ., data = datos_train_prep,
                   method = "nb",
                   tuneGrid = hiperparametros,
                   metric = "Accuracy",
                   trControl = control_train)

modelo_nb

#Accuracy   Kappa    
#0.2386108  0.1730854




###################### LDA #####################

# Hiperparámetros

hiperparametros <- data.frame(parameter = "none")

set.seed(123)

seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)

for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}

seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================

set.seed(123)

modelo_lda <- train(specie ~ ., data = datos_train_prep,
                    method = "lda",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train)
modelo_lda

modelo_lda$finalModel

#Accuracy   Kappa    
#0.2110249  0.1165223
