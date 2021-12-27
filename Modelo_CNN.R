library(keras)
library(openxlsx)
library(tensorflow)

library(listarrays)

setwd('C:/Users/Usuario/Desktop/TFM')



data<-read.xlsx('12sp.xlsx')


data$specie<-as.factor(data$specie)

table(data$specie)

## Train y test

set.seed(123)
train<-sample(1:nrow(data),round(4*nrow(data)/5))

data_train<-data[train,]
data_test<-data[-train,]

table(data_train$specie)
table(data_test$specie)

## Imágenes

setwd('C:/Users/Usuario/Desktop/TFM/Imagenes')

im_train<-data_train$filename
im_test<-data_test$filename


  loadpics = function(filenames) {
    a <- lapply(filenames, image_load)
    b <- lapply(a, image_to_array)
    c <- lapply(b,image_array_resize, height = 100, width = 100)
    d <- normalize(c, axis = 1)
    return(d)
    }



train_x<-loadpics(im_train)
test_x<-loadpics(im_test)

setwd('C:/Users/Usuario/Desktop/TFM')


load("test_x_11.rda")
load("train_x_11.rda")


train_y<-to_categorical(as.integer(c(data_train$specie)))
test_y<-to_categorical(as.integer(c(data_test$specie)))


###############

model<- keras_model_sequential() 

model %>% 
  layer_conv_2d(filters = 20, kernel_size = c(3, 3),
                activation = "relu", input_shape = c(100, 100, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = "sigmoid") %>%
  layer_dense(units = 12, activation = "softmax")


summary(modelo)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


model %>% 
  fit(
    train_x, train_y, 
    epochs = 30, 
    batch_size = 56,
    validation_split = .2
    )


model %>%
  evaluate(train_x,train_y)

model %>%
  evaluate(test_x,test_y)
