library(keras)
library(tensorflow)
#library(kerasR)
library(tcltk)
library(EBImage)

path <- tk_choose.files()
model <- load_model_tf(path, custom_objects=NULL, compile=TRUE)

path <- tk_choose.files()
img <- readImage(path)
display(img)
img <- resize(img, w = 244, h = 244)
display(img)
x <- image_to_array(img)
x <- array_reshape(x, c(1, dim(x)))
preds <- model %>% predict(x)

preds
