# source: https://tensorflow.rstudio.com/tutorials/beginners/
# lightly modified by Jay

# load the reticulate library, an R interface to Python
library(reticulate)

# modify the below line of code depending on how you
# 1) installed Python; 2) setup your Python
# virtual environment for the keras & tensorflow package 
# here, the use_condaenv() function specifies which Conda managed Python virtual 
# environment (created on my computer) to use
# more details: https://rstudio.github.io/reticulate/articles/versions.html
use_condaenv("py312", required = TRUE)

# load the keras3 library, a high level API to tensorflow (and other deep
# learning library in Python)
# by default, keras3 uses tensorflow as its backend
library(keras3)

# 1. load data
mnist <- dataset_mnist()

# print number of training and test images
cat("number of training images: ", dim(mnist$train$x)[1], "\n")
cat("number of testing images: ", dim(mnist$test$x)[1], "\n")

# display first 9 images from the training set
# https://stackoverflow.com/questions/37953644/r-image-plot-mnist-dataset
par(mfcol=c(3,3))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:9) { 
  img <- mnist$train$x[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(mnist$train$y[i]))
}

# 2. Re-scale the x variable
mnist$train$x <- mnist$train$x/255
mnist$test$x <- mnist$test$x/255

# 3. modeling
# 3.1 specify a model
model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(28, 28)) %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(10, activation = "softmax")

# 3.2 show the model summary
summary(model)

# 3.3 compile the model
model %>% 
  compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

# 3.4 fit the model
model %>% 
  fit(
    x = mnist$train$x, y = mnist$train$y,
    epochs = 5,
    validation_split = 0.3,
    verbose = 2
  )

# 4. predict using the model
predictions <- predict(model, mnist$test$x)
# head(predictions, 2)

# display first 9 images in the testing set and its model predictions
par(mfcol=c(3,3))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:9) { 
  img <- mnist$test$x[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(which.max(predictions[i, ]) - 1))
}

# 5. evaluate the model / report model performance
model %>% 
  evaluate(mnist$test$x, mnist$test$y, verbose = 0)

