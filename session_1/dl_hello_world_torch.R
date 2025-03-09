# source: https://mlverse.github.io/luz/articles/examples/mnist-cnn.html
# lightly modified by Jay

# Packages ----------------------------------------------------------------
library(torch)
library(torchvision)
library(luz)

set.seed(1)
torch_manual_seed(1)

# Datasets and loaders ----------------------------------------------------
dir <- "./mnist" # caching directory

train_ds <- mnist_dataset(
  dir,
  download = TRUE,
  transform = transform_to_tensor
)

test_ds <- mnist_dataset(
  dir,
  train = FALSE,
  transform = transform_to_tensor
)

train_dl <- dataloader(train_ds, batch_size = 128, shuffle = TRUE)
test_dl <- dataloader(test_ds, batch_size = 128)

# print number of training and test images
cat("number of training images: ", dim(train_ds$data)[1], "\n")
cat("number of testing images: ", dim(test_ds$data)[1], "\n")

# display first 9 images from the training set
# https://stackoverflow.com/questions/37953644/r-image-plot-mnist-dataset
par(mfcol=c(3,3))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:9) { 
  img <- train_ds$data[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(train_ds$targets[i] - 1))
}

# Building the model/network ---------------------------------------------------
net <- nn_module(
  "Net",
  initialize = function() {
    # nn_conv2d(in_channels, out_channels, kernel_size, stride)
    self$conv1 <- nn_conv2d(1, 32, 3, 1)
    self$conv2 <- nn_conv2d(32, 64, 3, 1)
    self$dropout <- nn_dropout(0.5)
    self$fc <- nn_linear(1600, 10)
  },
  forward = function(x) {
    x %>%
      self$conv1() %>% nnf_relu() %>%
      nnf_max_pool2d(2) %>%
      self$conv2() %>% nnf_relu() %>% 
      nnf_max_pool2d(2) %>% 
      torch_flatten(start_dim = 2) %>% 
      self$dropout() %>% 
      self$fc()
  }
)

# Train -------------------------------------------------------------------
fitted <- net %>%
  setup(
    loss = nn_cross_entropy_loss(),
    optimizer = optim_adam,
    metrics = list(
      luz_metric_accuracy()
    )
  ) %>%
  fit(train_dl, epochs = 1, valid_data = test_dl) # just train for 1 epoch

# Making predictions ------------------------------------------------------
preds <- predict(fitted, test_dl)
preds$shape

# display first 9 images in the testing set and its model predictions
par(mfcol=c(3,3))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:9) { 
  img <- test_ds$data[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(as.integer(torch_argmax(preds[i, ])) - 1))
}

# evaluate the model / report model performance
evaluation <- fitted %>% evaluate(data = test_dl)
print(evaluation)
