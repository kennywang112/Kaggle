library(keras)
library(tensorflow)
library(magick)
library(viridis)
tf$compat$v1$disable_eager_execution()

train_dir<-file.path('../input/cat-and-dog/training_set/training_set')
test_dir<-file.path('../input/cat-and-dog/test_set/test_set')
train_cats_dir<-file.path('../input/cat-and-dog/training_set/training_set/cats')

#Set the vgg16 convolution
conv_base<-application_vgg16(weights = 'imagenet',include_top = FALSE,input_shape = c(150,150,3))
summary(conv_base)
model<-keras_model_sequential()%>%
  conv_base%>%layer_flatten()%>%layer_dense(unit=256,activation='relu')%>%layer_dense(units = 1,activation='sigmoid')
summary(model)

#freeze the weight
cat('before freezing:',length(model$trainable_weights),'\n')
freeze_weights(conv_base)
cat('after freezing:',length(model$trainable_weights),'\n')

train_datagen=image_data_generator(
  rescale = 1/255,rotation_range = 40,width_shift_range = 0.2,height_shift_range = 0.2,
  shear_range = 0.2,zoom_range = 0.2,horizontal_flip = TRUE,fill_mode = 'nearest'
)
test_datagen<-image_data_generator(rescale = 1/255)
train_generator<-flow_images_from_directory(
  train_dir,train_datagen,target_size = c(150,150),
  batch_size = 20,class_mode = 'binary'
)

#Using Data Augmentation and plot one of the cat
fnames<-list.files(train_cats_dir,full.names = TRUE)
img_path<-fnames[[2]]
img<-image_load(img_path,target_size = c(150,150))
img_array<-image_to_array(img)
img_array<-array_reshape(img_array,c(1,150,150,3))
img_tensor<-img_array/255
augmentation_generator<-flow_images_from_data(img_array,generator = train_datagen,batch_size = 1)
op<-par(mfrow=c(2,2),pty='s',mar=c(1,0,1,0))
for(i in 1:4){
  batch<-generator_next(augmentation_generator)
  plot(as.raster(batch[1,,,]))
}

#Set compile that using binary crossentropy and rmsprop
#Fit the model that freezed
#Why freeze ? While the model is training , the layer will be initialized randomly , and a large weight update will propagated through the network and destroy previously learned representations
model%>%compile(
  loss='binary_crossentropy',optimizer=optimizer_rmsprop(learning_rate = 2e-5),metrics=c('accuracy')
)
history<-model%>%fit(
  train_generator,steps_per_epoch=100,epochs=30
)
plot(history)

test_generator<-flow_images_from_directory(
  test_dir,test_datagen,target_size = c(150,150),batch_size = 20,class_mode = 'binary'
)
model%>%evaluate(test_generator,steps = 30)

#Fit the model unfreezed and set the learning rate lower,and then increase the epoch to 50
unfreeze_weights(conv_base,from='block3_conv1')
model%>%compile(
  loss='binary_crossentropy',optimizer=optimizer_rmsprop(learning_rate = 1e-5),metrics=c('accuracy')
)
history<-model%>%fit(
  train_generator,steps_per_epoch=100,epochs=50
)
plot(history)
model%>%evaluate(test_generator,steps = 50)

#Which part of cat is important for the model?
#Grad-CAM:This model determine the importance with heatmap !
model<-application_vgg16(weights = 'imagenet')
#Reshape the image
image_121<-file.path('../input/cat-and-dog/training_set/training_set/cats/cat.121.jpg')
image_1927<-file.path('../input/cat-and-dog/training_set/training_set/cats/cat.1927.jpg')
image_3000<-file.path('../input/cat-and-dog/training_set/training_set/cats/cat.3000.jpg')
load<-function(img) {
  img_out<-image_load(img, target_size = c(224, 224)) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, 224, 224, 3))
  return(img_out)
}

grad_cam<-function(model,img_out,y,sample_images){
  preds <- model %>% predict(img_out)
  max=which.max(preds[1,])# which has the biggest possibilities of the image
  cat_output<-model$output[,max]
  last_conv_layer<-model%>%get_layer('block5_conv3')# layers of the feature map
  grads<-k_gradients(cat_output,last_conv_layer$output)[[1]] # the biggest possibilities predict gradient , use block5_conv3 feature map
  pooled_grads<-k_mean(grads,axis=c(1,2,3))
  iterate<-k_function(list(model$input),list(pooled_grads,last_conv_layer$output[1,,,])) # Given a sample image, allows you to access the output feature maps defined pooled_grads and block5_conv3
  c(pooled_grads_value,conv_layer_output_value)%<-%iterate(list(img_out))
  for(i in 1:512){
    conv_layer_output_value[,,i]<-conv_layer_output_value[,,i]*pooled_grads_value[[i]]
  }
  heatmap<-apply(conv_layer_output_value,c(1,2),mean)
  heatmap <- pmax(heatmap, 0)
  heatmap <- heatmap / max(heatmap) #standardization
  write_heatmap <- function(heatmap, filename, width=224, height=224 ,bg = "white", col = terrain.colors(12)) {
    png(filename, width = width, height = height, bg = bg) #output as png
    op = par(mar = c(0,0,0,0))
    on.exit({par(op); dev.off()}, add = TRUE)
    rotate <- function(x) t(apply(x, 2, rev))
    image(rotate(heatmap), axes = FALSE, asp = 1, col = col)
  }
  write_heatmap(heatmap ,paste("image.", y, ".png", sep = ""))
#We have the heatmap , now overlay on original image
  image <- image_read(sample_images)
  info<-image_info(image)
  geometry<-sprintf('%dx%d!',info$width,info$height)
  pal<-col2rgb(viridis(20),alpha = TRUE) # create a blended/transparent version of the heatmap
  alpha<-floor(seq(0,255,length=ncol(pal)))
  pal_col<-rgb(t(pal),alpha = alpha,maxColorValue = 255)
  write_heatmap(heatmap,paste('image.',y,'.png',sep=''),width = 14,height = 14,bg=NA,col=pal_col) # cover the image
  return(geometry)
}

plot_image<-function(sample_images){
  image_heat<-load(sample_images)/255
  geom_heat<-grad_cam(model,image_heat,1,sample_images)
  image_heat<-image_read(paste('image.',1,'.png',sep=''))%>%image_resize(geom_heat,filter='quadratic')
  image_heat<-image_composite(image=image_heat,
                              composite_image = image_convert(image_read(sample_images), colorspace = 'gray')
                              ,operator = 'blend',compose_args = '30')
  plot(image_heat)
}
plot_image(image_121)
plot_image(image_1927)
plot_image(image_3000)
