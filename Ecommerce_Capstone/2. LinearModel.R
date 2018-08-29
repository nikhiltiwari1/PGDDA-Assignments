#########################################################################################
## Linear Regression - Camera
########################################################################################
linear_camera <- CameraAccessory
linear_camera$week_no <- NULL
linear_camera$Month <- NULL

#Scaling the dataset
linear_camera[,1:13] <- data.frame(scale(linear_camera[,1:13], center = TRUE))

set.seed(100)
trainindices= sample(1:nrow(linear_camera), 0.7*nrow(linear_camera))
#Generate the train data set
train = linear_camera[trainindices,]
#Similarly store the rest of the observations into an object "test".
test = linear_camera[-trainindices,]

cam_model1 <- lm(tot_gmv~. , data = train)
summary(cam_model1)
alias(cam_model1)

step_cam_model1 <- stepAIC(cam_model1, direction = "both")
summary(step_cam_model1)
vif(step_cam_model1)
alias(step_cam_model1)

#Content.Marketing
step_cam_model2 <- lm(tot_gmv ~ list_price + TV + Sponsorship + 
                        X.Affiliates + SEM + NPS + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xEid...Rathayatra.sale + promotion_type.xNo_promotion + 
                        promotion_type.xRakshabandhan.Sale , data = train)
summary(step_cam_model2)
vif(step_cam_model2)

#Low significance and high VIF, all the variables have been removed step by step
#list_price, NPS, promotion_type.xEid...Rathayatra.sale, CameraBag, xCameraTripod, CameraBattery
step_cam_model3 <- lm(tot_gmv ~ TV + Sponsorship + 
                        X.Affiliates + SEM + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale , data = train)
summary(step_cam_model3)
vif(step_cam_model3)

#Low significance and high VIF, all the variables have been removed step by step
#Flash, Teleconverter, xCameraLEDLight, xFlashShoeAdapter,xReflectorUmbrella,xNo_promotion,New.Year.Sale
step_cam_model4 <- lm(tot_gmv ~ TV + Sponsorship + 
                        X.Affiliates + SEM + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xDaussera.sale + 
                        promotion_type.xRakshabandhan.Sale , data = train)
summary(step_cam_model4)
vif(step_cam_model4)


#discount_over_mrp
step_cam_model5 <- lm(tot_gmv ~ TV + Sponsorship + 
                        X.Affiliates + SEM + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xDaussera.sale + 
                        promotion_type.xRakshabandhan.Sale , data = train)
summary(step_cam_model5)
vif(step_cam_model5)

########################################################
# Validation

gmv_prediction <- predict(step_cam_model5, test)
test$predicted_gmv <- gmv_prediction

cam_r <- cor(test$tot_gmv, test$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared
#R squared on test 0.839
###################################
#Estimating elasticity 

cam_final_model <- step_cam_model5

elasticity <- function(var) {
  x <- as.numeric(cam_final_model$coefficients[var])
  return(x)
}

var_list <- list()

for(i in 2:length(cam_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(cam_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(cam_final_model$coefficients[2:length(cam_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Linear Model") +xlab("Variables")
############################################################################################
#Cross validation

cam_lm_cv <- cv.lm(data = train, form.lm = formula(tot_gmv ~ TV + Sponsorship + 
                                                     X.Affiliates + SEM + product_analytic_vertical.xCameraAccessory + 
                                                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                                                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                                                     product_analytic_vertical.xCameraHousing + 
                                                     product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                                                     product_analytic_vertical.xCameraRemoteControl + 
                                                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                                                     product_analytic_vertical.xLens + 
                                                     product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                                                     product_analytic_vertical.xTelescope + promotion_type.xDaussera.sale + 
                                                     promotion_type.xRakshabandhan.Sale), m =10)

#ms = 0.236
#########################################################################################
## Linear Regression - GamingAccessory
########################################################################################
linear_game <- GamingAccessory
linear_game$week_no <- NULL
linear_game$Month <- NULL

linear_game[,1:13] <- data.frame(scale(linear_game[,1:13], center = TRUE))
set.seed(100)

trainindices = sample(1:nrow(linear_game), 0.7*nrow(linear_game))
#Generate the train data set
train_game = linear_game[trainindices,]
#Similarly store the rest of the observations into an object "test".
test_game = linear_game[-trainindices,]

game_model1 <- lm(tot_gmv~. , data = train_game)
summary(game_model1)
alias(game_model1)

step_game_model1 <- stepAIC(game_model1, direction = "both")
summary(step_game_model1)
vif(step_game_model1)
alias(step_game_model1)

#Online.marketing
step_game_model2 <- lm(tot_gmv ~ TV + Digital + Sponsorship + Content.Marketing + 
                         X.Affiliates + Radio + Other + NPS + product_analytic_vertical.xCoolingPad + 
                         product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingChargingStation + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xEid...Rathayatra.sale + promotion_type.xPacman + 
                         promotion_type.xRepublic.Day , data = train_game)
summary(step_game_model2)
vif(step_game_model2)

#Low significance and high VIF, all the variables have been removed step by step
#Other, X.Affiliates, Big.Diwali.Sale, Rathayatra, TV
step_game_model3 <- lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                         Radio + NPS + product_analytic_vertical.xCoolingPad + 
                         product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingChargingStation + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xPacman + 
                         promotion_type.xRepublic.Day , data = train_game)
summary(step_game_model3)
vif(step_game_model3)


#Low significance and high VIF, all the variables have been removed step by step
#Radio, xPacman, Republic.Day, xGamingChargingStation, xCoolingPad
step_game_model4 <- lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                         NPS + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xDaussera.sale, data = train_game)
summary(step_game_model4)
vif(step_game_model4)

#Low significance and high VIF, all the variables have been removed step by step
#xGamingSpeaker, Content.Marketing, NPS
step_game_model5 <- lm(tot_gmv ~ Digital + Sponsorship + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         promotion_type.xDaussera.sale, data = train_game)
summary(step_game_model5)
vif(step_game_model5)

########################################################
# Validation

gmv_game_prediction <- predict(step_game_model5, test_game)
test_game$predicted_gmv <- gmv_game_prediction

cam_r <- cor(test_game$tot_gmv, test_game$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared
#R squared on test 0.544
###################################
#Estimating elasticity 

game_final_model <- step_game_model5

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var])
  return(x)
}

var_list_game <- list()

for(i in 2:length(game_final_model$coefficients)) {
  var_list_game[i-1] <- elasticity(names(game_final_model$coefficients)[i])
}

elasticity.outputs.game <- data.frame(names(game_final_model$coefficients[2:length(game_final_model$coefficients)]))
elasticity.outputs.game <- cbind(elasticity.outputs.game,do.call(rbind.data.frame, var_list_game))
colnames(elasticity.outputs.game) <- c("Variable","Elasticity")

elasticity.outputs.game$direction <- ifelse(elasticity.outputs.game$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs.game, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Linear Model") +xlab("Variables")

############################################################################################
#Cross validation

game_lm_cv <- cv.lm(data = train_game , form.lm = formula(tot_gmv ~ Digital + Sponsorship + 
                                                            product_analytic_vertical.xGamePad + 
                                                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                                                            product_analytic_vertical.xGamingMouse + 
                                                            promotion_type.xDaussera.sale), m =10)


#ms 0.297
#########################################################################################
## Linear Regression - HomeAudio
########################################################################################
linear_home <- HomeAudio
linear_home$week_no <- NULL
linear_home$Month <- NULL

linear_home[,1:13] <- data.frame(scale(linear_home[,1:13], center = TRUE))

set.seed(100)
trainindices= sample(1:nrow(linear_home), 0.7*nrow(linear_home))
#Generate the train data set
train_home = linear_home[trainindices,]
#Similarly store the rest of the observations into an object "test".
test_home = linear_home[-trainindices,]

home_model <- lm(tot_gmv~. , data = train_home)
summary(home_model)
alias(home_model)

step_home_model1 <- stepAIC(home_model, direction = "both")
summary(step_home_model1)
vif(step_home_model1)
alias(step_home_model1)

#Low significance and high VIF, all the variables have been removed step by step
#xHiFiSystem,xSoundMixer,xBSD
step_home_model2 <- lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + Online.marketing + 
                         X.Affiliates + Other + NPS + product_analytic_vertical.xDJController + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                         product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker + 
                         promotion_type.xBig.Diwali.Sale + 
                         promotion_type.xEid...Rathayatra.sale , data = train_home)
summary(step_home_model2)
vif(step_home_model2)

#Low significance and high VIF, all the variables have been removed step by step
#Online.marketing,Affiliates, Other,xDJController,NPS
step_home_model3 <- lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                         product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker + 
                         promotion_type.xBig.Diwali.Sale + 
                         promotion_type.xEid...Rathayatra.sale , data = train_home)
summary(step_home_model3)
vif(step_home_model3)


#Low significance and high VIF, all the variables have been removed step by step
#xDockingStation,xDock
step_home_model4 <- lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                         product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker + 
                         promotion_type.xBig.Diwali.Sale + 
                         promotion_type.xEid...Rathayatra.sale , data = train_home)
summary(step_home_model4)
vif(step_home_model4)

########################################################
# Validation

gmv_home_prediction <- predict(step_home_model4, test_home)
test_home$predicted_gmv <- gmv_home_prediction

cam_r <- cor(test_home$tot_gmv, test_home$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared
#Accuracy on test 0.813
###################################
#Estimating elasticity 

home_final_model <- step_home_model4

elasticity <- function(var) {
  x <- as.numeric(home_final_model$coefficients[var])
  return(x)
}

var_list_home <- list()

for(i in 2:length(home_final_model$coefficients)) {
  var_list_home[i-1] <- elasticity(names(home_final_model$coefficients)[i])
}

elasticity.outputs.home <- data.frame(names(home_final_model$coefficients[2:length(home_final_model$coefficients)]))
elasticity.outputs.home <- cbind(elasticity.outputs.home,do.call(rbind.data.frame, var_list_home))
colnames(elasticity.outputs.home) <- c("Variable","Elasticity")

elasticity.outputs.home$direction <- ifelse(elasticity.outputs.home$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs.home, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Accessory - Linear Model") +xlab("Variables")

############################################################################################
#Cross validation

home_lm_cv <- cv.lm(data = train_home , form.lm = formula(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                                                            product_analytic_vertical.xFMRadio + 
                                                            product_analytic_vertical.xHomeAudioSpeaker + 
                                                            promotion_type.xBig.Diwali.Sale + 
                                                            promotion_type.xEid...Rathayatra.sale), m =10)

#ms 0.158



