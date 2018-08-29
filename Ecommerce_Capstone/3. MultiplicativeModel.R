#########################################################################################
## Multiplicative model - Camera
########################################################################################

multi_camera <- CameraAccessory
multi_camera$week_no <- NULL
multi_camera$Month <- NULL


#Treatment of zero values
summary(multi_camera)
multi_camera$TV[which(multi_camera$TV == 0)] <- 0.01
multi_camera$Radio[which(multi_camera$Radio == 0)] <- 0.01
multi_camera$Other[which(multi_camera$Other == 0)] <- 0.01
multi_camera$Content.Marketing[which(multi_camera$Content.Marketing == 0)] <- 0.01

#Log of the numerical variables
multi_camera[,1:13] <- data.frame(sign(multi_camera[,1:13])*log(abs(multi_camera[,1:13])))

trainindices.m= sample(1:nrow(multi_camera), 0.6*nrow(multi_camera))
#Generate the train data set
train.m = multi_camera[trainindices.m,]
#Similarly store the rest of the observations into an object "test".
test.m = multi_camera[-trainindices.m,]

model.mul1 <- lm(tot_gmv~., data = train.m)
summary(model.mul1)
alias(model.mul1)

model.mul2 <- stepAIC(model.mul1, direction = "both")
summary(model.mul2)
vif(model.mul2)
alias(model.mul2)

#Low significance and high VIF, all the variables have been removed step by step
#xRepublic, xValentine, Radio, Online.marketing, other
model.mul3 <- lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                   X.Affiliates + SEM + NPS + 
                   product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale + 
                   promotion_type.xIndependence.Sale + promotion_type.xRakshabandhan.Sale, data = train.m)
summary(model.mul3)
vif(model.mul3)

#Low significance and high VIF, all the variables have been removed step by step
#Rathayatra, TV, xDaussera, SEM, CameraLEDLight,xReflectorUmbrella,xSoftbox
model.mul4 <- lm(tot_gmv ~ list_price + Content.Marketing + 
                   X.Affiliates + NPS + 
                   product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xDaussera.sale + 
                   promotion_type.xIndependence.Sale + promotion_type.xRakshabandhan.Sale, data = train.m)
summary(model.mul4)
vif(model.mul4)

############################################################
#Validating on the test data set
predict_cam <- predict(model.mul4, test.m)
test.m$predicted_gmv <- predict_cam

# Now, we need to test the r square between actual and predicted sales.
cam_r <- cor(test.m$tot_gmv, test.m$predicted_gmv)
cam_rsquared <- cor(test.m$tot_gmv, test.m$predicted_gmv)^2
cam_rsquared
#Rsquared: 0.789 on test dataset
############################################################
cam.mul.model <- model.mul4

elasticity <- function(var) {
  x <- as.numeric(cam.mul.model$coefficients[var])
  return(x)
}

varlist.mul.cam <- list()

for(i in 2:length(cam.mul.model$coefficients)) {
  varlist.mul.cam[i-1] <- elasticity(names(cam.mul.model$coefficients)[i])
}

elasticity.cam.mul <- data.frame(names(cam.mul.model$coefficients[2:length(cam.mul.model$coefficients)]))
elasticity.cam.mul <- cbind(elasticity.cam.mul,do.call(rbind.data.frame, varlist.mul.cam))
colnames(elasticity.cam.mul) <- c("Variable","Elasticity")

elasticity.cam.mul$direction <- ifelse(elasticity.cam.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.cam.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative Model") +xlab("Variables")
##################################################################################
#Cross validation

cam.mul.cv <- cv.lm(data = train.m , form.lm = formula(tot_gmv ~ list_price + Content.Marketing + 
                                                         X.Affiliates + NPS + 
                                                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                                                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                                                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                                                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                                                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                                                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                                                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                                                         product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                                                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                                                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                                                         promotion_type.xDaussera.sale + 
                                                         promotion_type.xIndependence.Sale + promotion_type.xRakshabandhan.Sale), m =10)
#ms 1.14
#########################################################################################
## Multiplicative model - GamingAccessory
########################################################################################

multi_gaming <- GamingAccessory
multi_gaming$week_no <- NULL
multi_gaming$Month <- NULL

#Treatment of NULL values
summary(multi_gaming)
multi_gaming$TV[which(multi_gaming$TV == 0)] <- 0.01
multi_gaming$Radio[which(multi_gaming$Radio == 0)] <- 0.01
multi_gaming$Other[which(multi_gaming$Other == 0 )] <- 0.01
multi_gaming$Content.Marketing[which(multi_gaming$Content.Marketing == 0)] <- 0.01

#Taking log of the data for multiplicative behaviour
#Log of the numerical variables
multi_gaming[,1:13] <- data.frame(sign(multi_gaming[,1:13])*log(abs(multi_gaming[,1:13])))

trainindices.game.m= sample(1:nrow(multi_gaming), 0.6*nrow(multi_gaming))
#Generate the train data set
train.game.m = multi_gaming[trainindices.game.m,]
#Similarly store the rest of the observations into an object "test".
test.game.m = multi_gaming[-trainindices.game.m,]

model.game.mul1 <- lm(tot_gmv~., data = train.game.m)
summary(model.game.mul1)
alias(model.game.mul1)

#Sponsorship -  high VIF and low significance
model.game.mul2 <- stepAIC(model.game.mul1, direction = "both")
summary(model.game.mul2)
vif(model.game.mul2)
alias(model.game.mul2)

#Low significance and high VIF, all the variables have been removed step by step
#xBSD, NPS,xGamingAdapter, Digital, Big.Diwali.Sale
model.game.mul3 <- lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                        Online.marketing + X.Affiliates + SEM + Radio + Other + 
                        product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                        product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                        product_analytic_vertical.xGamingChargingStation + 
                        product_analytic_vertical.xGamingGun + product_analytic_vertical.xGamingHeadset + 
                        product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                        product_analytic_vertical.xGamingSpeaker + 
                        promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale, data = train.game.m)
summary(model.game.mul3)
vif(model.game.mul3)

#Low significance and high VIF, all the variables have been removed step by step
#promotion_type.xNo_promotion, Independence.Sale,xRakshabandhan.sale,xGameControlMount,Other
model.game.mul4 <- lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                        Online.marketing + X.Affiliates + SEM + Radio + 
                        product_analytic_vertical.xCoolingPad + 
                        product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                        product_analytic_vertical.xGamingChargingStation + 
                        product_analytic_vertical.xGamingGun + product_analytic_vertical.xGamingHeadset + 
                        product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                        product_analytic_vertical.xGamingSpeaker + 
                        promotion_type.xDaussera.sale , data = train.game.m)
summary(model.game.mul4)
vif(model.game.mul4)

#Low significance and high VIF, all the variables have been removed step by step
#Online.marketing,X.Affiliates,Sponsorship, Content.Marketing
model.game.mul5 <- lm(tot_gmv ~ list_price + TV + SEM + Radio + 
                        product_analytic_vertical.xCoolingPad + 
                        product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                        product_analytic_vertical.xGamingChargingStation + 
                        product_analytic_vertical.xGamingGun + product_analytic_vertical.xGamingHeadset + 
                        product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                        product_analytic_vertical.xGamingSpeaker + 
                        promotion_type.xDaussera.sale , data = train.game.m)
summary(model.game.mul5)
vif(model.game.mul5)


############################################################
#Validating on the test data set
predict_game_mul <- predict(model.game.mul5, test.game.m)
test.game.m$predicted_gmv <- predict_game_mul

# Now, we need to test the r square between actual and predicted sales.
cam_r <- cor(test.game.m$tot_gmv, test.game.m$predicted_gmv)
cam_rsquared <- cor(test.game.m$tot_gmv, test.game.m$predicted_gmv)^2
cam_rsquared
#Rsquare: 0.69 
############################################################
game.mul.model <- model.game.mul5

elasticity <- function(var) {
  x <- as.numeric(game.mul.model$coefficients[var] * mean(train.game.m[,var])/mean(train.game.m$tot_gmv))
  return(x)
}

varlist.mul.game <- list()

for(i in 2:length(game.mul.model$coefficients)) {
  varlist.mul.game[i-1] <- elasticity(names(game.mul.model$coefficients)[i])
}

elasticity.game.mul <- data.frame(names(game.mul.model$coefficients[2:length(game.mul.model$coefficients)]))
elasticity.game.mul <- cbind(elasticity.game.mul,do.call(rbind.data.frame, varlist.mul.game))
colnames(elasticity.game.mul) <- c("Variable","Elasticity")

elasticity.game.mul$direction <- ifelse(elasticity.game.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.game.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multiplicative Model") +xlab("Variables")
##################################################################################
#Cross validation

game.mul.cv <- cv.lm(data = train.game.m , form.lm = formula(tot_gmv ~ list_price + TV + SEM + Radio + 
                                                               product_analytic_vertical.xCoolingPad + 
                                                               product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                                                               product_analytic_vertical.xGamingChargingStation + 
                                                               product_analytic_vertical.xGamingGun + product_analytic_vertical.xGamingHeadset + 
                                                               product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                                                               product_analytic_vertical.xGamingSpeaker + 
                                                               promotion_type.xDaussera.sale), m =10)

#ms 1.24
#########################################################################################
## Multiplicative model - HomeAudio
########################################################################################
multi_home <- HomeAudio
multi_home$week_no <- NULL
multi_home$Month <- NULL

#Treatment of NULL values
summary(multi_home)
multi_home$TV[which(multi_home$TV == 0)] <- 0.01
multi_home$Radio[which(multi_home$Radio == 0)] <- 0.01
multi_home$Other[which(multi_home$Other == 0 )] <- 0.01
multi_home$Content.Marketing[which(multi_home$Content.Marketing == 0)] <- 0.01

#Taking log of the data for multiplicative behaviour
#Log of the numerical variables
multi_home[,1:13] <- data.frame(sign(multi_home[,1:13])*log(abs(multi_home[,1:13])))

trainindices.home.m= sample(1:nrow(multi_home), 0.6*nrow(multi_home))
#Generate the train data set
train.home.m = multi_home[trainindices.home.m,]
#Similarly store the rest of the observations into an object "test".
test.home.m = multi_home[-trainindices.home.m,]

model.home.mul1 <- lm(tot_gmv~., data = train.home.m)
summary(model.home.mul1)
alias(model.home.mul1)


model.home.mul2 <- stepAIC(model.home.mul1, direction = "both")
summary(model.home.mul2)
vif(model.home.mul2)
alias(model.home.mul2)

#Low significance and high VIF, all the variables have been removed step by step
#Rathayatra.sale, New.Year.Sale , NPS, Dock,xHiFiSystem
model.home.mul3 <- lm(tot_gmv ~ list_price + TV + Digital + Sponsorship + Content.Marketing + 
                        Online.marketing + X.Affiliates + Radio + Other + product_analytic_vertical.xDJController + 
                        product_analytic_vertical.xDockingStation + 
                        product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer + 
                        promotion_type.xDaussera.sale, data = train.home.m)
summary(model.home.mul3)
vif(model.home.mul3)

#Low significance and high VIF, all the variables have been removed step by step
#Radio, Online.marketing, X.Affiliates,Other,Content.Marketing, TV
model.home.mul4 <- lm(tot_gmv ~ list_price + Digital + Sponsorship + 
                        product_analytic_vertical.xDJController + 
                        product_analytic_vertical.xDockingStation + 
                        product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer + 
                        promotion_type.xDaussera.sale, data = train.home.m)
summary(model.home.mul4)
vif(model.home.mul4)

############################################################
#Validating on the test data set
predict_home_mul <- predict(model.home.mul4, test.home.m)
test.home.m$predicted_gmv <- predict_home_mul

# Now, we need to test the r square between actual and predicted sales.
cam_r <- cor(test.home.m$tot_gmv, test.home.m$predicted_gmv)
cam_rsquared <- cor(test.home.m$tot_gmv, test.home.m$predicted_gmv)^2
cam_rsquared
#Accuracy: 0.679 ie. 67.9% on test dataset
############################################################
home.mul.model <- model.home.mul4

elasticity <- function(var) {
  x <- as.numeric(home.mul.model$coefficients[var] * mean(train.home.m[,var])/mean(train.home.m$tot_gmv))
  return(x)
}

varlist.mul.home <- list()

for(i in 2:length(home.mul.model$coefficients)) {
  varlist.mul.home[i-1] <- elasticity(names(home.mul.model$coefficients)[i])
}

elasticity.home.mul <- data.frame(names(home.mul.model$coefficients[2:length(home.mul.model$coefficients)]))
elasticity.home.mul <- cbind(elasticity.home.mul,do.call(rbind.data.frame, varlist.mul.home))
colnames(elasticity.home.mul) <- c("Variable","Elasticity")

elasticity.home.mul$direction <- ifelse(elasticity.home.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.home.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Accessory - Multiplicative Model") +xlab("Variables")
##################################################################################
#Cross validation

home.mul.cv <- cv.lm(data = train.home.m , form.lm = formula(tot_gmv ~ list_price + Digital + Sponsorship + 
                                                               product_analytic_vertical.xDJController + 
                                                               product_analytic_vertical.xDockingStation + 
                                                               product_analytic_vertical.xFMRadio + 
                                                               product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSlingBox + 
                                                               product_analytic_vertical.xSoundMixer + 
                                                               promotion_type.xDaussera.sale), m =10)
ms = 0.863
