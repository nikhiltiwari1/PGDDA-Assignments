##################Modeling - Camera Accessory#####################
str(CameraAccessory)
sum(is.na(CameraAccessory))
CameraAccessoryKoyck <- CameraAccessory
library(DataCombine)
# Creating Lag variable 
CameraAccessoryKoyck <- slide(CameraAccessoryKoyck, Var = "tot_gmv",slideBy = -1)
str(CameraAccessoryKoyck)

Camera_koyck <- na.omit(CameraAccessoryKoyck)
Camera_koyck1 <- data.frame(scale(Camera_koyck[,c(1:15)]))
Camera_koyck2 <- data.frame(Camera_koyck[,16:77])
Camera_koyck3 <- data.frame(scale(Camera_koyck[,78]))

Camera_koyck <- cbind(Camera_koyck1, Camera_koyck2, Camera_koyck3)
str(Camera_koyck)
#Camera_koyck <- na.omit(CameraAccessoryKoyck)
str(Camera_koyck)
# removing month and year columns
Camera_koyck <- Camera_koyck[,-c(1,2)]
# splitting train & test sets
set.seed(100)
trainindices= sample(1:nrow(Camera_koyck), 0.6*nrow(Camera_koyck))
train_c = Camera_koyck[trainindices,]
test_c = Camera_koyck[-trainindices,]
str(train_c)
names(train_c)
## building first overall model
Koyck_model1 <- lm(tot_gmv~.,train_c)
summary(Koyck_model1)



# Evaluating the first models for significant predictors
Koyck_model2 <- stepAIC(Koyck_model1,direction = "both")
summary(Koyck_model2)
vif(Koyck_model2)

# removing Radio as vif is very high
Koyck_model3<- lm(formula = tot_gmv ~ list_price + TV + Digital + Sponsorship + 
                    Content.Marketing + X.Affiliates + Radio + NPS + 
                    product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                    product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                    product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                    product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                    product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                    product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                    promotion_type.xBig.Diwali.Sale + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                  data = train_c)


summary(Koyck_model3)
vif(Koyck_model3)

# removing NPS as p-value is very high
Koyck_model4<- lm(formula = tot_gmv ~ list_price + TV + Digital + Sponsorship + 
                    Content.Marketing + X.Affiliates + Radio + 
                    product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                    product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                    product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                    product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                    product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                    product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                    promotion_type.xBig.Diwali.Sale + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                  data = train_c)

summary(Koyck_model4)
vif(Koyck_model4)

# removing Content.Marketing as vif is high
Koyck_model5 <- lm(formula = tot_gmv ~ list_price + TV + Digital + Sponsorship + X.Affiliates + Radio + 
                     product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                     product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                     product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                     product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                     product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                   data = train_c)
summary(Koyck_model5)
vif(Koyck_model5)

# removing promotion_type.xBig.Diwali.Sale as high p-value
Koyck_model6 <-  lm(formula = tot_gmv ~ list_price + TV + Digital + Sponsorship + X.Affiliates + Radio + 
                      product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                      product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                      product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                      product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                      product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                    data = train_c)

summary(Koyck_model6)
vif(Koyck_model6)

# removing TV as p-value is very high
Koyck_model7 <- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + X.Affiliates + Radio + 
                     product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                     product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                     product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                     product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                     product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                   data = train_c)
summary(Koyck_model7)
vif(Koyck_model7)

# removing radio as it has high p-value
Koyck_model8 <- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + X.Affiliates + 
                     product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                     product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                     product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                     product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                     product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                   data = train_c)
summary(Koyck_model8)
vif(Koyck_model8)

# removing promotion_type.xChristmas...New.Year.Sale as it has relatively high p-value
Koyck_model9 <- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + X.Affiliates + 
                     product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                     product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                     product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                     product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                     product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                   data = train_c)
summary(Koyck_model9)
vif(Koyck_model9)


# final model after AIC and VIF tuning
final_koyck_camera <- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + X.Affiliates + 
                           product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                           product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                           product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                           product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                           product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                           product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                           product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                           product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                           product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                           promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                         data = train_c)
summary(final_koyck_camera)
vif(final_koyck_camera)
par(mfrow = c(2,2))
plot(final_koyck_camera, main = "Final Camera Accessory - Koyck Model")
dev.off()

## Cross validation
crossval <- cv.lm(data = train_c, form.lm = formula(final_koyck_camera),m = 10,
                  main = "Cross Validation Camera Accessory Model")
attr(crossval, "ms")
# Cross validation(ms) = 0.232

# Predicting Values
pred_koyck_cam <- predict(final_koyck_camera, test_c)
RMSE(test_c$tot_gmv,pred_koyck_cam)
cam_r_koyck <- cor(test_c$tot_gmv, pred_koyck_cam)
cam_rsquared <- cam_r_koyck^2
cam_rsquared
#Rsquared on test: 0.731
###################################
#Estimating elasticity 

cam_final_model <- final_koyck_camera

elasticity <- function(var) {
  x <- as.numeric(cam_final_model$coefficients[var] * mean(train_c[,var])/mean(train_c$tot_gmv))
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
  ggtitle("Camera Accessory - Koyck Model") +xlab("Variables")
###########################################

##################Modeling - Gaming Accessory#####################
str(GamingAccessory)
sum(is.na(GamingAccessory))
GamingAccessoryKoyck <- GamingAccessory
library(DataCombine)
# Creating Lag variable 
GamingAccessoryKoyck <- slide(GamingAccessoryKoyck, Var = "tot_gmv",slideBy = -1)
str(GamingAccessoryKoyck)

Game_koyck <- na.omit(GamingAccessoryKoyck)
Game_koyck1 <- data.frame(scale(Game_koyck[,c(1:15)]))
Game_koyck2 <- data.frame(Game_koyck[,16:77])
Game_koyck3 <- data.frame(scale(Game_koyck[,78]))

Game_koyck <- cbind(Game_koyck1, Game_koyck2, Game_koyck3)
str(Game_koyck)
# removing month and year columns
Game_koyck <- Game_koyck[,-c(1,2)]
# splitting train & test sets
set.seed(100)
trainindices= sample(1:nrow(Game_koyck), 0.6*nrow(Game_koyck))
train_g = Game_koyck[trainindices,]
test_g = Game_koyck[-trainindices,]
str(train_g)
names(train_g)
## building first overall model
Koyck_model1 <- lm(tot_gmv~.,train_g)
summary(Koyck_model1)

# Evaluating the first models for significant predictors
Koyck_model2 <- stepAIC(Koyck_model1,direction = "both")
summary(Koyck_model2)
vif(Koyck_model2)

# removing SEM as vif is very high
Koyck_model3<- lm(formula = tot_gmv ~ TV + Digital + Sponsorship + X.Affiliates + 
                    NPS + product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingHeadset + 
                    product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                    product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xJoystickGamingWheel + 
                    promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale, data = train_g)
summary(Koyck_model3)
vif(Koyck_model3)

# removing product_analytic_vertical.xJoystickGamingWheel as p-value is moderately high
Koyck_model4<- lm(formula = tot_gmv ~ TV + Digital + Sponsorship + X.Affiliates + 
                    NPS + product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingHeadset + 
                    product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                    product_analytic_vertical.xGamingSpeaker + 
                    promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale, data = train_g)
                    
summary(Koyck_model4)
vif(Koyck_model4)



# final model after AIC and VIF tuning
final_koyck_gaming <-  lm(formula = tot_gmv ~ TV + Digital + Sponsorship + X.Affiliates + 
                            NPS + product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingHeadset + 
                            product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                            product_analytic_vertical.xGamingSpeaker + 
                            promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                            promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                            promotion_type.xRakshabandhan.Sale, data = train_g)
summary(final_koyck_gaming)
vif(final_koyck_gaming)
par(mfrow = c(2,2))
plot(final_koyck_gaming, main = "Final Gaming Accessory - Koyck Model")
dev.off()
## Cross validation
crossval <- cv.lm(data = train_g, form.lm = formula(final_koyck_gaming),m = 10,
                  main = "Cross Validation Gaming Accessory Model")
attr(crossval, "ms")
# Cross validation (ms) = 0.319
# Predicting Values
pred_koyck_game <- predict(final_koyck_gaming, test_g)
RMSE(test_g$tot_gmv,pred_koyck_game)
game_r_koyck <- cor(test_g$tot_gmv, pred_koyck_game)
game_rsquared <- game_r_koyck^2
game_rsquared
#Rsquared on test: 0.669
###################################
#Estimating elasticity 

game_final_model <- final_koyck_gaming

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var] * mean(train_g[,var])/mean(train_g$tot_gmv))
  return(x)
}

var_list <- list()

for(i in 2:length(game_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(game_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(game_final_model$coefficients[2:length(game_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Koyck Model") +xlab("Variables")
###########################################


##################Modeling - Home Audio#####################
str(HomeAudio)
sum(is.na(HomeAudio))
HomeAudioKoyck <- HomeAudio
library(DataCombine)
# Creating Lag variable 
HomeAudioKoyck <- slide(HomeAudioKoyck, Var = "tot_gmv",slideBy = -1)
str(HomeAudioKoyck)

HomeA_koyck <- na.omit(HomeAudioKoyck)
HomeA_koyck1 <- data.frame(scale(HomeA_koyck[,c(1:15)]))
HomeA_koyck2 <- data.frame(HomeA_koyck[,16:77])
HomeA_koyck3 <- data.frame(scale(HomeA_koyck[,78]))

HomeA_koyck <- cbind(HomeA_koyck1, HomeA_koyck2, HomeA_koyck3)
str(HomeA_koyck)
# removing month and year columns
HomeA_koyck <- HomeA_koyck[,-c(1,2)]
# splitting train & test sets
trainindices= sample(1:nrow(HomeA_koyck), 0.6*nrow(HomeA_koyck))
train_h = HomeA_koyck[trainindices,]
test_h = HomeA_koyck[-trainindices,]
str(train_h)
names(train_h)
## building first overall model
Koyck_model1 <- lm(tot_gmv~.,train_h)
summary(Koyck_model1)

# Evaluating the first models for significant predictors
Koyck_model2 <- stepAIC(Koyck_model1,direction = "both")
summary(Koyck_model2)
vif(Koyck_model2)

# removing Online.marketing  as vif is very high
Koyck_model3<- lm(formula = tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                    X.Affiliates + discount_over_mrp + product_analytic_vertical.xDJController + 
                    product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                    product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                    promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                  data = train_h)
summary(Koyck_model3)
vif(Koyck_model3)

# removing X.Affiliates as p-value is high
Koyck_model4 <-  lm(formula = tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                      discount_over_mrp + product_analytic_vertical.xDJController + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                    data = train_h)
summary(Koyck_model4)
vif(Koyck_model4)

# removing promotion_type.xEid...Rathayatra.sale as high p-value
Koyck_model5 <-  lm(formula = tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                      discount_over_mrp + product_analytic_vertical.xDJController + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale, 
                    data = train_h)
summary(Koyck_model5)
vif(Koyck_model5)

# removing promotion_type.xEid...Rathayatra.sale as high p-value
Koyck_model6 <-  lm(formula = tot_gmv ~ Digital + Sponsorship +  
                      discount_over_mrp + product_analytic_vertical.xDJController + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale, 
                    data = train_h)
summary(Koyck_model6)
vif(Koyck_model6)

# removing product_analytic_vertical.xDJController as high p-value
Koyck_model7 <-  lm(formula = tot_gmv ~ Digital + Sponsorship +  
                      discount_over_mrp + product_analytic_vertical.xDock +  
                      product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale, 
                    data = train_h)
summary(Koyck_model7)
vif(Koyck_model7)


# final model after AIC and VIF tuning
final_koyck_homeA <-  lm(formula = tot_gmv ~ Digital + Sponsorship +  
                           discount_over_mrp + product_analytic_vertical.xDock +  
                           product_analytic_vertical.xDockingStation + 
                           product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                           promotion_type.xDaussera.sale, 
                         data = train_h)
summary(final_koyck_homeA)
vif(final_koyck_homeA)
par(mfrow = c(2,2))
plot(final_koyck_gaming, main = "Final Home Audio - Koyck Model")
dev.off()
## Cross validation
crossval <- cv.lm(data = train_h, form.lm = formula(final_koyck_homeA),m = 10, 
                  main = "Cross Validation Home Audio Model")
attr(crossval, "ms")
#Cross validation(ms): 0.41

# Predicting Values
pred_koyck_homeA <- predict(final_koyck_homeA, test_h)
RMSE(test_h$tot_gmv,pred_koyck_homeA)
homeA_r_koyck <- cor(test_h$tot_gmv, pred_koyck_homeA)
homeA_rsquared <- homeA_r_koyck^2
homeA_rsquared
#Rsquared on test: 0.896
###################################
#Estimating elasticity 

homeA_final_model <- final_koyck_homeA

elasticity <- function(var) {
  x <- as.numeric(homeA_final_model$coefficients[var] * mean(train_h[,var])/mean(train_h$tot_gmv))
  return(x)
}

var_list <- list()

for(i in 2:length(homeA_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(homeA_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(homeA_final_model$coefficients[2:length(homeA_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Koyck Model") +xlab("Variables")
###########################################

