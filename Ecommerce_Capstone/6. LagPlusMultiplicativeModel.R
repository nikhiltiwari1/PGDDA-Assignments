#######################################################################
### Start Building multipiactive lag model for camera accessory 
#######################################################################
weekly_order_ad_data_mullag_cam <- weekly_order_ad_data_lag_cam1 


weekly_order_ad_data_mullag_cam_tmp  <- lapply (weekly_order_ad_data_mullag_cam[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
weekly_order_ad_data_mullag_cam <- data.frame( cbind( weekly_order_ad_data_mullag_cam_tmp ,weekly_order_ad_data_mullag_cam[,36:97] ))
weekly_order_ad_data_mullag_cam <- na.omit(weekly_order_ad_data_mullag_cam)

set.seed(200)


trainindices= sample(1:nrow(weekly_order_ad_data_mullag_cam), 0.7*nrow(weekly_order_ad_data_mullag_cam))
#Generate the train data set
train_lagmul_cam = weekly_order_ad_data_mullag_cam[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_lagmul_cam = weekly_order_ad_data_mullag_cam[-trainindices,]



cam_model1 <- lm(tot_gmv~. , data = train_lagmul_cam)
summary(cam_model1)


step_cam_model1 <- stepAIC(cam_model1, direction = "both")
summary(step_cam_model1)
vif (step_cam_model1 )

## Removed online_ad_stock due to high vif 
cam_model2 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing + X.Affiliates + 
                     SEM + Radio + Other + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock + dig_ad_stock + spon_ad_stock + content_ad_stock + 
                     sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model2)
vif (cam_model2 )




## Removed    Other due to high vif 
cam_model3 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing + X.Affiliates + 
                     Radio + SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock + dig_ad_stock + spon_ad_stock + content_ad_stock + 
                     sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model3)
vif (cam_model3 )



## Removed    X.Affiliates due to high vif 
cam_model4 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     Radio + SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock + dig_ad_stock + spon_ad_stock + content_ad_stock + 
                     sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model4)
vif (cam_model4 )



## Removed    spon_ad_stock due to high vif 
cam_model5 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     Radio + SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock + dig_ad_stock  + content_ad_stock + 
                     sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model5)
vif (cam_model5 )



## Removed    sem_ad_stock due to high vif 
cam_model6 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     Radio + SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock + dig_ad_stock  + content_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model6)
vif (cam_model6 )



## Removed    sem_ad_stock due to high vif 
cam_model7 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     Radio + SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock   + content_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model7)
vif (cam_model7 )



## Removed    Radio due to high vif 
cam_model8 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock   + content_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model8)
vif (cam_model8 )



## Removed    Month due to high vif 
cam_model9 <- lm ( tot_gmv ~ week_no  + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock   + content_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model9)
vif (cam_model9 )




## Removed    week_no due to high vif 
cam_model10 <- lm ( tot_gmv ~    list_price + TV + Digital + 
                      Sponsorship + Content.Marketing + Online.marketing  + 
                      SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock   + content_ad_stock + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model10)
vif (cam_model10 )



## Removed    Content.Marketing  due to high vif 
cam_model11 <- lm ( tot_gmv ~    list_price + TV + Digital + 
                      Sponsorship  + Online.marketing  + 
                      SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock   + content_ad_stock + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model11)
vif (cam_model11 )



## Removed    NPS  due to high vif 
cam_model12 <- lm ( tot_gmv ~    list_price + TV + Digital + 
                      Sponsorship  + Online.marketing  + 
                      SEM +  + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock   + content_ad_stock + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model12)
vif (cam_model12 )



## Removed    SEM  due to high vif 
cam_model13 <- lm ( tot_gmv ~    list_price + TV + Digital + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock   + content_ad_stock + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model13)
vif (cam_model13 )



## Removed    content_ad_stock  due to high vif 
cam_model14 <- lm ( tot_gmv ~    list_price + TV + Digital + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model14)
vif (cam_model14 )


## Removed    Digital  due to high vif 
cam_model15 <- lm ( tot_gmv ~    list_price + TV +  + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model15)
vif (cam_model15 )


## Remopved promotion_type.xBSD.5 due to high P 


cam_model16 <- lm ( tot_gmv ~    list_price + TV +  + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale  + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model16)
vif (cam_model16 )


## Removed gmv_lag_2 due to high vif 
cam_model17 <- lm ( tot_gmv ~    list_price + TV +  + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale  + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model17)
vif (cam_model17 )



## Removed gmv_lag_3 due to high vif 
cam_model18 <- lm ( tot_gmv ~    list_price + TV +  + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale  + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model18)
vif (cam_model18 )



## Removed gmv_lag_1 due to high vif 
cam_model19 <- lm ( tot_gmv ~    list_price + TV +  + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp +   
                      gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale  + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model19)
vif (cam_model19 )



## Removed tv ad stock due to high vif 
cam_model20 <- lm (  tot_gmv ~    list_price + TV +  + 
                       Sponsorship  + Online.marketing  + 
                       + discount_over_mrp +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                       promotion_type.xBig.Diwali.Sale  + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                       promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model20)
vif (cam_model20 )



## Removed tv  due to high vif 
cam_model21 <- lm (  tot_gmv ~    list_price +  
                       Sponsorship  + Online.marketing  + 
                       + discount_over_mrp +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                       promotion_type.xBig.Diwali.Sale  + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                       promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model21)
vif (cam_model21 )




## Removed Online.marketing  due to high vif 
cam_model22 <- lm (  tot_gmv ~    list_price +  
                       Sponsorship  +   
                       + discount_over_mrp +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                       promotion_type.xBig.Diwali.Sale  + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                       promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model22)
vif (cam_model22 )




## Removed list_price_lag_2  due to high vif 
cam_model23 <- lm (  tot_gmv ~    list_price +  
                       Sponsorship  +   
                       + discount_over_mrp +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3  + price_change_from_w1 
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                       promotion_type.xBig.Diwali.Sale  + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                       promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model23)
vif (cam_model23 )




## Removed promotion_type.xPacman , promotion_type.xValentine.s.Day , promotion_type.xRepublic.Day , promotion_type.xBig.Diwali.Sale due to high vif 
cam_model24 <- lm (  tot_gmv ~    list_price +  
                       Sponsorship  +   
                       + discount_over_mrp +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3  + price_change_from_w1 
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope 
                     + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion   
                     , data = train_lagmul_cam)

summary(cam_model24)
vif (cam_model24 )


## Removed price_change_from_w1  , discount_over_mrp due to high vif 
cam_model25 <- lm (  tot_gmv ~    list_price +  
                       Sponsorship  +   
                       +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3   
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope 
                     + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion   
                     , data = train_lagmul_cam)

summary(cam_model25)
vif (cam_model25 )


## Model evalution on test data set 

gmv_cam_prediction <- predict(cam_model25, test_lagmul_cam[,-3])
test_lagmul_cam$predicted_gmv <- gmv_cam_prediction

cam_r <- cor(test_lagmul_cam$tot_gmv, test_lagmul_cam$predicted_gmv  )
cam_rsquared <- cam_r^2
cam_rsquared   ####Rsquare on test = 0.871

## Model evalution using cross validation 
cam_lm_cv <- cv.lm(data = weekly_order_ad_data_mullag_cam , form.lm = cam_model25, m =10)
###   ms 
###  0.67

cam_final_model <- cam_model25

elasticity <- function(var) {
  x <- as.numeric(cam_final_model$coefficients[var] )
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
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)
ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera  Accessory -  Lag Distributed Multipicative  Model") +xlab("Variables")

###################################################################################################
####### Start Building multipiactive lag model for gaming accessory 
##################################################################################################

weekly_order_ad_data_mullag_game <- weekly_order_ad_data_lag_game1 
weekly_order_ad_data_mullag_game <-na.omit(weekly_order_ad_data_mullag_game)

weekly_order_ad_data_mullag_game_tmp  <- lapply (weekly_order_ad_data_mullag_game[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
weekly_order_ad_data_mullag_game <- data.frame( cbind( weekly_order_ad_data_mullag_game_tmp ,weekly_order_ad_data_mullag_game[,36:97] ))


set.seed(200)


trainindices= sample(1:nrow(weekly_order_ad_data_mullag_game), 0.7*nrow(weekly_order_ad_data_mullag_game))
#Generate the train data set
train_lagmul_game = weekly_order_ad_data_mullag_game[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_lagmul_game = weekly_order_ad_data_mullag_game[-trainindices,]



game_model1 <- lm(tot_gmv~. , data = train_lagmul_game)
summary(game_model1)


step_game_model1 <- stepAIC(game_model1, direction = "both")
summary(step_game_model1)
vif (step_game_model1 )

## Removed content_ad_stock  due to high vif 
step_game_model2 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Content.Marketing + 
                           Online.marketing + X.Affiliates + SEM + Radio + Other + NPS + 
                           discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                           gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                           price_change_from_w1 + list_price_change_from_w2 + tv_ad_stock + 
                           dig_ad_stock + spon_ad_stock  + affiliate_ad_stock + 
                           product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                           product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                           product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                           product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                           promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                           promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                           promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model2)
vif (step_game_model2 )

## Removed Month due to high vif 
step_game_model3 <- lm ( tot_gmv ~ week_no  + list_price + TV + Content.Marketing + 
                           Online.marketing + X.Affiliates + SEM + Radio + Other + NPS + 
                           discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                           gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                           price_change_from_w1 + list_price_change_from_w2 + tv_ad_stock + 
                           dig_ad_stock + spon_ad_stock  + affiliate_ad_stock + 
                           product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                           product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                           product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                           product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                           promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                           promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                           promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model3)
vif (step_game_model3 )

## Removed  TV ad stock due to high vif 
step_game_model4 <- lm ( tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                           Online.marketing + X.Affiliates + SEM + Radio + Other + NPS + 
                           discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                           gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                           price_change_from_w1 + list_price_change_from_w2  + 
                           dig_ad_stock + spon_ad_stock  + affiliate_ad_stock + 
                           product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                           product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                           product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                           product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                           promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                           promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                           promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model4)
vif (step_game_model4 )

## Removed dig_ad_stock due to high vif 
step_game_model5 <- lm (  tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                            Online.marketing + X.Affiliates + SEM + Radio + Other + NPS + 
                            discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                            gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                            price_change_from_w1 + list_price_change_from_w2  + 
                            spon_ad_stock  + affiliate_ad_stock + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                            product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                            product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                            product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                            promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                            promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                            promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model5)
vif (step_game_model5 )

## removed SEM due to high vif 

step_game_model6 <- lm (  tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                            Online.marketing + X.Affiliates  + Radio + Other + NPS + 
                            discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                            gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                            price_change_from_w1 + list_price_change_from_w2  + 
                            spon_ad_stock  + affiliate_ad_stock + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                            product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                            product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                            product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                            promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                            promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                            promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model6)
vif (step_game_model6)


## removed NPS due to high v
step_game_model7 <- lm (   tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                             Online.marketing + X.Affiliates  + Radio + Other  + 
                             discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                             price_change_from_w1 + list_price_change_from_w2  + 
                             spon_ad_stock  + affiliate_ad_stock + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model7)
vif (step_game_model7)

## removed gmv_lag_1 due to high vif 
step_game_model8 <- lm ( tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                           Online.marketing + X.Affiliates  + Radio + Other  + 
                           discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                           gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                           price_change_from_w1 + list_price_change_from_w2  + 
                           spon_ad_stock  + affiliate_ad_stock + 
                           product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                           product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                           product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                           product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                           promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                           promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                           promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model8)
vif (step_game_model8)


## removed list_price_lag_2

step_game_model9 <- lm ( tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                           Online.marketing + X.Affiliates  + Radio + Other  + 
                           discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                           gmv_change_from_w2 + gmv_change_from_w3  + 
                           price_change_from_w1 + list_price_change_from_w2  + 
                           spon_ad_stock  + affiliate_ad_stock + 
                           product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                           product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                           product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                           product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                           promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                           promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                           promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model9)
vif (step_game_model9)

## removed week_no due to high vif 

step_game_model10 <- lm ( tot_gmv ~    list_price  + TV +Content.Marketing + 
                            Online.marketing + X.Affiliates  + Radio + Other  + 
                            discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                            gmv_change_from_w2 + gmv_change_from_w3  + 
                            price_change_from_w1 + list_price_change_from_w2  + 
                            spon_ad_stock  + affiliate_ad_stock + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                            product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                            product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                            product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                            promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                            promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                            promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model10)
vif (step_game_model10)

## removed affiliate_ad_stock due to high vif 

step_game_model11 <- lm (  tot_gmv ~    list_price  + TV +Content.Marketing + 
                             Online.marketing + X.Affiliates  + Radio + Other  + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  + 
                             spon_ad_stock   + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model11)
vif (step_game_model11)

## removed spon_ad_stock and affiliate ad stockdue to high vif 
step_game_model12 <- lm (  tot_gmv ~    list_price  + TV +Content.Marketing + 
                             Online.marketing + X.Affiliates  + Radio + Other  + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model12)
vif (step_game_model12)


## removed   Content.Marketing  due to high vif 
step_game_model13 <- lm (  tot_gmv ~    list_price  + TV  + 
                             Online.marketing + X.Affiliates  + Radio + Other  + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model13)
vif (step_game_model13)


## removed   Online.marketing  due to high vif 
step_game_model14 <- lm (  tot_gmv ~    list_price  + TV   
                           + X.Affiliates  + Radio + Other  + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model14)
vif (step_game_model14)



## removed   Other  due to high vif 
step_game_model15 <- lm (  tot_gmv ~    list_price  + TV   
                           + X.Affiliates  + Radio   + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model15)
vif (step_game_model15)


## removed   TV  due to high vif 
step_game_model16 <- lm (  tot_gmv ~    list_price     
                           + X.Affiliates  + Radio   + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model16)
vif (step_game_model16)


## removed motion_type.xChristmas...New.Year.Sale , promotion_type.xBSD.5  due to high P
step_game_model17 <- lm (  tot_gmv ~    list_price     
                           + X.Affiliates  + Radio   + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController  + 
                             promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman  
                           , data = train_lagmul_game)
summary(step_game_model17)
vif (step_game_model17)

## removed price_change_from_w1 due to high P 
step_game_model18 <- lm (  tot_gmv ~    list_price     
                           + X.Affiliates  + Radio   + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController  + 
                             promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman  
                           , data = train_lagmul_game)
summary(step_game_model18)
vif (step_game_model18)


## removed gmv_lag_3 , Radio , product_analytic_vertical.xGamingMemoryCard due to high P 
step_game_model19 <- lm (  tot_gmv ~    list_price     
                           + X.Affiliates     + 
                             discount_over_mrp  + gmv_lag_2  + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard  + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController  + 
                             promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman  
                           , data = train_lagmul_game)
summary(step_game_model19)
vif (step_game_model19)



## removed promotion_type.xNo_promotion , list_price_change_from_w2 , product_analytic_vertical.xGamingAdapter due to high P 
step_game_model20 <- lm (  tot_gmv ~    list_price     
                           + X.Affiliates     + 
                             discount_over_mrp  + gmv_lag_2  + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit  + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard  + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController  + 
                             promotion_type.xDaussera.sale + 
                             promotion_type.xPacman  
                           , data = train_lagmul_game)
summary(step_game_model20)
vif (step_game_model20)

## Model evalution on test dataset
gmv_cam_prediction <- predict(step_game_model20, test_lagmul_game[,-3])
test_lagmul_game$predicted_gmv <- gmv_cam_prediction

game_r <- cor(test_lagmul_game$tot_gmv, test_lagmul_game$predicted_gmv  )
game_rsquared <- game_r^2
game_rsquared   ####Rsquare on test = 0.763

## Model evalution using cross validation 
game_lm_cv <- cv.lm(data = weekly_order_ad_data_mullag_game , form.lm = step_game_model20, m =10)

##ms - 0.91

##########################################################################################
###Elasticity plot for multipicative lag model for gaming accessories 
########################################################################################
game_final_model <- step_game_model20

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var] )
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
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming  Accessory -  Lag Distributed Multipicative  Model") +xlab("Variables")



#################################################################################################
### Start building distributed lag multipicative model for home audio
################################################################################################
weekly_order_ad_data_mullag_home <- weekly_order_ad_data_lag_home1
weekly_order_ad_data_mullag_home <-na.omit(weekly_order_ad_data_mullag_home)

weekly_order_ad_data_mullag_home_tmp  <- lapply (weekly_order_ad_data_mullag_home[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
weekly_order_ad_data_mullag_home <- data.frame( cbind( weekly_order_ad_data_mullag_home_tmp ,weekly_order_ad_data_mullag_home[,36:97] ))


set.seed(200)


trainindices= sample(1:nrow(weekly_order_ad_data_mullag_home), 0.7*nrow(weekly_order_ad_data_mullag_home))
#Generate the train data set
train_lagmul_home = weekly_order_ad_data_mullag_home[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_lagmul_home = weekly_order_ad_data_mullag_home[-trainindices,]



home_model1 <- lm(tot_gmv~. , data = train_lagmul_home)
summary(home_model1)


step_home_model1 <- stepAIC(home_model1, direction = "both")
summary(step_home_model1)
vif (step_home_model1 )

## Removed spon_ad_stock very high vif 
home_model2 <- lm(tot_gmv ~ week_no + Month + list_price + TV + Content.Marketing + 
                    Online.marketing + X.Affiliates + SEM + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    dig_ad_stock  + online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model2)
vif(home_model2)


## Removed SEM very high vif 
home_model3 <- lm(tot_gmv ~ week_no + Month + list_price + TV + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    dig_ad_stock  + online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model3)
vif(home_model3)

## Removed week_no very high vif 
home_model4 <- lm(tot_gmv ~   Month + list_price + TV + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    dig_ad_stock  + online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model4)
vif(home_model4)


## Removed TV very high vif 
home_model5 <- lm(tot_gmv ~   Month + list_price  + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    dig_ad_stock  + online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model5)
vif(home_model5)


## Removed Month very high vif 
home_model6 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    dig_ad_stock  + online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model6)
vif(home_model6)


## Removed dig_ad_stock very high vif 
home_model7 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model7)
vif(home_model7)



## Removed sem_ad_stock very high vif 
home_model8 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    online_ad_stock + radio_ad_stock + 
                    affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model8)
vif(home_model8)


## Removed Other very high vif 
home_model9 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                    Online.marketing + X.Affiliates   + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    online_ad_stock + radio_ad_stock + 
                    affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model9)
vif(home_model9)


## Removed promotion_type.xValentine.s.Day ,promotion_type.xRepublic.Day ,product_analytic_vertical.xFMRadio  very high P 
home_model10 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                     list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                     online_ad_stock + radio_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale + promotion_type.xNo_promotion 
                   , 
                   data = train_lagmul_home)
summary(home_model10)
vif(home_model10)



## Removed promotion_type.xNo_promotion   very high P 
home_model11 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                     list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                     online_ad_stock + radio_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model11)
vif(home_model11)


## Removed list_price_lag_1   very high vif 
home_model12 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                     + list_price_lag_2 + price_change_from_w1 + 
                     online_ad_stock + radio_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model12)
vif(home_model12)


## Removed gmv_lag_2 ,gmv_lag_3  very high P 
home_model13 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w1 + gmv_change_from_w3 + 
                     + list_price_lag_2 + price_change_from_w1 + 
                     online_ad_stock + radio_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model13)
vif(home_model13)


## Removed radio_ad_stock ,price_change_from_w1  very high P 
home_model14 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w1 + gmv_change_from_w3 + 
                     + list_price_lag_2  + 
                     online_ad_stock  + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model14)
vif(home_model14)


## Removed online_ad_stock  very high P 
home_model15 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w1 + gmv_change_from_w3 + 
                     + list_price_lag_2  
                   + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model15)
vif(home_model15)


## Removed Content.Marketing  very high P 
home_model16 <- lm(tot_gmv ~     list_price   + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w1 + gmv_change_from_w3 + 
                     + list_price_lag_2  
                   + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model16)
vif(home_model16)


## Removed gmv_change_from_w1  very high P 
home_model17 <- lm(tot_gmv ~     list_price   + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w3 + 
                     + list_price_lag_2  
                   + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model17)
vif(home_model17)


## Removed list_price_lag_2  very high P 
home_model18 <- lm(tot_gmv ~     list_price   + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w3 
                   +   
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model18)
vif(home_model18)


## Removed list_price  very high P 
home_model19 <- lm(tot_gmv ~        
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w3 
                   +   
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model19)
vif(home_model19)


## Removed Online.marketing ,affiliate_ad_stock very high vif 
home_model20 <- lm(tot_gmv ~        
                     X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w3 
                   +   
                     product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model20)
vif(home_model20)


## Removed gmv_lag_1 due to very high vif 
home_model21 <- lm(tot_gmv ~        
                     X.Affiliates    + 
                     gmv_change_from_w3 
                   +   
                     product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model21)
vif(home_model21)


## Removed promotion_type.xChristmas...New.Year.Sale , promotion_type.xDaussera.sale due to very high P 
home_model22 <- lm(tot_gmv ~        
                     X.Affiliates    + 
                     gmv_change_from_w3 
                   +   
                     product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer  
                   , 
                   data = train_lagmul_home)
summary(home_model22)
vif(home_model22)




home_cam_prediction <- predict(home_model22, test_lagmul_home[,-3])
test_lagmul_home$predicted_gmv <- home_cam_prediction

home_r <- cor(test_lagmul_home$tot_gmv, test_lagmul_home$predicted_gmv  )
home_rsquared <- home_r^2
home_rsquared   #### Rsquare on test =  0.782

game_lm_cv <- cv.lm(data = weekly_order_ad_data_mullag_home , form.lm = home_model22, m =10)

##ms - 0.89

home_final_model <- home_model22

elasticity <- function(var) {
  x <- as.numeric(home_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(home_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(home_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(home_final_model$coefficients[2:length(home_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home  Audio -  Lag Distributed Multipicative  Model") +xlab("Variables")



