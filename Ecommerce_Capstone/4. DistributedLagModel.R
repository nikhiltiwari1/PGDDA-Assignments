## Base data set creation for lag model 

## Add sales for last 3 week

weekly_order_ad_data_with_lag <- weekly_order_ad_data %>% arrange ( week_no) %>% group_by(product_analytic_sub_category ,product_analytic_vertical) %>%  mutate(gmv_lag_1 = lag(tot_gmv, 1))  %>%  mutate(gmv_lag_2 = lag(tot_gmv, 2)) %>%  mutate(gmv_lag_3 = lag(tot_gmv, 3)) 
## add change in sales  last 3 weeks

weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category ,product_analytic_vertical ) %>%  mutate(gmv_change_from_w1 = (tot_gmv-lag(tot_gmv, 1))/tot_gmv) %>%  mutate(gmv_change_from_w2 = (tot_gmv-lag(tot_gmv, 2))/tot_gmv) %>%  mutate(gmv_change_from_w3 = (tot_gmv-lag(tot_gmv, 3))/tot_gmv) 

## add list price for   last 3 weeks
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(list_price_lag_1 = lag(list_price, 1))  %>%  mutate(list_price_lag_2 = lag(list_price, 2)) %>%  mutate(list_price_lag_3 = lag(list_price, 3)) 

#### add list price change  for   last 3 weeks
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(price_change_from_w1 = (list_price-lag(list_price, 1))/list_price) %>%  mutate(list_price_change_from_w2 = (list_price-lag(list_price, 2))/list_price) %>%  mutate(list_price_change_from_w3 = (list_price-lag(list_price, 3))/list_price)

#### add TV ad stcok. 60% effect of current week is propagating to next week
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(tv_ad_stock = TV+ if_else ( is.na (lag(TV, 1)*.6),0, lag(TV, 1)*.6) + if_else ( is.na (lag(TV, 2)*.36),0, lag(TV, 2)*.36) + if_else ( is.na (lag(TV, 3)*.22),0, lag(TV, 3)*.22) + if_else ( is.na (lag(TV, 4)*.13),0, lag(TV, 4)*.13)  + if_else ( is.na (lag(TV, 5)*.07),0, lag(TV, 5)*.07)  + if_else ( is.na (lag(TV, 6)*.05),0, lag(TV, 6)*.05) ) 

#### add Digital ad stcok. 20% effect of current week is propagating to next week

weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(dig_ad_stock = Digital+ if_else ( is.na (lag(Digital, 1)*.2),0, lag(Digital, 1)*.2) + if_else ( is.na (lag(Digital, 2)*.04),0, lag(Digital, 2)*.04)   )

#### add sponsorship ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(spon_ad_stock = Sponsorship+ if_else ( is.na (lag(Sponsorship, 1)*.2),0, lag(Sponsorship, 1)*.2) + if_else ( is.na (lag(Sponsorship, 2)*.04),0, lag(Sponsorship, 2)*.04)   )  

#### add content marketing ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(content_ad_stock = Content.Marketing+ if_else ( is.na (lag(Content.Marketing, 1)*.2),0, lag(Content.Marketing, 1)*.2) + if_else ( is.na (lag(Content.Marketing, 2)*.04),0, lag(Content.Marketing, 2)*.04)   ) 

#### add online marketing ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(online_ad_stock = Online.marketing+ if_else ( is.na (lag(Online.marketing, 1)*.2),0, lag(Online.marketing, 1)*.2) + if_else ( is.na (lag(Online.marketing, 2)*.04),0, lag(Online.marketing, 2)*.04)   )

#### add Radio ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(radio_ad_stock = Radio+ if_else ( is.na (lag(SEM, 1)*.2),0, lag(Radio, 1)*.2) + if_else ( is.na (lag(Radio, 2)*.04),0, lag(Radio, 2)*.04)   )

#### add SEM ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(sem_ad_stock = SEM+ if_else ( is.na (lag(SEM, 1)*.2),0, lag(SEM, 1)*.2) + if_else ( is.na (lag(SEM, 2)*.04),0, lag(SEM, 2)*.04)   )

#### add Affiliate  ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(affiliate_ad_stock = X.Affiliates+ if_else ( is.na (lag(X.Affiliates, 1)*.2),0, lag(X.Affiliates, 1)*.2) + if_else ( is.na (lag(X.Affiliates, 2)*.04),0, lag(X.Affiliates, 2)*.04)   )

## Filter out CameraAccessory", GamingAccessory and  HomeAudio sub category
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% filter ( product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio"))

weekly_order_ad_data_with_lag <- as.data.frame(weekly_order_ad_data_with_lag)

## Since these dataframe has been created using weekly_order_ad_data, so we have character variables in  this dataset
## Convert char vars in factorial
weekly_order_ad_data_with_lag_chr <- weekly_order_ad_data_with_lag[,c(5,32,34)]
weekly_order_ad_data_with_lag_chr_fact <- data.frame(sapply(weekly_order_ad_data_with_lag_chr, function(x) factor(x)))
str(weekly_order_ad_data_with_lag_chr_fact)

# creating dummy variables for factor attributes
dummies1<- data.frame(sapply(weekly_order_ad_data_with_lag_chr_fact, 
                             function(x) data.frame(model.matrix(~x-1,data =weekly_order_ad_data_with_lag_chr_fact))[,-1]))


## Combine dummies with other varaiables to create master data set 
weekly_order_ad_data_with_lag_overall <- cbind ( weekly_order_ad_data_with_lag[, -c(5,32,34)], dummies1 ) 

cor_var <- cor ( weekly_order_ad_data_with_lag_overall[-4])

## Since these variables are highly corelated  or direct proxy to sales , so taking them out 
weekly_order_ad_data_with_lag_overall$tot_week <- NULL 
weekly_order_ad_data_with_lag_overall$total_row <- NULL
weekly_order_ad_data_with_lag_overall$avg_mrp <- NULL
weekly_order_ad_data_with_lag_overall$avg_price <- NULL
weekly_order_ad_data_with_lag_overall$tot_units <- NULL
weekly_order_ad_data_with_lag_overall$no_of_orders <- NULL
weekly_order_ad_data_with_lag_overall$tot_product_mrp <- NULL
weekly_order_ad_data_with_lag_overall$avg_gmv <- NULL
weekly_order_ad_data_with_lag_overall$value_per_visitor <- NULL
weekly_order_ad_data_with_lag_overall$Year <- NULL
weekly_order_ad_data_with_lag_overall$no_of_customer <- NULL
weekly_order_ad_data_with_lag_overall$delayed_delivery_cnt <- NULL
weekly_order_ad_data_with_lag_overall$early_delivery_cnt <- NULL
weekly_order_ad_data_with_lag_overall$onetime_delivery_cnt <- NULL
weekly_order_ad_data_with_lag_overall$cod_cnt <- NULL
weekly_order_ad_data_with_lag_overall$prepaid_cnt <- NULL


##weekly_order_ad_data_with_lag_overall [,c(1,2,4:36)] <- scale(weekly_order_ad_data_with_lag_overall [,c(1,2,4:36)])

# Create sub data set for each sub category
weekly_order_ad_data_lag_cam <- filter ( weekly_order_ad_data_with_lag_overall ,weekly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("CameraAccessory")) 
weekly_order_ad_data_lag_cam$product_analytic_sub_category <- NULL

weekly_order_ad_data_lag_game <- filter ( weekly_order_ad_data_with_lag_overall ,weekly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("GamingAccessory")) 
weekly_order_ad_data_lag_game$product_analytic_sub_category <- NULL

weekly_order_ad_data_lag_home <- filter ( weekly_order_ad_data_with_lag_overall ,weekly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("HomeAudio")) 
weekly_order_ad_data_lag_home$product_analytic_sub_category <- NULL

######################################################################################################
## Start building distributed lag model for camera sub category 
#################################################################################################

weekly_order_ad_data_lag_cam1 <- weekly_order_ad_data_lag_cam
weekly_order_ad_data_lag_cam <- na.omit(weekly_order_ad_data_lag_cam)

weekly_order_ad_data_lag_cam [,c(1:35)] <- scale(weekly_order_ad_data_lag_cam [,c(1:35)])


set.seed(200)


trainindices= sample(1:nrow(weekly_order_ad_data_lag_cam), 0.7*nrow(weekly_order_ad_data_lag_cam))
#Generate the train data set
train_cam = weekly_order_ad_data_lag_cam[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_cam = weekly_order_ad_data_lag_cam[-trainindices,]



cam_model1 <- lm(tot_gmv~. , data = train_cam)
summary(cam_model1)


step_cam_model1 <- stepAIC(cam_model1, direction = "both")
summary(step_cam_model1)
vif(step_cam_model1)


## Removed content_ad_stock as high vif
step_cam_model2.1 <- lm(tot_gmv ~ TV + Sponsorship + Content.Marketing + 
                          X.Affiliates + SEM + Other + NPS + discount_over_mrp + list_price_lag_2 + 
                          list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                          tv_ad_stock  + sem_ad_stock + product_analytic_vertical.xCameraAccessory + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                          product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                          product_analytic_vertical.xCameraHousing  + 
                          product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                          product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                          product_analytic_vertical.xLens  + 
                          product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                          product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                          promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                          promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model2.1)
vif(step_cam_model2.1)



## removed sem_ad_stock as high vif
step_cam_model3 <- lm(tot_gmv ~ TV + Sponsorship + Content.Marketing + 
                        X.Affiliates + SEM + Other + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model3)
vif(step_cam_model3)

## removed TV due to high vif

step_cam_model4 <- lm(tot_gmv ~   Sponsorship + Content.Marketing + 
                        X.Affiliates + SEM + Other + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model4)
vif(step_cam_model4)



## removed Content.Marketing due to high vif

step_cam_model5 <- lm(tot_gmv ~   Sponsorship  + 
                        X.Affiliates + SEM + Other + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model5)
vif(step_cam_model5)

## Removed SEM as high vif


step_cam_model6 <- lm(tot_gmv ~   Sponsorship  + 
                        X.Affiliates  + Other + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model6)
vif(step_cam_model6)

## Removed other  as p is high 

step_cam_model7 <- lm(tot_gmv ~   Sponsorship  + 
                        X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model7)
vif(step_cam_model7)

## Removed product_analytic_vertical.xCameraEyeCup as p is high

step_cam_model8 <- lm(tot_gmv ~   Sponsorship  + 
                        X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model8)
vif(step_cam_model8)



### Removed product_analytic_vertical.xSoftbox  , product_analytic_vertical.xCameraFilmRolls due to high p
step_cam_model10 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                         list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                         promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model10)
vif(step_cam_model10)


### Removed promotion_type.xChristmas...New.Year.Sale due to high p 
step_cam_model11 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                         list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                         promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model11)
vif(step_cam_model11)

### Removed promotion_type.xNo_promotion due to high p 
step_cam_model12 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                         list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale 
                       , data = train_cam)


summary(step_cam_model12)
vif(step_cam_model12)


### Removed list_price_change_from_w2 due to high vif 
step_cam_model13 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                         list_price_lag_3  + list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale 
                       , data = train_cam)


summary(step_cam_model13)
vif(step_cam_model13)

## Removed list_price_lag_3 due to high vif 

step_cam_model14 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                         list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale 
                       , data = train_cam)


summary(step_cam_model14)
vif(step_cam_model14)

## Removed list_price_lag_2 due to high P 
step_cam_model15 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp  + 
                         list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale 
                       , data = train_cam)


summary(step_cam_model15)
vif(step_cam_model15)

## Removed list_price_change_from_w3 due to high vif 

step_cam_model16 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp   
                       + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale 
                       , data = train_cam)


summary(step_cam_model16)
vif(step_cam_model16)

#################################################################################
## Model evalution for test data set
################################################################################

gmv_cam_prediction <- predict(step_cam_model16, test_cam[,-3])
test_cam$predicted_gmv <- gmv_cam_prediction

cam_r <- cor(test_cam$tot_gmv, test_cam$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared   ####Rsquaredo on test = 0.798

## Cross validation 
cam_lm_cv <- cv.lm(data = weekly_order_ad_data_lag_cam , form.lm = step_cam_model16, m =10)
###   ms 
###  0.317

################################################################
#### Plot elasticity for lag disttibuted model - Camera accessory sub category 
################################################################

cam_final_model <- step_cam_model16

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
  ggtitle("Camera Accessory - Lag Model") +xlab("Variables")



###################################################################################################
### Start building distirubuted lag model for game accessory category
##################################################################################################
weekly_order_ad_data_lag_game <- filter ( weekly_order_ad_data_with_lag_overall ,weekly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("GamingAccessory")) 
weekly_order_ad_data_lag_game$product_analytic_sub_category <- NULL
weekly_order_ad_data_lag_game1 <- weekly_order_ad_data_lag_game
weekly_order_ad_data_lag_game <- na.omit(weekly_order_ad_data_lag_game)

weekly_order_ad_data_lag_game [,c(1:35)] <- scale(weekly_order_ad_data_lag_game [,c(1:35)])


set.seed(100)


trainindices= sample(1:nrow(weekly_order_ad_data_lag_game), 0.7*nrow(weekly_order_ad_data_lag_game))
#Generate the train data set
train_game = weekly_order_ad_data_lag_game[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_game = weekly_order_ad_data_lag_game[-trainindices,]

##train[, 1:36] <- as.data.frame(scale(train[, 1:36]))

game_model1 <- lm(tot_gmv~. , data = train_game)
summary(game_model1)


step_game_model1 <- stepAIC(game_model1, direction = "both")
summary(step_game_model1)
vif(step_game_model1)

## Removed online_ad_stock due to high vif 

step_game_model3 <- lm( tot_gmv ~ list_price + Sponsorship + SEM + Radio + 
                          Other + NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                          gmv_change_from_w3 + list_price_lag_2 + content_ad_stock + 
                          sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                          promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                          promotion_type.xDaussera.sale 
                        + promotion_type.xRepublic.Day, 
                        data = train_game)

summary(step_game_model3)
vif(step_game_model3)

##  sem_ad_stock due to high vif


step_game_model4 <- lm(tot_gmv ~ list_price + Sponsorship + SEM + Radio + 
                         Other + NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2 + content_ad_stock + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model4)
vif(step_game_model4)

## Removed Other due to high vif
step_game_model5 <- lm(tot_gmv ~ list_price + Sponsorship + SEM + Radio + 
                         NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2 + content_ad_stock + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model5)
vif(step_game_model5)


## Removed Radio due to high vif
step_game_model6 <- lm(tot_gmv ~ list_price + Sponsorship + SEM  + 
                         NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2 + content_ad_stock + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model6)
vif(step_game_model6)


## Removed content_ad_stock as vif is high 
step_game_model7 <- lm(tot_gmv ~ list_price + Sponsorship + SEM  + 
                         NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2  + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model7)
vif(step_game_model7)



## Removed list_price as p is high 
step_game_model8 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                         NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2  + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model8)
vif(step_game_model8)

## Removed promotion_type.xBig.Diwali.Sale as p is high 
step_game_model9 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                         NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2  + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model9)
vif(step_game_model9)

## Removed promotion_type.xBSD.5 and promotion_type.xRepublic.Day as p is high 

step_game_model10 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                          gmv_change_from_w3 + list_price_lag_2  + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker  
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model10)
vif(step_game_model10)

## Removed list_price_lag_2 as p is high
step_game_model11 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                          gmv_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker  
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model11)
vif(step_game_model11)

## Removed gmv_change_from_w3 and NPS as p is high
step_game_model12 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 
                        + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker  
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model12)
vif(step_game_model12)




## Removed  discount_over_mrp as p is  is high
step_game_model13 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          gmv_lag_1 + gmv_change_from_w2 
                        + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker  
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model13)
vif(step_game_model13)


## Removed product_analytic_vertical.xGamingSpeaker as P is  is high
step_game_model14 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          gmv_lag_1 + gmv_change_from_w2 
                        + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse   
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model14)
vif(step_game_model14)



## Removed gmv_lag_1 as P is  is high
step_game_model15 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          gmv_change_from_w2 
                        + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse   
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model15)
vif(step_game_model15)

## Removed gmv_change_from_w2 as P is  is high
step_game_model16 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse   
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model16)
vif(step_game_model16)

### Model evalution on test data set for game accessory

gmv_game_prediction <- predict(step_game_model16, test_game[,-3])
test_game$predicted_gmv <- gmv_game_prediction

game_r <- cor(test_game$tot_gmv, test_game$predicted_gmv)
game_rsquared <- game_r^2
game_rsquared   ##Rsquare on test = 0.68

## Cross validation
game_lm_cv <- cv.lm(data = weekly_order_ad_data_lag_game , form.lm = step_game_model16, m =10)

####ms   
##0.322

##################################################################################

game_final_model <- step_game_model16

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
  ggtitle("Gaming  Accessory -  LagDistributed  Model") +xlab("Variables")


#######################################################################################

######################################################################################
weekly_order_ad_data_lag_home1 <- weekly_order_ad_data_lag_home
weekly_order_ad_data_lag_home <- na.omit(weekly_order_ad_data_lag_home)

weekly_order_ad_data_lag_home [,c(1:35)] <- scale(weekly_order_ad_data_lag_home [,c(1:35)])


set.seed(100)


trainindices= sample(1:nrow(weekly_order_ad_data_lag_home), 0.7*nrow(weekly_order_ad_data_lag_home))
#Generate the train data set
train_home = weekly_order_ad_data_lag_home[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_home = weekly_order_ad_data_lag_home[-trainindices,]


home_model1 <- lm(tot_gmv~. , data = train_home)
summary(home_model1)


step_home_model1 <- stepAIC(home_model1, direction = "both")
summary(step_home_model1)
vif(step_home_model1)

## Removed online_ad_stock due to high vif 
step_home_model2 <- lm( tot_gmv ~ week_no + Digital + Content.Marketing + 
                          Online.marketing + X.Affiliates + Radio + discount_over_mrp + 
                          gmv_lag_3 + list_price_change_from_w3 + dig_ad_stock + content_ad_stock + 
                          radio_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                          promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model2)
vif(step_home_model2)


## Removed content_ad_stock due to high vif 
step_home_model3 <- lm(tot_gmv ~ week_no + Digital + Content.Marketing + 
                         Online.marketing + X.Affiliates + Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3 + dig_ad_stock  + 
                         radio_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model3)
vif(step_home_model3)


## Removed X.Affiliates due to high vif 
step_home_model4 <- lm(tot_gmv ~ week_no + Digital + Content.Marketing + 
                         Online.marketing  + Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3 + dig_ad_stock  + 
                         radio_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model4)
vif(step_home_model4)


## Removed dig_ad_stock due to high vif 
step_home_model5 <- lm(tot_gmv ~ week_no + Digital + Content.Marketing + 
                         Online.marketing  + Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3   + 
                         radio_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model5)
vif(step_home_model5)



## Removed  radio_ad_stock due to high vif 
step_home_model6 <- lm(tot_gmv ~ week_no + Digital + Content.Marketing + 
                         Online.marketing  + Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3   + 
                         affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home) 


summary(step_home_model6)
vif(step_home_model6)


## Removed Online.marketing due to high vif 
step_home_model7 <- lm(tot_gmv ~ week_no + Digital + Content.Marketing + 
                         Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3   + 
                         affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model7)
vif(step_home_model7)


## Removed Content.Marketing due to high vif 
step_home_model8 <- lm(tot_gmv ~ week_no + Digital  + 
                         Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3   + 
                         affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model8)
vif(step_home_model8)


## Removed promotion_type.xChristmas...New.Year.Sale due to high P 
step_home_model9 <- lm(tot_gmv ~ week_no + Digital  + 
                         Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3   + 
                         affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model9)
vif(step_home_model9)


## Removed gmv_lag_3 due to high P 
step_home_model10 <- lm(tot_gmv ~ week_no + Digital  + 
                          Radio + discount_over_mrp + 
                          list_price_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale + 
                          promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model10)
vif(step_home_model10)




## Removed Radio due to high P 
step_home_model11 <- lm(tot_gmv ~ week_no + Digital  + 
                          discount_over_mrp + 
                          list_price_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale + 
                          promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model11)
vif(step_home_model11)

##Removed promotion_type.xNo_promotion due to high p 

step_home_model12 <- lm(tot_gmv ~ week_no + Digital  + 
                          discount_over_mrp + 
                          list_price_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale 
                        , data = train_home  ) 


summary(step_home_model12)
vif(step_home_model12)


##Removed Digital due to high p 

step_home_model13 <- lm(tot_gmv ~ week_no   + 
                          discount_over_mrp + 
                          list_price_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale 
                        , data = train_home   ) 


summary(step_home_model13)
vif(step_home_model13)


##Removed week_no due to high p 

step_home_model14 <- lm(tot_gmv ~    
                          discount_over_mrp + 
                          list_price_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale 
                        , data = train_home  ) 


summary(step_home_model14)
vif(step_home_model14)

## Removed list_price_change_from_w3 due to high p 
step_home_model15 <- lm(tot_gmv ~    
                          discount_over_mrp 
                        + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale 
                        , data = train_home  ) 


summary(step_home_model15) 
vif(step_home_model15)

## Model evalution for distributed lag model - home audio 

gmv_home_prediction <- predict(step_home_model15, test_home[,-3])
test_home$predicted_gmv <- gmv_home_prediction

home_r <- cor(test_home$tot_gmv, test_home$predicted_gmv)
home_rsquared <- home_r^2
home_rsquared   ###Rsquare on test = 0.74

## 10 fold Cross validation 

home_lm_cv <- cv.lm(data = weekly_order_ad_data_lag_home , form.lm = step_home_model15, m =10)
###ms 
###0.42

##############################################################################################
## Elasticity plot for distributed lag model - Home audio 
#############################################################################################
home_final_model <- step_home_model15

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
  ggtitle("Gaming  Accessory -  LagDistributed  Model") +xlab("Variables")






