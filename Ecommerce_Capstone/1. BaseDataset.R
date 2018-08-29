## Load required packages 

requiredPackages = c('DataCombine','scales','glmnet','DAAG','caret','GGally','corrplot','lubridate','gdata','ggplot2','dplyr','reshape','tidyr','data.table','MASS','car')

#Installing the required packages
for(item in requiredPackages){
  if(!require(item,character.only = TRUE)){
    install.packages(item)
  }
  library(item, character.only = TRUE)
}


############################################################################################
#### Read the ad spend details 
###########################################################################################
ad_dtls <- read.xls("Media data and other information.xlsx", sheet = 2, header = TRUE, skip = 1)
ad_dtls$year_month <- paste(ad_dtls$Year , ad_dtls$Month , sep = '-')
ad_dtls$Year <- as.numeric(ad_dtls$Year)
ad_dtls$Month <- as.numeric(ad_dtls$Month)

ad_dtls$year_month <- factor(ad_dtls$year_month, levels = ad_dtls$year_month[order(ad_dtls$Year , ad_dtls$Month)])

#ad_dtls[order(ad_dtls$Year, ad_dtls$Month),] %>% ggplot(aes(x = year_month, y = Total.Investment)) + geom_bar(stat = "identity")
ggplot(ad_dtls, aes(x = year_month, y = Total.Investment)) + geom_bar(stat = "identity")

## Spend  is lowest for aug
## Spend is high in Sep,Oct,Dec,Mar

##ad_dtls $ month_no [ad_dtls $ Year == 2015] <- (ad_dtls $ Month [ad_dtls $ Year == 2015]) 
##ad_dtls $ month_no [ad_dtls $ Year == 2016] <- (ad_dtls $ Month [ad_dtls $ Year == 2016]) 

#Investments Radio and others is assigned 0 for NA values
sapply(ad_dtls , function(x) sum(is.na(x)))
ad_dtls[which(is.na(ad_dtls$Radio)), "Radio"] <- 0 
ad_dtls[which(is.na(ad_dtls$Other)), "Other"] <- 0

ad_dtls_long <- gather(ad_dtls, Medium, Spend, 3:12)

ggplot(ad_dtls_long, aes (x = Month, y = Spend, colour = Medium)) + geom_line() + 
  scale_x_discrete(name="Months since May 2015", limits=seq(1,12,1))

#Removing Total investments from data, as it is the sum of all the mediums and converted to crore value
ad_dtls$Total.Investment <- NULL
ad_dtls[,3:11] <- ad_dtls[,3:11] * 10000000

##########################################################################################
# Import the order data set
##########################################################################################
order_rawdata <- read.csv ( "ConsumerElectronics.csv" , header = T , stringsAsFactors = F)
nrow(order_rawdata)  ##1648824
str(order_rawdata)

######################################################################################
## Data related checks 
#####################################################################################

## No issue with case sensitivity
sapply(order_rawdata, function(x) length(unique(toupper(x)))-length(unique(tolower(x)))) 

## NA values are there in gmv, cust_id, pincode. 4904 missing values in each column
sapply(order_rawdata, function(x){sum(is.na(x))})   

filter(order_rawdata , order_rawdata$gmv < 0 )  ## 0 row
filter(order_rawdata , order_rawdata$gmv == 0 ) ## 1349 rows 

filter(order_rawdata , order_rawdata$units < 0 )  ## 0 row
filter(order_rawdata , order_rawdata$units == 0 ) ## 0 row

table(order_rawdata$deliverybdays)  ## rows with negative as well as very high deliverybdays 
table(order_rawdata$deliverycdays)  ## rows with negative as well as very high deliverycdasy

unique(order_rawdata$s1_fact.order_payment_type)  ## Two payment type available - COD , Prepaid 
table(order_rawdata$s1_fact.order_payment_type)  ## COD is preferred one 

table(order_rawdata$sla)  ## rows with 0 sla. They are same day delivery. Few rows with high sla

length(unique(order_rawdata$pincode)) ## 7565 unique values 
length(unique(order_rawdata$cust_id)) ## 1201090 distinct customers 

unique(order_rawdata$product_analytic_super_category) ## Single value CE 
unique(order_rawdata$product_analytic_sub_category)   ## 14 distinct values 
unique(order_rawdata$product_analytic_category)       ## 5 distinct values 
unique(order_rawdata$product_analytic_vertical)       ## 74 distinct values 

## 51 product_analytic_vertical under CameraAccessory", "GamingAccessory", "HomeAudio"
filter ( order_rawdata ,order_rawdata$product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio")) %>% group_by(product_analytic_sub_category , product_analytic_vertical)%>% summarise(  count = n()) 

filter(order_rawdata , order_rawdata$product_mrp < 0 )  ## 0 row
nrow(filter(order_rawdata , order_rawdata$product_mrp == 0 )) ## 5308 row


min(order_rawdata$order_date)   ## "2015-05-19 13:42:09"
max (order_rawdata$order_date ) ## "2016-07-25 01:19:45"

## Create a year-month variable 
order_rawdata$year_month <- paste(order_rawdata$Year, order_rawdata$Month , sep = '-')
order_rawdata$order_date <- as.Date(order_rawdata$order_date)
order_rawdata$start_week_date <-   floor_date(as.Date(order_rawdata$order_date), unit="week" , week_start = getOption("lubridate.week.start", 1))
order_rawdata[which(order_rawdata$start_week_date < '2015-07-01'),"start_week_date"] <- '2015-07-01'


## Find out the week number from a date
order_rawdata$week_no <-   strftime( order_rawdata$start_week_date ,format="%V")
## Create a year-month variable 
order_rawdata$year_month <- paste(order_rawdata$Year, order_rawdata$Month , sep = '-')
## Filter for date range July 2015 to June 2016
order_rawdata <- subset(order_rawdata, order_rawdata$order_date >= '2015-07-01' & order_rawdata$order_date < '2016-07-01')
## Filter out the rows  having missing values 
row.has.na <- apply(order_rawdata, 1, function(x){any(is.na(x))})
sum(row.has.na) #4904
## Remove the missing values 
order_rawdata <- order_rawdata[!row.has.na,]
order_rawdata$week_no <- as.numeric(order_rawdata$week_no)
## Add a varaiable to specify month number as per week start date 
order_rawdata$month_asper_week_startdate <- format(order_rawdata$start_week_date , "%m")

order_rawdata$month_asper_week_startdate <- as.numeric(order_rawdata$month_asper_week_startdate)
## Number the week from 1 to 53
## July 1st week will be 1 and june last week will be 53 


order_rawdata$week_no[order_rawdata$Year == 2015 ] <- (order_rawdata$week_no[order_rawdata$Year == 2015 ]) -26
order_rawdata$week_no[order_rawdata$Year == 2016 & order_rawdata$week_no !=53 ] <- (order_rawdata$week_no[order_rawdata$Year == 2016 & order_rawdata$week_no !=53 ]) +27
order_rawdata[which(order_rawdata$Year == 2016 & order_rawdata$Month==1 &order_rawdata$week_no == 53 ), "week_no"] <- order_rawdata[which(order_rawdata$Year == 2016 & order_rawdata$Month==1 &order_rawdata$week_no == 53 ), "week_no"] - 26



## Filter out the rows having mrp value 0 
order_rawdata <- order_rawdata[!order_rawdata$product_mrp == 0,]

##order_rawdata$deliverybdays <- gsub('\N' , '0' , order_rawdata$deliverybdays )
##order_rawdata$deliverycdays <- gsub('\\N' , '0' , order_rawdata$deliverycdays )

order_rawdata$deliverybdays[order_rawdata$deliverybdays < 0] = 0
order_rawdata$deliverycdays[order_rawdata$deliverycdays < 0] = 0
order_rawdata$product_procurement_sla [order_rawdata$product_procurement_sla <0 ] =0

order_rawdata$deliverybdays <- as.numeric(order_rawdata$deliverybdays)
order_rawdata$deliverycdays <- as.numeric(order_rawdata$deliverycdays)
order_rawdata$sla <- as.numeric(order_rawdata$sla)
order_rawdata$delivery_on_time <- order_rawdata $sla - (order_rawdata$deliverybdays+order_rawdata$deliverycdays+order_rawdata$product_procurement_sla)
order_rawdata$delivery_status[order_rawdata$delivery_on_time < 0] <- 'Delayed'
order_rawdata$delivery_status[order_rawdata$delivery_on_time == 0] <- 'On time'
order_rawdata$delivery_status[order_rawdata$delivery_on_time > 0] <- 'Early'

###########################################################################
##  Read the promotional details 
##########################################################################
special_sale <- read.xls("Media data and other information.xlsx", sheet = 3, header = TRUE ,stringsAsFactors = F   )
special_sale$Year[1:6] <- 2015
special_sale$Year[7:12] <- 2016
special_sale$X <- NULL
#Derived manually from holidays
special_sale$start_week_no <- c(3,7,9,16,19,26,30,32,34,33,37,48)
special_sale$end_week_no <- c(3,8,9,16,20,27,30,32,34,34,37,48)
special_sale$promotion_type <- trim(sapply(special_sale$Sales.Calendar, function(x) substr(x , 1,  (regexpr("\\(", x[1])-1 ))))
special_sale$Sales.Calendar <- NULL
special_sale_long <- gather ( special_sale , week_type , week_no , 2:3)
special_sale_long$week_type <- NULL

##############################################################################
## Read the satisfaction score 
#############################################################################
month_np_score <- read.xls("Media data and other information.xlsx", sheet = 4, header = TRUE)
str(month_np_score)
month_np_score <- month_np_score[2:13]
t_month_np_score <- transpose(month_np_score)
t_month_np_score$Month <- c(seq(7,12,1),seq(1,6,1))
colnames(t_month_np_score)[1] <- "NPS"

#############################################################################
### Aggragate the multiple dataset to create a master dataset at weekly level 
#############################################################################

## Group the data at weekly level 
weekly_order_data <- order_rawdata %>% group_by ( Year, month_asper_week_startdate,  product_analytic_category,product_analytic_sub_category, product_analytic_vertical,year_month , week_no)%>% summarise( prepaid_cnt =  sum(ifelse (s1_fact.order_payment_type =='Prepaid' , 1 , 0)) ,cod_cnt =  sum(ifelse (s1_fact.order_payment_type =='COD' , 1,0)) ,delayed_delivery_cnt =sum(ifelse (delivery_status =='Delayed' , 1 , 0)), early_delivery_cnt =sum(ifelse (delivery_status =='Early' , 1 , 0)), onetime_delivery_cnt =sum(ifelse (delivery_status =='On time' , 1 , 0)), tot_gmv = sum(gmv) , tot_units = sum(units) , tot_product_mrp = sum( as.numeric (product_mrp)), avg_gmv = mean(gmv) , avg_mrp = mean(product_mrp) , no_of_customer = length(unique(cust_id)), no_of_orders = length(unique(order_id)) , list_price = (tot_gmv/tot_units) , avg_price = mean(list_price) )

colnames(weekly_order_data)[2] <- "Month"

## Merge the ad data with weekly data 
weekly_order_ad_data <- merge(weekly_order_data ,ad_dtls , by=c("Year" , "Month"))

## dont need this variable any more
weekly_order_ad_data$year_month.y <- NULL 

## Merge the NPS data  
weekly_order_ad_data <- merge(weekly_order_ad_data ,t_month_np_score , by=c(  "Month"))

## Find out how many entries are there in a month  
week_in_a_month <- weekly_order_ad_data %>% group_by( Month ) %>% summarize (  tot_week = length(unique(week_no)) )

weekly_order_ad_data <- merge(weekly_order_ad_data ,week_in_a_month, by = c ( "Month") )

rows_ina_week <- weekly_order_ad_data %>% group_by( week_no ) %>% summarize ( total_row = n())

weekly_order_ad_data <- merge(weekly_order_ad_data ,rows_ina_week, by = c ( "week_no") )

## Convert monthly ad spend into weekly ad spend 
weekly_order_ad_data[,c(22:30)] <- weekly_order_ad_data[,c(22:30)]/(weekly_order_ad_data$tot_week*weekly_order_ad_data$total_row)

## Add the promotional sale name in dataset
##weekly_order_ad_data <- merge(weekly_order_ad_data ,special_sale_long, by.x = c ( "Year" , "week_no")  , by.y   = c ( "Year" , "week_no") , all.x=TRUE )

weekly_order_ad_data$promotion_type <- NULL
for (row_no  in 1:nrow(special_sale) ) {
  for (week in special_sale[row_no,2] : special_sale[row_no,3] ){
    print(paste("The week is", week))
    weekly_order_ad_data[which(weekly_order_ad_data$week_no==week),"promotion_type"]  <-   special_sale[row_no,4]
  }
}

#############################################################################################
## Engineered variables 
###############################################################################################
weekly_order_ad_data$discount_over_mrp <- (weekly_order_ad_data$tot_product_mrp-weekly_order_ad_data$tot_gmv)/weekly_order_ad_data$tot_product_mrp
weekly_order_ad_data$Holiday_week <- ifelse (is.na(weekly_order_ad_data$promotion_type) , 'N','Y' )
weekly_order_ad_data[which(is.na(weekly_order_ad_data$promotion_type)), "promotion_type"] <- "No_promotion"
weekly_order_ad_data$value_per_visitor <- weekly_order_ad_data$tot_gmv/weekly_order_ad_data$no_of_customer

#############################################################################
### Perform EDA analysis on weekly_order_ad_data and order_rawdata
## EDA will show we need different ad spend for each product category 
#############################################################################

## Month wise ad spend details 
ggplot(ad_dtls_long, aes (x = Month, y = Spend, colour = Medium)) + geom_line() + 
  scale_x_discrete(name="Months since May 2015", limits=seq(1,12,1))

## Create week wise sale and ad spend details for various sub category level 
ad_sale_dtls <- weekly_order_ad_data %>% group_by (product_analytic_sub_category, week_no)%>% 
  summarise(tot_sales = sum(tot_gmv) ,
            tot_tv_spend = sum (TV), tot_dig_spend = sum (Digital), 
            tot_spon_spend = sum(Sponsorship) , tot_content_spend = sum(Content.Marketing),
            tot_online_spend = sum(Online.marketing) ,tot_aff_spend = sum(X.Affiliates),
            tot_sem_spend = sum(SEM) ,tot_radio_spend = sum(Radio), 
            tot_oter_spend = sum(Other))


## weekly sale details for different sub category 
p <- ggplot(ad_sale_dtls , aes ( x = week_no , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "week", y = "Sales " ) + ggtitle ( " Sales  vs Total Ad spend")

## Total TV ad spend vs sales details
p <- ggplot(ad_sale_dtls , aes ( x = tot_tv_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs TV Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_dig_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Digital Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_spon_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Sponsor Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_content_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Content Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_online_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Online Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_aff_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Affiliate Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_sem_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs SEM Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_radio_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Radio Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_oter_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Other Ad")

## Sale at different promotional and non promotional weeks
weekly_order_ad_data %>% group_by ( promotion_type) %>% summarise( Avg_sale = mean(tot_gmv)) %>% ggplot( aes ( x=promotion_type, y =Avg_sale  )) + geom_bar(stat = "identity")

## Sale at different promotional weeks for different sub categories 
weekly_order_ad_data %>% group_by ( product_analytic_sub_category ,promotion_type) %>% summarise( Avg_sale = mean(tot_gmv)) %>% ggplot( aes ( x=promotion_type, y =Avg_sale  )) + geom_bar(stat = "identity") +facet_wrap( ~ product_analytic_sub_category, nrow =2, ncol = 7)+theme(axis.text.x=element_text(angle = -90, hjust = 0))

## weekly ad spend vs sales 
sale_vs_week_ad <- weekly_order_ad_data %>% group_by ( week_no) %>% summarise(tot_sales = sum(tot_gmv) , ad_spend = sum(TV+Digital+Sponsorship+Content.Marketing+Online.marketing+X.Affiliates+SEM+Radio+Other))
sale_vs_week_ad_long <- gather(sale_vs_week_ad, Type, Spend, 2:3)
ggplot(sale_vs_week_ad_long, aes ( x= week_no , y = Spend , color = Type))+geom_line()

### Disocunt percentage vs average sales

discount_vs_sales <- weekly_order_ad_data[,c("tot_gmv" , "discount_over_mrp")]
discount_vs_sales$discount_range <- ifelse (discount_vs_sales$discount_over_mrp <= .1 , 'up to 10', ifelse ( discount_vs_sales$discount_over_mrp > .1 & discount_vs_sales$discount_over_mrp <= .3 , 'up to 30', ifelse(discount_vs_sales$discount_over_mrp > .3 & discount_vs_sales$discount_over_mrp <= .5 , 'up to 50','>50') ))
discount_vs_sales %>% group_by(discount_range) %>% summarise( avg_sale = mean(tot_gmv)) %>% ggplot(aes ( x=discount_range , y =avg_sale  )) + geom_bar(stat = "identity")

## Avg discount at different promotional and non promotional week
weekly_order_ad_data %>% group_by(promotion_type) %>% summarise(avg_disc = mean(discount_over_mrp)) %>% ggplot(aes(x= promotion_type, y =avg_disc )) + geom_bar(stat = "identity")

## Nps vs week
weekly_order_ad_data %>% group_by(week_no) %>% summarise(nps = mean(NPS)) %>% ggplot(aes(x= week_no, y =nps )) + geom_bar(stat = "identity")

## payment type vs number of orders
order_rawdata %>% group_by ( s1_fact.order_payment_type) %>% summarise(order_cnt = n()) %>% ggplot(aes(x= s1_fact.order_payment_type, y =order_cnt )) + geom_bar(stat = "identity")

## delivery_status  vs number of orders
order_rawdata %>% group_by ( delivery_status) %>% summarise(order_cnt = n()) %>% ggplot(aes(x= delivery_status, y =order_cnt )) + geom_bar(stat = "identity")


############################################################################
###create 3 different data set & add engineered kpis
#############################################################################
unique(weekly_order_ad_data$product_analytic_category)
## Since model needs to be built at sub category level, this varaiable is needed 
weekly_order_ad_data$product_analytic_category <- NULL 
weekly_order_ad_data$year_month.x <- NULL 

## Create a dataset only for Home audio , camera accessory and gaming accessories 
weekly_order_ad_data <- filter ( weekly_order_ad_data ,weekly_order_ad_data$product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio")) 

## Dummy variable creation for character data types 
weekly_order_ad_data_chr <- weekly_order_ad_data[,c(5,32,34)]
weekly_order_ad_data_fact <- data.frame(sapply(weekly_order_ad_data_chr, function(x) factor(x)))
str(weekly_order_ad_data_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(weekly_order_ad_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =weekly_order_ad_data_fact))[,-1]))

## Create master data set by appending dummies with main data set 
weekly_order_ad_data_overall <- cbind(weekly_order_ad_data[,c(1:4,6:31,33,35)],dummies) 
View(weekly_order_ad_data_overall) 

###############################################################################################
### Outlier treatment 
###############################################################################################


boxplot(weekly_order_ad_data_overall$tot_gmv )
boxplot(weekly_order_ad_data_overall$tot_units)
boxplot(weekly_order_ad_data_overall$tot_product_mrp)
boxplot(weekly_order_ad_data_overall$TV)
boxplot(weekly_order_ad_data_overall$Digital)
boxplot(weekly_order_ad_data_overall$Sponsorship)
boxplot(weekly_order_ad_data_overall$Content.Marketing)
boxplot(weekly_order_ad_data_overall$Online.marketing)
boxplot(weekly_order_ad_data_overall$X.Affiliates)
boxplot(weekly_order_ad_data_overall$SEM)
boxplot(weekly_order_ad_data_overall$Radio)
boxplot(weekly_order_ad_data_overall$Other)


## Since there are lots of outliers  in dataset, they cant be removed. 
## So they have been capped by appropiate quantile decided by looking at data spread

overall_quantile <- sapply(weekly_order_ad_data_overall[,c("tot_gmv","tot_units", "tot_product_mrp" , "TV" ,"Digital",
                                                           "Sponsorship", "Content.Marketing", "Online.marketing" ,"X.Affiliates", "SEM" ,"Radio" , "Other" )], 
                           function(x) quantile(x,seq(0,1,.01),na.rm = T)) 

## remove_outliers function for capping the value to specific quantile

remove_outliers <- function(x , lower_quantile, upper_quantile) {
  qnt <- quantile(x, probs=c(lower_quantile, upper_quantile), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < qnt[1]] <- qnt[1]
  y[x > qnt[2]] <- qnt[2]
  y
}

weekly_order_ad_data_overall$tot_gmv <- remove_outliers (weekly_order_ad_data_overall$tot_gmv,0, .97 ) 
weekly_order_ad_data_overall$tot_units <- remove_outliers (weekly_order_ad_data_overall$tot_units,0, .97 ) 
weekly_order_ad_data_overall$tot_product_mrp <- remove_outliers (weekly_order_ad_data_overall$tot_product_mrp,0, .97 ) 
weekly_order_ad_data_overall$TV <- remove_outliers (weekly_order_ad_data_overall$TV,0, .98 ) 
weekly_order_ad_data_overall$Digital <- remove_outliers (weekly_order_ad_data_overall$Digital,0, .95 ) 
weekly_order_ad_data_overall$Sponsorship <- remove_outliers (weekly_order_ad_data_overall$Sponsorship,0, .95 ) 
weekly_order_ad_data_overall$Content.Marketing <- remove_outliers (weekly_order_ad_data_overall$Content.Marketing,0, .95 ) 
weekly_order_ad_data_overall$SEM <- remove_outliers (weekly_order_ad_data_overall$SEM,0, .95 ) 
weekly_order_ad_data_overall$Radio <- remove_outliers (weekly_order_ad_data_overall$Radio,0, .95 ) 
weekly_order_ad_data_overall$Other <- remove_outliers (weekly_order_ad_data_overall$Other,0, .95 )

## Find out  how many distinct values are there are for different columns
sapply(weekly_order_ad_data_overall, function(x) length(unique(x)))

weekly_order_ad_data_overall$total_row <- NULL
weekly_order_ad_data_overall$tot_week <- NULL

##  Take back up of master dataset weekly_order_ad_data_overall

weekly_order_ad_data_overall2 <- weekly_order_ad_data_overall

## Check the correlation among multiple varaiables to decide which varaiables are highly corelated with each other
## Column 4 has been excluded as it contains sub category 

corr <- cor(weekly_order_ad_data_overall2[,-c (4)])

##Depending on higher corelation or since there vars are direct proxy to sales , so taking them out
weekly_order_ad_data_overall$avg_mrp <- NULL
weekly_order_ad_data_overall$avg_price <- NULL
weekly_order_ad_data_overall$tot_units <- NULL
weekly_order_ad_data_overall$no_of_orders <- NULL
weekly_order_ad_data_overall$tot_product_mrp <- NULL
weekly_order_ad_data_overall$avg_gmv <- NULL
weekly_order_ad_data_overall$value_per_visitor <- NULL
weekly_order_ad_data_overall$Year <- NULL
weekly_order_ad_data_overall$no_of_customer <- NULL
weekly_order_ad_data_overall$delayed_delivery_cnt <- NULL
weekly_order_ad_data_overall$early_delivery_cnt <- NULL
weekly_order_ad_data_overall$onetime_delivery_cnt <- NULL
weekly_order_ad_data_overall$cod_cnt <- NULL
weekly_order_ad_data_overall$prepaid_cnt <- NULL

## Create 3 data set HomeAudio, GamingAccessory and CameraAccessory  for model building. 
list2env(split( weekly_order_ad_data_overall[,-3], weekly_order_ad_data_overall$product_analytic_sub_category), envir = .GlobalEnv)
str(HomeAudio)
str(GamingAccessory)
str(CameraAccessory)

nrow(HomeAudio)
nrow(GamingAccessory)
nrow(CameraAccessory)

