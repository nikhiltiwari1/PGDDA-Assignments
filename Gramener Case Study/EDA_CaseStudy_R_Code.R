################# EDA Case Study - R Code ###################
#############################################################
requiredPackages = c('stringr','DescTools','lubridate','tidyverse')

#Installing the required packages
for(item in requiredPackages){
  if(!require(item,character.only = TRUE)){
    install.packages(item)
  }
  library(item, character.only = TRUE)
}

loan <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)  
str(loan)

################# Factorizing the required columns #################
factorColumns <- c('term','grade','sub_grade','emp_length','home_ownership','member_id','verification_status','loan_status','purpose',
                   'addr_state','delinq_2yrs','mths_since_last_delinq','mths_since_last_record','open_acc','inq_last_6mths','pub_rec','total_acc','initial_list_status',
                   'policy_code','application_type','chargeoff_within_12_mths','pub_rec_bankruptcies','tax_liens')

loan[,factorColumns] <- lapply(loan[,factorColumns], factor)
str(loan)

################# Cleaning - Assigning NULL to unwanted columns #################
loan$pymnt_plan<-NULL
loan$url<-NULL
loan$desc<-NULL
loan$title<-NULL
loan$earliest_cr_line<-NULL
loan$mths_since_last_major_derog<-NULL
loan$annual_inc_joint<-NULL
loan$dti_joint<-NULL
loan$acc_now_delinq<-NULL
loan$tot_coll_amt<-NULL
loan$tot_cur_bal<-NULL
loan$open_acc_6m<-NULL
loan$open_il_6m<-NULL
loan$open_il_12m<-NULL
loan$open_il_24m<-NULL
loan$mths_since_rcnt_il<-NULL
loan$total_bal_il<-NULL
loan$il_util<-NULL
loan$open_rv_12m<-NULL
loan$open_rv_24m<-NULL
loan$max_bal_bc<-NULL
loan$all_util<-NULL
loan$total_rev_hi_lim<-NULL
loan$inq_fi<-NULL
loan$total_cu_tl<-NULL
loan$inq_last_12m<-NULL
loan$acc_open_past_24mths<-NULL
loan$avg_cur_bal<-NULL
loan$bc_open_to_buy<-NULL
loan$bc_util<-NULL
loan$delinq_amnt<-NULL
loan$mo_sin_old_il_acct<-NULL
loan$mo_sin_old_rev_tl_op<-NULL
loan$mo_sin_rcnt_rev_tl_op<-NULL
loan$mo_sin_rcnt_tl<-NULL
loan$mort_acc<-NULL
loan$mths_since_recent_bc<-NULL
loan$mths_since_recent_bc_dlq<-NULL
loan$mths_since_recent_inq<-NULL
loan$mths_since_recent_revol_delinq<-NULL
loan$num_accts_ever_120_pd<-NULL
loan$num_actv_bc_tl<-NULL
loan$num_actv_rev_tl<-NULL
loan$num_bc_sats<-NULL
loan$num_bc_tl<-NULL
loan$num_il_tl<-NULL
loan$num_op_rev_tl<-NULL
loan$num_rev_accts<-NULL
loan$num_rev_tl_bal_gt_0<-NULL
loan$num_sats<-NULL
loan$num_tl_120dpd_2m<-NULL
loan$num_tl_30dpd<-NULL
loan$num_tl_90g_dpd_24m<-NULL
loan$num_tl_op_past_12m<-NULL
loan$pct_tl_nvr_dlq<-NULL
loan$percent_bc_gt_75<-NULL
loan$tot_hi_cred_lim<-NULL
loan$total_bal_ex_mort<-NULL
loan$total_bc_limit<-NULL
loan$total_il_high_credit_limit<-NULL

################# Data Cleaning - Date conversions #################
loan$issue_d <- as.Date(loan$issue_d,"%b-%d")
loan$last_pymnt_d <- as.Date(loan$last_pymnt_d, "%b-%d")

################# Deriving Issue Date column - Issue_dyr (Year) Issue_dm (Month) #################
issue_dyr <- data.frame(issue_dyr = year(loan$issue_d))
loan <- cbind(loan, issue_dyr)
issue_dmt <- data.frame(issue_dmt = month(loan$issue_d))
loan <- cbind(loan, issue_dmt)

loan$issue_dyr <- as.factor(loan$issue_dyr)
loan$issue_dmt <- as.factor(loan$issue_dmt)

str(loan)
summary(loan)

################# Considering only the defaulted and full paid loans #################
loan<-subset(loan,loan$loan_status!='Current')


################# Formatting Income #################
loan$int_rate<-as.factor(round(as.numeric(sub('%',"",loan$int_rate)),digits = 0))
loan$income_bin<-round(loan$annual_inc/10000,digits = 0)*10000
loan$income_bin<-as.factor(loan$income_bin)
loan$loan_amnt_bin<-round(loan$loan_amnt/1000,digits = 0)*1000
loan$dti_bin<-round(loan$dti,digits = 0)
#loan$loan_amnt_bin<-as.factor(loan$loan_amnt)


################# Checking and removing outliers of Annual income column #################
boxplot(loan$annual_inc)
quantile(loan$annual_inc,c(1:100)/100)
loan<-subset(loan,loan$annual_inc<=89000)
boxplot(loan$annual_inc)

################# Box Plot - Annual income vs Loan Status #################
ggplot(loan, aes(loan$loan_status, loan$annual_inc)) + geom_boxplot() + ylim(0,150000) +
  labs(title = "Annual income vs Loan Status",
       x = "Loan Status",
       y = "Annual income")

################# Histogram - Verification vs Status #################
ggplot(loan, aes(x=factor(loan$verification_status), fill = loan$loan_status)) +
  geom_bar(position = "dodge") + 
  labs(title = "Verification vs Status",
       x = "Verification Status",
       y = "Count")


################# Histogram - Grade vs Status #################
ggplot(loan, aes(x=factor(loan$grade), fill = loan$loan_status)) + 
  geom_bar(position = "dodge") +
  labs(title = "Grade vs Status",
       x = "Grade",
       y = "Count")

#Histogram - Annual Income vs Status
ggplot(loan, aes(x=loan$annual_inc, fill = factor(loan$loan_status))) +  
  geom_histogram(bandwidth = 10000) +
  labs(title = "Annual Income vs Status",
       x = "Annual Income",
       y = "Status")


################# Histogram - dti vs Status #################
ggplot(loan, aes(x=loan$dti, fill = factor(loan$loan_status))) +  
  geom_histogram(bandwidth = 30) +
  labs(title = "DTI vs Status",
       x = "DTI",
       y = "Status")


################# Jitter Plot - "Annual Income vs Status" #################
ggplot(loan, aes(x=factor(loan$loan_status), y= loan$annual_inc)) + 
  geom_jitter(position = position_jitter(width = 0.5)) +
  labs(title = "Annual Income vs Status",
       x = "Annual Income",
       y = "Status")

################# Univariate analysis #################

################# Annual Income
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(income_bin)) + 
  geom_bar(fill='red') + ylim(0,6000)+ ggtitle('UA of Annual Income (Loan Paid off)')

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(income_bin)) + 
  geom_bar(fill='blue') + ylim(0,6000) + ggtitle('UA of Annual Income (Loan Defaulted)')

################# Term
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(term))+geom_bar(fill='red')+ylim(0,25000)+ggtitle('UA of Term (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(term))+geom_bar(fill='blue')+ylim(0,25000)+ggtitle('UA of Term (Loan Defaulted)')

################# Interest Rate
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(int_rate))+geom_bar(fill='red')+ylim(0,4000)+ggtitle('UA of Interest Rate (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(int_rate))+geom_bar(fill='blue')+ylim(0,4000)+ggtitle('UA of Interest Rate (Loan Defaulted)')

################# Loan Amount
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(loan_amnt_bin))+geom_bar(fill='red')+ylim(0,3000)+ggtitle('UA of Loan Amount (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(loan_amnt_bin))+geom_bar(fill='blue')+ylim(0,3000)+ggtitle('UA of Loan Amount (Loan Paid Defaulted)')

################# Grade
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(grade))+geom_bar(fill='red')+ylim(0,9000)+ggtitle('UA of Grade (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(grade))+geom_bar(fill='blue')+ylim(0,9000)+ggtitle('UA of Grade (Loan Defaulted)')

################# Emp_length
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(emp_length))+geom_bar(fill='red')+ylim(0,5000)+ggtitle('UA of Employee Length (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(emp_length))+geom_bar(fill='Blue')+ylim(0,5000)+ggtitle('UA of Employee Length (Loan Defaulted)')

################# Home Ownership
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(home_ownership))+geom_bar(fill='red')+ylim(0,15000)+ggtitle('UA of Home Ownership (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(home_ownership))+geom_bar(fill='blue')+ylim(0,15000)+ggtitle('UA of Home Ownership (Loan Defaulted)')

################# purpose
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(purpose))+geom_bar(fill='red')+ylim(0,12500)+ggtitle('UA of Purpose (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(purpose))+geom_bar(fill='blue')+ylim(0,12500)+ggtitle('UA of Purpose (Loan Defaulted)')

################# Delinq_2yr
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(delinq_2yrs))+geom_bar(fill='red')+ylim(0,25000)+ggtitle('UA of Delinq_2yrs (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(delinq_2yrs))+geom_bar(fill='Blue')+ylim(0,25000)+ggtitle('UA of Delinq_2yrs (Loan Defaulted)')

################# Public Record
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(pub_rec))+geom_bar(fill='red')+ylim(0,25000)+ggtitle('UA of Public Record (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(pub_rec))+geom_bar(fill='Blue')+ylim(0,25000)+ggtitle('UA of Public Record (Loan Defaulted)')

################# Public Record Bankrupcy
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(pub_rec_bankruptcies))+geom_bar(fill='red')+ylim(0,25000)+ggtitle('UA of Public Record (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(pub_rec_bankruptcies))+geom_bar(fill='Blue')+ylim(0,25000)+ggtitle('UA of Public Record (Loan Defaulted)')

################# Verification Status
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(verification_status))+geom_bar(fill='red')+ylim(0,13000)+ggtitle('UA of Verification Status (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(verification_status))+geom_bar(fill='Blue')+ylim(0,13000)+ggtitle('UA of Verification Status (Loan Defaulted)')

################# DTI
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(dti_bin))+geom_bar(fill='red')+ylim(0,1500)+ggtitle('UA of DTI (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(dti_bin))+geom_bar(fill='blue')+ylim(0,1500)+ggtitle('UA of DTI (Loan Defaulted)')

################# Open_acc
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(open_acc))+geom_bar(fill='red')+ylim(0,3000)+ggtitle('UA of Open_acc (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(open_acc))+geom_bar(fill='blue')+ylim(0,3000)+ggtitle('UA of Open_acc (Loan Defaulted)')

################# Total_acc
ggplot(subset(loan,loan$loan_status!="Charged Off"),aes(total_acc))+geom_bar(fill='red')+ylim(0,1500)+ggtitle('UA of Total_acc (Loan Paid off)')
ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(total_acc))+geom_bar(fill='blue')+ylim(0,1500)+ggtitle('UA of Total_acc (Loan Defaulted)')



#################  Bivariate Analysis #################

################# Loan Issued per Month #################
ggplot(loan, aes(loan$issue_dmt, fill = factor(loan$loan_status))) + 
  geom_histogram(stat="count", bandwidth = 10) + 
  labs(title = "Issue DMT and frequency",
       x = "Issued per Month",
       y = "Count")

################# Grade Distribution among the data set ################# 
Desc(loan$grade, main="Grade Distribution", plotit = 1)

################# Loan Term distribution among the data set ################# 
Desc(loan$term, main="Loan Term Distribution", plotit = 1)

################# Loan Amount distribution among the data set ################# 
Desc(loan$loan_amnt, main="Loan Amount Distribution", plotit = 1)

################# Loan Amount By Term among the data set ################# 
ggplot(loan, aes(loan$term,loan$loan_amnt)) + geom_boxplot(aes(fill = loan$term)) +
  labs(title = "Loan amount by term",
       x = "Term",
       y = "Loan amount")

################# Interest Rate by Grade among the data set #################
ggplot(loan, aes(loan$grade,loan$int_rate)) + geom_boxplot(aes(fill = grade)) +
  labs(title = "Interest rate by grade",
       x = "Grade",
       y = "Interest rate")

################# Lending amount per month among the data set #################

lending_amt = loan %>% 
  select(issue_dmt, loan_amnt) %>%
  group_by(issue_dmt) %>%
  summarise(Amount = sum(loan_amnt))

summary(lending_amt)
lending_amt

ggplot(lending_amt, aes(x = issue_dmt, y = Amount)) + geom_point() + 
  labs(title = "Loan amount issued by month",
       x = "Issued Month",
       y= "Total Amount")

################# Loan Amount status distribution among the data set #################
Desc(loan$loan_status, main = "Loan amount status distribution", plotit = 1)

################# Loan Status by Grade among the data set #################
ggplot(loan, aes(loan$grade, fill = factor(loan$loan_status))) + geom_bar(position = "fill") + 
  labs(title = "Loan status by grade",
       x = "Grade",
       y = "Rate")

################# Stacked Representation #################
ggplot(loan, aes(loan$grade, fill = factor(loan$loan_status))) + geom_bar(position = "stack") + 
  labs(title = "Loan status by grade",
       x = "Grade",
       y = "Rate")


################# Loan disbursement growth rate #################
amt_group_table = loan %>% 
  select(issue_dmt, loan_status, loan_amnt) %>% 
  group_by(issue_dmt, loan_status) %>% 
  summarise(Amount = sum(loan_amnt))

summary(amt_group_table)

ggplot(amt_group_table, aes(x = issue_dmt, y = Amount, col = factor(loan_status))) + geom_point() + 
  labs(title = "Loan amount distribution among loan statuses",
       x = "Issued date",
       y = "Amount")

################# Annual Income distribution of borrowers #################
Desc(loan$annual_inc, main = "Annual Income distribution", plotit = 1)

################# Annual Income by Grade#################
ggplot(loan, aes(grade,annual_inc)) + geom_boxplot() + ylim(0,100000) +
  labs(title = "Annual Income by Grade",
       x = "Grade",
       y = "Annual income")

################# Purpose distribution of the Loan Amount #################
Desc(loan$purpose, main = "Purpose Distribution of Loan Amount", plotit = 1)

################# Fully paid vs Charged Off loans excluding "on going" ################# 
loan2 <- subset(loan, loan$loan_status == "Fully Paid" | loan$loan_status == "Charged Off")
table(loan2$loan_status)
loan2$is_good = factor(ifelse(loan2$loan_status == "Fully Paid",1,0))
str(loan2$is_good)

################# Segmented Bivariate Analysis - Loan disbursement Vs Loan Status and Grade #################
Loand_group_table = loan %>% 
  select(loan_status, loan_amnt, grade) %>% 
  group_by(loan_status, grade) %>% 
  summarise(Amount = sum(loan_amnt))

summary(Loand_group_table)

ggplot(Loand_group_table, aes(x = grade,y = Amount, fill = loan_status)) + 
  geom_bar(stat="identity",position = "dodge") + geom_text(aes(label = Amount), position= position_dodge(width=0.9), vjust=-.5, color="black") +
  theme(legend.position = "bottom") +
  labs(title = "Loan amount distribution Vs Loan Status per Grades",
       x = "Grades",
       y = "Amount")

################# Segmented Bivariate Analysis - Loan disbursement Vs Verification Status and Loan Status #################
Loand_group_table = loan %>% 
  select(loan_status, loan_amnt, verification_status) %>% 
  group_by(loan_status, verification_status) %>% 
  summarise(Amount = sum(loan_amnt))

summary(Loand_group_table)

ggplot(Loand_group_table, aes(x = verification_status,y = Amount, fill = loan_status)) + 
  geom_bar(stat="identity",position = "dodge") + geom_text(aes(label = Amount), position= position_dodge(width=0.9), vjust=-.5, color="black") +
  theme(legend.position = "bottom") +
  labs(title = "Loan amount distribution Vs Loan Status per Grades",
       x = "Grades",
       y = "Amount")

################# Writing file for Tableau representation #################
write.csv(loan2,file = "analysed_loan_data.csv", na = "")

###################### End of R Code  #######################
#############################################################