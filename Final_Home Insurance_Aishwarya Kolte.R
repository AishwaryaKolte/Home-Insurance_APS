########### This is a complete R notebook, right from data wrangling to data visualization #########

setwd('C:/Users/DELL/Downloads/APS')
getwd()
library(ggplot2)
library(dplyr)
library(lubridate)
library(skimr)
home_df=read.csv("home_insurance.csv")
View(home_df)

# Identity the types of columns in the dataset and understand the data.
colnames(home_df)
str(home_df)
nrow(home_df)

#There are a lot of blank values in the data, lets convert them to NA.
home_df[home_df==""]<-NA

#let's check 1st 6 rows of the data
head(home_df)


################################ Data Wrangling ####################################

#Finding missing values percentage in each column
missing_values=data.frame(colSums(is.na(home_df)))%>%rename("nulls"="colSums.is.na.home_df..")
missing_values=missing_values%>%mutate(percentage=round(nulls/nrow(home_df)*100,2))
missing_values_final <- (subset(missing_values,percentage > 0)%>%arrange(by=percentage))
missing_values_final
rownames(missing_values_final)

#MTA_FAP ,MTA_APRP,CAMPAIGN_DESC, PAYMENT_FREQUENCY, CLERICAL, P1_PT_EMP_STATUS has more than 50 % values missing.
#RISK_RATED_AREA_C&B has 45% and 29% missing values
#QUOTE_DATE has 49.5% missing and MTA_DATE has 89.62% missing values.
#others in missing_values_final has 26.20 and LAST_ANN_PREM_GROSS has 25.81% missing values.


#Changing the format of the Quote_Date to d/m/y
home_df <- home_df %>%
  mutate(QUOTE_DATE = as.Date(QUOTE_DATE, format = "%m/%d/%Y")) %>%
  mutate(QUOTE_DATE = format(QUOTE_DATE, "%d/%m/%Y"))



home_df=home_df%>%select(-c(CLERICAL,P1_PT_EMP_STATUS,CAMPAIGN_DESC,MTA_DATE))

unique(home_df$PAYMENT_FREQUENCY)
home_df$PAYMENT_FREQUENCY=replace(home_df$PAYMENT_FREQUENCY,is.na(home_df$PAYMENT_FREQUENCY),0)
list_0_impute <- c("MTA_FAP","MTA_APRP","PAYMENT_FREQUENCY")

# home_df$MTA_FLAG Because mt is no we can say the values for mta_fap and mta_aprp is 0
for (col in list_0_impute){
  if (any(is.na(home_df[col]))){
    home_df[[col]][is.na(home_df[[col]])] <- 0}}
 
# Checking for Duplicate Values
sum(duplicated(home_df))
# There are no duplicated values.

#To impute missing date values in R, you can utilize the na.locf() 
#function from the zoo package, which stands for "last observation carried forward."
library(zoo)

home_df_og=home_df #To save the current format to home_df_og

home_df$QUOTE_DATE<-na.locf(home_df$QUOTE_DATE)

home_df <-home_df%>%select(-COVER_START)

colnames(home_df)

#Imputing RISK_RATED_AREA_B

# RISK_RATED_AREA_B has 45% missing value we will treat this and all other missing values are
# 29% which is not significat so we can drop them so as to not introduce
#anymore bias into the data

hist(home_df$RISK_RATED_AREA_B,col = "red",breaks = 80)
boxplot(home_df$RISK_RATED_AREA_B)
#By looking at the histogram and boxplot we can see its normally distributed with some outliers 
#Impute missing values in 'RISK_RATED_AREA_B' with the median, excluding 10% of extreme values.
home_df$RISK_RATED_AREA_B[is.na(home_df$RISK_RATED_AREA_B)] <-median(home_df$RISK_RATED_AREA_B,na.rm = T,trim=1)


#We have handled most of missing values only 26% are now missing so we are storing them and 
#dropping them and doing the analysis on the rest of the cleaned data

rows_with_na <- home_df %>%
  filter_all(any_vars(is.na(.)))   #Storing NA's
colSums(is.na(rows_with_na))  
View(rows_with_na)

rows_without_na <-home_df %>%
  filter_all(all_vars(!is.na(.)))   #Dropping NA's
colSums(is.na(rows_without_na))

cleaned_df <- rows_without_na

df=cleaned_df


########################################################################################
#########################################################################################
################################ EDA ####################################################


#########Categorical Data Handling and Analysis

selected_data <- df %>%
  select_if(is.character)              
premiums_=df%>%select(c(Police,LAST_ANN_PREM_GROSS))
cat_data=inner_join(selected_data,premiums_,by="Police")
col_names=names(cat_data) 
col_names=col_names[-c(1,9,39,38)]
col_names=list(col_names)   #list of categories excluding date,policy,premium

looper=unlist(col_names)  
#Beacause i faced a problem when i run my
#code without unlisting but the same had worked with not unlisting 
#also so to avoid any kind of error we will be using unlisted

looper    #This is a feature that will be used to loop over all the categorical features

#For loop showing average_premium and percenatage of values present

grouped_list <- list()

for (col in looper) {
  # Group the data and calculate the mean
  grouped_data <- cat_data %>%
    group_by_at(vars(col)) %>%
    summarise(avg_premium = mean(LAST_ANN_PREM_GROSS), counts = n(), .groups = "drop") %>%
    mutate(percentage = counts / nrow(cat_data) * 100) %>%
    select(col, avg_premium, percentage,counts)
  
  # Add the data to the list
  grouped_list[[col]] <-grouped_data
}
#This list contain the data derived from above loop were all categorical columns element 
#have been compared of how much average premium are they generating and what are their 
#percentage 
grouped_list


#skimr function useful to understand the data
skim_to_wide(df)

#Quote date is of no use so we extract the month from the quote to understand any relations
df$quote_month <-month(df$QUOTE_DATE)   #Quote_month                         


df$P1_DOB <-dmy(df$P1_DOB)
df$age <- round(difftime(today(),df$P1_DOB,units="days")/365)  #Age column


age <-df%>%group_by(age)%>%summarise(avg_prem = mean(LAST_ANN_PREM_GROSS),
                                     cnt_prct =round(n()/nrow(df),3),
                                     cnt=n())%>%arrange(-cnt)
tail(age)


df$age <- as.numeric(df$age)


df <- df[-c(1,18,61)] # Removing quote date dob and policy number 
View(df)


########################################################################
##############################Correlation analysis#####################

#One hot encoding binary categories and using factors for catgeories with many features
#Combine all of these with existing numerical features and find correlation with Premium
str(df)

#binary categories
binary_vars <-df%>%
  select_if(function(x) is.logical(x) | all(x %in% c("Y","N")))%>%
  mutate(across(everything(),~ifelse(.=="Y",1,0)))

#note fuction(x) used inside select_if is a anonymous function similar to lambda function you see in Python 


#categorical not binary
categorical_vars <- df %>%
  select_if(function(x) is.character(x) && n_distinct(x) > 2) 

str(categorical_vars)

for (col in names(categorical_vars)){
  categorical_vars[[col]] <-as.numeric(as.factor(categorical_vars[[col]]))}
str(categorical_vars)

str(df)
numeric_vars <-df%>%
  select_if(function(x) is.numeric(x))
View(numeric_vars)
combined_df <- cbind(categorical_vars,binary_vars,numeric_vars)
colnames(combined_df)
str(combined_df)

correlation_matrix <- cor(combined_df)
correalation_df <-data.frame(correlation_matrix)
correalation_df%>%select(LAST_ANN_PREM_GROSS)

grouped_list$CONTENTS_COVER

###CORRELATION OF ONLY THE NUMERIACAL FEATURES
correl_matrix_numeric <- cor(numeric_vars)
corr_df_numerical <- data.frame(correl_matrix_numeric)
corr_df_numerical%>%select(LAST_ANN_PREM_GROSS)



###############################################################

#We have 17 unique values for year build
#We will find the average premium paid by Yearnbilt
length(unique(combined_df$YEARBUILT))

combined_df%>%select(YEARBUILT,LAST_ANN_PREM_GROSS)%>%
  group_by(YEARBUILT)%>%summarise(avg_premium_paid =mean(LAST_ANN_PREM_GROSS),
                                  count=n())%>% arrange(-avg_premium_paid)


#############################################################
################################ FINDINGS ###################


# MTA_FAP LAST_ANNUAL_PREM SAFE_INSTALLED shows high correlataion these must influence the premium amount paid significatly
#DIFF :#MTA _FAP & APRP .4 and .2 correlation
#YEAR BUILT,PROP_TYPE,OWNERSHIP_TYPE, are showing negative of .24,.26, and .28
#SUM_INSURED_BUILDINGS  0.59  and NCD_GRANTED_YEARS_B 0.46
#LEGAL add on Pre and Post renewal is .21 and .18
#SAFE Installed is only .1 here and SPEC_ITEM_PREM is .23

# Analysis of Premiums Based on Policy Status
# Highest average premium was paid by 'Cancelled' policies
# Second highest is 'Lapsed,' representing 27% of the data; 'Live' policies constitute 70% of the data
# Used ANOVA to determine if there are significant differences in premiums among policy statuses
# ANOVA result: No significant differences observed (DIFF: Nothing)


#For the policies that opted for MTA_FLAG are paying a higher premium but is only 29% of data
#T-test result: Null hypothesis accepted, indicating no significant difference in premiums

#Key care add_on pays higher but is only 5.25% of the population
# T-test result: Null hypothesis accepted, indicating no significant difference in premiums

#Garden_add_on pays higher but is only 7.28% 
#T-test result: Null hypothesis accpeted

#Legal_add on opted clients pays a higher premium
#T-test result: null rejected : There is significant difference in the premium paid

#Payment method variables 
#anova result : No significant differences

#Almost 100 percentage of clients are of PH occupancy status

#Bus_use pays higher premium but is only 1.5% of population
#T-test result :Null rejected: There is significant difference in the amout paid
#Bus_use pays higher premium

#Not flood proof property has a higher average premium
#T-test result :null accepted: No significant differences

#Houses with installed Alarms pay higher average premium logically 
#it should be the opposite it maybe beacause 92% of policies doen't have alarms
#Employee status Anova should be conducted before any conclusions

#Clients who made claims in the last 3 years have to pay higher premium


#Anova Tests

#Anova amongst Employee status
names(df)
employee_status=df%>%select(P1_EMP_STATUS,LAST_ANN_PREM_GROSS)

emp_values=unique(df$P1_EMP_STATUS)
employee_status_df =list()

set.seed(120)
for (i in emp_values){
  theresult <-employee_status%>%filter(P1_EMP_STATUS==i)%>%select(LAST_ANN_PREM_GROSS)%>%
    sample_n(size = 25,replace = T)
  vectors <-unlist(theresult)
  
  employee_status_df[[i]] <-theresult}

names(employee_status_df)

employee_aov_df <-data.frame(employee_status_df) #We have got the right format with no names
employee_stack <-stack(employee_aov_df)
names(employee_stack)
summary(aov(values~ind,data=employee_stack))
# P-value is less than 0.05 and F-value greater than the critical value
# We reject the null hypothesis and conclude there is a significant difference
# in the premium paid by clients with different employee statuses
# Result: No significant difference observed (DIFF: Same result)

#Anova amongst payment methods and their premium paid
pay_methods <- df%>%select(PAYMENT_METHOD,LAST_ANN_PREM_GROSS)
pay_values=unique(df$PAYMENT_METHOD)
pay_status =list()

set.seed(120)
for (i in pay_values){
  theresult <-pay_methods%>%filter(PAYMENT_METHOD==i)%>%select(LAST_ANN_PREM_GROSS)%>%
    sample_n(size = 30,replace = F)
  vectors <-unlist(theresult)
  
  pay_status[[i]] <-theresult}

names(pay_status)

pay_aov_df <-data.frame(pay_status) #We have got the right format with no names
pay_stack <-stack(pay_aov_df)
names(pay_stack)
summary(aov(values~ind,data=pay_stack))
# P-value and F critical both suggest accepting the null hypothesis
# Implying no significant difference in premiums
# Upon increasing the sample size, P-value decreases to 0.17
# Conclusion: With the larger sample, the P-value is still above the significance level,
# indicating that there is no significant difference in premiums even with the increased sample size

#Anova amongst different policy status
names(df)
pol_methods <- df%>%select(POL_STATUS,LAST_ANN_PREM_GROSS)
pol_values=unique(df$POL_STATUS)
pol_status =list()
set.seed(120)
for (i in pol_values){
  theresult <-pol_methods%>%filter(POL_STATUS==i)%>%select(LAST_ANN_PREM_GROSS)%>%
    sample_n(size = 25,replace = T)
  vectors <-unlist(theresult)
  
  pol_status[[i]] <-theresult}

names(pol_status)

pol_aov_df <-data.frame(pol_status) #We have got the right format with no names
pol_stack <-stack(pol_aov_df)
names(pol_stack)
summary(aov(values~ind,data=pol_stack))
#P value greater than 0.05
#concluding that there is no significant difference in means of these policy  status


#For policy status and payment method
#There is no significant difference in means of the categories of these columns
#So these are not a strong indicator eventhough the data tells a different story because its unbalanced
#It is no statistically significant 
#These has only a small_influence


#On the other hand, employee_status has a significant difference in the permium paid

#p_VAL less THAN SIGNIFICANCE SO WE reject the NULL
#There is signifincat difference in premium paid by differenct occupational status.


grouped_list$P1_MAR_STATUS

#Anova amongst different mar status
names(df)
mar_methods <- df%>%select(P1_MAR_STATUS,LAST_ANN_PREM_GROSS)
mar_values=unique(df$P1_MAR_STATUS)
mar_status =list()
set.seed(120)
for (i in mar_values){
  theresult <-mar_methods%>%filter(P1_MAR_STATUS==i)%>%select(LAST_ANN_PREM_GROSS)%>%
    sample_n(size = 26,replace = T)
  vectors <-unlist(theresult)
  
  mar_status[[i]] <-theresult
}

names(mar_status)

mar_aov_df <-data.frame(mar_status) #We have got the right format with no names
mar_stack <-stack(mar_aov_df)
names(mar_stack)
summary(aov(values~ind,data=mar_stack))



grouped_list$POL_STATUS        #    -0.05%

grouped_list$P1_EMP_STATUS     #    -0.05%

grouped_list$PAYMENT_METHOD    #    +0.01%

grouped_list$BUS_USE           #    +0.04

grouped_list$SAFE_INSTALLED    #    +0.31%

#T-test  

#Safe installed t test

set.seed(120)
safe_df <-df%>%select(SAFE_INSTALLED,LAST_ANN_PREM_GROSS)
safe_df_names <-unique(df$SAFE_INSTALLED)
safe_list <- list()
for (col in safe_df_names){
  safe_result <-safe_df%>%filter(SAFE_INSTALLED==col&LAST_ANN_PREM_GROSS>0)%>%
    select(LAST_ANN_PREM_GROSS)%>%sample_n(size=35,replace=FALSE)
  safe_list[[col]]<-safe_result}
names(safe_list)
t_safe_df <- data.frame(safe_list)
names(t_safe_df)
t.test(t_safe_df$LAST_ANN_PREM_GROSS,t_safe_df$LAST_ANN_PREM_GROSS.1,mu=0)
#p val is less than significance level, so we reject the null hypothesis
#concluding that there is signifincant difference in the premium amout paid by
#safe installed and not installed

#the sample data showing houses with safety installed pays more premium which
#doesnt seem that logical so to check again for any patterns we will 
#find the mean of the premium amount which is greater than 0
#compare with the sample for houses with safe installed

the_val <-df%>%select(LAST_ANN_PREM_GROSS)%>%filter(LAST_ANN_PREM_GROSS >0)%>%summarise(mean=mean(LAST_ANN_PREM_GROSS))
t.test(safe_list$Y,mu=187.768,alternative = ("greater"))
#P val less than 0.05 
#So we conclude that houses with safe installed pays greater premium


#Bus Opt T-test

bus_df <-df%>%select(BUS_USE,LAST_ANN_PREM_GROSS)
bus_df_names <-unique(df$BUS_USE)
bus_list <- list()
set.seed(100)
for (col in bus_df_names){
  bus_result <-bus_df%>%filter(BUS_USE==col&LAST_ANN_PREM_GROSS>0)%>%
    select(LAST_ANN_PREM_GROSS)%>%sample_n(size=35,replace=FALSE)
  bus_list[[col]]<-bus_result}
t_bus_df <- data.frame(bus_list)
names(t_bus_df)
t.test(t_bus_df$LAST_ANN_PREM_GROSS,t_bus_df$LAST_ANN_PREM_GROSS.1,mu=0)

#P value less than significance value, so we reject the null.
#There is  significant diiference in premium paid by homes with bus route and without bus route
t.test(bus_list$N,mu=187.768,alternative = ("greater"))
#Houses which is yes for bus route pays a higher averahe premium



#Flood Poof T_test

grouped_list$FLOODING
# Given that 98% of houses are flood-proof, a T-test with a sample size of 29 is conducted
# Objective: Determine if there is a significant difference in the average premium paid by houses
# Comparing houses that are not flood-proof against those that are flood-proof
# Conclusion: If the T-test indicates a significant difference, it suggests that the average premium paid
# by houses not flood-proof is greater than that of flood-proof houses

flood_df <-df%>%select(FLOODING,LAST_ANN_PREM_GROSS)
flood_df_names <-unique(df$FLOODING)
flood_list <- list()
set.seed(120)
for (col in flood_df_names){
  flood_result <-flood_df%>%filter(FLOODING==col&LAST_ANN_PREM_GROSS>0)%>%
    select(LAST_ANN_PREM_GROSS)%>%sample_n(size=35,replace=FALSE)
  flood_list[[col]]<-flood_result}
names(flood_list)
t_flood_df <- data.frame(flood_list)
names(t_flood_df)
t.test(t_flood_df$LAST_ANN_PREM_GROSS,t_flood_df$LAST_ANN_PREM_GROSS.1,mu=0)
#P val less than 0.05
#so we  reject the null hypothesis
#conclude that there is significant difference
#in the insurance premium paid by flood proof  and not flood proof houses
t.test(flood_list$N,mu=187.768,alternative = ("greater"))
#The houses with no flood proof pays a higher premium



alarm_df <-df%>%select(APPR_ALARM,LAST_ANN_PREM_GROSS)
alarm_df_names <-unique(df$APPR_ALARM)
alarm_list <- list()
set.seed(120)
for (col in alarm_df_names){
  alarm_result <-alarm_df%>%filter(APPR_ALARM==col&LAST_ANN_PREM_GROSS>0)%>%
    select(LAST_ANN_PREM_GROSS)%>%sample_n(size=35,replace=FALSE)
  alarm_list[[col]]<-alarm_result}
t_alarm_df <- data.frame(alarm_list)
names(t_alarm_df)
t.test(t_alarm_df$LAST_ANN_PREM_GROSS,t_alarm_df$LAST_ANN_PREM_GROSS.1,mu=0)
# p val is less than significance level, so we reject the null hypothesis.
#concluding that there is significant difference in premiums for houses with appropriate alarms.
t.test(alarm_list$Y,mu=187.768,alternative = ("greater"))



#ChiSquare Test of Independence /Association Tests


# Function to Perform Chi-Square Test and Return Summary
chi_test <-function(df,x,y){
  thedata <-df%>%select({{x}},{{y}})
  tbles <-table(thedata)
  return(chisq.test(tbles))}

# Chi-Square Test for CLAIM3YEARS and POL_STATUS
thedata <-df%>%select(CLAIM3YEARS,POL_STATUS)
tble <-table(thedata)
chisq.test(tble)
chi_test(df,CLAIM3YEARS,POL_STATUS)
#null hypothesis : No Association 
#alterntive hypothesis : There is Association
#Pval is less than significance value, so we conclude that there is association
names(df)


chi_test(df,"FLOODING","SUBSIDENCE")


chi_test(df,"BUS_USE","SAFE_INSTALLED")
#P val less than 0.05 so reject the null and say that there is association

chi_test(df,"POL_STATUS","PAYMENT_METHOD")
#P val less than 0.05 so reject the null and say that there is association

chi_test(df,"POL_STATUS","SAFE_INSTALLED")
#P val less than 0.05 so reject the null and say that there is association

chi_test(df,"POL_STATUS","OCC_STATUS")
#P val is slightly greater than 0.05 so accept the null and say that there is association
#We conclude that there is slight independence between these variables.


########################################### EDA END ########################################
#############################################################################################
############################################################################################

#Merging Post and Pre-Renewal fields to create binary variables for Key Care, Legal, Garden, and Home Emergency Add-Ons, and binary indicators for House Property Add-Ons HP1, HP2, and HP3.

df$KEYCARE_ADDON <- if_else(df$KEYCARE_ADDON_POST_REN=="Y"|df$KEYCARE_ADDON_PRE_REN=="Y","Y","N")
df$LEGAL_ADDON <- if_else(df$LEGAL_ADDON_POST_REN=="Y"|df$LEGAL_ADDON_PRE_REN=="Y","Y","N")
df$GARDEN_ADDON <- if_else(df$GARDEN_ADDON_PRE_REN=="Y"|df$GARDEN_ADDON_POST_REN=="Y","Y","N")
df$HOME_EM_ADDON <- if_else(df$HOME_EM_ADDON_PRE_REN=="Y"|df$HOME_EM_ADDON_POST_REN=="Y","Y","N")


df$HP1_ADDON <-if_else(df$HP1_ADDON_POST_REN=="Y"|
                         df$HP1_ADDON_PRE_REN=="Y",1,0)
df$HP2_ADDON <-if_else(df$HP2_ADDON_POST_REN=="Y"|
                         df$HP2_ADDON_PRE_REN=="Y",1,0)
df$HP3_ADDON <-if_else(df$HP3_ADDON_POST_REN=="Y"|
                         df$HP3_ADDON_PRE_REN=="Y",1,0)

select(df, KEYCARE_ADDON, LEGAL_ADDON, GARDEN_ADDON, HOME_EM_ADDON, HP1_ADDON, HP2_ADDON, HP3_ADDON)


#Before Visualizing everything we will iterate over everything that is not needed 
#How are we deciding if the features are not needed
#By the Hypothesis tests we have done 
#We will only select feature that are significantly influencing the premium paid by its elements
#These are most Applicable on categorical data or discrete data 

grouped_list$OCC_STATUS

#Lastly under the policy status lapsed policies are policies where clients didn't complete their
#payment obligations<  we will create a column resiliated indicating a client has defaulted in their
#payments
df$resiliated <-ifelse(df$POL_STATUS=="Lapsed","Y","N")

#Resiliated contracts Premium tends to be higher
names(df)
df$SEC_DISC_REQ
df$PAYMENT_METHOD
df$OCC_STATUS


df%>%group_by(resiliated)%>%summarise(avg=median(LAST_ANN_PREM_GROSS,trim=1))
fraud_df <-df%>%select("P1_EMP_STATUS","P1_MAR_STATUS","age","LEGAL_ADDON","PROP_TYPE","P1_POLICY_REFUSED",
                       CLAIM3YEARS,MTA_FLAG,P1_SEX,"resiliated","SEC_DISC_REQ","PAYMENT_METHOD","OCC_STATUS")
names(df)
customer_features <- c("P1_EMP_STATUS","P1_MAR_STATUS","age","LEGAL_ADDON","PROP_TYPE","P1_POLICY_REFUSED",
                       "CLAIM3YEARS","MTA_FLAG","P1_SEX","SEC_DISC_REQ","PAYMENT_METHOD","OCC_STATUS")
numeric_variables <-c("NCD_GRANTED_YEARS_B","NCD_GRANTED_YEARS_C","SUM_INSURED_BUILDINGS","SUM_INSURED_CONTENTS","UNSPEC_HRP_PREM")

fraud_numeric_df <-df%>%select(numeric_variables,"resiliated")

fraud_numeric_df%>%group_by(resiliated)%>%summarise(std=var(NCD_GRANTED_YEARS_B))
fraud_numeric_df%>%group_by(resiliated)%>%summarise(std=mean(SUM_INSURED_BUILDINGS))

par(mfrow=c(3,2))  # Setting the plotting layout

for (feature in numeric_variables) {
  boxplot(fraud_numeric_df[[feature]] ~ fraud_numeric_df$resiliated,
          main = paste("Boxplot of", feature, "by resiliated"),
          xlab = "Resiliated"
          ,ylab="")}

par(mfrow = c(3, 2)) 


library(caret)
View(fraud_numeric_df)
pair_kdr <- featurePlot(x = fraud_numeric_df[,1:5],
                        y = factor(fraud_numeric_df$resiliated),
                        plot = "density",
                        strip = strip.custom(par.strip.text = list(cex = 0.7)),
                        scales = list(x = list(relation = "free"),
                                      y = list(relation = "free")))
library(patchwork)
fraud_list <- list()

for (name in customer_features) {
  itergroup <- fraud_df %>% filter(resiliated == "Y") %>% 
    group_by(resiliated, across(all_of(name))) %>%  
    summarise(cnt = n(), percentage = n() / nrow(fraud_df) * 100) %>% 
    mutate(perct = cnt / sum(cnt)) %>%
    arrange(-percentage)
  
  fraud_list[[name]] <- itergroup}

plot_list <- list()

for (name in names(fraud_list)) {
  plot <- ggplot(data = plot_list[[name]], aes(x = !!sym(name), y = percentage)) +
    geom_bar(stat = "identity", fill = "lightgreen", alpha = 0.8) +
    labs(title = paste("Distribution of", name, "among defaulted contracts"),
         x = name, y = "Percentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
  plot_list[[name]] <- plot}

# Arrange plots in a grid layout
grid_plots <- wrap_plots(plotlist = plot_list, ncol = 2)  # Change ncol according to desired layout
grid_plots

fraud_list

fraud_chi_tests =list()
for (name in customer_features){
  test=chi_test(fraud_df,"resiliated",name)
  fraud_chi_tests[[name]] <-test}
fraud_chi_tests 

#Chi_square test indicates all of these features have some kind of association.
fraud_list

#Insights from resiliated policies
#Gender     :  40% female and 59% male
#MTA_FLAG   : 70% is Yes and 30% is No
#CLAIM3YEARS :89% is No AND 11% is YES
#Police emission : 99% is No and 0.01 % is YES
#PROP_TYPE        :10 is 28% 2 is 21% 1 is 16% these are probably random factors which are insignificant
#LEGAL_ADDON     :63%  is  YES AND 36% is NO
#Age     :These are random factors which are insignificant for predicting defaulting
#Marital status  :M is 44% O is 17% w is 14% and P is 11% and S is 05%
#EMP_STATUS : R is 76%  E is 18% and S is 2% others are less than 1%
#OCC_STATUS :Not significant because 99% is PH
#Payment_method :62% is NonDD and 33% is pure DD and there is only  4% default from DD-other payments
#SEC_DISC_REQ : 79% IS YES


#######################################
selected_columns <- c(
  "APPR_LOCKS",
  "SAFE_INSTALLED",
  "YEARBUILT",
  "SUM_INSURED_BUILDINGS",
  "SUM_INSURED_CONTENTS",
  "NCD_GRANTED_YEARS_B",
  "SPEC_SUM_INSURED",
  "SPEC_ITEM_PREM",
  "UNSPEC_HRP_PREM",
  "BEDROOMS",
  "PAYMENT_FREQUENCY",
  "MTA_FAP",
  "MTA_APRP",
  "CLAIM3YEARS",
  "age",
  "LEGAL_ADDON",
  "HOME_EM_ADDON",
  "KEYCARE_ADDON",
  "GARDEN_ADDON",
  "HP1_ADDON",
  "HP2_ADDON",
  "HP3_ADDON",
  "CONTENTS_COVER",
  "P1_EMP_STATUS",
  "P1_MAR_STATUS",
  "P1_SEX","FLOODING","BUS_USE","LAST_ANN_PREM_GROSS","resiliated"
)
subsetted_df <- df%>%select(selected_columns)

subsetted_df%>%filter(HP3_ADDON==1&HP2_ADDON==1)


#write.csv(subsetted_df,file = "C:/Users/DELL/Downloads/APS/subsetted_df.csv",row.names = F)
#subsetted_df <-read.csv("C:/Users/DELL/Downloads/APS.csv")


str(subsetted_df)

numerical_features_vizz <-subsetted_df%>%
  select_if(function(x) is.numeric(x) && n_distinct(x) > 2)
str(numerical_features_vizz)

# Set up the plotting grid
par(mfrow = c(3, 2))

# Loop through each numerical feature and create a histogram
for (feature in names(numerical_features_vizz)) {
  hist(numerical_features_vizz[[feature]], col = heat.colors(8), main = feature)}


# Set up the plotting grid
par(mfrow = c(3, 2))

# Loop through each numerical feature and create boxplot
for (feature in names(numerical_features_vizz)) {
  boxplot(numerical_features_vizz[[feature]],main=feature)}

scatter_names <-names(numerical_features_vizz)[-11]


# Loop through each feature and create a scatter plot with a smooth line

cor(numerical_features_vizz)

#The following subplots for scatter plot code takes the most amount of time you can comment this out
#if you are running the whole script together

#Reshaping to long format
library(tidyr)
common=numerical_features_vizz$LAST_ANN_PREM_GROSS
df_long <- gather(numerical_features_vizz,key="feature",value = "value",-LAST_ANN_PREM_GROSS)
ggplot(data=df_long,aes(x=LAST_ANN_PREM_GROSS,y=value))+
  geom_point()+
  facet_wrap(~feature,scales="free_y",ncol=3)


categorical_features_vizz <- subsetted_df%>%
  select_if(function(x) is.character(x) && n_distinct(x)>2)

binary_vars_vizz <-subsetted_df%>%
  select_if(function(x) is.logical(x) | all(x %in% c("Y","N"))| all(x %in% c(1,0)))
dim(categorical_features_vizz)
dim(binary_vars_vizz)

all_categories_vizz <-cbind(categorical_features_vizz,binary_vars_vizz,subsetted_df$LAST_ANN_PREM_GROSS)%>%
  rename("LAST_ANN_PREM_GROSS"="subsetted_df$LAST_ANN_PREM_GROSS")


#We will plot bar graph showing PREMIUM PAID for all the categories
#For showing all the plots in one grid we will be ploting categories with binary features seperately
non_binary <- cbind(categorical_features_vizz,subsetted_df$LAST_ANN_PREM_GROSS)%>%
  rename("LAST_ANN_PREM_GROSS"="subsetted_df$LAST_ANN_PREM_GROSS")


#EMPLOYEE_STATUS
emp_Status <- all_categories_vizz%>%group_by(P1_EMP_STATUS)%>%summarise(
  avg=mean(LAST_ANN_PREM_GROSS))
emp_Plot <- ggplot(data=emp_Status,aes(x=reorder(P1_EMP_STATUS,-avg),y=avg))+
  geom_bar(stat="identity",aes(fill=P1_EMP_STATUS))+
  geom_text(aes(label=round(avg),vjust=-0.1))+
  theme(axis.text=element_blank())+
  labs(title="Barplot of Employeement status against ANNUAL PREMIUM PAID",x="Employment Status",y="AVerage premium paid")+
  theme(legend.position = "top")
emp_Plot

#INSIGHTS: 
#C PAYS THE HIGHEST PREMIUM FOLLOWED BY S THEN v THEN E THEN F THEN H THEN I THEN N THEN A THEN U AND LASTLY R
#CORPORATE -> STUDENT ->VIP ->EMPLOYEE ->FARMER ->HOUSEWIFE ->I ->N ->A -> U -> RETIRED



#MAR_STATUS
#According to the anova with a sample cof 26 there isn't much of a difference in the premium paid

ggplot(data=non_binary,aes(x=P1_MAR_STATUS,y=LAST_ANN_PREM_GROSS))+
  geom_boxplot()
mar_df <- non_binary%>%group_by(P1_MAR_STATUS)%>%summarise(avg=mean(LAST_ANN_PREM_GROSS))
ggplot(data=mar_df,aes(x=reorder(P1_MAR_STATUS,-avg),y=avg))+geom_bar(stat="identity",aes(fill=P1_MAR_STATUS))+
  theme(axis.text.x = element_blank())

names(non_binary)

#Marital  status doesn't contribute to premium paid            


#BINARY CAETGORIES
#"APPR_LOCKS"          "SAFE_INSTALLED"      "PAYMENT_FREQUENCY"   "CLAIM3YEARS"        
#"LEGAL_ADDON"         "HOME_EM_ADDON"       "KEYCARE_ADDON"       "GARDEN_ADDON"       
# "HP1_ADDON"           "HP2_ADDON"           "HP3_ADDON"           "CONTENTS_COVER"     
# "FLOODING"            "BUS_USE"             "LAST_ANN_PREM_GROSS"

binary_ <- cbind(binary_vars_vizz,subsetted_df$LAST_ANN_PREM_GROSS)%>%
  rename("LAST_ANN_PREM_GROSS"="subsetted_df$LAST_ANN_PREM_GROSS")
names(binary_vars_vizz)


#We will differentiate rest of  categorical into binary features 
#features of house and customer and also add up the extra added house property to create a new feature 
#of no of house properties added

binary_customer <-c("PAYMENT_FREQUENCY","CLAIM3YEARS","LEGAL_ADDON","CONTENTS_COVER","HOME_EM_ADDON","KEYCARE_ADDON","GARDEN_ADDON")
binary_hosue    <-c("APPR_LOCKS","SAFE_INSTALLED","FLOODING","BUS_USE","HP1_ADDON","HP2_ADDON","HP3_ADDON") 

unique(binary_$PAYMENT_FREQUENCY)
#There is only two column 1 and 0 . 0 is imputed values
#We will not be considering that and remove from the visualization process and variables.


#Claim last 3 years
ggplot(data=binary_,aes(x=CLAIM3YEARS,y=LAST_ANN_PREM_GROSS))+
  geom_boxplot()
c3 <- binary_%>%group_by(CLAIM3YEARS)%>%summarise(
  avg=median(LAST_ANN_PREM_GROSS))

ggplot(data=c3,aes(x=CLAIM3YEARS,y=avg))+
  geom_bar(stat="identity",aes(fill=CLAIM3YEARS))+
  labs(title="Premium difference of customers who claimed during last 3 years")+
  theme(legend.position="top")
#INSIGHTS : The average premium paid by clients who had initiated claims in past 3 years pays a higher premium

# LEGAL_ADDON
ggplot(data=binary_,aes(x=LEGAL_ADDON,y=LAST_ANN_PREM_GROSS))+
  geom_boxplot()
c4 <- binary_%>%group_by(LEGAL_ADDON)%>%summarise(
  avg=median(LAST_ANN_PREM_GROSS))

ggplot(data=c4,aes(x=LEGAL_ADDON,y=avg))+
  geom_bar(stat="identity",aes(fill=LEGAL_ADDON))+
  labs(title="Premium difference of clients who opted legal_addon ")+
  theme(legend.position="top")
#INSIGHTS :The average premium paid by clients who opted for legal add on pays a higher premium

# Contents cover
names(binary_)
ggplot(data=binary_,aes(x=CONTENTS_COVER,y=LAST_ANN_PREM_GROSS))+
  geom_boxplot()
c5 <- binary_%>%group_by(CONTENTS_COVER)%>%summarise(
  avg=median(LAST_ANN_PREM_GROSS))

ggplot(data=c5,aes(x=CONTENTS_COVER,y=avg))+
  geom_bar(stat="identity")
#INSIGHTS: Average  premium paid by clients who opted for contents cover is higher 


#HOME_EM_ADDON

ggplot(data=binary_,aes(x=HOME_EM_ADDON,y=LAST_ANN_PREM_GROSS))+
  geom_boxplot()
c6 <- binary_%>%group_by(HOME_EM_ADDON)%>%summarise(
  avg=median(LAST_ANN_PREM_GROSS))

ggplot(data=c6,aes(x=HOME_EM_ADDON,y=avg))+
  geom_bar(stat="identity",aes(fill=HOME_EM_ADDON))+
  labs(title="Average PRemium of HOuses Who opted for Emergencies")+
  theme_minimal()+
  theme(legend.position = "top")
#INSIGHTS :Home emergencies opted clients pays a higher premium


#KEYCARE_ADDON

ggplot(data=binary_,aes(x=KEYCARE_ADDON,y=LAST_ANN_PREM_GROSS))+
  geom_boxplot()
c7 <- binary_%>%group_by(KEYCARE_ADDON)%>%summarise(
  avg=median(LAST_ANN_PREM_GROSS))

ggplot(data=c7,aes(x=KEYCARE_ADDON,y=avg))+
  geom_bar(stat="identity")
#INSIGHTS :Keycare opted clients pays a higher premium


#GARDEN_ADDON
ggplot(data=binary_,aes(x=GARDEN_ADDON,y=LAST_ANN_PREM_GROSS))+
  geom_boxplot()
c8 <- binary_%>%group_by(GARDEN_ADDON)%>%summarise(
  avg=median(LAST_ANN_PREM_GROSS))

ggplot(data=c8,aes(x=GARDEN_ADDON,y=avg))+
  geom_bar(stat="identity")
#INSIGHTS :Garden opted clients pays a higher premium


###########FEATURES OF HOUSE

# APPR_LOCKS
ggplot(data = binary_, aes(x = APPR_LOCKS, y = LAST_ANN_PREM_GROSS)) +
  geom_boxplot()

c9 <- binary_ %>% group_by(APPR_LOCKS) %>% summarise(
  avg = median(LAST_ANN_PREM_GROSS))

ggplot(data = c9, aes(x = APPR_LOCKS, y = avg)) +
  geom_bar(stat = "identity",aes(fill=APPR_LOCKS))+
  labs(title="Difference in PRemium paid by houses with appropriate locks",x="Lock Type",y="Average Preium")+
  theme(legend.position = "top")
#INSIGHTS :Houses with APPR_LOCKS PAYS A higher premium


# SAFE_INSTALLED
ggplot(data = binary_, aes(x = SAFE_INSTALLED, y = LAST_ANN_PREM_GROSS)) +
  geom_boxplot()

c10 <- binary_ %>% group_by(SAFE_INSTALLED) %>% summarise(
  avg = median(LAST_ANN_PREM_GROSS))

ggplot(data = c10, aes(x = SAFE_INSTALLED, y = avg)) +
  geom_bar(stat = "identity",aes(fill=SAFE_INSTALLED))+
  labs(title = "Difference of premium paid by houses with Safe installed")+
  theme(legend.position = "top")
#INSIGHTS: houses with safe installed pays a higher premium

# FLOODING
ggplot(data = binary_, aes(x = FLOODING, y = LAST_ANN_PREM_GROSS)) +
  geom_boxplot()

c11 <- binary_ %>% group_by(FLOODING) %>% summarise(
  avg = median(LAST_ANN_PREM_GROSS))

ggplot(data = c11, aes(x = FLOODING, y = avg)) +
  geom_bar(stat = "identity",aes(fill=FLOODING))+
  labs(title="FLOODING status of houses against premium paid",x="FLOODING",Y="Average Premium paid")+
  theme(legend.position = "top")

#INSIGHTS :houses which are not flood proof pays a higher premium

# BUS_USE
ggplot(data = binary_, aes(x = BUS_USE, y = LAST_ANN_PREM_GROSS)) +
  geom_boxplot()

c12 <- binary_ %>% group_by(BUS_USE) %>% summarise(
  avg = median(LAST_ANN_PREM_GROSS))

ggplot(data = c12, aes(x = BUS_USE, y = avg)) +
  geom_bar(stat = "identity",aes(fill=BUS_USE))+
  labs(title = "Difference in premium paid by houses with bus option")
#INSGIHTS : houses which has bus_use option pays higher premium


# HP1_ADDON
c13 <- binary_ %>% group_by(HP1_ADDON) %>% summarise(
  avg = median(LAST_ANN_PREM_GROSS))

ggplot(data = c13, aes(x = HP1_ADDON, y = avg)) +
  geom_bar(stat = "identity")

# HP2_ADDON
c14 <- binary_ %>% group_by(HP2_ADDON) %>% summarise(
  avg = median(LAST_ANN_PREM_GROSS))

ggplot(data = c14, aes(x = HP2_ADDON, y = avg)) +
  geom_bar(stat = "identity")

# HP3_ADDON
c15 <- binary_ %>% group_by(HP3_ADDON) %>% summarise(
  avg = median(LAST_ANN_PREM_GROSS))

ggplot(data = c15, aes(x = HP3_ADDON, y = avg)) +
  geom_bar(stat = "identity")

# Assuming binary_ is a data frame
binary_$no_of_house_addon <- rowSums(binary_[c("HP1_ADDON", "HP2_ADDON", "HP3_ADDON")])
unique(binary_$no_of_house_addon )
ggplot(data=binary_,aes(x=LAST_ANN_PREM_GROSS,y=no_of_house_addon))+
  geom_point()

#INSIGHTS :There is no as such hike in premium paid with the number of houses add on 


names(subsetted_df)
security_dicount <-subsetted_df%>%select(SEC_DISC_REQ,LAST_ANN_PREM_GROSS)
ggplot(data=security_dicount,aes(x=SEC_DISC_REQ,y=LAST_ANN_PREM_GROSS))+
  geom_boxplot()
security_df <-subsetted_df%>%group_by(SEC_DISC_REQ)%>%summarise(avg=mean(LAST_ANN_PREM_GROSS))

ggplot(data=security_df,aes(x=SEC_DISC_REQ,y=avg))+
  geom_bar(stat="identity",aes(fill=SEC_DISC_REQ))

ggplot(data=security_dicount,aes(x=LAST_ANN_PREM_GROSS,fill=SEC_DISC_REQ))+
  geom_histogram(position = "identity", alpha = 0.5, bins = 10)

#Clients who have opted for security discount pays higher premium
#This is a unlikely scenario and T-test also shows no significance.




#Analysis of Customers who are likely to fraud on their payments

#Insights from resiliated policies
#Gender     :  40% female and 59% male
#MTA_FLAG   : 70% is Yes and 30% is NO
#CLAIM3YEARS :89% iS NO AND 11% is YES
#Police emission : 99% is no and 0.01 % is yes
#LEGAL_ADDON     :63% is YES AND 36% is NO
#Maritial status  :M is 44%, O is 17%, w is 14%, P is 11% ,and S is 05%
#EMP_STATUS : R is 76%,  E is 18%, S is 2% and  others are less than 1%
#Payment_method :62% is NonDD and 33% is pure DD and there is only  4% default from DD-other payments
#SEC_DISC_REQ : 79% is YES


