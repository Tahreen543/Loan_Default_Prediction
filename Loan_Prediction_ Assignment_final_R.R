getwd()
dir.create("LoanDefaultAnalysis_R")
wd = "/content/LoanDefaultAnalysis_R"
str(allPackage <- installed.packages(.Library, priority = "high"))
install.packages("DataExplorer")
install.packages("readxl")
url <- "https://raw.githubusercontent.com/lemoinef/Loan-Default-Prediction/master/loans_2007.csv"
raw_data <- read.csv(url)
head(raw_data,5)
write.csv(raw_data,"raw_loan_default.csv")
str(raw_data)
library(readxl)
data<- read_excel("/content/LoanDefaultAnalysis_R/LCDataDictionary.xlsx")
data
dim(raw_data)
library(dplyr)  
features_1st_13_set <- c('id','member_id','loan_amnt','funded_amnt','funded_amnt_inv','term',
                         'int_rate','installment','grade','sub_grade','emp_title','emp_length','home_ownership')
subset(data, LoanStatNew %in% features_1st_13_set)
raw_data %>% 
  select(features_1st_13_set) %>% 
  head(5)
## drop columns which does not provide any meaningful interpetation.
drop_col_1st_set <- c('id', 'member_id', 'funded_amnt', 'funded_amnt_inv', 'grade', 'sub_grade', 'emp_title')
features_2nd_13_set <- c('annual_inc','verification_status','issue_d','loan_status','pymnt_plan','purpose',
                         'title','zip_code','addr_state','dti','delinq_2yrs','earliest_cr_line', 'inq_last_6mths')

subset(data, LoanStatNew %in% features_2nd_13_set)
## drop columns which does not provide any meaningful interpetation.
drop_col_2nd_set <- c('issue_d','zip_code')
features_3rd_13_set <- c('open_acc', 'pub_rec', 'revol_bal','revol_util','total_acc', 'initial_list_status','out_prncp',
                         'out_prncp_inv','total_pymnt','total_pymnt_inv','total_rec_prncp','total_rec_int', 'total_rec_late_fee')
subset(data, LoanStatNew %in% features_3rd_13_set)

raw_data %>% 
  select(features_3rd_13_set) %>% 
  head(5)
## drop columns which does not provide any meaningful interpetation.
drop_col_3rd_set <- c('out_prncp', 'out_prncp_inv', 'total_pymnt', 'total_pymnt_inv','total_rec_prncp', 'total_rec_int', 'total_rec_late_fee')

features_4th_13_set <- c('recoveries','collection_recovery_fee', 'last_pymnt_d', 'last_pymnt_amnt','last_credit_pull_d',
                         'collections_12_mths_ex_med','policy_code','application_type','acc_now_delinq','chargeoff_within_12_mths',
                         'delinq_amnt','pub_rec_bankruptcies','tax_liens')
subset(data, LoanStatNew %in% features_4th_13_set)
raw_data %>% 
  select(features_4th_13_set) %>% 
  head(5)
## drop columns which does not provide any meaningful interpetation.
drop_col_4th_set <- c('recoveries', 'collection_recovery_fee', 'last_pymnt_d', 'last_pymnt_amnt')
columns_to_drop <- c(drop_col_1st_set,drop_col_2nd_set,drop_col_3rd_set,drop_col_4th_set)
columns_to_drop
filtered_data <- raw_data %>% 
  select(select = -c(drop_col_1st_set,drop_col_2nd_set,drop_col_3rd_set,drop_col_4th_set))
filtered_data %>% head(5)
str(filtered_data)
dim(filtered_data)
write.csv(filtered_data,"filtered_loan_default.csv")
filtered_data %>%  
    count(loan_status,sort = TRUE)

library(ggplot2)
install.packages("forcats")
library(forcats)
options(repr.plot.width=15, repr.plot.height=10)

ggplot(filtered_data,aes(x=fct_infreq(loan_status)))+
  geom_bar(position="dodge")+ 
  labs(title="Vertical Bar Plot of the Loan Status", x="Loan Status", y = "Frequency")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),text = element_text(size = 14))

# Make plots wider 
options(repr.plot.width=15, repr.plot.height=10)

ggplot(filtered_data,aes(x=fct_rev(fct_infreq(loan_status))))+
  geom_bar(position="dodge")+ 
  labs(title="Vertical Bar Plot of the Loan Status", x="Loan Status", y = "Frequency")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),text = element_text(size = 14))

df <- filtered_data %>% 
  filter(loan_status %in% c("Fully Paid", "Charged Off"))

write.csv(df,"loan_data.csv")

# Make plots wider 
options(repr.plot.width=15, repr.plot.height=10)

ggplot(df,aes(x=fct_infreq(loan_status)))+
  geom_bar(position="dodge")+ 
  labs(title="Vertical Bar Plot of the Loan Status", x="Loan Status", y = "Frequency")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5),text = element_text(size = 14))
library(DataExplorer)
library(tidyr)
library(magrittr)

df %>% 
  create_report(
    output_file = "loan_default_profile_report",
    output_dir  =  ,
    y= "loan_status",
    report_title = "EDA Report - Loan Default Analysis"
  )
install.packages("skimr")

library(skimr)
df %>% skim()

single_value_columns <- df %>%
  select_if(function(col) length(unique(col))==1)

names(single_value_columns)

c('pymnt_plan',
  'initial_list_status',
  'collections_12_mths_ex_med',
  'policy_code',
  'application_type',
  'acc_now_delinq',
  'chargeoff_within_12_mths',
  'delinq_amnt',
  'tax_liens')
subset(df, select = c('pymnt_plan',
                      'initial_list_status',
                      'collections_12_mths_ex_med',
                      'policy_code',
                      'application_type',
                      'acc_now_delinq',
                      'chargeoff_within_12_mths',
                      'delinq_amnt',
                      'tax_liens')) 

df <- df %>% 
  select(select = -c('pymnt_plan',
                     'initial_list_status',
                     'collections_12_mths_ex_med',
                     'policy_code',
                     'application_type',
                     'acc_now_delinq',
                     'chargeoff_within_12_mths',
                     'delinq_amnt',
                     'tax_liens'))



dim(df)

colSums(is.na(df))

null_count <- sapply(df, function(x) sum(is.na(x)))
data.frame(null_count)
df <- df %>% 
  select(select = -c('pub_rec_bankruptcies'))

sapply(df, class)
df <- df %>% 
  select(select = -c('pub_rec_bankruptcies'))
table(sapply(df, class))
df <- df %>% mutate_if(is.character, as.factor) 
col_names <- df %>% 
  select_if(is.character) %>% 
  names()
df %>%  
  count(int_rate,sort = TRUE)

df <- df %>% mutate(int_rate = as.numeric(gsub("%", "", int_rate)))

df %>%  
  count(revol_util,sort = TRUE)

df <- df %>% mutate(revol_util = as.numeric(gsub("%", "", revol_util)))
df
df %>%  
  count(term,sort = TRUE)
loan_term.map <- c(" 36 months"= 36,
                   " 60 months"= 60) 
df$term <- loan_term.map[as.character(df$term)]

df$term

mapping_dict = {
  "emp_length": {
    "10+ years": 10,
    "9 years": 9,
    "8 years": 8,
    "7 years": 7,
    "6 years": 6,
    "5 years": 5,
    "4 years": 4,
    "3 years": 3,
    "2 years": 2,
    "1 year": 1,
    "< 1 year": 0,
    "n/a": 0
  }
}
   
df %>%  
  count(emp_length,sort = TRUE)
empployement_duraion.map <- c("10+ years"= 10,
                              "9 years"= 9,
                              "8 years"= 8,
                              "7 years"= 7,
                              "6 years"= 6,
                              "5 years"= 5,
                              "4 years"= 4,
                              "3 years"= 3,
                              "2 years"= 2,
                              "1 year"= 1,
                              "< 1 year"= 0,
                              "n/a"= 0)    
df$emp_length <- empployement_duraion.map[as.character(df$emp_length)]


df %>%  
  count(home_ownership,sort = TRUE)
df %>%  
  count(verification_status,sort = TRUE)
install.packages("caret")
library(caret)
df %>%  
  count(purpose,sort = TRUE)

df %>%  
  count(title,sort = TRUE)


ggplot(df,aes(x=loan_amnt))+
  geom_histogram()+
  labs(title="Distribution of Loan Amount",
       x="Loan Amount")

ggplot(df,aes(x=term))+
  geom_histogram()+
  labs(title="Distribution of Term",
       x="Term")

ggplot(df,aes(x=int_rate))+
  geom_histogram()+
  labs(title="Distribution of Rate of Interest",
       x="Rate of Interest")

ggplot(df,aes(x=installment))+
  geom_histogram()+
  labs(title="Distribution of Installment",
       x="Installment")

ggplot(df,aes(x=emp_length))+
  geom_histogram()+
  labs(title="Distribution of Employee Length",
       x="Employee Lenght")

ggplot(df,aes(x=annual_inc))+
  geom_histogram()+
  labs(title="Distribution of Annual Income of Customers",
       x="Annual Income Of Customers")

ggplot(df,aes(x=annual_inc))+
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 20) + 
  labs(title="Distribution of Annual Income of Customers",
       subtitle = "number of bins = 20",
       x="Annual Income Of Customers")

ggplot(df,aes(x=revol_bal))+
  geom_density(fill = "indianred3") +
  labs(title = "Revolving Balance")

ggplot(df,aes(x=revol_util))+
  geom_density(fill = "indianred3") +
  labs(title = "Revolving Utilization")


plotdata <- df %>%
  count(term) %>%
  arrange(desc(term)) %>%
  mutate(prop = round(n * 100 / sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5  *prop)

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = term)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void()


ggplot(df, aes(x = term)) + 
  geom_bar()


ggplot(df, aes(x = emp_length))+
  geom_bar()

ggplot(df, aes(x = home_ownership))+
  geom_bar()

ggplot(df, aes(x = verification_status))+
  geom_bar()

ggplot(df, aes(x = loan_status))+
  geom_bar()

ggplot(df, aes(x = purpose))+
  geom_bar()
library(ggplot2)

ggplot(df,
       aes(x = loan_amnt, 
           y = int_rate +
  geom_point()
ggplot(df,
        aes(x =loan_amnt,
             y=int_rate ))+
    geom_point()

ggplot(df,
       aes(x = loan_amnt,
           y = term))+
    geom_point(color="steelblue")+
    geom_smooth(method= "lm")

ggplot(df,
       aes(x = int_rate,
           y = term))+
  geom_point(color="steelblue")+
  geom_smooth(method= "lm")  

ggplot(df,
       aes(x =int_rate,
           y=annul_inc))+
  geom_point()
library(superheat)
superheat(df,
          scale=TRUE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.text.size=.05,
          rom.dendrogram=TRUE)
data(df)
superheat(df,scale=TRUE)

ggplot(df,
       aes(x=loan_status,
           y=loan_amnt))+
  geom_bar(stat="identity")
       
ggplot(df,
       aes(x=loan_status,
           y=int_rate))+
  geom_bar(stat="identity")    

ggplot(df,
       aes(x=loan_status,
           y=term))+
  geom_bar(stat="identity")  
ggplot(df,
       aes(x=loan_status,
           y=term))+
  geom_boxplot()+
  labs(title="Term w.r.t Loan Status")

ggplot(df,
       aes(x=loan_status,
           y=int_rate)+
  geom_boxplot()+
  labs(title="Rate of INterest")
  
  
  ggplot(df, 
         aes(x = loan_status,
             y = int_rate))+
    geom_boxplot() +
    labs(title = "RAte of Interest w.r.t Loan Status
                   
 
ggplot(df,
      aes(x= loan_status,
          y= int_rate))+
  geom_violin()+
  labs(title= "Loanstatus distribution by ROI")
 
ggplot(df,
      aes(x= loan_status,
          y= loan_amnt))+
  geom_violin()+
  labs(title= "Loanstatus distribution by Loan Amount")
 
 
ggplot(df,
      aes(x= loan_status,
          y= installment))+
  geom_violin()+
  labs(title= "Loanstatus distribution by Installment") 
 
 
ggplot(df,
      aes(x= loan_status,
          y= emp_length()+
  labs(title= "Loanstatus distribution by Employee Length
  
 
ggcorrplot(df,
           hc.order=TRUE,
           type="lower",
           lab=TRUE)
         
superheat(df,scale =TRUE)
         
names(df)
df_numeric <- c( "loan_amnt","term" ,"int_rate","installment" ,"emp_length","annual_inc")
df_numeric
df%>%
select(df_numeric)%>%
head(5)
superheat(df_numeric, scale = TRUE)
data(df_numeric)
superheat(df_numeric, scale = TRUE)


superheat(df,
          scale = TRUE,
          left.label.text.size = 3,
          bottom.label.text.size = 3,
          bottom.label.size = .05,
          row.dendrogram = TRUE)

ggplot(df,
       aes(x=loan_status,
           y=int_rate)+
         geom_boxplot()+
         labs(title="Rate of INterest")
       
df%>%ggplot(aes(x=emp_length))+
  geom_line(aes(y=home_ownership))+
  geom_line(aes(y=verification_status),color="blue")

df%>% gather(loan_status,home_ownership,verification_status,purpose)%>%
  ggplot(aes(x =term , y=value))+geom_line()+
  facet_grid(key ~.)
     
df %>% ggplot(aes(x =verification_status,y=loan_amnt))+
  geom_point()+geom_smooth()
  
