# Load the necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(readr)
#import dataset
imported_dataset<-read_csv("C:/Desktop2.0/Data Science Programming/diabetes+130-us+hospitals+for+years+1999-2008/diabetic_data.csv")
print(imported_dataset)
# Check the structure of the data set
str(imported_dataset)
#check for the missing values
any(is.na(imported_dataset))
sum(is.na(imported_dataset))
# Count how often '?' appears
sapply(imported_dataset,function(x) sum(x == '?', na.rm = TRUE))
# convert '?' to NA
imported_dataset[imported_dataset == '?'] <- NA
# Check for missing values again 
sum(is.na(imported_dataset))
#Select specific columns and drop rows with missing values in certain columns
cleaned_imported_dataset <- imported_dataset %>%
  select(-weight, -payer_code, -medical_specialty,-A1Cresult,-metformin,-repaglinide,-nateglinide,-chlorpropamide,-glimepiride,
         -acetohexamide,-glipizide,-glyburide,-tolbutamide,-pioglitazone,-rosiglitazone,-acarbose,-miglitol,-troglitazone) %>%
  filter(
      !is.na(race) &
      !is.na(diag_1) &
      !is.na(diag_2) &
      !is.na(diag_3))
head(cleaned_imported_dataset)
sum(is.na(cleaned_imported_dataset))
str(cleaned_imported_dataset)
#check for the unique values in the columns
cleaned_imported_dataset <- subset(cleaned_imported_dataset, gender!= "Unknown/Invalid"& gender!= "NO" & race != "Other" &race != "NO")
unique(cleaned_imported_dataset$race)
unique(cleaned_imported_dataset$gender)
#check for the duplicates
duplicates <- duplicated(cleaned_imported_dataset)
print(duplicates)
duplicates_count <- sum(duplicates)
print(duplicates_count)
#scale imported data
numeric_columns <- sapply(cleaned_imported_dataset, is.numeric)
print(numeric_columns)
z_score <- scale(cleaned_imported_dataset[, numeric_columns])
print(z_score)
#check for the outliers
outliers <- cleaned_imported_dataset[apply(abs(z_score) > 1, 1, any), ]
print(outliers)
nrow(outliers)
#check for the statistics of the data set
numeric_stats<-data.frame(
   mean = sapply(cleaned_imported_dataset[, numeric_columns], mean),
   median = sapply(cleaned_imported_dataset[, numeric_columns], median),
   sd = sapply(cleaned_imported_dataset[, numeric_columns], sd),
   min = sapply(cleaned_imported_dataset[, numeric_columns], min),
   max = sapply(cleaned_imported_dataset[, numeric_columns], max),
   var= sapply(cleaned_imported_dataset[, numeric_columns], var)
 )
 print(numeric_stats)
#t-test, for the following columns time_in_hospital, number of lab procedures, number of medications.
one_sample_ttest_time_in_hospital<-t.test(cleaned_imported_dataset$time_in_hospital, mu = 4)
print(one_sample_ttest_time_in_hospital)
#H0=mean=mu
#H1=mean!=mu
#According to the t-test,the p-value is 2.2e-16 or (0.00000000000000022) so we reject the H0 and accept the H1

one_sample_ttest_num_medications<-t.test(cleaned_imported_dataset$num_medications, mu = 2)
print(one_sample_ttest_num_medications)
#H0=mean=mu
#H1=mean!=mu
#According to the t-test,the p-value is 2.2e-16 or (0.00000000000000022) so we reject the H0 and accept the H1

#two sample t-test for the following columns number_outpatient,number_inpatient
two_sample_paired_ttest<-t.test(cleaned_imported_dataset$number_inpatient,
       cleaned_imported_dataset$number_outpatient,
       paired = TRUE)
print(two_sample_paired_ttest)
#H0=mean=mean
#H1=mean!=mean
#According to the t-test, the p-value is2.2e-16 (0.00000000000000022) so we reject the H0 and accept the H

#Alone dataset includes two columns race,medication numbers and time in hospital
race_anova_test <- data.frame(
  race = factor((cleaned_imported_dataset$race)),
  medication_numbers = cleaned_imported_dataset$num_medications,
  number_lab_procedures = cleaned_imported_dataset$num_lab_procedures,
  time_in_hospital = cleaned_imported_dataset$time_in_hospital
)
race_anova_test%>%
  mutate(race = factor(race)) %>%
  group_by(race) %>%
  sample_n(size = 10) %>%
  summarise(
    medication_mean = mean(medication_numbers),
    time_in_hospital = mean(time_in_hospital)
)
levels(race_anova_test$race)

race_medication_anova <- aov(medication_numbers ~ race, race_anova_test)
summary(race_medication_anova)# p-value < 0.05 there's at least one group different than others

#chi square test for the following columns

table_race_gender<-table(cleaned_imported_dataset$race, cleaned_imported_dataset$gender)
table_race_gender
chi_square_test_for_race_gender<-chisq.test(table_race_gender)
print(chi_square_test_for_race_gender)
#H0=there is no relationship between the two variables
#H1=there is a relationship between the two variables
#According to the chi-square test, the p-value is 0.0000000000000002 (2.2e-16) so we reject the H0 and accept the H1

table_age_insulin<-table(cleaned_imported_dataset$age, cleaned_imported_dataset$insulin)
table_age_insulin
chi_square_test_for_age_insulin<-chisq.test(table_age_insulin)
print(chi_square_test_for_age_insulin)
#H0=there is no relationship between the two variables
#H1=there is a relationship between the two variables
#According to the chi-square test, the p-value is 0.0000000000000002 (2.2e-16) so we reject the H0 and accept the H1

#correlation Test is a statistical test applied only on numerical columns to see weather the columns related to each other or not
#first we will ask our-self,As the number of diagnoses increases, does the number of medications prescribed also tend to increase?
#lets see
medications_diagnoses_test <- data.frame(
  no_medication <- cleaned_imported_dataset$num_medications,
  no_diagnoses <- cleaned_imported_dataset$number_diagnoses
)
#check the type of columns must be integer to proceed the correlation test
str(medications_diagnoses_test)

#changing column names into smaller form
names(medications_diagnoses_test) = c('no_medication','no_diagonses')

correlation_test = cor.test(medications_diagnoses_test$no_medication,medications_diagnoses_test$no_diagonses,method = 'pearson')
print(correlation_test)
#the correlation test is 0.2408395 which indicates that is appeared a weak positive linear correlation

#Visualiztion
#visualizing number of race groups
ggplot(cleaned_imported_dataset,aes(factor(race),fill = factor(race)))+
  geom_bar()+
  theme_classic()+
  geom_text(stat = 'count',aes(label = ..count..,color = 'black',size = 4),vjust = -0.5)

#visualizing number of medications for each group
head_race_anova = head(race_anova_test,2000)
ggplot(head_race_anova, aes(x=race, y=medication_numbers,color=race))+
  geom_boxplot()+
  geom_jitter(shape=15)+
  theme_minimal()

#visualizing time in hospital for each group
ggplot(head_race_anova, aes(x = race, y = time_in_hospital, color = race)) +
  geom_boxplot()+
  geom_jitter(shape = 15,alpha = 1) +
  ggtitle("time in hospital for each group") +
    theme_minimal()
#Visualizing the correlation between number of medications and number of diagnoses using a scatter plot
ggplot(head(medications_diagnoses_test,2000),aes(x = no_medication,y = no_diagonses))+
  geom_point(color = 'blue')+
  ggtitle('Correlation Test Scatter plot')
  theme_classic()+
 theme_minimal()
#Visualizing the correlation between number of medications and number of diagnoses using a heatmap
ggplot(medications_diagnoses_test, aes(x = no_medication, y = no_diagnoses)) +
  geom_bin2d(bins = 30) +  # Adjust bins for granularity
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "Heatmap of Medications vs Diagnoses counts",
       x = "Number of Medications",
       y = "Number of Diagnoses",
       fill = "Count") +
       theme_light()
# Visualizing the distribution of time in hospital across genders
ggplot(cleaned_imported_dataset, aes(x = gender,y = time_in_hospital,fill = gender))+
  geom_bar(stat = 'identity',width = 0.4)+
  labs(
    title = "Histogram for Time in Hospital per Gender",
    x = "Time in Hospital",
    y = "Gender"
  )+
  theme_minimal()

