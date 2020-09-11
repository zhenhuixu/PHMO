##--- PHMO Analysis: Table 1 ---##
##--- Brooke Alhanti, April 2020 --------##

rm(list=ls())

library(tidyverse)
library(tableone)

##-- Reading in data --##
setwd("C:/Users/ba124/Box/MSSP CY21 Model")
dat19 = readRDS("dat_19.rds")



## select variables for table --##
work = dat19 %>% select(`Total Expenditure Amount`, log_Cost_Care, `Cost Efficiency`, log_Cost_Eff, Sex, Age, Dual, 
                        NonDual, Disabled, ESRD, Cont_Att, Death, 'Total Hospital Discharges', 'ED Visits', 'UnplannedAdmits',
'Readmits', 'Total Primary Care Services', 'Primary Care Services with a Primary Care Physician',
'Primary Care Services with a Specialist Physician', 'Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist',
'Days in Hospice', 'Skilled Nursing Facility or Unit Discharges', 'Skilled Nursing Facility or Unit Utilization Days',
'Computed Tomography (CT) Events', 'Magnetic Resonance Imaging (MRI) Events',
HCC_Cancer, HCC_Diabetes, HCC_CAD, HCC_85, HCC_111, HCC_CKD, HCC,
PrimaryPracticeTIN, 'Total Eligibility Fraction', Three_Year_Average, log_Avg_3)


##-- Create Table_One --##
cat = c("Sex", "Dual", "NonDual", "Disabled", "ESRD", "Cont_Att", "Death", "HCC_Cancer", "HCC_Diabetes", "HCC_CAD", 
        "HCC_85", "HCC_111", "HCC_CKD", "PrimaryPracticeTIN")
tab1 = CreateTableOne(data = work, factorVars = cat)

cont <- c("Total Expenditure Amount", "log_Cost_Care", "Cost Efficiency", "log_Cost_Eff", "Age", 
          "Total Hospital Discharges", "ED Visits", "UnplannedAdmits", "Readmits", "Total Primary Care Services", 
          "Primary Care Services with a Primary Care Physician", "Primary Care Services with a Specialist Physician", 
          "Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist", 
          "Days in Hospice", "Skilled Nursing Facility or Unit Discharges", 
          "Skilled Nursing Facility or Unit Utilization Days", "Computed Tomography (CT) Events", 
          "Magnetic Resonance Imaging (MRI) Events", "HCC", "Total Eligibility Fraction", 
          "Three_Year_Average", "log_Avg_3")

print(tab1, nonnormal = cont)



##-- Select just top 10% of costs --##
top = work %>% filter(`Total Expenditure Amount` > quantile(work$`Total Expenditure Amount`, .90))

##-- Create Table --##
cat = c("Sex", "Dual", "NonDual", "Disabled", "ESRD", "Cont_Att", "Death", "HCC_Cancer", "HCC_Diabetes", "HCC_CAD", 
        "HCC_85", "HCC_111", "HCC_CKD", "PrimaryPracticeTIN")
tab1 = CreateTableOne(data = top, factorVars = cat)

cont <- c("Total Expenditure Amount", "log_Cost_Care", "Cost Efficiency", "log_Cost_Eff", "Age", 
          "Total Hospital Discharges", "ED Visits", "UnplannedAdmits", "Readmits", "Total Primary Care Services", 
          "Primary Care Services with a Primary Care Physician", "Primary Care Services with a Specialist Physician", 
          "Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist", 
          "Days in Hospice", "Skilled Nursing Facility or Unit Discharges", 
          "Skilled Nursing Facility or Unit Utilization Days", "Computed Tomography (CT) Events", 
          "Magnetic Resonance Imaging (MRI) Events", "HCC", "Total Eligibility Fraction", 
          "Three_Year_Average", "log_Avg_3")

print(tab1, nonnormal = cont)







