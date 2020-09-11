##--- PHMO Analysis: Table 7 (Negative Binomial Models) ---##
##--- Zhenhui Xu, May 2020 --------##

rm(list=ls())

library(tidyverse)
library(ggplot2)

##-- Reading in data --##
setwd("E:/Graduate_year_1/PHMO")
dat19 = readRDS("dat_19.rds")

#remove the following PatientKeys - an extreme value (very low cost, impossible days in hospice, >20 MRIs)
dat19 = dat19 %>% filter(PatientKey != 421661,  PatientKey != 359067, PatientKey != 142780)

#create the percentages of each category of Primary Care Services
dat19 <- dat19 %>% 
  mutate(`Primary Care Services with a Primary Care Physician (%)`= `Primary Care Services with a Primary Care Physician`/`Total Primary Care Services`) %>%
  mutate(`Primary Care Services with a Specialist Physician (%)`= `Primary Care Services with a Specialist Physician`/`Total Primary Care Services`) %>%
  mutate(`Primary Care Services with APP (%)` = `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`/`Total Primary Care Services`)

##----- outcome: Total Number of Primary Care Services -----##
#create a binary indicator of Total Number of Primary Care Services
dat19 <- dat19 %>% mutate(Total_PCS_binary = ifelse(`Total Primary Care Services`==0, 0, 1))

dat19 %>% group_by(Total_PCS_binary) %>% tally()

#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(Total_PCS_binary ~ Sex, data=sex19, family = "binomial")
summary(fit1)

fit2 = glm(Total_PCS_binary ~ Age, data=dat19, family = "binomial")
summary(fit2)

fit3 = glm(Total_PCS_binary ~ ESRD, data=dat19, family = "binomial")
summary(fit3)

fit4 = glm(Total_PCS_binary ~ Dual, data=dat19, family = "binomial")
summary(fit4)

fit5 = glm(Total_PCS_binary ~ NonDual, data=dat19, family = "binomial")
summary(fit5)

fit6 = glm(Total_PCS_binary ~ Disabled, data=dat19, family = "binomial")
summary(fit6)

fit7 = glm(Total_PCS_binary ~ Cont_Att, data=dat19, family = "binomial")
summary(fit7)

fit8 = glm(Total_PCS_binary ~ Death, data=dat19, family = "binomial")
summary(fit8)

fit9 = glm(Total_PCS_binary ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(Total_PCS_binary ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)

fit11 = glm(Total_PCS_binary ~ log_Avg_3, data=dat19, family = "binomial")
summary(fit11)

fit12 = glm(Total_PCS_binary ~ `Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

fit13 = glm(Total_PCS_binary ~ `ED Visits`, data=dat19, family = "binomial")
summary(fit13)

fit14 = glm(Total_PCS_binary ~ `UnplannedAdmits`, data=dat19, family = "binomial")
summary(fit14)

fit15 = glm(Total_PCS_binary ~ `Readmits`, data=dat19, family = "binomial")
summary(fit15)

#fit16 = glm(Total_PCS_binary ~ `Total Primary Care Services`, data=dat19, family = "binomial")
#summary(fit16)

#fit17 = glm(Total_PCS_binary ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
#summary(fit17)

#fit18 = glm(Total_PCS_binary ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
#summary(fit18)

#fit19 = glm(Total_PCS_binary ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
#summary(fit19)

#fit34 = glm(Total_PCS_binary ~ `Primary Care Services with a Primary Care Physician (%)`, data = dat19, family = "binomial")
#summary(fit34)

#fit35 = glm(Total_PCS_binary ~ `Primary Care Services with a Specialist Physician (%)`, data = dat19, family = "binomial")
#summary(fit35)

#fit36 = glm(Total_PCS_binary ~ `Primary Care Services with APP (%)`, data = dat19, family = "binomial")
#summary(fit36)

fit20 = glm(Total_PCS_binary ~ `Days in Hospice`, data=dat19, family = "binomial")
summary(fit20)

fit21 = glm(Total_PCS_binary ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(Total_PCS_binary ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

fit23 = glm(Total_PCS_binary ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(Total_PCS_binary ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(Total_PCS_binary ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(Total_PCS_binary ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(Total_PCS_binary ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(Total_PCS_binary ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(Total_PCS_binary ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(Total_PCS_binary ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(Total_PCS_binary ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(Total_PCS_binary ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(Total_PCS_binary ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

r1 = cbind(round(coef(fit1)[2], 5),  round(confint(fit1)[2,1], 5),  round(confint(fit1)[2,2], 5))
r2 = cbind(round(coef(fit2)[2], 5),  round(confint(fit2)[2,1], 5),  round(confint(fit2)[2,2], 5))
r3 = cbind(round(coef(fit3)[2], 5),  round(confint(fit3)[2,1], 5),  round(confint(fit3)[2,2], 5))
r4 = cbind(round(coef(fit4)[2], 5),  round(confint(fit4)[2,1], 5),  round(confint(fit4)[2,2], 5))
r5 = cbind(round(coef(fit5)[2], 5),  round(confint(fit5)[2,1], 5),  round(confint(fit5)[2,2], 5))
r6 = cbind(round(coef(fit6)[2], 5),  round(confint(fit6)[2,1], 5),  round(confint(fit6)[2,2], 5))
r7 = cbind(round(coef(fit7)[2], 5),  round(confint(fit7)[2,1], 5),  round(confint(fit7)[2,2], 5))
r8 = cbind(round(coef(fit8)[2], 5),  round(confint(fit8)[2,1], 5),  round(confint(fit8)[2,2], 5))
r9 = cbind(round(coef(fit9)[2], 5),  round(confint(fit9)[2,1], 5),  round(confint(fit9)[2,2], 5))
r10 = cbind(round(coef(fit10)[2], 5), round(confint(fit10)[2,1], 5), round(confint(fit10)[2,2], 5))
r11 = cbind(round(coef(fit11)[2], 5), round(confint(fit11)[2,1], 5), round(confint(fit11)[2,2], 5))
r12 = cbind(round(coef(fit12)[2], 5), round(confint(fit12)[2,1], 5), round(confint(fit12)[2,2], 5))
r13 = cbind(round(coef(fit13)[2], 5), round(confint(fit13)[2,1], 5), round(confint(fit13)[2,2], 5))
r14 = cbind(round(coef(fit14)[2], 5), round(confint(fit14)[2,1], 5), round(confint(fit14)[2,2], 5))
r15 = cbind(round(coef(fit15)[2], 5), round(confint(fit15)[2,1], 5), round(confint(fit15)[2,2], 5))
#r16 = cbind(round(coef(fit16)[2], 5), round(confint(fit16)[2,1], 5), round(confint(fit16)[2,2], 5))
r16 = cbind(NA, NA, NA)
row.names(r16) = "`Total Primary Care Services`"
#r17 = cbind(round(coef(fit17)[2], 5), round(confint(fit17)[2,1], 5), round(confint(fit17)[2,2], 5))
r17 = cbind(NA, NA, NA)
row.names(r17) = "`Primary Care Services with a Primary Care Physician`"
#r18 = cbind(round(coef(fit18)[2], 5), round(confint(fit18)[2,1], 5), round(confint(fit18)[2,2], 5))
r18 = cbind(NA, NA, NA)
row.names(r18) = "`Primary Care Services with a Specialist Physician`"
#r19 = cbind(round(coef(fit19)[2], 5), round(confint(fit19)[2,1], 5), round(confint(fit19)[2,2], 5))
r19 = cbind(NA, NA, NA)
row.names(r19) = "`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`"
r20 = cbind(round(coef(fit20)[2], 5), round(confint(fit20)[2,1], 5), round(confint(fit20)[2,2], 5))
r21 = cbind(round(coef(fit21)[2], 5), round(confint(fit21)[2,1], 5), round(confint(fit21)[2,2], 5))
r22 = cbind(round(coef(fit22)[2], 5), round(confint(fit22)[2,1], 5), round(confint(fit22)[2,2], 5))
r23 = cbind(round(coef(fit23)[2], 5), round(confint(fit23)[2,1], 5), round(confint(fit23)[2,2], 5))
r24 = cbind(round(coef(fit24)[2], 5), round(confint(fit24)[2,1], 5), round(confint(fit24)[2,2], 5))
r25 = cbind(round(coef(fit25)[2], 5), round(confint(fit25)[2,1], 5), round(confint(fit25)[2,2], 5))
r26 = cbind(round(coef(fit26)[2], 5), round(confint(fit26)[2,1], 5), round(confint(fit26)[2,2], 5))
r27 = cbind(round(coef(fit27)[2], 5), round(confint(fit27)[2,1], 5), round(confint(fit27)[2,2], 5))
r28 = cbind(round(coef(fit28)[2], 5), round(confint(fit28)[2,1], 5), round(confint(fit28)[2,2], 5))
r29 = cbind(round(coef(fit29)[2], 5), round(confint(fit29)[2,1], 5), round(confint(fit29)[2,2], 5))
r30 = cbind(round(coef(fit30)[2], 5), round(confint(fit30)[2,1], 5), round(confint(fit30)[2,2], 5))
r31 = cbind(round(coef(fit31)[2], 5), round(confint(fit31)[2,1], 5), round(confint(fit31)[2,2], 5))
#r32 = cbind(round(coef(fit32)[2], 5), round(confint(fit32)[2,1], 5), round(confint(fit32)[2,2], 5))
r33 = cbind(round(coef(fit33)[2], 5), round(confint(fit33)[2,1], 5), round(confint(fit33)[2,2], 5))
#r34 = cbind(round(coef(fit34)[2], 5), round(confint(fit34)[2,1], 5), round(confint(fit34)[2,2], 5))
r34 = cbind(NA, NA, NA)
row.names(r34) = "`Primary Care Services with a Primary Care Physician (%)`"
#r35 = cbind(round(coef(fit35)[2], 5), round(confint(fit35)[2,1], 5), round(confint(fit35)[2,2], 5))
r35 = cbind(NA, NA, NA)
row.names(r35) = "`Primary Care Services with a Specialist Physician (%)`"
#r36 = cbind(round(coef(fit36)[2], 5), round(confint(fit36)[2,1], 5), round(confint(fit36)[2,2], 5))
r36 = cbind(NA, NA, NA)
row.names(r36) = "`Primary Care Services with APP (%)`"

tab.o = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                         exp(r9), exp(r10), exp(r11), exp(r12), r13, exp(r14), 
                         exp(r15), exp(r20), exp(r21), exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                         exp(r28), exp(r29), exp(r30), exp(r31), exp(r33))

r.names = rownames(tab.o)
tab.o = as_tibble(cbind.data.frame(r.names, tab.o))
colnames(tab.o) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.o

