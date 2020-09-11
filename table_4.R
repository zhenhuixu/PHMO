##--- PHMO Analysis: Table 4 (logistic for eligibility outcomes models) ---##
##--- Brooke Alhanti, April 2020 --------##

rm(list=ls())

library(tidyverse)
library(splines)


##-- Reading in data --##
setwd("E:/Graduate_year_1/PHMO")
dat19 = readRDS("dat_19.rds")


#remove the following PatientKeys - an extreme value (very low cost, impossible days in hospice, >20 MRIs)
dat19 = dat19 %>% filter(PatientKey != 421661,  PatientKey != 359067, PatientKey != 142780)

dat19 <- dat19 %>% mutate(PrimaryCare = replace_na(PrimaryCare, "Missing"))
dat19$PrimaryCare <- as.factor(dat19$PrimaryCare)

##----- outcome: Dual -----##
#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(Dual ~ Sex, data=sex19, family = "binomial")
summary(fit1)

fit2 = glm(Dual ~ Age, data=dat19, family = "binomial")
summary(fit2)

fit3 = glm(Dual ~ ESRD, data=dat19, family = "binomial")
summary(fit3)

#fit4 = glm(Dual ~ Dual, data=dat19, family = "binomial")
#summary(fit4)

fit5 = glm(Dual ~ NonDual, data=dat19, family = "binomial")
summary(fit5)

fit6 = glm(Dual ~ Disabled, data=dat19, family = "binomial")
summary(fit6)

fit7 = glm(Dual ~ Cont_Att, data=dat19, family = "binomial")
summary(fit7)

fit8 = glm(Dual ~ Death, data=dat19, family = "binomial")
summary(fit8)

fit9 = glm(Dual ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(Dual ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)

log_Avg19 <- dat19 %>% filter(!is.infinite(log_Avg_3))
fit11 = glm(Dual ~ log_Avg_3, data=log_Avg19, family = "binomial")
summary(fit11)

fit12 = glm(Dual ~ `Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

fit13 = glm(Dual ~ `ED Visits`, data=dat19, family = "binomial")
summary(fit13)

fit14 = glm(Dual ~ UnplannedAdmits, data=dat19, family = "binomial")
summary(fit14)

fit15 = glm(Dual ~ Readmits, data=dat19, family = "binomial")
summary(fit15)

fit16 = glm(Dual ~ `Total Primary Care Services`, data=dat19, family = "binomial")
summary(fit16)

fit17 = glm(Dual ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
summary(fit17)

fit18 = glm(Dual ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
summary(fit18)

fit19 = glm(Dual ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
summary(fit19)

fit20 = glm(Dual ~ `Days in Hospice`, data=dat19, family = "binomial")
summary(fit20)

fit21 = glm(Dual ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(Dual ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

fit23 = glm(Dual ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(Dual ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(Dual ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(Dual ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(Dual ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(Dual ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(Dual ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(Dual ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(Dual ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(Dual ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(Dual ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

fit34 =  glm(Dual ~ PrimaryCare, data=dat19, family = "binomial")
summary(fit34)

##--- Setting up Dual table ---##

r1 = cbind(round(coef(fit1)[2], 5),  round(confint(fit1)[2,1], 5),  round(confint(fit1)[2,2], 5))
r2 = cbind(round(coef(fit2)[2], 5),  round(confint(fit2)[2,1], 5),  round(confint(fit2)[2,2], 5))
r3 = cbind(round(coef(fit3)[2], 5),  round(confint(fit3)[2,1], 5),  round(confint(fit3)[2,2], 5))
r4 = cbind(0,  0,  0)
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
r16 = cbind(round(coef(fit16)[2], 5), round(confint(fit16)[2,1], 5), round(confint(fit16)[2,2], 5))
r17 = cbind(round(coef(fit17)[2], 5), round(confint(fit17)[2,1], 5), round(confint(fit17)[2,2], 5))
r18 = cbind(round(coef(fit18)[2], 5), round(confint(fit18)[2,1], 5), round(confint(fit18)[2,2], 5))
r19 = cbind(round(coef(fit19)[2], 5), round(confint(fit19)[2,1], 5), round(confint(fit19)[2,2], 5))
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
r34a = cbind(round(coef(fit34)[2], 5), round(confint(fit34)[2,1], 5), round(confint(fit34)[2,2], 5))
r34b = cbind(round(coef(fit34)[3], 5), round(confint(fit34)[3,1], 5), round(confint(fit34)[3,2], 5))

tab.e = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                         exp(r9), exp(r10), exp(r11), exp(r12), exp(r13), exp(r14), 
                         exp(r15), exp(r16), exp(r17), exp(r18),  exp(r19), exp(r20), exp(r21), 
                         exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                         exp(r28), exp(r29), exp(r30), exp(r31), exp(r33), exp(r34a), exp(r34b))


r.names = rownames(tab.e)
tab.e = as_tibble(cbind.data.frame(r.names, tab.e))
colnames(tab.e) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.e

###









##----- outcome: NonDual -----##
#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(NonDual ~ Sex, data=sex19, family = "binomial")
summary(fit1)

fit2 = glm(NonDual ~ Age, data=dat19, family = "binomial")
summary(fit2)

fit3 = glm(NonDual ~ ESRD, data=dat19, family = "binomial")
summary(fit3)

fit4 = glm(NonDual ~ Dual, data=dat19, family = "binomial")
summary(fit4)

#fit5 = glm(NonDual ~ NonDual, data=dat19, family = "binomial")
#summary(fit5)

fit6 = glm(NonDual ~ Disabled, data=dat19, family = "binomial")
summary(fit6)

fit7 = glm(NonDual ~ Cont_Att, data=dat19, family = "binomial")
summary(fit7)

fit8 = glm(NonDual ~ Death, data=dat19, family = "binomial")
summary(fit8)

fit9 = glm(NonDual ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(NonDual ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)

fit11 = glm(NonDual ~ log_Avg_3, data=log_Avg19, family = "binomial")
summary(fit11)

fit12 = glm(NonDual ~ `Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

fit13 = glm(NonDual ~ `ED Visits`, data=dat19, family = "binomial")
summary(fit13)

fit14 = glm(NonDual ~ UnplannedAdmits, data=dat19, family = "binomial")
summary(fit14)

fit15 = glm(NonDual ~ Readmits, data=dat19, family = "binomial")
summary(fit15)

fit16 = glm(NonDual ~ `Total Primary Care Services`, data=dat19, family = "binomial")
summary(fit16)

fit17 = glm(NonDual ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
summary(fit17)

fit18 = glm(NonDual ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
summary(fit18)

fit19 = glm(NonDual ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
summary(fit19)

fit20 = glm(NonDual ~ `Days in Hospice`, data=dat19, family = "binomial")
summary(fit20)

fit21 = glm(NonDual ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(NonDual ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

fit23 = glm(NonDual ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(NonDual ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(NonDual ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(NonDual ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(NonDual ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(NonDual ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(NonDual ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(NonDual ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(NonDual ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(NonDual ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(NonDual ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

fit34 =  glm(NonDual ~ PrimaryCare, data=dat19, family = "binomial")
summary(fit34)

##--- Setting up Non-Dual table ---##

r1 = cbind(round(coef(fit1)[2], 5),  round(confint(fit1)[2,1], 5),  round(confint(fit1)[2,2], 5))
r2 = cbind(round(coef(fit2)[2], 5),  round(confint(fit2)[2,1], 5),  round(confint(fit2)[2,2], 5))
r3 = cbind(round(coef(fit3)[2], 5),  round(confint(fit3)[2,1], 5),  round(confint(fit3)[2,2], 5))
r4 = cbind(round(coef(fit4)[2], 5),  round(confint(fit4)[2,1], 5),  round(confint(fit4)[2,2], 5))
r5 = cbind(0,  0,  0) 
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
r16 = cbind(round(coef(fit16)[2], 5), round(confint(fit16)[2,1], 5), round(confint(fit16)[2,2], 5))
r17 = cbind(round(coef(fit17)[2], 5), round(confint(fit17)[2,1], 5), round(confint(fit17)[2,2], 5))
r18 = cbind(round(coef(fit18)[2], 5), round(confint(fit18)[2,1], 5), round(confint(fit18)[2,2], 5))
r19 = cbind(round(coef(fit19)[2], 5), round(confint(fit19)[2,1], 5), round(confint(fit19)[2,2], 5))
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
r34a = cbind(round(coef(fit34)[2], 5), round(confint(fit34)[2,1], 5), round(confint(fit34)[2,2], 5))
r34b = cbind(round(coef(fit34)[3], 5), round(confint(fit34)[3,1], 5), round(confint(fit34)[3,2], 5))

tab.f = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                         exp(r9), exp(r10), exp(r11), exp(r12), exp(r13), exp(r14), 
                         exp(r15), exp(r16), exp(r17), exp(r18),  exp(r19), exp(r20), exp(r21), 
                         exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                         exp(r28), exp(r29), exp(r30), exp(r31), exp(r33), exp(r34a), exp(r34b))

r.names = rownames(tab.f)
tab.f = as_tibble(cbind.data.frame(r.names, tab.f))
colnames(tab.f) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.f

###









##----- outcome: Disabled -----##
#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(Disabled ~ Sex, data=sex19, family = "binomial")
summary(fit1)

fit2 = glm(Disabled ~ Age, data=dat19, family = "binomial")
summary(fit2)

fit3 = glm(Disabled ~ ESRD, data=dat19, family = "binomial")
summary(fit3)

fit4 = glm(Disabled ~ Dual, data=dat19, family = "binomial")
summary(fit4)

fit5 = glm(Disabled ~ NonDual, data=dat19, family = "binomial")
summary(fit5)

#fit6 = glm(Disabled ~ Disabled, data=dat19, family = "binomial")
#summary(fit6)

fit7 = glm(Disabled ~ Cont_Att, data=dat19, family = "binomial")
summary(fit7)

fit8 = glm(Disabled ~ Death, data=dat19, family = "binomial")
summary(fit8)

fit9 = glm(Disabled ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(Disabled ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)

fit11 = glm(Disabled ~ log_Avg_3, data=log_Avg19, family = "binomial")
summary(fit11)

fit12 = glm(Disabled ~ dat19$`Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

fit13 = glm(Disabled ~ `ED Visits`, data=dat19, family = "binomial")
summary(fit13)

fit14 = glm(Disabled ~ `UnplannedAdmits`, data=dat19, family = "binomial")
summary(fit14)

fit15 = glm(Disabled ~ `Readmits`, data=dat19, family = "binomial")
summary(fit15)

fit16 = glm(Disabled ~ `Total Primary Care Services`, data=dat19, family = "binomial")
summary(fit16)

fit17 = glm(Disabled ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
summary(fit17)

fit18 = glm(Disabled ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
summary(fit18)

fit19 = glm(Disabled ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
summary(fit19)

fit20 = glm(Disabled ~ `Days in Hospice`, data=dat19, family = "binomial")
summary(fit20)

fit21 = glm(Disabled ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(Disabled ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

fit23 = glm(Disabled ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(Disabled ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(Disabled ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(Disabled ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(Disabled ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(Disabled ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(Disabled ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(Disabled ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(Disabled ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(Disabled ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(Disabled ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

fit34 =  glm(Disabled ~ PrimaryCare, data=dat19, family = "binomial")
summary(fit34)

##--- Setting up Dual table ---##

r1 = cbind(round(coef(fit1)[2], 5),  round(confint(fit1)[2,1], 5),  round(confint(fit1)[2,2], 5))
r2 = cbind(round(coef(fit2)[2], 5),  round(confint(fit2)[2,1], 5),  round(confint(fit2)[2,2], 5))
r3 = cbind(round(coef(fit3)[2], 5),  round(confint(fit3)[2,1], 5),  round(confint(fit3)[2,2], 5))
r4 = cbind(round(coef(fit4)[2], 5),  round(confint(fit4)[2,1], 5),  round(confint(fit4)[2,2], 5))
r5 = cbind(round(coef(fit5)[2], 5),  round(confint(fit5)[2,1], 5),  round(confint(fit5)[2,2], 5))
r6 = cbind(0,  0,  0)
r7 = cbind(round(coef(fit7)[2], 5),  round(confint(fit7)[2,1], 5),  round(confint(fit7)[2,2], 5))
r8 = cbind(round(coef(fit8)[2], 5),  round(confint(fit8)[2,1], 5),  round(confint(fit8)[2,2], 5))
r9 = cbind(round(coef(fit9)[2], 5),  round(confint(fit9)[2,1], 5),  round(confint(fit9)[2,2], 5))
r10 = cbind(round(coef(fit10)[2], 5), round(confint(fit10)[2,1], 5), round(confint(fit10)[2,2], 5))
r11 = cbind(round(coef(fit11)[2], 5), round(confint(fit11)[2,1], 5), round(confint(fit11)[2,2], 5))
r12 = cbind(round(coef(fit12)[2], 5), round(confint(fit12)[2,1], 5), round(confint(fit12)[2,2], 5))
r13 = cbind(round(coef(fit13)[2], 5), round(confint(fit13)[2,1], 5), round(confint(fit13)[2,2], 5))
r14 = cbind(round(coef(fit14)[2], 5), round(confint(fit14)[2,1], 5), round(confint(fit14)[2,2], 5))
r15 = cbind(round(coef(fit15)[2], 5), round(confint(fit15)[2,1], 5), round(confint(fit15)[2,2], 5))
r16 = cbind(round(coef(fit16)[2], 5), round(confint(fit16)[2,1], 5), round(confint(fit16)[2,2], 5))
r17 = cbind(round(coef(fit17)[2], 5), round(confint(fit17)[2,1], 5), round(confint(fit17)[2,2], 5))
r18 = cbind(round(coef(fit18)[2], 5), round(confint(fit18)[2,1], 5), round(confint(fit18)[2,2], 5))
r19 = cbind(round(coef(fit19)[2], 5), round(confint(fit19)[2,1], 5), round(confint(fit19)[2,2], 5))
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
r34a = cbind(round(coef(fit34)[2], 5), round(confint(fit34)[2,1], 5), round(confint(fit34)[2,2], 5))
r34b = cbind(round(coef(fit34)[3], 5), round(confint(fit34)[3,1], 5), round(confint(fit34)[3,2], 5))

tab.g = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                         exp(r9), exp(r10), exp(r11), exp(r12), exp(r13), exp(r14), 
                         exp(r15), exp(r16), exp(r17), exp(r18),  exp(r19), exp(r20), exp(r21), 
                         exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                         exp(r28), exp(r29), exp(r30), exp(r31), exp(r33), exp(r34a), exp(r34b))


r.names = rownames(tab.g)
tab.g = as_tibble(cbind.data.frame(r.names, tab.g))
colnames(tab.g) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.g

###









##----- outcome: ESRD -----##
#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(ESRD ~ Sex, data=sex19, family = "binomial")
summary(fit1)

fit2 = glm(ESRD ~ Age, data=dat19, family = "binomial")
summary(fit2)

#fit3 = glm(ESRD ~ ESRD, data=dat19, family = "binomial")
#summary(fit3)

fit4 = glm(ESRD ~ Dual, data=dat19, family = "binomial")
summary(fit4)

fit5 = glm(ESRD ~ NonDual, data=dat19, family = "binomial")
summary(fit5)

fit6 = glm(ESRD ~ Disabled, data=dat19, family = "binomial")
summary(fit6)

fit7 = glm(ESRD ~ Cont_Att, data=dat19, family = "binomial")
summary(fit7)

fit8 = glm(ESRD ~ Death, data=dat19, family = "binomial")
summary(fit8)

fit9 = glm(ESRD ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(ESRD ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)

fit11 = glm(ESRD ~ log_Avg_3, data=log_Avg19, family = "binomial")
summary(fit11)

fit12 = glm(ESRD ~ `Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

fit13 = glm(ESRD ~ `ED Visits`, data=dat19, family = "binomial")
summary(fit13)

fit14 = glm(ESRD ~ UnplannedAdmits, data=dat19, family = "binomial")
summary(fit14)

fit15 = glm(ESRD ~ Readmits, data=dat19, family = "binomial")
summary(fit15)

fit16 = glm(ESRD ~ `Total Primary Care Services`, data=dat19, family = "binomial")
summary(fit16)

fit17 = glm(ESRD ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
summary(fit17)

fit18 = glm(ESRD ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
summary(fit18)

fit19 = glm(ESRD ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
summary(fit19)

fit20 = glm(ESRD ~ `Days in Hospice`, data=dat19, family = "binomial")
summary(fit20)

fit21 = glm(ESRD ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(ESRD ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

fit23 = glm(ESRD ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(ESRD ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(ESRD ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(ESRD ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(ESRD ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(ESRD ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(ESRD ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(ESRD ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(ESRD ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(ESRD ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(ESRD ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

fit34 =  glm(ESRD ~ PrimaryCare, data=dat19, family = "binomial")
summary(fit34)

##--- Setting up Dual table ---##

r1 = cbind(round(coef(fit1)[2], 5),  round(confint(fit1)[2,1], 5),  round(confint(fit1)[2,2], 5))
r2 = cbind(round(coef(fit2)[2], 5),  round(confint(fit2)[2,1], 5),  round(confint(fit2)[2,2], 5))
r3 = cbind(0,  0,  0) 
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
r16 = cbind(round(coef(fit16)[2], 5), round(confint(fit16)[2,1], 5), round(confint(fit16)[2,2], 5))
r17 = cbind(round(coef(fit17)[2], 5), round(confint(fit17)[2,1], 5), round(confint(fit17)[2,2], 5))
r18 = cbind(round(coef(fit18)[2], 5), round(confint(fit18)[2,1], 5), round(confint(fit18)[2,2], 5))
r19 = cbind(round(coef(fit19)[2], 5), round(confint(fit19)[2,1], 5), round(confint(fit19)[2,2], 5))
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
r34a = cbind(round(coef(fit34)[2], 5), round(confint(fit34)[2,1], 5), round(confint(fit34)[2,2], 5))
r34b = cbind(round(coef(fit34)[3], 5), round(confint(fit34)[3,1], 5), round(confint(fit34)[3,2], 5))

tab.h = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                         exp(r9), exp(r10), exp(r11), exp(r12), exp(r13), exp(r14), 
                         exp(r15), exp(r16), exp(r17), exp(r18),  exp(r19), exp(r20), exp(r21), 
                         exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                         exp(r28), exp(r29), exp(r30), exp(r31), exp(r33), exp(r34a), exp(r34b))


r.names = rownames(tab.h)
tab.h = as_tibble(cbind.data.frame(r.names, tab.h))
colnames(tab.h) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.h

###

