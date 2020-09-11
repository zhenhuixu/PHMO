##--- PHMO Analysis: Table 3 (logistic models) ---##
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

dat19 <- dat19 %>% 
  mutate(`Primary Care Services with a Primary Care Physician (%)`= `Primary Care Services with a Primary Care Physician`/`Total Primary Care Services`) %>%
  mutate(`Primary Care Services with a Specialist Physician (%)`= `Primary Care Services with a Specialist Physician`/`Total Primary Care Services`) %>%
  mutate(`Primary Care Services with APP (%)` = `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`/`Total Primary Care Services`)

#replace missing values into 0
dat19 <- dat19 %>% 
  mutate(`Primary Care Services with a Primary Care Physician (%)` = replace_na(`Primary Care Services with a Primary Care Physician (%)`,0)) %>%
  mutate(`Primary Care Services with a Specialist Physician (%)` = replace_na(`Primary Care Services with a Specialist Physician (%)`,0)) %>%
  mutate(`Primary Care Services with APP (%)` = replace_na(`Primary Care Services with APP (%)`,0))

##----- outcome: Cont_Att -----##
#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(Cont_Att ~ Sex, data=sex19, family = "binomial")
summary(fit1)

fit2 = glm(Cont_Att ~ Age, data=dat19, family = "binomial")
summary(fit2)

fit3 = glm(Cont_Att ~ ESRD, data=dat19, family = "binomial")
summary(fit3)

fit4 = glm(Cont_Att ~ Dual, data=dat19, family = "binomial")
summary(fit4)

fit5 = glm(Cont_Att ~ NonDual, data=dat19, family = "binomial")
summary(fit5)

fit6 = glm(Cont_Att ~ Disabled, data=dat19, family = "binomial")
summary(fit6)

#fit7 = glm(Cont_Att ~ Cont_Att, data=dat19, family = "binomial")
#summary(fit7)

fit8 = glm(Cont_Att ~ Death, data=dat19, family = "binomial")
summary(fit8)

fit9 = glm(Cont_Att ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(Cont_Att ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)


log_Avg19 <- dat19 %>% filter(!is.infinite(log_Avg_3))
fit11 = glm(Cont_Att ~ log_Avg_3, data=log_Avg19, family = "binomial")
summary(fit11)

fit12 = glm(Cont_Att ~ dat19$`Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

fit13 = glm(Cont_Att ~ `ED Visits`, data=dat19, family = "binomial")
summary(fit13)

fit14 = glm(Cont_Att ~ `UnplannedAdmits`, data=dat19, family = "binomial")
summary(fit14)

fit15 = glm(Cont_Att ~ `Readmits`, data=dat19, family = "binomial")
summary(fit15)

fit16 = glm(Cont_Att ~ `Total Primary Care Services`, data=dat19, family = "binomial")
summary(fit16)

fit17 = glm(Cont_Att ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
summary(fit17)

fit18 = glm(Cont_Att ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
summary(fit18)

fit19 = glm(Cont_Att ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
summary(fit19)

fit20 = glm(Cont_Att ~ `Days in Hospice`, data=dat19, family = "binomial")
summary(fit20)

fit21 = glm(Cont_Att ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(Cont_Att ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

fit23 = glm(Cont_Att ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(Cont_Att ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(Cont_Att ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(Cont_Att ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(Cont_Att ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(Cont_Att ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(Cont_Att ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(Cont_Att ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(Cont_Att ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(Cont_Att ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(Cont_Att ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

fit34 =  glm(Cont_Att ~ PrimaryCare, data=dat19, family = "binomial")
summary(fit34)

fit35 = glm(Cont_Att ~ `Primary Care Services with a Primary Care Physician (%)`, data = dat19, family = "binomial")
summary(fit35)

fit36 = glm(Cont_Att ~ `Primary Care Services with a Specialist Physician (%)`, data = dat19, family = "binomial")
summary(fit36)

fit37 = glm(Cont_Att ~ `Primary Care Services with APP (%)`, data = dat19, family = "binomial")
summary(fit37)

##--- Setting up Cont_Att table ---##

r1 = cbind(round(coef(fit1)[2], 5),  round(confint(fit1)[2,1], 5),  round(confint(fit1)[2,2], 5))
r2 = cbind(round(coef(fit2)[2], 5),  round(confint(fit2)[2,1], 5),  round(confint(fit2)[2,2], 5))
r3 = cbind(round(coef(fit3)[2], 5),  round(confint(fit3)[2,1], 5),  round(confint(fit3)[2,2], 5))
r4 = cbind(round(coef(fit4)[2], 5),  round(confint(fit4)[2,1], 5),  round(confint(fit4)[2,2], 5))
r5 = cbind(round(coef(fit5)[2], 5),  round(confint(fit5)[2,1], 5),  round(confint(fit5)[2,2], 5))
r6 = cbind(round(coef(fit6)[2], 5),  round(confint(fit6)[2,1], 5),  round(confint(fit6)[2,2], 5))
r7 = cbind(0,  0,  0)
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
r35 = cbind(round(coef(fit35)[2], 5), round(confint(fit35)[2,1], 5), round(confint(fit35)[2,2], 5))
r36 = cbind(round(coef(fit36)[2], 5), round(confint(fit36)[2,1], 5), round(confint(fit36)[2,2], 5))
r37 = cbind(round(coef(fit37)[2], 5), round(confint(fit37)[2,1], 5), round(confint(fit37)[2,2], 5))


tab.c = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                          exp(r9), exp(r10), exp(r11), exp(r12), exp(r13), exp(r14), 
                          exp(r15), exp(r16), exp(r17), exp(r18),  exp(r19), exp(r35), exp(r36), exp(r37), exp(r20), exp(r21), 
                          exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                          exp(r28), exp(r29), exp(r30), exp(r31), exp(r33), exp(r34a), exp(r34b))

r.names = rownames(tab.c)
tab.c = as_tibble(cbind.data.frame(r.names, tab.c))
colnames(tab.c) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.c

###









##----- outcome: Death -----##
#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(Death ~ Sex, data=sex19)
summary(fit1)

fit2 = glm(Death ~ Age, data=dat19, family = "binomial")
summary(fit2)

fit3 = glm(Death ~ ESRD, data=dat19, family = "binomial")
summary(fit3)

fit4 = glm(Death ~ Dual, data=dat19, family = "binomial")
summary(fit4)

fit5 = glm(Death ~ NonDual, data=dat19, family = "binomial")
summary(fit5)

fit6 = glm(Death ~ Disabled, data=dat19, family = "binomial")
summary(fit6)

fit7 = glm(Death ~ Cont_Att, data=dat19, family = "binomial")
summary(fit7)

#fit8 = glm(Death ~ Death, data=dat19, family = "binomial")
#summary(fit8)

fit9 = glm(Death ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(Death ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)

fit11 = glm(Death ~ log_Avg_3, data=log_Avg19, family = "binomial")
summary(fit11)

fit12 = glm(Death ~ dat19$`Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

fit13 = glm(Death ~ `ED Visits`, data=dat19, family = "binomial")
summary(fit13)

fit14 = glm(Death ~ `UnplannedAdmits`, data=dat19, family = "binomial")
summary(fit14)

fit15 = glm(Death ~ `Readmits`, data=dat19, family = "binomial")
summary(fit15)

fit16 = glm(Death ~ `Total Primary Care Services`, data=dat19, family = "binomial")
summary(fit16)

fit17 = glm(Death ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
summary(fit17)

fit18 = glm(Death ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
summary(fit18)

fit19 = glm(Death ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
summary(fit19)

fit20 = glm(Death ~ `Days in Hospice`, data=dat19, family = "binomial")
summary(fit20)

fit21 = glm(Death ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(Death ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

fit23 = glm(Death ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(Death ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(Death ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(Death ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(Death ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(Death ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(Death ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(Death ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(Death ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(Death ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(Death ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

fit34 =  glm(Death ~ PrimaryCare, data=dat19, family = "binomial")
summary(fit34)

fit35 = glm(Death ~ `Primary Care Services with a Primary Care Physician (%)`, data = dat19, family = "binomial")
summary(fit35)

fit36 = glm(Death ~ `Primary Care Services with a Specialist Physician (%)`, data = dat19, family = "binomial")
summary(fit36)

fit37 = glm(Death ~ `Primary Care Services with APP (%)`, data = dat19, family = "binomial")
summary(fit37)

##--- Setting up Death table ---##

r1 = cbind(round(coef(fit1)[2], 5),  round(confint(fit1)[2,1], 5),  round(confint(fit1)[2,2], 5))
r2 = cbind(round(coef(fit2)[2], 5),  round(confint(fit2)[2,1], 5),  round(confint(fit2)[2,2], 5))
r3 = cbind(round(coef(fit3)[2], 5),  round(confint(fit3)[2,1], 5),  round(confint(fit3)[2,2], 5))
r4 = cbind(round(coef(fit4)[2], 5),  round(confint(fit4)[2,1], 5),  round(confint(fit4)[2,2], 5))
r5 = cbind(round(coef(fit5)[2], 5),  round(confint(fit5)[2,1], 5),  round(confint(fit5)[2,2], 5))
r6 = cbind(round(coef(fit6)[2], 5),  round(confint(fit6)[2,1], 5),  round(confint(fit6)[2,2], 5))
r7 = cbind(round(coef(fit7)[2], 5),  round(confint(fit7)[2,1], 5),  round(confint(fit7)[2,2], 5))
r8 = cbind(0,  0,  0)
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
r35 = cbind(round(coef(fit35)[2], 5), round(confint(fit35)[2,1], 5), round(confint(fit35)[2,2], 5))
r36 = cbind(round(coef(fit36)[2], 5), round(confint(fit36)[2,1], 5), round(confint(fit36)[2,2], 5))
r37 = cbind(round(coef(fit37)[2], 5), round(confint(fit37)[2,1], 5), round(confint(fit37)[2,2], 5))

tab.d = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                         exp(r9), exp(r10), exp(r11), exp(r12), exp(r13), exp(r14), 
                         exp(r15), exp(r16), exp(r17), exp(r18),  exp(r19), exp(r35), exp(r36), exp(r37), exp(r20), exp(r21), 
                         exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                         exp(r28), exp(r29), exp(r30), exp(r31), exp(r33), exp(r34a), exp(r34b))


r.names = rownames(tab.d)
tab.d = as_tibble(cbind.data.frame(r.names, tab.d))
colnames(tab.d) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.d

###




