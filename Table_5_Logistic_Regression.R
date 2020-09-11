##--- PHMO Analysis: Table 5 (logistic models) ---##
##--- Zhenhui Xu, May 2020 --------##

rm(list=ls())

library(tidyverse)

##-- Reading in data --##
setwd("E:/Graduate_year_1/PHMO")
dat19 = readRDS("dat_19.rds")

#remove the following PatientKeys - an extreme value (very low cost, impossible days in hospice, >20 MRIs)
dat19 = dat19 %>% filter(PatientKey != 421661,  PatientKey != 359067, PatientKey != 142780)

dat19 <- dat19 %>% mutate(PrimaryCare = replace_na(PrimaryCare, "Missing"))
dat19$PrimaryCare <- as.factor(dat19$PrimaryCare)

#create new variables to represent the percent of different types of primary care services
dat19 <- dat19 %>% 
  mutate(`Primary Care Services with a Primary Care Physician (%)`= `Primary Care Services with a Primary Care Physician`/`Total Primary Care Services`) %>%
  mutate(`Primary Care Services with a Specialist Physician (%)`= `Primary Care Services with a Specialist Physician`/`Total Primary Care Services`) %>%
  mutate(`Primary Care Services with APP (%)` = `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`/`Total Primary Care Services`)

#replace missing values into 0
dat19 <- dat19 %>% 
  mutate(`Primary Care Services with a Primary Care Physician (%)` = replace_na(`Primary Care Services with a Primary Care Physician (%)`,0)) %>%
  mutate(`Primary Care Services with a Specialist Physician (%)` = replace_na(`Primary Care Services with a Specialist Physician (%)`,0)) %>%
  mutate(`Primary Care Services with APP (%)` = replace_na(`Primary Care Services with APP (%)`,0))

##----- outcome: ED_Visits -----##
#create a binary indicator of ED Visits
dat19 <- dat19 %>% mutate(ED_binary = ifelse(`ED Visits`==0, 0, 1))

#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(ED_binary ~ Sex, data=sex19, family = "binomial")
summary(fit1)

fit2 = glm(ED_binary ~ Age, data=dat19, family = "binomial")
summary(fit2)

fit3 = glm(ED_binary ~ ESRD, data=dat19, family = "binomial")
summary(fit3)

fit4 = glm(ED_binary ~ Dual, data=dat19, family = "binomial")
summary(fit4)

#The odds ratio of nondual is 0.458
#Those with nondual have a lower risk of ED visits
fit5 = glm(ED_binary ~ NonDual, data=dat19, family = "binomial")
summary(fit5)

fit6 = glm(ED_binary ~ Disabled, data=dat19, family = "binomial")
summary(fit6)

fit7 = glm(ED_binary ~ Cont_Att, data=dat19, family = "binomial")
summary(fit7)

fit8 = glm(ED_binary ~ Death, data=dat19, family = "binomial")
summary(fit8)

fit9 = glm(ED_binary ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(ED_binary ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)

log_Avg19 <- dat19 %>% filter(!is.infinite(log_Avg_3))
fit11 = glm(ED_binary ~ log_Avg_3, data=log_Avg19, family = "binomial")
summary(fit11)

#some of the fitted probabilities numerically 0 or 1
#probably due to seperation of total hospital discharges
fit12 = glm(ED_binary ~ `Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

#fit13 = glm(ED_binary ~ `ED Visits`, data=dat19, family = "binomial")
#summary(fit13)

fit14 = glm(ED_binary ~ `UnplannedAdmits`, data=dat19, family = "binomial")
summary(fit14)

fit15 = glm(ED_binary ~ `Readmits`, data=dat19, family = "binomial")
summary(fit15)

fit16 = glm(ED_binary ~ `Total Primary Care Services`, data=dat19, family = "binomial")
summary(fit16)

fit17 = glm(ED_binary ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
summary(fit17)

fit18 = glm(ED_binary ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
summary(fit18)

fit19 = glm(ED_binary ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
summary(fit19)

fit34 = glm(ED_binary ~ `Primary Care Services with a Primary Care Physician (%)`, data = dat19, family = "binomial")
summary(fit34)

fit35 = glm(ED_binary ~ `Primary Care Services with a Specialist Physician (%)`, data = dat19, family = "binomial")
summary(fit35)

fit36 = glm(ED_binary ~ `Primary Care Services with APP (%)`, data = dat19, family = "binomial")
summary(fit36)

fit20 = glm(ED_binary ~ `Days in Hospice`, data=dat19, family = "binomial")
summary(fit20)

fit21 = glm(ED_binary ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(ED_binary ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

#models can be fitted
#function confint does not work
fit23 = glm(ED_binary ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(ED_binary ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(ED_binary ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(ED_binary ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(ED_binary ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(ED_binary ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(ED_binary ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(ED_binary ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(ED_binary ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(ED_binary ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(ED_binary ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

dat19 <- dat19 %>% mutate(Primary_Care = ifelse(PrimaryCare=="Primary Care",1,0))

fit37 =  glm(ED_binary ~ PrimaryCare, data=dat19, family = "binomial")
summary(fit37)

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
#r13 = cbind(round(coef(fit13)[2], 5), round(confint(fit13)[2,1], 5), round(confint(fit13)[2,2], 5))
r13 = cbind(NA, NA, NA)
row.names(r13) = "`ED Visits`"
r14 = cbind(round(coef(fit14)[2], 5), round(confint(fit14)[2,1], 5), round(confint(fit14)[2,2], 5))
r15 = cbind(round(coef(fit15)[2], 5), round(confint(fit15)[2,1], 5), round(confint(fit15)[2,2], 5))
r16 = cbind(round(coef(fit16)[2], 5), round(confint(fit16)[2,1], 5), round(confint(fit16)[2,2], 5))
r17 = cbind(round(coef(fit17)[2], 5), round(confint(fit17)[2,1], 5), round(confint(fit17)[2,2], 5))
r18 = cbind(round(coef(fit18)[2], 5), round(confint(fit18)[2,1], 5), round(confint(fit18)[2,2], 5))
r19 = cbind(round(coef(fit19)[2], 5), round(confint(fit19)[2,1], 5), round(confint(fit19)[2,2], 5))
r20 = cbind(round(coef(fit20)[2], 5), round(confint(fit20)[2,1], 5), round(confint(fit20)[2,2], 5))
r21 = cbind(round(coef(fit21)[2], 5), round(confint(fit21)[2,1], 5), round(confint(fit21)[2,2], 5))
r22 = cbind(round(coef(fit22)[2], 5), round(confint(fit22)[2,1], 5), round(confint(fit22)[2,2], 5))
r23 = cbind(round(coef(fit23)[2], 5), round(confint.default(fit23)[2,1], 5), round(confint.default(fit23)[2,2], 5))
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
r34 = cbind(round(coef(fit34)[2], 5), round(confint(fit34)[2,1], 5), round(confint(fit34)[2,2], 5))
r35 = cbind(round(coef(fit35)[2], 5), round(confint(fit35)[2,1], 5), round(confint(fit35)[2,2], 5))
r36 = cbind(round(coef(fit36)[2], 5), round(confint(fit36)[2,1], 5), round(confint(fit36)[2,2], 5))
r37a = cbind(round(coef(fit37)[2], 5), round(confint(fit37)[2,1], 5), round(confint(fit37)[2,2], 5))
r37b = cbind(round(coef(fit37)[3], 5), round(confint(fit37)[3,1], 5), round(confint(fit37)[3,2], 5))

tab.i = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                         exp(r9), exp(r10), exp(r11), exp(r12), r13, exp(r14), 
                         exp(r15), exp(r16), exp(r17), exp(r18),  exp(r19), exp(r20), exp(r21), 
                         exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                         exp(r28), exp(r29), exp(r30), exp(r31), exp(r33), exp(r34), exp(r35), 
                         exp(r36), exp(r37a), exp(r37b))

r.names = rownames(tab.i)
tab.i = as_tibble(cbind.data.frame(r.names, tab.i))
colnames(tab.i) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.i


##----- outcome: UnplannedAdmits -----##
#create a binary indicator of UnplannedAdmits
dat19 <- dat19 %>% mutate(UnplannedAdmits_binary = ifelse(UnplannedAdmits==0, 0, 1))

dat19 %>% group_by(UnplannedAdmits_binary) %>% tally()

#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(UnplannedAdmits_binary ~ Sex, data=sex19, family = "binomial")
summary(fit1)

fit2 = glm(UnplannedAdmits_binary ~ Age, data=dat19, family = "binomial")
summary(fit2)

fit3 = glm(UnplannedAdmits_binary ~ ESRD, data=dat19, family = "binomial")
summary(fit3)

fit4 = glm(UnplannedAdmits_binary ~ Dual, data=dat19, family = "binomial")
summary(fit4)

fit5 = glm(UnplannedAdmits_binary ~ NonDual, data=dat19, family = "binomial")
summary(fit5)

fit6 = glm(UnplannedAdmits_binary ~ Disabled, data=dat19, family = "binomial")
summary(fit6)

fit7 = glm(UnplannedAdmits_binary ~ Cont_Att, data=dat19, family = "binomial")
summary(fit7)

fit8 = glm(UnplannedAdmits_binary ~ Death, data=dat19, family = "binomial")
summary(fit8)

fit9 = glm(UnplannedAdmits_binary ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(UnplannedAdmits_binary ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)

log_Avg19 <- dat19 %>% filter(!is.infinite(log_Avg_3))
fit11 = glm(UnplannedAdmits_binary ~ log_Avg_3, data=log_Avg19, family = "binomial")
summary(fit11)

#plot(dat19$UnplannedAdmits_binary ~ dat19$log_Avg_3, cex=.75, col="gray", pch=20, ylim=c(0, 2))
#points(fitted(fit11)~dat19$log_Avg_3[!is.na(dat19$log_Avg_3)], pch=19, col=2)

fit12 = glm(UnplannedAdmits_binary ~ `Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

#plot(dat19$UnplannedAdmits_binary ~ dat19$`Total Hospital Discharges`, cex=.75, col="gray", pch=20, ylim=c(0, 2))
#points(fitted(fit12)~dat19$`Total Hospital Discharges`, pch=19, col=2)

fit13 = glm(UnplannedAdmits_binary ~ `ED Visits`, data=dat19, family = "binomial")
summary(fit13)

#plot(dat19$UnplannedAdmits_binary ~ dat19$`ED Visits`, cex=.75, col="gray", pch=20, ylim=c(0, 2))
#points(fitted(fit13)~dat19$`ED Visits`, pch=19, col=2)

#fit14 = glm(UnplannedAdmits_binary ~ `UnplannedAdmits`, data=dat19, family = "binomial")
#summary(fit14)

fit15 = glm(UnplannedAdmits_binary ~ `Readmits`, data=dat19, family = "binomial")
summary(fit15)

fit16 = glm(UnplannedAdmits_binary ~ `Total Primary Care Services`, data=dat19, family = "binomial")
summary(fit16)

fit17 = glm(UnplannedAdmits_binary ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
summary(fit17)

fit18 = glm(UnplannedAdmits_binary ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
summary(fit18)

fit19 = glm(UnplannedAdmits_binary ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
summary(fit19)

fit34 = glm(UnplannedAdmits_binary ~ `Primary Care Services with a Primary Care Physician (%)`, data = dat19, family = "binomial")
summary(fit34)

fit35 = glm(UnplannedAdmits_binary ~ `Primary Care Services with a Specialist Physician (%)`, data = dat19, family = "binomial")
summary(fit35)

fit36 = glm(UnplannedAdmits_binary ~ `Primary Care Services with APP (%)`, data = dat19, family = "binomial")
summary(fit36)

fit20 = glm(UnplannedAdmits_binary ~ `Days in Hospice`, data=dat19, family = "binomial")
summary(fit20)

fit21 = glm(UnplannedAdmits_binary ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(UnplannedAdmits_binary ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

fit23 = glm(UnplannedAdmits_binary ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(UnplannedAdmits_binary ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(UnplannedAdmits_binary ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(UnplannedAdmits_binary ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(UnplannedAdmits_binary ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(UnplannedAdmits_binary ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(UnplannedAdmits_binary ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(UnplannedAdmits_binary ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(UnplannedAdmits_binary ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(UnplannedAdmits_binary ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(UnplannedAdmits_binary ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

fit37 =  glm(UnplannedAdmits_binary ~ PrimaryCare, data=dat19, family = "binomial")
summary(fit37)

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
r12 = cbind(round(coef(fit12)[2], 5), round(confint.default(fit12)[2,1], 5), round(confint.default(fit12)[2,2], 5))
r13 = cbind(round(coef(fit13)[2], 5), round(confint.default(fit13)[2,1], 5), round(confint.default(fit13)[2,2], 5))
#r14 = cbind(round(coef(fit14)[2], 5), round(confint(fit14)[2,1], 5), round(confint(fit14)[2,2], 5))
r14 = cbind(NA, NA, NA)
row.names(r14) = "UnplannedAdmits"
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
r34 = cbind(round(coef(fit34)[2], 5), round(confint(fit34)[2,1], 5), round(confint(fit34)[2,2], 5))
r35 = cbind(round(coef(fit35)[2], 5), round(confint(fit35)[2,1], 5), round(confint(fit35)[2,2], 5))
r36 = cbind(round(coef(fit36)[2], 5), round(confint(fit36)[2,1], 5), round(confint(fit36)[2,2], 5))
r37a = cbind(round(coef(fit37)[2], 5), round(confint(fit37)[2,1], 5), round(confint(fit37)[2,2], 5))
r37b = cbind(round(coef(fit37)[3], 5), round(confint(fit37)[3,1], 5), round(confint(fit37)[3,2], 5))

tab.j = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                         exp(r9), exp(r10), exp(r11), exp(r12), exp(r13), r14,
                         exp(r15), exp(r16), exp(r17), exp(r18),  exp(r19), exp(r20), exp(r21), 
                         exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                         exp(r28), exp(r29), exp(r30), exp(r31), exp(r33), exp(r34), exp(r35), exp(r36),
                         exp(r37a), exp(r37b))

r.names = rownames(tab.j)
tab.j = as_tibble(cbind.data.frame(r.names, tab.j))
colnames(tab.j) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.j

##----- outcome: Readmits -----##
#create a binary indicator of Readmissions
dat19 <- dat19 %>% mutate(Readmits_binary = ifelse(Readmits==0, 0, 1))

dat19 %>% group_by(Readmits_binary) %>% tally()

#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(Readmits_binary ~ Sex, data=sex19, family = "binomial")
summary(fit1)

fit2 = glm(Readmits_binary ~ Age, data=dat19, family = "binomial")
summary(fit2)

fit3 = glm(Readmits_binary ~ ESRD, data=dat19, family = "binomial")
summary(fit3)

fit4 = glm(Readmits_binary ~ Dual, data=dat19, family = "binomial")
summary(fit4)

fit5 = glm(Readmits_binary ~ NonDual, data=dat19, family = "binomial")
summary(fit5)

fit6 = glm(Readmits_binary ~ Disabled, data=dat19, family = "binomial")
summary(fit6)

fit7 = glm(Readmits_binary ~ Cont_Att, data=dat19, family = "binomial")
summary(fit7)

fit8 = glm(Readmits_binary ~ Death, data=dat19, family = "binomial")
summary(fit8)

fit9 = glm(Readmits_binary ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(Readmits_binary ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)

log_Avg19 <- dat19 %>% filter(!is.infinite(log_Avg_3))
fit11 = glm(Readmits_binary ~ log_Avg_3, data=log_Avg19, family = "binomial")
summary(fit11)

fit12 = glm(Readmits_binary ~ `Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

fit13 = glm(Readmits_binary ~ `ED Visits`, data=dat19, family = "binomial")
summary(fit13)

#plot(dat19$Readmits_binary ~ dat19$`ED Visits`, cex=.75, col="gray", pch=20, ylim=c(0, 2))
#points(fitted(fit13)~dat19$`ED Visits`, pch=19, col=2)

fit14 = glm(Readmits_binary ~ `UnplannedAdmits`, data=dat19, family = "binomial")
summary(fit14)

#fit15 = glm(Readmits_binary ~ `Readmits`, data=dat19, family = "binomial")
#summary(fit15)

fit16 = glm(Readmits_binary ~ `Total Primary Care Services`, data=dat19, family = "binomial")
summary(fit16)

fit17 = glm(Readmits_binary ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
summary(fit17)

fit18 = glm(Readmits_binary ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
summary(fit18)

fit19 = glm(Readmits_binary ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
summary(fit19)

fit34 = glm(Readmits_binary ~ `Primary Care Services with a Primary Care Physician (%)`, data = dat19, family = "binomial")
summary(fit34)

fit35 = glm(Readmits_binary ~ `Primary Care Services with a Specialist Physician (%)`, data = dat19, family = "binomial")
summary(fit35)

fit36 = glm(Readmits_binary ~ `Primary Care Services with APP (%)`, data = dat19, family = "binomial")
summary(fit36)

fit20 = glm(Readmits_binary ~ `Days in Hospice`, data=dat19, family = "binomial")
summary(fit20)

fit21 = glm(Readmits_binary ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(Readmits_binary ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

fit23 = glm(Readmits_binary ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(Readmits_binary ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(Readmits_binary ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(Readmits_binary ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(Readmits_binary ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(Readmits_binary ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(Readmits_binary ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(Readmits_binary ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(Readmits_binary ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(Readmits_binary ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(Readmits_binary ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

fit37 =  glm(Readmits_binary ~ PrimaryCare, data=dat19, family = "binomial")
summary(fit37)

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
r13 = cbind(round(coef(fit13)[2], 5), round(confint.default(fit13)[2,1], 5), round(confint.default(fit13)[2,2], 5))
r14 = cbind(round(coef(fit14)[2], 5), round(confint(fit14)[2,1], 5), round(confint(fit14)[2,2], 5))
r15 <- cbind(NA, NA, NA)
row.names(r15) = "Readmits"
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
r34 = cbind(round(coef(fit34)[2], 5), round(confint(fit34)[2,1], 5), round(confint(fit34)[2,2], 5))
r35 = cbind(round(coef(fit35)[2], 5), round(confint(fit35)[2,1], 5), round(confint(fit35)[2,2], 5))
r36 = cbind(round(coef(fit36)[2], 5), round(confint(fit36)[2,1], 5), round(confint(fit36)[2,2], 5))
r37a = cbind(round(coef(fit37)[2], 5), round(confint(fit37)[2,1], 5), round(confint(fit37)[2,2], 5))
r37b = cbind(round(coef(fit37)[3], 5), round(confint(fit37)[3,1], 5), round(confint(fit37)[3,2], 5))

tab.k = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                         exp(r9), exp(r10), exp(r11), exp(r12), exp(r13), 
                         exp(r14), r15, exp(r16), exp(r17), exp(r18),  exp(r19), exp(r20), exp(r21), 
                         exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                         exp(r28), exp(r29), exp(r30), exp(r31), exp(r33), exp(r34), exp(r35), exp(r36),
                         exp(r37a), exp(r37b))

r.names = rownames(tab.k)
tab.k = as_tibble(cbind.data.frame(r.names, tab.k))
colnames(tab.k) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.k

##----- outcome: Days in Hospice -----##
#create a binary indicator of Days in Hospice
dat19 <- dat19 %>% mutate(Days_in_Hospice_binary = ifelse(`Days in Hospice`==0, 0, 1))

dat19 %>% group_by(Days_in_Hospice_binary) %>% tally()

#remove Sex=Other (n=1)
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = glm(Days_in_Hospice_binary ~ Sex, data=sex19, family = "binomial")
summary(fit1)

fit2 = glm(Days_in_Hospice_binary ~ Age, data=dat19, family = "binomial")
summary(fit2)

fit3 = glm(Days_in_Hospice_binary ~ ESRD, data=dat19, family = "binomial")
summary(fit3)

fit4 = glm(Days_in_Hospice_binary ~ Dual, data=dat19, family = "binomial")
summary(fit4)

fit5 = glm(Days_in_Hospice_binary ~ NonDual, data=dat19, family = "binomial")
summary(fit5)

fit6 = glm(Days_in_Hospice_binary ~ Disabled, data=dat19, family = "binomial")
summary(fit6)

fit7 = glm(Days_in_Hospice_binary ~ Cont_Att, data=dat19, family = "binomial")
summary(fit7)

fit8 = glm(Days_in_Hospice_binary ~ Death, data=dat19, family = "binomial")
summary(fit8)

fit9 = glm(Days_in_Hospice_binary ~ `Total Eligibility Fraction`, data=dat19, family = "binomial")
summary(fit9)

fit10 = glm(Days_in_Hospice_binary ~ Three_Year_Average, data=dat19, family = "binomial")
summary(fit10)

log_Avg19 <- dat19 %>% filter(!is.infinite(log_Avg_3))
fit11 = glm(Days_in_Hospice_binary ~ log_Avg_3, data=log_Avg19, family = "binomial")
summary(fit11)

fit12 = glm(Days_in_Hospice_binary ~ `Total Hospital Discharges`, data=dat19, family = "binomial")
summary(fit12)

fit13 = glm(Days_in_Hospice_binary ~ `ED Visits`, data=dat19, family = "binomial")
summary(fit13)

#plot(dat19$Readmits_binary ~ dat19$`ED Visits`, cex=.75, col="gray", pch=20, ylim=c(0, 2))
#points(fitted(fit13)~dat19$`ED Visits`, pch=19, col=2)

fit14 = glm(Days_in_Hospice_binary ~ UnplannedAdmits, data=dat19, family = "binomial")
summary(fit14)

fit15 = glm(Days_in_Hospice_binary ~ Readmits, data=dat19, family = "binomial")
summary(fit15)

fit16 = glm(Days_in_Hospice_binary ~ `Total Primary Care Services`, data=dat19, family = "binomial")
summary(fit16)

fit17 = glm(Days_in_Hospice_binary ~ `Primary Care Services with a Primary Care Physician`, data=dat19, family = "binomial")
summary(fit17)

fit18 = glm(Days_in_Hospice_binary ~ `Primary Care Services with a Specialist Physician`, data=dat19, family = "binomial")
summary(fit18)

fit19 = glm(Days_in_Hospice_binary ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19, family = "binomial")
summary(fit19)

fit34 = glm(Days_in_Hospice_binary ~ `Primary Care Services with a Primary Care Physician (%)`, data = dat19, family = "binomial")
summary(fit34)

fit35 = glm(Days_in_Hospice_binary ~ `Primary Care Services with a Specialist Physician (%)`, data = dat19, family = "binomial")
summary(fit35)

fit36 = glm(Days_in_Hospice_binary ~ `Primary Care Services with APP (%)`, data = dat19, family = "binomial")
summary(fit36)

#fit20 = glm(Days_in_Hospice_binary ~ `Days in Hospice`, data=dat19, family = "binomial")
#summary(fit20)

fit21 = glm(Days_in_Hospice_binary ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19, family = "binomial")
summary(fit21)

fit22 = glm(Days_in_Hospice_binary ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19, family = "binomial")
summary(fit22)

fit23 = glm(Days_in_Hospice_binary ~ `Computed Tomography (CT) Events`, data=dat19, family = "binomial")
summary(fit23)

fit24 = glm(Days_in_Hospice_binary ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19, family = "binomial")
summary(fit24)

fit25 = glm(Days_in_Hospice_binary ~ HCC_Cancer, data=dat19, family = "binomial")
summary(fit25)

fit26 = glm(Days_in_Hospice_binary ~ HCC_Diabetes, data=dat19, family = "binomial")
summary(fit26)

fit27 = glm(Days_in_Hospice_binary ~ HCC_CAD, data=dat19, family = "binomial")
summary(fit27)

fit28 = glm(Days_in_Hospice_binary ~ HCC_85, data=dat19, family = "binomial")
summary(fit28)

fit29 = glm(Days_in_Hospice_binary ~ HCC_111, data=dat19, family = "binomial")
summary(fit29)

fit30 = glm(Days_in_Hospice_binary ~ HCC_CKD, data=dat19, family = "binomial")
summary(fit30)

fit31 = glm(Days_in_Hospice_binary ~ HCC, data=dat19, family = "binomial")
summary(fit31)

#fit32 = glm(Readmits_binary ~ PrimaryPracticeTIN, data=dat19, family = "binomial")
#summary(fit32)

fit33 =  glm(Days_in_Hospice_binary ~ OutNetwork, data=dat19, family = "binomial")
summary(fit33)

fit37 =  glm(Days_in_Hospice_binary ~ PrimaryCare, data=dat19, family = "binomial")
summary(fit37)

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
r13 = cbind(round(coef(fit13)[2], 5), round(confint.default(fit13)[2,1], 5), round(confint.default(fit13)[2,2], 5))
r14 = cbind(round(coef(fit14)[2], 5), round(confint(fit14)[2,1], 5), round(confint(fit14)[2,2], 5))
r15 = cbind(round(coef(fit15)[2], 5), round(confint(fit15)[2,1], 5), round(confint(fit15)[2,2], 5))
r16 = cbind(round(coef(fit16)[2], 5), round(confint(fit16)[2,1], 5), round(confint(fit16)[2,2], 5))
r17 = cbind(round(coef(fit17)[2], 5), round(confint(fit17)[2,1], 5), round(confint(fit17)[2,2], 5))
r18 = cbind(round(coef(fit18)[2], 5), round(confint(fit18)[2,1], 5), round(confint(fit18)[2,2], 5))
r19 = cbind(round(coef(fit19)[2], 5), round(confint(fit19)[2,1], 5), round(confint(fit19)[2,2], 5))
#r20 = cbind(round(coef(fit20)[2], 5), round(confint(fit20)[2,1], 5), round(confint(fit20)[2,2], 5))
r20 <- cbind(NA, NA, NA)
row.names(r20) = "Days in Hospice"
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
r34 = cbind(round(coef(fit34)[2], 5), round(confint(fit34)[2,1], 5), round(confint(fit34)[2,2], 5))
r35 = cbind(round(coef(fit35)[2], 5), round(confint(fit35)[2,1], 5), round(confint(fit35)[2,2], 5))
r36 = cbind(round(coef(fit36)[2], 5), round(confint(fit36)[2,1], 5), round(confint(fit36)[2,2], 5))
r37a = cbind(round(coef(fit37)[2], 5), round(confint(fit37)[2,1], 5), round(confint(fit37)[2,2], 5))
r37b = cbind(round(coef(fit37)[3], 5), round(confint(fit37)[3,1], 5), round(confint(fit37)[3,2], 5))

tab.l = rbind.data.frame(exp(r1), exp(r2), exp(r3), exp(r4), exp(r5), exp(r6), exp(r7), exp(r8), 
                         exp(r9), exp(r10), exp(r11), exp(r12), exp(r13), 
                         exp(r14), exp(r15), exp(r16), exp(r17), exp(r18),  exp(r19), exp(r20), exp(r21), 
                         exp(r22), exp(r23), exp(r24), exp(r25), exp(r26), exp(r27),
                         exp(r28), exp(r29), exp(r30), exp(r31), exp(r33), exp(r34), exp(r35), exp(r36),
                         exp(r37a), exp(r37b))

r.names = rownames(tab.l)
tab.l = as_tibble(cbind.data.frame(r.names, tab.l))
colnames(tab.l) = c("Variable", "OR", "Lower Bound", "Upper Bound")

tab.l