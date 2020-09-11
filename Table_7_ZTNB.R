##--- PHMO Analysis: Table 7 (Negative Binomial Models) ---##
##--- Zhenhui Xu, May 2020 --------##

rm(list=ls())

library(tidyverse)
library(MASS)
library(VGAM)

##-- Reading in data --##
setwd("E:/Graduate_year_1/PHMO")
dat19 = readRDS("dat_19.rds")

#remove the following PatientKeys - an extreme value (very low cost, impossible days in hospice, >20 MRIs)
dat19 = dat19 %>% filter(PatientKey != 421661,  PatientKey != 359067, PatientKey != 142780)

dat19 <- dat19 %>% 
  mutate(`Primary Care Services with a Primary Care Physician (%)`= `Primary Care Services with a Primary Care Physician`/`Total Primary Care Services`) %>%
  mutate(`Primary Care Services with a Specialist Physician (%)`= `Primary Care Services with a Specialist Physician`/`Total Primary Care Services`) %>%
  mutate(`Primary Care Services with APP (%)` = `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`/`Total Primary Care Services`)

##----- outcome: Total Primary Care Services -----##

#select those with Total Primary Care Services >=1
dat19_tab7 <- dat19 %>% filter(`Total Primary Care Services`!=0)

#remove Sex=Other (n=1)
sex19 = dat19_tab7 %>% filter(Sex!="Other")
fit1 = vglm(`Total Primary Care Services` ~ Sex, data=sex19, family = posnegbinomial())
summary(fit1)

fit2 <- vglm(`Total Primary Care Services` ~ Age, family = posnegbinomial(), data = dat19_tab7)
summary(fit2)

fit3 = vglm(`Total Primary Care Services` ~ ESRD, data=dat19_tab7, family = posnegbinomial())
summary(fit3)

fit4 = vglm(`Total Primary Care Services` ~ Dual, data=dat19_tab7, family = posnegbinomial())
summary(fit4)

fit5 = vglm(`Total Primary Care Services` ~ NonDual, data=dat19_tab7, family = posnegbinomial())
summary(fit5)

fit6 = vglm(`Total Primary Care Services` ~ Disabled, data=dat19_tab7, family = posnegbinomial())
summary(fit6)

fit7 = vglm(`Total Primary Care Services` ~ Cont_Att, data=dat19_tab7, family = posnegbinomial())
summary(fit7)

#check the over dispersion assumption
#fit7_2 <- vglm(`Total Primary Care Services` ~ Cont_Att, data=dat19_tab7, family = pospoisson())
#r7_2 = cbind(round(coef(fit7_2)[2], 5),  round(confint(fit7_2)[2,1], 5),  round(confint(fit7_2)[2,2], 5))
#dLL7 <- 2 * (logLik(fit7) - logLik(fit7_2))
#p.logtest <- pchisq(dLL7, df = 1, lower.tail = FALSE)

fit8 = vglm(`Total Primary Care Services` ~ Death, data=dat19_tab7, family = posnegbinomial())
summary(fit8)

fit9 = vglm(`Total Primary Care Services` ~ `Total Eligibility Fraction`, data=dat19_tab7, family = posnegbinomial())
summary(fit9)

fit10 = vglm(`Total Primary Care Services` ~ Three_Year_Average, data=dat19_tab7, family = posnegbinomial())
summary(fit10)

fit11 = vglm(`Total Primary Care Services` ~ log_Avg_3, data=dat19_tab7, family = posnegbinomial())
summary(fit11)

fit12 = vglm(`Total Primary Care Services` ~ `Total Hospital Discharges`, data=dat19_tab7, family = posnegbinomial())
summary(fit12)

fit13 = vglm(`Total Primary Care Services` ~ `ED Visits`, data=dat19_tab7, family = posnegbinomial())
summary(fit13)

fit14 = vglm(`Total Primary Care Services` ~ UnplannedAdmits, data=dat19_tab7, family = posnegbinomial())
summary(fit14)

fit15 = vglm(`Total Primary Care Services` ~ Readmits, data=dat19_tab7, family = posnegbinomial())
summary(fit15)

#fit16 = vglm(`Total Primary Care Services` ~ `Total Primary Care Services`, data=dat19_tab7, family = posnegbinomial())
#summary(fit16)

#fit17 = vglm(`Total Primary Care Services` ~ `Primary Care Services with a Primary Care Physician`, data=dat19_tab7, family = posnegbinomial())
#summary(fit17)

#fit18 = vglm(`Total Primary Care Services` ~ `Primary Care Services with a Specialist Physician`, data=dat19_tab7, family = posnegbinomial())
#summary(fit18)

#fit19 = vglm(`Total Primary Care Services` ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19_tab7, family = posnegbinomial())
#summary(fit19)

#fit34 = vglm(`Total Primary Care Services` ~ `Primary Care Services with a Primary Care Physician (%)`, data=dat19_tab7, family = posnegbinomial())
#summary(fit34)

#fit35 = vglm(`Total Primary Care Services` ~ `Primary Care Services with a Specialist Physician (%)`, data=dat19_tab7, family = posnegbinomial())
#summary(fit35)

#fit36 = vglm(`Total Primary Care Services` ~ `Primary Care Services with APP (%)`, data=dat19_tab7, family = posnegbinomial())
#summary(fit36)

fit20 = vglm(`Total Primary Care Services` ~ `Days in Hospice`, data=dat19_tab7, family = posnegbinomial())
summary(fit20)

fit21 = vglm(`Total Primary Care Services` ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19_tab7, family = posnegbinomial())
summary(fit21)

fit22 = vglm(`Total Primary Care Services` ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19_tab7, family = posnegbinomial())
summary(fit22)

fit23 = vglm(`Total Primary Care Services` ~ `Computed Tomography (CT) Events`, data=dat19_tab7, family = posnegbinomial())
summary(fit23)

fit24 = vglm(`Total Primary Care Services` ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19_tab7, family = posnegbinomial())
summary(fit24)

fit25 = vglm(`Total Primary Care Services` ~ HCC_Cancer, data=dat19_tab7, family = posnegbinomial())
summary(fit25)

fit26 = vglm(`Total Primary Care Services` ~ HCC_Diabetes, data=dat19_tab7, family = posnegbinomial())
summary(fit26)

fit27 = vglm(`Total Primary Care Services` ~ HCC_CAD, data=dat19_tab7, family = posnegbinomial())
summary(fit27)

fit28 = vglm(`Total Primary Care Services` ~ HCC_85, data=dat19_tab7, family = posnegbinomial())
summary(fit28)

fit29 = vglm(`Total Primary Care Services` ~ HCC_111, data=dat19_tab7, family = posnegbinomial())
summary(fit29)

fit30 = vglm(`Total Primary Care Services` ~ HCC_CKD, data=dat19_tab7, family = posnegbinomial())
summary(fit30)

fit31 = vglm(`Total Primary Care Services` ~ HCC, data=dat19_tab7, family = posnegbinomial())
summary(fit31)

#fit32 = vglm(`Total Primary Care Services` ~ PrimaryPracticeTIN, data=dat19_tab7, family = posnegbinomial())
#summary(fit32)

fit33 = vglm(`Total Primary Care Services` ~ OutNetwork, data=dat19_tab7, family = posnegbinomial())
summary(fit33)

r1 = cbind(round(coef(fit1)[3], 5),  round(confint(fit1)[3,1], 5),  round(confint(fit1)[3,2], 5))
r2 = cbind(round(coef(fit2)[3], 5),  round(confint(fit2)[3,1], 5),  round(confint(fit2)[3,2], 5))
r3 = cbind(round(coef(fit3)[3], 5),  round(confint(fit3)[3,1], 5),  round(confint(fit3)[3,2], 5))
r4 = cbind(round(coef(fit4)[3], 5),  round(confint(fit4)[3,1], 5),  round(confint(fit4)[3,2], 5))
r5 = cbind(round(coef(fit5)[3], 5),  round(confint(fit5)[3,1], 5),  round(confint(fit5)[3,2], 5))
r6 = cbind(round(coef(fit6)[3], 5),  round(confint(fit6)[3,1], 5),  round(confint(fit6)[3,2], 5))
r7 = cbind(round(coef(fit7)[3], 5),  round(confint(fit7)[3,1], 5),  round(confint(fit7)[3,2], 5))
r8 = cbind(round(coef(fit8)[3], 5),  round(confint(fit8)[3,1], 5),  round(confint(fit8)[3,2], 5))
r9 = cbind(round(coef(fit9)[3], 5),  round(confint(fit9)[3,1], 5),  round(confint(fit9)[3,2], 5))
r10 = cbind(round(coef(fit10)[3], 5), round(confint(fit10)[3,1], 5), round(confint(fit10)[3,2], 5))
r11 = cbind(round(coef(fit11)[3], 5), round(confint(fit11)[3,1], 5), round(confint(fit11)[3,2], 5))
r12 = cbind(round(coef(fit12)[3], 5), round(confint(fit12)[3,1], 5), round(confint(fit12)[3,2], 5))
r13 = cbind(round(coef(fit13)[3], 5), round(confint(fit13)[3,1], 5), round(confint(fit13)[3,2], 5))
r14 = cbind(round(coef(fit14)[3], 5), round(confint(fit14)[3,1], 5), round(confint(fit14)[3,2], 5))
r15 = cbind(round(coef(fit15)[3], 5), round(confint(fit15)[3,1], 5), round(confint(fit15)[3,2], 5))
#r16 = cbind(round(coef(fit16)[3], 5), round(confint(fit16)[3,1], 5), round(confint(fit16)[3,2], 5))
#r17 = cbind(round(coef(fit17)[3], 5), round(confint(fit17)[3,1], 5), round(confint(fit17)[3,2], 5))
#r18 = cbind(round(coef(fit18)[3], 5), round(confint(fit18)[3,1], 5), round(confint(fit18)[3,2], 5))
#r19 = cbind(round(coef(fit19)[3], 5), round(confint(fit19)[3,1], 5), round(confint(fit19)[3,2], 5))
r20 = cbind(round(coef(fit20)[3], 5), round(confint(fit20)[3,1], 5), round(confint(fit20)[3,2], 5))
r21 = cbind(round(coef(fit21)[3], 5), round(confint(fit21)[3,1], 5), round(confint(fit21)[3,2], 5))
r22 = cbind(round(coef(fit22)[3], 5), round(confint(fit22)[3,1], 5), round(confint(fit22)[3,2], 5))
r23 = cbind(round(coef(fit23)[3], 5), round(confint(fit23)[3,1], 5), round(confint(fit23)[3,2], 5))
r24 = cbind(round(coef(fit24)[3], 5), round(confint(fit24)[3,1], 5), round(confint(fit24)[3,2], 5))
r25 = cbind(round(coef(fit25)[3], 5), round(confint(fit25)[3,1], 5), round(confint(fit25)[3,2], 5))
r26 = cbind(round(coef(fit26)[3], 5), round(confint(fit26)[3,1], 5), round(confint(fit26)[3,2], 5))
r27 = cbind(round(coef(fit27)[3], 5), round(confint(fit27)[3,1], 5), round(confint(fit27)[3,2], 5))
r28 = cbind(round(coef(fit28)[3], 5), round(confint(fit28)[3,1], 5), round(confint(fit28)[3,2], 5))
r29 = cbind(round(coef(fit29)[3], 5), round(confint(fit29)[3,1], 5), round(confint(fit29)[3,2], 5))
r30 = cbind(round(coef(fit30)[3], 5), round(confint(fit30)[3,1], 5), round(confint(fit30)[3,2], 5))
r31 = cbind(round(coef(fit31)[3], 5), round(confint(fit31)[3,1], 5), round(confint(fit31)[3,2], 5))
#r32 = cbind(round(coef(fit32)[3], 5), round(confint(fit32)[3,1], 5), round(confint(fit32)[3,2], 5))
r33 = cbind(round(coef(fit33)[3], 5), round(confint(fit33)[3,1], 5), round(confint(fit33)[3,2], 5))
#r34 = cbind(round(coef(fit34)[3], 5), round(confint(fit34)[3,1], 5), round(confint(fit34)[3,2], 5))
#r35 = cbind(round(coef(fit35)[3], 5), round(confint(fit35)[3,1], 5), round(confint(fit35)[3,2], 5))
#r36 = cbind(round(coef(fit36)[3], 5), round(confint(fit36)[3,1], 5), round(confint(fit36)[3,2], 5))

tab.p = rbind.data.frame(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, 
                         r15, r20, r21, r22, r23, r24, r25, r26, r27,
                         r28, r29, r30, r31, r33)

r.names = rownames(tab.p)
tab.p = as_tibble(cbind.data.frame(r.names, tab.p))
colnames(tab.p) = c("Variable", "Beta", "Lower Bound", "Upper Bound")

tab.p