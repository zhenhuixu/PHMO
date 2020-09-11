##--- PHMO Analysis: Table 6 (Negative Binomial Models) ---##
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


##----- outcome: ED_Visits -----##
#sex, Dual, Cont_Att, Total Eligibility Fraction, MRI Events, HCC_Cancer, HCC_Diabetes, HCC_CKD

#select those with ED Visits >=1
dat19_tab6 <- dat19 %>% filter(`ED Visits`!=0)

#remove Sex=Other (n=1)
#warning: there is no need to fit a positive NBD or the distribution is centered on the value 1
#the confidence interval of sex is too wide to interpret (0.0005-1970)
sex19 = dat19_tab6 %>% filter(Sex!="Other")
fit1 = vglm(`ED Visits` ~ Sex, data=sex19, family = posnegbinomial())
summary(fit1)

#the zero-truncated negative binomial regression
fit2 <- vglm(`ED Visits` ~ Age, family = posnegbinomial(), data = dat19_tab6)
summary(fit2)

fit3 = vglm(`ED Visits` ~ ESRD, data=dat19_tab6, family = posnegbinomial())
summary(fit3)

#warning: solution near the boundary
#confidence interval: 0.0117-198.07
fit4 = vglm(`ED Visits` ~ Dual, data=dat19_tab6, family = posnegbinomial())
summary(fit4)

fit5 = vglm(`ED Visits` ~ NonDual, data=dat19_tab6, family = posnegbinomial())
#summary(fit5)

fit6 = vglm(`ED Visits` ~ Disabled, data=dat19_tab6, family = posnegbinomial())
summary(fit6)

#zero-truncated Poisson regression might be better
#the confidence interval is too wide to interpret
#fit7 = vglm(`ED Visits` ~ Cont_Att, data=dat19_tab6, family = posnegbinomial())
#summary(fit7)

#check the over dispersion assumption
fit7_2 <- vglm(`ED Visits` ~ Cont_Att, data=dat19_tab6, family = pospoisson())
r7_2 = cbind(round(coef(fit7_2)[2], 5),  round(confint(fit7_2)[2,1], 5),  round(confint(fit7_2)[2,2], 5))
#dLL7 <- 2 * (logLik(fit7) - logLik(fit7_2))
#p.logtest <- pchisq(dLL7, df = 1, lower.tail = FALSE)

fit8 = vglm(`ED Visits` ~ Death, data=dat19_tab6, family = posnegbinomial())
#summary(fit8)

fit9 = vglm(`ED Visits` ~ `Total Eligibility Fraction`, data=dat19_tab6, family = posnegbinomial())
#summary(fit9)

fit10 = vglm(`ED Visits` ~ Three_Year_Average, data=dat19_tab6, family = posnegbinomial())
#summary(fit10)

log_Avg19 <- dat19_tab6 %>% filter(!is.infinite(log_Avg_3))
fit11 = vglm(`ED Visits` ~ log_Avg_3, data=log_Avg19, family = posnegbinomial())
summary(fit11)

fit12 = vglm(`ED Visits` ~ `Total Hospital Discharges`, data=dat19_tab6, family = posnegbinomial())
#summary(fit12)

#fit13 = vglm(`ED Visits` ~ `ED Visits`, data=dat19_tab6, family = posnegbinomial())
#summary(fit13)

fit14 = vglm(`ED Visits` ~ UnplannedAdmits, data=dat19_tab6, family = posnegbinomial())
#summary(fit14)

fit15 = vglm(`ED Visits` ~ Readmits, data=dat19_tab6, family = posnegbinomial())
#summary(fit15)

fit16 = vglm(`ED Visits` ~ `Total Primary Care Services`, data=dat19_tab6, family = posnegbinomial())
summary(fit16)

fit17 = vglm(`ED Visits` ~ `Primary Care Services with a Primary Care Physician`, data=dat19_tab6, family = posnegbinomial())
summary(fit17)

fit18 = vglm(`ED Visits` ~ `Primary Care Services with a Specialist Physician`, data=dat19_tab6, family = posnegbinomial())
summary(fit18)

fit19 = vglm(`ED Visits` ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19_tab6, family = posnegbinomial())
summary(fit19)

fit34 = vglm(`ED Visits` ~ `Primary Care Services with a Primary Care Physician (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit34)

fit35 = vglm(`ED Visits` ~ `Primary Care Services with a Specialist Physician (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit35)

fit36 = vglm(`ED Visits` ~ `Primary Care Services with APP (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit36)

fit20 = vglm(`ED Visits` ~ `Days in Hospice`, data=dat19_tab6, family = posnegbinomial())
summary(fit20)

fit21 = vglm(`ED Visits` ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19_tab6, family = posnegbinomial())
summary(fit21)

fit22 = vglm(`ED Visits` ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19_tab6, family = posnegbinomial())
summary(fit22)

fit23 = vglm(`ED Visits` ~ `Computed Tomography (CT) Events`, data=dat19_tab6, family = posnegbinomial())
#summary(fit23)

fit24 = vglm(`ED Visits` ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19_tab6, family = posnegbinomial())
summary(fit24)

fit25 = vglm(`ED Visits` ~ HCC_Cancer, data=dat19_tab6, family = posnegbinomial())
summary(fit25)

fit26 = vglm(`ED Visits` ~ HCC_Diabetes, data=dat19_tab6, family = posnegbinomial())
summary(fit26)

fit27 = vglm(`ED Visits` ~ HCC_CAD, data=dat19_tab6, family = posnegbinomial())
summary(fit27)

fit28 = vglm(`ED Visits` ~ HCC_85, data=dat19_tab6, family = posnegbinomial())
summary(fit28)

fit29 = vglm(`ED Visits` ~ HCC_111, data=dat19_tab6, family = posnegbinomial())
summary(fit29)

fit30 = vglm(`ED Visits` ~ HCC_CKD, data=dat19_tab6, family = posnegbinomial())
summary(fit30)

fit31 = vglm(`ED Visits` ~ HCC, data=dat19_tab6, family = posnegbinomial())
summary(fit31)

#fit32 = vglm(`ED Visits` ~ PrimaryPracticeTIN, data=dat19_tab6, family = posnegbinomial())
#summary(fit32)

fit33 = vglm(`ED Visits` ~ OutNetwork, data=dat19_tab6, family = posnegbinomial())
summary(fit33)

fit37 = vglm(`ED Visits` ~ PrimaryCare, data=dat19_tab6, family = posnegbinomial())
summary(fit37)

r1 = cbind(round(coef(fit1)[3], 5),  round(confint(fit1)[3,1], 5),  round(confint(fit1)[3,2], 5))
r2 = cbind(round(coef(fit2)[3], 5),  round(confint(fit2)[3,1], 5),  round(confint(fit2)[3,2], 5))
r3 = cbind(round(coef(fit3)[3], 5),  round(confint(fit3)[3,1], 5),  round(confint(fit3)[3,2], 5))
r4 = cbind(round(coef(fit4)[3], 5),  round(confint(fit4)[3,1], 5),  round(confint(fit4)[3,2], 5))
r5 = cbind(round(coef(fit5)[3], 5),  round(confint(fit5)[3,1], 5),  round(confint(fit5)[3,2], 5))
r6 = cbind(round(coef(fit6)[3], 5),  round(confint(fit6)[3,1], 5),  round(confint(fit6)[3,2], 5))
#r7 = cbind(round(coef(fit7)[3], 5),  round(confint(fit7)[3,1], 5),  round(confint(fit7)[3,2], 5))
r8 = cbind(round(coef(fit8)[3], 5),  round(confint(fit8)[3,1], 5),  round(confint(fit8)[3,2], 5))
r9 = cbind(round(coef(fit9)[3], 5),  round(confint(fit9)[3,1], 5),  round(confint(fit9)[3,2], 5))
r10 = cbind(round(coef(fit10)[3], 5), round(confint(fit10)[3,1], 5), round(confint(fit10)[3,2], 5))
r11 = cbind(round(coef(fit11)[3], 5), round(confint(fit11)[3,1], 5), round(confint(fit11)[3,2], 5))
r12 = cbind(round(coef(fit12)[3], 5), round(confint(fit12)[3,1], 5), round(confint(fit12)[3,2], 5))
#r13 = cbind(round(coef(fit13)[3], 5), round(confint(fit13)[3,1], 5), round(confint(fit13)[3,2], 5))
r13 = cbind(NA, NA, NA)
row.names(r13) = "`ED Visits`"
r14 = cbind(round(coef(fit14)[3], 5), round(confint(fit14)[3,1], 5), round(confint(fit14)[3,2], 5))
r15 = cbind(round(coef(fit15)[3], 5), round(confint(fit15)[3,1], 5), round(confint(fit15)[3,2], 5))
r16 = cbind(round(coef(fit16)[3], 5), round(confint(fit16)[3,1], 5), round(confint(fit16)[3,2], 5))
r17 = cbind(round(coef(fit17)[3], 5), round(confint(fit17)[3,1], 5), round(confint(fit17)[3,2], 5))
r18 = cbind(round(coef(fit18)[3], 5), round(confint(fit18)[3,1], 5), round(confint(fit18)[3,2], 5))
r19 = cbind(round(coef(fit19)[3], 5), round(confint(fit19)[3,1], 5), round(confint(fit19)[3,2], 5))
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
r34 = cbind(round(coef(fit34)[3], 5), round(confint(fit34)[3,1], 5), round(confint(fit34)[3,2], 5))
r35 = cbind(round(coef(fit35)[3], 5), round(confint(fit35)[3,1], 5), round(confint(fit35)[3,2], 5))
r36 = cbind(round(coef(fit36)[3], 5), round(confint(fit36)[3,1], 5), round(confint(fit36)[3,2], 5))
r37a = cbind(round(coef(fit37)[3], 5), round(confint(fit37)[3,1], 5), round(confint(fit37)[3,2], 5))
r37b = cbind(round(coef(fit37)[4], 5), round(confint(fit37)[4,1], 5), round(confint(fit37)[4,2], 5))

tab.m = rbind.data.frame(r1, r2, r3, r4, r5, r6, r7_2, r8, r9, r10, r11, r12, r13, r14, 
                         r15, r16, r17, r18,  r19, r20, r21, r22, r23, r24, r25, r26, r27,
                         r28, r29, r30, r31, r33, r34, r35, r36, r37a, r37b)

r.names = rownames(tab.m)
tab.m = as_tibble(cbind.data.frame(r.names, tab.m))
colnames(tab.m) = c("Variable", "Beta", "Lower Bound", "Upper Bound")

tab.m


##----- outcome: UnplannedAdmits -----##
#select those with UnplannedAdmits >=1
#6286 observations selected
dat19_tab6 <- dat19 %>% filter(UnplannedAdmits!=0)

#remove Sex=Other (n=1)
#warning: there is no need to fit a positive NBD or the distribution is centered on the value 1
#the confidence interval of sex is too wide to interpret (0.0005-1970)
sex19 = dat19_tab6 %>% filter(Sex!="Other")
fit1 = vglm(UnplannedAdmits ~ Sex, data=sex19, family = posnegbinomial())
summary(fit1)

#the zero-truncated negative binomial regression
fit2 <- vglm(UnplannedAdmits ~ Age, family = posnegbinomial(), data = dat19_tab6)
summary(fit2)

fit3 = vglm(UnplannedAdmits ~ ESRD, data=dat19_tab6, family = posnegbinomial())
summary(fit3)

#warning: solution near the boundary
#confidence interval: 0.0117-198.07
fit4 = vglm(UnplannedAdmits ~ Dual, data=dat19_tab6, family = posnegbinomial())
summary(fit4)

fit5 = vglm(UnplannedAdmits ~ NonDual, data=dat19_tab6, family = posnegbinomial())
summary(fit5)

fit6 = vglm(UnplannedAdmits ~ Disabled, data=dat19_tab6, family = posnegbinomial())
summary(fit6)

#zero-truncated Poisson regression might be better
#the confidence interval is too wide to interpret
fit7 = vglm(UnplannedAdmits ~ Cont_Att, data=dat19_tab6, family = posnegbinomial())
summary(fit7)

#fit7_2 <- vglm(UnplannedAdmits ~ Cont_Att, data=dat19_tab6, family = pospoisson())
#r7_2 = cbind(round(coef(fit7_2)[2], 5),  round(confint(fit7_2)[2,1], 5),  round(confint(fit7_2)[2,2], 5))
#(dLL7 <- 2 * (logLik(fit7) - logLik(fit7_2)))
#pchisq(dLL7, df = 1, lower.tail = FALSE)

fit8 = vglm(UnplannedAdmits ~ Death, data=dat19_tab6, family = posnegbinomial())
summary(fit8)

fit9 = vglm(UnplannedAdmits ~ `Total Eligibility Fraction`, data=dat19_tab6, family = posnegbinomial())
summary(fit9)

fit10 = vglm(UnplannedAdmits ~ Three_Year_Average, data=dat19_tab6, family = posnegbinomial())
summary(fit10)

log_Avg19 <- dat19_tab6 %>% filter(!is.infinite(log_Avg_3))
fit11 = vglm(UnplannedAdmits ~ log_Avg_3, data=log_Avg19, family = posnegbinomial())
summary(fit11)

#not converged
fit12 = vglm(UnplannedAdmits ~ `Total Hospital Discharges`, data=dat19_tab6, family = posnegbinomial())
#summary(fit12)

#not converged
fit13 = vglm(UnplannedAdmits ~ `ED Visits`, data=dat19_tab6, family = posnegbinomial())
#summary(fit13)

#fit14 = vglm(UnplannedAdmits ~ UnplannedAdmits, data=dat19_tab6, family = posnegbinomial())
#summary(fit14)

#not converged
fit15 = vglm(UnplannedAdmits ~ Readmits, data=dat19_tab6, family = posnegbinomial())
#summary(fit15)

fit16 = vglm(UnplannedAdmits ~ `Total Primary Care Services`, data=dat19_tab6, family = posnegbinomial())
summary(fit16)

fit17 = vglm(UnplannedAdmits ~ `Primary Care Services with a Primary Care Physician`, data=dat19_tab6, family = posnegbinomial())
summary(fit17)

fit18 = vglm(UnplannedAdmits ~ `Primary Care Services with a Specialist Physician`, data=dat19_tab6, family = posnegbinomial())
summary(fit18)

fit19 = vglm(UnplannedAdmits ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19_tab6, family = posnegbinomial())
summary(fit19)

fit34 = vglm(UnplannedAdmits ~ `Primary Care Services with a Primary Care Physician (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit34)

fit35 = vglm(UnplannedAdmits ~ `Primary Care Services with a Specialist Physician (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit35)

fit36 = vglm(UnplannedAdmits ~ `Primary Care Services with APP (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit36)

fit20 = vglm(UnplannedAdmits ~ `Days in Hospice`, data=dat19_tab6, family = posnegbinomial())
summary(fit20)

fit21 = vglm(UnplannedAdmits ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19_tab6, family = posnegbinomial())
summary(fit21)

fit22 = vglm(UnplannedAdmits ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19_tab6, family = posnegbinomial())
summary(fit22)

fit23 = vglm(UnplannedAdmits ~ `Computed Tomography (CT) Events`, data=dat19_tab6, family = posnegbinomial())
#summary(fit23)

fit24 = vglm(UnplannedAdmits ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19_tab6, family = posnegbinomial())
summary(fit24)

fit25 = vglm(UnplannedAdmits ~ HCC_Cancer, data=dat19_tab6, family = posnegbinomial())
summary(fit25)

fit26 = vglm(UnplannedAdmits ~ HCC_Diabetes, data=dat19_tab6, family = posnegbinomial())
summary(fit26)

fit27 = vglm(UnplannedAdmits ~ HCC_CAD, data=dat19_tab6, family = posnegbinomial())
summary(fit27)

fit28 = vglm(UnplannedAdmits ~ HCC_85, data=dat19_tab6, family = posnegbinomial())
summary(fit28)

fit29 = vglm(UnplannedAdmits ~ HCC_111, data=dat19_tab6, family = posnegbinomial())
summary(fit29)

fit30 = vglm(UnplannedAdmits ~ HCC_CKD, data=dat19_tab6, family = posnegbinomial())
summary(fit30)

fit31 = vglm(UnplannedAdmits ~ HCC, data=dat19_tab6, family = posnegbinomial())
summary(fit31)

#fit32 = vglm(UnplannedAdmits ~ PrimaryPracticeTIN, data=dat19_tab6, family = posnegbinomial())
#summary(fit32)

fit33 = vglm(UnplannedAdmits ~ OutNetwork, data=dat19_tab6, family = posnegbinomial())
summary(fit33)

fit37 = vglm(UnplannedAdmits ~ PrimaryCare, data=dat19_tab6, family = posnegbinomial())
summary(fit37)

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
#r14 = cbind(round(coef(fit14)[3], 5), round(confint(fit14)[3,1], 5), round(confint(fit14)[3,2], 5))
r14 = cbind(NA, NA, NA)
row.names(r14) = "UnplannedAdmits"
r15 = cbind(round(coef(fit15)[3], 5), round(confint(fit15)[3,1], 5), round(confint(fit15)[3,2], 5))
r16 = cbind(round(coef(fit16)[3], 5), round(confint(fit16)[3,1], 5), round(confint(fit16)[3,2], 5))
r17 = cbind(round(coef(fit17)[3], 5), round(confint(fit17)[3,1], 5), round(confint(fit17)[3,2], 5))
r18 = cbind(round(coef(fit18)[3], 5), round(confint(fit18)[3,1], 5), round(confint(fit18)[3,2], 5))
r19 = cbind(round(coef(fit19)[3], 5), round(confint(fit19)[3,1], 5), round(confint(fit19)[3,2], 5))
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
r34 = cbind(round(coef(fit34)[3], 5), round(confint(fit34)[3,1], 5), round(confint(fit34)[3,2], 5))
r35 = cbind(round(coef(fit35)[3], 5), round(confint(fit35)[3,1], 5), round(confint(fit35)[3,2], 5))
r36 = cbind(round(coef(fit36)[3], 5), round(confint(fit36)[3,1], 5), round(confint(fit36)[3,2], 5))
r37a = cbind(round(coef(fit37)[3], 5), round(confint(fit37)[3,1], 5), round(confint(fit37)[3,2], 5))
r37b = cbind(round(coef(fit37)[4], 5), round(confint(fit37)[4,1], 5), round(confint(fit37)[4,2], 5))

tab.n = rbind.data.frame(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, 
                         r15, r16, r17, r18,  r19, r20, r21, r22, r23, r24, r25, r26, r27,
                         r28, r29, r30, r31, r33, r34, r35, r36, r37a, r37b)

r.names = rownames(tab.n)
tab.n = as_tibble(cbind.data.frame(r.names, tab.n))
colnames(tab.n) = c("Variable", "Beta", "Lower Bound", "Upper Bound")

tab.n

##----- outcome: Readmits -----##
#select those with Readmits >=1
#1195 observations selected
dat19_tab6 <- dat19 %>% filter(Readmits!=0)

#remove Sex=Other (n=1)
#warning: there is no need to fit a positive NBD or the distribution is centered on the value 1
#the confidence interval of sex is too wide to interpret (0.0005-1970)
sex19 = dat19_tab6 %>% filter(Sex!="Other")
fit1 = vglm(Readmits ~ Sex, data=sex19, family = posnegbinomial())
summary(fit1)

#the zero-truncated negative binomial regression
fit2 <- vglm(Readmits ~ Age, family = posnegbinomial(), data = dat19_tab6)
summary(fit2)

fit3 = vglm(Readmits ~ ESRD, data=dat19_tab6, family = posnegbinomial())
summary(fit3)

#warning: solution near the boundary
#confidence interval: 0.0117-198.07
fit4 = vglm(Readmits ~ Dual, data=dat19_tab6, family = posnegbinomial())
summary(fit4)

fit5 = vglm(Readmits ~ NonDual, data=dat19_tab6, family = posnegbinomial())
summary(fit5)

fit6 = vglm(Readmits ~ Disabled, data=dat19_tab6, family = posnegbinomial())
summary(fit6)

#zero-truncated Poisson regression might be better
#the confidence interval is too wide to interpret
fit7 = vglm(Readmits ~ Cont_Att, data=dat19_tab6, family = posnegbinomial())
summary(fit7)

fit8 = vglm(Readmits ~ Death, data=dat19_tab6, family = posnegbinomial())
summary(fit8)

fit9 = vglm(Readmits ~ `Total Eligibility Fraction`, data=dat19_tab6, family = posnegbinomial())
summary(fit9)

fit10 = vglm(Readmits ~ Three_Year_Average, data=dat19_tab6, family = posnegbinomial())
summary(fit10)

log_Avg19 <- dat19_tab6 %>% filter(!is.infinite(log_Avg_3))
fit11 = vglm(Readmits ~ log_Avg_3, data=log_Avg19, family = posnegbinomial())
summary(fit11)

fit12 = vglm(Readmits ~ `Total Hospital Discharges`, data=dat19_tab6, family = posnegbinomial())
summary(fit12)

fit13 = vglm(Readmits ~ `ED Visits`, data=dat19_tab6, family = posnegbinomial())
summary(fit13)

#probably some outliers cause the fitting problem
fit14 = vglm(Readmits ~ UnplannedAdmits, data=dat19_tab6, family = posnegbinomial())
summary(fit14)

#fit15 = vglm(Readmits ~ Readmits, data=dat19_tab6, family = posnegbinomial())
#summary(fit15)

fit16 = vglm(Readmits ~ `Total Primary Care Services`, data=dat19_tab6, family = posnegbinomial())
summary(fit16)

fit17 = vglm(Readmits ~ `Primary Care Services with a Primary Care Physician`, data=dat19_tab6, family = posnegbinomial())
summary(fit17)

fit18 = vglm(Readmits ~ `Primary Care Services with a Specialist Physician`, data=dat19_tab6, family = posnegbinomial())
summary(fit18)

fit19 = vglm(Readmits ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19_tab6, family = posnegbinomial())
summary(fit19)

fit34 = vglm(Readmits ~ `Primary Care Services with a Primary Care Physician (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit34)

fit35 = vglm(Readmits ~ `Primary Care Services with a Specialist Physician (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit35)

fit36 = vglm(Readmits ~ `Primary Care Services with APP (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit36)

fit20 = vglm(Readmits ~ `Days in Hospice`, data=dat19_tab6, family = posnegbinomial())
summary(fit20)

fit21 = vglm(Readmits ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19_tab6, family = posnegbinomial())
summary(fit21)

fit22 = vglm(Readmits ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19_tab6, family = posnegbinomial())
summary(fit22)

fit23 = vglm(Readmits ~ `Computed Tomography (CT) Events`, data=dat19_tab6, family = posnegbinomial())
summary(fit23)

fit24 = vglm(Readmits ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19_tab6, family = posnegbinomial())
summary(fit24)

fit25 = vglm(Readmits ~ HCC_Cancer, data=dat19_tab6, family = posnegbinomial())
summary(fit25)

fit26 = vglm(Readmits ~ HCC_Diabetes, data=dat19_tab6, family = posnegbinomial())
summary(fit26)

fit27 = vglm(Readmits ~ HCC_CAD, data=dat19_tab6, family = posnegbinomial())
summary(fit27)

fit28 = vglm(Readmits ~ HCC_85, data=dat19_tab6, family = posnegbinomial())
summary(fit28)

fit29 = vglm(Readmits ~ HCC_111, data=dat19_tab6, family = posnegbinomial())
summary(fit29)

fit30 = vglm(Readmits ~ HCC_CKD, data=dat19_tab6, family = posnegbinomial())
summary(fit30)

fit31 = vglm(Readmits ~ HCC, data=dat19_tab6, family = posnegbinomial())
summary(fit31)

#fit32 = vglm(Readmits ~ PrimaryPracticeTIN, data=dat19_tab6, family = posnegbinomial())
#summary(fit32)

fit33 = vglm(Readmits ~ OutNetwork, data=dat19_tab6, family = posnegbinomial())
summary(fit33)

fit37 = vglm(Readmits ~ PrimaryCare, data=dat19_tab6, family = posnegbinomial())
summary(fit37)

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
#r14 = cbind(round(coef(fit14)[3], 5), round(confint(fit14)[3,1], 5), round(confint(fit14)[3,2], 5))
r14 = cbind(NA, NA, NA)
row.names(r14) = "UnplannedAdmits"
r15 = cbind(NA, NA, NA)
row.names(r15) = "Readmits"
#r15 = cbind(round(coef(fit15)[3], 5), round(confint(fit15)[3,1], 5), round(confint(fit15)[3,2], 5))
r16 = cbind(round(coef(fit16)[3], 5), round(confint(fit16)[3,1], 5), round(confint(fit16)[3,2], 5))
r17 = cbind(round(coef(fit17)[3], 5), round(confint(fit17)[3,1], 5), round(confint(fit17)[3,2], 5))
r18 = cbind(round(coef(fit18)[3], 5), round(confint(fit18)[3,1], 5), round(confint(fit18)[3,2], 5))
r19 = cbind(round(coef(fit19)[3], 5), round(confint(fit19)[3,1], 5), round(confint(fit19)[3,2], 5))
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
r34 = cbind(round(coef(fit34)[3], 5), round(confint(fit34)[3,1], 5), round(confint(fit34)[3,2], 5))
r35 = cbind(round(coef(fit35)[3], 5), round(confint(fit35)[3,1], 5), round(confint(fit35)[3,2], 5))
r36 = cbind(round(coef(fit36)[3], 5), round(confint(fit36)[3,1], 5), round(confint(fit36)[3,2], 5))
r37a = cbind(round(coef(fit37)[3], 5), round(confint(fit37)[3,1], 5), round(confint(fit37)[3,2], 5))
r37b = cbind(round(coef(fit37)[4], 5), round(confint(fit37)[4,1], 5), round(confint(fit37)[4,2], 5))

tab.o = rbind.data.frame(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, 
                         r15, r16, r17, r18,  r19, r20, r21, r22, r23, r24, r25, r26, r27,
                         r28, r29, r30, r31, r33, r34, r35, r36, r37)

r.names = rownames(tab.o)
tab.o = as_tibble(cbind.data.frame(r.names, tab.o))
colnames(tab.o) = c("Variable", "Beta", "Lower Bound", "Upper Bound")

tab.o

##----- outcome: Days in Hospice -----##
#select those with Days in HOspice >=1
#4511 observations selected
dat19_tab6 <- dat19 %>% filter(`Days in Hospice`!=0)

#remove Sex=Other (n=1)
#warning: there is no need to fit a positive NBD or the distribution is centered on the value 1
#the confidence interval of sex is too wide to interpret (0.0005-1970)
sex19 = dat19_tab6 %>% filter(Sex!="Other")
fit1 = vglm(`Days in Hospice` ~ Sex, data=sex19, family = posnegbinomial())
summary(fit1)

#the zero-truncated negative binomial regression
fit2 <- vglm(`Days in Hospice` ~ Age, family = posnegbinomial(), data = dat19_tab6)
summary(fit2)

fit3 = vglm(`Days in Hospice` ~ ESRD, data=dat19_tab6, family = posnegbinomial())
summary(fit3)

#warning: solution near the boundary
#confidence interval: 0.0117-198.07
fit4 = vglm(`Days in Hospice` ~ Dual, data=dat19_tab6, family = posnegbinomial())
summary(fit4)

fit5 = vglm(`Days in Hospice` ~ NonDual, data=dat19_tab6, family = posnegbinomial())
summary(fit5)

fit6 = vglm(`Days in Hospice` ~ Disabled, data=dat19_tab6, family = posnegbinomial())
summary(fit6)

#zero-truncated Poisson regression might be better
#the confidence interval is too wide to interpret
fit7 = vglm(`Days in Hospice` ~ Cont_Att, data=dat19_tab6, family = posnegbinomial())
summary(fit7)

fit8 = vglm(`Days in Hospice` ~ Death, data=dat19_tab6, family = posnegbinomial())
summary(fit8)

fit9 = vglm(`Days in Hospice` ~ `Total Eligibility Fraction`, data=dat19_tab6, family = posnegbinomial())
summary(fit9)

#fit10 = vglm(`Days in Hospice` ~ Three_Year_Average, data=dat19_tab6, family = posnegbinomial())
#summary(fit10)

log_Avg19 <- dat19_tab6 %>% filter(!is.infinite(log_Avg_3))
fit11 = vglm(`Days in Hospice` ~ log_Avg_3, data=log_Avg19, family = posnegbinomial())
summary(fit11)

fit12 = vglm(`Days in Hospice` ~ `Total Hospital Discharges`, data=dat19_tab6, family = posnegbinomial())
summary(fit12)

fit13 = vglm(`Days in Hospice` ~ `ED Visits`, data=dat19_tab6, family = posnegbinomial())
summary(fit13)

#probably some outliers cause the fitting problem
fit14 = vglm(`Days in Hospice` ~ UnplannedAdmits, data=dat19_tab6, family = posnegbinomial())
summary(fit14)

fit15 = vglm(`Days in Hospice` ~ Readmits, data=dat19_tab6, family = posnegbinomial())
summary(fit15)

fit16 = vglm(`Days in Hospice` ~ `Total Primary Care Services`, data=dat19_tab6, family = posnegbinomial())
summary(fit16)

fit17 = vglm(`Days in Hospice` ~ `Primary Care Services with a Primary Care Physician`, data=dat19_tab6, family = posnegbinomial())
summary(fit17)

fit18 = vglm(`Days in Hospice` ~ `Primary Care Services with a Specialist Physician`, data=dat19_tab6, family = posnegbinomial())
summary(fit18)

fit19 = vglm(`Days in Hospice` ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19_tab6, family = posnegbinomial())
summary(fit19)

fit34 = vglm(`Days in Hospice` ~ `Primary Care Services with a Primary Care Physician (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit34)

fit35 = vglm(`Days in Hospice` ~ `Primary Care Services with a Specialist Physician (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit35)

fit36 = vglm(`Days in Hospice` ~ `Primary Care Services with APP (%)`, data=dat19_tab6, family = posnegbinomial())
summary(fit36)

#fit20 = vglm(`Days in Hospice` ~ `Days in Hospice`, data=dat19_tab6, family = posnegbinomial())
#summary(fit20)

fit21 = vglm(`Days in Hospice` ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19_tab6, family = posnegbinomial())
summary(fit21)

fit22 = vglm(`Days in Hospice` ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19_tab6, family = posnegbinomial())
summary(fit22)

fit23 = vglm(`Days in Hospice` ~ `Computed Tomography (CT) Events`, data=dat19_tab6, family = posnegbinomial())
summary(fit23)

fit24 = vglm(`Days in Hospice` ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19_tab6, family = posnegbinomial())
summary(fit24)

fit25 = vglm(`Days in Hospice` ~ HCC_Cancer, data=dat19_tab6, family = posnegbinomial())
summary(fit25)

fit26 = vglm(`Days in Hospice` ~ HCC_Diabetes, data=dat19_tab6, family = posnegbinomial())
summary(fit26)

fit27 = vglm(`Days in Hospice` ~ HCC_CAD, data=dat19_tab6, family = posnegbinomial())
summary(fit27)

fit28 = vglm(`Days in Hospice` ~ HCC_85, data=dat19_tab6, family = posnegbinomial())
summary(fit28)

fit29 = vglm(`Days in Hospice` ~ HCC_111, data=dat19_tab6, family = posnegbinomial())
summary(fit29)

fit30 = vglm(`Days in Hospice` ~ HCC_CKD, data=dat19_tab6, family = posnegbinomial())
summary(fit30)

fit31 = vglm(`Days in Hospice` ~ HCC, data=dat19_tab6, family = posnegbinomial())
summary(fit31)

#fit32 = vglm(`Days in Hospice` ~ PrimaryPracticeTIN, data=dat19_tab6, family = posnegbinomial())
#summary(fit32)

fit33 = vglm(`Days in Hospice` ~ OutNetwork, data=dat19_tab6, family = posnegbinomial())
summary(fit33)

fit37 = vglm(`Days in Hospice` ~ PrimaryCare, data=dat19_tab6, family = posnegbinomial())
summary(fit37)

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
r16 = cbind(round(coef(fit16)[3], 5), round(confint(fit16)[3,1], 5), round(confint(fit16)[3,2], 5))
r17 = cbind(round(coef(fit17)[3], 5), round(confint(fit17)[3,1], 5), round(confint(fit17)[3,2], 5))
r18 = cbind(round(coef(fit18)[3], 5), round(confint(fit18)[3,1], 5), round(confint(fit18)[3,2], 5))
r19 = cbind(round(coef(fit19)[3], 5), round(confint(fit19)[3,1], 5), round(confint(fit19)[3,2], 5))
#r20 = cbind(round(coef(fit20)[3], 5), round(confint(fit20)[3,1], 5), round(confint(fit20)[3,2], 5))
r20 = cbind(NA, NA, NA)
rownames(r20) = "`Days in Hospice`"
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
r34 = cbind(round(coef(fit34)[3], 5), round(confint(fit34)[3,1], 5), round(confint(fit34)[3,2], 5))
r35 = cbind(round(coef(fit35)[3], 5), round(confint(fit35)[3,1], 5), round(confint(fit35)[3,2], 5))
r36 = cbind(round(coef(fit36)[3], 5), round(confint(fit36)[3,1], 5), round(confint(fit36)[3,2], 5))
r37a = cbind(round(coef(fit37)[3], 5), round(confint(fit37)[3,1], 5), round(confint(fit37)[3,2], 5))
r37b = cbind(round(coef(fit37)[4], 5), round(confint(fit37)[4,1], 5), round(confint(fit37)[4,2], 5))

tab.p = rbind.data.frame(r1, r2, r3, r4, r5, r6, r7, r8, r9, r11, r12, r13, r14, 
                         r15, r16, r17, r18,  r19, r20, r21, r22, r23, r24, r25, r26, r27,
                         r28, r29, r30, r31, r33, r34, r35, r36, r37a, r37b)

r.names = rownames(tab.p)
tab.p = as_tibble(cbind.data.frame(r.names, tab.p))
colnames(tab.p) = c("Variable", "Beta", "Lower Bound", "Upper Bound")

tab.p
