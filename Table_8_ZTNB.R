##--- PHMO Analysis: Table 8 (Negative Binomial Models) ---##
##--- Zhenhui Xu, May 2020 --------##

rm(list=ls())

library(tidyverse)
library(MASS)
library(VGAM)

##-- Reading in data --##
setwd("C:/Users/ba124/Box/MSSP CY21 Model")
#setwd("E:/Graduate_year_1/PHMO")
dat19 = readRDS("dat_19.rds")

#remove the following PatientKeys - an extreme value (very low cost, impossible days in hospice, >20 MRIs)
dat19_tab7 = dat19 %>% filter(PatientKey != 421661,  PatientKey != 359067, PatientKey != 142780)

##----- outcome: Primary Care Services: PCP -----##

#dat19_tab7 = dat19_tab7 %>% filter(`Primary Care Services with a Primary Care Physician` > 0)
#why are we removing the 0 here? 

#remove Sex=Other (n=1)
sex19 = dat19_tab7 %>% filter(Sex!="Other")
fit1 = vglm(`Primary Care Services with a Primary Care Physician` ~ Sex, data=sex19, offset = `Total Primary Care Services`,family = posnegbinomial())
summary(fit1)

fit2 = vglm(`Primary Care Services with a Primary Care Physician` ~ Age, data=dat19_tab7, offset = `Total Primary Care Services`,family = posnegbinomial())
summary(fit2)

fit3 = vglm(`Primary Care Services with a Primary Care Physician` ~ ESRD, data=dat19_tab7, offset = `Total Primary Care Services`,family = posnegbinomial())
summary(fit3)

fit4 = vglm(`Primary Care Services with a Primary Care Physician` ~ Dual, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit4)

fit5 = vglm(`Primary Care Services with a Primary Care Physician` ~ NonDual, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit5)

fit6 = vglm(`Primary Care Services with a Primary Care Physician` ~ Disabled, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit6)

fit7 = vglm(`Primary Care Services with a Primary Care Physician` ~ Cont_Att, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit7)

fit8 = vglm(`Primary Care Services with a Primary Care Physician` ~ Death, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit8)

fit9 = vglm(`Primary Care Services with a Primary Care Physician` ~ `Total Eligibility Fraction`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit9)

fit10 = vglm(`Primary Care Services with a Primary Care Physician` ~ Three_Year_Average, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit10)

fit11 = vglm(`Primary Care Services with a Primary Care Physician` ~ log_Avg_3, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit11)

fit12 = vglm(`Primary Care Services with a Primary Care Physician` ~ `Total Hospital Discharges`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit12)




new = dat19 %>% mutate(y=`Primary Care Services with a Primary Care Physician`/`Total Primary Care Services`)
#set all NAs of Y to 0


fit13 = glm(y ~ `ED Visits`, data=new, weights=`Total Primary Care Services`,  family=binomial)

summary(fit13)







fit14 = vglm(`Primary Care Services with a Primary Care Physician` ~ UnplannedAdmits, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit14)

fit15 = vglm(`Primary Care Services with a Primary Care Physician` ~ Readmits, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit15)

fit16 = vglm(`Primary Care Services with a Primary Care Physician` ~ `Days in Hospice`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit16)

fit17 = vglm(`Primary Care Services with a Primary Care Physician` ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit17)

fit18 = vglm(`Primary Care Services with a Primary Care Physician` ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit18)

fit19 = vglm(`Primary Care Services with a Primary Care Physician` ~ `Computed Tomography (CT) Events`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit19)

fit20 = vglm(`Primary Care Services with a Primary Care Physician` ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit20)

fit21 = vglm(`Primary Care Services with a Primary Care Physician` ~ HCC_Cancer, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit21)

fit22 = vglm(`Primary Care Services with a Primary Care Physician` ~ HCC_Diabetes, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit22)

fit23 = vglm(`Primary Care Services with a Primary Care Physician` ~ HCC_CAD, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit23)

fit24 = vglm(`Primary Care Services with a Primary Care Physician` ~ HCC_85, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit24)

fit25 = vglm(`Primary Care Services with a Primary Care Physician` ~ HCC_111, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit25)

fit26 = vglm(`Primary Care Services with a Primary Care Physician` ~ HCC_CKD, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit26)

fit27 = vglm(`Primary Care Services with a Primary Care Physician` ~ HCC, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit27)

fit28 = vglm(`Primary Care Services with a Primary Care Physician` ~ OutNetwork, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit28)

r1 = cbind(round(coef(fit1)[3], 5),  round(confint(fit1)[3,1], 5),  round(confint(fit1)[3,2], 5))
r2 = cbind(round(coef(fit2)[3], 5),  round(confint(fit2)[3,1], 5),  round(confint(fit2)[3,2], 5))
r3 = cbind(round(coef(fit3)[3], 5),  round(confint(fit3)[3,1], 5),  round(confint(fit3)[3,2], 5))
r4 = cbind(round(coef(fit4)[3], 5),  round(confint(fit4)[3,1], 5),  round(confint(fit4)[3,2], 5))
r5 = cbind(round(coef(fit5)[3], 5),  round(confint(fit5)[3,1], 5),  round(confint(fit5)[3,2], 5))
r6 = cbind(round(coef(fit6)[3], 5),  round(confint(fit6)[3,1], 5),  round(confint(fit6)[3,2], 5))
r7 = cbind(round(coef(fit7)[3], 5),  round(confint(fit7)[3,1], 5),  round(confint(fit7)[3,2], 5))
r8 = cbind(round(coef(fit8)[3], 5),  round(confint(fit8)[3,1], 5),  round(confint(fit8)[3,2], 5))
#r9 = cbind(round(coef(fit9)[3], 5),  round(confint(fit9)[3,1], 5),  round(confint(fit9)[3,2], 5))
#r10 = cbind(round(coef(fit10)[3], 5), round(confint(fit10)[3,1], 5), round(confint(fit10)[3,2], 5))
r11 = cbind(round(coef(fit11)[3], 5), round(confint(fit11)[3,1], 5), round(confint(fit11)[3,2], 5))
#r12 = cbind(round(coef(fit12)[3], 5), round(confint(fit12)[3,1], 5), round(confint(fit12)[3,2], 5))
#r13 = cbind(round(coef(fit13)[3], 5), round(confint(fit13)[3,1], 5), round(confint(fit13)[3,2], 5))
#r14 = cbind(round(coef(fit14)[3], 5), round(confint(fit14)[3,1], 5), round(confint(fit14)[3,2], 5))
r15 = cbind(round(coef(fit15)[3], 5), round(confint(fit15)[3,1], 5), round(confint(fit15)[3,2], 5))
r16 = cbind(round(coef(fit16)[3], 5), round(confint(fit16)[3,1], 5), round(confint(fit16)[3,2], 5))
r17 = cbind(round(coef(fit17)[3], 5), round(confint(fit17)[3,1], 5), round(confint(fit17)[3,2], 5))
r18 = cbind(round(coef(fit18)[3], 5), round(confint(fit18)[3,1], 5), round(confint(fit18)[3,2], 5))
#r19 = cbind(round(coef(fit19)[3], 5), round(confint(fit19)[3,1], 5), round(confint(fit19)[3,2], 5))
r20 = cbind(round(coef(fit20)[3], 5), round(confint(fit20)[3,1], 5), round(confint(fit20)[3,2], 5))
r21 = cbind(round(coef(fit21)[3], 5), round(confint(fit21)[3,1], 5), round(confint(fit21)[3,2], 5))
r22 = cbind(round(coef(fit22)[3], 5), round(confint(fit22)[3,1], 5), round(confint(fit22)[3,2], 5))
r23 = cbind(round(coef(fit23)[3], 5), round(confint(fit23)[3,1], 5), round(confint(fit23)[3,2], 5))
r24 = cbind(round(coef(fit24)[3], 5), round(confint(fit24)[3,1], 5), round(confint(fit24)[3,2], 5))
r25 = cbind(round(coef(fit25)[3], 5), round(confint(fit25)[3,1], 5), round(confint(fit25)[3,2], 5))
r26 = cbind(round(coef(fit26)[3], 5), round(confint(fit26)[3,1], 5), round(confint(fit26)[3,2], 5))
r27 = cbind(round(coef(fit27)[3], 5), round(confint(fit27)[3,1], 5), round(confint(fit27)[3,2], 5))
#r28 = cbind(round(coef(fit28)[3], 5), round(confint(fit28)[3,1], 5), round(confint(fit28)[3,2], 5))

tab.q = rbind.data.frame(r1, r2, r3, r4, r5, r6, r7, r8, r11, 
                         r15, r16, r17, r18, r20, r21, r22, r23, r24, r25, r26, r27)

r.names = rownames(tab.q)
tab.q = as_tibble(cbind.data.frame(r.names, tab.q))
colnames(tab.q) = c("Variable", "Beta", "Lower Bound", "Upper Bound")

tab.q














##----- outcome: Primary Care Services: Specialists -----##

#remove the following PatientKeys - an extreme value (very low cost, impossible days in hospice, >20 MRIs)
dat19_tab7 = dat19 %>% filter(PatientKey != 421661,  PatientKey != 359067, PatientKey != 142780)

dat19_tab7 = dat19_tab7 %>% filter(`Primary Care Services with a Specialist Physician` > 0)

#remove Sex=Other (n=1)
sex19 = dat19_tab7 %>% filter(Sex!="Other")
fit1 = vglm(`Primary Care Services with a Specialist Physician` ~ Sex, data=sex19, offset = `Total Primary Care Services`,family = posnegbinomial())
summary(fit1)

fit2 = vglm(`Primary Care Services with a Specialist Physician` ~ Age, data=dat19_tab7, offset = `Total Primary Care Services`,family = posnegbinomial())
summary(fit2)

fit3 = vglm(`Primary Care Services with a Specialist Physician` ~ ESRD, data=dat19_tab7, offset = `Total Primary Care Services`,family = posnegbinomial())
summary(fit3)

fit4 = vglm(`Primary Care Services with a Specialist Physician` ~ Dual, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit4)

fit5 = vglm(`Primary Care Services with a Specialist Physician` ~ NonDual, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit5)

fit6 = vglm(`Primary Care Services with a Specialist Physician` ~ Disabled, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit6)

fit7 = vglm(`Primary Care Services with a Specialist Physician` ~ Cont_Att, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit7)

fit8 = vglm(`Primary Care Services with a Specialist Physician` ~ Death, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit8)

fit9 = vglm(`Primary Care Services with a Specialist Physician` ~ `Total Eligibility Fraction`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit9)

fit10 = vglm(`Primary Care Services with a Specialist Physician` ~ Three_Year_Average, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit10)

fit11 = vglm(`Primary Care Services with a Specialist Physician` ~ log_Avg_3, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit11)

fit12 = vglm(`Primary Care Services with a Specialist Physician` ~ `Total Hospital Discharges`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit12)

fit13 = vglm(`Primary Care Services with a Specialist Physician` ~ `ED Visits`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit13)

fit14 = vglm(`Primary Care Services with a Specialist Physician` ~ UnplannedAdmits, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit14)

fit15 = vglm(`Primary Care Services with a Specialist Physician` ~ Readmits, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit15)

fit16 = vglm(`Primary Care Services with a Specialist Physician` ~ `Days in Hospice`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit16)

fit17 = vglm(`Primary Care Services with a Specialist Physician` ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit17)

fit18 = vglm(`Primary Care Services with a Specialist Physician` ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit18)

fit19 = vglm(`Primary Care Services with a Specialist Physician` ~ `Computed Tomography (CT) Events`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit19)

fit20 = vglm(`Primary Care Services with a Specialist Physician` ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit20)

fit21 = vglm(`Primary Care Services with a Specialist Physician` ~ HCC_Cancer, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit21)

fit22 = vglm(`Primary Care Services with a Specialist Physician` ~ HCC_Diabetes, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit22)

fit23 = vglm(`Primary Care Services with a Specialist Physician` ~ HCC_CAD, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit23)

fit24 = vglm(`Primary Care Services with a Specialist Physician` ~ HCC_85, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit24)

fit25 = vglm(`Primary Care Services with a Specialist Physician` ~ HCC_111, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit25)

fit26 = vglm(`Primary Care Services with a Specialist Physician` ~ HCC_CKD, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit26)

fit27 = vglm(`Primary Care Services with a Specialist Physician` ~ HCC, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit27)

fit28 = vglm(`Primary Care Services with a Specialist Physician` ~ OutNetwork, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit28)

r1 = cbind(round(coef(fit1)[3], 5),  round(confint(fit1)[3,1], 5),  round(confint(fit1)[3,2], 5))
r2 = cbind(round(coef(fit2)[3], 5),  round(confint(fit2)[3,1], 5),  round(confint(fit2)[3,2], 5))
r3 = cbind(round(coef(fit3)[3], 5),  round(confint(fit3)[3,1], 5),  round(confint(fit3)[3,2], 5))
r4 = cbind(round(coef(fit4)[3], 5),  round(confint(fit4)[3,1], 5),  round(confint(fit4)[3,2], 5))
r5 = cbind(round(coef(fit5)[3], 5),  round(confint(fit5)[3,1], 5),  round(confint(fit5)[3,2], 5))
r6 = cbind(round(coef(fit6)[3], 5),  round(confint(fit6)[3,1], 5),  round(confint(fit6)[3,2], 5))
r7 = cbind(round(coef(fit7)[3], 5),  round(confint(fit7)[3,1], 5),  round(confint(fit7)[3,2], 5))
r8 = cbind(round(coef(fit8)[3], 5),  round(confint(fit8)[3,1], 5),  round(confint(fit8)[3,2], 5))
#r9 = cbind(round(coef(fit9)[3], 5),  round(confint(fit9)[3,1], 5),  round(confint(fit9)[3,2], 5))
r10 = cbind(round(coef(fit10)[3], 5), round(confint(fit10)[3,1], 5), round(confint(fit10)[3,2], 5))
r11 = cbind(round(coef(fit11)[3], 5), round(confint(fit11)[3,1], 5), round(confint(fit11)[3,2], 5))
r12 = cbind(round(coef(fit12)[3], 5), round(confint(fit12)[3,1], 5), round(confint(fit12)[3,2], 5))
#r13 = cbind(round(coef(fit13)[3], 5), round(confint(fit13)[3,1], 5), round(confint(fit13)[3,2], 5))
r14 = cbind(round(coef(fit14)[3], 5), round(confint(fit14)[3,1], 5), round(confint(fit14)[3,2], 5))
r15 = cbind(round(coef(fit15)[3], 5), round(confint(fit15)[3,1], 5), round(confint(fit15)[3,2], 5))
r16 = cbind(round(coef(fit16)[3], 5), round(confint(fit16)[3,1], 5), round(confint(fit16)[3,2], 5))
r17 = cbind(round(coef(fit17)[3], 5), round(confint(fit17)[3,1], 5), round(confint(fit17)[3,2], 5))
r18 = cbind(round(coef(fit18)[3], 5), round(confint(fit18)[3,1], 5), round(confint(fit18)[3,2], 5))
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

tab.r = rbind.data.frame(r1, r2, r3, r4, r5, r6, r7, r8, r10, r11, r12, r14,
                         r15, r16, r17, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28)

r.names = rownames(tab.r)
tab.r = as_tibble(cbind.data.frame(r.names, tab.r))
colnames(tab.r) = c("Variable", "Beta", "Lower Bound", "Upper Bound")

tab.r

saveRDS(tab.r, "tabr.rds")

##----- outcome: Primary Care Services: APP -----##

#remove the following PatientKeys - an extreme value (very low cost, impossible days in hospice, >20 MRIs)
dat19_tab7 = dat19 %>% filter(PatientKey != 421661,  PatientKey != 359067, PatientKey != 142780)

dat19_tab7 = dat19_tab7 %>% filter(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`/`Total Primary Care Services` > 0)

#remove Sex=Other (n=1)
sex19 = dat19_tab7 %>% filter(Sex!="Other")
fit1 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ Sex, data=sex19, offset = `Total Primary Care Services`,family = posnegbinomial())
summary(fit1)

fit2 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ Age, data=dat19_tab7, offset = `Total Primary Care Services`,family = posnegbinomial())
summary(fit2)

fit3 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ ESRD, data=dat19_tab7, offset = `Total Primary Care Services`,family = posnegbinomial())
summary(fit3)

fit4 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ Dual, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit4)

fit5 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ NonDual, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit5)

fit6 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ Disabled, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit6)

fit7 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ Cont_Att, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit7)

fit8 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ Death, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit8)

fit9 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ `Total Eligibility Fraction`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit9)

fit10 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ Three_Year_Average, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit10)

fit11 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ log_Avg_3, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit11)

fit12 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ `Total Hospital Discharges`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit12)

fit13 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ `ED Visits`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit13)

fit14 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ UnplannedAdmits, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit14)

fit15 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ Readmits, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit15)

fit15 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ `Days in Hospice`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit15)

fit16 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit16)

fit17 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit17)

fit18 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ `Computed Tomography (CT) Events`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit18)

fit19 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit19)

fit20 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ HCC_Cancer, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit20)

fit21 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ HCC_Diabetes, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit21)

fit22 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ HCC_CAD, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit22)

fit23 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ HCC_85, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit23)

fit24 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ HCC_111, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit24)

fit25 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ HCC_CKD, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit25)

fit26 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ HCC, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit26)

fit27 = vglm(`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` ~ OutNetwork, data=dat19_tab7, offset = `Total Primary Care Services`, family = posnegbinomial())
summary(fit27)

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
r20 = cbind(round(coef(fit20)[3], 5), round(confint(fit20)[3,1], 5), round(confint(fit20)[3,2], 5))
r21 = cbind(round(coef(fit21)[3], 5), round(confint(fit21)[3,1], 5), round(confint(fit21)[3,2], 5))
r22 = cbind(round(coef(fit22)[3], 5), round(confint(fit22)[3,1], 5), round(confint(fit22)[3,2], 5))
r23 = cbind(round(coef(fit23)[3], 5), round(confint(fit23)[3,1], 5), round(confint(fit23)[3,2], 5))
r24 = cbind(round(coef(fit24)[3], 5), round(confint(fit24)[3,1], 5), round(confint(fit24)[3,2], 5))
r25 = cbind(round(coef(fit25)[3], 5), round(confint(fit25)[3,1], 5), round(confint(fit25)[3,2], 5))
r26 = cbind(round(coef(fit26)[3], 5), round(confint(fit26)[3,1], 5), round(confint(fit26)[3,2], 5))
r27 = cbind(round(coef(fit27)[3], 5), round(confint(fit27)[3,1], 5), round(confint(fit27)[3,2], 5))

tab.s = rbind.data.frame(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14,
                         r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28)

r.names = rownames(tab.s)
tab.s = as_tibble(cbind.data.frame(r.names, tab.s))
colnames(tab.s) = c("Variable", "Beta", "Lower Bound", "Upper Bound")

tab.s

saveRDS(tab.s, "tabs.rds")
