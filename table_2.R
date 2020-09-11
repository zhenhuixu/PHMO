##--- PHMO Analysis: Table 2 ---##
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


##----- outcome: log_Cost_Care -----##
sex19 = dat19 %>% filter(Sex!="Other")
fit1 = lm(log_Cost_Care ~ Sex, data=sex19)
summary(fit1)
  

#plot(dat19$Cost_Care ~ dat19$Age)
fit2 = lm(log_Cost_Care ~ Age, data=dat19)
summary(fit2)
res2 = resid(fit2)
#plot(res2~dat19$Age, ylab="Residuals", xlab="Age", main="Residuals for Age") 
#abline(0, 0)
#one extreme value - removed when PatientKey 421661 removed


fit3 = lm(log_Cost_Care ~ ESRD, data=dat19)
summary(fit3)

fit4 = lm(log_Cost_Care ~ Dual, data=dat19)
summary(fit4)

fit5 = lm(log_Cost_Care ~ NonDual, data=dat19)
summary(fit5)

fit6 = lm(log_Cost_Care ~ Disabled, data=dat19)
summary(fit6)

fit7 = lm(log_Cost_Care ~ Cont_Att, data=dat19)
summary(fit7)

fit8 = lm(log_Cost_Care ~ Death, data=dat19)
summary(fit8)


#plot(dat19$Cost_Care ~ dat19$`Total Eligibility Fraction`)
fit9 = lm(log_Cost_Care ~ `Total Eligibility Fraction`, data=dat19)
summary(fit9)
res9 = resid(fit9)
#plot(res9~dat19$`Total Eligibility Fraction`, ylab="Residuals", xlab="Total Eligibility Fraction", main="Residuals for Total Eligibility Fraction") 
#abline(0, 0)
#one extreme value - removed when PatientKey 421661 removed


#plot(dat19$Cost_Care ~ dat19$Three_Year_Average)
fit10 = lm(log_Cost_Care ~ Three_Year_Average, data=dat19)
summary(fit10)
#res10 = resid(fit10)
#plot(res10~dat19$Three_Year_Average, ylab="Residuals", xlab="Three Year Average", main="Residuals for Three_Year_Average") 
#abline(0, 0)
#one extreme value - removed when PatientKey 421661 removed

log_Avg19 <- dat19 %>% filter(!is.infinite(log_Avg_3))
fit11 = lm(log_Cost_Care ~ log_Avg_3, data=log_Avg19)
summary(fit11)


#splines on most of the following
#par(mfrow = c(1, 1))
#plot(dat19$Cost_Care ~ dat19$`Total Hospital Discharges`, pch=20) 
#fit12 = lm(log_Cost_Care ~ ns(dat19$`Total Hospital Discharges`, knots=c(5)), data=dat19)
#summary(fit12)
fit12 = lm(log_Cost_Care ~ `Total Hospital Discharges`, data=dat19)
summary(fit12)
#better plots
# par(mfrow = c(1, 2))
# plot(fitted(fit12), resid(fit12), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals (2)")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit12), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit12), col = "dodgerblue", lwd = 2)
# 
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Care ~ dat19$`Total Hospital Discharges`, cex=.75, col="gray", pch=20)
# points(fitted(fit12)~dat19$`Total Hospital Discharges`, pch=19, col=2)
##


#plot(dat19$Cost_Care ~ dat19$`ED Visits`)
#fit13 = lm(log_Cost_Care ~ ns(`ED Visits`, knots=c(20)),  data=dat19)
fit13 = lm(log_Cost_Care ~ `ED Visits`,  data=dat19)
# summary(fit13)
# #residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit13), resid(fit13), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit13), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit13), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Care ~ dat19$`ED Visits`, cex=.75, col="gray", pch=20)
# points(fitted(fit13)~dat19$`ED Visits`, pch=19, col=4)


#plot(dat19$Cost_Care ~ dat19$`UnplannedAdmits`)
#fit14 = lm(log_Cost_Care ~ ns(`UnplannedAdmits`, knots=c(5)), data=dat19)
#summary(fit14)
fit14 = lm(log_Cost_Care ~ `UnplannedAdmits`, data=dat19)
# #residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit14), resid(fit14), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit14), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit14), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Care ~ dat19$`UnplannedAdmits`, cex=.75, col="gray", pch=20)
# points(fitted(fit14)~dat19$`UnplannedAdmits`, pch=19, col=2)


#plot(dat19$Cost_Care ~ dat19$`Readmits`)
#fit15 = lm(log_Cost_Care ~ ns(`Readmits`, knots=c(6)), data=dat19)
#summary(fit15)
fit15 = lm(log_Cost_Care ~ `Readmits`, data=dat19)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit15), resid(fit15), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit15), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit15), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Care ~ dat19$`Readmits`, cex=.75, col="gray", pch=20)
# points(fitted(fit15)~dat19$`Readmits`, pch=19, col=3)


#plot(dat19$Cost_Care ~ dat19$`Total Primary Care Services`)
fit16 = lm(log_Cost_Care ~ `Total Primary Care Services`, data=dat19)
# #summary(fit16)
# #residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit16), resid(fit16), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit16), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit16), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Care ~ dat19$`Total Primary Care Services`, cex=.75, col="gray", pch=20)
# points(fitted(fit16)~dat19$`Total Primary Care Services`, pch=19, col=2)




#plot(dat19$Cost_Care ~ dat19$`Primary Care Services with a Primary Care Physician`)
fit17 = lm(log_Cost_Care ~ `Primary Care Services with a Primary Care Physician`, data=dat19)
summary(fit17)

#plot(dat19$Cost_Care ~ dat19$`Primary Care Services with a Specialist Physician`)
fit18 = lm(log_Cost_Care ~ `Primary Care Services with a Specialist Physician`, data=dat19)
summary(fit18)

#plot(dat19$Cost_Care ~ dat19$`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`)
fit19 = lm(log_Cost_Care ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19)
summary(fit19)



#plot(dat19$Cost_Care ~ dat19$`Days in Hospice`)
fit20 = lm(log_Cost_Care ~ `Days in Hospice`, data=dat19)
# summary(fit20)
# #residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit20), resid(fit20), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit20), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit20), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Care ~ dat19$`Days in Hospice`, cex=.75, col="gray", pch=20)
# points(fitted(fit20)~dat19$`Days in Hospice`, pch=19, col=2)


#plot(dat19$Cost_Care ~ dat19$`Skilled Nursing Facility or Unit Discharges`)
fit21 = lm(log_Cost_Care ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19)
# #residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit21), resid(fit21), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit21), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit21), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Care ~ dat19$`Skilled Nursing Facility or Unit Discharges`, cex=.75, col="gray", pch=20)
# points(fitted(fit21)~dat19$`Skilled Nursing Facility or Unit Discharges`, pch=19, col=2)
# 

#plot(dat19$Cost_Care ~ dat19$`Skilled Nursing Facility or Unit Utilization Days`)
fit22 = lm(log_Cost_Care ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit22), resid(fit22), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit22), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit22), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Care ~ dat19$`Skilled Nursing Facility or Unit Utilization Days`, cex=.75, col="gray", pch=20)
# points(fitted(fit22)~dat19$`Skilled Nursing Facility or Unit Utilization Days`, pch=19, col=2)


#plot(dat19$Cost_Care ~ dat19$`Computed Tomography (CT) Events`, data=dat19)
#fit23 = lm(log_Cost_Care ~ ns(`Computed Tomography (CT) Events`, knots=c(10)), data=dat19)
fit23 = lm(log_Cost_Care ~ `Computed Tomography (CT) Events`, data=dat19)
summary(fit23)
# #residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit23), resid(fit23), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit23), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit23), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Care ~ dat19$`Computed Tomography (CT) Events`, cex=.75, col="gray", pch=20)
# points(fitted(fit23)~dat19$`Computed Tomography (CT) Events`, pch=19, col=3)



#plot(dat19$Cost_Care ~ dat19$`Magnetic Resonance Imaging (MRI) Events`)
fit24 = lm(log_Cost_Care ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19)
# #residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit24), resid(fit24), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit24), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit24), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Care ~ dat19$`Magnetic Resonance Imaging (MRI) Events`, cex=.75, col="gray", pch=20)
# points(fitted(fit24)~dat19$`Magnetic Resonance Imaging (MRI) Events`, pch=19, col=2)


fit25 = lm(log_Cost_Care ~ HCC_Cancer, data=dat19)
summary(fit25)

fit26 = lm(log_Cost_Care ~ HCC_Diabetes, data=dat19)
summary(fit26)

fit27 = lm(log_Cost_Care ~ HCC_CAD, data=dat19)
summary(fit27)

fit28 = lm(log_Cost_Care ~ HCC_85, data=dat19)
summary(fit28)

fit29 = lm(log_Cost_Care ~ HCC_111, data=dat19)
summary(fit29)

fit30 = lm(log_Cost_Care ~ HCC_CKD, data=dat19)
summary(fit30)

#plot(dat19$Cost_Care ~ dat19$HCC)
fit31 = lm(log_Cost_Care ~ HCC, data=dat19)
#summary(fit31)
#res31 = resid(fit31)
#plot(res31~dat19$HCC, ylab="Residuals", xlab="HCC Risk Score", main="Residuals for HCC Risk Score") 
#abline(0, 0) #same issues with this one


#fit32 = lm(log_Cost_Care ~ PrimaryPracticeTIN, data=dat19)
#summary(fit32)


#fit33 =  lm(log_Cost_Care ~ ns(OutNetwork, knots=c(500)), data=dat19)
#summary(fit33)
fit33 =  lm(log_Cost_Care ~ OutNetwork, data=dat19)
summary(fit33)
# plot(dat19$log_Cost_Care ~ dat19$OutNetwork, cex=.75, col="gray", pch=20)
# points(fitted(fit33)~dat19$OutNetwork, pch=19, col=2)

fit34 = lm(log_Cost_Care ~ PrimaryCare, data = dat19)
summary(fit34)

fit35 = lm(log_Cost_Care ~ `Primary Care Services with a Primary Care Physician (%)`, data = dat19)
summary(fit35)

fit36 = lm(log_Cost_Care ~ `Primary Care Services with a Specialist Physician (%)`, data = dat19)
summary(fit36)

fit37 = lm(log_Cost_Care ~ `Primary Care Services with APP (%)`, data = dat19)
summary(fit37)

##--- Setting up table ---##

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
#r12a = cbind(round(coef(fit12)[2], 5), round(confint(fit12)[2,1], 5), round(confint(fit12)[2,2], 5))
r12 = cbind(round(coef(fit12)[2], 5), round(confint(fit12)[2,1], 5), round(confint(fit12)[2,2], 5))
#r12b = cbind(round(coef(fit12)[3], 5), round(confint(fit12)[3,1], 5), round(confint(fit12)[3,2], 5))
#r13a = cbind(round(coef(fit13)[2], 5), round(confint(fit13)[2,1], 5), round(confint(fit13)[2,2], 5))
r13 = cbind(round(coef(fit13)[2], 5), round(confint(fit13)[2,1], 5), round(confint(fit13)[2,2], 5))
#r13b = cbind(round(coef(fit13)[3], 5), round(confint(fit13)[3,1], 5), round(confint(fit13)[3,2], 5))
#r14a = cbind(round(coef(fit14)[2], 5), round(confint(fit14)[2,1], 5), round(confint(fit14)[2,2], 5))
r14 = cbind(round(coef(fit14)[2], 5), round(confint(fit14)[2,1], 5), round(confint(fit14)[2,2], 5))
#r14b = cbind(round(coef(fit14)[3], 5), round(confint(fit14)[3,1], 5), round(confint(fit14)[3,2], 5))
#r15a = cbind(round(coef(fit15)[2], 5), round(confint(fit15)[2,1], 5), round(confint(fit15)[2,2], 5))
r15 = cbind(round(coef(fit15)[2], 5), round(confint(fit15)[2,1], 5), round(confint(fit15)[2,2], 5))
#r15b = cbind(round(coef(fit15)[3], 5), round(confint(fit15)[3,1], 5), round(confint(fit15)[3,2], 5))
r16 = cbind(round(coef(fit16)[2], 5), round(confint(fit16)[2,1], 5), round(confint(fit16)[2,2], 5))
r17 = cbind(round(coef(fit17)[2], 5), round(confint(fit17)[2,1], 5), round(confint(fit17)[2,2], 5))
r18 = cbind(round(coef(fit18)[2], 5), round(confint(fit18)[2,1], 5), round(confint(fit18)[2,2], 5))
r19 = cbind(round(coef(fit19)[2], 5), round(confint(fit19)[2,1], 5), round(confint(fit19)[2,2], 5))
r20 = cbind(round(coef(fit20)[2], 5), round(confint(fit20)[2,1], 5), round(confint(fit20)[2,2], 5))
r21 = cbind(round(coef(fit21)[2], 5), round(confint(fit21)[2,1], 5), round(confint(fit21)[2,2], 5))
r22 = cbind(round(coef(fit22)[2], 5), round(confint(fit22)[2,1], 5), round(confint(fit22)[2,2], 5))
#r23a = cbind(round(coef(fit23)[2], 5), round(confint(fit23)[2,1], 5), round(confint(fit23)[2,2], 5))
r23 = cbind(round(coef(fit23)[2], 5), round(confint(fit23)[2,1], 5), round(confint(fit23)[2,2], 5))
#r23b = cbind(round(coef(fit23)[3], 5), round(confint(fit23)[3,1], 5), round(confint(fit23)[3,2], 5))
r24 = cbind(round(coef(fit24)[2], 5), round(confint(fit24)[2,1], 5), round(confint(fit24)[2,2], 5))
r25 = cbind(round(coef(fit25)[2], 5), round(confint(fit25)[2,1], 5), round(confint(fit25)[2,2], 5))
r26 = cbind(round(coef(fit26)[2], 5), round(confint(fit26)[2,1], 5), round(confint(fit26)[2,2], 5))
r27 = cbind(round(coef(fit27)[2], 5), round(confint(fit27)[2,1], 5), round(confint(fit27)[2,2], 5))
r28 = cbind(round(coef(fit28)[2], 5), round(confint(fit28)[2,1], 5), round(confint(fit28)[2,2], 5))
r29 = cbind(round(coef(fit29)[2], 5), round(confint(fit29)[2,1], 5), round(confint(fit29)[2,2], 5))
r30 = cbind(round(coef(fit30)[2], 5), round(confint(fit30)[2,1], 5), round(confint(fit30)[2,2], 5))
r31 = cbind(round(coef(fit31)[2], 5), round(confint(fit31)[2,1], 5), round(confint(fit31)[2,2], 5))
#r32 = cbind(round(coef(fit32)[2], 5), round(confint(fit32)[2,1], 5), round(confint(fit32)[2,2], 5))
#r33a = cbind(round(coef(fit33)[2], 5), round(confint(fit33)[2,1], 5), round(confint(fit33)[2,2], 5))
r33 = cbind(round(coef(fit33)[2], 5), round(confint(fit33)[2,1], 5), round(confint(fit33)[2,2], 5))
#r33b = cbind(round(coef(fit33)[3], 5), round(confint(fit33)[2,1], 5), round(confint(fit33)[2,2], 5))
r34a = cbind(round(coef(fit34)[2], 5), round(confint(fit34)[2,1], 5), round(confint(fit34)[2,2], 5))
r34b = cbind(round(coef(fit34)[3], 5), round(confint(fit34)[3,1], 5), round(confint(fit34)[3,2], 5))
r35 = cbind(round(coef(fit35)[2], 5), round(confint(fit35)[2,1], 5), round(confint(fit35)[2,2], 5))
r36 = cbind(round(coef(fit36)[2], 5), round(confint(fit36)[2,1], 5), round(confint(fit36)[2,2], 5))
r37 = cbind(round(coef(fit37)[2], 5), round(confint(fit37)[2,1], 5), round(confint(fit37)[2,2], 5))

tab.a = rbind.data.frame(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, 
                         r15, r16, r17, r18,  r19, r35, r36, r37, r20, r21, r22, r23, r24, r25, r26, r27,
                         r28, r29, r30, r31, r33, r34a, r34b)

r.names = rownames(tab.a)
tab.a = as_tibble(cbind.data.frame(r.names, tab.a))
colnames(tab.a) = c("Variable", "Beta", "Lower Bound", "Upper Bound")

tab.a

###



























##----- outcome: log_Cost_Eff -----##

#remove rows with missing outcome:
dat19 = dat19 %>%  filter(!(is.na(log_Cost_Eff)))


#models 
sex19 = dat19 %>% filter(Sex!="Other")
fit_1 = lm(log_Cost_Eff ~ Sex, data=sex19)
summary(fit_1)

##plot(dat19$Cost_Care ~ dat19$Age)
fit_2 = lm(log_Cost_Eff ~ Age, data=dat19)
summary(fit_2)
res2 = resid(fit_2)
#plot(res2~dat19$Age, ylab="Residuals", xlab="Age", main="Residuals for Age") 
#abline(0, 0)

fit_3 = lm(log_Cost_Eff ~ ESRD, data=dat19)
summary(fit_3)

fit_4 = lm(log_Cost_Eff ~ Dual, data=dat19)
summary(fit_4)

fit_5 = lm(log_Cost_Eff ~ NonDual, data=dat19)
summary(fit_5)

fit_6 = lm(log_Cost_Eff ~ Disabled, data=dat19)
summary(fit_6)

fit_7 = lm(log_Cost_Eff ~ Cont_Att, data=dat19)
summary(fit_7)

fit_8 = lm(log_Cost_Eff ~ Death, data=dat19)
summary(fit_8)


#plot(dat19$Cost_Care ~ dat19$`Total Eligibility Fraction`)
fit_9 = lm(log_Cost_Eff ~ `Total Eligibility Fraction`, data=dat19)
summary(fit_9)
res9 = resid(fit_9)
#plot(res9~dat19$`Total Eligibility Fraction`, ylab="Residuals", xlab="Total Eligibility Fraction", main="Residuals for Total Eligibility Fraction") 
#abline(0, 0)


#plot(dat19$Cost_Eff ~ dat19$Three_Year_Average)
#remove one extreme data point 
dat_3ya = dat19 %>% filter(PatientKey != 159996)
fit_10 = lm(log_Cost_Eff ~ Three_Year_Average, data=dat_3ya)
#summary(fit_10)
res10 = resid(fit_10)
#plot(res10~dat_3ya$Three_Year_Average, ylab="Residuals", xlab="Three Year Average", main="Residuals for Three_Year_Average") 
#abline(0, 0)

fit_11 = lm(log_Cost_Eff ~ log_Avg_3, data=log_Avg19)
summary(fit_11)



#fit_12 = lm(log_Cost_Eff ~ ns(dat19$`Total Hospital Discharges`, knots=c(5)), data=dat19)
fit_12 = lm(log_Cost_Eff ~ dat19$`Total Hospital Discharges`, data=dat19)
summary(fit_12)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit_12), resid(fit_12), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit_12), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit_12), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Eff ~ dat19$`Total Hospital Discharges`, cex=.75, col="gray", pch=20)
# points(fitted(fit_12)~dat19$`Total Hospital Discharges`, pch=19, col=2)


#fit_13 = lm(log_Cost_Eff ~ ns(`ED Visits`, knots=c(20)), data=dat19)
fit_13 = lm(log_Cost_Eff ~ `ED Visits`, data=dat19)
summary(fit_13)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit_13), resid(fit_13), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit_13), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit_13), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Eff ~ dat19$`ED Visits`, cex=.75, col="gray", pch=20)
# points(fitted(fit_13)~dat19$`ED Visits`, pch=19, col=2)



#fit_14 = lm(log_Cost_Eff ~ ns(`UnplannedAdmits`, knots=c(5)), data=dat19)
fit_14 = lm(log_Cost_Eff ~ `UnplannedAdmits`, data=dat19)
summary(fit_14)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit_14), resid(fit_14), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit_14), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit_14), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Eff ~ dat19$`UnplannedAdmits`, cex=.75, col="gray", pch=20)
# points(fitted(fit_14)~dat19$`UnplannedAdmits`, pch=19, col=2)
# 



#plot(dat19$Cost_Care ~ dat19$`Readmits`)
#fit_15 = lm(log_Cost_Eff ~ ns(`Readmits`, knots=c(6)),data=dat19)
fit_15 = lm(log_Cost_Eff ~ `Readmits`,data=dat19)
summary(fit_15)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit_15), resid(fit_15), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit_15), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit_15), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Eff ~ dat19$`Readmits`, cex=.75, col="gray", pch=20)
# points(fitted(fit_15)~dat19$`Readmits`, pch=19, col=3)
# 



#plot(dat19$Cost_Care ~ dat19$`Total Primary Care Services`)
fit_16 = lm(log_Cost_Eff ~ `Total Primary Care Services`, data=dat19)
summary(fit_16)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit_16), resid(fit_16), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit_16), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit_16), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Eff ~ dat19$`Total Primary Care Services`, cex=.75, col="gray", pch=20)
# points(fitted(fit_16)~dat19$`Total Primary Care Services`, pch=19, col=2)



#plot(dat19$Cost_Care ~ dat19$`Primary Care Services with a Primary Care Physician`)
fit_17 = lm(log_Cost_Eff ~ `Primary Care Services with a Primary Care Physician`, data=dat19)
summary(fit_17)

#plot(dat19$Cost_Care ~ dat19$`Primary Care Services with a Specialist Physician`)
fit_18 = lm(log_Cost_Eff ~ `Primary Care Services with a Specialist Physician`, data=dat19)
summary(fit_18)

#plot(dat19$Cost_Care ~ dat19$`Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`)
fit_19 = lm(log_Cost_Eff ~ `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`, data=dat19)
summary(fit_19)



#plot(dat19$Cost_Care ~ dat19$`Days in Hospice`)
fit_20 = lm(log_Cost_Eff ~ `Days in Hospice`, data=dat19)
summary(fit_20)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit_20), resid(fit_20), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit_20), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit_20), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Eff ~ dat19$`Days in Hospice`, cex=.75, col="gray", pch=20)
# points(fitted(fit_20)~dat19$`Days in Hospice`, pch=19, col=3)
# 


#plot(dat19$Cost_Care ~ dat19$`Skilled Nursing Facility or Unit Discharges`)
fit_21 = lm(log_Cost_Eff ~ `Skilled Nursing Facility or Unit Discharges`, data=dat19)
summary(fit_21)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit_21), resid(fit_21), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit_21), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit_21), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Eff ~ dat19$`Skilled Nursing Facility or Unit Discharges`, cex=.75, col="gray", pch=20)
# points(fitted(fit_21)~dat19$`Skilled Nursing Facility or Unit Discharges`, pch=19, col=1)
# 


#plot(dat19$Cost_Care ~ dat19$`Skilled Nursing Facility or Unit Utilization Days`)
fit_22 = lm(log_Cost_Eff ~ `Skilled Nursing Facility or Unit Utilization Days`, data=dat19)
summary(fit_22)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit_22), resid(fit_22), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit_22), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit_22), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Eff ~ dat19$`Skilled Nursing Facility or Unit Utilization Days`, cex=.75, col="gray", pch=20)
# points(fitted(fit_22)~dat19$`Skilled Nursing Facility or Unit Utilization Days`, pch=19, col=2)



#plot(dat19$Cost_Care ~ dat19$`Computed Tomography (CT) Events`, data=dat19)
#fit_23 = lm(log_Cost_Eff ~ ns(`Computed Tomography (CT) Events`, knots=c(10)), data=dat19)
fit_23 = lm(log_Cost_Eff ~ `Computed Tomography (CT) Events`, data=dat19)
summary(fit_23)
#residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit_23), resid(fit_23), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit_23), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit_23), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Eff ~ dat19$`Computed Tomography (CT) Events`, cex=.75, col="gray", pch=20)
# points(fitted(fit_23)~dat19$`Computed Tomography (CT) Events`, pch=19, col=2)



#plot(dat19$Cost_Care ~ dat19$`Magnetic Resonance Imaging (MRI) Events`)
fit_24 = lm(log_Cost_Eff ~ `Magnetic Resonance Imaging (MRI) Events`, data=dat19)
summary(fit_24)
# #residual plots
# par(mfrow = c(1, 2))
# plot(fitted(fit_24), resid(fit_24), col = "grey", pch = 20,
#      xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
# abline(h = 0, col = "darkorange", lwd = 2)
# qqnorm(resid(fit_24), main = "Normal Q-Q Plot", col = "darkgrey")
# qqline(resid(fit_24), col = "dodgerblue", lwd = 2)
# #fit plot
# par(mfrow = c(1, 1))
# plot(dat19$log_Cost_Eff ~ dat19$`Magnetic Resonance Imaging (MRI) Events`, cex=.75, col="gray", pch=20)
# points(fitted(fit_24)~dat19$`Magnetic Resonance Imaging (MRI) Events`, pch=19, col=2)



fit_25 = lm(log_Cost_Eff ~ HCC_Cancer, data=dat19)
summary(fit_25)


fit_26 = lm(log_Cost_Eff ~ HCC_Diabetes, data=dat19)
summary(fit_26)


fit_27 = lm(log_Cost_Eff ~ HCC_CAD, data=dat19)
summary(fit_27)


fit_28 = lm(log_Cost_Eff ~ HCC_85, data=dat19)
summary(fit_28)


fit_29 = lm(log_Cost_Eff ~ HCC_111, data=dat19)
summary(fit_29)


fit_30 = lm(log_Cost_Eff ~ HCC_CKD, data=dat19)
summary(fit_30)




#plot(dat19$Cost_Care ~ dat19$HCC)
fit_31 = lm(log_Cost_Eff ~ HCC, data=dat19)
summary(fit_31)
res31 = resid(fit_31)
#plot(res31~dat19$HCC, ylab="Residuals", xlab="HCC Risk Score", main="Residuals for HCC Risk Score") 
#abline(0, 0) #same issues with this one


#fit_32 = lm(log_Cost_Eff ~ PrimaryPracticeTIN, data=dat19)
#summary(fit_32)

#fit33 =  lm(log_Cost_Eff ~ ns(OutNetwork, knots=c(500)), data=dat19)
fit_33 =  lm(log_Cost_Eff ~ OutNetwork, data=dat19)
summary(fit_33)
#plot(dat19$log_Cost_Eff ~ dat19$OutNetwork, cex=.75, col="gray", pch=20)
#points(fitted(fit33)~dat19$OutNetwork, pch=19, col=2)

fit_34 = lm(log_Cost_Eff ~ PrimaryCare, data = dat19)
summary(fit_34)

fit_35 = lm(log_Cost_Eff ~ `Primary Care Services with a Primary Care Physician (%)`, data = dat19)
summary(fit_35)

fit_36 = lm(log_Cost_Eff ~ `Primary Care Services with a Specialist Physician (%)`, data = dat19)
summary(fit_36)

fit_37 = lm(log_Cost_Eff ~ `Primary Care Services with APP (%)`, data = dat19)
summary(fit_37)

##--- Setting up table ---##

r1 = cbind(round(coef(fit_1)[2], 5),  round(confint(fit_1)[2,1], 5),  round(confint(fit_1)[2,2], 5))
r2 = cbind(round(coef(fit_2)[2], 5),  round(confint(fit_2)[2,1], 5),  round(confint(fit_2)[2,2], 5))
r3 = cbind(round(coef(fit_3)[2], 5),  round(confint(fit_3)[2,1], 5),  round(confint(fit_3)[2,2], 5))
r4 = cbind(round(coef(fit_4)[2], 5),  round(confint(fit_4)[2,1], 5),  round(confint(fit_4)[2,2], 5))
r5 = cbind(round(coef(fit_5)[2], 5),  round(confint(fit_5)[2,1], 5),  round(confint(fit_5)[2,2], 5))
r6 = cbind(round(coef(fit_6)[2], 5),  round(confint(fit_6)[2,1], 5),  round(confint(fit_6)[2,2], 5))
r7 = cbind(round(coef(fit_7)[2], 5),  round(confint(fit_7)[2,1], 5),  round(confint(fit_7)[2,2], 5))
r8 = cbind(round(coef(fit_8)[2], 5),  round(confint(fit_8)[2,1], 5),  round(confint(fit_8)[2,2], 5))
r9 = cbind(round(coef(fit_9)[2], 5),  round(confint(fit_9)[2,1], 5),  round(confint(fit_9)[2,2], 5))
r10 = cbind(round(coef(fit_10)[2], 5), round(confint(fit_10)[2,1], 5), round(confint(fit_10)[2,2], 5))
r11 = cbind(round(coef(fit_11)[2], 5), round(confint(fit_11)[2,1], 5), round(confint(fit_11)[2,2], 5))
#r12a = cbind(round(coef(fit_12)[2], 5), round(confint(fit_12)[2,1], 5), round(confint(fit_12)[2,2], 5))
r12 = cbind(round(coef(fit_12)[2], 5), round(confint(fit_12)[2,1], 5), round(confint(fit_12)[2,2], 5))
#r12b = cbind(round(coef(fit_12)[3], 5), round(confint(fit_12)[3,1], 5), round(confint(fit_12)[3,2], 5))
#r13a = cbind(round(coef(fit_13)[2], 5), round(confint(fit_13)[2,1], 5), round(confint(fit_13)[2,2], 5))
r13 = cbind(round(coef(fit_13)[2], 5), round(confint(fit_13)[2,1], 5), round(confint(fit_13)[2,2], 5))
#r13b = cbind(round(coef(fit_13)[3], 5), round(confint(fit_13)[3,1], 5), round(confint(fit_13)[3,2], 5))
#r14a = cbind(round(coef(fit_14)[2], 5), round(confint(fit_14)[2,1], 5), round(confint(fit_14)[2,2], 5))
r14 = cbind(round(coef(fit_14)[2], 5), round(confint(fit_14)[2,1], 5), round(confint(fit_14)[2,2], 5))
#r14b = cbind(round(coef(fit_14)[3], 5), round(confint(fit_14)[3,1], 5), round(confint(fit_14)[3,2], 5))
#r15a = cbind(round(coef(fit_15)[2], 5), round(confint(fit_15)[2,1], 5), round(confint(fit_15)[2,2], 5))
r15 = cbind(round(coef(fit_15)[2], 5), round(confint(fit_15)[2,1], 5), round(confint(fit_15)[2,2], 5))
#r15b = cbind(round(coef(fit_15)[3], 5), round(confint(fit_15)[3,1], 5), round(confint(fit_15)[3,2], 5))
r16 = cbind(round(coef(fit_16)[2], 5), round(confint(fit_16)[2,1], 5), round(confint(fit_16)[2,2], 5))
r17 = cbind(round(coef(fit_17)[2], 5), round(confint(fit_17)[2,1], 5), round(confint(fit_17)[2,2], 5))
r18 = cbind(round(coef(fit_18)[2], 5), round(confint(fit_18)[2,1], 5), round(confint(fit_18)[2,2], 5))
r19 = cbind(round(coef(fit_19)[2], 5), round(confint(fit_19)[2,1], 5), round(confint(fit_19)[2,2], 5))
r20 = cbind(round(coef(fit_20)[2], 5), round(confint(fit_20)[2,1], 5), round(confint(fit_20)[2,2], 5))
r21 = cbind(round(coef(fit_21)[2], 5), round(confint(fit_21)[2,1], 5), round(confint(fit_21)[2,2], 5))
r22 = cbind(round(coef(fit_22)[2], 5), round(confint(fit_22)[2,1], 5), round(confint(fit_22)[2,2], 5))
#r23a = cbind(round(coef(fit_23)[2], 5), round(confint(fit_23)[2,1], 5), round(confint(fit_23)[2,2], 5))
r23 = cbind(round(coef(fit_23)[2], 5), round(confint(fit_23)[2,1], 5), round(confint(fit_23)[2,2], 5))
#r23b = cbind(round(coef(fit_23)[3], 5), round(confint(fit_23)[3,1], 5), round(confint(fit_23)[3,2], 5))
r24 = cbind(round(coef(fit_24)[2], 5), round(confint(fit_24)[2,1], 5), round(confint(fit_24)[2,2], 5))
r25 = cbind(round(coef(fit_25)[2], 5), round(confint(fit_25)[2,1], 5), round(confint(fit_25)[2,2], 5))
r26 = cbind(round(coef(fit_26)[2], 5), round(confint(fit_26)[2,1], 5), round(confint(fit_26)[2,2], 5))
r27 = cbind(round(coef(fit_27)[2], 5), round(confint(fit_27)[2,1], 5), round(confint(fit_27)[2,2], 5))
r28 = cbind(round(coef(fit_28)[2], 5), round(confint(fit_28)[2,1], 5), round(confint(fit_28)[2,2], 5))
r29 = cbind(round(coef(fit_29)[2], 5), round(confint(fit_29)[2,1], 5), round(confint(fit_29)[2,2], 5))
r30 = cbind(round(coef(fit_30)[2], 5), round(confint(fit_30)[2,1], 5), round(confint(fit_30)[2,2], 5))
r31 = cbind(round(coef(fit_31)[2], 5), round(confint(fit_31)[2,1], 5), round(confint(fit_31)[2,2], 5))
#r32 = cbind(round(coef(fit_32)[2], 5), round(confint(fit_32)[2,1], 5), round(confint(fit_32)[2,2], 5))
#r33a = cbind(round(coef(fit33)[2], 5), round(confint(fit33)[2,1], 5), round(confint(fit33)[2,2], 5))
r33 = cbind(round(coef(fit_33)[2], 5), round(confint(fit_33)[2,1], 5), round(confint(fit_33)[2,2], 5))
#r33b = cbind(round(coef(fit33)[3], 5), round(confint(fit33)[3,1], 5), round(confint(fit33)[3,2], 5))
r34a = cbind(round(coef(fit_34)[2], 5), round(confint(fit_34)[2,1], 5), round(confint(fit_34)[2,2], 5))
r34b = cbind(round(coef(fit_34)[3], 5), round(confint(fit_34)[3,1], 5), round(confint(fit_34)[3,2], 5))
r35 = cbind(round(coef(fit_35)[2], 5), round(confint(fit_35)[2,1], 5), round(confint(fit_35)[2,2], 5))
r36 = cbind(round(coef(fit_36)[2], 5), round(confint(fit_36)[2,1], 5), round(confint(fit_36)[2,2], 5))
r37 = cbind(round(coef(fit_37)[2], 5), round(confint(fit_37)[2,1], 5), round(confint(fit_37)[2,2], 5))

tab.b = rbind.data.frame(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, 
                         r15, r16, r17, r18,  r19, r35, r36, r37, r20, r21, r22, r23, r24, r25, r26, r27,
                         r28, r29, r30, r31, r33, r34a, r34b)

r.names = rownames(tab.b)
tab.b = as_tibble(cbind.data.frame(r.names, tab.b))
colnames(tab.b) = c("Variable", "Beta", "Lower Bound", "Upper Bound")

tab.b

###








#explanatory variables: 
# Sex
# Age
# ESRD
# Disabled
# Dual
# NonDual
# Cont_Att
# Death
# `Total Eligibility Fraction`
# Three_Year_Average
# log_Avg_3
# `Total Hospital Discharges`
# `ED Visits`
# `UnplannedAdmits`
# `Readmits`  
# `Total Primary Care Services`  
# `Primary Care Services with a Primary Care Physician` 
# `Primary Care Services with a Specialist Physician`  
# `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist` 
# `Days in Hospice`  
# `Skilled Nursing Facility or Unit Discharges`  
# `Skilled Nursing Facility or Unit Utilization Days` 
# `Computed Tomography (CT) Events`  
# `Magnetic Resonance Imaging (MRI) Events` 
# HCC_Cancer
# HCC_Diabetes
# HCC_CAD
# HCC_85 
# HCC_111 
# HCC_CKD 
# HCC
# PrimaryPracticeTIN


