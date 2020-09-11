#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(broom)
library(GGally)
library(shinythemes)
library(kableExtra)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  dat19 <- readRDS("Data/dat_19.rds")
  #remove the following PatientKeys - an extreme value (very low cost, impossible days in hospice, >20 MRIs)
  dat19 = dat19 %>% filter(PatientKey != 421661,  PatientKey != 359067, PatientKey != 142780)
  dat19 = dat19 %>% filter(Sex!="Other")
  
  dat19 <- dat19 %>% mutate(PrimaryCare = replace_na(PrimaryCare, "Missing"))
  dat19$PrimaryCare <- factor(dat19$PrimaryCare, levels=c("Primary Care", "Specialist", "Missing"))
  
  #create percentages of primary care services
  dat19 <- dat19 %>% 
    mutate(`Primary Care Services with a Primary Care Physician (%)`= `Primary Care Services with a Primary Care Physician`/`Total Primary Care Services`) %>%
    mutate(`Primary Care Services with a Specialist Physician (%)`= `Primary Care Services with a Specialist Physician`/`Total Primary Care Services`) %>%
    mutate(`Primary Care Services with APP (%)` = `Primary Care Services with a Nurse Practitioner/Physician Assistant/Clinical Nurse Specialist`/`Total Primary Care Services`)
  
  #replace missing values into 0
  dat19 <- dat19 %>% 
    mutate(`Primary Care Services with a Primary Care Physician (%)` = replace_na(`Primary Care Services with a Primary Care Physician (%)`,0)) %>%
    mutate(`Primary Care Services with a Specialist Physician (%)` = replace_na(`Primary Care Services with a Specialist Physician (%)`,0)) %>%
    mutate(`Primary Care Services with APP (%)` = replace_na(`Primary Care Services with APP (%)`,0))
  
  #replace NA of CHF and COPD with zero
  dat19 <- dat19 %>%
    mutate(HCC_85 = replace_na(HCC_85, 0)) %>%
    mutate(HCC_111 = replace_na(HCC_111, 0))
  
  #merge with correct TINs number
  TINs <- read.csv("Data/Practice_TIN_map.csv")
  TINs <- TINs %>% rename(PrimaryPractice = PracticeName) %>%
    rename(PrimaryPracticeTIN_mapping = PrimaryPracticeTIN)
  dat19 <- dat19 %>% left_join(TINs, by="PrimaryPractice")
  
  #rename null values in Primary Practice to Other
  dat19$TaxEntityName <- as.character(dat19$TaxEntityName)
  dat19 <- dat19 %>% mutate(TaxEntityName = replace_na(TaxEntityName, "Other"))
  dat19$TaxEntityName[dat19$TaxEntityName=="DUKE UNIVERSITY HOSPITAL"]="DUKE UNIVERSITY HEALTH SYSTEMS INC"
  dat19$TaxEntityName <- as.factor(dat19$TaxEntityName)
  
  com_Replace <- function(x){
    x <- gsub("HCC_","",x)
    x <- gsub("85","CHF", x)
    x <- gsub("111","COPD", x)
    return(x)
  }
  
  var_Replace <- function(x){
    x <- gsub("`","",x)
    x <- gsub("UnplannedAdmits","Unplanned Admissions",x)
    x <- gsub("Readmits","Readmissions",x)
  }
  
  output$heat <- renderPlotly({
    fit_var <- c("Total Hospital Discharges", "Unplanned Admissions", "Readmissions", "ED Visits",
                                        "Computed Tomography (CT) Events", "Magnetic Resonance Imaging (MRI) Events",
                                        "Days in Hospice", "Total Primary Care Services", "Primary Care Services with a Primary Care Physician",
                                        "Skilled Nursing Facility or Unit Discharges")
    fit_out <- c("Log(Cost of Care)", "Log(Cost Efficiency)")
    CC_coef <- c(0.1588, 0.1459, 0.1630, 0.0345, 0.0380, 0.0515, 0.0019, 0.0087, -0.0034, 0.1118)
    CE_coef <- c(0.6021, 0.5533, 0.3249, 0.1632, 0.1834, 0.3419, 0.0073, 0.0713, -0.2087, 0.1326)
    
    mat = t(matrix(c(CC_coef, CE_coef), nrow = 10, ncol = 2))
    conditions.text <- as.vector(outer(paste0("Outcome variable:",fit_out,"\n"), paste0("Hospital Utilizations:",fit_var,"\n"), paste0))
    fit_num <- c(0.1588, 0.6021, 0.1459, 0.5533, 0.1630, 0.3249, 0.0345, 0.1632, 0.0380, 0.1834,
                 0.0515, 0.3419, 0.0019, 0.0073, 0.0087, 0.0713, -0.0034, -0.2087, 0.1118, 0.1326)
    conditions.text <- paste0(conditions.text,"Magnitude of Association:",fit_num)
    text.mat <- matrix(conditions.text, nrow= nrow(mat), ncol = ncol(mat))
    
    colorlength <- 100
    
    null_value <- (0 - min(fit_num)) / (max(fit_num) - min(fit_num))        
    border <- as.integer(null_value * colorlength)
    colorscale <- as.list(1:colorlength)
    
    #colorscale below zero
    s <- scales::seq_gradient_pal("blue", "white", "Lab")(seq(0,1,length.out=border))
    for (i in 1:border) {
      colorscale[[i]] <- c((i - 1) / colorlength, s[i])
    }
    
    #colorscale above zero
    s <- scales::seq_gradient_pal("white", "red", "Lab")(seq(0,1,length.out=colorlength - border))
    for (i in 1:(colorlength - border)) {
      colorscale[[i + border]] <- c((i + border) / colorlength, s[i])
    }
    
    plot_ly(source = "heat_plot") %>%
      add_heatmap(
        x=fit_var,
        y=fit_out,
        z=mat,
        hoverinfo = 'text',
        text = text.mat,
        colors = colorRamp(c("blue","white","red")),
        colorscale = colorscale,
        colorbar = list(len = 1, limits = c(-1,1)))
  })
  
  output$models <- renderPlot({
    
    clickData <- event_data("plotly_click", source = "heat_plot")
    if (is.null(clickData)) return(NULL)
    
    out <- gsub("Log\\(Cost of Care\\)","log_Cost_Care",clickData[["y"]])
    out <- gsub("Log\\(Cost Efficiency\\)", "log_Cost_Eff", out)
    var <- gsub("Unplanned Admissions", "UnplannedAdmits", clickData[["x"]])
    var <- gsub("Readmissions", "Readmits", var)
    
    if (var=="Primary Care Services with a Primary Care Physician"){
      var = "Primary Care Services with a Primary Care Physician (%)"
      adjusted_var = "`+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      if (var=="Skilled Nursing Facility or Unit Discharges"){
        adjusted_var = "`+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "`+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(out, "~`", var, adjusted_var))
    fit <- lm(form, data = dat19)
    plt <- ggcoef(fit, exclude_intercept=TRUE)
    plt$data$term <- gsub("SexMale", "Sex(Male=1)",plt$data$term)
    plt$data$term <- gsub("Cont_Att", "Continuous Attribution",plt$data$term)
    plt$data$term <- gsub("UnplannedAdmits", "Unplanned Admissions",plt$data$term)
    plt$data$term <- gsub("Readmits", "Readmissions",plt$data$term)
    plt$data$term <- gsub("HCC_85", "HCC_CHF",plt$data$term)
    plt$data$term <- gsub("HCC_111", "HCC_COPD",plt$data$term)
    plt$data$term <- gsub("PrimaryCareSpecialist", "Primary Practice(Specialist=1)",plt$data$term)
    
    plt
  })
  
  output$coef <- renderUI({
    clickData <- event_data("plotly_click", source = "heat_plot")
    if (is.null(clickData)) return(NULL)
    
    out <- gsub("Log\\(Cost of Care\\)","log_Cost_Care",clickData[["y"]])
    out <- gsub("Log\\(Cost Efficiency\\)", "log_Cost_Eff", out)
    var <- gsub("Unplanned Admissions", "UnplannedAdmits", clickData[["x"]])
    var <- gsub("Readmissions", "Readmits", var)
    
    if (var=="Primary Care Services with a Primary Care Physician"){
      var = "Primary Care Services with a Primary Care Physician (%)"
      adjusted_var = "`+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      if (var=="Skilled Nursing Facility or Unit Discharges"){
        adjusted_var = "`+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "`+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(out, "~`", var, adjusted_var))
    fit <- lm(form, data = dat19)
    
    
    fit.sum <- summary(fit)
    
    var <- var_Replace(var)
    
    HTML(paste(
      "<b>Coefficients: </b>", "The coefficient of ", var, " is ", round(fit$coefficients[2], 3), 
      ". The 95% confidence interval is (", round(confint(fit)[2,1], 3), ",", round(confint(fit)[2,2], 3), ").","<p/>",
      var, " and other variables (comorbidities, sex, age and etc.) could explain ", round(fit.sum$adj.r.squared, 3)*100, 
      "% variability of ", out,"<p/>",
      "<b>Note: </b>", "The dots represent the coefficients of these variables and the width of each line represents the confidence interval. ","<p/>",
      "The more further the dot is from the dashed line, the larger the effect of the variable is.","<p/>",
      "If the line around the dot crosses the vertical dashed line, this variable is not significant.",
      sep=""
    ))
  })
  
  output$tab1 <- function(){
    
    if (input$OR=="`Primary Care Services with a Primary Care Physician`"){
      var = "`Primary Care Services with a Primary Care Physician (%)`"
      adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      var=input$OR
      if (input$OR=="`Skilled Nursing Facility or Unit Discharges`"){
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(input$out, "~", var, adjusted_var))
    
    fit <- lm(form, data = dat19)
    
    explanatory <- gsub("`", "", var)
    work <- dat19
    origin <- predict(fit, work)
    
    work[explanatory] = work[explanatory]*(1-input$prop)
    pop <- predict(fit, work)
    
    pred <- data.frame(origin = origin, pop = pop, ESRD = work$ESRD, Dual=work$Dual, 
                       NonDual = work$NonDual, Disabled = work$Disabled, PrimaryCare = work$PrimaryCare,
                       HCC_Cancer = work$HCC_Cancer, HCC_CKD = work$HCC_CKD, HCC_85 = work$HCC_85,
                       HCC_Diabetes = work$HCC_Diabetes, HCC_111 = work$HCC_111, HCC_CAD = work$HCC_CAD,
                       TaxEntityName = work$TaxEntityName)
    
    ##filter by selected eligibility reason and primary practice
    if (input$eligibility!="all"){
      pred = pred %>% filter(!!as.name(input$eligibility)==1)
    }
    
    if (input$primarycare!="all"){
      pred = pred %>% filter(PrimaryCare==input$primarycare)
      pred = pred %>% filter(PrimaryCare==input$primarycare)
    }
    
    if (input$comorbidities!="all"){
      pred = pred %>% filter(!!as.name(input$comorbidities)==1)
      pred = pred %>% filter(!!as.name(input$comorbidities)==1)
    }
    if (input$tins!="all"){
      pred = pred %>% filter(TaxEntityName==input$tins)
      pred = pred %>% filter(TaxEntityName==input$tins)
    }
    
    if (input$out=="log_Cost_Care"){
      origin <- sum(exp(pred$origin))
      pop <- sum(exp(pred$pop))
      popCost = paste0("$",round(origin-pop, digits = 1))
      indCost = paste0("$",round((origin-pop)/dim(pred)[1], digits=1))
      tab1 <- data.frame("Population Cost Reduction" = popCost, "Individual Cost Reduction" = indCost,
                         "Number of Beneficiaries" = dim(pred)[1])
    }
    else {
      origin <- sum(pred$origin)
      pop <- sum(pred$pop)
      popCost = round((origin-pop)/origin*100, digits = 1)
      indCost = round((origin-pop)/dim(pred)[1], digits=3)
      tab1 <- data.frame("Population Cost Efficiency Reduction" = paste0(popCost, "%"), "Individual Cost Efficiency Reduction" = indCost,
                         "Number of Beneficiaries" = dim(pred)[1])
    }
    
    rownames(tab1) <- "Overall Population"
    tab1 %>% kable() %>% kable_styling()
      
  }
  
  tab_com <- function(fit, var, elig, pc, com, prop, tins){
    tab <- data.frame(matrix(NA, nrow = length(com), ncol = 3))
    if (input$out2=="log_Cost_Care"){
      colnames(tab) <- c("Population Cost Reduction", "Individual Cost Reduction", "Number of Beneficiaries")
    } else {
      colnames(tab) <- c("Population Cost Efficiency Reduction", "Individual Cost Efficiency Reduction", "Number of Beneficiaries")
    }
    
    com_name <- vector()
    
    for (i in 1:length(com)){
      work <- dat19 %>% filter(!!as.name(com[i])==1)
      origin <- predict(fit, work)
      
      work[var] = work[var]*(1-prop)
      pop <- predict(fit, work)
      
      pred <- data.frame(origin = origin, pop = pop, ESRD = work$ESRD, Dual=work$Dual, 
                         NonDual = work$NonDual, Disabled = work$Disabled, PrimaryCare = work$PrimaryCare,
                         TaxEntityName=work$TaxEntityName)
      
      if (elig!="all"){
        pred = pred %>% filter(!!as.name(elig)==1)
      }
      
      if (pc!="all"){
        pred = pred %>% filter(PrimaryCare==pc)
      }
      if (tins!="all"){
        pred = pred %>% filter(TaxEntityName==tins)
      }
      
      
      if (input$out2=="log_Cost_Care"){
        origin <- sum(exp(pred$origin))
        pop <- sum(exp(pred$pop))
        popCost = round(origin-pop, digits = 1)
        indCost = round((origin-pop)/dim(pred)[1], digits=1)
      }
      else {
        origin <- sum(pred$origin)
        pop <- sum(pred$pop)
        popCost = round((origin-pop)/origin*100, digits = 1)
        indCost = round((origin-pop)/dim(pred)[1], digits=3)
      }
      
      
      tab[i,] <- c(popCost, indCost, dim(pred)[1])
      com_name <- append(com_name, com_Replace(com[i]))
      
    }
    
    rownames(tab) <- com_name
    
    tab
    
  }
  
  output$compare <- renderPlot({
    if (input$OR2=="`Primary Care Services with a Primary Care Physician`"){
      var = "`Primary Care Services with a Primary Care Physician (%)`"
      adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      var=input$OR2
      if (input$OR2=="`Skilled Nursing Facility or Unit Discharges`"){
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(input$out2, "~", var, adjusted_var))
    
    fit <- lm(form, data = dat19)
    
    explanatory <- gsub("`", "", var)
    tab <- tab_com(fit, explanatory, input$eligibility2, input$primarycare2, input$comorbidities2, input$prop2, input$tins2)
    tab$Comorbidities <- rownames(tab)
    
    if (input$out2=="log_Cost_Care"){
      ggplot(tab, aes(x=Comorbidities)) +
        geom_bar(stat = "identity",aes(y=`Population Cost Reduction`), fill = "steelblue", width = 0.3) +
        geom_point(aes(y=`Individual Cost Reduction`*3000, label = `Individual Cost Reduction`), size = 2) +
        geom_text(aes(y=`Individual Cost Reduction`*3000, 
                      label = paste0("Individual Cost: $", `Individual Cost Reduction`)), hjust = 0, vjust=-1)+
        
        scale_y_continuous(
          name = "Cost of Care",
          sec.axis = sec_axis(~./3000, name="Individual Cost")
        )
    } else{
      ggplot(tab, aes(x=Comorbidities)) +
        geom_bar(stat = "identity",aes(y=`Population Cost Efficiency Reduction`), fill = "steelblue", width = 0.3) +
        geom_point(aes(y=`Individual Cost Efficiency Reduction`*100, label = `Individual Cost Efficiency Reduction`), size = 2) +
        geom_text(aes(y=`Individual Cost Efficiency Reduction`*100, label = paste0("Individual Cost Efficiency: ", `Individual Cost Efficiency Reduction`)), hjust = 0, vjust=-1)+
        
        scale_y_continuous(
          name = "Population Cost Efficiency (%)",
          sec.axis = sec_axis(~./100, name="Individual Cost Efficiency")
        )
    }
    
  })
  
  output$tab2 <- function(){
    if (input$OR2=="`Primary Care Services with a Primary Care Physician`"){
      var = "`Primary Care Services with a Primary Care Physician (%)`"
      adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      var=input$OR2
      if (input$OR2=="`Skilled Nursing Facility or Unit Discharges`"){
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(input$out2, "~", var, adjusted_var))
    
    fit <- lm(form, data = dat19)
    
    explanatory <- gsub("`", "", var)
    tab <- tab_com(fit, explanatory, input$eligibility2, input$primarycare2, input$comorbidities2, input$prop2, input$tins2)
    
    if (input$out2=="log_Cost_Care"){
      tab$`Population Cost Reduction` <- paste0("$",tab$`Population Cost Reduction`)
      tab$`Individual Cost Reduction` <- paste0("$",tab$`Individual Cost Reduction`)
    }
    else {
      tab$`Population Cost Efficiency Reduction` = paste0(tab$`Population Cost Efficiency Reduction`, "%")
    }
    
    tab %>% kable() %>% kable_styling()
    
  }
  
  tab_elig <- function(fit, var, elig, pc, com, prop, tins){
    tab <- data.frame(matrix(NA, nrow = length(elig), ncol = 3))
    
    if (input$out3=="log_Cost_Care"){
      colnames(tab) <- c("Population Cost Reduction", "Individual Cost Reduction", "Number of Beneficiaries")
    } else {
      colnames(tab) <- c("Population Cost Efficiency Reduction", "Individual Cost Efficiency Reduction", "Number of Beneficiaries")
    }
    elig_name <- vector()
    
    for (i in 1:length(elig)){
      work <- dat19 %>% filter(!!as.name(elig[i])==1)
      origin <- predict(fit, work)
      
      work[var] = work[var]*(1-prop)
      pop <- predict(fit, work)
      
      pred <- data.frame(origin = origin, pop = pop, PrimaryCare = work$PrimaryCare, HCC_Cancer=work$HCC_Cancer,
                         HCC_CAD = work$HCC_CAD, HCC_Diabetes = work$HCC_Diabetes, HCC_CKD = work$HCC_CKD,
                         HCC_85 = work$HCC_85, HCC_111 = work$HCC_111, TaxEntityName=work$TaxEntityName)
      
      if (com!="all"){
        pred = pred %>% filter(!!as.name(com)==1)
      }
      
      if (pc!="all"){
        pred = pred %>% filter(PrimaryCare==pc)
      }
      
      if (tins!="all"){
        pred = pred %>% filter(TaxEntityName==tins)
      }
      
      if (input$out3=="log_Cost_Care"){
        origin <- sum(exp(pred$origin))
        pop <- sum(exp(pred$pop))
        popCost = round(origin-pop, digits = 1)
        indCost = round((origin-pop)/dim(pred)[1], digits=1)
      }
      else {
        origin <- sum(pred$origin)
        pop <- sum(pred$pop)
        popCost = round((origin-pop)/origin*100, digits = 1)
        indCost = round((origin-pop)/dim(pred)[1], digits=3)
      }
      
      tab[i,] <- c(popCost, indCost, dim(pred)[1])
      elig_name <- append(elig_name, elig[i])
      
    }
    
    rownames(tab) <- elig_name
    
    tab
    
  }
  
  output$bar <- renderPlot({
    if (input$OR3=="`Primary Care Services with a Primary Care Physician`"){
      var = "`Primary Care Services with a Primary Care Physician (%)`"
      adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      var=input$OR3
      if (input$OR3=="`Skilled Nursing Facility or Unit Discharges`"){
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(input$out3, "~", var, adjusted_var))
    
    fit <- lm(form, data = dat19)
    
    explanatory <- gsub("`", "", var)
    tab <- tab_elig(fit, explanatory, input$eligibility3, input$primarycare3, input$comorbidities3, input$prop3, input$tins3)
    tab$EligibilityReason <- rownames(tab)
    
    if (input$out3=="log_Cost_Care"){
      ggplot(tab, aes(x=EligibilityReason)) +
        geom_bar(stat = "identity",aes(y=`Population Cost Reduction`), fill = "steelblue", width = 0.3) +
        geom_point(aes(y=`Individual Cost Reduction`*2000, label = `Individual Cost Reduction`), size = 2) +
        geom_text(aes(y=`Individual Cost Reduction`*2000, 
                      label = paste0("Individual Cost: $", `Individual Cost Reduction`)), hjust = 0, vjust=-1)+
        
        scale_y_continuous(
          name = "Cost of Care",
          sec.axis = sec_axis(~./2000, name="Individual Cost")
        )
    } else{
      ggplot(tab, aes(x=EligibilityReason)) +
        geom_bar(stat = "identity",aes(y=`Population Cost Efficiency Reduction`), fill = "steelblue", width = 0.3) +
        geom_point(aes(y=`Individual Cost Efficiency Reduction`*100, label = `Individual Cost Efficiency Reduction`), size = 2) +
        geom_text(aes(y=`Individual Cost Efficiency Reduction`*100, label = paste0("Individual Cost Efficiency: ", `Individual Cost Efficiency Reduction`)), hjust = 0, vjust=-1)+
        scale_y_continuous(
          name = "Population Cost Efficiency (%)",
          sec.axis = sec_axis(~./100, name="Individual Cost Efficiency")
        )
    }
    
  })
  
  
  output$tab3 <- function(){
    if (input$OR3=="`Primary Care Services with a Primary Care Physician`"){
      var = "`Primary Care Services with a Primary Care Physician (%)`"
      adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      var=input$OR3
      if (input$OR3=="`Skilled Nursing Facility or Unit Discharges`"){
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(input$out3, "~", var, adjusted_var))
    
    fit <- lm(form, data = dat19)
    explanatory <- gsub("`", "", var)
    
    tab <- tab_elig(fit, explanatory, input$eligibility3, input$primarycare3, input$comorbidities3, input$prop3, input$tins3)
    
    if (input$out3=="log_Cost_Care"){
      tab$`Population Cost Reduction` <- paste0("$",tab$`Population Cost Reduction`)
      tab$`Individual Cost Reduction` <- paste0("$",tab$`Individual Cost Reduction`)
    }
    else {
      tab$`Population Cost Efficiency Reduction` <- paste0(tab$`Population Cost Efficiency Reduction`, "%")
    }
    tab %>% kable() %>% kable_styling()
  }
  
  tab_pc <- function(fit, var, elig, pc, com, prop, tins){
    tab <- data.frame(matrix(NA, nrow = length(pc), ncol = 3))
    if (input$out4=="log_Cost_Care"){
      colnames(tab) <- c("Population Cost Reduction", "Individual Cost Reduction", "Number of Beneficiaries")
    } else {
      colnames(tab) <- c("Population Cost Efficiency Reduction", "Individual Cost Efficiency Reduction", "Number of Beneficiaries")
    }
    pc_name <- vector()
    
    for (i in 1:length(pc)){
      work <- dat19 %>% filter(PrimaryCare==pc[i])
      origin <- predict(fit, work)
      
      work[var] = work[var]*(1-prop)
      pop <- predict(fit, work)
      
      pred <- data.frame(origin = origin, pop = pop, ESRD = work$ESRD, Dual=work$Dual, NonDual = work$NonDual, 
                         Disabled = work$Disabled, PrimaryCare = work$PrimaryCare, HCC_Cancer=work$HCC_Cancer,
                         HCC_CAD = work$HCC_CAD, HCC_Diabetes = work$HCC_Diabetes, HCC_CKD = work$HCC_CKD,
                         HCC_85 = work$HCC_85, HCC_111 = work$HCC_111, TaxEntityName = work$TaxEntityName)
      
      if (elig!="all"){
        pred = pred %>% filter(!!as.name(elig)==1)
      }
      if (com!="all"){
        pred = pred %>% filter(!!as.name(com)==1)
      }
      if (tins!="all"){
        pred = pred %>% filter(TaxEntityName==tins)
      }
      
      if (input$out4=="log_Cost_Care"){
        origin <- sum(exp(pred$origin))
        pop <- sum(exp(pred$pop))
        popCost = round(origin-pop, digits = 1)
        indCost = round((origin-pop)/dim(pred)[1], digits=1)
      }
      else {
        origin <- sum(pred$origin)
        pop <- sum(pred$pop)
        popCost = round((origin-pop)/origin*100, digits = 1)
        indCost = round((origin-pop)/dim(pred)[1], digits=3)
      }
      
      tab[i,] <- c(popCost, indCost, dim(pred)[1])
      pc_name <- append(pc_name, pc[i])
      
    }
    
    rownames(tab) <- pc_name
    
    tab
    
  }
  
  output$bar2 <- renderPlot({
    if (input$OR4=="`Primary Care Services with a Primary Care Physician`"){
      var = "`Primary Care Services with a Primary Care Physician (%)`"
      adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      var=input$OR4
      if (input$OR4=="`Skilled Nursing Facility or Unit Discharges`"){
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(input$out4, "~", var, adjusted_var))
    
    fit <- lm(form, data = dat19)
    
    explanatory <- gsub("`", "", var)
    tab <- tab_pc(fit, explanatory, input$eligibility4, input$primarycare4, input$comorbidities4, input$prop4, input$tins4)
    tab$PrimaryPractice <- rownames(tab)
    
    if (input$out4=="log_Cost_Care"){
      ggplot(tab, aes(x=PrimaryPractice)) +
        geom_bar(stat = "identity",aes(y=`Population Cost Reduction`), fill = "steelblue", width = 0.3) +
        geom_point(aes(y=`Individual Cost Reduction`*2000, label = `Individual Cost Reduction`), size = 2) +
        geom_text(aes(y=`Individual Cost Reduction`*2000, label = paste0("Individual Cost: $", `Individual Cost Reduction`)), hjust = 0, vjust=-1)+
        
        scale_y_continuous(
          name = "Population Cost",
          sec.axis = sec_axis(~./2000, name="Individual Cost")
        )
    } else {
      ggplot(tab, aes(x=PrimaryPractice)) +
        geom_bar(stat = "identity",aes(y=`Population Cost Efficiency Reduction`), fill = "steelblue", width = 0.3) +
        geom_point(aes(y=`Individual Cost Efficiency Reduction`*100, label = `Individual Cost Efficiency Reduction`), size = 2) +
        geom_text(aes(y=`Individual Cost Efficiency Reduction`*100, label = paste0("Individual Cost Efficiency: ", `Individual Cost Efficiency Reduction`)), hjust = 0, vjust=-1)+
        
        scale_y_continuous(
          name = "Population Cost Efficiency (%)",
          sec.axis = sec_axis(~./100, name="Individual Cost Efficiency")
        )
    }

    
  })
  
  
  output$tab4 <- function(){
    if (input$OR4=="`Primary Care Services with a Primary Care Physician`"){
      var = "`Primary Care Services with a Primary Care Physician (%)`"
      adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      var=input$OR4
      if (input$OR4=="`Skilled Nursing Facility or Unit Discharges`"){
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(input$out4, "~", var, adjusted_var))
    
    fit <- lm(form, data = dat19)
    explanatory <- gsub("`", "", var)
    
    tab <- tab_pc(fit, explanatory, input$eligibility4, input$primarycare4, input$comorbidities4, input$prop4, input$tins4)
    
    if (input$out4=="log_Cost_Care"){
      tab$`Population Cost Reduction` <- paste0("$",tab$`Population Cost Reduction`)
      tab$`Individual Cost Reduction` <- paste0("$",tab$`Individual Cost Reduction`)
    }
    else {
      tab$`Population Cost Efficiency Reduction` <- paste0(tab$`Population Cost Efficiency Reduction`, "%")
    }
    tab %>% kable() %>% kable_styling()
  }
  
  
  
  tab_tins <- function(fit, var, elig, tins, com, prop,pc){
    tab <- data.frame(matrix(NA, nrow = length(tins), ncol = 3))
    if (input$out5=="log_Cost_Care"){
      colnames(tab) <- c("Population Cost Reduction", "Individual Cost Reduction", "Number of Beneficiaries")
    } else {
      colnames(tab) <- c("Population Cost Efficiency Reduction", "Individual Cost Efficiency Reduction", "Number of Beneficiaries")
    }
    tins_name <- vector()
    
    for (i in seq_along(tins)){
      work <- dat19 %>% filter(TaxEntityName==tins[i])
      origin <- predict(fit, work)
      
      work[var] = work[var]*(1-prop)
      pop <- predict(fit, work)
      
      pred <- data.frame(origin = origin, pop = pop, ESRD = work$ESRD, Dual=work$Dual, NonDual = work$NonDual, 
                         Disabled = work$Disabled, PrimaryCare = work$PrimaryCare, HCC_Cancer=work$HCC_Cancer,
                         HCC_CAD = work$HCC_CAD, HCC_Diabetes = work$HCC_Diabetes, HCC_CKD = work$HCC_CKD,
                         HCC_85 = work$HCC_85, HCC_111 = work$HCC_111)
      
      if (elig!="all"){
        pred = pred %>% filter(!!as.name(elig)==1)
      }
      if (com!="all"){
        pred = pred %>% filter(!!as.name(com)==1)
      }
      if (pc!="all"){
        pred = pred %>% filter(PrimaryCare==pc)
      }
      
      if (input$out5=="log_Cost_Care"){
        origin <- sum(exp(pred$origin))
        pop <- sum(exp(pred$pop))
        popCost = round(origin-pop, digits = 1)
        indCost = round((origin-pop)/dim(pred)[1], digits=1)
      }
      else {
        origin <- sum(pred$origin)
        pop <- sum(pred$pop)
        popCost = round((origin-pop)/origin*100, digits = 1)
        indCost = round((origin-pop)/dim(pred)[1], digits=3)
      }
      
      tab[i,] <- c(popCost, indCost, dim(pred)[1])
      tins_name <- append(tins_name, tins[i])
      
    }
    
    rownames(tab) <- tins_name
    
    tab
    
  }
  
  output$bar3 <- renderPlot({
    if (input$OR5=="`Primary Care Services with a Primary Care Physician`"){
      var = "`Primary Care Services with a Primary Care Physician (%)`"
      adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      var=input$OR5
      if (input$OR5=="`Skilled Nursing Facility or Unit Discharges`"){
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(input$out5, "~", var, adjusted_var))
    
    fit <- lm(form, data = dat19)
    
    explanatory <- gsub("`", "", var)
    tab <- tab_tins(fit, explanatory, input$eligibility5, input$tins5, input$comorbidities5, input$prop5, input$primarycare5)
    tab$tins <- rownames(tab)
    
    if (input$out5=="log_Cost_Care"){
      ggplot(tab, aes(x=tins)) +
        geom_bar(stat = "identity",aes(y=`Population Cost Reduction`), fill = "steelblue", width = 0.3) +
        geom_point(aes(y=`Individual Cost Reduction`*2000, label = `Individual Cost Reduction`), size = 2) +
        geom_text(aes(y=`Individual Cost Reduction`*2000, label = paste0("Individual Cost: $", `Individual Cost Reduction`)), hjust = 0, vjust=-1)+
        xlab("Primary Practice TINs")+
        
        scale_y_continuous(
          name = "Population Cost",
          sec.axis = sec_axis(~./2000, name="Individual Cost")
        )
    } else {
      ggplot(tab, aes(x=tins)) +
        geom_bar(stat = "identity",aes(y=`Population Cost Efficiency Reduction`), fill = "steelblue", width = 0.3) +
        geom_point(aes(y=`Individual Cost Efficiency Reduction`*100, label = `Individual Cost Efficiency Reduction`), size = 2) +
        geom_text(aes(y=`Individual Cost Efficiency Reduction`*100, label = paste0("Individual Cost Efficiency: ", `Individual Cost Efficiency Reduction`)), hjust = 0, vjust=-1)+
        
        scale_y_continuous(
          name = "Population Cost Efficiency (%)",
          sec.axis = sec_axis(~./100, name="Individual Cost Efficiency")
        )
    }
    
    
  })
  
  output$tab5 <- function(){
    if (input$OR5=="`Primary Care Services with a Primary Care Physician`"){
      var = "`Primary Care Services with a Primary Care Physician (%)`"
      adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Primary Care Services`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
    } else {
      var=input$OR5
      if (input$OR5=="`Skilled Nursing Facility or Unit Discharges`"){
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+`Total Hospital Discharges`+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
      else {
        adjusted_var = "+Sex+Age+Dual+ESRD+Disabled+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+OutNetwork+HCC_CKD+PrimaryCare"
      }
    }
    
    form <- as.formula(paste0(input$out5, "~", var, adjusted_var))
    
    fit <- lm(form, data = dat19)
    explanatory <- gsub("`", "", var)
    
    tab <- tab_tins(fit, explanatory, input$eligibility5, input$tins5, input$comorbidities5, input$prop5, input$primarycare5)
    
    
    if (input$out5=="log_Cost_Care"){
      tab$`Population Cost Reduction` <- paste0("$",tab$`Population Cost Reduction`)
      tab$`Individual Cost Reduction` <- paste0("$",tab$`Individual Cost Reduction`)
    }
    else {
      tab$`Population Cost Efficiency Reduction` <- paste0(tab$`Population Cost Efficiency Reduction`, "%")
    }
    tab %>% kable() %>% kable_styling()
  }
  
  drills <- reactiveValues(
    category = NULL
  )
  
  categories <- unique(TINs$TaxEntityName)
  
  #current_category <- reactiveVal()
  
  tin_dat <- reactive({
    #if (!length(current_category())) {
    if (!length(drills$category)) {
      return(count(dat19, TaxEntityName))
    }
    dat19 %>%
      #filter(TaxEntityName == current_category()) %>%
      filter(TaxEntityName == drills$category) %>%
      count(EligibilityReason)
  })
  
  output$pie <- renderPlotly({
    #work <- count(dat19, TaxEntityName)
    work <- setNames(tin_dat(), c("x", "y"))
    plot_ly(work) %>%
      add_pie(
        labels = ~x,
        values = ~y,
        customdata = ~x
      ) 
  })
  
  tin_filter <- reactive({
    #if (!length(current_category())) {
    if (!length(drills$category)) {
      return(dat19)
    }
    dat19 %>% filter(TaxEntityName == drills$category)
  })
  
  output$heatmap2 <- renderPlotly({
    d <- tin_filter()
    out <- c("log_Cost_Care","log_Cost_Eff")
    var <- c("`Total Hospital Discharges`", "UnplannedAdmits", "Readmits", "`ED Visits`",
             "`Computed Tomography (CT) Events`", "`Magnetic Resonance Imaging (MRI) Events`",
             "`Days in Hospice`", "`Total Primary Care Services`", 
             "`Primary Care Services with a Primary Care Physician (%)`+`Total Primary Care Services`",
             "`Skilled Nursing Facility or Unit Discharges`+`Total Hospital Discharges`")
    
    #check the categories of primarycare
    if (dim(distinct(d, PrimaryCare))[1]<=1) adj = "+Sex+Age+Dual+Disabled+ESRD+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+HCC_CKD+OutNetwork"
    else adj="+Sex+Age+Dual+Disabled+ESRD+Cont_Att+Death+HCC_Cancer+HCC_Diabetes+HCC_CAD+HCC_85+HCC_111+HCC_CKD+OutNetwork+PrimaryCare"
    
    coef_vector <- vector() 
    conf_vector <- vector()

    for (j in 1:length(out)){
      for (i in 1:length(var)){
        form <- as.formula(
          paste0(out[j],"~",var[i],adj)
        )
        fit <- lm(form, data = d)
        coef_vector <- append(coef_vector,fit$coefficients[2])
        conf_vector <- append(conf_vector,ifelse(confint(fit)[2,1]*confint(fit)[2,2]<0,0,1))
      }  
    }
    
    coef_vector <- round(replace_na(coef_vector,0),3)
    
    ann <- list()
    j=0
    for (i in 1:length(conf_vector)){
      if (conf_vector[i]==0){
        j=j+1
        xaxis = ifelse(i<=10,"Log(Cost of Care)","Log(Cost Efficiency)")
        yaxis = gsub("Admits"," Admissions",gsub("admits","admissions",gsub("`","",names(coef_vector[i]))))
        ann[[j]] <- list(x=xaxis,y=yaxis, text="Uninterpretable",xref="x",yref="y",showarrow=FALSE)
      }
    }
    
    mat = t(matrix(coef_vector, nrow = 2, ncol = 10))
    conditions.text <- as.vector(outer(paste0("Hospital Utilizations:",gsub("Admits"," Admissions",gsub("admits","admissions",gsub("`","",names(coef_vector[1:10])))),"\n"), paste0("Outcome variable:",c("Log(Cost Care)","Log(Cost Efficiency)"),"\n"), paste0))
    conditions.text <- paste0(conditions.text,"Magnitude of Association:",coef_vector)
    text.mat <- matrix(conditions.text, nrow= nrow(mat), ncol = ncol(mat))
    
    colorlength <- 100
    
    null_value <- (0 - min(coef_vector)) / (max(coef_vector) - min(coef_vector))        
    border <- as.integer(null_value * colorlength)
    colorscale <- as.list(1:colorlength)
    
    #colorscale below zero
    s <- scales::seq_gradient_pal("blue", "white", "Lab")(seq(0,1,length.out=border))
    for (i in 1:border) {
      colorscale[[i]] <- c((i - 1) / colorlength, s[i])
    }
    
    #colorscale above zero
    s <- scales::seq_gradient_pal("white", "red", "Lab")(seq(0,1,length.out=colorlength - border))
    for (i in 1:(colorlength - border)) {
      colorscale[[i + border]] <- c((i + border) / colorlength, s[i])
    }
    
   
    plot_ly(source = "heat_plot") %>%
      add_heatmap(
        x=c("Log(Cost of Care)","Log(Cost Efficiency)"),
        y=gsub("Admits"," Admissions",gsub("admits","admissions",gsub("`","",names(coef_vector[1:10])))),
        z=matrix(coef_vector,nrow=10,ncol = 2),
        hoverinfo = 'text',
        text = text.mat,
        colors = colorRamp(c("blue","white","red")),
        colorscale = colorscale,
        colorbar = list(len = 1, limits = c(-1,1)),
        wdith = 500,
        height=1000
        )%>%
      layout(margin=list(l=350,r=20, b=10, t=10), annotations=ann)
  })
  
  observeEvent(event_data("plotly_click"), {
    x <- event_data("plotly_click")$customdata[[1]]
    if (!length(x)) return()
    
    drills$category <- x
  })
  
  output$history <- renderUI({
    if (!length(drills$category)) 
      return("Click the pie chart to drilldown")
    
    categoryInput <- selectInput(
      "category", "TIN", 
      choices = categories, selected = drills$category
    )
    sd <- filter(dat19, TaxEntityName %in% drills$category)
    fluidRow(
      column(3, categoryInput)
    )
  })
  
  observeEvent(input$category, {
    drills$category <- input$category
  })
  
  #observe({
  #  cd <- event_data("plotly_click")$customdata[[1]]
  #  if (isTRUE(cd %in% categories)) current_category(cd)
  #})
  
  output$back <- renderUI({
    if (length(drills$category)) 
      actionButton("clear", "Back", icon("chevron-left"))
  })
  
  # clear the chosen category on back button press
  observeEvent(input$clear, {drills$category<-NULL})
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
})