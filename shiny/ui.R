#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  
  theme = shinytheme("flatly"),
  
  # Application title
  title = "PHMO Project",
  tabPanel("Adjusted Models",
            plotlyOutput("heat"),
            plotOutput("models"),
            htmlOutput("coef")
  ),
  navbarMenu(
    "Projection",
    tabPanel("Overall",
             sidebarLayout(
               sidebarPanel(
                 selectInput("OR","Explanatory Variable",
                             list("Total Hospital Discharges"="`Total Hospital Discharges`",
                                  "Unplanned Admission"="UnplannedAdmits",
                                  "Readmissions"="Readmits",
                                  "ED Visits"="`ED Visits`",
                                  "CT Events"="`Computed Tomography (CT) Events`",
                                  "MRI Event"="`Magnetic Resonance Imaging (MRI) Events`",
                                  "Days in Hospice"="`Days in Hospice`",
                                  "Total Primary Care Services"="`Total Primary Care Services`",
                                  "Primary Care Services: Primary Care Physician"="`Primary Care Services with a Primary Care Physician`",
                                  "Skilled Nursing Facility/Units Discharges"="`Skilled Nursing Facility or Unit Discharges`")),
                 sliderInput("prop", "The selected health care utilization decreases by",
                             0,1,0, step = 0.01),
                 selectInput("out", "Outcome Variable",
                             list("Cost of Care"="log_Cost_Care",
                                  "Cost Efficiency"="`Cost Efficiency`")),
                 selectInput("eligibility","Eligibility Reasons",
                             list("All"="all",
                                  "ESRD"="ESRD",
                                  "Disabled"="Disabled",
                                  "Aged/Nondual"="NonDual",
                                  "Aged/Dual"="Dual"
                             )),
                 selectInput("primarycare","Primary Practices",
                             list("All"="all",
                                  "Primary Care"="Primary Care",
                                  "Specialists"="Specialist"
                             )),
                 selectInput("tins","Primary Practices TINs",
                             list("All"="all",
                                  "DUKE PRIMARY CARE"="DUKE PRIMARY CARE",
                                  "PRIVATE DIAGNOSTIC CLINIC PLLC"="PRIVATE DIAGNOSTIC CLINIC PLLC"
                             )),
                 selectInput("comorbidities", "Comorbidities",
                             list("All"="all",
                               "Cancer"="HCC_Cancer",
                                  "CAD"="HCC_CAD",
                                  "Diabetes"="HCC_Diabetes",
                                  "CHF"="HCC_85",
                                  "COPD"="HCC_111",
                                  "CKD"="HCC_CKD"))
               ),
               mainPanel(
                 tableOutput("tab1")
               )
             )
    ),
    tabPanel(
      "Comorbidities",
      sidebarLayout(
        sidebarPanel(
          selectInput("OR2","Explanatory Variable",
                      list("Total Hospital Discharges"="`Total Hospital Discharges`",
                           "Unplanned Admission"="UnplannedAdmits",
                           "Readmissions"="Readmits",
                           "ED Visits"="`ED Visits`",
                           "CT Events"="`Computed Tomography (CT) Events`",
                           "MRI Event"="`Magnetic Resonance Imaging (MRI) Events`",
                           "Days in Hospice"="`Days in Hospice`",
                           "Total Primary Care Services"="`Total Primary Care Services`",
                           "Primary Care Services: Primary Care Physician"="`Primary Care Services with a Primary Care Physician`",
                           "Skilled Nursing Facility/Units Discharges"="`Skilled Nursing Facility or Unit Discharges`")),
          sliderInput("prop2", "The selected health care utilization decreases by",
                      0,1,0, step = 0.01),
          selectInput("out2", "Outcome Variable",
                      list("Cost of Care"="log_Cost_Care",
                           "Cost Efficiency"="`Cost Efficiency`")),
          checkboxGroupInput("comorbidities2", "Comorbidities:",
                             choices = c("Cancer"="HCC_Cancer",
                                         "Coronary Artery Disease"="HCC_CAD",
                                         "Congestive Heart Failure"="HCC_85",
                                         "Chronic Obstructive Pulmonary Disease"="HCC_111",
                                         "Chronic Kidney Disease"="HCC_CKD",
                                         "Diabetes"="HCC_Diabetes"), 
                             selected = c("HCC_Cancer", "HCC_CAD")),
          selectInput("eligibility2","Eligibility Reasons",
                      list("All"="all",
                           "ESRD"="ESRD",
                           "Disabled"="Disabled",
                           "Aged/Nondual"="NonDual",
                           "Aged/Dual"="Dual"
                      )),
          selectInput("primarycare2","Primary Practices",
                      list("All"="all",
                           "Primary Care"="Primary Care",
                           "Specialists"="Specialist"
                      )),
          selectInput("tins2","Primary Practices TINs",
                      list("All"="all",
                           "DUKE PRIMARY CARE"="DUKE PRIMARY CARE",
                           "PRIVATE DIAGNOSTIC CLINIC PLLC"="PRIVATE DIAGNOSTIC CLINIC PLLC"
                      ))
        ),
        mainPanel(
          plotOutput("compare"),
          tableOutput("tab2")
        )
      )
      
    ),
    
  tabPanel("Eligibility Reasons",
           sidebarLayout(
             sidebarPanel(
               selectInput("OR3","Explanatory Variable",
                           list("Total Hospital Discharges"="`Total Hospital Discharges`",
                                "Unplanned Admission"="UnplannedAdmits",
                                "Readmissions"="Readmits",
                                "ED Visits"="`ED Visits`",
                                "CT Events"="`Computed Tomography (CT) Events`",
                                "MRI Event"="`Magnetic Resonance Imaging (MRI) Events`",
                                "Days in Hospice"="`Days in Hospice`",
                                "Total Primary Care Services"="`Total Primary Care Services`",
                                "Primary Care Services: Primary Care Physician"="`Primary Care Services with a Primary Care Physician`",
                                "Skilled Nursing Facility/Units Discharges"="`Skilled Nursing Facility or Unit Discharges`")),
               sliderInput("prop3", "The selected health care utilization decreases by",
                           0,1,0, step = 0.01),
               selectInput("out3", "Outcome Variable",
                           list("Cost of Care"="log_Cost_Care",
                                "Cost Efficiency"="`Cost Efficiency`")),
               checkboxGroupInput("eligibility3", "Eligibility Reasons:",
                                  choices = c("ESRD"="ESRD",
                                              "Disabled"="Disabled",
                                              "Aged/Nondual"="NonDual",
                                              "Aged/Dual"="Dual"), 
                                  selected = c("ESRD", "Disabled","NonDual","Dual")),
               selectInput("primarycare3","Primary Practices",
                           list("All"="all",
                                "Primary Care"="Primary Care",
                                "Specialists"="Specialist"
                           )),
               selectInput("tins3","Primary Practices TINs",
                           list("All"="all",
                                "DUKE PRIMARY CARE"="DUKE PRIMARY CARE",
                                "PRIVATE DIAGNOSTIC CLINIC PLLC"="PRIVATE DIAGNOSTIC CLINIC PLLC"
                           )),
               selectInput("comorbidities3", "Comorbidities",
                           list("All"="all",
                                "Cancer"="HCC_Cancer",
                                "CAD"="HCC_CAD",
                                "Diabetes"="HCC_Diabetes",
                                "CHF"="HCC_85",
                                "COPD"="HCC_111",
                                "CKD"="HCC_CKD"))
               
             ),
             mainPanel(
               plotOutput("bar"),
               tableOutput("tab3")
             )
           )
           ),
  tabPanel("Primary Practices",
    sidebarLayout(
      sidebarPanel(
        selectInput("OR4","Explanatory Variable",
                    list("Total Hospital Discharges"="`Total Hospital Discharges`",
                         "Unplanned Admission"="UnplannedAdmits",
                         "Readmissions"="Readmits",
                         "ED Visits"="`ED Visits`",
                         "CT Events"="`Computed Tomography (CT) Events`",
                         "MRI Event"="`Magnetic Resonance Imaging (MRI) Events`",
                         "Days in Hospice"="`Days in Hospice`",
                         "Total Primary Care Services"="`Total Primary Care Services`",
                         "Primary Care Services: Primary Care Physician"="`Primary Care Services with a Primary Care Physician`",
                         "Skilled Nursing Facility/Units Discharges"="`Skilled Nursing Facility or Unit Discharges`")),
        sliderInput("prop4", "The selected health care utilization decreases by",
                    0,1,0, step = 0.01),
        selectInput("out4", "Outcome Variable",
                    list("Cost of Care"="log_Cost_Care",
                         "Cost Efficiency"="`Cost Efficiency`")),
        checkboxGroupInput(
          "primarycare4","Primary Practices",
          choices = c("Primary Care"="Primary Care",
                      "Specialists"="Specialist"),
          selected = c("Primary Care", "Specialist")
        ),
        selectInput("eligibility4","Eligibility Reasons",
                    list("All"="all",
                         "ESRD"="ESRD",
                         "Disabled"="Disabled",
                         "Aged/Nondual"="NonDual",
                         "Aged/Dual"="Dual"
                    )),
        selectInput("comorbidities4", "Comorbidities",
                    list("All"="all",
                         "Cancer"="HCC_Cancer",
                         "CAD"="HCC_CAD",
                         "Diabetes"="HCC_Diabetes",
                         "CHF"="HCC_85",
                         "COPD"="HCC_111",
                         "CKD"="HCC_CKD")),
        selectInput("tins4","Primary Practices TINs",
                    list("All"="all",
                         "DUKE PRIMARY CARE"="DUKE PRIMARY CARE",
                         "PRIVATE DIAGNOSTIC CLINIC PLLC"="PRIVATE DIAGNOSTIC CLINIC PLLC"
                    ))
      ),
      mainPanel(
        plotOutput("bar2"),
        tableOutput("tab4")
      )
    )
  ),
  tabPanel("Primary Practices TINs",
           sidebarLayout(
             sidebarPanel(
               selectInput("OR5","Explanatory Variable",
                           list("Total Hospital Discharges"="`Total Hospital Discharges`",
                                "Unplanned Admission"="UnplannedAdmits",
                                "Readmissions"="Readmits",
                                "ED Visits"="`ED Visits`",
                                "CT Events"="`Computed Tomography (CT) Events`",
                                "MRI Event"="`Magnetic Resonance Imaging (MRI) Events`",
                                "Days in Hospice"="`Days in Hospice`",
                                "Total Primary Care Services"="`Total Primary Care Services`",
                                "Primary Care Services: Primary Care Physician"="`Primary Care Services with a Primary Care Physician`",
                                "Skilled Nursing Facility/Units Discharges"="`Skilled Nursing Facility or Unit Discharges`")),
               sliderInput("prop5", "The selected health care utilization decreases by",
                           0,1,0, step = 0.01),
               selectInput("out5", "Outcome Variable",
                           list("Cost of Care"="log_Cost_Care",
                                "Cost Efficiency"="`Cost Efficiency`")),
               checkboxGroupInput(
                 "tins5","Primary Practices TINs",
                 choices = c("DUKE PRIMARY CARE"="DUKE PRIMARY CARE",
                             "PRIVATE DIAGNOSTIC CLINIC PLLC"="PRIVATE DIAGNOSTIC CLINIC PLLC"
                             #"CAROLINA FAMILY HEALTH CENTERS"="CAROLINA FAMILY HEALTH CENTERS",
                             #"VANCE FAMILY MEDICINE"="VANCE FAMILY MEDICINE",
                             #"DUKE LIFEPOINT MARIA PARHAM PHYSICIAN PRACTICES, LLC"="DUKE LIFEPOINT MARIA PARHAM PHYSICIAN PRACTICES, LLC",
                             #"ROXBORO MEDICAL ASSOCIATES"="ROXBORO MEDICAL ASSOCIATES",
                             #"TRIANGLE COMMUNITY PHYSICIANS, PA"="TRIANGLE COMMUNITY PHYSICIANS, PA",
                             #"BECKFORD AVENUE MEDICAL ASSOCIATES"="BECKFORD AVENUE MEDICAL ASSOCIATES ",
                             #"NORTH STATE MEDICAL CENTER"="NORTH STATE MEDICAL CENTER",
                             #"LINCOLN COMMUNITY HEALTH CENTER"="LINCOLN COMMUNITY HEALTH CENTER",
                             #"DUKE LIFEPOINT WILSON PHYSICIAN PRACTICES"="DUKE LIFEPOINT WILSON PHYSICIAN PRACTICES",
                             #"SUNDAR INTERNAL MEDICINE ASSOCIATES, INC"="SUNDAR INTERNAL MEDICINE ASSOCIATES, INC",
                             #"INTERNAL MEDICINE OF WAKEFIELD"="INTERNAL MEDICINE OF WAKEFIELD ",
                             #"DUKE UNIVERSITY HEALTH SYSTEMS INC"="DUKE UNIVERSITY HEALTH SYSTEMS INC",
                             #"ROXBORO INTERNAL MEDICINE & PEDS"="ROXBORO INTERNAL MEDICINE & PEDS",
                             #"ALLMED CLINIC, PA"="ALLMED CLINIC, PA",
                             #"PRIMARY MEDICAL CARE"="PRIMARY MEDICAL CARE"
                             ),
                 selected = c("DUKE PRIMARY CARE", "PRIVATE DIAGNOSTIC CLINIC PLLC", "CAROLINA FAMILY HEALTH CENTERS",
                              "VANCE FAMILY MEDICINE")
               ),
               selectInput("eligibility5","Eligibility Reasons",
                           list("All"="all",
                                "ESRD"="ESRD",
                                "Disabled"="Disabled",
                                "Aged/Nondual"="NonDual",
                                "Aged/Dual"="Dual"
                           )),
               selectInput("comorbidities5", "Comorbidities",
                           list("All"="all",
                                "Cancer"="HCC_Cancer",
                                "CAD"="HCC_CAD",
                                "Diabetes"="HCC_Diabetes",
                                "CHF"="HCC_85",
                                "COPD"="HCC_111",
                                "CKD"="HCC_CKD")),
               selectInput("primarycare5","Primary Practices",
                           list("All"="all",
                                "Primary Care"="Primary Care",
                                "Specialists"="Specialist"
                           ))
               
             ),
             mainPanel(
               plotOutput("bar3"),
               tableOutput("tab5")
             )
           )
  )
  ),
  tabPanel("Primary Practices",
               uiOutput("history"),
               plotlyOutput("pie"),
               uiOutput("back"),
               plotlyOutput("heatmap2"))

  
))