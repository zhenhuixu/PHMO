# PHMO
This project aims to provide information on the 2021 financial plan of Duke Connected Care (DCC). The data is 2019 EHR DCC electronic health records (EHR). The dataset contains protected health information (PHI). Thus, the data would not be uploaded to github.



### Primary Outcome

* Cost of Care

* Cost Efficiency

### Explanatory Variables

* Age
* Gender
* MRI
* CT scans
* Total Hospital Discharges
* Unplanned Admissions
* Readmissions
* Discharges to SNF
* Out of Network 
* Emergency Department (ED) Visits
* Primary Care Physicians Visits
* Total Primary Care Visits



### Statistical Analysis Plan

1. Univariate linear regression models are used to provide information on the association between explanatory variables and outcome variables.
2. Multivariate linear regression models are used to provide information on the association between hospital utilizations and outcome variables after adjusting for demographical data.
3. Interactive visualization tool (ShinyApp) is  built to help clinicians or other users dive into the data without coding.



### Desciption of tables

* Table 1  

  Descriptive summary statistics are shown in this table. Median, 25% quartile, 75% quartile are reported for the overall population and top 10% population respectively. The top 10% population is defined as the beneficiaries whose cost of care ranks top 10% in 2019.

* Table 2

  The outcome variables are cost of care and cost efficiency. The model is univariate models. The association between explantory outcome variables and explantory variables are shown in the form of the estimate beta coefficients and confidence interval. 

  Here is the example of interpretations of beta coefficient:

  ​		One unit increase in **x** is associated with **beta** increase/decrease in y.

  Please note that this interpretation is only valid when the confidence interval do not include 0. If the confidence interval include 0, there is no statistically significant association between these two variables.

  

  