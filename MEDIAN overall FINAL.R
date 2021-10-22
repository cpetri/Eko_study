#install gtsummary 

#run gtsummary 
library(gtsummary)
library(tidyverse)
library(dplyr) 


# Change the maximum amount shown on the screen
options(max.print = 3000)

#open DUOEF csv file 
baselineDUO <- read.csv(file.choose("Macintosh HD:/Users/seri/Desktop/DUO-EF/DUO-EF DATA/DUOEF19_DATA_2021-02-05_1529"), 
                        stringsAsFactors = TRUE, na.strings = c("NA",""))

#prepare baseline echo_sex data to add new column 
baselineDUO <- mutate(baselineDUO,Female = echo_sex )
baselineDUO$echo_sex <- as.numeric(baselineDUO$echo_sex)


baselineDUO$echo_lvef_percentage <- as.numeric(baselineDUO$echo_lvef_percentage)


baselineDUO <-baselineDUO %>% mutate(HF_classification = case_when( baselineDUO$echo_lvef_percentage <= 40 ~ 'LVEF ≤ 40', 
                                                                    baselineDUO$echo_lvef_percentage > 40 ~ 'LVEF > 40')) 

baselineDUO$HF_classification <- as.factor(baselineDUO$HF_classification)


# Divide age into categorical groups 
baselineDUO <- baselineDUO %>% mutate(AgeGroup
                                      = case_when(baselineDUO$echo_age >= 18  & baselineDUO$echo_age < 70 ~ '18 - 69', 
                                                  baselineDUO$echo_age >=70  ~ '70+' ))







# change recode subcategory labels for ethnicity 
baselineDUO$echo_ethnicity <- recode( baselineDUO$echo_ethnicity,
                                      '1' = "White", 
                                      '2' = "White", 
                                      '3'= "White",
                                      '4' = "White", 
                                      '5'= "Mixed", 
                                      '6'= "Mixed", 
                                      '7'= "Mixed",
                                      '8' = "Mixed",
                                      '9'= "Asian", 
                                      '10'= "Asian", 
                                      '11'= "Asian", 
                                      '12'= "Asian",
                                      '13' = "Asian", 
                                      '14' = "Black", 
                                      '15' = "Black", 
                                      '16' = "Black", 
                                      '17' = "Other",
                                      '18' = "Other")

# change recode subcategory labels for source of referral
baselineDUO$echo_ref_source <- recode( baselineDUO$echo_ref_source,'1' = "GP (community)", 
                                       '2' = "Inpatient", '3' = "Outpatient")




#alcohol PMH merge the 2 columns to form 1 overall column for smoking
baselineDUO <- baselineDUO %>% mutate(smoking =case_when(baselineDUO$echo_noncardiac_pmh___7 ==1 | baselineDUO$echo_noncardiac_pmh___8 ==1 ~ '1', 
                                                         baselineDUO$echo_noncardiac_pmh___7 == 0 & baselineDUO$echo_noncardiac_pmh___8 ==0 ~ '0'))

baselineDUO <- mutate(baselineDUO,smoking = smoking )
baselineDUO$smoking<- as.numeric(baselineDUO$smoking)
#alcohol PMH merge the 2 columns to form 1 overall column for alcohol
baselineDUO <- baselineDUO %>% mutate(alcohol = case_when(baselineDUO$echo_noncardiac_pmh___9 ==1 |baselineDUO$echo_noncardiac_pmh___10 ==1 ~ '1', 
                                                          baselineDUO$echo_noncardiac_pmh___9 == 0 & baselineDUO$echo_noncardiac_pmh___10 ==0 ~ '0'))

baselineDUO <- mutate(baselineDUO,alcohol = alcohol )
baselineDUO$alcohol<- as.numeric(baselineDUO$alcohol)
#prepare baseline HF data to add new column for patients with PMH of Heart Failure 
baselineDUO <- mutate(baselineDUO,Heart_Failure = echo_hf_pmh___2 + echo_hf_pmh___3 + echo_hf_pmh___4 )



#prepare baseline HF data to add new column for patients with Pacemaker + CRT
baselineDUO <- baselineDUO %>% mutate(pacemaker_CRT = case_when(baselineDUO$echo_cardiac_pmh___8==1 |baselineDUO$echo_cardiac_pmh___9 ==1 ~ '1', 
                                                                baselineDUO$echo_cardiac_pmh___8 == 0 & baselineDUO$echo_cardiac_pmh___9 ==0 ~ '0'))


baselineDUO <- mutate(baselineDUO,pacemaker_CRT = pacemaker_CRT )
baselineDUO$pacemaker_CRT<- as.numeric(baselineDUO$pacemaker_CRT)


#let's see what we made! - you should be able to visualise the newly added HF column
head(baselineDUO)  






############## cardiac PMH################

#prepare baseline HTN data to add new column for patients with PMH of Hypertension 
baselineDUO <- mutate(baselineDUO,Hypertension = echo_cardiac_pmh___1 )

#prepare baseline MI data to add new column for patients with PMH of Myocardial Infarction 
baselineDUO <- mutate(baselineDUO,Myocardial_Infarction = echo_cardiac_pmh___2 )

#prepare baseline HTN data to add new column for patients with PMH of Ischaemic Heart Disease 
baselineDUO <- mutate(baselineDUO,Ischaemic_Heart_Disease = echo_cardiac_pmh___3 )

#prepare baseline MI data to add new column for patients with PMH of Atrial Fibrillation
baselineDUO <- mutate(baselineDUO,Atrial_Fibrillation = echo_cardiac_pmh___4 )    

#prepare baseline HTN data to add new column for patients with PMH of Other Arrhythmia 
baselineDUO <- mutate(baselineDUO,Other_Arrhythmia = echo_cardiac_pmh___5 )

#prepare baseline MI data to add new column for patients with PMH of Valvular Heart Disease 
baselineDUO <- mutate(baselineDUO, Valvular_Heart_Disease = echo_cardiac_pmh___6 )    

#prepare baseline MI data to add new column for patients with PMH of Implanted Cardioverter-Defibrilator
baselineDUO <- mutate(baselineDUO,Implanted_Cardioverter_Defibrilator = echo_cardiac_pmh___7 )        

#prepare baseline MI data to add new column for patients with PMH of Cardiac Resynchronisation Therapy
baselineDUO <- mutate(baselineDUO,Cardiac_Resynchronisation_Therapy = echo_cardiac_pmh___8 )        


#prepare baseline MI data to add new column for patients with PMH of Pacemaker
baselineDUO <- mutate(baselineDUO,Pacemaker = echo_cardiac_pmh___9 )    

#prepare baseline MI data to add new column for patients with PMH of Other history of Cardiac Disease
baselineDUO <- mutate(baselineDUO,Other_history_of_cardiac_disease = echo_cardiac_pmh___10 )        

#prepare baseline MI data to add new column for patients with PMH of Cardiac Interventions (e.g CABG/PCI/Valve surgery)
baselineDUO <- mutate(baselineDUO,Cardiac_Interventions = echo_cardiac_pmh___11 )         

#prepare baseline MI data to add new column for patients with PMH of Cardiomyopathy
baselineDUO <- mutate(baselineDUO,Cardiomyopathy = echo_cardiac_pmh___12 )         




############## non- cardiac PMH################    

#prepare baseline Diabetes data to add new column for patients with PMH of Diabetes 
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___1 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Obesity 
baselineDUO <- mutate(baselineDUO,Obesity = echo_noncardiac_pmh___2 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Stroke/TIA 
baselineDUO <- mutate(baselineDUO,Stroke  = echo_noncardiac_pmh___3 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Chronic Kidney Disease 
baselineDUO <- mutate(baselineDUO,Kidney = echo_noncardiac_pmh___4 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Cancer - active 
baselineDUO <- mutate(baselineDUO,Cancer = echo_noncardiac_pmh___5 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Cancer - previous  
baselineDUO <- mutate(baselineDUO,Cancer = echo_noncardiac_pmh___6 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Smoking  
baselineDUO <- mutate(baselineDUO,Smoking  = smoking )    

#prepare baseline Diabetes data to add new column for patients with PMH of Alcohol excess
baselineDUO <- mutate(baselineDUO,Alcohol = alcohol   )    

#prepare baseline Hypercholesterolaemia data to add new column for patients with PMH of Hypercholesteraemia 
baselineDUO <- mutate(baselineDUO,Hypercholesterolaemia = echo_noncardiac_pmh___11 )    

#prepare baseline Hypercholesterolaemia data to add new column for patients with PMH of Pregnancy 
baselineDUO <- mutate(baselineDUO,Pregnancy = echo_noncardiac_pmh___12 )    




############## overall ################    

#prepare baseline Diabetes data to add new column for patients with PMH of Diabetes 
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___1 )    

#prepare baseline Hypercholesterolaemia data to add new column for patients with PMH of Hypercholesteraemia 
baselineDUO <- mutate(baselineDUO,Hypercholesterolaemia = echo_noncardiac_pmh___13 )    

# create final summary table with information on past medical history
baselineDUO %>% 
  select(echo_age, 
         AgeGroup, 
         echo_sex,  
         echo_lvef_percentage,
         echo_ethnicity, 
         echo_cardiac_pmh___1, 
         echo_cardiac_pmh___2,
         echo_cardiac_pmh___4,
         echo_cardiac_pmh___6,
         echo_noncardiac_pmh___1,
         echo_noncardiac_pmh___3,
         echo_noncardiac_pmh___4,
         smoking,
         alcohol,
         echo_noncardiac_pmh___11,
         echo_noncardiac_pmh___12,
         echo_noncardiac_pmh___13,
         pacemaker_CRT) %>%
  
  tbl_summary(
    label = list(echo_age ~ "Median Age", 
                 AgeGroup ~ "Age Distribution",
                 echo_sex ~ "Male, n", 
                 echo_ethnicity ~ "Ethnicity, n", 
                 echo_lvef_percentage ~ "Median TTE LVEF %", 
                 echo_cardiac_pmh___1 ~ "Hypertension, n", 
                 echo_cardiac_pmh___2 ~ "Myocardial Infarction, n", 
                 echo_cardiac_pmh___4 ~ "Atrial Fibrillation, n", 
                 echo_cardiac_pmh___6 ~ "Valvular Heart Disease, n", 
                 echo_noncardiac_pmh___1 ~ "Diabetes, n",
                 echo_noncardiac_pmh___3 ~ "Stroke/TIA, n",
                 echo_noncardiac_pmh___4 ~ "Chronic Kidney Disease, n",
                 smoking ~ "Smoking (current or previos), n",
                 alcohol ~ "Alcohol excess (current or previous), n",
                 echo_noncardiac_pmh___11 ~ "Hypercholesterolaemia, n",
                 echo_noncardiac_pmh___12 ~ "Pregnancy, n",
                 echo_noncardiac_pmh___13 ~ "COPD, n" 
    ),
    type = list(echo_lvef_percentage ~ 'continuous', echo_ethnicity ~ 'categorical'),
    statistic = list(c(echo_age,echo_lvef_percentage) ~ "{median} ({IQR})")) %>% 
  bold_labels()




