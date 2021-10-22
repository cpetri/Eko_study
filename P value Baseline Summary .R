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

# change recode subcategory labels for sex 
baselineDUO$echo_sex <- recode( baselineDUO$echo_sex, '1' = "Male", '0'= "Female")

baselineDUO$echo_lvef_percentage <- as.numeric(baselineDUO$echo_lvef_percentage)


baselineDUO <-baselineDUO %>% mutate(HF_classification = case_when( baselineDUO$echo_lvef_percentage <= 40 ~ 'LVEF ≤ 40', 
                                                                    baselineDUO$echo_lvef_percentage > 40 ~ 'LVEF > 40')) 

baselineDUO$HF_classification <- as.factor(baselineDUO$HF_classification)


# Divide age into categorical groups 
baselineDUO <- baselineDUO %>% mutate(AgeGroup
                                      = case_when(baselineDUO$echo_age >= 0  & baselineDUO$echo_age < 40 ~ '< 40', 
                                                  baselineDUO$echo_age >= 40  & baselineDUO$echo_age <= 49 ~ '40 -49',
                                                  baselineDUO$echo_age >= 50  & baselineDUO$echo_age <= 59 ~ '50 -59',
                                                  baselineDUO$echo_age >= 60  & baselineDUO$echo_age <= 69 ~ '60 -69',
                                                  baselineDUO$echo_age >= 70  & baselineDUO$echo_age <= 79 ~ '70 -79',
                                                  baselineDUO$echo_age >=80  ~ '80+',
                                                  
                                                  
                                      ))


# change recode subcategory labels for ethnicity 
baselineDUO$echo_ethnicity <- recode( baselineDUO$echo_ethnicity,'1' = "English / Welsh / Scottish / N Irish/ British", '2' = "Irish", '3'= "Gypsy/Irish Traveller",
                                      '4' = "Any other White background", '5'= "White and Black Caribbean", 
                                      '6'= "White and Black African", '7'= "White and Asian",
                                      '8' = "Any other Mixed/Multiple ethnic background",
                                      '9'= "Indian", '10'= "Pakistani", '11'= "Bangladeshi", '12'= "Chinese",
                                      '13' = "Other Asian background", '14' = "African", '15' = "Caribbean", 
                                      '16' = "Any other Black/ African/Caribbean background", '17' = "Arab",
                                      '18' = "Any other ethnic group")

# change recode subcategory labels for source of referral
baselineDUO$echo_ref_source <- recode( baselineDUO$echo_ref_source,'1' = "GP (community)", 
                                       '2' = "Inpatient", '3' = "Outpatient")


#prepare baseline HF data to add new column for patients with PMH of Heart Failure 
baselineDUO <- mutate(baselineDUO,Heart_Failure = echo_hf_pmh___2 + echo_hf_pmh___3 + echo_hf_pmh___4 )

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
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___2 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Stroke/TIA 
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___3 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Chronic Kidney Disease 
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___4 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Cancer - active 
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___5 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Cancer - previous  
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___6 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Smoking - current  
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___7 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Smoking - ex 
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___8 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Alcohol excess - current 
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___9 )    

#prepare baseline Diabetes data to add new column for patients with PMH of Alcohol excess - previous   
baselineDUO <- mutate(baselineDUO,Diabetes = echo_noncardiac_pmh___10 )    

#prepare baseline Hypercholesterolaemia data to add new column for patients with PMH of Hypercholesteraemia 
baselineDUO <- mutate(baselineDUO,Hypercholesterolaemia = echo_noncardiac_pmh___11 )    

#prepare baseline Hypercholesterolaemia data to add new column for patients with PMH of Pregnancy 
baselineDUO <- mutate(baselineDUO,Hypercholesterolaemia = echo_noncardiac_pmh___12 )    



##########Percentage with LVEF <40% ##############
baselineDUO %>% 
  select(echo_age, 
         AgeGroup, 
         echo_sex, 
         echo_lvef_percentage, 
         echo_ref_source, 
         HF_classification,
         echo_ethnicity, 
         Heart_Failure, 
         echo_cardiac_pmh___1, 
         echo_cardiac_pmh___2,
         echo_cardiac_pmh___3,
         echo_cardiac_pmh___4,
         echo_cardiac_pmh___6, 
         echo_cardiac_pmh___7, 
         echo_cardiac_pmh___8,
         echo_cardiac_pmh___9,
         echo_cardiac_pmh___11,
         echo_cardiac_pmh___12,
         echo_noncardiac_pmh___1,
         echo_noncardiac_pmh___2,
         echo_noncardiac_pmh___3,
         echo_noncardiac_pmh___4,
         echo_noncardiac_pmh___11,
         echo_noncardiac_pmh___13) %>%
  group_by(HF_classification) %>% 
  
  
  tbl_summary(
    by = HF_classification,
    label = list(echo_age ~ "Mean Age", 
                 AgeGroup ~ "Age Distribution",
                 echo_sex ~ "Sex, n", 
                 echo_lvef_percentage ~ "Mean Echo LVEF",
                 echo_ref_source ~ "Referral source", 
                 HF_classification ~ "LVEF ≤ 40%",
                 echo_ethnicity ~ "Ethnicity, n", 
                 Heart_Failure ~ "Heart Failure, n", 
                 echo_cardiac_pmh___1 ~ "Hypertension, n", 
                 echo_cardiac_pmh___2 ~ "Myocardial Infarction, n", 
                 echo_cardiac_pmh___3 ~ "Ischaemic Heart Disease, n", 
                 echo_cardiac_pmh___4 ~ "Atrial Fibrillation, n" ,
                 echo_cardiac_pmh___6 ~ "Valvular Heart Disease, n", 
                 echo_cardiac_pmh___7 ~ "Implanted Cardioverter-Defibrilator, n", 
                 echo_cardiac_pmh___8 ~ "Cardiac Resynchronisation Therapy, n", 
                 echo_cardiac_pmh___9 ~ "Pacemaker, n", 
                 echo_cardiac_pmh___11 ~ "Cardiac Interventions e.g CABG/PCI/Valve Surgery, n",
                 echo_cardiac_pmh___12 ~ "Cardiomyopathy, n", 
                 echo_noncardiac_pmh___1 ~ "Diabetes, n",
                 echo_noncardiac_pmh___2 ~ "Obesity, n",
                 echo_noncardiac_pmh___3 ~ "Stroke/TIA, n",
                 echo_noncardiac_pmh___4 ~ "Chronic Kidney Disease, n",
                 echo_noncardiac_pmh___11 ~ "Hypercholesterolaemia, n",
                 echo_noncardiac_pmh___13 ~ "Chronic Obstructive Pulmonary Disease, n"
                ),
    type = list(echo_lvef_percentage ~ 'continuous', echo_ethnicity ~ 'categorical', echo_sex ~ 'categorical', HF_classification ~ 'categorical'),
    statistic = list(c(echo_age,echo_lvef_percentage) ~ "{mean} ({sd})")) %>% 
  bold_labels() %>% 
  add_p(list(echo_age ~ "t.test",
             AgeGroup ~ "chisq.test",
             echo_sex ~ "chisq.test", 
             echo_lvef_percentage ~ "t.test",
             Heart_Failure ~ "chisq.test", 
             echo_cardiac_pmh___1 ~ "chisq.test", 
             echo_cardiac_pmh___2 ~ "chisq.test", 
             echo_cardiac_pmh___3 ~ "chisq.test", 
             echo_cardiac_pmh___4 ~ "chisq.test", 
             echo_cardiac_pmh___6 ~ "chisq.test", 
             echo_cardiac_pmh___7 ~ "chisq.test", 
             echo_cardiac_pmh___8 ~ "chisq.test", 
             echo_cardiac_pmh___9 ~ "chisq.test", 
             echo_cardiac_pmh___11 ~ "chisq.test",
             echo_cardiac_pmh___12 ~ "chisq.test", 
             echo_noncardiac_pmh___1 ~ "chisq.test",
             echo_noncardiac_pmh___2 ~ "chisq.test",
             echo_noncardiac_pmh___3 ~ "chisq.test",
             echo_noncardiac_pmh___4 ~ "chisq.test",
             echo_noncardiac_pmh___11 ~ "chisq.test",
             echo_noncardiac_pmh___13 ~ "chisq.test"), 
        test.args = all_tests("t.test") ~ list(var.equal = TRUE), include= -c(echo_ethnicity, echo_ref_source))




####overall ####

baselineDUO %>% 
  select(echo_age, 
         AgeGroup, 
         echo_sex, 
         echo_lvef_percentage, 
         echo_ref_source, 
         HF_classification,
         echo_ethnicity, 
         Heart_Failure, 
         echo_cardiac_pmh___1, 
         echo_cardiac_pmh___2,
         echo_cardiac_pmh___3,
         echo_cardiac_pmh___4,
         echo_cardiac_pmh___6, 
         echo_cardiac_pmh___7, 
         echo_cardiac_pmh___8,
         echo_cardiac_pmh___9,
         echo_cardiac_pmh___11,
         echo_cardiac_pmh___12,
         echo_noncardiac_pmh___1,
         echo_noncardiac_pmh___2,
         echo_noncardiac_pmh___3,
         echo_noncardiac_pmh___4,
         echo_noncardiac_pmh___11,
         echo_noncardiac_pmh___13) %>%


  tbl_summary(
    label = list(echo_age ~ "Mean Age", 
                 AgeGroup ~ "Age Distribution",
                 echo_sex ~ "Sex, n", 
                 echo_lvef_percentage ~ "Mean Echo LVEF",
                 echo_ref_source ~ "Referral source", 
                 HF_classification ~ "LVEF ≤ 40%",
                 echo_ethnicity ~ "Ethnicity, n", 
                 Heart_Failure ~ "Heart Failure, n", 
                 echo_cardiac_pmh___1 ~ "Hypertension, n", 
                 echo_cardiac_pmh___2 ~ "Myocardial Infarction, n", 
                 echo_cardiac_pmh___3 ~ "Ischaemic Heart Disease, n", 
                 echo_cardiac_pmh___4 ~ "Atrial Fibrillation, n" ,
                 echo_cardiac_pmh___6 ~ "Valvular Heart Disease, n", 
                 echo_cardiac_pmh___7 ~ "Implanted Cardioverter-Defibrilator, n", 
                 echo_cardiac_pmh___8 ~ "Cardiac Resynchronisation Therapy, n", 
                 echo_cardiac_pmh___9 ~ "Pacemaker, n", 
                 echo_cardiac_pmh___11 ~ "Cardiac Interventions e.g CABG/PCI/Valve Surgery, n",
                 echo_cardiac_pmh___12 ~ "Cardiomyopathy, n", 
                 echo_noncardiac_pmh___1 ~ "Diabetes, n",
                 echo_noncardiac_pmh___2 ~ "Obesity, n",
                 echo_noncardiac_pmh___3 ~ "Stroke/TIA, n",
                 echo_noncardiac_pmh___4 ~ "Chronic Kidney Disease, n",
                 echo_noncardiac_pmh___11 ~ "Hypercholesterolaemia, n",
                 echo_noncardiac_pmh___13 ~ "Chronic Obstructive Pulmonary Disease, n"
    ),
    type = list(echo_lvef_percentage ~ 'continuous', echo_ethnicity ~ 'categorical', echo_sex ~ 'categorical', HF_classification ~ 'categorical'),
    statistic = list(c(echo_age,echo_lvef_percentage) ~ "{mean} ({sd})")) %>% 
  bold_labels()
