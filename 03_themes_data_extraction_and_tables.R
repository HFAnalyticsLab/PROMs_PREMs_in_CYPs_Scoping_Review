#Data cleaning and extraction from Mural  

rm(list=ls())

# Libraries ---------------------------------------------------------------

library(flextable)
library(here)
library(tidyverse)
library(tidyr)
library(janitor)
library(stringr)
library(gtsummary)
library(readxl)
library(ggh4x)
library(DescTools)
library(officer)


source(here('for_reference', 'source.R'))
source('functions.R')



# Get study demographics -------------------------------------------------------
raw<-read_csv(paste0(data_loc,'/raw_data4.csv'))

raw<-raw %>% 
  clean_names() %>% 
  mutate(collection= case_when(str_detect(collection_method, 
                                          regex("mix", ignore_case = TRUE))~"mixed", 
                               str_detect(collection_method,
                                          regex("phone and", ignore_case = TRUE))~"mixed",
                               str_detect(collection_method,
                                          regex("and electronic", ignore_case = TRUE))~"mixed",
                               str_detect(collection_method, 
                                          regex("pen", ignore_case = TRUE))~"pen_and_ppr",
                               str_detect(collection_method, 
                                          regex("paper", ignore_case = TRUE))~"pen_and_ppr",
                               str_detect(collection_method, 
                                          regex("stated", ignore_case = TRUE))~"not_stated",
                               str_detect(collection_method, 
                                          regex("specified", ignore_case = TRUE))~"not_stated",
                               str_detect(collection_method, 
                                          regex("mention", ignore_case = TRUE))~"not_stated",
                               str_detect(collection_method, 
                                          regex("telephone", ignore_case = TRUE))~"telephone",
                               str_detect(collection_method, 
                                          regex("ePRO", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("electronic", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("elec", ignore_case = TRUE))~"electronic", 
                               str_detect(collection_method, 
                                          regex("web", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("online", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("KLIK", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("ele", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("computer", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("n/a", ignore_case = TRUE))~"not applicable",
                               is.na(collection_method)~ "not applicable",
                               TRUE~"not applicable"))

clean_df<-raw %>% 
  mutate(type=case_when((is.na(prem_s_used_including_tool_details) | str_detect(prem_s_used_including_tool_details, 
                                                                                regex("n/a", ignore_case = TRUE))) 
                        & (!is.na(prom_s_reported_including_tool_details)| 
                             !str_detect(prom_s_reported_including_tool_details,regex("n/a", ignore_case = TRUE)))~ "PROM", 
                        (!is.na(prem_s_used_including_tool_details) | !str_detect(prem_s_used_including_tool_details, 
                                                                                  regex("n/a", ignore_case = TRUE))) 
                        & (is.na(prom_s_reported_including_tool_details)| str_detect(prom_s_reported_including_tool_details,
                                                                                     regex("n/a", ignore_case = TRUE)))~ "PREM", 
                        (!is.na(prem_s_used_including_tool_details) | !str_detect(prem_s_used_including_tool_details, 
                                                                                  regex("n/a", ignore_case = TRUE))) 
                        & (!is.na(prom_s_reported_including_tool_details)| !str_detect(prom_s_reported_including_tool_details,
                                                                                       regex("n/a", ignore_case = TRUE)))~ "PROM_and_PREM")) 


clean_df<-clean_df %>% 
  mutate(participant=case_when(participant_type_s=="Patient"~ "Patient", 
                               participant_type_s=="Proxy"~ "Proxy", 
                               participant_type_s=="Clinician"~ "Clinician", 
                               participant_type_s=="Patient and proxy"~ "Patient and proxy",
                               participant_type_s=="Clinician and proxy"~ "Clinician and proxy",
                               participant_type_s=="Patient, proxy and clinician"~ "Patient, proxy and clinician", 
                               TRUE ~ NA_character_)) 

clean_df<-clean_df %>% 
  mutate(outpatient=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("outpatient", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(inpatient=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("inpatient", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("wad", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("in and", ignore_case = TRUE))~1,
                             TRUE~0)) %>%
  rowwise() %>% 
  mutate(mixture=sum(across(c(outpatient:inpatient)))) %>% 
  ungroup() %>% 
  mutate(mixture=ifelse(mixture>1,1,0)) %>% 
  mutate(not_stated=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("stated", ignore_case = TRUE))~1,
                              (inpatient==0 & outpatient==0 & mixture==0) ~ 1,
                              TRUE~0)) %>% 
  rowwise() %>% 
  mutate(check_setting=sum(across(c(outpatient:not_stated)))) %>% 
  ungroup() 


clean_df<-clean_df %>%   
  mutate(cardio=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                     regex("cardi", ignore_case = TRUE))~1, 
                          str_detect(context_setting_e_g_ward_emergency_care, 
                                     regex("heart", ignore_case = TRUE))~1,
                          TRUE~0)) %>% 
  mutate(mental_health=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                            regex("mental", ignore_case = TRUE))~1, 
                                 str_detect(context_setting_e_g_ward_emergency_care, 
                                            regex("CAMHS", ignore_case = TRUE))~1,
                                 TRUE~0)) %>% 
  mutate(surgery=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("surgery", ignore_case = TRUE))~1, 
                           TRUE~0)) %>% 
  mutate(oncology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                       regex("oncology", ignore_case = TRUE))~1,
                            str_detect(context_setting_e_g_ward_emergency_care, 
                                       regex("cancer", ignore_case = TRUE))~1,
                            str_detect(context_setting_e_g_ward_emergency_care, 
                                       regex("cance", ignore_case = TRUE))~1,
                            TRUE~0)) %>%  
  mutate(palliative=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("palliative", ignore_case = TRUE))~1,
                              str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("end", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(respiratory=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                          regex("asthma", ignore_case = TRUE))~1,
                               str_detect(context_setting_e_g_ward_emergency_care, 
                                          regex("respiratory", ignore_case = TRUE))~1,
                               str_detect(context_setting_e_g_ward_emergency_care, 
                                          regex("cystic", ignore_case = TRUE))~1,
                               TRUE~0)) %>% 
  mutate(burn=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                   regex("burn", ignore_case = TRUE))~1,
                        TRUE~0)) %>% 
  mutate(dermatology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                          regex("derm", ignore_case = TRUE))~1,
                               TRUE~0)) %>% 
  mutate(psychology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("psycho", ignore_case = TRUE))~1,
                              str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("DBT", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(psychiatry=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("psychi", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(nephrology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("neph", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(neurology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("neuro", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("brain", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("Epili", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("spinal", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("cranio", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        "Epilepsy")~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("TSC", ignore_case = TRUE))~1,
                             TRUE~0)) %>% 
  mutate(rheumatology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("rheu", ignore_case = TRUE))~1,
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("athritis", ignore_case = TRUE))~1, 
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("JIA", ignore_case = TRUE))~1,
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("Systemic Sclerosis", ignore_case = TRUE))~1,
                                TRUE~0)) %>%  
  mutate(haematology =case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("hemo", ignore_case = TRUE))~1,
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("hema", ignore_case = TRUE))~1, 
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("blood", ignore_case = TRUE))~1,
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("sickle", ignore_case = TRUE))~1,
                                TRUE~0)) %>% 
  mutate(gastroenterology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                               regex("gastro", ignore_case = TRUE))~1,
                                    str_detect(context_setting_e_g_ward_emergency_care, 
                                               regex("IBD", ignore_case = TRUE))~1,
                                    TRUE~0)) %>%  
  mutate(obstetrics_neonatology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                                     regex("obs", ignore_case = TRUE))~1,
                                          str_detect(context_setting_e_g_ward_emergency_care, 
                                                     regex("neona", ignore_case = TRUE))~1,
                                          TRUE~0)) %>% 
  mutate(opthalmology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("Ophthal", ignore_case = TRUE))~1,
                                str_detect(context_setting_e_g_ward_emergency_care,"Opthalmology")~1,
                                TRUE~0)) %>% 
  mutate(endocrinology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                            regex("endo", ignore_case = TRUE))~1,
                                 str_detect(context_setting_e_g_ward_emergency_care, 
                                            regex("diabetes", ignore_case = TRUE))~1,
                                 str_detect(context_setting_e_g_ward_emergency_care, 
                                            regex("adrenal", ignore_case = TRUE))~1,
                                 TRUE~0)) %>% 
  mutate(orthopaedic=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                          regex("ortho", ignore_case = TRUE))~1,
                               TRUE~0)) %>% 
  mutate(immunology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("immu", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(gender_services=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                              regex("gender", ignore_case = TRUE))~1,
                                   TRUE~0)) %>% 
  mutate(pain=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                   regex("pai", ignore_case = TRUE))~1,
                        str_detect(context_setting_e_g_ward_emergency_care,"pain")~1,
                        str_detect(context_setting_e_g_ward_emergency_care, 
                                   regex("pain", ignore_case = TRUE))~1,
                        TRUE~0)) %>% 
  mutate(urology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("urology", ignore_case = TRUE))~1,
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      "urology")~1,
                           TRUE~0)) %>% 
  mutate(occupational_therapy=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                                   regex("Occupational", ignore_case = TRUE))~1,
                                        TRUE~0)) %>% 
  mutate(transplant=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("transp", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(rehabilitation=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                             regex("rehab", ignore_case = TRUE))~1,
                                  TRUE~0)) %>% 
  mutate(intensive=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("intensive", ignore_case = TRUE))~1,
                             TRUE~0)) %>% 
  mutate(transition=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("transition", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(allergy=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("allergy", ignore_case = TRUE))~1,
                           TRUE~0)) %>% 
  mutate(not_applicable=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                             regex("n/a", ignore_case = TRUE))~1,
                                  str_detect(context_setting_e_g_ward_emergency_care, 
                                             regex("NA", ignore_case = FALSE))~1,
                                  is.na(context_setting_e_g_ward_emergency_care)~1,
                                  TRUE~0)) %>% 
  rowwise() %>% 
  mutate(sum_care=sum(across(c(cardio:not_applicable)))) %>% 
  ungroup() %>% 
  mutate(not_stated_speciality=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                                    regex("stated", ignore_case = TRUE))~1,
                                         sum_care==0 ~ 1,
                                         TRUE~0)) %>% 
  ungroup() 


clean_df<-clean_df %>%   
  mutate(context=case_when(is.na(context_setting_e_g_ward_emergency_care)~ "not_stated", 
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("n/a", ignore_case = TRUE))~"not_stated",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("network", ignore_case = TRUE))~"registry_network",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("registry", ignore_case = TRUE))~"registry_network",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("centres", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      "^centres$")~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("hospitals", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("clinics", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("trusts", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("units", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("groups", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("stated", ignore_case = TRUE))~"not_stated",
                           TRUE~"single_centre")) 


clean_df<-clean_df %>% 
  mutate(country=ifelse(str_detect(country_in_which_the_study_conducted, "Other: [A-Z]"), gsub("Other: ", "",country_in_which_the_study_conducted),
                        country_in_which_the_study_conducted)) %>% 
  mutate(country=gsub("Other:", "Other", country)) %>% 
  mutate(country=gsub("UK", "United Kingdom", country)) %>% 
  mutate(country=gsub("USA", "United States", country)) %>% 
  mutate(country=gsub("US", "United States", country)) %>% 
  mutate(country=ifelse(str_detect(country, "and "),"multiple_countries", country)) %>% 
  mutate(country=ifelse(is.na(country), "Not stated", country)) %>% 
  mutate(country=gsub("Europe", "multiple_countries", country))

demog<-clean_df %>% 
  mutate(covidence_number=as.character(covidence_number)) %>% 
  select(covidence_number, author=study_id_2, title=title_3,year_of_publication, type, country,
         collection,context,participant, study_design, participants_details_e_g_age_sex_and_number, prom_s_reported_including_tool_details, 
         prem_s_used_including_tool_details,
         cardio:not_stated_speciality) %>%
  select(-sum_care) %>%
  pivot_longer(cardio:not_stated_speciality, names_to="speciality", values_to="count") %>% 
  group_by(covidence_number, author, type, country,
           collection,context,participant, study_design, participants_details_e_g_age_sex_and_number, prom_s_reported_including_tool_details, 
           prem_s_used_including_tool_details) %>% 
  filter(count>0) %>% 
  summarise(speciality=paste(speciality, collapse=",")) %>% 
  mutate_at(vars(!starts_with("t")), list(clean_up)) %>% 
  mutate_at(vars(!starts_with("t")), list(title)) %>% 
  mutate(type=case_when(type=="PROM"~ "PROMs",
                        type=="PREM"~ "PREMs",
                        type=="PROM_and_PREM"~ "PROMs and PREMs")) 



# Themes from mural -------------------------------------------------------




# How PROMs and PREMs are used  -------------------------------------------

results<-read_excel(paste0(dt_loc,'/mural_data.xlsx'), sheet='How_they_are_used')


#Tally table in results

tally_one_level(raw=results)

used_tally <-tally_findings

write.csv(used_tally, here('results', 'used_tally.csv'))

extract_cleaning_one_level(raw=results)

#Summary Table
used_df<-table_findings

# How PROMs and PREMs are applied in clinical practice --------------------

data_applied<-read_excel(paste0(dt_loc,'/mural_data.xlsx'), sheet='how_data_is_applied')

#Tally table for results
tally_one_level(raw=data_applied)

data_applied_tally<-tally_findings

write.csv(data_applied_tally, here('results', 'data_applied_tally.csv'))


#For Summary table 
extract_cleaning_one_level(raw=data_applied)
data_applied_df<-table_findings



# How PROMs and PREMs are used for development of services ---------------

data_develop<-read_excel(paste0(dt_loc,'/mural_data.xlsx'), sheet='development_of_services')

#Tally table for results

tally_one_level(raw=data_develop)

data_develop_tally<-tally_findings

write.csv(data_develop_tally, here('results', 'data_develop_tally.csv'))

#Summary Table

extract_cleaning_one_level(raw=data_develop)

data_develop_df<-table_findings


# Patient groups ----------------------------------------------------------

pt_groups<-read_excel(paste0(dt_loc,'/mural_data.xlsx'), sheet='patient_groups')

tally_one_level(raw=pt_groups)

pt_groups_tally<-tally_findings

write.csv(pt_groups_tally, here('results', 'pt_groups_tally.csv'))

#Summary Table


extract_cleaning_one_level(raw=pt_groups)

pt_groups_df<-table_findings

# CYP_vs_proxies ----------------------------------------------------------

cyp_groups<-read_excel(paste0(dt_loc,'/mural_data.xlsx'), sheet='CYP_vs_Proxies')

#Tally Table for results
tally_one_level(raw=cyp_groups)

cyp_groups_tally<-tally_findings

write.csv(cyp_groups_tally, here('results', 'cyp_groups.csv'))

#Summary Table

extract_cleaning_one_level(raw=cyp_groups)

cyp_groups_df<-table_findings



# Barriers ----------------------------------------------------------------

barriers<-read_excel(paste0(dt_loc,'/mural_data.xlsx'), sheet='barriers')

#Summary Table

extract_cleaning_twolevels(raw=barriers)

barriers_df<-table_findings

#write.csv(barriers_df, here('results', 'barriers_overview.csv'))


# Facilitators ------------------------------------------------------------

facilitators<-read_excel(paste0(dt_loc,'/mural_data.xlsx'), sheet='facilitators')

#Summary Table
extract_cleaning_twolevels(raw=facilitators)

facilitators_df<-table_findings

#write.csv(facilitators_df, here('results', 'facilitators_overview.csv'))


# Summary tables in word ---------------------------------------------------
# Create a Word document
doc <- read_docx()

start.landscape(doc)

#Insert how results were used table
heading_text <- "Table 1: How PROMs/PREMs were used"

my_flextable <- flextable(used_df)
my_flextable <- set_table_properties(my_flextable, width = .9, layout = "autofit")
my_flextable<-fontsize(my_flextable, size=8, part="body")
doc <- body_add_par(doc, value = heading_text, style = "heading 1")
doc <- body_add_flextable(doc, value = my_flextable)

# Insert how data is applied table 
heading_text <- "Table 2: How PROMs/PREMs data were applied"

my_flextable <- flextable(data_applied_df)  
my_flextable <- set_table_properties(my_flextable, width = .9, layout = "autofit")
my_flextable<-fontsize(my_flextable, size=8, part="body")
doc <- body_add_par(doc, value = heading_text, style = "heading 1")
doc <- body_add_flextable(doc, value = my_flextable)

# Insert how data is used in service development table 
heading_text <- "Table 3: How PROMs/PREMs data were used for service development"

my_flextable <- flextable(data_develop_df)  
my_flextable <- set_table_properties(my_flextable, width = .9, layout = "autofit")
my_flextable<-fontsize(my_flextable, size=8, part="body")
doc <- body_add_par(doc, value = heading_text, style = "heading 1")
doc <- body_add_flextable(doc, value = my_flextable)


# Insert which patient groups routine collection is not integral for
heading_text <- "Table 4: Which patient groups routine colleciton is not integral for"

my_flextable <- flextable(pt_groups_df)  
my_flextable <- set_table_properties(my_flextable, width = .9, layout = "autofit")
my_flextable<-fontsize(my_flextable, size=8, part="body")
doc <- body_add_par(doc, value = heading_text, style = "heading 1")
doc <- body_add_flextable(doc, value = my_flextable)

# Insert CYPs vs proxy reporting
heading_text <- "Table 5: CYPs vs Proxies report"

my_flextable <- flextable(cyp_groups_df)  
my_flextable <- set_table_properties(my_flextable, width = .9, layout = "autofit")
my_flextable<-fontsize(my_flextable, size=8, part="body")
doc <- body_add_par(doc, value = heading_text, style = "heading 1")
doc <- body_add_flextable(doc, value = my_flextable)

# barriers
heading_text <- "Barriers"

my_flextable <- flextable(barriers_df) 
my_flextable <- set_table_properties(my_flextable, width = .9, layout = "autofit")
my_flextable<-fontsize(my_flextable, size=8, part="body")
doc <- body_add_par(doc, value = heading_text, style = "heading 1")
doc <- body_add_flextable(doc, value = my_flextable)

# facilitators
heading_text <- "Facilitators"


my_flextable <- flextable(facilitators_df)  
my_flextable <- set_table_properties(my_flextable, width = .9, layout = "autofit")
my_flextable<-fontsize(my_flextable, size=8, part="body")
doc <- body_add_par(doc, value = heading_text, style = "heading 1")
doc <- body_add_flextable(doc, value = my_flextable)

end.landscape(doc)

print(doc, target=here('results', 'trial.docx'))


# extract_references_to_Check ------------------------------------------------------

references_one_level(raw=results)

results_refs<-tab_df

write.csv(results_refs, here('results', 'results_refs.csv'))


references_one_level(raw=data_applied)

data_applied_refs<-tab_df

write.csv(data_applied_refs, here('results', 'data_applied_refs.csv'))

references_one_level(raw=data_develop)

data_develop_refs<-tab_df

write.csv(data_develop_refs, here('results', 'data_develop_refs.csv'))

references_one_level(raw=pt_groups)

pt_groups_refs<-tab_df

write.csv(pt_groups_refs, here('results', 'pt_groups_refs.csv'))

references_one_level(raw=cyp_groups)

cyp_groups_refs<-tab_df

write.csv(cyp_groups_refs, here('results', 'cyp_groups_refs.csv'))


references_two_level(raw=barriers)

barriers_refs<-tab_df

write.csv(barriers_refs, here('results', 'barriers_refs.csv'))

references_two_level(raw=facilitators)

facilitators_refs<-tab_df

write.csv(facilitators_refs, here('results', 'facilitators_refs.csv'))

write.csv(demog, here('results', 'demog.csv'))

