# Use of primary care by number of additional conditions: Uses the 2015/2016 CMD cohort
# Prepared by Mai Stafford
# Created 17th January 2020
# Last edit date: 24th Feb 2020

## @knitr primary libraries

#Load primary libraries
library(tidyverse)
library(here)
library(flextable)
library(officer)
library(gridExtra)
library(table1)
library(scales)

 my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("", "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD), "", "Median"=sprintf("%s", MEDIAN)))
 }




## @knitr load data and code for plots
if(!exists("count_bar_fun", mode="function")) source(here("Analysis","Analysis_code","plotting_outcomes.R"))

#Open data for linked CMD cohort 
outcomes_linked <- readRDS(here("Analysis","Processed_data","cprd_hes_linked_outcomes.rds"))


# list of key outcomes ----------------------------------------------------

cprd_outcome_vars <- c('TotCons_adj','TotMHRefs_adj', 'AllDrugs_adj', 'PsychDrugs_adj')

cprd_outcome_vars_cat <- c('TotCons_adjg','TotMHRefs_adjg', 'TotMHRefs_binary', 'AllDrugs_adjg', 'PsychDrugs_adjg')


# Looking at censor bias ------------------------------------------------
## @knitr censor bias
# length of followup and prim care use by number of addit conditions
outcomes_linked <- outcomes_linked %>% 
  mutate(censored = case_when(years_in_study_cprd <2   ~ "censored",
                              years_in_study_cprd == 2 ~ "not censored"))
table1(~years_in_study_cprd | condsgroup, data=outcomes_linked, render.continuous=my.render.cont)

table1(~years_in_study_cprd + TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_binary) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg)| factor(censored) , data=outcomes_linked)

# Consultations -----------------------------------------------------------
## @knitr consultations

table1(~TotCons_adj | condsgroup, data=outcomes_linked, overall = F)
percent_stack_bar_fun(outcomes_linked, 'TotCons_adjg', 'condsgroup') + ggtitle('Total Number of GP Consultations')


## @knitr consultations demog
###Look at relationship between demographics and additional conditions:
##age isnt very interest here
ggplot(outcomes_linked, aes(x=TotCons_adj, colour=agegroup3, fill=agegroup3)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.2, position='identity') + xlim(NA, 50) + facet_grid(condsgroup ~ .)


##sex - possibly men are less likely to come in for multiple consultations?? 

ggplot(outcomes_linked, aes(x=TotCons_adj, colour=gender, fill=gender)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.5, position='identity') + xlim(NA, 50) + facet_grid(condsgroup ~ .)
fit <- lm(TotCons_adj ~ condsgroup + gender, data=outcomes_linked) # test if number  of consultations is assoc with gender adj for number of conditions
summary(fit)
fit <- lm(TotCons_adj ~ condsgroup*gender, data=outcomes_linked) # test gender x number of conditions interaction
summary(fit)


## @knitr consultations imd
##IMD
#count with X number of consultations, by number of additional conditions and IMD
ggplot(outcomes_linked %>% filter(imd2015_5==1 | imd2015_5 == 5), aes(x=TotCons_adj, colour=imd2015_5, fill=imd2015_5)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.2, position='identity') + xlim(NA, 50) + facet_grid(condsgroup ~ .)+ ggtitle("Number of GP consultations")
table1(~TotCons_adjg | condsgroup * imd2015_5, data=outcomes_linked %>% filter(imd2015_5==1 | imd2015_5 == 5), overall = F)
table1(~TotCons_adj | condsgroup * imd2015_5, data=outcomes_linked %>% filter(imd2015_5==1 | imd2015_5 == 5), overall = F)
fit <- lm(TotCons_adj ~ condsgroup + imd2015_5, data=outcomes_linked) # test if number  of consultations is assoc with IMD adj for number of conditions
summary(fit)



# All drugs -----------------------------------------------------------
## @knitr drugs
table1(~AllDrugs_adj | condsgroup, data=outcomes_linked, overall = F)
percent_stack_bar_fun(outcomes_linked, 'AllDrugs_adjg', 'condsgroup') + ggtitle('Number of drugs prescribed')


## @knitr drugs demog
##age
ggplot(outcomes_linked, aes(x=AllDrugs_adj, colour=agegroup3, fill=agegroup3)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.5, position='identity') + xlim(NA, 50) + facet_grid(condsgroup ~ .)

##sex 
ggplot(outcomes_linked, aes(x=AllDrugs_adj, colour=gender, fill=gender)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.5, position='identity') + xlim(NA, 50) + facet_grid(condsgroup ~ .)
fit <- lm(AllDrugs_adj ~ condsgroup + gender, data=outcomes_linked) # test if number  of medications is assoc with gender adj for number of conditions
summary(fit)
fit <- lm(AllDrugs_adj ~ condsgroup*gender, data=outcomes_linked) # test gender x number of conditions interaction
summary(fit)

## @knitr drugs imd
ggplot(outcomes_linked %>% filter(imd2015_5 == 1 | imd2015_5 ==5), aes(x=AllDrugs_adj, colour=imd2015_5, fill=imd2015_5)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.25, position='identity') + xlim(NA, 50) + facet_grid(condsgroup ~ .)

percent_stack_two_bar_fun(outcomes_linked %>% filter(imd2015_5 == 1 | imd2015_5 ==5), 'AllDrugs_adjg', 'condsgroup','imd2015_5')
table1(~AllDrugs_adj | imd2015_5, data=outcomes_linked %>% filter(imd2015_5==1 | imd2015_5 == 5), overall = F)
table1(~AllDrugs_adj | condsgroup * imd2015_5, data=outcomes_linked %>% filter(imd2015_5==1 | imd2015_5 == 5), overall = F)
fit <- lm(AllDrugs_adj ~ condsgroup + imd2015_5, data=outcomes_linked) # test if number  of medications is assoc with IMD adj for number of conditions
summary(fit)


# Psychiatric drugs -------------------------------------------------------
## @knitr psych drugs
table1(~PsychDrugs_adjg | condsgroup, data=outcomes_linked, overall = F)
percent_stack_bar_fun(outcomes_linked, 'PsychDrugs_adjg', 'condsgroup') + ggtitle('Number of psychiatric drugs prescribed')


## @knitr psych drugs demog
percent_stack_bar_fun(outcomes_linked, 'PsychDrugs_adjg', 'agegroup3')
percent_dodge_two_bar_fun(outcomes_linked, 'PsychDrugs_adjg', 'condsgroup', 'agegroup3')
percent_stack_bar_fun(outcomes_linked, 'PsychDrugs_adjg', 'gender')
percent_stack_two_bar_fun(outcomes_linked, 'PsychDrugs_adjg', 'condsgroup', 'gender')

## @knitr psych drugs imd
percent_stack_bar_fun(outcomes_linked %>% filter(!is.na(imd2015_5)), 'PsychDrugs_adjg', 'imd2015_5')
#table1(~PsychDrugs_adjg | imd2015_5, data=outcomes_linked %>% filter(imd2015_5==1 | imd2015_5 == 5), overall = F)
table1(~PsychDrugs_adjg | imd2015_5, data=outcomes_linked  %>% filter(!is.na(imd2015_5)), overall = F)
percent_stack_two_bar_fun(outcomes_linked  %>% filter(!is.na(imd2015_5)), 'PsychDrugs_adjg', 'condsgroup', 'imd2015_5')
table1(~PsychDrugs_adjg | condsgroup *imd2015_5, data=outcomes_linked %>% filter(imd2015_5==1 | imd2015_5 == 5), overall = F)
table1(~PsychDrugs_adj | condsgroup * imd2015_5, data=outcomes_linked %>% filter(imd2015_5==1 | imd2015_5 == 5), overall = F)
fit <- lm(PsychDrugs_adj ~ condsgroup + imd2015_5, data=outcomes_linked) # test if number of psych medications is assoc with IMD adj for number of conditions
summary(fit)
fit <- lm(PsychDrugs_adj ~ condsgroup*imd2015_5, data=outcomes_linked) # test interaction between IMD and number of conditions
summary(fit)



# MH referrals -----------------------------------------------------------
## @knitr mh_referrals
table1(~TotMHRefs_binary | condsgroup, data=outcomes_linked, overall = F)
binary_percent_dodge_bar_fun(outcomes_linked, 'TotMHRefs_binary', 'condsgroup')


## @knitr mh_referrals demog
##age shows big relationship - better referrals, severity or less likely to be recorded as depressed unless bad?
binary_percent_dodge_two_bar_fun(outcomes_linked, 'TotMHRefs_binary', 'condsgroup', 'agegroup3')

##sex - men have more - better referrals, severity or less likely to be recorded as depressed unless bad?
binary_percent_dodge_two_bar_fun(outcomes_linked, 'TotMHRefs_binary', 'condsgroup', 'gender')

## @knitr mh_referrals imd
##IMD
binary_percent_dodge_two_bar_fun(outcomes_linked%>% filter(!is.na(imd2015_5)), 'TotMHRefs_binary', 'condsgroup', 'imd2015_5')
table1(~TotMHRefs_binary | condsgroup * imd2015_5, data=outcomes_linked %>% filter(!is.na(imd2015_5)), overall = F)

# Tables by demographic ---------------------------------------------------

## @knitr by demographic
##by demographic - tables hard to interpret - think this should be plots
table1(~TotCons_adj + factor(TotCons_adjg) + factor(TotMHRefs_binary) + AllDrugs_adj + factor(AllDrugs_adjg) + factor(PsychDrugs_adjg) | condsgroup, data=outcomes_linked, render.continuous=my.render.cont)
table1(~TotCons_adj + factor(TotCons_adjg) + factor(TotMHRefs_binary) + AllDrugs_adj + factor(AllDrugs_adjg) + factor(PsychDrugs_adjg) | condsgroup*imd2015_5, data=outcomes_linked, render.continuous=my.render.cont)
table1(~TotCons_adj + factor(TotCons_adjg) + factor(TotMHRefs_binary) + AllDrugs_adj + factor(AllDrugs_adjg) + factor(PsychDrugs_adjg) | condsgroup*gender, data=outcomes_linked, render.continuous=my.render.cont)

table1(~TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_adjg) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg) | gender*condsgroup, data=outcomes_linked, overall=F)

table1(~TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_adjg) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg) | agegroup3*condsgroup, data=outcomes_linked, overall=F)

outcomes_depr <- subset(outcomes_linked, imd2015_5==1 | imd2015_5==5)

table1(~TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_adjg) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg) | imd2015_5*condsgroup, data=outcomes_depr, overall=F)


# length of followup and prim care use for those with and without specific addit conditions
# by condition  ---------------------------------
## @knitr by condition
# CVD (=CHD + STR + PVD)
outcomes_linked <- outcomes_linked %>%
  mutate(cardiov=select(.,c(CHD, STR, PVD)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(cardiovasc=ifelse(cardiov>1, 1, cardiov))

outcomes_linked<-outcomes_linked %>% 
  mutate(ALC_lab = case_when(ALC == 1 ~ "ALC",
                             ALC == 0 ~ "No ALC"))

table1(~years_in_study_cprd | ALC_lab, data=outcomes_linked)
table1(~TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_adjg) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg) | ALC_lab, data=outcomes_linked, overall=F)

outcomes_linked<-outcomes_linked %>% 
  mutate(PSO_lab = case_when(PSO == 1 ~ "PSO",
                             PSO == 0 ~ "No PSO"))
table1(~years_in_study_cprd | PSO_lab, data=outcomes_linked )
table1(~TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_adjg) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg) | PSO_lab, data=outcomes_linked)

outcomes_linked<-outcomes_linked %>% 
  mutate(PNC_lab = case_when(PNC == 1 ~ "PNC",
                             PNC == 0 ~ "No PNC"))
table1(~years_in_study_cprd | PNC_lab, data=outcomes_linked )
table1(~TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_adjg) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg) | PNC_lab, data=outcomes_linked)

table1(~years_in_study_cprd | cardiovasc, data=outcomes_linked )
table1(~TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_adjg) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg) | cardiovasc, data=outcomes_linked )

table1(~years_in_study_cprd | COP, data=outcomes_linked )
table1(~TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_adjg) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg) | COP, data=outcomes_linked )

table1(~years_in_study_cprd | CAN, data=outcomes_linked )
table1(~TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_adjg) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg) | CAN, data=outcomes_linked )

outcomes_linked<-outcomes_linked %>% 
  mutate(DIB_lab = case_when(DIB == 1 ~ "DIB",
                             DIB == 0 ~ "No DIB"))

table1(~years_in_study_cprd | DIB_lab, data=outcomes_linked )
table1(~TotCons_adj + factor(TotCons_adjg) + TotMHRefs_adj + factor(TotMHRefs_adjg) + AllDrugs_adj + factor(AllDrugs_adjg) + PsychDrugs_adj + factor(PsychDrugs_adjg) | DIB_lab, data=outcomes_linked )

# condition testing ---------------------------------
## @knitr condition testing
# test for differences in use by specific conditions adjusting for total number of conditions
alc_reg <- lm(TotCons_adj ~ agegroup + gender + numconds + ALC, data=outcomes_linked) # repeat for other outcomes if this is useful
summary(alc_reg)
alc_reg <- lm(TotMHRefs_adj ~ agegroup + gender + numconds + ALC, data=outcomes_linked)
summary(alc_reg)
alc_reg <- lm(AllDrugs_adj ~ agegroup + gender + numconds + ALC, data=outcomes_linked)
summary(alc_reg)
alc_reg <- lm(PsychDrugs_adj ~ agegroup + gender + numconds + ALC, data=outcomes_linked)
summary(alc_reg)



# summary tables for utilisation outcomes ---------------------------------

## @knitr summary_tables
outcomes_linked$strata <- "Percent"
summary_factor <- outcomes_linked %>% 
  select(strata, !!cprd_outcome_vars_cat) %>% 
  add_count(strata, name='var_d') %>% 
  pivot_longer(cols = -one_of(c('strata', 'var_d')), names_to = 'variable') %>% 
  mutate(variable=paste(variable, value, sep='_')) %>% 
  dplyr::group_by(strata, variable) %>% 
  dplyr::summarise(var_d=max(var_d), var_n=n(), var_p=var_n/var_d) %>% 
  dplyr::mutate(var_p=ifelse(var_n<5, NA, var_p), 
                var_p=ifelse(var_d-var_n<5, NA, var_p), 
                var_p=scales::percent(var_p)) %>% 
  # mutate(var_p=scales::percent(var_p)) %>% 
  select(-var_d) %>% 
  dplyr::rename(value=var_p)

##reordering
ord_sum_factor <- summary_factor %>% 
  separate(variable, sep = "_adjg_", into = c("Var", "Level")) %>%
  mutate(ord_Var = factor(Var, level = c('TotCons','TotMHRefs', 'AllDrugs', 'PsychDrugs'))) %>% 
  mutate(ord_level = fct_relevel(Level, "None", '1', "1-5",'2',"2+","3+", "6-10", '11-15')) %>% 
  arrange(ord_Var, ord_level)



flex <- ord_sum_factor%>%
  select(-ord_Var, -ord_level) %>% 
  pivot_wider(names_from = strata, values_from = value) %>% 
  flextable()  %>% 
  #set_header_labels(labeled_variable='Variable') %>% 
  theme_box() %>% 
  align_text_col() %>% 
  bold(i=1, bold = TRUE, part = "header") %>% 
  bold(j=1, bold = TRUE, part = "body") %>% 
  width(width = 1.8)

doc <- read_docx()
doc <- body_add_flextable(doc, value = flex)
print(doc, target = here('Analysis', 'Analysis_Results', 'Outcomes', 'cprd_frequency_descriptives.docx'))





