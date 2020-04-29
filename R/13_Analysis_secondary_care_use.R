# Describes size of the cohort and their characteristics for the baseline year (2015/2016) for the sample WITH linked HES data
# Prepared by Mai Stafford
# Created Dec 2019
# Last edit date: 21st Feb 2020


## @knitr secondary libraries
#Load libraries
library(tidyverse)
library(here)
library(flextable)
library(officer)
library(gridExtra)
library(table1)
library(scales)

## @knitr load data and code for secondary care

if(!exists("count_bar_fun", mode="function")) source(here("Analysis","Analysis_code","plotting_outcomes.R"))

outcomes_linked <- readRDS(here("Analysis","Processed_data","cprd_hes_linked_outcomes.rds"))

# list of key outcomes ----------------------------------------------------
## @knitr outcome_vars
##APC
hes_apc_outcome_vars <- c('APCspells_adj', 'APCemerg_adj', 'APCelec_adj', 'APClos_adj', 'APCmh_spells_adj', 'APCmh_los_adj') 
hes_apc_outcome_vars_cat<-paste0(hes_apc_outcome_vars,'_cat')

hes_ae_outcome_vars <- c('AEatts_adj', 'AEmh_att_adj') 
hes_ae_outcome_vars_cat<-paste0(hes_ae_outcome_vars,'_cat')

hes_op_outcome_vars <- c('OPappts_adj', 'OPmiss_adj', 'OPmh_appts_adj', 'OPmh_miss_adj') 
hes_op_outcome_vars_cat<-paste0(hes_op_outcome_vars,'_cat')

hes_outcome_vars_cat <- c(hes_apc_outcome_vars_cat, 'APCmh_spells_binary',hes_ae_outcome_vars_cat, 'AEmh_att_binary',hes_op_outcome_vars_cat, 'OPmh_appts_binary')



# demo tables + plots -----------------------------------------------------------
## @knitr demo tables + plots


## key framing: volume of MH care needed in complex groups. 
## denominator needs to be about the size of the group?? 
## - count vs the proportion of the demographic group that have an eg APC spell?


gender_freq <- outcomes_linked %>% 
  count(gender,condsgroup) %>% 
  group_by(gender) %>% 
  mutate(prop=prop.table(n))

ggplot(gender_freq, aes(x=gender, y = prop, fill=condsgroup)) + geom_bar(position="stack", stat="identity")
#ggplot(gender_freq, aes(x=gender, y = n, fill=condsgroup)) + geom_bar(position="dodge", stat="identity")


agegroup3_freq <- outcomes_linked %>% 
  count(agegroup3,condsgroup) %>% 
  group_by(agegroup3) %>% 
  mutate(prop=prop.table(n))

ggplot(agegroup3_freq, aes(x=agegroup3, y = prop, fill=condsgroup)) + geom_bar(position="stack", stat="identity")
#ggplot(agegroup3_freq, aes(x=agegroup3, y = n, fill=condsgroup)) + geom_bar(position="dodge", stat="identity")


imd_freq <- outcomes_linked %>%
  filter(!is.na(imd2015_5)) %>% 
  count(imd2015_5,condsgroup) %>% 
  group_by(imd2015_5) %>% 
  mutate(prop=prop.table(n))

ggplot(imd_freq, aes(x=imd2015_5, y = prop, fill=condsgroup)) + geom_bar(position="stack", stat="identity")
#ggplot(imd_freq, aes(x=imd2015_5, y = n, fill=condsgroup)) + geom_bar(position="dodge", stat="identity")


ethnicity_freq <- outcomes_linked %>%
  filter(!is.na(ethnicity)) %>% 
  count(ethnicity,condsgroup) %>% 
  group_by(ethnicity) %>% 
  mutate(prop=prop.table(n))

ggplot(ethnicity_freq, aes(x=ethnicity, y = prop, fill=condsgroup)) + geom_bar(position="stack", stat="identity")
#ggplot(ethnicity_freq, aes(x=ethnicity, y = n, fill=condsgroup)) + geom_bar(position="dodge", stat="identity")

# APC Plotting code -----------------------------------------------------------  
## @knitr apc plotting 
## APC spells by condition
percent_stack_bar_fun(outcomes_linked, 'APCspells_adj_cat', 'condsgroup')
table1(~APCspells_binary | condsgroup, data=outcomes_linked)

## @knitr apc demog 
binary_percent_dodge_two_bar_fun(outcomes_linked, 'APCspells_binary', 'condsgroup', 'agegroup3') # Need to do this for APCemerg and APCelec too
binary_percent_dodge_two_bar_fun(outcomes_linked, 'APCspells_binary', 'condsgroup', 'gender')
fit <- glm(APCspells_binary ~ condsgroup + gender, data=outcomes_linked) # test if spells assoc with gender adj for number of additional conditions
summary(fit)
fit <- glm(APCemerg_binary ~ condsgroup + gender, data=outcomes_linked) # test if spells assoc with gender adj for number of additional conditions
summary(fit)
# count APCspells without maternity spells
outcomes_linked <- outcomes_linked %>% 
  mutate(APCspells_adj_nomaternity = APCspells_adj-APCmat_adj)
table1(~APCspells_adj_nomaternity | condsgroup*gender, data=outcomes_linked)


## @knitr apc plotting imd
binary_percent_dodge_two_bar_fun(outcomes_linked %>% filter(!is.na(imd2015_5)), 'APCspells_binary', 'condsgroup', 'imd2015_5')
fit <- glm(APCspells_binary ~ condsgroup + imd2015_5, data=outcomes_linked, family = "binomial") # test if likelihood of admission is assoc with IMD adjusted for number of additional conditions
summary(fit)
fit <- glm(APCspells_binary ~ condsgroup*imd2015_5, data=outcomes_linked, family = "binomial") # test interaction of IMD and number of additional conditions
summary(fit)
fit <- glm(APCemerg_binary ~ condsgroup + imd2015_5, data=outcomes_linked, family = "binomial") # test if likelihood of admission is assoc with IMD adjusted for number of additional conditions
summary(fit)
fit <- glm(APCelec_binary ~ condsgroup + imd2015_5, data=outcomes_linked, family = "binomial") # test if likelihood of admission is assoc with IMD adjusted for number of additional conditions
summary(fit)

## @knitr apc los
## APC spells LOS by condition
percent_stack_bar_fun(outcomes_linked, 'APClos_adj_cat', 'condsgroup')
table1(~APClos_adj_cat | condsgroup, data=outcomes_linked)


# APC MH plotting code -----------------------------------------------------------  
## @knitr apc mh plotting 
###############
## MH spell - yes / no binary
binary_percent_dodge_bar_fun(outcomes_linked, 'APCmh_spells_binary', 'condsgroup')
table1(~APCmh_spells_binary | condsgroup, data=outcomes_linked)
fit <- glm(APCmh_spells_binary ~ condsgroup, data=outcomes_linked, family = "binomial") # test if likelihood of MH admission varies by number of additional conditions
summary(fit)


## @knitr apc mh demog 

binary_percent_dodge_bar_fun(outcomes_linked, 'APCmh_spells_binary', 'agegroup3')
##by age
binary_percent_dodge_two_bar_fun(outcomes_linked, 'APCmh_spells_binary', 'condsgroup', 'agegroup3')
binary_percent_dodge_bar_fun(outcomes_linked, 'APCmh_spells_binary', 'gender')

##by sex
binary_percent_dodge_two_bar_fun(outcomes_linked, 'APCmh_spells_binary', 'condsgroup', 'gender')

## @knitr apc mh plotting imd
binary_percent_dodge_bar_fun(outcomes_linked %>% filter(!is.na(imd2015_5)), 'APCmh_spells_binary', 'imd2015_5')
binary_percent_dodge_bar_fun(outcomes_linked, 'APCmh_spells_binary', 'ethnicity')

binary_percent_dodge_two_bar_fun(outcomes_linked %>% filter(!is.na(imd2015_5)), 'APCmh_spells_binary', 'condsgroup', 'imd2015_5')


# AE Plotting code -----------------------------------------------------------
## @knitr ae plotting 
percent_stack_bar_fun(outcomes_linked, 'AEatts_adj_cat', 'condsgroup')
table1(~AEatts_binary | condsgroup, data=outcomes_linked)

## @knitr ae demog
#age
ggplot(outcomes_linked, aes(x=AEatts_adj, colour=agegroup3, fill=agegroup3)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.2, position='identity') + xlim(NA, 10) + facet_grid(condsgroup ~ .)
percent_dodge_two_bar_fun(outcomes_linked, 'AEatts_adj_cat', 'condsgroup','agegroup3')

#sex - not very interesting
ggplot(outcomes_linked, aes(x=AEatts_adj, colour=gender, fill=gender)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.5, position='identity') + xlim(NA, 10) + facet_grid(condsgroup ~ .)
percent_dodge_two_bar_fun(outcomes_linked, 'AEatts_adj_cat', 'condsgroup','gender')
fit <- glm(AEatts_binary ~ condsgroup + gender, data=outcomes_linked, family = "binomial") # test if likelihood of ED visit is assoc with gender adjusted for number of additional conditions
summary(fit)


#imd
## @knitr ae plotting imd
percent_stack_bar_fun(outcomes_linked %>%  filter(!is.na(imd2015_5)), 'AEatts_adj_cat','imd2015_5')
binary_percent_dodge_two_bar_fun(outcomes_linked %>%  filter(!is.na(imd2015_5)), 'AEatts_binary', 'condsgroup','imd2015_5')
table1(~AEatts_binary | condsgroup*imd2015_5, data=outcomes_linked %>% filter(!is.na(imd2015_5)), overall = F)
fit <- glm(AEatts_binary ~ condsgroup + imd2015_5, data=outcomes_linked, family = "binomial") # test if likelihood of ED visit is assoc with IMD adjusted for number of additional conditions
summary(fit)


#ethnicity - numbers are a real issue
#count_dodge_two_bar_fun(outcomes_linked, 'AEatts_adj_cat', 'condsgroup','ethnicity')
#percent_dodge_two_bar_fun(outcomes_linked, 'AEatts_adj_cat', 'condsgroup','ethnicity')


## @knitr ae plan vs unplan
percent_stack_bar_fun(outcomes_linked, 'AEunplan_adj', 'condsgroup')
table1(~AEunplan_adj | condsgroup, data=outcomes_linked)

# A&E mental health atts -----------------------------------------------------------
## @knitr ae mh plotting 

## A&E mental health atts
## counts of MH 

outcomes_linked %>%
  count(AEmh_att_binary,condsgroup) %>% 
  group_by(condsgroup) %>% 
  mutate(prop=prop.table(n))

binary_percent_dodge_bar_fun(outcomes_linked, 'AEmh_att_binary', 'condsgroup')
table1(~AEmh_att_binary | condsgroup, data=outcomes_linked)
fit <- glm(AEmh_att_binary ~ condsgroup, data=outcomes_linked, family = "binomial") # test if likelihood of MH ED attendance varies by number of additional conditions
summary(fit)

## @knitr ae mh demog 
binary_percent_dodge_bar_fun(outcomes_linked, 'AEmh_att_binary', 'agegroup3')
binary_percent_dodge_bar_fun(outcomes_linked, 'AEmh_att_binary', 'gender')

## @knitr ae mh plotting imd
binary_percent_dodge_bar_fun(outcomes_linked %>% filter(!is.na(imd2015_5)), 'AEmh_att_binary', 'imd2015_5')
binary_percent_dodge_two_bar_fun(outcomes_linked %>% filter(!is.na(imd2015_5)), 'AEmh_att_binary', 'condsgroup', 'imd2015_5')

### ISSUE WITH SMALL NUMBERS  -----------------------------------
##by age 
binary_percent_dodge_two_bar_fun(outcomes_linked, 'AEmh_att_binary', 'condsgroup', 'agegroup3')

##by sex

binary_percent_dodge_two_bar_fun(outcomes_linked, 'AEmh_att_binary', 'condsgroup', 'gender')

##by imd

binary_percent_dodge_two_bar_fun(outcomes_linked %>% filter(!is.na(imd2015_5)), 'AEmh_att_binary', 'condsgroup', 'imd2015_5')



# OP Plotting code -----------------------------------------------------------
## @knitr op plotting 
percent_stack_bar_fun(outcomes_linked, 'OPappts_adj_cat', 'condsgroup')
table1(~OPappts_adj | condsgroup, data=outcomes_linked)

## @knitr op demog
#age 
percent_stack_two_bar_fun(outcomes_linked, 'OPappts_adj_cat', 'condsgroup','agegroup3')
ggplot(outcomes_linked, aes(x=OPappts_adj, colour=agegroup3, fill=agegroup3)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.5, position='identity') + xlim(NA, 15) + facet_grid(condsgroup ~ .)

#sex 
percent_stack_two_bar_fun(outcomes_linked, 'OPappts_adj_cat', 'condsgroup','gender')
ggplot(outcomes_linked, aes(x=OPappts_adj, colour=gender, fill=gender)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.5, position='identity') + xlim(NA, 50) + facet_grid(condsgroup ~ .)
fit <- lm(OPappts_adj ~ condsgroup + gender, data=outcomes_linked) # test if number of OP visits is assoc with gender adj for number of conditions
summary(fit)


## @knitr op plotting imd
percent_stack_two_bar_fun(outcomes_linked %>%  filter(!is.na(imd2015_5)), 'OPappts_adj_cat', 'condsgroup','imd2015_5')
ggplot(outcomes_linked %>%  filter(!is.na(imd2015_5)), aes(x=OPappts_adj, colour=imd2015_5, fill=imd2015_5)) + geom_histogram(aes(y=..density..),binwidth=1, alpha=0.5, position='identity') + xlim(NA, 15) + facet_grid(condsgroup ~ .)
fit <- lm(OPappts_adj ~ condsgroup + imd2015_5, data=outcomes_linked) # test if number  of OP visits is assoc with IMD adj for number of conditions
summary(fit)


###############
## @knitr op missed
outcomes_clean_OP_appts <- outcomes_linked %>% 
  filter(OPappts_binary == TRUE) %>% ## must have an OP appt to miss it
  mutate(OPprop_miss_adj = (OPmiss_adj/OPappts_adj)) %>% 
  mutate(OPprop_miss_adj_cat = cut(OPprop_miss_adj, breaks=c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf),
                                   labels=c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")) )  


table1(~OPmiss_adj_cat +OPprop_miss_adj_cat | condsgroup, data=outcomes_clean_OP_appts, overall=F)
p1<-percent_stack_bar_fun(outcomes_clean_OP_appts, 'OPmiss_adj_cat', 'condsgroup') + ggtitle("Missed OP appointments (for those who had any)")
p2<-percent_stack_bar_fun(outcomes_clean_OP_appts, 'OPprop_miss_adj_cat', 'condsgroup') 
grid.arrange(p1,p2)

## @knitr op missed demog
#age 
percent_stack_two_bar_fun(outcomes_clean_OP_appts, 'OPprop_miss_adj_cat', 'condsgroup','agegroup3')

#sex 
percent_stack_two_bar_fun(outcomes_clean_OP_appts, 'OPprop_miss_adj_cat', 'condsgroup','gender')

## @knitr op missed imd
#imd  
table1(~OPmiss_adj_cat +OPprop_miss_adj_cat | condsgroup * imd2015_5, data=outcomes_clean_OP_appts %>% filter(imd2015_5==1 | imd2015_5 ==5), overall=F)
percent_stack_two_bar_fun(outcomes_clean_OP_appts %>% filter(imd2015_5==1 | imd2015_5 ==5), 'OPprop_miss_adj_cat','condsgroup', 'imd2015_5') 



# OP mental health atts -----------------------------------------------------------
## @knitr op mh plotting
binary_percent_dodge_bar_fun(outcomes_linked, 'OPmh_appts_binary', 'condsgroup')
table1(~OPmh_appts_binary | condsgroup, data=outcomes_linked)

## @knitr op mh demog 
### POSS ISSUE WITH SMALL NUMBERS  -----------------------------------
##by age 
binary_percent_dodge_bar_fun(outcomes_linked, 'OPmh_appts_binary','agegroup3')
binary_percent_dodge_two_bar_fun(outcomes_linked, 'OPmh_appts_binary', 'condsgroup', 'agegroup3')

##by sex
binary_percent_dodge_bar_fun(outcomes_linked, 'OPmh_appts_binary','gender')
binary_percent_dodge_two_bar_fun(outcomes_linked, 'OPmh_appts_binary', 'condsgroup', 'gender')

##by imd
## @knitr op mh plotting imd
binary_percent_dodge_bar_fun(outcomes_linked %>%  filter(!is.na(imd2015_5)), 'OPmh_appts_binary','imd2015_5')
binary_percent_dodge_two_bar_fun(outcomes_linked%>% filter(!is.na(imd2015_5)), 'OPmh_appts_binary', 'condsgroup', 'imd2015_5')



#  Tables 
table1(~OPappts_adj + factor(APCspells_binary) + factor(APCelec_binary) + factor(APCemerg_binary) + factor(AEatts_binary) + factor(OPmh_appts_binary) + factor(APCmh_spells_binary) + factor(AEmh_att_binary)  | condsgroup, data=outcomes_linked, render.continuous=my.render.cont)
table1(~OPappts_adj + factor(APCspells_binary) + factor(APCelec_binary) + factor(APCemerg_binary) + factor(AEatts_binary) + factor(OPmh_appts_binary) + factor(APCmh_spells_binary) + factor(AEmh_att_binary)  | condsgroup*imd2015_5, data=outcomes_linked, render.continuous=my.render.cont)
table1(~OPappts_adj + factor(APCspells_binary) + factor(APCelec_binary) + factor(APCemerg_binary) + factor(AEatts_binary) + factor(OPmh_appts_binary) + factor(APCmh_spells_binary) + factor(AEmh_att_binary)  | condsgroup*gender, data=outcomes_linked, render.continuous=my.render.cont)




# Tables - broken, KH to fix! ---------------------------------------------

# summary tables of each variable ---------------------------------
## @knitr summary_tables
outcomes$strata <- "Percent"
summary_factor <- outcomes %>% 
  select(strata, !!hes_outcome_vars_cat) %>% 
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
  separate(variable, sep = "_adj_cat_", into = c("Var", "Level")) %>%
  mutate(ord_Var = factor(Var, level = c("APCspells", "APCmh_spells", "APClos", "APCmh_los", "AEatts", "AEmh_att", "OPappts", "OPmiss", "OPmh_appts","OPmh_miss"))) %>% 
  mutate(ord_level = fct_relevel(Level, "None", "1-5", "6-10")) %>% 
  arrange(ord_Var, ord_level)

##APC
apc_ord_sum_factor <- ord_sum_factor %>% 
  filter(str_detect(Var, "APC"))
##OP
op_ord_sum_factor <- ord_sum_factor %>% 
  filter(str_detect(Var, "OP"))

##AE
ae_ord_sum_factor <- ord_sum_factor %>% 
  filter(str_detect(Var, "AE"))


apc_flex <- apc_ord_sum_factor %>%
  select(-ord_Var, -ord_level) %>% 
  pivot_wider(names_from = strata, values_from = value) %>% 
  flextable()  %>% 
  #set_header_labels(labeled_variable='Variable') %>% 
  theme_box() %>% 
  align_text_col() %>% 
  bold(i=1, bold = TRUE, part = "header") %>% 
  bold(j=1, bold = TRUE, part = "body") %>% 
  width(width = 1.8)

op_flex <- op_ord_sum_factor %>%
  select(-ord_Var, -ord_level) %>% 
  pivot_wider(names_from = strata, values_from = value) %>% 
  flextable()  %>% 
  #set_header_labels(labeled_variable='Variable') %>% 
  theme_box() %>% 
  align_text_col() %>% 
  bold(i=1, bold = TRUE, part = "header") %>% 
  bold(j=1, bold = TRUE, part = "body") %>% 
  width(width = 1.8)

ae_flex <- ae_ord_sum_factor %>%
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
apc_doc <- body_add_flextable(doc, value = apc_flex)
print(apc_doc, target = here('Analysis', 'Analysis_Results', 'Outcomes', 'hes_apc_frequency_descriptives.docx'))


doc <- read_docx()
op_doc <- body_add_flextable(doc, value = op_flex)
print(op_doc, target = here('Analysis', 'Analysis_Results', 'Outcomes', 'hes_op_frequency_descriptives.docx'))

doc <- read_docx()
ae_doc <- body_add_flextable(doc, value = ae_flex)
print(ae_doc, target = here('Analysis', 'Analysis_Results', 'Outcomes', 'hes_ae_frequency_descriptives.docx'))




## @knitr table1


# Edit Table 1 to remove max and min
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("", "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

#Does secondary care use vary by additional conditions?
# length of followup and prim care use by number of addit conditions
table1(~APCspells + factor(APCspellsg) + APCovernightspells + factor(APCovernightspellsg) + APCemerg + factor(APCemergg) + APCelec + factor(APCelecg) + APClos + factor(APClosg) | gender*condsgroup, data=outcomes_linked, render.continuous=my.render.cont)
table1(~ACSadms + factor(ACSadmsg) + OPatts + factor(OPattsg) + OPmiss + factor(OPmissg) + AEseen + factor(AEseeng) | gender*condsgroup, data=CPRDHES_IMD, render.continuous=my.render.cont) 
table1(~APCspells + factor(APCspellsg) + APCovernightspells + factor(APCovernightspellsg) + APCemerg + factor(APCemergg) + APCelec + factor(APCelecg) + APClos + factor(APClosg) | agegroup3*condsgroup, data=CPRDHES_IMD, render.continuous=my.render.cont)
table1(~ACSadms + factor(ACSadmsg) + OPatts + factor(OPattsg) + OPmiss + factor(OPmissg) + AEseen + factor(AEseeng) | agegroup3*condsgroup, data=CPRDHES_IMD, render.continuous=my.render.cont) 

CPRDHES_depr <- subset(CPRDHES_IMD, imd2015_5==1 | imd2015_5==5)
table1(~APCspells + factor(APCspellsg) + APCovernightspells + factor(APCovernightspellsg) + APCemerg + factor(APCemergg) + APCelec + factor(APCelecg) + APClos + factor(APClosg) | imd2015g*condsgroup, data=CPRDHES_depr, render.continuous=my.render.cont)
table1(~ACSadms + factor(ACSadmsg) + OPatts + factor(OPattsg) + OPmiss + factor(OPmissg) + AEseen + factor(AEseeng) |  imd2015g*condsgroup, data=CPRDHES_depr, render.continuous=my.render.cont)



