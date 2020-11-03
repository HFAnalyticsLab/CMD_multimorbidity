

library(tidyverse)
library(here)
library(scales)
library(broom)
library(gridExtra)

outcomes_linked <- readRDS(here::here("Analysis","Processed_data","cprd_hes_linked_outcomes.rds"))

outcomes_linked <- outcomes_linked %>% 
  mutate(secondary_mh_binary=select(.,c(OPmh_appts_binary, AEmh_att_binary, APCmh_spells_binary)) %>% rowSums(na.rm=TRUE))

outcomes_linked <- outcomes_linked %>% 
  mutate(secondary_binary=select(.,c(OPappts_binary, AEatts_binary, APCspells_binary)) %>% rowSums(na.rm=TRUE))

outcomes_linked$condsgroup <- cut(outcomes_linked$numconds, breaks=c(-Inf, 0, 2, Inf),
                                  labels=c("CMD only", "CMD+1 / +2", "CMD+3+")) 


thf_red_gradient <- seq_gradient_pal('#dd0031','#f7bfcc', 'Lab')(seq(0,1, length.out = 3))
thf_categories <- c('#dd0031', '#53a9cd', '#744284')

 

# Check censoring ---------------------------------------------------------

outcomes_linked <- outcomes_linked %>% 
  mutate(censored = case_when(years_in_study_cprd <2   ~ "censored",
                              years_in_study_cprd == 2 ~ "not censored"))

summ_censoring<-outcomes_linked %>% 
  group_by(censored) %>% 
  summarise(count=n(), Cons = median(TotCons_adj, na.rm=T),Cons_LQ = quantile(TotCons_adj, 0.25),Cons_UQ = quantile(TotCons_adj, 0.75),Drugs = median(AllDrugs_adj, na.rm=T), Drugs_LQ = quantile(AllDrugs_adj, 0.25),Drugs_UQ = quantile(AllDrugs_adj, 0.75), PsychDrugs=sum(PsychDrugs_adj>1), PsychDrugs_freq=sum(PsychDrugs_adj>1)/n(), OPappts=sum(OPappts_binary>0),OPappts_freq=sum(OPappts_binary>0)/n(),APCspells=sum(APCspells_binary>0),APCspells_freq=sum(APCspells_binary>0)/n(),AEatts=sum(AEatts_binary>0),AEatts_freq=sum(AEatts_binary>0)/n())
summ_censoring<-summ_censoring %>% 
  mutate_if(is.numeric, round, digits=1)

write.csv(summ_censoring, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives','summary_health_use_by_censoring.csv'), row.names = F)            

# Healthcare use by condition [values saved, not plots]---------------------------------------------

##number with median value
primary_care<-outcomes_linked %>% 
  group_by(condsgroup) %>% 
  summarise(cons_med = round(median(TotCons_adj),1), 
            n_cons_med = sum(round(TotCons_adj,1) == round(median(TotCons_adj),1)), 
            cons_uq = round(quantile(TotCons_adj, 0.75),1),
            n_cons_uq = sum(round(TotCons_adj,1) == round(quantile(TotCons_adj, 0.75),1)),
            cons_lq = round(quantile(TotCons_adj, 0.25),1),
            n_cons_lq = sum(round(TotCons_adj,1) == round(quantile(TotCons_adj, 0.25),1)),
            drugs_med = round(median(AllDrugs_adj),1),
            n_drugs_med = sum(round(AllDrugs_adj,1) == round(median(AllDrugs_adj),1)), 
            drugs_uq = round(quantile(AllDrugs_adj, 0.75),1),
            n_drugs_uq = sum(round(AllDrugs_adj,1) == round(quantile(AllDrugs_adj, 0.75),1)),
            drugs_lq = round(quantile(AllDrugs_adj, 0.25),1),
            n_drugs_lq = sum(round(AllDrugs_adj,1) == round(quantile(AllDrugs_adj, 0.25),1))) 

write.csv(primary_care, here::here('Analysis', 'Analysis_Results', 'utilisation_descriptives', 'plot_cond_prim.csv'))

plot_cond_prim<-primary_care %>% 
  select(-starts_with('n_')) %>% 
  pivot_longer(-c(condsgroup),
    names_to = c('measure','.value'),
    names_sep = '_'
  )


p1<-ggplot(data=plot_cond_prim, aes(x= measure, y = med, fill=condsgroup)) + 
  geom_bar(position='dodge', stat='identity', colour="black") + 
  geom_errorbar(aes(ymin=lq, ymax=uq),width=0.2, position=position_dodge(width = 0.9), colour='black') +
  scale_x_discrete(limits = c('cons','drugs'), labels=c('cons' = 'Consultations', 'drugs'='Drugs Prescribed')) +
  ggtitle('Primary Care Use') +
  xlab('')+
  ylab('Median') +
  scale_fill_manual(values=rev(thf_categories)) +
  theme_bw()

plot_cond_second <- outcomes_linked %>%
  group_by(condsgroup) %>%
  add_tally(name='total_n') %>%
  mutate(any_secondary=if_else(APCspells_binary !=0 | AEatts_binary !=0 | OPappts_binary !=0, 1, 0)) %>%
  summarise_at(vars('total_n','APCspells_binary','AEatts_binary', 'OPappts_binary', 'any_secondary'), mean) %>% 
  pivot_longer(-c(condsgroup, total_n), names_to = "measure", values_to = "prop") %>% 
  mutate(n_true = prop*total_n) %>% 
  mutate(n_false = (1-prop)*total_n)

write.csv(plot_cond_second, here::here('Analysis', 'Analysis_Results', 'utilisation_descriptives', 'plot_cond_second.csv'))

p2<-ggplot(data = plot_cond_second, aes(x= measure, y = prop * 100, fill=condsgroup)) + 
  geom_bar(position='dodge', stat='identity', colour='black') +
  ylab('Percentage of patients') + 
  scale_x_discrete(limits = c('OPappts_binary','APCspells_binary','AEatts_binary'), labels=c('AEatts_binary' = 'Emergency Att', 'APCspells_binary'='Inpatient Spell', 'OPappts_binary' = 'Outpatient Appt')) +
  ggtitle('Secondary Care Use')+
  xlab('')+
  scale_fill_manual(values=rev(thf_categories)) +
  theme_bw()


##Inpatient spells
plot_cond_inpat <- outcomes_linked %>%
  group_by(condsgroup) %>% 
  summarise_at(vars('APCelec_binary', 'APCemerg_binary'), mean) %>% 
  pivot_longer(-condsgroup, names_to = "measure", values_to = "prop") 

ggplot(data = plot_cond_inpat, aes(x= measure, y = prop * 100, fill=condsgroup, label=round(prop * 100, 1))) + 
  geom_bar(position='dodge', stat='identity') +
  ylab('Percentage of patients') + 
  geom_text(vjust=1.5,position=position_dodge(width=0.9)) +
  scale_fill_manual(values=rev(thf_categories)) 


# Mental health -related healthcare use [values saved, not plots ]-----------------------------------

plot_cond_psych_drugs <- outcomes_linked %>%
  group_by(condsgroup) %>% 
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','PlusOnePsychDrugs'), mean) %>% 
  pivot_longer(-c(condsgroup, total_n), names_to = "measure", values_to = "prop")  %>% 
  mutate(n_true = prop*total_n) %>% 
  mutate(n_false = (1-prop)*total_n)

write.csv(plot_cond_psych_drugs, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_psych_drugs.csv'))

ggplot(data = plot_cond_psych_drugs, aes(x= measure, y = prop * 100, fill=condsgroup)) + 
  geom_bar(position='dodge', stat='identity', colour='black') +
  ylab('Percentage of patients') + 
  xlab('Prescriptions for more than one type of psychiatric drug') +
  ggtitle('Mental health prescriptions') +
  scale_fill_manual(values=rev(thf_categories)) +
  theme_bw() + 
  theme(axis.text.x = element_blank())

plot_cond_secondmh<- outcomes_linked %>%
  group_by(condsgroup) %>% 
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','APCmh_spells_binary','AEmh_att_binary','OPmh_appts_binary'), mean) %>% 
  pivot_longer(-c(condsgroup, total_n), names_to = "measure", values_to = "prop")  %>% 
  mutate(n_true = prop*total_n) %>% 
  mutate(n_false = (1-prop)*total_n)

write.csv(plot_cond_secondmh, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_secondmh.csv'))

ggplot(data=plot_cond_secondmh, aes(x= measure, y = prop * 100, fill=condsgroup)) + 
  geom_bar(position='dodge', stat='identity', colour='black') +
  ylab('Percentage of patients') +
  ggtitle('Secondary mental healthcare use') +
  scale_x_discrete(limits = c('OPmh_appts_binary','APCmh_spells_binary','AEmh_att_binary'), labels=c('AEmh_att_binary' = 'Emergency MH Att', 'APCmh_spells_binary'='Inpatient MH Spell', 'OPmh_appts_binary' = 'Outpatient MH Appt')) +  
  scale_fill_manual(values=rev(thf_categories)) +
  theme_bw() 

# Healthcare use by condition * IMD [ Figure 3 long-read ] ------------------

imd_nonpsychdrug <- outcomes_linked %>% 
  mutate(nonpsychdrug = AllDrugs_adj - PsychDrugs_adj) %>% 
  group_by(condsgroup, imd_3) %>% 
  summarise(n_total=n(),
            nonpsychdrug_med = round(median(nonpsychdrug),1),
            n_nonpsychdrug_med = sum(round(nonpsychdrug,1) == round(median(nonpsychdrug),1)))
            
imd_primary_care<-outcomes_linked %>% 
  group_by(condsgroup, imd_3) %>% 
  summarise(n_total=n(),
            drugs_med = round(median(AllDrugs_adj),1),
            n_drugs_med = sum(round(AllDrugs_adj,1) == round(median(AllDrugs_adj),1)), 
            drugs_uq = round(quantile(AllDrugs_adj, 0.75),1),
            n_drugs_uq = sum(round(AllDrugs_adj,1) == round(quantile(AllDrugs_adj, 0.75),1)),
            drugs_lq = round(quantile(AllDrugs_adj, 0.25),1),
            n_drugs_lq = sum(round(AllDrugs_adj,1) == round(quantile(AllDrugs_adj, 0.25),1)),
            cons_med = round(median(TotCons_adj),1),
            n_cons_med = sum(round(TotCons_adj,1) == round(median(TotCons_adj),1)), 
            cons_uq = round(quantile(TotCons_adj, 0.75),1),
            n_cons_uq = sum(round(TotCons_adj,1) == round(quantile(TotCons_adj, 0.75),1)),
            cons_lq = round(quantile(TotCons_adj, 0.25),1),
            n_cons_lq = sum(round(TotCons_adj,1) == round(quantile(TotCons_adj, 0.25),1))) 

##SDC issues - round values in row 6 to nearest 0.2 rather than 0.1, to avoid small numbers
imd_primary_care[6,c('condsgroup','imd_3','cons_uq','n_cons_uq')]<-outcomes_linked %>%
  filter(condsgroup=="CMD+1 / +2" & imd_3=="3") %>% 
  group_by(condsgroup, imd_3) %>% 
  summarise(cons_uq = ceiling(quantile(TotCons_adj, 0.75)*5)/5,
            n_cons_uq = sum(ceiling(TotCons_adj*5)/5 == ceiling(quantile(TotCons_adj, 0.75)*5)/5))

write.csv(imd_primary_care, here::here('Analysis', 'Analysis_Results', 'utilisation_descriptives', 'plot_cond_prim_imd.csv'))

plot_cond_prim_imd<-imd_primary_care %>% 
  select(-starts_with('n_')) %>% 
  pivot_longer(-c(condsgroup, imd_3),
               names_to = c('measure','.value'),
               names_sep = '_') 

plot_cond_prim_imd$measure <- factor(plot_cond_prim_imd$measure, levels = c('drugs','cons'))


drugs <- plot_cond_prim_imd %>%  
  filter(measure ==  'drugs') %>% 
  ggplot(aes(x= imd_3, y = med, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  geom_errorbar(aes(ymin=lq, ymax=uq),width=0.2, position=position_dodge(width = 0.9), colour='black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Median') +
  ylim(c(0,20.5))+
  ggtitle('Medications') +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 

cons <- plot_cond_prim_imd %>%  
  filter(measure ==  'cons') %>% 
  ggplot(aes(x= imd_3, y = med, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  geom_errorbar(aes(ymin=lq, ymax=uq),width=0.2, position=position_dodge(width = 0.9), colour='black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Median') +
  ylim(c(0,20.5))+
  ggtitle('Consultations') +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 


prim<-grid.arrange(cons,drugs, nrow=1, top="The median number of primary care consultations or medications, by number of additional long-term conditions and deprivation level")


ggsave(plot=prim,filename = here::here('Analysis', 'Analysis_Results','Utilisation_descriptives', 'plot_cond_prim_imd.png'), width = 12, height = 7)


# secondary use by condition * IMD [Figure 5 - long read] ----------------------------------------
plot_cond_second <- outcomes_linked %>%
  group_by(condsgroup, imd_3) %>%
  add_tally(name='total_n') %>%
  mutate(any_secondary=if_else(APCspells_binary !=0 | AEatts_binary !=0 | OPappts_binary !=0, 1, 0)) %>%
  summarise_at(vars('total_n','any_secondary'), mean) %>% 
  pivot_longer(-c(condsgroup, imd_3, total_n), names_to = "measure", values_to = "prop") %>%
  mutate(n_true = prop*total_n) %>%
  mutate(n_false = (1-prop)*total_n)
write.csv(plot_cond_second , here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_any_second_imd.csv'))


## Outpatients
plot_cond_op_imd <- outcomes_linked %>%
  group_by(condsgroup, imd_3) %>%
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','OPappts_binary' ), mean) %>%
  pivot_longer(-c(condsgroup, imd_3, total_n), names_to = "measure", values_to = "prop") %>%
  mutate(n_true = prop*total_n) %>%
  mutate(n_false = (1-prop)*total_n)

write.csv(plot_cond_op_imd, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_op_imd.csv'))

plot_cond_op_imd %>%  
  ggplot(aes(x= imd_3, y = prop * 100, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Percentage of patients') +
  ylim(c(NA,84))+
  ggtitle('Outpatient appointments, by number of additional long-term conditions and deprivation level') +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 

ggsave(filename = here::here('Analysis', 'Analysis_Results','Utilisation_descriptives', 'plot_cond_op_imd.png'), width = 12, height = 7)


## inpatients (total)
plot_cond_ip_imd <- outcomes_linked %>%
  group_by(condsgroup, imd_3) %>%
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','APCspells_binary' ), mean) %>%
  pivot_longer(-c(condsgroup, imd_3, total_n), names_to = "measure", values_to = "prop") %>%
  mutate(n_true = prop*total_n) %>%
  mutate(n_false = (1-prop)*total_n)

write.csv(plot_cond_ip_imd, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_ip_imd.csv'))

plot_cond_ip_imd %>%  
  ggplot(aes(x= imd_3, y = prop * 100, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Percentage of patients') +
  ylim(c(NA,84))+
  ggtitle('Hospital admissions, by number of additional long-term conditions and deprivation level') +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 

ggsave(filename = here::here('Analysis', 'Analysis_Results','Utilisation_descriptives', 'plot_cond_ip_imd.png'), width = 12, height = 7)




## Inpatient spells - emergency  vs elective 

plot_cond_inpat_imd <- outcomes_linked %>%
  group_by(condsgroup, imd_3) %>%
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','APCelec_binary', 'APCemerg_binary'), mean) %>%
  pivot_longer(-c(condsgroup, imd_3, total_n), names_to = "measure", values_to = "prop") %>%
  mutate(n_true = prop*total_n) %>%
  mutate(n_false = (1-prop)*total_n)

plot_cond_inpat_imd$measure <- factor(plot_cond_inpat_imd$measure, levels = c('APCelec_binary','APCemerg_binary', 'AEatts_binary'))

write.csv(plot_cond_inpat_imd, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_inpat_imd.csv'))

elec <- plot_cond_inpat_imd %>%  
  filter(measure ==  'APCelec_binary') %>% 
  ggplot(aes(x= imd_3, y = prop * 100, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Percentage of patients') +
  ylim(c(NA,37))+
  ggtitle('Elective inpatient spells') +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 


emerg <- plot_cond_inpat_imd %>%  
  filter(measure ==  'APCemerg_binary') %>% 
  ggplot(aes(x= imd_3, y = prop * 100, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Percentage of patients') +
  ylim(c(NA,37))+
  ggtitle('Emergency inpatient spells') +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 


inpat<-grid.arrange(elec,emerg, nrow=1, top="Percentage of patients with elective or emergency inpatient spells, by number of additional long-term conditions and deprivation level")

ggsave(plot=inpat, filename = here::here('Analysis', 'Analysis_Results','Utilisation_descriptives', 'plot_cond_inpat_imd.png'), width = 12, height = 7)


## A&E attendances

plot_cond_ae_imd <- outcomes_linked %>%
  group_by(condsgroup, imd_3) %>%
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','AEatts_binary' ), mean) %>%
  pivot_longer(-c(condsgroup, imd_3, total_n), names_to = "measure", values_to = "prop") %>%
  mutate(n_true = prop*total_n) %>%
  mutate(n_false = (1-prop)*total_n)

write.csv(plot_cond_ae_imd, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_ae_imd.csv'))

plot_cond_ae_imd %>%  
  ggplot(aes(x= imd_3, y = prop * 100, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Percentage of patients') +
  ylim(c(NA,84))+
  ggtitle('A&E attendances, by number of additional long-term conditions and deprivation level') +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 

ggsave(filename = here::here('Analysis', 'Analysis_Results','Utilisation_descriptives', 'plot_cond_ae_imd.png'), width = 12, height = 7)


# Mental health -related healthcare use * IMD [Figure 4 long-read ] -----------------------------------


## Prescriptions

plot_cond_psych_drugs_imd <- outcomes_linked %>%
  filter(!is.na(imd_3)) %>% 
  group_by(condsgroup, imd_3) %>% 
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','PlusOnePsychDrugs'), mean) %>% 
  pivot_longer(-c(condsgroup, total_n, imd_3), names_to = "measure", values_to = "prop")  %>% 
  mutate(n_true = prop*total_n) %>% 
  mutate(n_false = (1-prop)*total_n)

write.csv(plot_cond_psych_drugs_imd, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_psych_drugs_imd.csv'))

ggplot(data = plot_cond_psych_drugs_imd, aes(x= imd_3, y = prop * 100, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Percentage of patients') +
  ggtitle('Prescriptions for more than one type of psychotropic medication, by number of additional long-term conditions and deprivation level') +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 


ggsave(filename = here::here('Analysis', 'Analysis_Results','Utilisation_descriptives', 'plot_cond_psych_drugs_imd.png'), width = 12, height = 7)


## Mental health related outpatient 
plot_cond_opmh_imd<- outcomes_linked %>%
  group_by(condsgroup, imd_3) %>% 
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','OPmh_appts_binary'), mean) %>% 
  pivot_longer(-c(condsgroup, total_n, imd_3), names_to = "measure", values_to = "prop")  %>% 
  mutate(n_true = prop*total_n) %>% 
  mutate(n_false = (1-prop)*total_n)

write.csv(plot_cond_opmh_imd, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_opmh_imd.csv'))


## plot not in long-read
ggplot(data = plot_cond_opmh_imd, aes(x= imd_3, y = prop * 100, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Percentage of patients') +
  ggtitle('Percentage with mental-health related outpatient appointments, by number of additional long-term conditions and deprivation level') +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  ylim(NA, 5.2) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 


ggsave(filename = here::here('Analysis', 'Analysis_Results','Utilisation_descriptives', 'plot_cond_opmh_imd.png'), width = 12, height = 7)

## Mental-health related inpatient - plot not in long-read
plot_cond_apcmh_imd<- outcomes_linked %>%
  group_by(condsgroup, imd_3) %>% 
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','APCmh_spells_binary'), mean) %>% 
  pivot_longer(-c(condsgroup, total_n, imd_3), names_to = "measure", values_to = "prop")  %>% 
  mutate(n_true = prop*total_n) %>% 
  mutate(n_false = (1-prop)*total_n)

write.csv(plot_cond_apcmh_imd, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_apcmh_imd.csv'))

ggplot(data = plot_cond_apcmh_imd, aes(x= imd_3, y = prop * 100, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Percentage of patients') +
  ggtitle('Percentage with mental-health related inpatient spells, by number of additional long-term conditions and deprivation level') +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  ylim(NA, 5.2) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 


ggsave(filename = here::here('Analysis', 'Analysis_Results','Utilisation_descriptives', 'plot_cond_apcpmh_imd.png'), width = 12, height = 7)

## mental health related A&E - plot not in long-read 

plot_cond_aemh_imd<- outcomes_linked %>%
  group_by(condsgroup, imd_3) %>% 
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','AEmh_att_binary'), mean) %>% 
  pivot_longer(-c(condsgroup, total_n, imd_3), names_to = "measure", values_to = "prop")  %>% 
  mutate(n_true = prop*total_n) %>% 
  mutate(n_false = (1-prop)*total_n)

write.csv(plot_cond_aemh_imd, here::here('Analysis', 'Analysis_Results', 'Utilisation_descriptives', 'plot_cond_aemh_imd.csv'))

ggplot(data = plot_cond_aemh_imd, aes(x= imd_3, y = prop * 100, fill=imd_3)) + 
  geom_bar(stat='identity', colour= 'black') +
  facet_wrap(~condsgroup, nrow = 1, labeller = labeller( condsgroup =
                                                           c('CMD only' = 'No additional conditions',
                                                             'CMD+1 / +2' = '1 - 2 additional conditions',
                                                             'CMD+3+' = '3 or more additional conditions'))) +
  labs(x = 'Level of deprivation', y = 'Percentage of patients') +
  ggtitle('Percentage with mental-health related A&E attendances, by number of additional long-term conditions and deprivation level') +
  ylim(NA, 5.2) +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") 


ggsave(filename = here::here('Analysis', 'Analysis_Results','Utilisation_descriptives', 'plot_cond_aemh_imd.png'), width = 12, height = 7)


# Healthcare use by condition * ethnicity [output not saved] ---------------------------------

ethnic_primary_care<-outcomes_linked %>% 
  filter(!is.na(ethnic)) %>% 
  group_by(condsgroup, ethnic) %>% 
  summarise(cons_med = round(median(TotCons_adj),1), 
            n_cons_med = sum(round(TotCons_adj,1) == round(median(TotCons_adj),1)), 
            cons_uq = round(quantile(TotCons_adj, 0.75),1),
            n_cons_uq = sum(round(TotCons_adj,1) == round(quantile(TotCons_adj, 0.75),1)),
            cons_lq = round(quantile(TotCons_adj, 0.25),1),
            n_cons_lq = sum(round(TotCons_adj,1) == round(quantile(TotCons_adj, 0.25),1)),
            drugs_med = round(median(AllDrugs_adj),1),
            n_drugs_med = sum(round(AllDrugs_adj,1) == round(median(AllDrugs_adj),1)), 
            drugs_uq = round(quantile(AllDrugs_adj, 0.75),1),
            n_drugs_uq = sum(round(AllDrugs_adj,1) == round(quantile(AllDrugs_adj, 0.75),1)),
            drugs_lq = round(quantile(AllDrugs_adj, 0.25),1),
            n_drugs_lq = sum(round(AllDrugs_adj,1) == round(quantile(AllDrugs_adj, 0.25),1))) 



plot_cond_prim_ethnic<-ethnic_primary_care %>% 
  select(-starts_with('n_')) %>% 
  pivot_longer(-c(condsgroup, ethnic),
               names_to = c('measure','.value'),
               names_sep = '_') 

cons<-ggplot(data=plot_cond_prim_ethnic %>% filter(measure=='cons'), aes(x= measure, y = med, fill=ethnic)) +
  geom_bar(position='dodge', stat='identity', colour= 'black') +
  geom_errorbar(aes(ymin=lq, ymax=uq),width=0.2, position=position_dodge(width = 0.9), colour='black') +
  ggtitle('Primary care consultations')+
  xlab('')+
  ylab('Median') +
  ylim(c(0,20.5))+
  scale_fill_brewer(palette = "Spectral", direction=-1) + 
  theme_bw()+
  facet_wrap(~condsgroup, nrow=1) + 
  theme(legend.position = 'none', axis.text.x = element_blank())

drugs<-ggplot(data=plot_cond_prim_ethnic %>% filter(measure=='drugs'), aes(x= measure, y = med, fill=ethnic)) +
  geom_bar(position='dodge', stat='identity', colour= 'black') +
  geom_errorbar(aes(ymin=lq, ymax=uq),width=0.2, position=position_dodge(width = 0.9), colour='black') +
  ggtitle('Number of prescribed drugs')+
  xlab('')+
  ylab('Median') +
  ylim(c(0,20.5))+
  scale_fill_brewer(palette = "Spectral", direction=-1) +
  theme_bw()+
  facet_wrap(~condsgroup, nrow=1) + 
  theme(legend.position = 'right', axis.text.x = element_blank())


plot_cond_second_ethnic <- outcomes_linked %>%
  filter(!is.na(ethnic)) %>% 
  group_by(condsgroup, ethnic) %>%
  add_tally(name='total_n') %>%
  summarise_at(vars('total_n','APCspells_binary','AEatts_binary', 'OPappts_binary'), mean) %>% 
  pivot_longer(-c(condsgroup, ethnic, total_n), names_to = "measure", values_to = "prop") %>% 
  mutate(n_true = prop*total_n) %>% 
  mutate(n_false = (1-prop)*total_n)

out<-ggplot(data=plot_cond_second_ethnic %>% filter(measure=='OPappts_binary'), aes(x= measure, y = prop * 100, fill=ethnic)) +
  geom_bar(position='dodge', stat='identity', colour= 'black') +
  ylab('Percentage of patients') +
  ggtitle('Outpatient Appts')+
  xlab('')+
  ylim(c(0,100))+
  scale_fill_brewer(palette = "Spectral", direction=-1) +
  theme_bw()+
  facet_wrap(~condsgroup, nrow=1) + 
  theme(legend.position = 'none', axis.text.x = element_blank())



inp<-ggplot(data=plot_cond_second_ethnic %>% filter(measure=='APCspells_binary'), aes(x= measure, y = prop * 100, fill=ethnic)) +
  geom_bar(position='dodge', stat='identity', colour= 'black') +
  ylab('Percentage of patients') +
  ggtitle('Inpatient Spells')+
  xlab('')+
  ylim(c(0,100))+
  scale_fill_brewer(palette = "Spectral", direction=-1) +
  theme_bw()+
  facet_wrap(~condsgroup, nrow=1) + 
  theme(legend.position = 'none', axis.text.x = element_blank())


ae<-ggplot(data=plot_cond_second_ethnic %>% filter(measure=='AEatts_binary'), aes(x= measure, y = prop * 100, fill=ethnic)) +
  geom_bar(position='dodge', stat='identity', colour= 'black') +
  ylab('Percentage of patients') +
  ggtitle('A&E Attendances')+
  xlab('')+
  ylim(c(0,100))+
  scale_fill_brewer(palette = "Spectral", direction=-1) +
  theme_bw()+
  facet_wrap(~condsgroup, nrow=1) + 
  theme(legend.position = 'none', axis.text.x = element_blank())

p_ethnic<-gridExtra::grid.arrange(gridExtra::grid.arrange(cons, drugs, nrow=1),gridExtra::grid.arrange(out, inp, ae, nrow=1), nrow=2)




