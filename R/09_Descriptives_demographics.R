# Describes size of the cohort and their characteristics for the baseline year (2015/2016) 
# includes comparison to general pop random sample

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(here)
library(haven)
library(RColorBrewer)
library(scales)
library(broom)

here() #check here sees root directory for project
source(here('filepaths.R')) #get file path for random general pop sample; HCUmorbid, containing ltsuse.sas7bdat

thf_red_gradient <- seq_gradient_pal('#dd0031','#f7bfcc', 'Lab')(seq(0,1, length.out = 3))
thf_categories <- c('#dd0031', '#53a9cd', '#744284')

# read in data - cprd sample ----------------------------------------------
cprd2015 <- readRDS(here::here("Analysis","Processed_data","cprd_hes_linked_outcomes.rds"))


#for comparison with random sample
cprd2015$condsgroup_n<-as.factor(as.numeric(cprd2015$condsgroup))

# read in data - random sample (from 17_150) ----------------------------------------------

randomsample <- read_sas(paste0(HCUmorbid, 'ltsuse.sas7bdat'))
randomadults <- subset(randomsample, startage>17)
randomadults <- randomadults %>% 
  mutate(gender = factor(gender)) %>%
  mutate(numMHconds=select(.,c(depanx, ALC, ANO, DEM, LEA, PSO)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(cardiov=select(.,c(CHD, STR, PVD)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(cardiovasc=ifelse(cardiov>1, 1, cardiov))

randomadults$gender <- ordered(randomadults$gender, levels=c(1,2), labels=c("Men", "Women"))

randomadults$agegroup3 <- cut(randomadults$startage, breaks=c(-Inf, 44, 64, Inf),
                              labels=c("18-44y", "45-64y", "65+y"))  

randomadults$condsgroup <- cut(randomadults$total, breaks=c(-Inf, 0, 2, Inf),
                               labels=c("0 conditions", "1 / 2 condition", "3+ conditions"))  

#for comparison with cmd sample
randomadults$condsgroup_n<-as.factor(as.numeric(randomadults$condsgroup))

randomadults <- randomadults %>% 
  mutate(imd_3 = as.factor(if_else(imd2015_10 <= 2,  1, if_else(imd2015_10 >=9, 3, 2))))

# Cohort descriptives -----------------------------------------------------
summary(cprd2015[,c("agegroup3","gender","imd_5f","ethnic","condsgroup")])
write.csv(cprd2015 %>% 
              group_by(agegroup3) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'Summary_Age.csv'), row.names = F)

write.csv(cprd2015 %>% 
            group_by(gender) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'Summary_Gender.csv'), row.names = F)

write.csv(cprd2015 %>% 
            group_by(imd_5f) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'Summary_IMD5.csv'), row.names = F)

write.csv(cprd2015 %>% 
            group_by(ethnic) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results', 'cohort_descriptives','Summary_Ethnicity.csv'))

write.csv(cprd2015 %>% 
            group_by(condsgroup) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results', 'cohort_descriptives','Summary_Conditions.csv'), row.names = F)

# Cohort descriptives; no. of conditions by demog ---------------------------------------

write.csv(cprd2015 %>% 
            group_by(agegroup3,condsgroup) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results', 'cohort_descriptives','Summary_Age_Conds.csv'), row.names = F)

write.csv(cprd2015 %>% 
            group_by(gender,condsgroup) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results', 'cohort_descriptives','Summary_Gender_Conds.csv'), row.names = F)

write.csv(cprd2015 %>% 
            group_by(imd_5f,condsgroup) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results', 'cohort_descriptives','Summary_IMD5_Conds.csv'), row.names = F)

write.csv(cprd2015 %>% 
            group_by(ethnic,condsgroup) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results', 'cohort_descriptives','Summary_Ethnicity_Conds.csv'), row.names = F)

# Random sample cohort descriptives ---------------------------------------

summary(randomadults[,c("agegroup3","gender","condsgroup")])
write.csv(randomadults %>% 
            group_by(agegroup3) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'Summary_Random_Age.csv'), row.names = F)

write.csv(randomadults %>% 
            group_by(gender) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'Summary_Random_Gender.csv'), row.names = F)

write.csv(randomadults %>% 
            group_by(condsgroup) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results', 'cohort_descriptives','Summary_Random_Conditions.csv'), row.names = F)

# Random sample cohort; no. of conditions by demog ---------------------------------------
write.csv(randomadults %>% 
            group_by(agegroup3,condsgroup) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'Summary_Random_Age_Conds.csv'), row.names = F)

write.csv(randomadults %>% 
            group_by(gender,condsgroup) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'Summary_Random_Gender_Conds.csv'), row.names = F)

write.csv(randomadults %>% 
            group_by(imd_3,condsgroup) %>% 
            summarise(N=n()) %>% 
            mutate(freq = N/sum(N)), file = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'Summary_Random_IMD_Conds.csv'), row.names = F)

# Compare CMD to random cohort - number of additional conditions ----------

cmd_age_cond<-cprd2015 %>% 
  count(agegroup3, condsgroup_n) %>% # count per group
  group_by(agegroup3) %>% 
  mutate(perc=(prop.table(n)*100)) %>% 
  mutate(cohort="CMD Pop")


random_age_cond<-randomadults %>% 
  count(agegroup3, condsgroup_n) %>% # count per group
  group_by(agegroup3) %>% 
  mutate(perc=(prop.table(n)*100)) %>% 
  mutate(cohort="General Pop")

plot_conds_cmd_vs_rand <- rbind(cmd_age_cond, random_age_cond) 
plot_conds_cmd_vs_rand %>% 
  ggplot(aes(fill=(condsgroup_n), x=cohort, y=perc)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~agegroup3) + 
  scale_fill_manual(values=rev(thf_categories), labels = c('0', '1-2','3+')) +
  labs(fill="Long term conditions", y= 'Percentage of patients') +
  scale_x_discrete(name = "", limits = c("General Pop", "CMD Pop")) +
  theme_bw() +
  theme(legend.position="bottom")+
  ggtitle("Percentage of patients with long term conditons")

write.csv(plot_conds_cmd_vs_rand,file = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'plot_conds_cmd_vs_rand.csv'), row.names = F)

# Number of conditions by IMD * age [Figure 1 long-read ] ---------------------------------------------


plot_cmd_conds_imd_age<-cprd2015 %>% 
  count(agegroup3, imd_3,condsgroup_n) %>% # count per group
  group_by(agegroup3, imd_3) %>% 
  mutate(perc=(prop.table(n)*100))  %>% 
  mutate(cohort="CMD")

plot_cmd_conds_imd_age %>% 
  ggplot(aes(fill=condsgroup_n, x=imd_3, y=perc)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~agegroup3, nrow=1, labeller = labeller( agegroup3 =
                                                        c('18-44y' = '18 - 44 years old',
                                                          '45-64y' = '45 - 64 years old',
                                                          '65+y' = '65 years and older'))) + 
  scale_fill_manual(values=rev(thf_categories), labels = c('0', '1 - 2 ','3+')) +
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  labs(fill="Number of additional long-term conditions", y= 'Percentage of patients', x= "Level of deprivation") +
  theme_bw() +
  theme(legend.position="bottom", axis.text.x = element_text(angle=45, hjust = 1))+
  ggtitle("Additional long-term conditons among people with common mental disorders, by age and deprivation level")

ggsave(filename = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'plot_cmd_conds_imd_age.png'), width = 12, height = 7)

write.csv(plot_cmd_conds_imd_age,file = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'plot_cmd_conds_imd_age.csv'), row.names = F)


# Number of conditions by gender * age ------------------------------------------

numconds_by_agegender <- cprd2015 %>% 
  group_by(gender, agegroup3) %>% 
  summarize(N=length(numconds), mean_numconds=mean(numconds), se_numconds=sd(numconds)/sqrt(length(numconds)))

plot_conds_gender_age<-cprd2015 %>% 
  count(agegroup3, gender,condsgroup) %>% # count per group
  group_by(agegroup3, gender) %>% 
  mutate(perc=(prop.table(n)*100))

plot_conds_gender_age %>% 
  ggplot(aes(fill=condsgroup, x=gender, y=perc)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~agegroup3) + 
  scale_fill_manual(values=rev(thf_categories), labels = c('0', '1-2','3+')) +
  labs(fill="Long term conditions", y= 'Percentage of patients', x= "Gender") +
  theme_bw() +
  theme(legend.position="bottom")+
  ggtitle("Percentage of patients with long term conditons by gender")

# Number of conditions by ethnicity * age ---------------------------------------

numconds_by_ageethnic <- cprd2015 %>% 
  group_by(ethnic, agegroup3) %>% 
  summarize(N=length(numconds), mean_numconds=mean(numconds), se_numconds=sd(numconds)/sqrt(length(numconds)))

plot_conds_ethnic_age<-cprd2015 %>% 
  count(agegroup3, ethnic,condsgroup) %>% # count per group
  group_by(agegroup3, ethnic) %>% 
  mutate(perc=(prop.table(n)*100)) 


plot_conds_ethnic_age %>% 
  ggplot(aes(fill=condsgroup, x=ethnic, y=perc)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~agegroup3) + 
  scale_fill_manual(values=rev(thf_categories), labels = c('0', '1-2','3+')) +
  labs(fill="Long term conditions", y= 'Percentage of patients', x= "Ethnicity") +
  theme_bw() +
  theme(legend.position="bottom")+
  ggtitle("Percentage of patients with long term conditons by ethnicity")

# Types of condition (CMD vs Random sample) -------------------------------------------

list_all_conds<-c("ALC","ANO","ANX","AST","ATR","BLI","BRO","CAN","CHD","CKD","CLD","CON","COP","DEM","DEP","DIB","DIV","EPI","HEF","HEL","HYP","IBD","IBS","LEA","MIG" ,"MSC","PEP","PNC","PRK","PRO","PSM","PSO","PVD","RHE","SCZ","SIN","STR","THY")

condits <- cprd2015 %>% 
  select(list_all_conds) %>% 
  select(-c(DEP,ANX)) %>% 
  summarize_all(mean) %>% 
  sort(decreasing = T) %>% 
  mutate(sample="CMD")

#check prop in random sample
list_rand_conds <- setdiff(list_all_conds, c('PEP','PSM')) #not in original random sample
rand_condits <- randomadults %>% 
  select(c(list_rand_conds)) %>% 
  summarize_all(list(mean)) %>% 
  as.data.frame() %>% 
  sort(decreasing = T) %>% 
  mutate(sample="random") 


# Sample * IMD * age * for pain [ Figure 2 long-read] -------------------------------------------

pain_imd_age <- cprd2015 %>% 
  select(c(PNC,imd_3, agegroup3)) %>% 
  group_by(imd_3, agegroup3) %>% 
  add_tally(name='total_n') %>% 
  summarize_all(list(mean)) %>% 
  pivot_longer(-c(imd_3, total_n, agegroup3), names_to='condition',values_to = 'prop') %>% 
  mutate(n_true = prop*total_n) %>% 
  mutate(n_false = (1-prop)*total_n) %>% 
  mutate(sample="CMD Pop") 

pain_imd_age_random <- randomadults %>% 
  select(c(PNC,imd_3, agegroup3)) %>% 
  group_by(imd_3, agegroup3) %>% 
  add_tally(name='total_n') %>% 
  summarize_all(list(mean)) %>% 
  pivot_longer(-c(imd_3, total_n, agegroup3), names_to='condition',values_to = 'prop') %>% 
  mutate(n_true = prop*total_n) %>% 
  mutate(n_false = (1-prop)*total_n) %>% 
  mutate(sample="General Pop") 

##combine for output file
pain<-rbind(pain_imd_age,pain_imd_age_random)
write.csv(pain, here::here('Analysis', 'Analysis_Results', 'cohort_descriptives', 'pain_imd_age_cmd_rand.csv'))

pain_imd_age %>% 
  ggplot(aes(x=imd_3, y=prop*100)) +
  facet_wrap(~agegroup3, nrow=1, labeller = labeller( agegroup3 =
                                                        c('18-44y' = '18 - 44 years old',
                                                          '45-64y' = '45 - 64 years old',
                                                          '65+y' = '65 years and older'))) +
  geom_bar(stat='identity', colour='black',aes(fill= imd_3)) +
  scale_fill_manual(values=rev(thf_red_gradient))+ 
  scale_x_discrete(labels= c('1' = "Low", '2' = 'Medium', '3' = 'High'))+
  labs(x = 'Level of deprivation', y = 'Percentage of patients', caption = ' Dashed line indicates percentages in general population sample') +
  ggtitle('Painful conditions among people with common mental disorders, by age and deprivation level') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") +
  geom_errorbar(data = pain_imd_age_random, aes(ymin=prop*100, ymax=prop*100),colour='black',linetype='dashed', width=0.9, position=position_dodge(width = 0.9)) 

ggsave(filename = here::here('Analysis', 'Analysis_Results','cohort_descriptives', 'pain_imd_age_cmd_rand.png'), width=12, height = 7)



