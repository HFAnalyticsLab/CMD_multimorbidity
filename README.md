# Common mental disorders and additional long-term conditions

#### Project Status: In progress

## Project Description

Depression and anxiety are common mental disorders, experienced by around 1 in 5 of us in the UK. But the NHS has recognised that mental healthcare services need improvement and will be investing substantially more money in this area over the next 4 years. 

We know that generally, patients coming to the NHS for treatment are increasingly likely to have several medical conditions that they are dealing with. This is also likely to be true for patients with depression or anxiety, but the number of depressed or anxious patients with additional medical conditions is not well understood and neither are their healthcare needs. 

We will describe how many patients with common mental disorders (depression or anxiety) are dealing with additional long-term conditions and whether there are certain groups of patients that are more likely to be affected (e.g. older people or those in more deprived areas). We will describe the demographic characteristics and additional conditions of people with common mental disorders over the last ten years. We will describe how additional conditions and other characteristics affect which types of NHS services patients with common mental disorders are currently using, and how often they use them.  

As the NHS invests more money in improving mental health services, it is important to understand the needs of patients with common mental disorders and the way in which they are using the NHS. This includes understanding how additional long-term conditions can affect the health needs of these patients, so that it is possible target improvements appropriately.

The code in this repository is used for data preparation, to define and describe the study cohort (eg demographic details, clinical characteristics) and to quantify health care utilisation and health outcomes.

## Data Sources

We are using data from the Clinical Practice Research Datalink (CPRD), ISAC protocol number 19_178. This includes linked HES, ONS death registration and area deprivation (IMD) data for the subset of patients for whom this is available.

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data were accessed in The Health Foundation's Secure Data Environment, which is a secure data analysis facility (accredited for the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information that could directly identify a patient or other individual was used. Variables labelled 'patid' do not refer to NHS IDs or other identifiable patient data.

Various code lists and reference files used in this analyis can be found in the 'code_lists' folder. The code list used for ethnicity was from Wright et al., 2017, and can be downloaded from the [Clinical Codes repository](https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/56/codelist/res56-ethnicity).

## How does it work?

As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on other patient-level CPRD extracts.

### Requirements

These scripts were written in R version 3.6.2 and RStudio version 1.2.5033. 

The following R packages (available on CRAN) are needed for the data preparation scripts (01 to 09):

* [**here**](https://cran.r-project.org/package=here)
* [**data.table**](https://cran.r-project.org/package=data.table)
* [**tidyverse**](https://cran.r-project.org/package=tidyverse)
* [**xml2**](https://cran.r-project.org/package=xml2)
* [**rvest**](https://cran.r-project.org/package=rvest)
* [**readxl**](https://cran.r-project.org/package=readxl)

The following additional R packages (available on CRAN) are needed for the data analysis scripts (10 to 13):

* [**plyr**](https://cran.r-project.org/package=plyr)
* [**table1**](https://cran.r-project.org/package=table1)
* [**haven**](https://cran.r-project.org/package=haven)
* [**margins**](https://cran.r-project.org/package=margins)
* [**flextable**](https://cran.r-project.org/package=flextable)
* [**officer**](https://cran.r-project.org/package=officer)
* [**gridExtra**](https://cran.r-project.org/package=gridExtra)
* [**scales**](https://cran.r-project.org/package=scales)

### Getting started

The 'R' folder contains:

1. CPRD data extract file combining:
* '01_Combine_CPRD_data.R' - reads in multiple large raw CPRD extracts, keeping selected columns, combines the extracts and saves to CSV.

2. Cambridge Multimorbidity Score v1.1 creation, see [CPRD at Cambridge](https://www.phpc.cam.ac.uk/pcu/cprd_cam/codelists):
* '02_Create_CamCodeList.R' - sources code lists from the Cambridge website and then combines them with 'Appendix_1_Cam_UD_based_logic.csv' to create a single long format reference file 'CamCodeList.rds'. **This script can be skipped** as the 'CamCodeList.rds' file is available in the 'code_lists' folder.
* '03_Create_Cambridge_Score.R' - uses CamCodeList.rds and CPRD data to identify the various conditions in the Cambridge Multimorbidity Score for the study cohort.

3. Checking patients' eligibility for the study cohort:
* '04_Check_CPRD_cohort_eligibility.R' - performs a range of basic checks on the data and then identifies which patients meet the study eligibility criteria (based on 'Appendix_2_CMD_Codes_No_Z_Drugs.xlsx' in the 'code_lists' folder).

4. Creating datasets for analysis:
* '05_Create_CPRD_trends_dataset.R' - joins the cohort eligibility files onto the Cambridge Multimorbidity Score data, HES linkage eligibility, linked IMD and ONS death registration data, to create a dataset for analysis of trends across the 10 study years.
* '06_Create_CPRD_analysis_datasets.R' - processes CPRD data on consultations, referrals and therapies, to create a dataset for analysis of primary care utilisation during follow-up for the 2015/16 cohort (relevant referrals based on 'Appendix_3_MH_Referral_Read_Codes.txt', relevant therapies based on 'Appendix_4_Psych_Prod_Codes.txt', both in the 'code_lists' folder). Also, harmonises ethnicity codings using both CPRD and HES sources (using 'res56-ethnicity.csv' in the 'code_lists' folder).
* '07_Create_HES_analysis_dataset.R' - processes HES data on admitted patient care, A&E attendances and outpatient care, to create a dataset for analysis of secondary care utilisation during follow-up for the 2015/16 cohort.
* '08_Create_ACSC_admissions.R' - identifies which admitted patient care spells were potentially avoidable chronic ambulatory care sensitive emergency admissions, using the method outlined by the [NHS Digital Clinical Indicators Team](https://files.digital.nhs.uk/BB/6DD6C7/CCG_2.6_I00757_S.pdf).
* '09_Create_HES_linked_outcomes.R' - creates a dataset with a variety of recoded explanatory variables, and primary and secondary care utilisation outcomes for the study cohort in the baseline year (2015/16).

5. Analysing the data:
* '10_Analysing_cohort_baseline_year.R' - analyses the cohort demographics and Cambridge Multimorbidity Score conditions in the study baseline year (2015/16) and compares it to a high-cost cohort from a previous study.
* '11_Analysis_across_years.R' - analyses trends in Cambridge Multimorbidity Score conditions across the 10 study years.
* '12_Analysis_primary_care_use.R' - analyses primary care utilisation (including consultations, drug therapies and mental health referrals) in the study baseline year (2015/16).
* '13_Analysis_secondary_care_use.R' - analyses secondary care utilisation (including inpatient, outpatient and A&E utilisation) in the study baseline year (2015/16).

## License

This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/CMD_multimorbidity/blob/master/LICENSE).

## Authors - please feel free to get in touch

* Will Parry, PhD - [website](http://willparry.net) / [on twitter](https://twitter.com/DrWillParry)
* Mai Stafford, PhD - [on twitter](https://twitter.com/stafford_xm)
* Karen Hodgson, PhD - [on github](https://github.com/KarenHodgson) / [on twitter](https://twitter.com/KarenHodgePodge)

adding a line
