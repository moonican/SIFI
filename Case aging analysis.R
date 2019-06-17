#Case aging analysis
#3/17/2019

#Import the libraries you will need for the analysis
library(tidyverse)
library(scales)
library(RColorBrewer)

#Import .csv downloaded from innovationlawlab.org and give it a generic name
parties_report <- read_csv("C:/Users/monica.whatley/Downloads/parties_report_20190317-1306.csv", 
col_types = cols(
  address = col_skip(), 
  anum = col_skip(), 
  city = col_skip(), 
  closed = col_date(format = "%Y-%m-%d"), 
  dob = col_skip(), 
  email = col_skip(), 
  gender = col_skip(), 
  last_entry_at = col_skip(), 
  last_entry_manner = col_skip(), 
  last_entry_place = col_skip(), 
  married_at = col_skip(), 
  married_in = col_skip(), 
  narrative = col_skip(), 
  num_children = col_skip(), 
  num_marriages = col_skip(), 
  opened = col_date(format = "%Y-%m-%d"), 
  phone = col_skip(), 
  pob = col_skip(), 
  relationship = col_skip(), 
  role = col_skip(), 
  ssn = col_skip(), 
  state = col_skip(), 
  zip = col_skip()
  )
)

#Clean up the file a bit
parties_report$case_type <- gsub("-.*","",parties_report$case_type)
parties_report$opened <- as.Date(parties_report$opened, format = "%Y-%m-%d")
parties_report$closed <- as.Date(parties_report$closed, format = "%Y-%m-%d")


#Show how many unique parties are in the file
length(unique(parties_report$cid))

#Show how many unique parties are in each case type
unique_parties <- parties_report %>% group_by(case_type) %>% summarize(unique_parties = n_distinct(cid))

#Subset the cases you want to use in the analysis
analysis_dataset <- parties_report %>% filter(is.na(closed))
analysis_dataset <- analysis_dataset %>% filter(attorney %in% c("FOLKSTON","IRWIN","STEWART","LA SALLE","PINE PRAIRIE"))
analysis_dataset <- analysis_dataset %>% filter(case_type %in% c("STE_REFERL ", "LAS_REFERL ", "PP_REFERL ", "FLK_REF ", "IRW_REFERL ", "FLK_SCR ", "IRW_SCR ", "LAS_SCR ", "PP_SCR ", "STE_SCR ", "STE_RS ", "FLK_RS ", "IRW_RS ", "LAS_RS ", "PP_RS "))

#Confirm how many unique parties are in each case type
unique_parties <- analysis_dataset %>% group_by(case_type) %>% summarize(unique_parties = n_distinct(cid))

#Create a new field calculating the time elapsed for each case
analysis_dataset <- analysis_dataset %>% mutate(time_elapsed = Sys.Date() - opened)

#Create new field collapsing the case types
analysis_dataset <- analysis_dataset %>% mutate(case_type_r = ifelse(case_type %in% c("STE_REFERL ","LAS_REFERL ","PP_REFERL ","FLK_REF ", "IRW_REFERL "),"Referral",ifelse(case_type %in% c("FLK_SCR ","LAS_SCR ","PP_SCR ","STE_SCR ","IRW_SCR "),"Screening","Release")))

#Confirm that you did this correctly
check <- analysis_dataset %>% group_by(case_type_r,case_type) %>% summarize(unique_parties = n_distinct((cid)))

#Plot elapsed time by case type and site
ggplot(analysis_dataset, aes(x=attorney, y=time_elapsed)) + geom_boxplot() + ggtitle("SIFI Wait Times by Case Type") + facet_grid(case_type_r ~ .)
ggplot(subset(analysis_dataset, case_type_r == "Referral"), aes(x=time_elapsed)) + geom_boxplot() + ggtitle("SIFI Wait Times by Case Type") + facet_grid(attorney ~ .)

ggplot(subset(analysis_dataset, case_type_r == "Referral"), aes(x=time_elapsed)) + geom_histogram() + ggtitle("SIFI Wait Times by Case Type") + facet_grid(attorney ~ .)

analysis_dataset$case_type_r <- factor(analysis_dataset$case_type_r, levels = c("Referral", "Screening", "Release"))
analysis_dataset$attorney <- factor(analysis_dataset$attorney, levels = c("STEWART","IRWIN","LA SALLE", "PINE PRAIRIE", "FOLKSTON"))

  ggplot(analysis_dataset, aes(case_type_r, time_elapsed)) + scale_fill_brewer(palette="Set1") +
  geom_boxplot(aes(fill = attorney)) + coord_flip() + ggtitle("SIFI Wait Times by Case Type") + labs(y = "Time Elapsed (Days)", x = "Case Type") + scale_y_continuous(breaks= seq(0,534,by=30)) + guides(fill = guide_legend(title="Site",reverse=TRUE))
  facet_grid(Group ~ ., scales = "free_y") 