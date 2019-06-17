#Parties report clean-up
#6.17.19

#Import the libraries you will need for the analysis
library(tidyverse)


#Import the .csv from the downloads folder
parties_report <- read_csv("C:/Users/monica.whatley/Downloads/parties_report_20190617-0911.csv", 
col_types = cols(address = col_skip(), 
city = col_skip(), dob = col_skip(), 
email = col_skip(), gender = col_skip(), 
last_entry_at = col_skip(), last_entry_manner = col_skip(), 
last_entry_place = col_skip(), married_at = col_skip(), 
married_in = col_skip(), narrative = col_skip(), 
num_children = col_skip(), num_marriages = col_skip(), 
phone = col_skip(), pob = col_skip(), 
relationship = col_skip(), role = col_skip(), 
ssn = col_skip(), state = col_skip(), 
zip = col_skip(),
closed = col_date(format = "%Y-%m-%d"), 
opened = col_date(format = "%Y-%m-%d")
)
)

#Clean up a little
parties_report$case_type <- gsub(" -.*","",parties_report$case_type)
parties_report$opened <- as.Date(parties_report$opened, format = "%Y-%m-%d")
parties_report$closed <- as.Date(parties_report$closed, format = "%Y-%m-%d")

#Show how many unique parties are in the file
length(unique(parties_report$cid))

#Show how many unique parties are in each case type
unique_parties <- parties_report %>% group_by(case_type) %>% summarize(unique_parties = n_distinct(cid))

#Show how many unique attorneys are in each case type
unique_attorneys <- parties_report %>% group_by(attorney) %>% summarize(unique_attorneys = n_distinct(cid))


#Subset the cases you want to use in the analysis
analysis_dataset <- parties_report %>% filter(is.na(closed))
analysis_dataset <- analysis_dataset %>% filter(attorney %in% c("FOLKSTON","IRWIN","STEWART","LA SALLE","PINE PRAIRIE"))
analysis_dataset <- analysis_dataset %>% filter(case_type %in% c("STE_REFERL", "LAS_REFERL", "PP_REFERL", "FLK_REF", "IRW_REFERL", "FLK_SCR", "IRW_SCR", "LAS_SCR", "PP_SCR", "STE_SCR", "STE_RS", "FLK_RS", "IRW_RS", "LAS_RS", "PP_RS"))

#Confirm how many unique parties are in each case type
unique_parties_small <- analysis_dataset %>% group_by(case_type) %>% summarize(unique_parties = n_distinct(cid))


#Create table of STE open cases for export and fax to Stewart Detention Center
fax_to_stewart<-select(analysis_dataset, firstname, lastname, anum, case_type, attorney)
fax_to_stewart<-filter(fax_to_stewart, attorney %in% c("STEWART"))
fax_to_stewart<-filter(fax_to_stewart, case_type %in% c("STE_SCR","STE_RS"))
fax_to_stewart$firstname <- gsub("\\(.*","",fax_to_stewart$firstname)
fax_to_stewart$lastname <- gsub("\\(.*","",fax_to_stewart$lastname)

#Save the stats to a .csv on my computer
statsFileName <- "C:/Users/monica.whatley/Documents/MONICA WHATLEY/Fax visitation list/2019-06-17/fax_to_stewart.csv"
write.csv(fax_to_stewart, file = statsFileName, na="")


#Create table of STE open cases for export and use in case rounds
case_rounds<-select(analysis_dataset, cid, firstname, lastname, anum, country_of_birth, language, opened, case_type, attorney)
case_rounds<-filter(case_rounds, attorney %in% c("STEWART"))
case_rounds<-filter(case_rounds, case_type %in% c("STE_SCR","STE_RS"))
case_rounds<-select(case_rounds, cid, firstname, lastname, anum, country_of_birth, language, opened, case_type)


#Save the stats to a .csv on my computer
statsFileName <- "C:/Users/monica.whatley/Documents/MONICA WHATLEY/Fax visitation list/2019-06-17/case_rounds.csv"
write.csv(case_rounds, file = statsFileName, na="")
