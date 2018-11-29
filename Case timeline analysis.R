library(tidyverse)
library(scales)

AllCases_AllSites <- AllCases_20181127

#Create month variable

AllCases_AllSites$OPENED_MONTH<-cut(AllCases_AllSites$OPENED,breaks="month")
AllCases_AllSites$CLOSED_MONTH<-cut(AllCases_AllSites$CLOSED,breaks="month")

#Winnow down the case types that you want

Analysis_Dataset <- AllCases_AllSites %>% filter(CASETYPE %in% c("STE_REFERL", "LAS_REFERL", "PP_REFERL", "FLK_REF", "IRW_REFERL", "FLK_SCR", "IRW_SCR", "LAS_SCR", "PP_SCR", "STE_SCR", "STE_Releas", "FLK_RS", "IRW_RS", "LAS_RS", "PP_RS"))
#whosnot <- AllCases_AllSites %>% filter(!CASETYPE %in% Analysis_Dataset$CASETYPE)

#Create new data frames for each case type

Referrals<-filter(Analysis_Dataset,(CASETYPE=="STE_REFERL"|CASETYPE=="LAS_REFERL"|CASETYPE=="FLK_REF"|CASETYPE=="IRW_REFERL"|CASETYPE=="PP_REFERL"))
Screenings<-filter(Analysis_Dataset,(CASETYPE=="FLK_SCR"|CASETYPE=="IRW_SCR"|CASETYPE=="LAS_SCR"|CASETYPE=="STE_SCR"|CASETYPE=="PP_SCR"))
Release<-filter(Analysis_Dataset,(CASETYPE=="STE_Releas"|CASETYPE=="FLK_RS"|CASETYPE=="IRW_RS"|CASETYPE=="LAS_RS"|CASETYPE=="PP_RS"))

Referrals<-mutate(Referrals,SCREENED=ifelse(LLNUM %in% Screenings$LLNUM,"Yes","No"))
Referrals<-mutate(Referrals,ACCEPTED=ifelse(LLNUM %in% Release$LLNUM,"Yes","No"))
Screenings<-mutate(Screenings,ACCEPTED=ifelse(LLNUM %in% Release$LLNUM,"Yes","No"))

#Format dates for each case type

Screenings$OPENED_MONTH <- as.Date(Screenings$OPENED_MONTH, format = "%Y-%m-%d")
Referrals$OPENED_MONTH <- as.Date(Referrals$OPENED_MONTH, format = "%Y-%m-%d")
Release$OPENED_MONTH <- as.Date(Release$OPENED_MONTH, format = "%Y-%m-%d")
  
Referrals$REF_OPENED_MONTH<-cut(Referrals$OPENED,breaks="month")
Referrals$REF_CLOSED_MONTH<-cut(Referrals$CLOSED,breaks="month")

Screenings$SCR_OPENED_MONTH<-cut(Screenings$OPENED,breaks="month")
Screenings$SCR_CLOSED_MONTH<-cut(Screenings$CLOSED,breaks="month")

Release$REL_OPENED_MONTH<-cut(Release$OPENED,breaks="month")
Release$REL_CLOSED_MONTH<-cut(Release$CLOSED,breaks="month")

Referrals$REF_OPENED_MONTH <- as.Date(Referrals$REF_OPENED_MONTH, format = "%Y-%m-%d")
Referrals$REF_CLOSED_MONTH <- as.Date(Referrals$REF_CLOSED_MONTH, format = "%Y-%m-%d")

Screenings$SCR_OPENED_MONTH <- as.Date(Screenings$SCR_OPENED_MONTH, format = "%Y-%m-%d")
Screenings$SCR_CLOSED_MONTH <- as.Date(Screenings$SCR_CLOSED_MONTH, format = "%Y-%m-%d")

Release$REL_OPENED_MONTH <- as.Date(Release$REL_OPENED_MONTH, format = "%Y-%m-%d")
Release$REL_CLOSED_MONTH <- as.Date(Release$REL_CLOSED_MONTH, format = "%Y-%m-%d")

#Merge by LawLab number in order to get open/closed dates on one line for each person

Referrals_merge<- Referrals %>% select("LLNUM","REF_OPENED_DATE" = "OPENED", "REF_CLOSED_DATE" = "CLOSED", "REF_OPENED_MONTH", "REF_CLOSED_MONTH", "ATTORNEY","REF_CASETYPE" = "CASETYPE")
Screenings_merge<- Screenings %>% select("LLNUM","SCR_OPENED_DATE" = "OPENED", "SCR_CLOSED_DATE" = "CLOSED", "SCR_OPENED_MONTH", "SCR_CLOSED_MONTH", "SCR_CASETYPE" = "CASETYPE")
Release_merge<- Release %>% select("LLNUM","REL_OPENED_DATE" = "OPENED", "REL_CLOSED_DATE" = "CLOSED", "REL_OPENED_MONTH", "REL_CLOSED_MONTH", "REL_CASETYPE" = "CASETYPE")

Clients_Merged<- full_join(Referrals_merge, Screenings_merge, by="LLNUM")
Clients_Merged<- full_join(Clients_Merged, Release_merge, by="LLNUM")

#Calculate time spent in each case type

Clients_Merged<- Clients_Merged %>% mutate(REF_TIME = REF_CLOSED_DATE - REF_OPENED_DATE)
Clients_Merged<- Clients_Merged %>% mutate(SCR_TIME = SCR_CLOSED_DATE - SCR_OPENED_DATE)
Clients_Merged<- Clients_Merged %>% mutate(REL_TIME = REL_CLOSED_DATE - REL_OPENED_DATE)


#Calculate alternate definition of time spent in each phase of the case

Clients_Merged<- Clients_Merged %>% mutate(REF_TIME2 = SCR_OPENED_DATE - REF_OPENED_DATE)
Clients_Merged<- Clients_Merged %>% mutate(SCR_TIME2 = REL_OPENED_DATE - SCR_OPENED_DATE)
Clients_Merged<- Clients_Merged %>% mutate(REL_TIME2 = REL_CLOSED_DATE - REL_OPENED_DATE)

#Calculate alternate definition of time spent in each phase using unmerged data
Analysis_Dataset <- Analysis_Dataset %>% mutate(WAIT_TIME = CLOSED - OPENED)
Analysis_Dataset$WAIT_TIME <- as.numeric(Analysis_Dataset$WAIT_TIME)

#Calculate acceptance status
Analysis_Dataset <- Analysis_Dataset %>% mutate(ACCEPTED=ifelse(LLNUM %in% Release$LLNUM,"Yes","No"))

#Box plot showing the distribution of wait times during each phase

Clients_Merged$REF_TIME2 <- as.numeric(Clients_Merged$REF_TIME2)

ggplot(Clients_Merged, aes(x=ATTORNEY, y=REF_TIME2)) + geom_boxplot() + ggtitle("SIFI referral wait time to screen")

ggplot(Analysis_Dataset, aes(x=CASETYPE, y=WAIT_TIME)) + geom_boxplot() + ggtitle("SIFI referral wait time to screen") + facet_grid(ACCEPTED ~ .)


#Try to summarize

Analysis_Dataset$WAIT_TIME[is.na(Analysis_Dataset$WAIT_TIME)] <- 0
mysummary<- Analysis_Dataset %>% arrange(CASETYPE, ACCEPTED) %>% group_by(CASETYPE, ACCEPTED) %>% summarize(median_wait=median(WAIT_TIME))
mysummary<- mysummary %>% spread(CASETYPE, ACCEPTED, WAIT_TIME)

#Export table

statsFileName <- "C:/Users/monica.whatley/Documents/MONICA WHATLEY/Weekly reporting/caseflow_stats.csv"

write.csv(mysummary, file = statsFileName, na="")

