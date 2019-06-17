library(tidyverse)
library(scales)

#Create general data frame name

Stewart_parties <- Stewart_parties_20190111
screencompletes_and_denyreps_STE <- screencompletes_and_denyreps_STE_20190111

#Create month variable

Stewart_parties$OPENED_MONTH<-cut(Stewart_parties$opened,breaks="month")
Stewart_parties$CLOSED_MONTH<-cut(Stewart_parties$closed,breaks="month")

#Clean casetype variable

Analysis_Dataset <- Stewart_parties
Analysis_Dataset <- separate(data = Analysis_Dataset, col = case_type, into = c("CASETYPE", "CASE TYPE DESCRIPTION"), sep = "-")

#Create new data frames for deny rep and screen complete

ScreenComplete <- screencompletes_and_denyreps_STE %>% filter(action=="ScreenComplete_STE") %>% select(cid,screencompletedate=date)
DenyRep<- filter(screencompletes_and_denyreps_STE,action=="DenyRep_STE") %>% select(cid,denyrepdate=date)

#Create new data frames for each case type

Referrals<-filter(Analysis_Dataset,CASETYPE=="STE_REFERL ")
Screenings<-filter(Analysis_Dataset,CASETYPE=="STE_SCR ")
Release<-filter(Analysis_Dataset,CASETYPE=="STE_RS ")

#Create new variables for different events

Referrals<-mutate(Referrals,SCREENED=ifelse(cid %in% Screenings$cid,"Yes","No"))
Referrals<-mutate(Referrals,ACCEPTED=ifelse(cid %in% Release$cid,"Yes","No"))
Referrals<-left_join(Referrals,ScreenComplete,by="cid")
Referrals<-left_join(Referrals,DenyRep,by="cid")

Screenings<-mutate(Screenings,ACCEPTED=ifelse(cid %in% Release$cid,"Yes","No"))
Screenings<-left_join(Screenings,ScreenComplete, by="cid")
Screenings<-left_join(Screenings,DenyRep,by="cid")

#Format dates for each case type
#STOPPED HERE ON 1/11/19 -- NEED TO FIX!!!!!!

Screenings$OPENED_MONTH <- as.Date(Screenings$OPENED_MONTH, format = "%Y-%m-%d")
Referrals$OPENED_MONTH <- as.Date(Referrals$OPENED_MONTH, format = "%Y-%m-%d")
Release$OPENED_MONTH <- as.Date(Release$OPENED_MONTH, format = "%Y-%m-%d")

Referrals$REF_OPENED_MONTH<-cut(Referrals$opened,breaks="month")
Referrals$REF_CLOSED_MONTH<-cut(Referrals$closed,breaks="month")

Screenings$SCR_OPENED_MONTH<-cut(Screenings$opened,breaks="month")
Screenings$SCR_CLOSED_MONTH<-cut(Screenings$closed,breaks="month")

Release$REL_OPENED_MONTH<-cut(Release$opened,breaks="month")
Release$REL_CLOSED_MONTH<-cut(Release$closed,breaks="month")

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

