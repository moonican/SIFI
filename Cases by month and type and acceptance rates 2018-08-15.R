#Import libraries needed for this analysis
library(tidyverse)
library(scales)

#Create a general data frame name that can be used regardless of the version imported
AllCases_AllSites <- AllCases_20181127

#Create new variables for the month opened/closed
AllCases_AllSites$OPENED_MONTH<-cut(AllCases_AllSites$OPENED,breaks="month")
AllCases_AllSites$CLOSED_MONTH<-cut(AllCases_AllSites$CLOSED,breaks="month")

#Subset OTG site-specific casetypes as a separate data frame
Analysis_Dataset <- AllCases_AllSites %>% filter(CASETYPE %in% c("STE_REFERL", "LAS_REFERL", "PP_REFERL", "FLK_REF", "IRW_REFERL", "FLK_SCR", "IRW_SCR", "LAS_SCR", "PP_SCR", "STE_SCR", "STE_Releas", "FLK_RS", "IRW_RS", "LAS_RS", "PP_RS"))

#Subset referrals, screenings, and release casetypes as separate data frames
Referrals<-filter(Analysis_Dataset,(CASETYPE=="STE_REFERL"|CASETYPE=="LAS_REFERL"|CASETYPE=="FLK_REF"|CASETYPE=="IRW_REFERL"|CASETYPE=="PP_REFERL"))
Screenings<-filter(Analysis_Dataset,(CASETYPE=="FLK_SCR"|CASETYPE=="IRW_SCR"|CASETYPE=="LAS_SCR"|CASETYPE=="STE_SCR"|CASETYPE=="PP_SCR"))
Release<-filter(Analysis_Dataset,(CASETYPE=="STE_Releas"|CASETYPE=="FLK_RS"|CASETYPE=="IRW_RS"|CASETYPE=="LAS_RS"|CASETYPE=="PP_RS"))

#Referrals<-filter(AllCases_20180815,CASETYPE =="STE_REFERL - Stewart Referral")
#Screenings<-filter(AllCases_20180815,CASETYPE=="STE_SCR - Stewart Screening Interview")
#Release<-filter(AllCases_20180815,CASETYPE=="STE_Releas - Stewart Release Strategy")

#Create new variable indicating whether the person was screened/accepted
Referrals<-mutate(Referrals,SCREENED=ifelse(LLNUM %in% Screenings$LLNUM,"Yes","No"))
Referrals<-mutate(Referrals,ACCEPTED=ifelse(LLNUM %in% Release$LLNUM,"Yes","No"))
Screenings<-mutate(Screenings,ACCEPTED=ifelse(LLNUM %in% Release$LLNUM,"Yes","No"))

#Format the date for new opened_month variable
Screenings$OPENED_MONTH <- as.Date(Screenings$OPENED_MONTH, format = "%Y-%m-%d")
Referrals$OPENED_MONTH <- as.Date(Referrals$OPENED_MONTH, format = "%Y-%m-%d")

#Subset new referrals/screenings as separate data frames
Referrals_new<-filter(Referrals,OPENED>='2018-04-01')
Screenings_new<-filter(Screenings,OPENED>='2018-04-01')

#Plot screenings per month and acceptance rate
ggplot(Screenings,aes(OPENED_MONTH)) + geom_bar(aes(fill=ACCEPTED)) + ggtitle("Screenings per month and acceptance rate") + labs(x = "Month", y = "Number of screenings") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

#Plot referrals per month and screen rate
ggplot(Referrals,aes(OPENED_MONTH)) + geom_bar(aes(fill=SCREENED)) + ggtitle("Referrals per month and screen rate") + labs(x = "Month", y = "Number of referrals") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

#Plot screenings per month and acceptance rate with labels
ggplot(Screenings %>% group_by(OPENED_MONTH) %>% count(OPENED_MONTH, ACCEPTED) %>%
         mutate(pct=n/sum(n)),             
       aes(OPENED_MONTH, n, fill=ACCEPTED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("Screenings per month and acceptance rate") + labs(x = "Month", y = "Number of screenings") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

#Plot referrals per month and screen rate with labels
ggplot(Referrals_new %>% group_by(OPENED_MONTH) %>% count(OPENED_MONTH, SCREENED) %>%    
         mutate(pct=n/sum(n)),             
       aes(OPENED_MONTH, n, fill=SCREENED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("Referrals per month and screen rate") + labs(x = "Month", y = "Number of referrals") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

#Plot referrals per month and screen rate with labels and panel by site
ggplot(Referrals %>% group_by(OPENED_MONTH, ATTORNEY) %>% count(OPENED_MONTH, SCREENED) %>%    
         mutate(pct=n/sum(n)),              
       aes(OPENED_MONTH, n, fill=SCREENED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("SIFI referrals per month and screen rate") + labs(x = "Month", y = "Number of referrals") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y")) + facet_grid(ATTORNEY ~ .)

#Plot screenings per month and acceptance rate with labels and panel by site
ggplot(Screenings %>% group_by(OPENED_MONTH, ATTORNEY) %>% count(OPENED_MONTH, ACCEPTED) %>%  
         mutate(pct=n/sum(n)),           
       aes(OPENED_MONTH, n, fill=ACCEPTED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("SIFI screenings per month and acceptance rate") + labs(x = "Month", y = "Number of screenings") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y")) + facet_grid(ATTORNEY ~ .)

#Save the stats to a .csv on my computer
statsFileName <- "C:/Users/monica.whatley/Documents/MONICA WHATLEY/Weekly reporting/sitestats.csv"
write.csv(summarytable, file = statsFileName, na="")