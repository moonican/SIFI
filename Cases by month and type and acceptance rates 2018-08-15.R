library(tidyverse)
library(scales)

AllCases_AllSites <- AllCases_20181127

AllCases_AllSites$OPENED_MONTH<-cut(AllCases_AllSites$OPENED,breaks="month")
AllCases_AllSites$CLOSED_MONTH<-cut(AllCases_AllSites$CLOSED,breaks="month")

Referrals<-filter(AllCases_AllSites,(CASETYPE=="STE_REFERL"|CASETYPE=="LAS_REFERL"|CASETYPE=="FLK_REF"|CASETYPE=="IRW_REFERL"))
Screenings<-filter(AllCases_AllSites,(CASETYPE=="FLK_SCR"|CASETYPE=="IRW_SCR"|CASETYPE=="LAS_SCR"|CASETYPE=="STE_SCR"))
Release<-filter(AllCases_AllSites,(CASETYPE=="STE_Releas"|CASETYPE=="FLK_RS"|CASETYPE=="IRW_RS"|CASETYPE=="LAS_RS"))


#Referrals<-filter(AllCases_20180815,CASETYPE =="STE_REFERL - Stewart Referral")
#Screenings<-filter(AllCases_20180815,CASETYPE=="STE_SCR - Stewart Screening Interview")
#Release<-filter(AllCases_20180815,CASETYPE=="STE_Releas - Stewart Release Strategy")

Referrals<-mutate(Referrals,SCREENED=ifelse(LLNUM %in% Screenings$LLNUM,"Yes","No"))
Referrals<-mutate(Referrals,ACCEPTED=ifelse(LLNUM %in% Release$LLNUM,"Yes","No"))
Screenings<-mutate(Screenings,ACCEPTED=ifelse(LLNUM %in% Release$LLNUM,"Yes","No"))

Screenings$OPENED_MONTH <- as.Date(Screenings$OPENED_MONTH, format = "%Y-%m-%d")
Referrals$OPENED_MONTH <- as.Date(Referrals$OPENED_MONTH, format = "%Y-%m-%d")

Referrals_new<-filter(Referrals,OPENED>='2018-04-01')
Screenings_new<-filter(Screenings,OPENED>='2018-04-01')

ggplot(Screenings,aes(OPENED_MONTH)) + geom_bar(aes(fill=ACCEPTED)) + ggtitle("Screenings per month and acceptance rate") + labs(x = "Month", y = "Number of screenings") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

ggplot(Referrals,aes(OPENED_MONTH)) + geom_bar(aes(fill=SCREENED)) + ggtitle("Referrals per month and screen rate") + labs(x = "Month", y = "Number of referrals") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

ggplot(Screenings %>% group_by(OPENED_MONTH) %>% count(OPENED_MONTH, ACCEPTED) %>%    # Group by region and species, then count number in each group
         mutate(pct=n/sum(n)),              # Calculate percent within each region
       aes(OPENED_MONTH, n, fill=ACCEPTED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("Screenings per month and acceptance rate") + labs(x = "Month", y = "Number of screenings") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

ggplot(Referrals_new %>% group_by(OPENED_MONTH) %>% count(OPENED_MONTH, SCREENED) %>%    # Group by region and species, then count number in each group
         mutate(pct=n/sum(n)),              # Calculate percent within each region
       aes(OPENED_MONTH, n, fill=SCREENED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("Referrals per month and screen rate") + labs(x = "Month", y = "Number of referrals") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))


ggplot(Referrals %>% group_by(OPENED_MONTH, ATTORNEY) %>% count(OPENED_MONTH, SCREENED) %>%    # Group by region and species, then count number in each group
         mutate(pct=n/sum(n)),              # Calculate percent within each region
       aes(OPENED_MONTH, n, fill=SCREENED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("SIFI referrals per month and screen rate") + labs(x = "Month", y = "Number of referrals") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y")) + facet_grid(ATTORNEY ~ .)

ggplot(Screenings %>% group_by(OPENED_MONTH, ATTORNEY) %>% count(OPENED_MONTH, ACCEPTED) %>%    # Group by region and species, then count number in each group
         mutate(pct=n/sum(n)),              # Calculate percent within each region
       aes(OPENED_MONTH, n, fill=ACCEPTED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("SIFI screenings per month and acceptance rate") + labs(x = "Month", y = "Number of screenings") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y")) + facet_grid(ATTORNEY ~ .)


statsFileName <- "C:/Users/monica.whatley/Documents/MONICA WHATLEY/Weekly reporting/sitestats.csv"

write.csv(summarytable, file = statsFileName, na="")