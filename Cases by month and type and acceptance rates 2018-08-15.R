#Import libraries needed for this analysis
library(tidyverse)
library(scales)

#Import .csv
AllCases_20190404 <- read_csv("~/MONICA WHATLEY/Weekly reporting/AllCases_20190404.csv", 
col_types = cols(CLOSED = col_date(format = "%m/%d/%Y"), 
OPENED = col_date(format = "%m/%d/%Y")))

#Create a general data frame name that can be used regardless of the version imported
AllCases_AllSites <- AllCases_20190404

#Clean up
AllCases_AllSites$CASETYPE <- gsub(" -.*","",AllCases_AllSites$CASETYPE)

#Create new variables for the month opened/closed
AllCases_AllSites$OPENED_MONTH<-cut(AllCases_AllSites$OPENED,breaks="month")
AllCases_AllSites$CLOSED_MONTH<-cut(AllCases_AllSites$CLOSED,breaks="month")

#Create new variables for the week opened/closed
AllCases_AllSites$OPENED_WEEK<-cut(AllCases_AllSites$OPENED,breaks="week")
AllCases_AllSites$CLOSED_WEEK<-cut(AllCases_AllSites$CLOSED,breaks="week")


#Subset OTG site-specific casetypes as a separate data frame
Analysis_Dataset <- AllCases_AllSites %>% filter(CASETYPE %in% c("STE_REFERL", "LAS_REFERL", "PP_REFERL", "FLK_REF", "IRW_REFERL", "FLK_SCR", "IRW_SCR", "LAS_SCR", "PP_SCR", "STE_SCR", "STE_RS", "FLK_RS", "IRW_RS", "LAS_RS", "PP_RS"))

#Confirm how many unique parties are in each case type
unique_parties <- Analysis_Dataset %>% group_by(CASETYPE) %>% summarize(unique_parties = n_distinct(LLNUM))

#Also subset only casetypes with ATTORNEY=OTG sites in that same data frame
Analysis_Dataset <- Analysis_Dataset %>% filter(ATTORNEY %in% c("FOLKSTON","IRWIN","STEWART","LA SALLE","PINE PRAIRIE"))

#Re-code casetype to reflect uniform code across sites for each casetype
Analysis_Dataset <- Analysis_Dataset %>% mutate(case_type_r = ifelse(CASETYPE %in% c("STE_REFERL","LAS_REFERL","PP_REFERL","FLK_REF", "IRW_REFERL"),"Referral",ifelse(CASETYPE %in% c("FLK_SCR","LAS_SCR","PP_SCR","STE_SCR","IRW_SCR"),"Screening","Release")))
check <- Analysis_Dataset %>% group_by(case_type_r,CASETYPE) %>% summarize(unique_parties = n_distinct((LLNUM)))

#Subset referrals, screenings, and release casetypes as separate data frames
Referrals<-filter(Analysis_Dataset,(CASETYPE=="STE_REFERL"|CASETYPE=="LAS_REFERL"|CASETYPE=="FLK_REF"|CASETYPE=="IRW_REFERL"|CASETYPE=="PP_REFERL"))
Screenings<-filter(Analysis_Dataset,(CASETYPE=="FLK_SCR"|CASETYPE=="IRW_SCR"|CASETYPE=="LAS_SCR"|CASETYPE=="STE_SCR"|CASETYPE=="PP_SCR"))
Release<-filter(Analysis_Dataset,(CASETYPE=="STE_RS"|CASETYPE=="FLK_RS"|CASETYPE=="IRW_RS"|CASETYPE=="LAS_RS"|CASETYPE=="PP_RS"))

#Create new variable indicating whether the person was screened/accepted
Referrals<-mutate(Referrals,SCREENED=ifelse(LLNUM %in% Screenings$LLNUM,"Yes","No"))
Referrals<-mutate(Referrals,ACCEPTED=ifelse(LLNUM %in% Release$LLNUM,"Yes","No"))
Screenings<-mutate(Screenings,ACCEPTED=ifelse(LLNUM %in% Release$LLNUM,"Yes","No"))

#Format the date for new opened_month and opened_week variables
Screenings$OPENED_MONTH <- as.Date(Screenings$OPENED_MONTH, format = "%Y-%m-%d")
Referrals$OPENED_MONTH <- as.Date(Referrals$OPENED_MONTH, format = "%Y-%m-%d")
Release$OPENED_MONTH <- as.Date(Release$OPENED_MONTH, format = "%Y-%m-%d")
Analysis_Dataset$OPENED_MONTH <- as.Date(Analysis_Dataset$OPENED_MONTH, format = "%Y-%m-%d")

Screenings$OPENED_WEEK <- as.Date(Screenings$OPENED_WEEK, format = "%Y-%m-%d")
Referrals$OPENED_WEEK <- as.Date(Referrals$OPENED_WEEK, format = "%Y-%m-%d")
Release$OPENED_WEEK <- as.Date(Release$OPENED_WEEK, format = "%Y-%m-%d")
Analysis_Dataset$OPENED_WEEK <- as.Date(Analysis_Dataset$OPENED_WEEK, format = "%Y-%m-%d")


#Subset new referrals/screenings as separate data frames
Referrals_new<-filter(Referrals,OPENED>='2018-04-01')
Screenings_new<-filter(Screenings,OPENED>='2018-04-01')
Release_new<-filter(Release,OPENED>='2018-04-01')

#Calculate open cases by month by site by case type
calculate_month<- Analysis_Dataset %>% group_by(OPENED_MONTH, ATTORNEY, case_type_r) %>% summarize(count=n())

#Calculate open Referrals by week by site and subset just since November 2018
calculate_week<- Analysis_Dataset %>% filter(OPENED>="2018-11-01" & case_type_r=="Referral") %>% group_by(OPENED_WEEK, ATTORNEY) %>% summarize(count=n())


#Plot referrals per week with labels and panel by site
ggplot(Referrals_new, aes(x=OPENED_WEEK)) + geom_bar() + ggtitle("SIFI referrals per week by site") + labs(x = "Week", y = "Number of referral cases opened") + scale_x_date(date_breaks="1 week", labels = date_format("%m-%d-%y")) + facet_grid(ATTORNEY ~ .)


#Plot referrals per week and screen rate with labels and panel by site
ggplot(Referrals_new %>% filter(OPENED>="2018-12-01" & OPENED <="2019-03-24") %>% group_by(OPENED_WEEK, ATTORNEY) %>% count(OPENED_WEEK, SCREENED) %>%    
         mutate(pct=n/sum(n)),              
       aes(OPENED_WEEK, n, fill=SCREENED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("SIFI referrals per week and screen rate") + labs(x = "Week", y = "Number of referrals") + scale_x_date(date_breaks="1 week", labels = date_format("%m-%d-%y")) + facet_grid(ATTORNEY ~ .)


counts <- Referrals %>% filter(OPENED>"2018-11-01") %>% group_by(OPENED_WEEK, ATTORNEY) %>% summarize(count=n()) 
averages <- counts %>% group_by(ATTORNEY) %>% summarize(average_ref_ct=mean(count))

#For Stewart, plot open cases by month by case type
ggplot(Analysis_Dataset %>% filter(ATTORNEY=="STEWART") %>% group_by(OPENED_MONTH, ATTORNEY, case_type_r) %>% summarize(count=n()), aes(x=OPENED_MONTH, y=count)) + geom_bar() + ggtitle("Stewart open cases per month") + labs(x = "Month", y = "Number of open cases") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y")) + facet_grid(case_type_r ~ .)


#Plot screenings per month and acceptance rate
ggplot((Screenings %>% filter(ATTORNEY=="STEWART")),aes(OPENED_MONTH)) + geom_bar(aes(fill=ACCEPTED)) + ggtitle("Screenings per month and acceptance rate") + labs(x = "Month", y = "Number of screenings") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

#Plot referrals per month and screen rate
ggplot((Referrals %>% filter(OPENED>="2018-11-01")),aes(OPENED_MONTH)) + geom_bar(aes(fill=SCREENED)) + ggtitle("Referrals per month and screen rate") + labs(x = "Month", y = "Number of referrals") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

#Plot screenings per month and acceptance rate with labels
ggplot(Screenings %>% group_by(OPENED_MONTH) %>% count(OPENED_MONTH, ACCEPTED) %>%
         mutate(pct=n/sum(n)),             
       aes(OPENED_MONTH, n, fill=ACCEPTED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("Screenings per month and acceptance rate") + labs(x = "Month", y = "Number of screenings") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

#Plot referrals per month and screen rate with labels
ggplot(Referrals_new %>% group_by(OPENED_MONTH) %>% count(OPENED_MONTH, SCREENED) %>%    
         mutate(pct=n/sum(n)),             
       aes(OPENED_MONTH, n, fill=SCREENED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("Referrals per month and screen rate") + labs(x = "Month", y = "Number of referrals") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

#Plot referrals per month and screen rate with labels and panel by site
ggplot(Referrals_new %>% group_by(OPENED_MONTH, ATTORNEY) %>% count(OPENED_MONTH, SCREENED) %>%    
         mutate(pct=n/sum(n)),              
       aes(OPENED_MONTH, n, fill=SCREENED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("SIFI referrals per month and screen rate") + labs(x = "Month", y = "Number of referrals") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y")) + facet_grid(ATTORNEY ~ .)

#Plot screenings per month and acceptance rate with labels and panel by site
ggplot(Screenings_new %>% group_by(OPENED_MONTH, ATTORNEY) %>% count(OPENED_MONTH, ACCEPTED) %>%  
         mutate(pct=n/sum(n)),           
       aes(OPENED_MONTH, n, fill=ACCEPTED)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), position=position_stack(vjust=0.5)) + ggtitle("SIFI screenings per month and acceptance rate") + labs(x = "Month", y = "Number of screenings") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y")) + facet_grid(ATTORNEY ~ .)


#Plot referrals, screenings, and release cases per month with labels and panel by site
ggplot(Release_new, aes(x=OPENED_MONTH)) + geom_bar() + ggtitle("SIFI release cases per month by site") + labs(x = "Month", y = "Number of release cases") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y")) + facet_grid(ATTORNEY ~ .)

#Line chart
ggplot(Referrals_new %>% group_by(OPENED_MONTH, ATTORNEY) %>% summarize(count=n()), aes(x=OPENED_MONTH, y=count, color=ATTORNEY)) + geom_line() + geom_point()
ggplot(Screenings_new %>% group_by(OPENED_MONTH, ATTORNEY) %>% summarize(count=n()), aes(x=OPENED_MONTH, y=count, color=ATTORNEY)) + geom_line() + geom_point()
ggplot(Release_new %>% group_by(OPENED_MONTH, ATTORNEY) %>% summarize(count=n()), aes(x=OPENED_MONTH, y=count, color=ATTORNEY)) + geom_line() + geom_point()
ggplot(Analysis_Dataset %>% group_by(OPENED_MONTH, ATTORNEY, case_type_r) %>% summarize(count=n()), aes(x=OPENED_MONTH, y=count, color=case_type_r)) + geom_line() + geom_point() + facet_grid(ATTORNEY ~ .)

#Line chart with most recent months only
ggplot(Analysis_Dataset %>% filter(OPENED_MONTH>"2018-11-01") %>% group_by(OPENED_MONTH, ATTORNEY, case_type_r) %>% summarize(count=n()), aes(x=OPENED_MONTH, y=count, color=case_type_r)) + geom_line() + geom_point() + facet_grid(ATTORNEY ~ .)


#Plot stacked bar chart of release cases opened by site
ggplot(Release_new %>% group_by(OPENED_MONTH, ATTORNEY) %>% summarize(count=n()),              
       aes(OPENED_MONTH, y=count, fill=ATTORNEY)) + geom_bar(stat="identity") + ggtitle("SIFI release cases per month by site") + labs(x = "Month", y = "Number of release cases") + scale_x_date(date_breaks="1 month", labels = date_format("%m-%y"))

#Save the stats to a .csv on my computer
statsFileName <- "C:/Users/monica.whatley/Documents/MONICA WHATLEY/Weekly reporting/sitestats.csv"
write.csv(summarytable, file = statsFileName, na="")