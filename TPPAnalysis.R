library(lubridate)
library(dplyr)

tp<-read.csv("TppData.csv")
tp$SSN<-as.numeric(   gsub("[[:punct:]]", "",  as.character(tp$SSN))    )
tp$case_app_recd_dt<-as.POSIXct(tp$case_app_recd_dt)
tp$lst_casest_upd_dtm<-as.POSIXct(tp$lst_casest_upd_dtm)

rw <- read.csv("TrainingData.csv")
rw$rptd_dt<-as.POSIXct(rw$rptd_dt)
rw$entry_dt<-NULL
rw$sum_entrydate<-as.POSIXct(rw$sum_entrydate)
rw$sum_dateaction<-as.POSIXct(rw$sum_dateaction)


#Step 1: Examine Reported Policies
ot<- tp %>% filter(case_stat_cd=='Reported') %>% group_by(SSN) %>% arrange(lst_casest_upd_dtm)  %>% filter(row_number()==1)

rw$term_lst_upd<-ot$lst_casest_upd_dtm[match(rw$ssn,ot$SSN)]
rw$has_term<-ifelse(!is.na(rw$term_lst_upd) & rw$term_lst_upd<rw$sum_entrydate,1,0)

#Step 2: Examine Policies that did not report
ot<- tp %>% filter(case_stat_cd %in% c('Withdraw','Incomple','NotTaken','Declined')) %>% group_by(SSN) %>% arrange(lst_casest_upd_dtm) %>% filter(row_number()==1)

rw$term_lst_upd<-ot$lst_casest_upd_dtm[match(rw$ssn,ot$SSN)]
rw$prev_term_failed<-ifelse(!is.na(rw$term_lst_upd) & rw$term_lst_upd<rw$sum_entrydate,1,0)

#Step 3: Look for applications that occur at same time.  Here we are ignoring the possibility of doubles.  could modify later

rw$term_lst_upd<-tp$lst_casest_upd_dtm[match(rw$ssn,tp$SSN)]
rw$term_rec_dt<-tp$case_app_recd_dt[match(rw$ssn,tp$SSN)]
rw$has_app_for_term<-ifelse(!is.na(rw$term_rec_dt) & rw$sum_entrydate < rw$term_lst_upd  & rw$term_rec_dt<rw$sum_dateaction,1,0)

rw<-rw[,names(rw) %in% c('polnum','has_term','prev_term_failed','has_app_for_term')]
save(file="TermHistory.Rda",rw)
