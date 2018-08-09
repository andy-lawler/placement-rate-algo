library(lubridate)
library(dplyr)

rw <- read.csv("TrainingData.csv")
rw$rptd_dt<-as.POSIXct(rw$rptd_dt)
rw$entry_dt<-NULL
rw$sum_entrydate<-as.POSIXct(rw$sum_entrydate)
rw$sum_dateaction<-as.POSIXct(rw$sum_dateaction)

number_current_apps<-function(entry,final){
  ct<-rep(0,length(entry))
  for (j in 1:(-1+length(entry))){
    for(i in (j+1):length(entry)){
      if(final[j]>entry[i]){
        ct[c(i,j)]=ct[c(i,j)]+1}
    }
  }
ct  
}

number_current_policies<-function(entry,final){
  ct<-rep(0,length(entry))
  for (j in 1:length(entry)){
     ct[j]<-sum(entry[j]>final)
  }
  ct  
}



#step 1: filter out all individuals with only a single policy
#potential future step: look at how many policies an individual has in force.  
#note: add filter by app type
ind <- rw  %>% group_by(fn,ln,ssn)  %>% filter(n()>1) %>% 
           mutate(num_prev_apps = rank(sum_entrydate,ties.method="min")-1 ) %>% 
           arrange(sum_entrydate) %>%  mutate(num_cur_apps = number_current_apps(sum_entrydate,pmin(rptd_dt,sum_dateaction))) %>%
           mutate(prev_surv=cumsum(app_type=='SRV')) %>%
           mutate(cur_pol=number_current_policies(sum_entrydate,rptd_dt))
save(ind,file='PR_Ind.Rda')

           
agt <- rw  %>% group_by(sum_agentcode) %>%  arrange(sum_entrydate) %>%
              mutate(agt_length_of_service=difftime(sum_entrydate,min(sum_entrydate),units="weeks")) 
            #  mutate(agt_success_rate=ifelse(row_number()>100,cumsum(status=='RPTD')/row_number(),0))
save(agt,file='PR_agt.Rda')

agcy <- rw  %>% group_by(sum_agency_code) %>%  arrange(sum_entrydate) %>%
  mutate(agcy_length_of_service=difftime(sum_entrydate,min(sum_entrydate),units="weeks"))
  #mutate(agcy_succ_rate=ifelse(n()>100,sum(status=='RPTD')/n(),0))
save(agcy,file='PR_agcy.Rda')