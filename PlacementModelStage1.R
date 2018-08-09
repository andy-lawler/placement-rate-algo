library(gbm)
library(pROC)
library(dplyr)
d<-read.csv("PolicyDataFromJan2014.csv")
d$marital[d$marital=='']='U'

d$target=ifelse(d$status=='RPTD',1,0)
d$sum_entrydate<-as.POSIXct(d$sum_entrydate)
d$aps_strt<-as.factor(d$aps_strt)
d$num_riders[is.na(d$num_riders)]=0
d$total_rider_amt[is.na(d$total_rider_amt)]=0
d$amount_1035[is.na(d$amount_1035)]=0
d$num_ratings[is.na(d$num_ratings)]=0
d$DBR[is.na(d$DBR)]=0
d$IPR[is.na(d$IPR)]=0
d$WMC[is.na(d$WMC)]=0
d$LISR[is.na(d$LISR)]=0
d$RTR1[is.na(d$RTR1)]=0
d$LTC[is.na(d$LTC)]=0
d$GIR[is.na(d$GIR)]=0
d$WP[is.na(d$WP)]=0
d$ALIR[is.na(d$ALIR)]=0
d$num_xclusions[is.na(d$num_xclusions)]=0

#Add features from existing model
d$ann_prem_cap<-ifelse(d$annual_prem>250000,250000,d$annual_prem)
d$wprem<-d$annual_prem + 0.1*(d$amount_1035+d$exp_premium)
  
#Group replacement_code.
d$intRepl<-ifelse(d$replacement_code %in% c('C','N','X'),1,0)
d$ExtRepl<-ifelse(d$replacement_code %in% c('C','F','M'),1,0)

###Load EZApp Data
ez<-read.csv("FullEZAppData.csv")
ez[,3:6]<-sapply(ez[,3:6],gsub,pattern="[[:punct:]]",replacement="")
ez[,3:6]<-sapply(ez[,3:6],as.numeric)
ez$Income=rowSums(cbind(ez[,"estSalary"],ez[,"estIncome"],ez[,"estGrossAnnualOtherIncome"]),na.rm=T)
ez$PolicyNumber<-as.integer(as.character(ez$PolicyNumber))

d$Income<-ez$Income[match(d$polnum,ez$PolicyNumber)]
d$netWorth<-ez$estNetworth[match(d$polnum,ez$PolicyNumber)]
rm(ez)

#impute missing Income, netWorth.  Means are quite large.  Using median instead
d$Income[is.na(d$Income)]=median(d$Income,na.rm=T)
d$netWorth[is.na(d$netWorth)]=median(d$netWorth,na.rm=T)

#Look at ratio of  of face value to income/networth.
d$can_afford<-ifelse(d$annual_prem/d$Income > d$annual_prem/d$netWorth, d$annual_prem/d$Income ,d$annual_prem/d$netWorth)

#merge agcy data
load(file="PR_agcy.Rda")
d$agcy_length_of_service<-as.numeric(agcy$agcy_length_of_service[match(d$polnum,agcy$polnum)])
rm(agcy)

#merge agt data
load(file="PR_agt.Rda")
d$agt_length_of_service<-as.numeric(agt$agt_length_of_service[match(d$polnum,agt$polnum)])
rm(agt)

#merge prev app/pol data
load(file="PR_Ind.Rda")
d$num_prev_apps<-ind$num_prev_apps[match(d$polnum,ind$polnum)]
d$num_prev_apps[is.na(d$num_prev_apps)]=0
d$num_cur_apps<-ind$num_cur_apps[match(d$polnum,ind$polnum)]
d$num_cur_apps[is.na(d$num_cur_apps)]=0
d$prev_surv<-ind$prev_surv[match(d$polnum,ind$polnum)]
d$prev_surv[is.na(d$prev_surv)]=0
d$cur_pol<-ind$cur_pol[match(d$polnum,ind$polnum)]
d$cur_pol[is.na(d$cur_pol)]=0
rm(ind)

#add term data
load(file='TermHistory.Rda')
d$has_term <- rw$has_term[match(d$polnum,rw$polnum)]
d$has_term[is.na(d$has_term)]=0
d$prev_term_failed<- rw$prev_term_failed[match(d$polnum,rw$polnum)]
d$prev_term_failed[is.na(d$prev_term_failed)]=0
d$has_app_for_term <- rw$has_app_for_term[match(d$polnum,rw$polnum)]
rm(rw)

notfts<-c("replacement_code","X","rptd_dt","iss_dt",'sum_entrydate','fn','ln','ssn','status','polnum','final_date')
model=gbm(target~., data=d[,!(names(d) %in% notfts)], n.trees=12000,interaction.depth=15,cv.folds=5,verbose=TRUE)
save(file="Full_GBM_With_EZApp.Rda",model)
