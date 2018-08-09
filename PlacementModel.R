library(gbm)
library(pROC) #visualizing ROC curves
library(dplyr)
now<-as.POSIXct(Sys.Date())
d<-read.csv("PolicyDataTest.csv")
d$marital[d$marital=='']='U'
#d$rptd_dt<-as.POSIXct(d$rptd_dt)
#Lets do some initial filtering as suggested by meeting
d<-filter(d,!(app_type %in% c('SRV','TMP','INC'))) #only doing whole life
#d<-filter(d,!(cov_code %in% c('CC1','SL100','SL20P')))

d$target=ifelse(d$status=='RPTD',1,0)
d$status<-NULL

#d$iss_dt<-as.POSIXct(d$iss_dt)
d$sum_entrydate<-as.POSIXct(d$sum_entrydate)

## Has the individual previously applied for insurance?  if so how many times (prevapp)? 
## What ratio of these policies report?  No data = 1/2 [rat_rptd] 
#d<-group_by(d,fn,ln,ssn) %>% arrange(sum_entrydate) %>% mutate(prevapp=row_number()-1) %>%  mutate(rat_rptd=ifelse(prevapp>0,(cumsum(target)-target)/prevapp,1/2)) %>% ungroup()

#For 2 policies: are the entry_dates close? Far?
ma <- d %>% group_by(fn,ln,ssn) %>% filter(n()==2) %>% arrange(sum_entrydate) %>% mutate(mult_apps=ifelse(difftime(sum_entrydate[2],sum_entrydate[1],units="weeks")>6,1,-1)) %>% ungroup()

d$mult_apps=ma$mult_apps[match(d$polnum,ma$polnum)]
d$mult_apps[is.na(d$mult_apps)]=0
d$mult_apps<-as.factor(d$mult_apps)
#rm(ma)
d$aps_strt<-as.factor(d$aps_strt)
d$num_riders[is.na(d$num_riders)]=0
d$total_rider_amt[is.na(d$total_rider_amt)]=0
d$amount_1035[is.na(d$amount_1035)]=0
d$num_ratings[is.na(d$num_ratings)]=0
#d$daysfroment<-as.numeric(difftime(now,d$sum_entrydate,units="days"))
#d$daysfromapp<-as.numeric(difftime(now,d$iss_dt,units="days"))
#d$daysfromapp[d$daysfromapp<0]=mean(d$daysfromapp[d$daysfromapp>0])
#d$daystoapp<-as.numeric(difftime(d$iss_dt,d$sum_entrydate,units='days'))
#d$daystoapp[d$daystoapp>10000]=mean(d$daystoapp[d$daystoapp<10000])

#Test out groupings of replacement_code.
d$intRepl<-ifelse(d$replacement_code %in% c('C','N','X'),1,0)
d$ExtRepl<-ifelse(d$replacement_code %in% c('C','F','M'),1,0)

d$replacement_code<-NULL
d$X<-NULL
d$polnum<-NULL
d$rptd_dt<-NULL
d$iss_dt<-NULL
d$sum_entrydate<-NULL
d$fn<-NULL
d$ln<-NULL
d$ssn<-NULL


set.seed(4321)
s<-sample(nrow(d),.7*nrow(d))
Train<-d[s,]
Test<-d[-s,]
rm(d)

model=gbm(target~., data=Train, n.trees=1000,interaction.depth=8)
preds<-predict(model,Test,n.trees=1000,type='response')

score=auc(Test$target,preds)
print(score)
