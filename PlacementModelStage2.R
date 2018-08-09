library(gbm)
library(pROC)
library(dplyr)
library(lubridate)

d<-read.csv("PolicyData.csv")
d$marital[d$marital=='']='U'
#Lets do some initial filtering as suggested by meeting

d$target=ifelse(d$status=='RPTD',1,0)
d$status<-NULL

d$iss_dt<-as.POSIXct(d$iss_dt)
d$sum_entrydate<-as.POSIXct(d$sum_entrydate)

#Look at all issued policies
d<-filter(d,year(d$iss_dt)<5555)
d$daystoapp<-as.numeric(difftime(d$iss_dt,d$sum_entrydate,units='days'))


d$aps_strt<-as.factor(d$aps_strt)
d$num_riders[is.na(d$num_riders)]=0
d$total_rider_amt[is.na(d$total_rider_amt)]=0
d$amount_1035[is.na(d$amount_1035)]=0
d$num_ratings[is.na(d$num_ratings)]=0

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
