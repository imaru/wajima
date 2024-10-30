# class 1-4
# 1: positive, past / pp
# 2: negative, past / pn
# 3: positive, future / fp
# 4: negative, future / fn

library(lubridate)

fr<-30
nframes<-63360

# fl<-choose.files()
fl<-'notta202410_classification_watanabe.csv'
dat<-read.csv(fl)
dat$frames<-as.integer(seconds(hms(dat$time)))*fr
dat$duration<-c(diff(dat$frames), nframes-tail(dat$frames,1))

classdat<-data.frame(matrix(0, nrow=nframes, ncol=6))
colnames(classdat)<-c('frame','non', 'pp','pn','fp','fn')
classdat$frame<-seq(1,nframes)

targetprd<-which(dat$speaker=='INFORMANT')

for (i in 1:length(targetprd)){
  classdat[seq(dat$frames[i], dat$frames[i]+dat$duration[i]-1),dat$class[targetprd[i]]+2]<-1
}
