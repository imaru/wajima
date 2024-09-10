library(jsonlite)
library(ndjson)
library(quantmod)

thre<-0.7
jsonlen<-200
fr<-30
ave<-fr*3

# tdir<-choose.dir()
tdir<-"C:\\Users\\imaru\\Documents\\pose\\res"
fl<-list.files(tdir, pattern='\\.json$')

nfl<-length(fl)
fdt<-data.frame()

for (i in 1:nfl){
  d<-ndjson::stream_in(paste(tdir,fl[i],sep='/'))
  dd<-d[[1]]
  for (j in 1:length(dd)){
    data<-fromJSON(dd[[j]])
    for (k in 1:length(data)){
      if (data[[k]]$confidence > thre){
        fdt[j+(i-1)*jsonlen,seq(1,17)]<-data[[k]]$keypoints$x
        fdt[j+(i-1)*jsonlen,seq(18,34)]<-data[[k]]$keypoints$y
      }
    }
  }
}

fdt[fdt==0]<-NA
adat<-rollmean(fdt,k=ave,na.pad=T)
adat<-cbind(adat,seq(1,nrow(adat)))
colnames(adat)<-c('NoseX','LeyeX','ReyeX','LearX','RearX','LshldX','RshldX','LelbX','RelbX','LwrstX','RwrstX','LhipX','RhipX','LkneeX','RkneeX','LanklX','RanklX','NoseY','LeyeY','ReyeY','LearY','RearY','LshldY','RshldY','LelbY','RelbY','LwrstY','RwrstY','LhipY','RhipY','LkneeY','RkneeY','LanklY','RanklY','frame')

library(tidyverse)
library(ggplot2)

ladat<-pivot_longer(data.frame(adat), cols=(c(RwrstX,RwrstY)), names_to = 'part', values_to = 'val')

gRwrst<-ggplot(data=ladat, aes(x=frame/30,y=val,colour = part))+geom_line()
plot(gRwrst)
