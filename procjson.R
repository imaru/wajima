library(jsonlite)
library(ndjson)
library(quantmod)

thre<-0.7
jsonlen<-200
fr<-30
ave<-fr*3

# tdir<-choose.dir()
tdir<-"C:\\Users\\imaru\\Dropbox\\Class\\2024\\2024wajima\\pose\\res"
fl<-list.files(tdir, pattern='\\.json$')

nfl<-length(fl)
fdt<-data.frame()

# for (i in 1:nfl){
#   d<-ndjson::stream_in(paste(tdir,fl[i],sep='/'))
#   dd<-d[[1]]
#   for (j in 1:length(dd)){
#     data<-fromJSON(dd[[j]])
#     for (k in 1:length(data)){
#       if (data[[k]]$confidence > thre){
#         fdt[j+(i-1)*jsonlen,seq(1,17)]<-data[[k]]$keypoints$x
#         fdt[j+(i-1)*jsonlen,seq(18,34)]<-data[[k]]$keypoints$y
#       }
#     }
#   }
# }

for (i in 1:nfl){
  d<-ndjson::stream_in(paste(tdir,fl[i],sep='/'))
  dd<-d[[1]]
  for (j in 1:length(dd)){
    data<-fromJSON(dd[[j]])
    
    #for (k in 1:length(data$confidence)){
      if (length(data$confidence)>1){
        maxp <- which(max(data$confidence)==data$confidence)
        if (data$confidence[maxp] > thre){
          fdt[j+(i-1)*jsonlen,seq(1,17)]<-t(data$keypoints[[maxp]][[1]])
          fdt[j+(i-1)*jsonlen,seq(18,34)]<-t(data$keypoints[[maxp]][[2]])
        }
      }
      else{
        if (data$confidence > thre){
          fdt[j+(i-1)*jsonlen,seq(1,17)]<-t(data$keypoints$x[[1]])
          fdt[j+(i-1)*jsonlen,seq(18,34)]<-t(data$keypoints$y[[1]])
        }
      }
    #}
  }
}

fdt[fdt==0]<-NA
adat<-rollmean(fdt,k=ave,na.pad=T)
adat<-cbind(adat,seq(1,nrow(adat)))
colnames(adat)<-c('NoseX','LeyeX','ReyeX','LearX','RearX','LshldX','RshldX','LelbX','RelbX','LwrstX','RwrstX','LhipX','RhipX','LkneeX','RkneeX','LanklX','RanklX','NoseY','LeyeY','ReyeY','LearY','RearY','LshldY','RshldY','LelbY','RelbY','LwrstY','RwrstY','LhipY','RhipY','LkneeY','RkneeY','LanklY','RanklY','frame')

h_adat <- adat[seq(1,30*120),]

handpos<-data.frame()
rhandmv<-sqrt((median(adat$RwrstX,na.rm=T)-adat$RwrstX)^2+(median(adat$RwrstY,na.rm=T)-adat$RwrstY)^2)
lhandmv<-sqrt((median(adat$LwrstX,na.rm=T)-adat$LwrstX)^2+(median(adat$LwrstY,na.rm=T)-adat$LwrstY)^2)

handmv<-cbind(rhandmv,lhandmv)
handmv<-cbind(handmv, seq(1,nrow(handmv)))
handmv<-data.frame(handmv)
colnames(handmv)<-c('Right','Left', 'Frame')

p_handmv<-handmv[1:30*130,]

library(tidyverse)
library(ggplot2)

ladat<-pivot_longer(data.frame(h_adat), cols=(c(RwrstX,RwrstY,LwrstX,LwrstY)), names_to = 'part', values_to = 'val')
lhand<-pivot_longer(handmv, cols=c(Right,Left), names_to='LR', values_to='diff')
p_lhand<-pivot_longer(p_handmv, cols=c(Right,Left), names_to='LR', values_to='diff')

ghand<-ggplot(data=lhand, aes(x=Frame/29, y=diff, color=LR))+geom_line()
p_ghand<-ggplot(data=p_lhand, aes(x=Frame/29, y=diff, color=LR))+geom_line(linewidth=3)+theme(text=element_text(size=24))

gRwrst<-ggplot(data=ladat, aes(x=frame/29,y=val,colour = part))+geom_line()

plot(p_ghand)
