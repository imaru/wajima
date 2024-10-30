library(jsonlite)
library(ndjson)
library(quantmod)
library(tidyverse)
library(ggplot2)

# R script for processing of pose and facial expression data

thre<-0.7
jsonlen<-200
fr<-30 # frame rate
period<-3 # period length for calculate variance
sums<-1

# tdir<-choose.dir()

# read class file
source('read_vocal_imaru.R')
imaru<-classdat
roll_imaru<-rollapplyr(imaru[,2:6], fr*period, max, na.rm=TRUE, by=fr*sums)

source('read_vocal_tanaka.R')
tanakay<-classdat

i_t <- cbind(imaru[,1], imaru[,2:6]*tanakay[,2:6])
roll_i_t<-rollapplyr(i_t[,2:6], fr*period, max, na.rm=TRUE, by=fr*sums)

# specify the open pose result file
facedir<-"C:\\Users\\imaru\\Dropbox\\Class\\2024\\2024wajima\\processed"
opr<-read.csv(paste(facedir,'wajima1_compressed.csv', sep='/'))

# for linux
# opr<-read.csv('../../wajima1_compressed.csv')

# specify a directory of json pose files
# tdir<-"C:\\Users\\imaru\\Dropbox\\Class\\2024\\2024wajima\\pose\\res1010"
# fl<-list.files(tdir, pattern='\\.json$')

# nfl<-length(fl)
# fdt<-data.frame()
# 
# read json files
# if more than one people are in the movie, select most confident one
# for (i in 1:nfl){
#   d<-ndjson::stream_in(paste(tdir,fl[i],sep='/'))
#   dd<-d[[1]]
#   for (j in 1:length(dd)){
#     data<-fromJSON(dd[[j]])
#     
#     #for (k in 1:length(data$confidence)){
#       if (length(data$confidence)>1){
#         maxp <- which(max(data$confidence)==data$confidence)
#         if (data$confidence[maxp] > thre){
#           fdt[j+(i-1)*jsonlen,seq(1,17)]<-t(data$keypoints[[maxp]][[1]])
#           fdt[j+(i-1)*jsonlen,seq(18,34)]<-t(data$keypoints[[maxp]][[2]])
#         }
#       }
#       else{
#         if (data$confidence > thre){
#           fdt[j+(i-1)*jsonlen,seq(1,17)]<-t(data$keypoints$x[[1]])
#           fdt[j+(i-1)*jsonlen,seq(18,34)]<-t(data$keypoints$y[[1]])
#         }
#       }
#     #}
#   }
# }

fdt<-readRDS('fdt1010.obj') # RDSファイルの使用
colnames(fdt)<-c('NoseX','LeyeX','ReyeX','LearX','RearX','LshldX','RshldX','LelbX','RelbX','LwrstX','RwrstX','LhipX','RhipX','LkneeX','RkneeX','LanklX','RanklX','NoseY','LeyeY','ReyeY','LearY','RearY','LshldY','RshldY','LelbY','RelbY','LwrstY','RwrstY','LhipY','RhipY','LkneeY','RkneeY','LanklY','RanklY')

fdt[fdt==0]<-NA
adat<-fdt
adat<-cbind(adat,seq(1,nrow(adat)))
colnames(adat)<-c('NoseX','LeyeX','ReyeX','LearX','RearX','LshldX','RshldX','LelbX','RelbX','LwrstX','RwrstX','LhipX','RhipX','LkneeX','RkneeX','LanklX','RanklX','NoseY','LeyeY','ReyeY','LearY','RearY','LshldY','RshldY','LelbY','RelbY','LwrstY','RwrstY','LhipY','RhipY','LkneeY','RkneeY','LanklY','RanklY','frame')

adat<-data.frame(adat)

# calculate variances 

# open face
HorizV<-rollapplyr(opr$gaze_angle_x, fr*period, sd, na.rm=TRUE, by=fr*sums)
VertV<-rollapplyr(opr$gaze_angle_y, fr*period, sd, na.rm=TRUE, by=fr*sums)
facePitchV<-rollapplyr(opr$pose_Rx, fr*period, sd, na.rm=TRUE, by=fr*sums)
faceYawV<-rollapplyr(opr$pose_Ry, fr*period, sd, na.rm=TRUE, by=fr*sums)
faceRollV<-rollapplyr(opr$pose_Rz, fr*period, sd, na.rm=TRUE, by=fr*sums)
AU45<-rollapplyr(opr$AU45_c, fr*period, max, na.rm=TRUE, by=fr*sums)
AU12<-rollapplyr(opr$AU12_r, fr*period, mean, na.rm=TRUE, by=fr*sums)

HorizM<-rollapplyr(opr$gaze_angle_x, fr*period, mean, na.rm=TRUE, by=fr*sums)
VertM<-rollapplyr(opr$gaze_angle_y, fr*period, mean, na.rm=TRUE, by=fr*sums)
facePitchM<-rollapplyr(opr$pose_Rx, fr*period, mean, na.rm=TRUE, by=fr*sums)
faceYawM<-rollapplyr(opr$pose_Ry, fr*period, mean, na.rm=TRUE, by=fr*sums)
faceRollM<-rollapplyr(opr$pose_Rz, fr*period, mean, na.rm=TRUE, by=fr*sums)

eyeX<-HorizV
eyeY<-VertV
facePitch<-facePitchV
faceYaw<-faceYawV
faceRoll<-faceRollV

# yolo
RwrstxV<-rollapplyr(adat$RwrstX, fr*period, sd, na.rm=TRUE, by=fr*sums)
RwrstxM<-rollapplyr(adat$RwrstX, fr*period, mean, na.rm=TRUE, by=fr*sums)
RwrstyV<-rollapplyr(adat$RwrstY, fr*period, sd, na.rm=TRUE, by=fr*sums)
RwrstyM<-rollapplyr(adat$RwrstY, fr*period, mean, na.rm=TRUE, by=fr*sums)
Rhand<-(RwrstxV+RwrstyV)/2
#Rhand<-(RwrstxV/RwrstxM+RwrstyV/RwrstyM)/2

LwrstxV<-rollapplyr(adat$LwrstX, fr*period, sd, na.rm=TRUE, by=fr*sums)
LwrstxM<-rollapplyr(adat$LwrstX, fr*period, mean, na.rm=TRUE, by=fr*sums)
LwrstyV<-rollapplyr(adat$LwrstY, fr*period, sd, na.rm=TRUE, by=fr*sums)
LwrstyM<-rollapplyr(adat$LwrstY, fr*period, mean, na.rm=TRUE, by=fr*sums)
Lhand<-(LwrstxV+LwrstyV)/2
#Lhand<-(LwrstxV/LwrstxM+LwrstyV/LwrstyM)/2


datlen<-min(nrow(roll_i_t),nrow(Rhand),nrow(faceRoll))
summary_takade <- data.frame(cbind(roll_i_t[1:datlen,], eyeX[1:datlen], eyeY[1:datlen], facePitch[1:datlen], faceYaw[1:datlen], faceRoll[1:datlen], Rhand[1:datlen], Lhand[1:datlen], AU45[1:datlen], AU12[1:datlen]))

colnames(summary_takade)<-c('non', 'pp', 'pn', 'fp', 'fn', 'eyeX', 'eyeY', 'facePitch', 'faceYaw', 'faceRoll', 'Rhand', 'Lhand','AU45','AU12')

s_tak <- summary_takade

s_tak$Rhand[which(is.na(s_tak$Rhand))]<-0
s_tak$Lhand[which(is.na(s_tak$Lhand))]<-0



library(ggheatmap)

ggheatmap::ggheatmap(summary_takade[,11:12])

library(brms)


pn_form<-bf(pn~eyeX+eyeY+facePitch+faceYaw+faceRoll+Rhand+Lhand+AU45+AU12)
pp_form<-bf(pp~eyeX+eyeY+facePitch+faceYaw+faceRoll+Rhand+Lhand+AU45+AU12)
fn_form<-bf(fn~eyeX+eyeY+facePitch+faceYaw+faceRoll+Rhand+Lhand+AU45+AU12)
fp_form<-bf(fp~eyeX+eyeY+facePitch+faceYaw+faceRoll+Rhand+Lhand+AU45+AU12)

# brms

lm_pn <- brm(
  formula=pn_form,
  family = bernoulli(),
  data=s_tak,
  prior = c(set_prior("",class='Intercept'))  
)

lm_pn

lm_pp <- brm(
  formula=pp_form,
  family = bernoulli(),
  data=s_tak,
  prior = c(set_prior("",class='Intercept'))  
)

lm_pp

lm_fn <- brm(
  formula=fn_form,
  family = bernoulli(),
  data=s_tak,
  prior = c(set_prior("",class='Intercept'))  
)

lm_fn

lm_fp <- brm(
  formula=fp_form,
  family = bernoulli(),
  data=s_tak,
  prior = c(set_prior("",class='Intercept'))  
)

lm_fp


library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# state space model 1 / 4 explanatory variables

dat1<-list(N=length(s_tak$pn), cat = s_tak$pn, eyex = s_tak$eyeX, eyey = s_tak$eyeY, AU45 = s_tak$AU45, AU12 = s_tak$AU12)
model1<-stan_model(file='ssm1.stan', model_name='ssm1')
fit1<-sampling(model1, data=dat1, iter=4000, warmup=2000, thin=4, chain=4)
res1<-rstan::extract(fit1)
print(fit1, pars=c('Intercept','b1','b2','b3','b4'), probs=c(0.025,0.5,0.975))

# state space model 2 / all explanatory variables

dat2<-list(N=length(s_tak$pn), cat = s_tak$pn, eyex = s_tak$eyeX, eyey = s_tak$eyeY, faceP = s_tak$facePitch, faceY = s_tak$faceYaw, faceR = s_tak$faceRoll, rH = s_tak$Rhand, lH = s_tak$Lhand, AU45 = s_tak$AU45, AU12 = s_tak$AU12)
model2<-stan_model(file='ssm2.stan', model_name='ssm2')
fit2<-sampling(model2, data=dat2, iter=4000, warmup=2000, thin=4, chain=4)
res2<-rstan::extract(fit2)
print(fit2, pars=c('b_x','b_y','b_P','b_Y', 'b_R', 'b_r', 'b_l', 'b_45', 'b_12'), probs=c(0.025,0.5,0.975))

library(cmdstanr)

# state space model 3 / all explanatory variables / all frames

dat2<-list(N=length(s_tak$pn), cat = s_tak$pn, eyex = s_tak$eyeX, eyey = s_tak$eyeY, faceP = s_tak$facePitch, faceY = s_tak$faceYaw, faceR = s_tak$faceRoll, rH = s_tak$Rhand, lH = s_tak$Lhand, AU45 = s_tak$AU45, AU12 = s_tak$AU12)

# cmdstanr

res2cmd<-cmdstan_model('ssm2.stan')
fit2cmd<-res2cmd$sample(
  data = dat2,
  chains = 4,
  refresh = 100,
  iter_warmup = 1000,
  iter_sampling = 2000,
  parallel_chains = 4
)
fit2cmd$print(c('b_x','b_y','b_P','b_Y', 'b_R', 'b_r', 'b_l', 'b_45', 'b_12'))


dat4<-list(N=length(s_tak$pn), pn = s_tak$pn, pp = s_tak$pp, fn = s_tak$fn, fp = s_tak$fp, eyex = s_tak$eyeX, eyey = s_tak$eyeY, faceP = s_tak$facePitch, faceY = s_tak$faceYaw, faceR = s_tak$faceRoll, rH = s_tak$Rhand, lH = s_tak$Lhand, AU45 = s_tak$AU45, AU12 = s_tak$AU12)

res4cmd<-cmdstan_model('ssm4.stan')
fit4cmd<-res4cmd$sample(
  data = dat4,
  chains = 4,
  refresh = 100,
  iter_warmup = 1000,
  iter_sampling = 2000,
  parallel_chains = 4
)
fit4cmd$print(c('k1','k2','k3','k4'), probs=c(0.025,0.5,0.975))


dat5<-list(N=length(s_tak$pn), cat = s_tak$pn, eyex = s_tak$eyeX, eyey = s_tak$eyeY, faceP = s_tak$facePitch, faceY = s_tak$faceYaw, faceR = s_tak$faceRoll, rH = s_tak$Rhand, lH = s_tak$Lhand, AU45 = s_tak$AU45, AU12 = s_tak$AU12)

# cmdstanr

res5cmd<-cmdstan_model('ssm5.stan')
fit5cmd<-res5cmd$sample(
  data = dat5,
  chains = 4,
  refresh = 100,
  iter_warmup = 2000,
  iter_sampling = 2000,
  parallel_chains = 12
)
fit5cmd$print(c('b_x','b_y','b_P','b_Y', 'b_R', 'b_r', 'b_l', 'b_45', 'b_12'))

# descripription



