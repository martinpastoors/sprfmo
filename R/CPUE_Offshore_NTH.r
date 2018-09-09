library(lme4)
library(MASS)
library(rmarkdown)     # rmarkdown functionality
library(pander)        # tables
library(lubridate)     # data handling
library(reshape2)      # reshaping data; e.g. cast
library(readxl)        # excel reader
library(broom)         # clean up statistics
library(scales)        # pretty scales
library(stringr)       # string manipulations
library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(mgcv)          # tensor spline for gam


#- Set paths
dataPath          <- "D:/Repository/SPRFMO/jjm/data/"
codePath          <- "D:/Repository/SPRFMO/jjm/assessment/"
outPath           <- "D:/Repository/SPRFMO/jjm/assessment/"

#- Load the offshore fleet data
load(file.path(dataPath,"offshore_data2.RData")) #data with corrected 1.4x factor

#- Filter for complete cases in data
cjm_byweek  <- cjm_byweek[complete.cases(cjm_byweek[,c("vesselcp","vesselcode2","year","month","catch","shootlat","shootlon","ssf","ELE")]),]
cjm_byday   <- cjm_byday[complete.cases(cjm_byday[,c("vesselcp","vesselcode2","year","month","day","catch","shootlat","shootlon","ssf","ELE")]),]

cjm_byweek$catch  <- round(cjm_byweek$catch)
cjm_byweek        <- subset(cjm_byweek,catch>0) #if they didn't catch anything for a whole week, this is suspicious and therefore I delete it

#- Plot the catch data distribution as a histogram and fit a negative binomial through it. Data is clearly overdispersed!
fit.params  <- fitdistr(round(cjm_byweek$catch), "Negative Binomial")
res <- hist(cjm_byweek$catch,breaks=100)
plot(y=res$density,x=res$mids,type="h",xlab="Catch by week",ylab="Density")
lines(dnbinom(0:3200,size=fit.params$estimate["size"],mu=fit.params$estimate["mu"]),x=0:3200,col=2)

fit.params  <- fitdistr(round(cjm_byday$catch), "Negative Binomial")
res <- hist(cjm_byday$catch,breaks=100)
plot(y=res$density,x=res$mids,type="h",xlab="Catch by week",ylab="Density")
lines(dnbinom(0:3200,size=fit.params$estimate["size"],mu=fit.params$estimate["mu"]),x=0:3200,col=2)

#----------------------------
#- Start fitting linear terms
#----------------------------

#- We want at least a year-effect, so include that already
store <- list()
for(iVar in c("month","ELE","lonlat","vesselcp","vesselcode2","ssf")){
  print(iVar)
  form       <- formula(paste("catch ~ year + offset(log(effort)) +",iVar))
  if(iVar == "lonlat")
    form     <- formula(paste("catch ~ year + offset(log(effort)) + shootlon*shootlat"))
  print(form)
  store[[iVar]] <- glm.nb(form,data=cjm_byweek)
}
par(mfrow=c(1,1),oma=c(4,0,0,0)); plot(y=do.call(rbind,lapply(store,AIC))[,1],x=1:length(store),xlab="",ylab="AIC",xaxt="n"); axis(1,at=1:length(store),labels=names(store),las=2)
par(mfrow=c(2,2)); gam.check(store[["vesselcode2"]])
anova(store[["vesselcode2"]])

#- year and  vesselcode2 plus additional variables
store <- list()
for(iVar in c("month","ELE","lonlat","vesselcp","ssf")){
  print(iVar)
  form       <- formula(paste("catch ~ year + vesselcode2 + offset(log(effort)) +",iVar))
  if(iVar == "lonlat")
    form     <- formula(paste("catch ~ year + vesselcode2 + offset(log(effort)) + shootlon*shootlat"))
  store[[iVar]] <- glm.nb(form,data=cjm_byweek)
}
par(mfrow=c(1,1),oma=c(4,0,0,0)); plot(y=do.call(rbind,lapply(store,AIC))[,1],x=1:length(store),xlab="",ylab="AIC",xaxt="n"); axis(1,at=1:length(store),labels=names(store),las=2)
par(mfrow=c(2,2)); gam.check(store[["month"]])
anova(store[["month"]])

#- Include month
store <- list()
for(iVar in c("ELE","lonlat","vesselcp","ssf")){
  print(iVar)
  form       <- formula(paste("catch ~ year + month + vesselcode2 + offset(log(effort)) +",iVar))
  if(iVar == "lonlat")
    form     <- formula(paste("catch ~ year + month + vesselcode2 + offset(log(effort)) + shootlon*shootlat"))
  store[[iVar]] <- glm.nb(form,data=cjm_byweek)
}
par(mfrow=c(1,1),oma=c(4,0,0,0)); plot(y=do.call(rbind,lapply(store,AIC))[,1],x=1:length(store),xlab="",ylab="AIC",xaxt="n"); axis(1,at=1:length(store),labels=names(store),las=2)
par(mfrow=c(2,2)); gam.check(store[["lonlat"]])
anova(store[["lonlat"]])

#- Include lonlat
store <- list()
for(iVar in c("ELE","vesselcp","ssf")){
  print(iVar)
  form       <- formula(paste("catch ~ year + month + vesselcode2 + shootlon * shootlat + offset(log(effort)) +",iVar))
  store[[iVar]] <- glm.nb(form,data=cjm_byweek)
}
par(mfrow=c(1,1),oma=c(4,0,0,0)); plot(y=do.call(rbind,lapply(store,AIC))[,1],x=1:length(store),xlab="",ylab="AIC",xaxt="n"); axis(1,at=1:length(store),labels=names(store),las=2)
par(mfrow=c(2,2)); gam.check(store[["ELE"]])
anova(store[["ELE"]])

#- Include ELE
store <- list()
for(iVar in c("vesselcp","ssf")){
  print(iVar)
  form       <- formula(paste("catch ~ year + month + vesselcode2 + shootlon * shootlat + ELE + offset(log(effort)) +",iVar))
  store[[iVar]] <- glm.nb(form,data=cjm_byweek)
}
par(mfrow=c(1,1),oma=c(4,0,0,0)); plot(y=do.call(rbind,lapply(store,AIC))[,1],x=1:length(store),xlab="",ylab="AIC",xaxt="n"); axis(1,at=1:length(store),labels=names(store),las=2)
par(mfrow=c(2,2)); gam.check(store[["vesselcp"]])
anova(store[["ssf"]]) # NOT SIGNIFICANT

#- Final linear model
formbase <- formula(paste("catch ~ year + month + vesselcode2 + shootlon * shootlat + ELE + offset(log(effort)) +",iVar))

#----------------------------
#- Start adding smoothers
#----------------------------
formbase <- formula(paste("catch ~ year + month + vesselcode2 +   shootlon*shootlat           + ELE + offset(log(effort))"))
formbasey<- formula(paste("catch ~ year + month + vesselcode2 +   shootlon*shootlat*year      + ELE + offset(log(effort))"))
forms    <- formula(paste("catch ~ year + month + vesselcode2 + s(shootlon,shootlat)          + ELE + offset(log(effort))"))
formsy   <- formula(paste("catch ~ year + month + vesselcode2 + s(shootlon,shootlat,by=year)  + ELE + offset(log(effort))"))

store    <- list()
store[["formbase"]] <- glm.nb(formbase, data=cjm_byweek)
store[["formbasey"]]<- glm.nb(formbasey,data=cjm_byweek)
store[["forms"]]    <- gam(   forms,    data=cjm_byweek,family=negbin(glm.nb(formbase,data=cjm_byweek)$theta))
store[["formsy"]]   <- gam(   forms,    data=cjm_byweek,family=negbin(glm.nb(formbasey,data=cjm_byweek)$theta))
par(mfrow=c(1,1),oma=c(4,0,0,0)); plot(y=do.call(rbind,lapply(store,AIC))[,1],x=1:length(store),xlab="",ylab="AIC",xaxt="n"); axis(1,at=1:length(store),labels=names(store),las=2)
x11(); par(mfrow=c(2,2)); gam.check(store[["formbase"]])
x11(); par(mfrow=c(2,2)); gam.check(store[["formbasey"]])
x11(); par(mfrow=c(2,2)); gam.check(store[["forms"]])
x11(); par(mfrow=c(2,2)); gam.check(store[["formsy"]])

#----------------------------
#- Put vesselcode2 as RE
#----------------------------
formbase   <- formula(paste("catch ~ year + month + shootlon*shootlat + ELE +    vesselcode2  + offset(log(effort))"))
formbaser  <- formula(paste("catch ~ year + month + shootlon*shootlat + ELE + (1|vesselcode2) + offset(log(effort))"))

store      <- list()
store[["glm.nb"]]   <- glm.nb(formbase,data=cjm_byweek)
store[["formbase"]] <- glm(  formbase, data=cjm_byweek,family=negative.binomial(store[["glm.nb"]]$theta))
store[["formbaser"]]<- glmer(formbaser,data=cjm_byweek,family=negative.binomial(store[["glm.nb"]]$theta))
par(mfrow=c(1,1),oma=c(4,0,0,0)); plot(y=do.call(rbind,lapply(store,AIC))[,1],x=1:length(store),xlab="",ylab="AIC",xaxt="n"); axis(1,at=1:length(store),labels=names(store),las=2)
x11(); par(mfrow=c(2,2)); gam.check(store[["formbase"]])
x11(); par(mfrow=c(2,2)); gam.check(store[["formbaser"]])

#----------------------------
#- Predict from formbase
#----------------------------
final     <- formula(paste("catch ~ year + month + s(shootlon,shootlat) + ELE +    
                                    vesselcode2  + offset(log(effort))"))
newdat    <- expand.grid(year=as.factor(2006:2016),
                         month=as.factor(3),
                         shootlon=quantile(cjm_byweek$shootlon,probs=c(0.5)),
                         shootlat=quantile(cjm_byweek$shootlat,probs=c(0.5)),
                         ELE=as.factor(0),
                         vesselcode2=names(rev(sort(colSums(table(cjm_byweek$year,cjm_byweek$vesselcode2)))))[1],
                         effort=7)
finalMod  <- gam(final,data=cjm_byweek,family=negbin(glm.nb(formbase,data=cjm_byweek)$theta))
plot(finalMod, all.terms=T, page=1)
pred      <- predict(finalMod,newdat,se.fit=T,type="link")
upr       <- exp(pred$fit + (1.96 * pred$se.fit))
lwr       <- exp(pred$fit - (1.96 * pred$se.fit))


par(oma=c(0,0,0,2))
plot(y=exp(pred$fit),x=2006:2016,xlab="Years",ylab="Standardized",type="l",ylim=c(0,4000))
lines(y=upr,x=2006:2016,lty=3)
lines(y=lwr,x=2006:2016,lty=3)
par(new=T)
plot(y=aggregate(cjm_byweek$catch/cjm_byweek$effort,by=list(cjm_byweek$year),median)$x,x=2006:2016,col=2,lty=2,xlab="",ylab="",xaxt="n",yaxt="n",type="l")
axis(4,col=2)
mtext(4,at=0.5,outer=T,text="Nominal")
legend("topleft",legend=c("Standardized","Nominal"),col=1:2,lty=1:2)
