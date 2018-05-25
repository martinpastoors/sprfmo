# ==================================================================
# EU CPUE analysis.r
#
# 17/09/2017: first version, adapted from read_kenmerken
# 21/09/2017: expanded to include GAM models for CPUE
# 08/05/2018: new reader in separate code; keeps the datetime objects (at UTC time)
#             reserve code; will be embedded in the rmarkdown version
# ==================================================================

# Reset lists
rm(list=ls())

# general libraries
library(readxl)        # excel reader from hadley; much quicker than the java based excel readers
library(lubridate)     # data and time functions
library(stringr)       # string manipulation
library(pander)        # for tables
library(reshape2)      # cast and melt dataframes
library(broom)         # clean up statistics
library(scales)        # pretty scales
library(tidyverse)     # data manipulation and piping
# library(gam)           # gam analysis
library(mgcv)          # tensor spline for gam

options(max.print=9999999)

# setwd("D:/SPRFMO/2017")

# set onedrive directory
onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 

# load spatial data
load(file.path(onedrive,"rdata/fao.RData"))
load(file.path(onedrive,"rdata/eez.RData"))
load(file.path(onedrive,"rdata/icesrectangles.RData"))

load(file.path(onedrive,"rdata/world.df.RData"))
load(file.path(onedrive,"rdata/fao.df.RData"))
load(file.path(onedrive,"rdata/eez.df.RData"))

# source utilities
source("../../prf/r/my utils.R")
source("../../gisland/r/geo_inside.R")

# ================================================================================
# Reading in additional files
# ================================================================================

vessels <-
  read_excel(path="D:/XXX/PFA/PFA vessel database.xlsx", col_names = TRUE) %>%
  lowcase() %>%
  rename(vesselname=vessel) %>% 
  filter(year == 2015) %>% 
  mutate_at(c("vesselname"), funs(tolower)) %>% 
  select(vesselname, enginemain, gt, loa, constructionyear)


ad <- 
  read_excel(path="D:/SPRFMO/2017/data/CPUE 2007-2016.xlsx", range=("A5:D14"), col_names = FALSE) %>% 
  setNames(., c("year","catch","days","cpue")) %>% 
  ungroup() %>% 
  mutate(rel_cpue = (cpue/ mean(cpue, na.rm=TRUE)),
         model    = "cpue",
         desc     = "raw CPUE") %>% 
  data.frame()

elnino <- 
  read_excel(path="D:/SPRFMO/2017/data/elnino.xlsx", col_names = TRUE) %>% 
  lowcase() %>% 
  gather(key=month, value=ssf, m12:m11) %>% 
  mutate(month = as.numeric(gsub("m","", month))) %>% 
  mutate(ELE   = 0, 
         ELE   = ifelse(ssf <= -0.5, -1, ELE),
         ELE   = ifelse(ssf >= 0.5 ,  1, ELE)) %>% 
  arrange(year, month)



# -----------------------------------------------------------------------------------
# make cjm dataset. 
# -----------------------------------------------------------------------------------

cjm  <- 
  rbind.all.columns(oldlists, newlists) %>% 
  mutate(
    species       = toupper(species),
    vesselcode    = gsub("\\s+|-", "", tolower(vesselcode)),
    vesselname    = tolower(vesselname)
  ) %>% 
  data.frame() %>% 
  
  filter(species == "CJM") %>% 
  
  # add the correction factor
  left_join(factor, by=c("year")) %>% 
  mutate(catch2 = catch * f) %>% 
  
  # aggregate all catches as catches per day; average the positions for each day. 
  group_by(vesselcountry, vesselname, vesselcallsign, vesselcode, vesselimo, 
           filename, worksheet, shootdate, year, month, day) %>% 
  summarise(catch       = sum(catch, na.rm=TRUE),
            catch2      = sum(catch2, na.rm=TRUE), 
            duration    = sum(duration, na.rm=TRUE),
            shootlon    = mean(shootlon, na.rm=TRUE), 
            shootlat    = mean(shootlat, na.rm=TRUE),
            haullon     = mean(haullon, na.rm=TRUE),
            haullat     = mean(haullat, na.rm=TRUE),
            temperature = mean(temperature, na.rm=TRUE), 
            geardepth   = mean(geardepth, na.rm=TRUE),
            nhaul1      = n_distinct(shootdatetime, na.rm=TRUE),
            nhaul2      = n_distinct(shoottime, na.rm=TRUE),
            nhaul3      = n_distinct(shootdate, na.rm=TRUE), 
            duration    = sum(duration, na.rm=TRUE)) %>% 
  
  mutate(cpue2     = ifelse(duration > 0, catch2/(24*duration), NA) ) %>% 
  mutate(nhaul     = max(nhaul1, nhaul2, nhaul3)) %>% 
  select(-nhaul1, -nhaul2, -nhaul3) %>% 
  
  # add the engine power
  left_join(vessels, by="vesselname") %>% 
  
  # add the el nino data
  left_join(elnino, by=c("year","month")) %>% 
  
  # log catch per day and catch per hour
  mutate(lcatch  = log(catch+1),
         lcatch2 = log(catch2+1),
         lcpue2  = log(cpue2+1) ) %>% 
  
  # add course lat and long
  mutate( shootlat2   = 10 * as.integer(shootlat/10),
          shootlon2   = 10 * as.integer(shootlon/10)) %>% 
  
  # create area (combine lat and lon)
  unite(area, shootlat2, shootlon2, sep=":", remove=FALSE) %>% 
  
  # add empty 2012 year
  # rbind.all.columns(., data.frame(year=2012, species="CJM", catch=NA, catch2=NA, lcatch=NA, lcatch2=NA)) %>% 
  
  # ignore years prior to 2006
  filter(year >= 2007) %>% 
  data.frame()

  
  

save(cjm, file="rdata/eu_cjm.RData")
# load(file="rdata/kenmerk_temp.RData")

count_not_na(cjm)  
glimpse(cjm)
summary(cjm)
# hist(cjm$year)
# filter(cjm, is.na(species) & catch == 0) 
# sortunique(cjm$species)
# filter(cjm, species == "CJM") %>% ungroup() %>% summarise(n = n()) %>% View()
# -------------------------------------------------------------------------------------
# check vessels and year
# -------------------------------------------------------------------------------------

group_by(cjm, year, day, vesselcode, vesselname) %>% 
  summarise(catch=sum(catch,na.rm=TRUE)) %>% 
  group_by(year, vesselname) %>% 
  summarise(catch = as.integer(sum(catch, na.rm=TRUE)),
            days  = n_distinct(day)) %>% 
  mutate(cpue = as.integer(catch / days)) %>% 

  dcast(year ~ vesselname, value.var="days", sum, margins=c("year","vesselname")) %>%
  write.csv(., file="days by vessel and year.csv", row.names=FALSE)
  
  # dcast(year ~ vesselname, value.var="catch", sum, margins=c("year","vesselname")) %>%
  # write.csv(., file="cjm by vessel and year.csv", row.names=FALSE)

group_by(cjm, year, day, vesselcode, vesselname) %>% 
  summarise(catch=sum(catch,na.rm=TRUE),
            catch2 = sum(catch2, na.rm=TRUE)) %>% 
  group_by(year, vesselcode, vesselname) %>% 
  summarise(catch = as.integer(sum(catch, na.rm=TRUE)),
            catch2 = as.integer(sum(catch2, na.rm=TRUE)),
            days  = n_distinct(day)) %>% 
  group_by(year) %>% 
  summarise(catch = sum(catch, na.rm=TRUE),
            catch2 = sum(catch2, na.rm=TRUE),
            days  = sum(days, na.rm=TRUE)) %>% 
  View()

# Check average catch per vessel per day
group_by(cjm, year, day, vesselname) %>% 
  summarise(catch=sum(catch,na.rm=TRUE)) %>% 
  group_by(year, vesselname) %>% 
  summarise(catch = as.integer(sum(catch, na.rm=TRUE)),
            days  = n_distinct(day)) %>% 
  group_by(vesselname) %>% 
  summarise(catch = (sum(catch, na.rm=TRUE)),
            days  = sum(days, na.rm=TRUE)) %>%
  mutate(cpue=catch/days) %>% 
  left_join(vessels, by=c("vesselname")) %>% 
  
  ggplot(aes(x=gt, y=cpue)) +
  geom_point()

# export catch and effort by vessel, month and year relative to the absolute catch per year
group_by(cjm, vesselname, year, month) %>% 
  summarise(catch = as.integer(sum(catch, na.rm=TRUE)),
            catch2= as.integer(sum(catch2,na.rm=TRUE)),
            days  = n_distinct(shootdate)) %>% 
  mutate(cpue=catch/days) %>% 
  dcast(year + vesselname ~ month, value.var = "catch", sum, margins=c("month", "vesselname")) %>% 
  write.csv(., file="pfa_fishingactivities in sprfmo.csv", na="", row.names=FALSE)


# -------------------------------------------------------------------------------------
# check data and positions
# -------------------------------------------------------------------------------------

count_not_na(cjm)  
count_na(cjm)  

unique(cjm$duration)

filter(cjm, !is.na(shootlat), is.na(shootlon)) %>% View()
filter(cjm, is.na(shootlat), !is.na(shootlon)) %>% View()
filter(cjm, shootlat > -10) %>% View()

range(cjm$shootlon, na.rm=TRUE)
range(cjm$shootlat, na.rm=TRUE)

# Check mean catch per trip; no indication of problems in units anymore?
cjm %>% 
  mutate(week = week(shootdate)) %>% 
  group_by(vesselcode, year, week) %>% 
  filter(!is.na(catch), catch != 0) %>% 
  summarise(catch = mean(catch, na.rm=TRUE)) %>% 
  group_by(vesselcode, year) %>% 
  summarise(mincatch = min(catch), 
            maxcatch = max(catch)) %>% 
  View()


# Check catch per day vs catch per hour
cjm %>% 
  ggplot(aes(x=lcatch2, y=lcpue2)) +
  theme_publication() +
  geom_point() +
  facet_wrap(~vesselname)

cjm %>% 
  ggplot(aes(x=shootdate, y=lcatch2)) +
  theme_publication() +
  geom_point(colour="blue", alpha=0.5) +
  geom_point(aes(y=lcpue2), colour="red", alpha=0.5) +
  facet_wrap(~vesselname)
# -------------------------------------------------------------------------------------
# plot all catch per day positions by vessel
# -------------------------------------------------------------------------------------

cjm %>% 
  ggplot(aes(x=shootlon, y=shootlat)) +
  theme_publication() +
  theme(axis.title = element_blank(), 
        legend.position = "none") +
  coord_quickmap(xlim=c(-120,-50) , ylim=c(-90,-10)) +
  geom_polygon(data=fao.df,   aes(long, lat, group=group), fill = NA,
               size=0.25, color="gray60", alpha=0.3) +
  geom_jitter(aes(colour=vesselname), size=0.8, alpha=0.5) +
  facet_wrap(~year, drop=FALSE, ncol=5)

# -------------------------------------------------------------------------------------
# plot cjm catch per day
# -------------------------------------------------------------------------------------

cjm %>% 
  group_by(vesselname, year, day) %>% 
  summarise(catch = sum(catch, na.rm=TRUE), 
            catch2 = sum(catch2, na.rm=TRUE), 
            shootlat = mean(shootlat, na.rm=TRUE), 
            shootlon = mean(shootlon, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=shootlon, y=shootlat)) +
  theme_publication() +
  theme(axis.title = element_blank()) +
  coord_quickmap(xlim=c(-120,-50) , ylim=c(-50,-10)) +
  # coord_quickmap(xlim=range(cpue$shootlon, na.rm=TRUE) , 
  #               ylim=range(cpue$shootlat, na.rm=TRUE)) +
  geom_polygon(data=fao.df,   aes(long, lat, group=group), fill = NA,
               size=0.25, color="gray60", alpha=0.3) +
  geom_point(aes(size=catch), colour="blue", alpha=0.3) +
  facet_wrap(~year, drop=FALSE, ncol=4)

# -------------------------------------------------------------------------------------
# plot cjm catch per day per rect
# -------------------------------------------------------------------------------------

t <- 
  cjm %>% 
  data.frame() %>% 
  filter(!is.na(shootlon)) %>% 
  filter(!is.na(shootlat)) %>% 
  mutate(rect = encode_zchords(x=shootlon, y=shootlat, dx = 1, dy = 0.5) ) %>% 

  group_by(vesselname, year, day, rect) %>% 
  summarise(catch  = sum(catch, na.rm=TRUE), 
            catch2 = sum(catch2, na.rm=TRUE)) %>% 
  group_by(year, rect) %>% 
  summarise(catch  = mean(catch, na.rm=TRUE), 
            catch2 = mean(catch2, na.rm=TRUE)) %>% 
  separate(rect, c("shootlon", "shootlat"), sep = ":", convert = TRUE, remove = FALSE) %>% 
  
  ungroup()


b <- 
  log_breaks(n=7)(c(1,max(select(t, catch), na.rm=TRUE))) 

td <-
  t %>% 
  mutate(catch = cut(catch,breaks=b, include.lowest=T, dig.lab=10) ) %>% 
  mutate(catch2= cut(catch2,breaks=b, include.lowest=T, dig.lab=10) ) %>% 
  filter(!is.na(catch))

td %>% 
  ggplot(aes(x=shootlon, y=shootlat)) +
  theme_publication() +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text             = element_text(size=12),
        legend.key.width  = unit(1, "cm")
  ) +  
  
  coord_quickmap(xlim=c(-120,-50) , ylim=c(-50,-10)) +
  geom_polygon(data=fao.df,   aes(long, lat, group=group), fill = NA,
               size=0.25, color="gray60", alpha=0.3) +
  geom_tile(aes(shootlon, shootlat, fill = catch), colour=NA, alpha=1.0) +
  scale_fill_brewer(palette = "YlOrRd") + 
  facet_wrap(~year, drop=FALSE, ncol=4)

# -------------------------------------------------------------------------------------
# plot CPUE trends by vessel
# -------------------------------------------------------------------------------------

m <-
  cjm %>% 
  # filter(year != 2012) %>% 
  group_by(year) %>%
  summarise(lcatch2 = mean(lcatch2, na.rm=TRUE), 
            day     = mean(day, na.rm=TRUE)) %>% 
  mutate(date = as.numeric(year) + day/365) %>% 
  data.frame() %>% 
  rbind.all.columns (data.frame(year=2012))
  
cjm %>% 
  mutate(date = year + day/365) %>% 
  ggplot(aes(date, lcatch2)) +
  theme_publication() +
  theme(legend.position="none")+
  geom_jitter(aes(colour=vesselcode)) +
  geom_line(data=m, aes(date, lcatch2), colour="gray20", linetype="dashed", size=1) +
  geom_point(data=m, aes(date, lcatch2)) +
  scale_x_continuous(breaks=seq(2005, 2017, by = 5)) 
  
  
# -------------------------------------------------------------------------------------
# plot CPUE trends by area (as lcatch or lcatch2)
# -------------------------------------------------------------------------------------

m <-
  cjm %>% 
  filter(year != 2012) %>% 
  filter(lcatch >= 0.001) %>% 
  filter(shootlat2 %in% c(-20, -30, -40)) %>% 
  filter(!grepl("NA", area)) %>% 
  group_by(year, area, shootlat2, shootlon2) %>%
  summarise(lcatch = mean(lcatch, na.rm=TRUE)) %>% 
  mutate(date = as.numeric(year) + 0.5) 

cjm %>% 
  filter(!grepl("NA", area)) %>% 
  filter(lcatch >= 0.001) %>% 
  filter(shootlat2 %in% c(-20, -30, -40)) %>% 
  mutate(date = year + day/365) %>% 
  mutate(shootlat2 = factor(shootlat2, levels = c("-20", "-30", "-40"))) %>% 
  # group_by(shootlat2) %>% filter(row_number() ==1) %>% select(shootlat2) %>% View()

  ggplot(aes(date, lcatch, group=area)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_jitter(colour="lightblue", alpha=0.5) +
  geom_line(data=m, aes(x=date, y=lcatch), colour="gray20", linetype="dashed", size=1, inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(2005, 2017, by = 5)) +
  facet_grid((shootlat2)~shootlon2)


# -------------------------------------------------------------------------------------
# GAM modelling
# -------------------------------------------------------------------------------------



# different models with increasing complexity
gam00 <- gam(lcatch2 ~ as.factor(year)                                          , data=cjm)
gam00a<- gam(lcatch  ~ as.factor(year)                                          , data=cjm)
gam01 <- gam(lcatch2 ~ as.factor(year) + vesselname                             , data=cjm)
gam02 <- gam(lcatch2 ~ as.factor(year) + gt                                     , data=cjm)
gam03 <- gam(lcatch2 ~ as.factor(year) + vesselname + te(shootlon,shootlat,k=5) , data=cjm)
gam04 <- gam(lcatch2 ~ as.factor(year) + vesselname + month                     , data=cjm)
gam05 <- gam(lcatch2 ~ as.factor(year) + gt + month                             , data=cjm)
gam06 <- gam(lcatch2 ~ as.factor(year) + gt + month + shootlat2                 , data=cjm)
gam07 <- gam(lcatch2 ~ as.factor(year) + gt + month + te(shootlat,k=5)          , data=cjm)
gam08 <- gam(lcatch2 ~ as.factor(year) + gt + month + te(shootlon, shootlat,k=5), data=cjm)
gam09 <- gam(lcatch2 ~ as.factor(year) + gt + month + ELE                       , data=cjm)
gam10 <- gam(lcatch2 ~ as.factor(year) + vesselname + ELE                       , data=cjm)
gam11 <- gam(lcpue2  ~ as.factor(year) + vesselname + ELE                       , data=cjm)

# create predictor datasets
gam00_pred  <-   expand.grid(year       = unique(cjm$year)) 
gam00a_pred <-   expand.grid(year       = unique(cjm$year)) 
gam01_pred  <-   expand.grid(year       = unique(cjm$year),
                            vesselname = cjm$vesselname[1]) 
gam02_pred  <-   expand.grid(year       = unique(cjm$year),
                            gt         = cjm$gt[1]) 
gam03_pred  <-   expand.grid(year       = unique(cjm$year),
                            vesselname = cjm$vesselname[1],
                            shootlon   = cjm$shootlon[1],
                            shootlat   = cjm$shootlat[1]) 
gam04_pred  <-   expand.grid(year       = unique(cjm$year),
                            vesselname = cjm$vesselname[1],
                            month      = cjm$month[1]) 
gam05_pred  <-   expand.grid(year       = unique(cjm$year),
                            gt         = cjm$gt[1],
                            month      = cjm$month[1]) 
gam06_pred  <-   expand.grid(year       = unique(cjm$year),
                            gt         = cjm$gt[1],
                            month      = cjm$month[1],
                            shootlat2  = cjm$shootlat2[1])
gam07_pred  <-   expand.grid(year       = unique(cjm$year),
                            gt         = cjm$gt[1],
                            month      = cjm$month[1],
                            shootlat   = cjm$shootlat[1])
gam08_pred  <-   expand.grid(year       = unique(cjm$year),
                            gt         = cjm$gt[1],
                            month      = cjm$month[1],
                            shootlat   = cjm$shootlat[1],
                            shootlon   = cjm$shootlon[1])
gam09_pred  <-   expand.grid(year       = unique(cjm$year),
                            gt         = cjm$gt[1],
                            month      = cjm$month[1],
                            ELE        = cjm$ELE[1])
gam10_pred <-   expand.grid(year       = unique(cjm$year),
                            vesselname = cjm$vesselname[1],
                            ELE        = cjm$ELE[1])
gam11_pred <-   expand.grid(year       = unique(cjm$year),
                            vesselname = cjm$vesselname[1],
                            ELE        = cjm$ELE[1])


# predict
gam00_pred$pred  <- exp(predict(gam00,  newdata=gam00_pred, type="response" ))
gam00a_pred$pred <- exp(predict(gam00a, newdata=gam00a_pred, type="response" ))
gam01_pred$pred  <- exp(predict(gam01,  newdata=gam01_pred, type="response" ))
gam02_pred$pred  <- exp(predict(gam02,  newdata=gam02_pred, type="response" ))
gam03_pred$pred  <- exp(predict(gam03,  newdata=gam03_pred, type="response" ))
gam04_pred$pred  <- exp(predict(gam04,  newdata=gam04_pred, type="response" ))
gam05_pred$pred  <- exp(predict(gam05,  newdata=gam05_pred, type="response" ))
gam06_pred$pred  <- exp(predict(gam06,  newdata=gam06_pred, type="response" ))
gam07_pred$pred  <- exp(predict(gam07,  newdata=gam07_pred, type="response" ))
gam08_pred$pred  <- exp(predict(gam08,  newdata=gam08_pred, type="response" ))
gam09_pred$pred  <- exp(predict(gam09,  newdata=gam09_pred, type="response" ))
gam10_pred$pred  <- exp(predict(gam10, newdata=gam10_pred,type="response" ))
gam11_pred$pred  <- exp(predict(gam11, newdata=gam11_pred,type="response" ))

gam00_pred  <- rbind.all.columns(gam00_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam00", desc="null")
gam00a_pred <- rbind.all.columns(gam00a_pred, data.frame(year=2012, pred=NA)) %>% mutate(model="gam00a",desc="null unraised")
gam01_pred  <- rbind.all.columns(gam01_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam01", desc="vessel")
gam02_pred  <- rbind.all.columns(gam02_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam02", desc="GT")
gam03_pred  <- rbind.all.columns(gam03_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam03", desc="vessel+spatial lat lon")
gam04_pred  <- rbind.all.columns(gam04_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam04", desc="vessel+month")
gam05_pred  <- rbind.all.columns(gam05_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam05", desc="gt+month")
gam06_pred  <- rbind.all.columns(gam06_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam06", desc="gt+month+lat")
gam07_pred  <- rbind.all.columns(gam07_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam07", desc="gt+month+spatial lat")
gam08_pred  <- rbind.all.columns(gam08_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam08", desc="gt+month+spatial lat lon")
gam09_pred  <- rbind.all.columns(gam09_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam09", desc="gt+month+ELE")
gam10_pred  <- rbind.all.columns(gam10_pred, data.frame(year=2012, pred=NA)) %>% mutate(model="gam10",desc="vesselname+ELE")
gam11_pred  <- rbind.all.columns(gam11_pred, data.frame(year=2012, pred=NA)) %>% mutate(model="gam11",desc="cpue vs vesselname+ELE")

# combine and plot
gam_comb <- 
  rbind.all.columns(gam00_pred, gam00a_pred) %>% 
  rbind.all.columns(., gam01_pred) %>% 
  rbind.all.columns(., gam02_pred) %>% 
  rbind.all.columns(., gam03_pred) %>% 
  rbind.all.columns(., gam04_pred) %>% 
  rbind.all.columns(., gam05_pred) %>% 
  rbind.all.columns(., gam06_pred) %>% 
  rbind.all.columns(., gam07_pred) %>% 
  rbind.all.columns(., gam08_pred) %>% 
  rbind.all.columns(., gam09_pred) %>% 
  rbind.all.columns(., gam10_pred) %>% 
  rbind.all.columns(., gam11_pred) %>% 
  rbind.all.columns(., select(ad, year, pred=cpue, desc, model)) 

ggplot(gam_comb, aes(x=year, y=pred)) +
  theme_publication() +
  geom_point() +
  geom_line() +
  expand_limits(y=0) + 
  scale_x_continuous(breaks=seq(2005, 2017, by = 5)) +
  facet_wrap(~model, scales="free_y")

# statistics checks
gam.check(gam01); summary(gam01); anova(gam01); AIC(gam01)
gam.check(gam02); summary(gam02); anova(gam02); AIC(gam02)
gam.check(gam03); summary(gam03); anova(gam03); AIC(gam03)
gam.check(gam04); summary(gam04); anova(gam04); AIC(gam04)
gam.check(gam05); summary(gam05); anova(gam05); AIC(gam05)
gam.check(gam06); summary(gam06); anova(gam06); AIC(gam06)
gam.check(gam07); summary(gam07); anova(gam07); AIC(gam07); plot(gam07, all.terms=T)
gam.check(gam08); summary(gam08); anova(gam08); AIC(gam08); plot(gam08, all.terms=T)
gam.check(gam09); summary(gam09); anova(gam09); AIC(gam09); plot(gam09, all.terms=T)


# AIC criteria
models <- c("gam00", "gam00a","gam01","gam02","gam03","gam04","gam05","gam06", "gam07", 
            "gam08", "gam09","gam10", "gam11")
desc   <- gam_comb %>% filter(year=="2015") %>% select(model, desc) %>% filter(model != "cpue")
aic    <- matrix(NA, nrow=length(models), ncol=1, dimnames=list(name=models, "AIC"))

for (i in models) {
  aic[i,]<-AIC(get(i))  
} 

aic <-
  as.data.frame(aic) %>% 
  rownames_to_column() %>% 
  rename(model=rowname) %>% 
  left_join(desc, by=c("model"))

View(aic)

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

gam_comb %>% 
  group_by(model) %>% 
  mutate(pred2 = scale_this(pred)) %>% 
  ggplot(aes(year, pred2)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_line(aes(colour=model)) +
  geom_point() +
  scale_x_continuous(breaks=seq(2005, 2017, by = 5)) +
  facet_wrap(~model)




# check the gam model
set.seed(1)
dat <- gamSim(1,n=400,scale=2)

## fit a GAM with quite low `k'
b <- gam(y~s(x0,k=6) + s(x1,k=6) + s(x2,k=6) + s(x3,k=6), data=dat)
plot(b,pages=1,residuals=TRUE) ## hint of a problem in s(x2)

b2 <- gam( y ~ s(x0) + te(x1,x2,k=4) + s(x3), data=dat)
rsd <- residuals(b2)
gam(rsd ~ s (x0,k=40,bs="cs"),gamma=1.4,data=dat) ## fine
gam(rsd ~ te(x1,x2,k=10,bs="cs"),gamma=1.4,data=dat) 
gam(rsd ~ s (x3,k=40,bs="cs"),gamma=1.4,data=dat) ## fine
gam.check(b2) ## shows same problem

set.seed(0)
dat <- gamSim(1,n=400,scale=2)
b <- gam(y~s(x0,k=6)+s(x1,k=6)+s(x2,k=6)+s(x3,k=6), data=dat,method="REML")
gam.check(b)
b

## edf for 3rd smooth is highest as proportion of k -- increase k
b <- gam(y~s(x0,k=6)+s(x1,k=6)+s(x2,k=12)+s(x3,k=6),
         data=dat,method="REML")
gam.check(b)
b

## edf substantially up, -ve REML substantially down
b <- gam(y~s(x0,k=6)+s(x1,k=6)+s(x2,k=24)+s(x3,k=6),
         data=dat,method="REML")
gam.check(b)
b

## slight edf increase and -ve REML change
b <- gam(y~s(x0,k=6)+s(x1,k=6)+s(x2,k=40)+s(x3,k=6),
         data=dat,method="REML")
gam.check(b)
b

## defintely stabilized (but really k around 20 would have been fine