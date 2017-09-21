# ==================================================================
# SPRFMO read EU catch effort
#
# 17/09/2017: first version, adapted from read_kenmerken
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
library(tidyverse)     # data manipulation and piping
# library(gam)           # gam analysis
library(mgcv)          # tensor spline for gam

options(max.print=9999999)

setwd("D:/SPRFMO/2017")

source("D:/GIT/mptools/R/my_utils.r")

load("D:/XXX/prf/rdata/world.df.RData")
load("D:/XXX/prf/rdata/eez.df.RData")
load("D:/XXX/prf/rdata/fao.df.RData")

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


# ================================================================================
# Reading the catch effort files
# ================================================================================

# Create file list for import 
file.list          <- list.files(
  path       = "data",
  recursive  = T,
  pattern    = "SPRFMO",
  full.names = TRUE,
  ignore.case= TRUE)
file.list <- file.list[!grepl("\\~",file.list)]


i <- 1
j <- 1

for (i in 1:length(file.list)){                                           

  # get number of worksheets
  j <- length( excel_sheets( file.list[i] ) )
  
  # read all the worksheets in the file
  for (j in 1:j) {
    
    print(paste(i, file.list[i], excel_sheets( file.list[i])[j], sep=" "))
    
    tmp <-
      read_excel(file.list[i], 
        sheet     = j, 
        col_names = FALSE, 
        col_types = "text", 
        range     = "A2:V1000", # cell_cols("A:V"),
        na        = "",
        skip      = 2) %>% 
      mutate  (
        file    = file.list[i],
        ws      = excel_sheets( file.list[i])[j]) 

    # print(names(tmp))
    
    # add to total dataset
    if (i==1 & j == 1) t<-tmp else t<-rbind(t,tmp)  
    
  } # end of j loop
} # end of i loop

# add names to columns
names(t) <-  c("vesselcountry","vesselname","vesselcallsign","vesselcode", "vesselimo", 
               "shootdatetime","hauldatetime", 
               "shootlat","shootlon","haullat","haullon",
               "targetspecies","trawltype","trawltype2", 
               "openingheight", "openingwidth", 
               "geardepth","waterdepth","marinemammalbycatch",
               "species","catch","discarded", "filename", "worksheet")

# filtering and manipulations
t <- 
  t %>% 
  filter(!is.na(vesselcountry)) %>% 
  filter(!grepl("vessel",tolower(vesselcountry))) %>% 
  
  mutate(
    catch         = gsub("\\s+", "",catch),
    catch         = as.numeric(catch) / 1000, 
    shootdatetime = gsub("\\s+", "",toupper(shootdatetime)),
    hauldatetime  = gsub("\\s+", "",toupper(hauldatetime)),
    shootdate     = substr(shootdatetime, 1, 11),
    shoottime     = substr(shootdatetime, 13, 20),
    hauldate      = substr(hauldatetime, 1, 11),
    haultime      = substr(hauldatetime, 13, 20)
  ) %>% 
  mutate_at(c("shootdate","hauldate"), funs(lubridate::ymd)) %>%
  as.data.frame()


# filter(t, year != year2) %>% View()



# -----------------------------------------------------------------------------------
# read old haullists
# -----------------------------------------------------------------------------------

old.file.name  <- list.files(path = "data", recursive  = F, pattern    = "haullists", full.names = TRUE,  ignore.case= TRUE)
oldlists <-
  read_excel(old.file.name, 
             sheet     = j, 
             col_names = FALSE, 
             col_types = "text", 
             range     = "A2:Z2000", # cell_cols("A:V"),
             na        = "",
             skip      = 2) %>% 
  mutate  (
    file    = old.file.name,
    ws      = excel_sheets( old.file.name) )

names(oldlists) <-  
             c("vesselcountry","vesselname","vesselcallsign","vesselcode", "vesselimo", 
               "shootdate","shoottime", "hauldate", "haultime","duration", 
               "shootlat","shootlon","haullat","haullon",
               "targetspecies","trawltype","trawltype2", 
               "openingheight", "openingwidth", 
               "geardepth","waterdepth","temperature", 
               "cjm","mas", "oth", "marinemammalbycatch",
               "filename", "worksheet")

# convert to long format (catch is in tonnes)
oldlists <-
  oldlists %>% 
  mutate(shootdate = ifelse(toupper(shootdate)=="NA",NA, shootdate),
         shootdate = as.Date(as.numeric(shootdate), origin = "1899-12-30"),
         hauldate  = ifelse(toupper(hauldate)=="NA",NA, hauldate),
         hauldate  = as.Date(as.numeric(hauldate), origin = "1899-12-30") 
         ) %>% 
  gather(key=species, value=catch, cjm:oth) %>% 
  filter(!is.na(vesselcountry)) %>% 
  data.frame()

# filter(oldlists, grepl("\\s+", shootlat)) %>% View()
# filter(t, grepl("\\s+", shootlat)) %>% View()

# -----------------------------------------------------------------------------------
# make cjm dataset. filter empty rows and rows with old header information
# -----------------------------------------------------------------------------------

factor <- 
  data.frame(year=c(2005:2017), f =c(1,1,1.4, 1.5, 1.45, 1.5, 1.5, 1, 1,1, 1, 1, 1))

cjm  <- 
  rbind.all.columns(t, oldlists) %>% 
  mutate(
    species       = toupper(species),
    vesselcode    = gsub("\\s+|-", "", vesselcode),
    # shootlat      = gsub("\\s+|-", "", shootlat),
    # shootlon      = gsub("\\s+|-", "", shootlon),
    discarded     = ifelse(is.na(discarded), "0", gsub("\\s+", "", discarded)),
    discarded     = as.numeric(discarded),
    catch         = ifelse(is.na(catch)    , "0", gsub("\\s+", "", catch)),
    catch         = as.numeric(catch)
  ) %>%
  mutate_at(c("shootlat","shootlon","haullat","haullon"),funs(as.numeric)) %>% 
  mutate_at(c("vesselcode","vesselname"),funs(tolower)) %>% 
  mutate(
    haullat  = ifelse(haullat == 0, NA, haullat),
    haullon  = ifelse(haullon == 0, NA, haullon),
    shootlat = ifelse(shootlat > 0, -1*shootlat, shootlat),
    shootlon = ifelse(shootlon > 0, -1*shootlon, shootlon),
    haullat  = ifelse(haullat > 0, -1*haullat, haullat),
    haullon  = ifelse(haullon > 0, -1*haullon, haullon),
    year     = year(shootdate),
    day      = yday(shootdate),
    month    = month(shootdate),
    species  = ifelse(is.na(species) & catch == 0, "CJM",species)) %>% 
  data.frame() %>% 
  
  filter(species == "CJM") %>% 
  
  # add the correction factor
  left_join(factor, by=c("year")) %>% 
  mutate(catch2 = catch * f) %>% 
  
  # add the engine power
  left_join(vessels, by="vesselname") %>% 
  
  # log catches
  mutate(lcatch  = log(catch+1),
         lcatch2 = log(catch2+1)) %>% 
  
  # add course lat and long
  mutate( shootlat2   = 10 * as.integer(shootlat/10),
          shootlon2   = 10 * as.integer(shootlon/10)) %>% 
  
  # create area (combine lat and lon)
  unite(area, shootlat2, shootlon2, sep=":", remove=FALSE) %>% 
  
  # add empty 2012 year
  # rbind.all.columns(., data.frame(year=2012, species="CJM", catch=NA, catch2=NA, lcatch=NA, lcatch2=NA)) %>% 
  
  # ignore years prior to 2007
  filter(year >= 2007)
  
  

save(cjm, file="rdata/eu_cjm.RData")
# load(file="rdata/kenmerk_temp.RData")

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
  summarise(catch=sum(catch,na.rm=TRUE)) %>% 
  group_by(year, vesselcode, vesselname) %>% 
  summarise(catch = as.integer(sum(catch, na.rm=TRUE)),
            days  = n_distinct(day)) %>% 
  group_by(year) %>% 
  summarise(catch = (sum(catch, na.rm=TRUE)),
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


# -------------------------------------------------------------------------------------
# check data and positions
# -------------------------------------------------------------------------------------

count_not_na(cjm)  
count_na(cjm)  

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


# plot all catch
cjm %>% 
  ggplot(aes(x=shootlon, y=shootlat)) +
  theme_publication() +
  theme(axis.title = element_blank()) +
  coord_quickmap(xlim=c(-120,-50) , ylim=c(-90,-10)) +
  # coord_quickmap(xlim=range(cpue$shootlon, na.rm=TRUE) , 
  #               ylim=range(cpue$shootlat, na.rm=TRUE)) +
  geom_polygon(data=fao.df,   aes(long, lat, group=group), fill = NA,
               size=0.25, color="gray60", alpha=0.3) +
  geom_point(size=2, colour="blue", alpha=0.3) +
  facet_wrap(~year, drop=FALSE, ncol=5)

# -------------------------------------------------------------------------------------
# plot cjm catch per haul
# -------------------------------------------------------------------------------------

cjm %>% 
  ggplot(aes(x=shootlon, y=shootlat)) +
  theme_publication() +
  theme(axis.title = element_blank()) +
  coord_quickmap(xlim=c(-120,-50) , ylim=c(-50,-10)) +
  # coord_quickmap(xlim=range(cpue$shootlon, na.rm=TRUE) , 
  #               ylim=range(cpue$shootlat, na.rm=TRUE)) +
  geom_polygon(data=fao.df,   aes(long, lat, group=group), fill = NA,
               size=0.25, color="gray60", alpha=0.3) +
  geom_point(aes(size=catch), colour="blue", alpha=0.3) +
  facet_wrap(~year, drop=FALSE, ncol=5)

cjm %>% 
  ggplot(aes(x=shootlon, y=shootlat)) +
  theme_publication() +
  theme(axis.title = element_blank()) +
  coord_quickmap(xlim=c(-120,-50) , ylim=c(-50,-10)) +
  # coord_quickmap(xlim=range(cpue$shootlon, na.rm=TRUE) , 
  #               ylim=range(cpue$shootlat, na.rm=TRUE)) +
  geom_polygon(data=fao.df,   aes(long, lat, group=group), fill = NA,
               size=0.25, color="gray60", alpha=0.3) +
  geom_jitter(aes(colour=vesselcode), alpha=0.3, size=1) +
  facet_wrap(~year, drop=FALSE, ncol=5) +
  guides(colour="none")

# -------------------------------------------------------------------------------------
# prepare data for cpue analysis
# -------------------------------------------------------------------------------------

# select areas to be included (more than x observations)
selected_areas <-
  cjm %>% 
  group_by(shootlat2, shootlon2) %>% 
  summarise(nobs = n()) %>% 
  filter(nobs > 100) %>% 
  select(shootlat2, shootlon2)

cjm2 <-
  selected_areas %>% 
  left_join(cjm, by=c("shootlat2","shootlon2")) %>% 
  group_by(vesselcode, vesselname, year, month, day, area, shootlat2, shootlon2, species) %>% 
  summarise(catch  = sum(catch, na.rm=TRUE),
            catch2 = sum(catch2, na.rm=TRUE),
            nhaul1 = n_distinct(shootdatetime, na.rm=TRUE),
            nhaul2 = n_distinct(shoottime, na.rm=TRUE),
            nhaul3 = n_distinct(shootdate, na.rm=TRUE)) %>% 
  mutate(nhaul   = max(nhaul1, nhaul2, nhaul3)) %>% 
  ungroup() %>% 
  mutate(lcatch  = log(catch+1),
         lcatch2 = log(catch2+1),
         year    = as.integer(year) ) %>% 
  select(-(nhaul1:nhaul3))


# -------------------------------------------------------------------------------------
# plot CPUE trends by vessel
# -------------------------------------------------------------------------------------

m <-
  cjm2 %>% 
  filter(year != 2012) %>% 
  group_by(year) %>%
  summarise(lcatch2 = mean(lcatch2, na.rm=TRUE)) %>% 
  mutate(date = as.numeric(year) + 0.5) 
  
cjm2 %>% 
  mutate(date = year + day/365) %>% 
  ggplot(aes(date, lcatch2, group=area)) +
  theme_publication() +
  theme(legend.position="none")+
  geom_jitter(aes(colour=vesselcode)) +
  geom_line(data=m, aes(date, lcatch2), colour="gray20", linetype="dashed", size=1, inherit.aes = FALSE) 
  
# -------------------------------------------------------------------------------------
# plot CPUE trends by area (as lcatch or lcatch2)
# -------------------------------------------------------------------------------------

m <-
  cjm2 %>% 
  filter(year != 2012) %>% 
  filter(lcatch >= 0.001) %>% 
  filter(!grepl("NA", area)) %>% 
  group_by(year, area, shootlat2, shootlon2) %>%
  summarise(lcatch = mean(lcatch, na.rm=TRUE)) %>% 
  mutate(date = as.numeric(year) + 0.5) 

cjm2 %>% 
  filter(!grepl("NA", area)) %>% 
  filter(lcatch >= 0.001) %>% 
  mutate(date = year + day/365) %>% 
  mutate(shootlat2 = factor(shootlat2, levels = c("-20", "-30", "-40"))) %>% 
  
  ggplot(aes(date, lcatch, group=area)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_jitter(aes(colour=area), alpha=0.5) +
  geom_line(data=m, aes(x=date, y=lcatch, colour=area), linetype="dashed", size=1, inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(2005, 2017, by = 5)) +
  facet_grid((shootlat2)~shootlon2)

# -------------------------------------------------------------------------------------

m <-
  cjm2 %>% 
  filter(year != 2012) %>% 
  filter(lcatch2 >= 0.001) %>% 
  filter(!grepl("NA", area)) %>% 
  group_by(year, area, shootlat2, shootlon2) %>%
  summarise(lcatch2 = mean(lcatch2, na.rm=TRUE)) %>% 
  mutate(date = as.numeric(year) + 0.5) 

cjm2 %>% 
  filter(!grepl("NA", area)) %>% 
  filter(lcatch2 >= 0.001) %>% 
  mutate(date = year + day/365) %>% 
  mutate(shootlat2 = factor(shootlat2, levels = c("-20", "-30", "-40"))) %>% 
  
  ggplot(aes(date, lcatch2, group=area)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_jitter(aes(colour=area), alpha=0.5, shape=16) +
  geom_line(data=m, aes(x=date, y=lcatch2, colour=area), linetype="dashed", size=1, inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(2005, 2017, by = 5)) +
  facet_grid((shootlat2)~shootlon2)

# -------------------------------------------------------------------------------------
# GLM modelling
# -------------------------------------------------------------------------------------

head(cjm2)

# how to deal with zero catches?
# what is the intercept referring to.

# models including different variables 
summary(glm0 <- glm(round(catch2)~1, family="poisson", 
                    data=cjm2[cjm2$year >= 2007,]))
summary(glm1 <- glm(round(catch2)~as.factor(year), family="poisson", 
                    data=cjm2))
summary(glm2 <- glm(round(catch2)~as.factor(year)+vesselname, family="poisson", 
                    data=cjm2))
summary(glm3 <- glm(round(catch2)~as.factor(year)+vesselname+area, family="poisson", 
                    data=cjm2[cjm2$year >= 2007,]))

glm3a <- tidy(glm3)

glm3a %>% 
  filter(grepl("year",term)) %>% 
  ungroup() %>% 
  mutate(rel_est = estimate/mean(estimate, na.rm=TRUE)) %>% 
  mutate(year = as.numeric(gsub("as.factor(year)","", term, fixed=TRUE))) %>% 
  ggplot(aes(year, (estimate))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2005, 2017, by = 5)) 
  




# -------------------------------------------------------------------------------------
# GAM modelling
# -------------------------------------------------------------------------------------

# different models with increasing complexity
gam0  <- gam(lcatch2 ~ as.factor(year)                                         , data=cjm)
gam0a <- gam(lcatch  ~ as.factor(year)                                         , data=cjm)
gam1  <- gam(lcatch2 ~ as.factor(year) + vesselname                            , data=cjm)
gam2  <- gam(lcatch2 ~ as.factor(year) + gt                                    , data=cjm)
gam3  <- gam(lcatch2 ~ as.factor(year) + vesselname + te(shootlon,shootlat,k=5), data=cjm)
gam4  <- gam(lcatch2 ~ as.factor(year) + vesselname + month                    , data=cjm)
gam5  <- gam(lcatch2 ~ as.factor(year) + gt + month                            , data=cjm)
gam6  <- gam(lcatch2 ~ as.factor(year) + gt + month + shootlat2                , data=cjm)
gam7  <- gam(lcatch2 ~ as.factor(year) + gt + month + te(shootlat,k=5)         , data=cjm)

# create predictor datasets
gam0_pred  <-   expand.grid(year       = unique(cjm$year)) 
gam0a_pred <-   expand.grid(year       = unique(cjm$year)) 
gam1_pred  <-   expand.grid(year       = unique(cjm$year),
                            vesselname = cjm$vesselname[1]) 
gam2_pred  <-   expand.grid(year       = unique(cjm$year),
                            gt         = cjm$gt[1]) 
gam3_pred  <-   expand.grid(year       = unique(cjm$year),
                            vesselname = cjm$vesselname[1],
                            shootlon   = cjm$shootlon[1],
                            shootlat   = cjm$shootlat[1]) 
gam4_pred  <-   expand.grid(year       = unique(cjm$year),
                            vesselname = cjm$vesselname[1],
                            month      = cjm$month[1]) 
gam5_pred  <-   expand.grid(year       = unique(cjm$year),
                            gt         = cjm$gt[1],
                            month      = cjm$month[1]) 
gam6_pred  <-   expand.grid(year       = unique(cjm$year),
                            gt         = cjm$gt[1],
                            month      = cjm$month[1],
                            shootlat2  = cjm$shootlat2[1])
gam7_pred  <-   expand.grid(year       = unique(cjm$year),
                            gt         = cjm$gt[1],
                            month      = cjm$month[1],
                            shootlat   = cjm$shootlat[1])


# predict
gam0_pred$pred  <- exp(predict(gam0,  newdata=gam0_pred, type="response" ))
gam0a_pred$pred <- exp(predict(gam0a, newdata=gam0a_pred, type="response" ))
gam1_pred$pred  <- exp(predict(gam1,  newdata=gam1_pred, type="response" ))
gam2_pred$pred  <- exp(predict(gam2,  newdata=gam2_pred, type="response" ))
gam3_pred$pred  <- exp(predict(gam3,  newdata=gam3_pred, type="response" ))
gam4_pred$pred  <- exp(predict(gam4,  newdata=gam4_pred, type="response" ))
gam5_pred$pred  <- exp(predict(gam5,  newdata=gam5_pred, type="response" ))
gam6_pred$pred  <- exp(predict(gam6,  newdata=gam6_pred, type="response" ))
gam7_pred$pred  <- exp(predict(gam7,  newdata=gam7_pred, type="response" ))

gam0_pred  <- rbind.all.columns(gam0_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam0", desc="null")
gam0a_pred <- rbind.all.columns(gam0a_pred, data.frame(year=2012, pred=NA)) %>% mutate(model="gam0a",desc="null unraised")
gam1_pred  <- rbind.all.columns(gam1_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam1", desc="vessel")
gam2_pred  <- rbind.all.columns(gam2_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam2", desc="GT")
gam3_pred  <- rbind.all.columns(gam3_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam3", desc="vessel+spatial")
gam4_pred  <- rbind.all.columns(gam4_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam4", desc="vessel+month")
gam5_pred  <- rbind.all.columns(gam5_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam5", desc="gt+month")
gam6_pred  <- rbind.all.columns(gam6_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam6", desc="gt+month+lat")
gam7_pred  <- rbind.all.columns(gam7_pred , data.frame(year=2012, pred=NA)) %>% mutate(model="gam7", desc="gt+month+spatial lat")

# plot
ggplot(gam0_pred,  aes(year, pred)) +  geom_point() + geom_line() + expand_limits(y=0) + scale_x_continuous(breaks=seq(2005, 2017, by = 5))
ggplot(gam0a_pred, aes(year, pred)) +  geom_point() + geom_line() + expand_limits(y=0) + scale_x_continuous(breaks=seq(2005, 2017, by = 5))
ggplot(gam1_pred,  aes(year, pred)) +  geom_point() + geom_line() + expand_limits(y=0) + scale_x_continuous(breaks=seq(2005, 2017, by = 5))
ggplot(gam2_pred,  aes(year, pred)) +  geom_point() + geom_line() + expand_limits(y=0) + scale_x_continuous(breaks=seq(2005, 2017, by = 5))
ggplot(gam3_pred,  aes(year, pred)) +  geom_point() + geom_line() + expand_limits(y=0) + scale_x_continuous(breaks=seq(2005, 2017, by = 5))
ggplot(gam4_pred,  aes(year, pred)) +  geom_point() + geom_line() + expand_limits(y=0) + scale_x_continuous(breaks=seq(2005, 2017, by = 5))
ggplot(gam5_pred,  aes(year, pred)) +  geom_point() + geom_line() + expand_limits(y=0) + scale_x_continuous(breaks=seq(2005, 2017, by = 5))

# combine and plot
gam_comb <- 
  rbind.all.columns(gam0_pred, gam0a_pred) %>% 
  rbind.all.columns(., gam1_pred) %>% 
  rbind.all.columns(., gam2_pred) %>% 
  rbind.all.columns(., gam3_pred) %>% 
  rbind.all.columns(., gam4_pred) %>% 
  rbind.all.columns(., gam5_pred) %>% 
  rbind.all.columns(., gam6_pred) %>% 
  rbind.all.columns(., gam7_pred) %>% 
  rbind.all.columns(., select(ad, year, pred=cpue, desc, model))

ggplot(gam_comb, aes(x=year, y=pred)) +
  theme_publication() +
  geom_point() +
  geom_line() +
  expand_limits(y=0) + 
  scale_x_continuous(breaks=seq(2005, 2017, by = 5)) +
  facet_wrap(~model, scales="free_y")

# statistics checks
gam.check(gam1); summary(gam1); anova(gam1); AIC(gam1)
gam.check(gam2); summary(gam2); anova(gam2); AIC(gam2)
gam.check(gam3); summary(gam3); anova(gam3); AIC(gam3)
gam.check(gam4); summary(gam4); anova(gam4); AIC(gam4)
gam.check(gam5); summary(gam5); anova(gam5); AIC(gam5)
gam.check(gam6); summary(gam6); anova(gam6); AIC(gam6)
AIC(c(gam4, gam3))

# AIC criteria
models <- c("gam0", "gam0a","gam1","gam2","gam3","gam4","gam5","gam6", "gam7")
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


cjm %>% 
  group_by(year) %>% 
  summarize(lcatch = mean(lcatch, na.rm=TRUE),
            lcatch2= mean(lcatch2,na.rm=TRUE)) %>% 
  ggplot(aes(x=year)) +
  geom_line(aes(y=lcatch), colour="red") +
  geom_line(aes(y=lcatch2), colour="blue")
