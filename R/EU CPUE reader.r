# ==================================================================
# SPRFMO read EU catch effort
#
# 17/09/2017: first version, adapted from read_kenmerken
# 21/09/2017: expanded to include GAM models for CPUE
# 08/05/2018: new reader; keep the datetime objects (at UTC time)
# 23/05/2018: finalized readed to offshore_eu2005_2017 data objects
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
source("../prf/r/my utils.R")
source("../gisland/r/geo_inside.R")

# ================================================================================
# Reading in additional files
# ================================================================================

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
# read old EU haullists (2005-2008)
# -----------------------------------------------------------------------------------

file.list  <- list.files(
         path = "D:/SPRFMO/2017/data", 
         recursive  = F, 
         pattern    = "haullists", 
         full.names = TRUE,  
         ignore.case= TRUE)

eu_2005_2008 <-
  read_excel(file.list, 
             sheet     = 1, 
             col_names = FALSE, 
             col_types = "text", 
             range     = "A2:Z2000", # cell_cols("A:V"),
             na        = "",
             skip      = 2) %>% 
  mutate  (
    file    = file.list,
    ws      = excel_sheets( file.list) 
  ) %>%
  
  # set variable names
  setNames(c("vesselcountry","vesselname","vesselcallsign","vesselcode", "vesselimo", 
             "shootdate", "shoottime", "hauldate", "haultime", "duration", 
             "shootlat","shootlon","haullat","haullon",
             "targetspecies","trawltype","trawltype2", 
             "openingheight", "openingwidth", 
             "geardepth","waterdepth","temperature", 
             "cjm","mas", "oth", "marinemammalbycatch",
             "filename", "worksheet")) %>% 

  # convert to long format (catch is in tonnes)
  filter(!is.na(vesselcountry)) %>% 
  
  # convert variable types
  mutate_at(c("shootdate", "shoottime", "hauldate","haultime", 
              "shootlat","shootlon","haullat","haullon",
              "openingheight", "openingwidth", "geardepth", "temperature"), funs(as.numeric)) %>% 
  
  mutate(
    shoottime     = ifelse(is.na(shoottime), 0, shoottime),
    haultime      = ifelse(is.na(haultime), 0, haultime),
    shootdatetime = as.POSIXct( (shootdate+shoottime) * (60*60*24), origin="1899-12-30", tz="GMT"),
    hauldatetime  = as.POSIXct( (hauldate + haultime) * (60*60*24), origin="1899-12-30", tz="GMT"),
    duration      = as.numeric(duration),
    duration      = ifelse(!is.na(shoottime) & !is.na(haultime), 
                           hauldatetime-shootdatetime, duration),
    duration      = ifelse(duration <= 0, NA, duration/(60*60)),
    temperature   = ifelse(temperature == "0", NA, as.numeric(temperature))
  ) %>% 
  
  select(-shootdate, -shoottime, -hauldate, -haultime)  %>% 
  
  gather(key=species, value=catch, cjm:oth) %>% 
  
  mutate_at(vars(catch), funs(as.numeric)) %>% 
  
  mutate(
    haullat       = ifelse(haullat == 0, NA, haullat),
    haullon       = ifelse(haullon == 0, NA, haullon),
    shootlat      = ifelse(shootlat > 0, -1*shootlat, shootlat),
    shootlon      = ifelse(shootlon > 0, -1*shootlon, shootlon),
    haullat       = ifelse(haullat > 0, -1*haullat, haullat),
    haullon       = ifelse(haullon > 0, -1*haullon, haullon),
    
    year          = year(shootdatetime),
    month         = month(shootdatetime),
    day           = yday(shootdatetime)
  ) %>% 
  data.frame()

# hist(eu_2005_2008$duration)

# filter(oldlists, duration <= 0) %>% View()
# filter(oldlists, grepl("\\s+", shootlat)) %>% View()
# filter(t, grepl("\\s+", shootlat)) %>% View()

# count_not_na(oldlists)  
# glimpse(oldlists)
# sortunique(oldlists$duration)
# sortunique(oldlists$temperature)
# hist(oldlists$temperature)
# hist(oldlists$shoottime)
# filter(oldlists, haullon ==-8.4) %>% View()
# summary(oldlists)
# unique(oldlists$year)

# ================================================================================
# Reading the SPRFMO catch effort files 2009-2017
# ================================================================================

# Create file list for import 
file.list   <- list.files(
      path = "D:/SPRFMO/2017/data", 
      recursive  = T,
      pattern    = "SPRFMO",
      full.names = TRUE,
      ignore.case= TRUE)
file.list <- file.list[!grepl("\\~",file.list)]


i <- 1
j <- 1

for (i in 1:length(file.list)){                                           

  # get number of worksheets in the file
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


# filtering and manipulations
eu_2009_2017 <- 
  t %>% 
  setNames(c("vesselcountry","vesselname","vesselcallsign","vesselcode", "vesselimo", 
             "shootdatetime","hauldatetime", 
             "shootlat","shootlon","haullat","haullon",
             "targetspecies","trawltype","trawltype2", 
             "openingheight", "openingwidth", 
             "geardepth","waterdepth","marinemammalbycatch",
             "species","catch","discarded", "filename", "worksheet")) %>% 
  filter(!is.na(vesselcountry)) %>% 
  filter(!grepl("vessel",tolower(vesselcountry))) %>% 
  
  mutate_at(c("shootlat","shootlon","haullat","haullon",
              "openingheight","openingwidth","geardepth"), funs(as.numeric))     %>% 
  
  mutate(
    vesselcode    = gsub("\\s+|-", "", vesselcode),
    vesselcallsign= gsub("\\.", "", toupper(vesselcallsign)),
    species       = tolower(species),
    
    shootdatetime = ymd_hms(shootdatetime), 
    hauldatetime  = ymd_hms(hauldatetime),
    year          = year(shootdatetime),
    month         = month(shootdatetime),
    day           = yday(shootdatetime),
    
    catch         = ifelse(is.na(catch)    , "0", gsub("\\s+", "", catch)),
    catch         = as.numeric(catch) / 1000, 
    
    discarded     = ifelse(is.na(discarded), "0", gsub("\\s+", "", discarded)),
    discarded     = as.numeric(discarded),
    
    species       = ifelse(is.na(species) & catch == 0, "cjm",species),
    
    duration      = ifelse(!is.na(shootdatetime) & !is.na(hauldatetime), 
                           hauldatetime-shootdatetime, NA),
    duration      = ifelse(duration<=0, NA, duration/3600),
    
    haullat       = ifelse(haullat == 0, NA, haullat),
    haullon       = ifelse(haullon == 0, NA, haullon),
    shootlat      = ifelse(shootlat > 0, -1*shootlat, shootlat),
    shootlon      = ifelse(shootlon > 0, -1*shootlon, shootlon),
    haullat       = ifelse(haullat > 0, -1*haullat, haullat),
    haullon       = ifelse(haullon > 0, -1*haullon, haullon)
  ) %>% 
  data.frame()

# hist(eu_2009_2017$duration)
# filter(eu_2009_2017, duration > 20) %>% View()

# sortunique(newlists$vesselcallsign)
# sortunique(newlists$temperature)
# hist(newlists$duration)
# hist(newlists$shoottime)
# hist(newlists$haultime)
# hist(log(newlists$catch+1))
# filter(newlists, grepl("20:3:",shoottime)) %>% View()
# filter(newlists, duration <=0) %>% View()
# filter(newlists, nchar(haultime)<8) %>% View()
# hms(as.data.frame(sortunique(newlists$shoottime)), quiet = FALSE)

# -----------------------------------------------------------------------------------
# make EU cjm dataset. 
# -----------------------------------------------------------------------------------

# CHECK: WHAT IS THE CORRECTION FACTOR FOR? COMPARED WITH AD's DATA?
factor <- 
  data.frame(year=c(2005:2017), f =c(1,1,1.4, 1.5, 1, 1, 1.5, 1, 1, 1, 1, 1, 1))

offshore_eu2005_2017  <- 
  bind_rows(eu_2005_2008, eu_2009_2017) %>% 
  mutate(
    species       = toupper(species),
    vesselcode    = gsub("\\s+|-", "", tolower(vesselcode)),
    vesselname    = tolower(vesselname)
  ) %>% 

  filter(species == "CJM") %>% 
  
  # add and apply the correction factor (see Report to 2009 WG)
  left_join(factor, by=c("year")) %>% 
  mutate(catch = catch * f) %>% 

  data.frame()

  
save(offshore_eu2005_2017, file="D:/SPRFMO/2018/rdata/offshore_eu2005_2017.RData")
# load(file="rdata/kenmerk_temp.RData")

# hist(eu_cjm2005_2017$duration)
# hist(filter(cjm_noneu, vesselcountry=="VUT")$duration)
# filter(cjm_eu, is.na(year)) %>% View()
