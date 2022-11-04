# ==================================================================
# Offshore CPUE reader only from SPRFMO.r
#
# 17/09/2017: first version, adapted from read_kenmerken
# 21/09/2017: expanded to include GAM models for CPUE
# 08/05/2018: new reader; keep the datetime objects (at UTC time)
# 23/05/2018: finalized readed to offshore_eu2005_2017 data objects
# 28/05/2018: kept raised EU catches apart from observed catches
# 08/08/2018: updated with 2017 data and incuded all CPUE data for offshore fleets; runs as sourced
# 12/08/2019: updated with new full dataset; includes China.
# 17/09/2019: only used data received from SPRFMO secretariat
# 11/08/2020: update for data received in august 2020
# ==================================================================

# Reset lists
# rm(list=ls())

# general libraries
# library(readxl)        # excel reader from hadley; much quicker than the java based excel readers
# library(lubridate)     # data and time functions
# library(stringr)       # string manipulation
# library(pander)        # for tables
# library(reshape2)      # cast and melt dataframes
# library(broom)         # clean up statistics
# library(scales)        # pretty scales
# library(tidyverse)     # data manipulation and piping

# set onedrive directory
# onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF')

# load spatial data
# load(file.path(onedrive,"rdata/fao.RData"))
# load(file.path(onedrive,"rdata/eez.RData"))
# load(file.path(onedrive,"rdata/icesrectangles.RData"))
# load(file.path(onedrive,"rdata/world.df.RData"))
# load(file.path(onedrive,"rdata/fao.df.RData"))
# load(file.path(onedrive,"rdata/eez.df.RData"))

# source utilities
# source("../prf/r/my utils.R")
# source("../gisland/r/geo_inside.R")

# data_path <- "D:/SPRFMO/data"

# Set filename for catch data
# fn <- "Offshorefleets 20190801.xlsx"
# fn <- "Offshorefleets 20200803.xlsx"
fn <- "Offshorefleets 20210708.xlsx"

# -----------------------------------------------------------------------------------
# read SPRFMO offshore dataset. 
# -----------------------------------------------------------------------------------

offshore_all <-
  read_excel(file.path(data_path,fn), 
             sheet     = 1, 
             col_names = TRUE, 
             col_types = "text", 
             # range     = "A2:Z2000", # cell_cols("A:V"),
             na        = "",
             skip      = 0,
             .name_repair = "unique") %>% 
  mutate  (file    = fn) %>%
  lowcase() %>% 
  rename(
    vesselcallsign = callsign, 
    vesselcode     = registrationnumber,
    vesselimo      = imonumber,
    vesselcountry  = flag,
    shootdatetime  = startdate,
    hauldatetime   = enddate,
    shootlat       = startlatitude,
    shootlon       = startlongitude,
    haullat        = endlatitude,
    haullon        = endlongitude,
    species        = caughtspeciescode,
    catch          = retainedspecieston
  ) %>% 
  
  # change variabele type
  mutate_at(c("shootlat","shootlon","haullat","haullon", "catch"), list(as.numeric)) %>% 
  
  # vesselname to lowercase
  mutate(vesselname = tolower(vesselname)) %>% 
  
  # specific modifications of variables
  mutate(
    shootdatetime2 = ifelse(grepl("^[0-9]{4}-|^[0-9]{4}/", shootdatetime), shootdatetime, NA),
    shootdatetime2 = ymd_hms(shootdatetime2),
    shootdatetime1 = ifelse(grepl("^[0-9]{4}-|^[0-9]{4}/", shootdatetime), NA, shootdatetime),
    shootdatetime1 = as.POSIXct(as.numeric(shootdatetime1) * (60*60*24), 
                                origin="1899-12-30", tz="GMT"),
    shootdatetime = ifelse(!is.na(shootdatetime2), shootdatetime2, shootdatetime1),
    shootdatetime = as.POSIXct(as.numeric(shootdatetime), tz="GMT", origin="1970-01-01") ) %>% 

  mutate(    
    hauldatetime2 = ifelse(grepl("^[0-9]{4}-|^[0-9]{4}/", hauldatetime), hauldatetime, NA),
    hauldatetime2 = ymd_hms(hauldatetime2),
    hauldatetime1 = ifelse(grepl("^[0-9]{4}-|^[0-9]{4}/", hauldatetime), NA, hauldatetime),
    hauldatetime1 = as.POSIXct(as.numeric(hauldatetime1)  * (60*60*24), 
                               origin="1899-12-30", tz="GMT"),
    hauldatetime  = ifelse(!is.na(hauldatetime2),  hauldatetime2,  hauldatetime1),
    hauldatetime  = as.POSIXct(as.numeric(hauldatetime), tz="GMT", origin="1970-01-01"),
    
    duration      = ifelse(!is.na(shootdatetime) & !is.na(hauldatetime), 
                           hauldatetime-shootdatetime, NA),
    duration      = ifelse(duration<=0, NA, duration/3600),
    year          = year(shootdatetime),
    month         = month(shootdatetime),
    day           = yday(shootdatetime)
  ) %>% 
  
  filter(targetspeciescode == "CJM") %>% 
  
  filter(!is.na(catch)) %>% 
  
  # added during 2020
  filter(validfishing == 1) %>% 
  
  filter(!vesselcode %in% c("1704","9505073-6210008")) %>%    #remove vessels that have problem with units of catch
  
  filter(!grepl("alina|sirius", tolower(vesselname))) %>%    #remove vessels that have way too many zero hauls
  
  mutate(
    species       = toupper(species),
    vesselcode    = gsub("\\s+|-", "", tolower(vesselcode)),
    vesselname    = tolower(vesselname),
    
    vesselcountry = ifelse(vesselcountry %in% c("GER"), "DEU",vesselcountry),
    vesselcountry = ifelse(vesselcountry %in% c("NED"), "NLD",vesselcountry),
    vesselcountry = ifelse(vesselcountry %in% c("LTU"), "LIT",vesselcountry),
    vesselcountry = ifelse(vesselname == "margiris", "LIT", vesselcountry),
    
    vesselcp      = ifelse (vesselcountry %in% c("NLD","DEU","POL","LIT"), "EU", vesselcountry)
    
  ) %>% 
  
  # catch corrections
  mutate(catch = ifelse(vesselname %in% c("annelies ilena", "franziska","helen mary", "jan maria", 
                                          "maartje theadora") & year == 2008,
                        1000*catch, catch)) %>% 
  
  dplyr::select(vesselname, vesselcountry, vesselcallsign, vesselcode, vesselimo, vesselcp, 
                shootdatetime, hauldatetime, duration, year, month, day, 
                shootlat, shootlon, haullat, haullon, 
                species, catch)


# create vesselcodes
sprfmo_vesselcodes <-
  offshore_all %>% 
  distinct(vesselcp, vesselname) %>% 
  group_by(vesselcp) %>% 
  mutate(vesselcode2 = paste0(vesselcp, row_number()))

# add vesselcode
offshore_all <-
  offshore_all %>% 
  left_join(sprfmo_vesselcodes, by=c("vesselname", "vesselcp"))

# CHECKS

# offshore_all %>% filter(vesselname=="annelies ilena", year==2008) %>% View()
# offshore_all %>% group_by(vesselname, vesselcode) %>% summarize(catch = sum(catch, na.rm=TRUE)) %>% View()

# offshore_all %>% 
#   filter(vesselname=="maartje theadora") %>% 
#   group_by(vesselname, vesselcode, year) %>% 
#   summarize(catch = as.integer(sum(catch, na.rm=TRUE))) %>% 
#   View()

# get(load(file.path(data_path, "offshore_all_sprfmo2008-2019.RData"))) %>% 
# offshore_all %>%
#   # filter(vesselname %in% c("maartje theadora", "helen mary", "annelies ilena", "franziska","jan maria",
#   #                          "margiris")) %>% 
#   # filter(year == 2008) %>% 
#   group_by(year, vesselname, vesselcountry) %>% 
#   summarize(avgcatch = scales::comma(mean(catch, na.rm=TRUE), accuracy=0.1)) %>% 
#   pivot_wider(names_from="year", values_from = "avgcatch") %>% 
#   arrange(vesselcountry, vesselname) %>% 
#   View()

# filter(cjm_noneu, !is.na(shootdatetime2)) %>% View()
# filter(cjm_noneu, !is.na(shootdatetime)) %>% View()
# glimpse(offshore_noneu)
# head(mutate(cjm_noneu, test = as.Date(cjm_noneu$shootdatetime3, origin="1970-01-01")))
# filter(offshore_noneu, is.na(validfishing) ) %>% View()
# range(offshore_noneu2007_2018$year, na.rm=T)
# sort(unique(offshore_all$year))
# offshore_all %>% filter(vesselcp=="EU") %>% distinct(vesselname, year) %>% View()
# offshore_all %>% filter(vesselname=="alina")  %>% View()
# count_not_na(offshore_all)

# plot catch per week per vessel per year
# offshore_all %>%
#   filter(species=="CJM") %>%
#   mutate(week = week(shootdatetime)) %>%
#   group_by(vesselcp, vesselcode2, vesselname, year, week) %>%
#   summarize(catch = sum(catch, na.rm=TRUE)) %>%
#   group_by(vesselcp, vesselcode2, vesselname, year) %>%
#   summarize(catch = mean(catch, na.rm=TRUE)) %>%
#   filter(vesselcp == "EU") %>%
# 
#   ggplot(aes(x=year, y=catch)) +
#   theme_publication() +
#   theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
#   geom_bar(stat="identity") +
#   geom_point(colour="red") +
#   facet_wrap(~vesselname)


# plot EU vessels by year
# offshore_all %>%
#   filter(species=="CJM") %>%
#   filter(vesselcp=="EU") %>%
#   group_by(vesselcode2, year, day) %>%
#   summarize(catch = sum(catch, na.rm=TRUE)) %>%
#   group_by(vesselcode2, year) %>%
#   summarize(catch = mean(catch, na.rm=TRUE)) %>%
# 
#   ggplot(aes(x=vesselcode2, y=catch)) +
#   theme_publication() +
#   theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
#   geom_bar(stat="identity") +
#   facet_wrap(~year)


