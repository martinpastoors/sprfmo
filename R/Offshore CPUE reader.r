# ==================================================================
# SPRFMO Offshore CPUE reader
#
# 17/09/2017: first version, adapted from read_kenmerken
# 21/09/2017: expanded to include GAM models for CPUE
# 08/05/2018: new reader; keep the datetime objects (at UTC time)
# 23/05/2018: finalized readed to offshore_eu2005_2017 data objects
# 28/05/2018: kept raised EU catches apart from observed catches
# 08/08/2018: updated with 2017 data and incuded all CPUE data for offshore fleets; runs as sourced
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
# library(gam)           # gam analysis
# library(mgcv)          # tensor spline for gam

# options(max.print=9999999)

# setwd("D:/SPRFMO/2017")

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

# -----------------------------------------------------------------------------------
# read old EU haullists (2005-2008)
# -----------------------------------------------------------------------------------

offshore_eu2005_2008 <-
  read_excel(file.path(data_path,"EU haullists 2005-2008.xlsx"), 
             sheet     = 1, 
             col_names = FALSE, 
             col_types = "text", 
             range     = "A2:Z2000", # cell_cols("A:V"),
             na        = "",
             skip      = 2) %>% 
  mutate  (
    file    = "EU haullists 2005-2008.xlsx",
    ws      = as.character(NA) 
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
  
  dplyr::select(-shootdate, -shoottime, -hauldate, -haultime)  %>% 
  
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


# ================================================================================
# Reading the SPRFMO EU catch effort files 2009 and beyond
# ================================================================================

# Create file list for import 
file.list   <- list.files(
      path = data_path, 
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
offshore_eu2009_2018 <- 
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

# hist(offshore_eu2009_2017$duration)
# filter(offshore_eu2009_2018, is.na(shootdatetime)) %>% View()
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
# combine EU cjm dataset. 
# -----------------------------------------------------------------------------------

# CHECK: WHAT IS THE CORRECTION FACTOR FOR? COMPARED WITH AD's DATA?
factor <- 
  data.frame(year=c(2005:2018), f =c(1,1,1.4, 1.5, 1, 1, 1.5, 1, 1, 1, 1, 1, 1, 1))

offshore_eu2005_2018  <- 
  bind_rows(offshore_eu2005_2008, offshore_eu2009_2018) %>% 

  # add and apply the correction factor (see Report to 2009 WG)
  left_join(factor, by=c("year")) %>% 
  mutate(catch2 = catch * f) %>% 

  data.frame()

  
save(offshore_eu2005_2018, file=file.path(data_path, "Offshore_eu2005_2018.RData"))

# -----------------------------------------------------------------------------------
# read non-EU dataset. 
# -----------------------------------------------------------------------------------

# Create file list for import 
file.list   <- list.files(
  path = data_path, 
  recursive  = T,
  pattern    = "Offshorefleets",
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
                 col_names = TRUE,
                 col_types = "text",
                 range     = "D1:X18960") 
    
    # add to total dataset
    if (i==1 & j == 1) t<-tmp else t<-rbind(t,tmp)  
    
  } # end of j loop
} # end of i loop


offshore_noneu2007_2017 <-
  t %>% 
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
  mutate_at(c("shootlat","shootlon",
              "haullat","haullon", "catch"), funs(as.numeric)) %>% 
  
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
  
  filter(!vesselcode %in% c("1704","9505073-6210008")) %>%    #remove vessels that have problem with units of catch
  # filter(validfishing == "1") %>% 
  
  dplyr::select(-discardedspecieskg, -discardedspecieston, -retainedspecieskg, -vesselid, -catchflagmms,
                -validfishing, -fishingmethodcode, -targetspeciescode)

save(offshore_noneu2007_2017, file=file.path(data_path, "Offshore_noneu2007_2017.RData"))

# filter(cjm_noneu, !is.na(shootdatetime2)) %>% View()
# filter(cjm_noneu, !is.na(shootdatetime)) %>% View()
# glimpse(offshore_noneu)
# head(mutate(cjm_noneu, test = as.Date(cjm_noneu$shootdatetime3, origin="1970-01-01")))
# filter(offshore_noneu, is.na(validfishing) ) %>% View()
# range(offshore_noneu$year, na.rm=T)

# -----------------------------------------------------------------------------------
# combine the offshore files
# -----------------------------------------------------------------------------------

offshore_all2005_2018 <-
  bind_rows(offshore_eu2005_2018, offshore_noneu2007_2017) %>% 
  filter(!is.na(catch)) %>% 
  mutate(
    species       = toupper(species),
    vesselcode    = gsub("\\s+|-", "", tolower(vesselcode)),
    vesselname    = tolower(vesselname),
    
    vesselcountry = ifelse(vesselcountry %in% c("GER"), "DEU",vesselcountry),
    vesselcountry = ifelse(vesselcountry %in% c("NED"), "NLD",vesselcountry),
    vesselcountry = ifelse(vesselname == "margiris", "LIT", vesselcountry),
    
    vesselcp      = ifelse(vesselcountry %in% c("DEU","LTU","NLD","LIT","NED","POL"), 
                           "EU", vesselcountry)
  ) %>% 
  dplyr::select(vesselname, vesselcountry, vesselcallsign, vesselcode, vesselimo, vesselcp, 
                shootdatetime, hauldatetime, duration, year, month, day, 
                shootlat, shootlon, haullat, haullon, 
                species, catch, catch2)


# create vesselcodes
sprfmo_vesselcodes <-
  offshore_all2005_2018 %>% 
  distinct(vesselcp, vesselname) %>% 
  group_by(vesselcp) %>% 
  mutate(vesselcode2 = paste0(vesselcp, row_number()))

# add vesselcode
offshore_all2005_2018 <-
  offshore_all2005_2018 %>% 
  left_join(sprfmo_vesselcodes, by=c("vesselname", "vesselcp"))

save(offshore_all2005_2018, file=file.path(data_path, "Offshore_all2005_2018.RData"))

# offshore_all %>% group_by(vesselcountry, vesselname) %>% filter(row_number() ==1) %>% select(vesselcountry, vesselname) %>% View()


# load the traditional CPUE dataset used in the assessment
fleet4 <- 
  read_excel(path="D:/SPRFMO/data/fleet4 CPUE.xlsx", col_names = TRUE) %>% 
  ungroup() %>% 
  lowcase() %>% 
  filter(vesselcp == "Overall") %>% 
  dplyr::select(-unit) %>% 
  spread(key=variable, value=value) %>% 
  mutate(model    = "old cpue",
         desc     = "Old Fleet 4 CPUE in assessment", 
         year     = as.factor(year)) %>% 
  data.frame()


