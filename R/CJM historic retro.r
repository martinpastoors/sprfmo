# -----------------------------------------------------------------------------------------------
# CJM Historic retro plot
#
# 27/09/2017 first version
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)
library(readxl)

# Load utils code
source("../mptools/r/my_utils.r")
getwd()
# load the data
cjm <- 
  read_excel(path="../jjm/excel/SPRFMO historical data.xlsx", 
             sheet = "Data",
             col_names = TRUE, col_types="numeric") %>% 
  lowcase()

# ---------------------------------------------------------------------------------------------
# Historic retros: plot stock data over different assessment years 
# ---------------------------------------------------------------------------------------------

# get the last assessment year and stock name
d <-
  cjm %>% 
  mutate(assessmenttype = ifelse(assessmentyear == max(assessmentyear),"last","assess"),
         tyear          = substr(as.character(assessmentyear),3,4)) %>% 
  rename(f = favg) %>% 
  data.frame()

# plot ssb
p1 <-
  d %>% 
  filter(!is.na(ssb)) %>%  
  
  ggplot(aes(year,ssb, group=assessmentyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = assessmenttype, size=assessmenttype, linetype=assessmenttype) ) +
  
  geom_dl(aes(label  = tyear, colour=assessmenttype), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual(values=c(last   = "red", assess = "black")) +
  
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  
  scale_size_manual(values=c(last   = 1.5,
                             assess = 0.8,
                             bench  = 1.2,
                             old    = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "SSB")  


# plot f
p2 <-
  d %>% 
  filter(!is.na(f)) %>%  
  
  ggplot(aes(year,f, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = assessmenttype, size=assessmenttype, linetype=assessmenttype) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual(values=c(last   = "red",
                               assess = "black",
                               bench  = "blue",
                               old    = "darkgreen")) +
  
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  
  scale_size_manual(values=c(last   = 1.5,
                             assess = 0.8,
                             bench  = 1.2,
                             old    = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "F")  

# plot recruitment
p3 <-
  d %>% 
  filter(!is.na(r)) %>%  
  
  ggplot(aes(year,r, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = assessmenttype, size=assessmenttype, linetype=assessmenttype) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual(values=c(last   = "red",
                               assess = "black",
                               bench  = "blue",
                               old    = "darkgreen")) +
  
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  
  scale_size_manual(values=c(last   = 1.5,
                             assess = 0.8,
                             bench  = 1.2,
                             old    = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "Recruitment")  


plot_grid(p1 + theme(legend.position = "none", axis.title = element_blank()), 
          p2 + theme(legend.position = "none", axis.title = element_blank()),
          p3 + theme(legend.position = "none", axis.title = element_blank()),
          ncol=3, align = 'h', rel_widths = c(3,3,3))

