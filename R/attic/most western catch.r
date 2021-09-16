offshore_all %>% 
  filter(!is.na(shootlon) & !is.na(shootlat)) %>% 
  filter(!is.na(year)) %>% 
  
  group_by(year) %>% 
  summarise(shootlon  = min(shootlon, na.rm=TRUE)) %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  
  ggplot(aes(x=year, y=shootlon)) +
  theme_publication() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text             = element_text(size=12),
        legend.key.width = unit(1, "cm"),
        panel.spacing    = unit(0.1, "lines") ) +  
  
  geom_point() +
  geom_line() +
  coord_flip() +
  scale_x_reverse(breaks=c(2005:2017)) +
  ggtitle("Jack mackerel most western catch by year")