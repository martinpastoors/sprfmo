dat %>% 
  group_by(area, year, age) %>% 
  summarise(value = sum(data, na.rm=TRUE)) %>% 
  group_by(area, age) %>% 
  mutate(value = value/mean(value, na.rm=TRUE)) %>% 
  mutate(yc = year - age) %>% 
  data.frame() %>% 
  
  ggplot() +
  
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border     = element_rect(colour="black" , size=0.1)) +
  theme(axis.ticks.y     = element_blank() ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  theme(panel.spacing = unit(0.2, "lines")) +
  
  geom_col(aes(year, value, fill = factor(yc))) + 
  scale_fill_crayola() +
  labs(x = NULL, y = NULL, title="Jack mackerel relative catch at age") +
  facet_grid(age ~ area, scale = "free_y", switch = "y")


dat %>% 
  group_by(year, age) %>% 
  summarise(value = sum(data, na.rm=TRUE)) %>% 
  group_by(age) %>% 
  mutate(value = value/mean(value, na.rm=TRUE)) %>% 
  mutate(yc = year - age) %>% 
  data.frame() %>% 
  
  ggplot() +
  
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border     = element_rect(colour="black" , size=0.1)) +
  theme(axis.ticks.y     = element_blank() ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  theme(panel.spacing = unit(0.2, "lines")) +
  
  geom_col(aes(year, value, fill = factor(yc))) + 
  scale_fill_crayola() +
  labs(x = NULL, y = NULL, title="Jack mackerel relative catch at age") +
  facet_grid(age ~ ., scale = "free_y", switch = "y")
