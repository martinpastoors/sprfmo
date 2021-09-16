


# compare fork length to total length for Peru
length %>%   
  filter(year == myyear) %>%
  filter(member == "Peru") %>% 
  mutate(quarter = paste0("Q", quarter)) %>% 
  mutate(yearquarter = paste0(year, quarter)) %>% 
  
  ggplot(aes(x=length, y=cumprop, group=lengthtype)) + 
  theme_publication() +
  theme(panel.spacing     = unit(2, "mm"),
        legend.key.width  = unit(1, "cm"),
        legend.title      = element_blank(),
        panel.grid.minor  = element_blank(),
        # legend.position = "null",
        text              = element_text(size=12) ) +
  
  labs(x = "length", y = "cumulative prop") +
  geom_line(aes(colour=lengthtype), alpha=1) +
  facet_grid(member~yearquarter)
# facet_grid(fleet~quarter)
# facet_grid(.~quarter)

# plot age-length keys
alk %>%   
  # filter(year == 2019) %>% 
  mutate(quarter = paste0("Q", quarter)) %>% 
  mutate(yearquarter = paste0(year, quarter)) %>% 
  
  ggplot(aes(x=length, y=age, group=quarter)) + 
  theme_publication() +
  theme(panel.spacing     = unit(2, "mm"),
        legend.key.width  = unit(1, "cm"),
        legend.title      = element_blank(),
        panel.grid.minor  = element_blank(),
        # legend.position = "null",
        text              = element_text(size=12) ) +
  
  labs(x = "length", y = "age") +
  
  geom_point(aes(size=prop, colour=member), alpha=0.5) +
  # facet_grid(fleet~quarter)
  facet_grid(year~quarter)

# plot catch
catch %>%
  mutate(value=as.numeric(value)) %>% 
  mutate(quarter = paste0("Q", quarter)) %>% 
  mutate(yearquarter = paste0(year, quarter)) %>% 
  
  ggplot(aes(x=yearquarter, y=value, group=member)) + 
  theme_publication() +
  theme(panel.spacing     = unit(2, "mm"),
        legend.key.width  = unit(1, "cm"),
        legend.title      = element_blank(),
        panel.grid.minor  = element_blank(),
        # legend.position = "null",
        text              = element_text(size=12) ) +
  
  labs(x = "quarter", y = "catch") +
  
  geom_bar(aes(fill=member), stat="identity", alpha=1) +
  facet_grid(fleet~.)
# facet_grid(.~quarter)

# plot canum
canum %>%   
  # filter(year == myyear) %>%
  filter(variable == "canum") %>% 
  mutate(quarter = paste0("Q", quarter)) %>% 
  mutate(yearquarter = paste0(year, quarter)) %>% 
  
  ggplot(aes(x=age, y=value, group=member)) + 
  theme_publication() +
  theme(panel.spacing     = unit(2, "mm"),
        legend.key.width  = unit(1, "cm"),
        legend.title      = element_blank(),
        panel.grid.minor  = element_blank(),
        # legend.position = "null",
        text              = element_text(size=12) ) +
  
  labs(x = "age", y = "canum") +
  
  geom_bar(aes(fill=member, group=member), stat="identity", alpha=1) +
  facet_grid(fleet~yearquarter)
# facet_grid(.~quarter)




