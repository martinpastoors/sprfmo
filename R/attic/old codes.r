Different models with increasing complexity. Same sequence as Li paper on Chinese CPUE. 

<!--GAM00: null model; year only ------------------------------------------------------ -->
  
  **GAM00. The basic model with only year as explanatory variable**
  
  ```{r message=FALSE, warning=FALSE, fig.asp=0.4}

# 00 null model: only year
gam00       <- gam(catch ~ year, offset=log(effort), data=cjm_byweek)
gam00_pred  <- 
  expand.grid(year = unique(cjm_byweek$year)) %>% 
  mutate     (pred = exp(predict(gam00,  newdata=., type="response" ))) %>% 
  mutate(model="gam00", desc="null") %>% 
  arrange(year)

summary(gam00)
anova(gam00)
AIC(gam00)

plot(gam00, all.terms=T, page=1)
gam.check(gam00)
gam00_pred %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  ggplot(aes(year,pred)) + 
  theme_publication() + 
  geom_point() + geom_line() + expand_limits(y=0) +
  labs(title = "gam00") +
  scale_x_continuous(breaks=seq(fy, ly, by = 5)) 


```



##### Page break

<!--GAM01: year and month ------------------------------------------------------ -->
  
  **GAM01: year and month**
  
  ```{r message=FALSE, warning=FALSE, fig.asp=0.4}

# 01: year and month

gam01       <- gam(lcpue ~ year + month,  data=cjm_byday)
gam01_pred  <-   
  expand.grid(year   = unique(cjm_byday$year), 
              # month  = cjm_byday$month[1]) %>% 
              month  = as.factor(3)) %>%         # month 3 has the average cpue for all months
  mutate     (pred  = exp(predict(gam01,newdata=., type="response" ))) %>% 
  mutate(model="gam01", desc="month") %>% 
  arrange    (year)

summary(gam01)
anova(gam01)
AIC(gam01)

plot(gam01, all.terms=T, page=1)
gam.check(gam01)
gam01_pred %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  ggplot(aes(year,pred)) + 
  theme_publication() + 
  geom_point() + geom_line() + expand_limits(y=0) +
  labs(title = "gam01") +
  scale_x_continuous(breaks=seq(fy, ly, by = 5)) 

```



<!--GAM02: year and month and vesselcode ------------------------------------------------------ -->
  
  ##### page break
  
  **GAM02: year, month and vesselcode**
  
  ```{r message=FALSE, warning=FALSE, fig.asp=0.8}

gam02       <- gam(lcpue ~ year + month + vesselcode2,  data=cjm_byday)
gam02_pred  <-   
  expand.grid(year        = unique(cjm_byday$year), 
              month       = as.factor("3"),
              vesselcode2 = as.factor("EU8")) %>% 
  mutate     (pred  = exp(predict(gam02,newdata=., type="response" ))) %>% 
  mutate(model="gam02", desc="month+vessel") %>% 
  arrange    (year)

summary(gam02)
anova(gam02)
AIC(gam02)

plot(gam02, all.terms=T, page=1)
gam.check(gam02)
gam02_pred %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  ggplot(aes(year,pred)) + 
  theme_publication() + 
  geom_point() + geom_line() + expand_limits(y=0) +
  labs(title = "gam02") +
  scale_x_continuous(breaks=seq(fy, ly, by = 5)) 

```

<!--GAM03: year and month and vesselcode and ELE ----------------------------------- -->
  
  ##### page break
  
  **GAM03: year, month, vesselcode and ELE**
  
  ```{r message=FALSE, warning=FALSE, fig.asp=0.8}

gam03       <- gam(lcpue ~ year + month + vesselcode2 + ELE , data=filter(cjm_byday, !is.na(ELE)))
gam03_pred  <-   
  expand.grid(year        = unique(cjm_byday$year),
              month       = as.factor(3),
              vesselcode2 = as.factor("EU8"),
              ELE         = cjm_byday$ELE[1]) %>% 
  mutate     (pred  = exp(predict(gam03,  
                                  newdata=., type="response" ))) %>% 
  mutate(model="gam03", desc="month vessel ELE") %>% 
  arrange    (year)

summary(gam03)
anova(gam03)
AIC(gam03)

plot(gam03, all.terms=T, page=1)
gam.check(gam03)
gam03_pred %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  ggplot(aes(year,pred)) + 
  theme_publication() + 
  geom_point() + geom_line() + expand_limits(y=0) +
  labs(title = "gam03") +
  scale_x_continuous(breaks=seq(fy, ly, by = 5)) 



```

<!--GAM04: year and month and vesselcode and ELE and s(shootlon) --------------------------- -->
  
  ##### page break
  
  **GAM04: year, month, vesselcode, ELE and s(lon)**
  
  ```{r message=FALSE, warning=FALSE, fig.asp=0.8}

gam04       <- gam(lcpue ~ year + month + vesselcode2 + ELE + te(shootlon, shootlat, k=5) ,
                   data=cjm_byday)
gam04_pred  <-   
  expand.grid(year       = unique(cjm_byday$year),
              month       = as.factor(3),
              vesselcode2 = as.factor("EU8"),
              ELE         = cjm_byday$ELE[1],
              shootlon    = cjm_byday$shootlon[1],
              shootlat    = cjm_byday$shootlat[1]) %>% 
  mutate     (pred  = exp(predict(gam04,  
                                  newdata=., type="response" ))) %>% 
  mutate(model="gam04", desc="month vessel ELE shootlon+shootlat") %>% 
  arrange    (year)

summary(gam04)
anova(gam04)
AIC(gam04)

plot(gam04, all.terms=T, page=1)
gam.check(gam04)
gam04_pred %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  ggplot(aes(year,pred)) + 
  theme_publication() + 
  geom_point() + geom_line() + expand_limits(y=0) +
  labs(title = "gam04") +
  scale_x_continuous(breaks=seq(fy, ly, by = 5)) 

```

<!--GAM05: year and month and vesselcode and ELE and te(lat, lon by year) --------- -->
  
  ##### page break
  
  **GAM05: year, month, vesselcode, ELE, te(lat, lon by year)**
  
  ```{r message=FALSE, warning=FALSE, fig.asp=0.8}

gam05       <- gam(lcpue ~ year + month + vesselcode2 + ELE + te(shootlon, shootlat, by=year, k=5),
                   data=cjm_byday)
gam05_pred  <-   
  expand.grid(year       = unique(cjm_byday$year),
              month       = as.factor(3),
              vesselcode2 = as.factor("EU8"),
              ELE         = cjm_byday$ELE[1],
              shootlon    = cjm_byday$shootlon[1],
              shootlat    = cjm_byday$shootlat[1]) %>% 
  mutate     (pred  = exp(predict(gam05,  
                                  newdata=., type="response" ))) %>% 
  mutate(model="gam05", desc="month vessel ELE shootlon shootlat") %>% 
  arrange    (year)

summary(gam05)
anova(gam05)
AIC(gam05)

plot(gam05, all.terms=T, page=1)
gam.check(gam05)
gam05_pred %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  ggplot(aes(year,pred)) + 
  theme_publication() + 
  geom_point() + geom_line() + expand_limits(y=0) +
  labs(title = "gam05") +
  scale_x_continuous(breaks=seq(fy, ly, by = 5)) 


```
```{r message=FALSE, warning=FALSE, fig.asp=0.8}

# Bind together
gam_comb <- 
  bind_rows(gam00_pred, gam01_pred) %>% 
  bind_rows(         ., gam02_pred) %>% 
  bind_rows(         ., gam03_pred) %>% 
  bind_rows(         ., gam04_pred) %>% 
  bind_rows(         ., gam05_pred) %>% 
  bind_rows(         ., rename(fleet4, pred=cpue)) %>% 
  mutate(year = as.numeric(as.character(year)))


ggplot(gam_comb, aes(x=year, y=pred)) +
  theme_publication() +
  geom_point(aes(colour=model)) +
  geom_line(aes(colour=model)) +
  expand_limits(y=0) + 
  scale_x_continuous(breaks=seq(fy, ly, by = 5)) 

```

```{r message=FALSE, warning=FALSE, fig.asp=0.5}

models <- c("gam00", "gam01","gam03","gam04", "gam05")
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

```

```{r message=FALSE, warning=FALSE, paged.print=TRUE}




# gam.check(gam01); summary(gam01); anova(gam01); AIC(gam01)
# gam.check(gam02); summary(gam02); anova(gam02); AIC(gam02)
# gam.check(gam03); summary(gam03); anova(gam03); AIC(gam03)
# gam.check(gam04); summary(gam04); anova(gam04); AIC(gam04)


```