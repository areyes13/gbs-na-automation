#2014 to 2016 yield
yield.2014.2016 <- cube.F %>% 
  gather(Date, tcv, starts_with('20')) %>% #Vert. Dates w gather
  select(Date,source,tcv) %>% #select only the columns I need
  group_by(Date, source)%>%
  summarize(tcv2 = sum(tcv))%>% 
  filter(source != "Open") %>% #filter out open opptys
  spread(source,tcv2) %>% #cast the source of data and tcv
  filter(Date >= ymd("2014-01-01") & Date<=ymd("2016-09-30"))  %>% 
  summarize( won = `Won`,
             not.won = `Not Won`,
             space.junk = `Space Junk`,
             yield = won / (not.won + space.junk))


write.csv(yield.2014.2016, "yield 2014 2016.csv")
