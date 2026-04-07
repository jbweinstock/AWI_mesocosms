## Combine + synthesize Food Webs group mesocosm data
## Date created: 07 Apr 2026
## Date updated: 07 Apr 2026

library(ggplot2)

# load data
chl_2018_summer = read.csv("mesocosm_data/Chl-a_Summer_2018.csv")
chl_2019_spring = read.csv("mesocosm_data/Chl-a_Spring_2019.csv")
chl_2021_summer = read.csv("mesocosm_data/Chl-a_Summer_2021.csv")

# standard error function 
st.err = function(data){
  sdev = sd(data)
  n = length(data)
  SE = sdev / sqrt(n)
  return(SE)
}

# clean and merge data
chl_2018_summer_agg = aggregate(chl_a ~ Date + Jday + treatment,
                                data = chl_2018_summer,
                                FUN = mean, na.action = na.omit)

chl_2018_summer_agg$SE = aggregate(chl_a ~ Date + Jday + treatment,
                                   data = chl_2018_summer,
                                   FUN = st.err, na.action = na.omit)$chl_a
chl_2018_summer_agg$nday = chl_2018_summer_agg$Jday - chl_2018_summer_agg$Jday[1]
chl_2018_summer_agg$season = "Summer"

chl_2019_spring_agg = aggregate(chl_a ~ Date + Jday + treatment,
                                data = chl_2019_spring,
                                FUN = mean, na.action = na.omit)

chl_2019_spring_agg$SE = aggregate(chl_a ~ Date + Jday + treatment,
                                data = chl_2019_spring,
                                FUN = st.err, na.action = na.omit)$chl_a

chl_2019_spring_agg$nday = chl_2019_spring_agg$Jday - chl_2019_spring_agg$Jday[1]
chl_2019_spring_agg$season = "Spring"

chl_2021_summer_agg = aggregate(chl_a ~ Date + Jday + treatment,
                                data = chl_2021_summer,
                                FUN = mean, na.action = na.omit)

chl_2021_summer_agg$SE = aggregate(chl_a ~ Date + Jday + treatment,
                                data = chl_2021_summer,
                                FUN = st.err, na.action = na.omit)$chl_a

chl_2021_summer_agg$nday = chl_2021_summer_agg$Jday - chl_2021_summer_agg$Jday[1]
chl_2021_summer_agg$season = "Summer"


chl_all = rbind(chl_2018_summer_agg,chl_2019_spring_agg,chl_2021_summer_agg)

chl_all$treatment_long = paste(chl_all$season, 
                               format(as.Date(chl_all$Date),"%Y"),
                               chl_all$treatment, 
                               sep="_")

# plots

ggplot(data = chl_all, aes(x=nday, y=chl_a, shape=treatment_long)) + 
  geom_point(aes(col = treatment)) + geom_line(aes(col = treatment)) + 
  #geom_errorbar(aes(ymin = (chl_a - SE),
  #                  ymax = (chl_a + SE))) +
  theme_bw()



