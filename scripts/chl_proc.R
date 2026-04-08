## Combine + synthesize Food Webs group mesocosm data
## Date created: 07 Apr 2026
## Date updated: 08 Apr 2026

library(ggplot2)
library(zoo) #na.approx

# load data
chl_2018_summer = read.csv("mesocosm_data/Chl-a_Summer_2018.csv")
chl_2019_spring = read.csv("mesocosm_data/Chl-a_Spring_2019.csv")
chl_2021_summer = read.csv("mesocosm_data/Chl-a_Summer_2021.csv")

chl_2018_summer$nday = chl_2018_summer$Jday - chl_2018_summer$Jday[1]
chl_2019_spring$nday = chl_2019_spring$Jday - chl_2019_spring$Jday[1]
chl_2021_summer$nday = chl_2021_summer$Jday - chl_2021_summer$Jday[1]


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

# plot

ggplot(data = chl_all, aes(x=nday, y=chl_a, shape=treatment_long)) + 
  geom_point(aes(col = treatment)) + geom_line(aes(col = treatment)) + 
  #geom_errorbar(aes(ymin = (chl_a - SE),
  #                  ymax = (chl_a + SE))) +
  theme_bw()


## Bloom summary stats... peak value + timing + SE's

chl_all_long = rbind(chl_2018_summer,chl_2019_spring,chl_2021_summer)
chl_all_long$Date = as.Date(chl_all_long$Date)
chl_all_long$season = ifelse(chl_all_long$Jday > 150,
                             yes = "Summer",
                             no = "Spring")

chl_all_long$treatment_long = paste(chl_all_long$season, 
                               format(as.Date(chl_all_long$Date),"%Y"),
                               chl_all_long$treatment, 
                               sep="_")


ggplot(data = chl_all_long, aes(x=nday, y=chl_a, shape=replicate)) + 
  geom_point(aes(col = treatment_long)) + geom_line(aes(col = treatment_long)) + 
  #geom_errorbar(aes(ymin = (chl_a - SE),
  #                  ymax = (chl_a + SE))) +
  theme_bw()


chl_long_rep_summary = aggregate(chl_a ~ treatment_long + replicate + treatment + season,
                             FUN = max,
                             data = chl_all_long)

chl_long_rep_summary$chl_max_Jday = NA
chl_long_rep_summary$chl_max_nday = NA
for(i in 1:length(chl_long_rep_summary$chl_max_Jday)){
  for(l in 1:length(chl_all_long$Jday)){
    if(is.na(chl_all_long$chl_a[l]) == FALSE & 
      chl_long_rep_summary$chl_a[i] == chl_all_long$chl_a[l] & 
      chl_long_rep_summary$treatment_long[i] == chl_all_long$treatment_long[l]){
      chl_long_rep_summary$chl_max_Jday[i] = chl_all_long$Jday[l]
      chl_long_rep_summary$chl_max_nday[i] = chl_all_long$nday[l]
    }
  }
}

chl_long_summary = aggregate(chl_a ~ treatment_long + treatment + season,
                             FUN = mean,
                             data = chl_long_rep_summary)
colnames(chl_long_summary) = c("treatment_long","treatment","season","mean_chl_max")

chl_long_summary$max_SE = aggregate(chl_a ~ treatment_long + treatment + season,
                             FUN = st.err,
                             data = chl_long_rep_summary)$chl_a

chl_long_summary$max_Jday = aggregate(chl_max_Jday ~ treatment_long + treatment + season,
                                    FUN = mean,
                                    data = chl_long_rep_summary)$chl_max_Jday
chl_long_summary$max_nday = aggregate(chl_max_nday ~ treatment_long + treatment + season,
                                      FUN = mean,
                                      data = chl_long_rep_summary)$chl_max_nday
chl_long_summary$max_day_SE = aggregate(chl_max_nday ~ treatment_long + treatment + season,
                                      FUN = st.err,
                                      data = chl_long_rep_summary)$chl_max_nday

chl_all_long$rep_treatment = paste(chl_all_long$replicate, chl_all_long$treatment_long, sep="_")


## Bloom summary stats... total chl + cumulative total

chl_all_long$chl_total = NA
chl_all_long$chl_cumsum = NA
chl_series = split(chl_all_long,chl_all_long$rep_treatment)

for(i in 1:length(chl_series)){ #for each replicate in each treatment...
  nn = length(chl_series[[i]]$chl_a) #check the length of the series
    if(nn %% 2 == 0){ #if length is even, then shave off last point
    n = nn-2
    
    ss = (chl_series[[i]]$chl_a[nn-1] + chl_series[[i]]$chl_a[chl_series[[i]]$chl_a[nn]]) / 2
    
    chl_series[[i]]$chl_total[nn-1] = ss
    
  }
  else if(nn %% 2 != 0){ #if length is odd, include it all
    n = nn-1
  }
  
  h=1 # defaulting to Simpson's 1/3 rule
  xValues = seq(from=1, to=n, by=2) #step size of 2
  
  data_series = na.approx(chl_series[[i]]$chl_a, na.rm = FALSE) #interpolate NA's
  
  for(q in 1:length(xValues)){
    n_sub <- xValues[[q]]-1
    n <- xValues[[q]]
    n_add <- xValues[[q]]+1
    
    v1 <- data_series[[n_sub+1]]
    v2 <- data_series[[n+1]]
    v3 <- data_series[[n_add+1]]
    
    s <- (h/3)*(v1+4*v2+v3)
    
    chl_series[[i]]$chl_total[n] = s
  }
  
  for(t in 2:length(xValues)){
    m = xValues[[t]]
    m_sub = xValues[[t]] - 2
    
    chl_series[[i]]$chl_cumsum[1] = chl_series[[i]]$chl_total[1]
    
    chl_series[[i]]$chl_cumsum[m] = chl_series[[i]]$chl_cumsum[m_sub] + chl_series[[i]]$chl_total[m]
  }
  if(nn %% 2 == 0){ #if length is even, add that last sum
    
    chl_series[[i]]$chl_cumsum[nn-1] = chl_series[[i]]$chl_cumsum[nn-3] + chl_series[[i]]$chl_total[nn-1]
  }
}

chl_all_long = unsplit(chl_series,chl_all_long$rep_treatment)

chl_all_long_noNA = chl_all_long[,-6]
chl_all_long_noNA = subset(chl_all_long_noNA, 
                           is.na(chl_all_long_noNA$chl_cumsum) == FALSE)

chl_agg_noNA = aggregate(chl_cumsum ~ treatment_long + nday + Date + Jday + treatment,
                         FUN = mean,
                         data = chl_all_long_noNA)
chl_agg_noNA$cumsum_SE = aggregate(chl_cumsum ~ treatment_long + nday + Date + Jday + treatment,
                         FUN = st.err,
                         data = chl_all_long_noNA)$chl_cumsum

ggplot(chl_agg_noNA, 
       aes(x=nday,y=chl_cumsum,col=treatment_long)) + 
  geom_point() + geom_line() + 
  geom_errorbar(aes(ymin = (chl_cumsum - cumsum_SE),
                    ymax = (chl_cumsum + cumsum_SE))) +
  theme_bw()














