## Combine + synthesize Food Webs group mesocosm data
## Date created: 07 Apr 2026
## Date updated: 09 Apr 2026

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


## Bloom summary stats... total chl + cumulative total + bloom end

# bloom end date

chl_all_long$delta_chl = NA
chl_all_long$over = NA
chl_series = split(chl_all_long,chl_all_long$rep_treatment)

for(i in 1:length(chl_series)){ #for each replicate in each treatment...
  for(c in 2:length(chl_series[[i]]$chl_a)){
    chl_series[[i]]$delta_chl[c] = chl_series[[i]]$chl_a[c] - chl_series[[i]]$chl_a[c-1]
  }
}


for(i in 1:length(chl_series)){ #for each replicate in each treatment...
  
  bloom_start_val = min(na.omit(chl_series[[i]]$chl_a[1:2]))
  
  bloom_end_val = ifelse(chl_series[[i]]$season[1] == "Spring",
         yes = bloom_start_val * 1.6, #use 60% buffer for Spring mesocosms
         no = bloom_start_val * 2) #use 100# buffer for Summer mesocosms
  
  delta_upper = 5
  delta_lower = -5
  
  for(l in 5:length(chl_series[[i]]$chl_a)){ #look at each chl-a value...
    
    if(is.na(chl_series[[i]]$chl_a[l]) == FALSE &
       is.na(chl_series[[i]]$delta_chl[l]) == FALSE &
       chl_series[[i]]$chl_a[l] <= bloom_end_val){
      
      if(chl_series[[i]]$delta_chl[l] <= delta_upper | 
         chl_series[[i]]$delta_chl[l] >= delta_lower){
        
        chl_series[[i]]$over[l] = TRUE
      }
    }
    if(is.na(chl_series[[i]]$chl_a[l]) == TRUE |
       is.na(chl_series[[i]]$delta_chl[l]) == TRUE |
       chl_series[[i]]$chl_a[l] > bloom_end_val |
       chl_series[[i]]$delta_chl[l] > delta_upper |
       chl_series[[i]]$delta_chl[l] < delta_lower){
      chl_series[[i]]$over[l] = FALSE
    }
  }
  
}


chl_all_long = unsplit(chl_series,chl_all_long$rep_treatment)

chl_all_long = chl_all_long[order(chl_all_long$treatment_long),]

chl_all_long$order = paste(chl_all_long$treatment_long,chl_all_long$replicate,sep = "_")

ggplot(data = chl_all_long) + 
  geom_point(aes(x=nday, y=chl_a, col=treatment)) + 
  geom_line(aes(x=nday, y=chl_a, col=treatment)) + 
  geom_vline(data = subset(chl_all_long, chl_all_long$over == TRUE),
             aes(xintercept = nday)) + 
  theme_bw() + 
  facet_wrap(vars(order), nrow=6, scales = "free",
             axes = "all", axis.labels = "all_y")


bloom_ends = subset(chl_all_long, chl_all_long$over == TRUE)
bloom_ends = aggregate(nday ~ rep_treatment + treatment + replicate + treatment_long + season,
                       FUN = min,
                       data = bloom_ends)

chl_long_rep_summary$bloom_end_nday = NA
for(i in 1:length(chl_long_rep_summary$treatment_long)){
  for(l in 1:length(bloom_ends$treatment_long)){
    if(chl_long_rep_summary$treatment_long[i] == bloom_ends$treatment_long[l] & 
       chl_long_rep_summary$replicate[i] == bloom_ends$replicate[l]){
      chl_long_rep_summary$bloom_end_nday[i] = bloom_ends$nday[l]
    }
  }
}

# If experiment ended before bloom did, just take last day for 'bloom end'
experiments_duration = aggregate(nday ~ rep_treatment + treatment_long + replicate,
                                 FUN = max,
                                 data = subset(chl_all_long, is.na(chl_all_long$chl_a) == FALSE))

for(i in 1:length(chl_long_rep_summary$treatment_long)){
  for(l in 1:length(experiments_duration$treatment_long)){
    if(is.na(chl_long_rep_summary$bloom_end_nday[i]) == TRUE & 
       chl_long_rep_summary$treatment_long[i] == experiments_duration$treatment_long[l] & 
       chl_long_rep_summary$replicate[i] == experiments_duration$replicate[l]){
      chl_long_rep_summary$bloom_end_nday[i] = experiments_duration$nday[l]
    }
  }
}

# now, take cumulative chl-a up to end of first bloom + total

chl_long_rep_summary$bloom_1_total_chl = NA
chl_long_rep_summary$total_chl = NA

chl_all_long$chl_total = NA
chl_all_long$chl_cumsum = NA

chl_series = split(chl_all_long,chl_all_long$rep_treatment)

for(i in 1:length(chl_series)){ #for each replicate in each treatment...
  for(r in 1:length(chl_long_rep_summary$treatment_long)){
    if(chl_long_rep_summary$treatment_long[r] == chl_series[[i]]$treatment_long[1] & 
       chl_long_rep_summary$replicate[r] == chl_series[[i]]$replicate[1]){
      firstbloom_df = subset(chl_series[[i]], chl_series[[i]]$nday <= chl_long_rep_summary$bloom_end_nday[r])
      }
    }
  
  data_series = na.approx(chl_series[[i]]$chl_a, na.rm = FALSE) #interpolate NA's
  
  nn = length(chl_series[[i]]$chl_a) #check the length of the series
  nn_mini = length(firstbloom_df$chl_a)
  
  if(nn %% 2 == 0){ #if length is even, then shave off last point + use trapez rule
    n = nn-2
    
    ss = (chl_series[[i]]$chl_a[nn-1] + chl_series[[i]]$chl_a[nn]) / 2
    
    chl_series[[i]]$chl_total[nn] = ss
  }
#  if(nn_mini %% 2 == 0){ #if length of subset is even, then use trapez rule for last point
#    
#    sss = (firstbloom_df$chl_a[nn_mini-1] + chl_series[[i]]$chl_a[nn_mini]) / 2
#    
#    chl_series[[i]]$chl_total[nn_mini-1] = sss
#  }
  if(nn %% 2 != 0){ #if length is odd, include it all
    l = nn-1
  }
  
  h=1 # defaulting to Simpson's 1/3 rule
  xValues = seq(from=1, to=l, by=2) #step size of 2
  
  for(q in 1:length(xValues)){
    n_sub <- xValues[[q]]-1
    n <- xValues[[q]]
    n_add <- xValues[[q]]+1
    
    v1 <- data_series[[n_sub+1]]
    v2 <- data_series[[n+1]]
    v3 <- data_series[[n_add+1]]
    
    s <- (h/3)*(v1+4*v2+v3)
    
    chl_series[[i]]$chl_total[n+1] = s
  }
  for(t in 2:length(xValues)){
    m = xValues[[t]] +1
    m_sub = xValues[[t]] - 1
    
    chl_series[[i]]$chl_cumsum[2] = chl_series[[i]]$chl_total[2]
    
    chl_series[[i]]$chl_cumsum[m] = chl_series[[i]]$chl_cumsum[m_sub] + chl_series[[i]]$chl_total[m]
    
  }
  if(nn %% 2 == 0){ #if length is even, add that last sum
    
    chl_series[[i]]$chl_cumsum[nn] = chl_series[[i]]$chl_cumsum[nn-2] + chl_series[[i]]$chl_total[nn]
  }
}

for(i in 1:length(chl_series)){
  
  chl_series[[i]]$chl_totals_noNA = na.approx(chl_series[[i]]$chl_cumsum,na.rm = FALSE)
  
  for(r in 1:length(chl_long_rep_summary$replicate)){
    if(chl_series[[i]]$treatment_long[1] == chl_long_rep_summary$treatment_long[r] & 
       chl_series[[i]]$replicate[1] == chl_long_rep_summary$replicate[r]){
      
      bloom_1_df = subset(chl_series[[i]], chl_series[[i]]$nday <= chl_long_rep_summary$bloom_end_nday[r])
      
      chl_long_rep_summary$bloom_1_total_chl[r] = max(na.omit(bloom_1_df$chl_totals_noNA))
      chl_long_rep_summary$total_chl[r] = max(na.omit(chl_series[[i]]$chl_totals_noNA))
    }
  }
}

chl_all_long = unsplit(chl_series,chl_all_long$rep_treatment)

chl_long_summary$end_nday = NA
chl_long_summary$end_nday_SE = NA
chl_long_summary$bloom1_totalchl = NA
chl_long_summary$bloom1_totalchl_SE = NA
chl_long_summary$totalchl = NA
chl_long_summary$totalchl_SE = NA

chl_long_summary = chl_long_summary[order(chl_long_summary$treatment_long),]
chl_long_rep_summary = chl_long_rep_summary[order(chl_long_rep_summary$treatment_long),]


chl_long_summary$end_nday = aggregate(bloom_end_nday ~ treatment_long,
                                      FUN = mean,
                                      data = chl_long_rep_summary)$bloom_end_nday
chl_long_summary$end_nday_SE = aggregate(bloom_end_nday ~ treatment_long,
                                      FUN = st.err,
                                      data = chl_long_rep_summary)$bloom_end_nday

chl_long_summary$bloom1_totalchl = aggregate(bloom_1_total_chl ~ treatment_long,
                                      FUN = mean,
                                      data = chl_long_rep_summary)$bloom_1_total_chl
chl_long_summary$bloom1_totalchl_SE = aggregate(bloom_1_total_chl ~ treatment_long,
                                         FUN = st.err,
                                         data = chl_long_rep_summary)$bloom_1_total_chl

chl_long_summary$totalchl = aggregate(total_chl ~ treatment_long,
                                      FUN = mean,
                                      data = chl_long_rep_summary)$total_chl
chl_long_summary$totalchl_SE = aggregate(total_chl ~ treatment_long,
                                         FUN = st.err,
                                         data = chl_long_rep_summary)$total_chl

chl_long_rep_summary$experiment = NA
for(i in 1:length(chl_long_rep_summary$experiment)){
  chl_long_rep_summary$experiment[i] = paste(strsplit(chl_long_rep_summary$treatment_long[i],"_")[[1]][1],strsplit(chl_long_rep_summary$treatment_long[i],"_")[[1]][2],sep="_")
  
}

ggplot(data = chl_long_rep_summary, aes(x=experiment,y=chl_max_nday,col=treatment)) + 
  geom_boxplot() + theme_bw()






## Delete code chunk below or put it (back) where it goes

for(i in 1:length(chl_long_rep_summary$treatment_long)){
  for(l in 1:length(chl_all_long$treatment_long)){
    if(chl_long_rep_summary$treatment_long[i] == chl_all_long$treatment_long[l] & 
       chl_long_rep_summary$replicate[i] == chl_all_long$replicate[l] & 
       chl_long_rep_summary$bloom_end_nday[i] == chl_all_long$nday[l]){
      
      chl_long_rep_summary$bloom_end_nday[i] = experiments_duration$nday[l]
    }
  }
}



