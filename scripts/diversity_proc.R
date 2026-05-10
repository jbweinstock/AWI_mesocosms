## Combine + synthesize Food Webs group mesocosm data
## Date created: 05 May 2026
## Date updated: 10 May 2026

library(ggplot2)
library(ggpubr) #ggarrange
library(zoo) #na.approx
library(vegan) #second way to calculate Shannon H'
library(tidyr)

# load/subset data
phytos_2018_summer = read.csv("mesocosm_data/phytos_2018_ugC.csv")
phytos_2019_spring = read.csv("mesocosm_data/phytos_2019_ugC.csv")
# zoos_2019_spring = read.csv("mesocosm_data/zoos_2019_ugC.csv")
# zoos_2019_spring = subset(zoos_2019_spring, 
#                           zoos_2019_spring$Scenario == "Ambient" |
#                             zoos_2019_spring$Scenario == "ERCP 8.5")
plankton_2021_summer = read.csv("mesocosm_data/small_plankton_2021_ugC.csv")

phytos_2021_summer = subset(plankton_2021_summer, 
                            plankton_2021_summer$Group == "Microphytoplankton" |
                              plankton_2021_summer$Group == "Nanophytoplankton")
phytos_2021_summer$Biomass = as.numeric(phytos_2021_summer$Biomass)
phytos_2021_summer$Day = as.integer(phytos_2021_summer$Day)
phytos_2021_summer$taxon = paste(phytos_2021_summer$Genus,
                                 phytos_2021_summer$Species,
                                 phytos_2021_summer$Size.class,
                                 sep=" ")


## get everything formatted the same + calculate total biomass per replicate per day

phytos_2019_spring$biomass = rowSums(phytos_2019_spring[,4:95])

phytos_2018_summer_mini = phytos_2018_summer[,-(1:7)]
phytos_2018_summer_long = pivot_longer(data = phytos_2018_summer_mini,
                                       col = c("Control","Control.1","Control.2",
                                               "ERCP.8.5","ERCP.8.5.1","ERCP.8.5.2"),
                                       names_to = "replicate",
                                       values_to = "biomass")

phytos_2018_summer_long$treatment = ifelse(phytos_2018_summer_long$replicate=="Control" |
                                             phytos_2018_summer_long$replicate=="Control.1" |
                                             phytos_2018_summer_long$replicate=="Control.2",
                                           yes = "Ambient",
                                           no = "ERCP 8.5")

phytos_2018_summer_wide = pivot_wider(data = phytos_2018_summer_long, 
                                      names_from = "Taxon.name",
                                      values_from = "biomass")
phytos_2018_summer_wide$total_biomass = rowSums(phytos_2018_summer_wide[,4:59])

phytos_2021_summer_mini = phytos_2021_summer[,-(3)]
phytos_2021_summer_mini = phytos_2021_summer_mini[,-(5:18)]

phytos_2021_summer_wide = pivot_wider(data = phytos_2021_summer_mini, 
                                      names_from = "taxon",
                                      values_from = "Biomass",
                                      values_fill = 0)
phytos_2021_summer_wide$total_biomass = rowSums(phytos_2021_summer_wide[,5:65])


ggplot(phytos_2021_summer_wide, aes(x=Day,y=total_biomass,fill=Replicate,col=Mesocosm)) + 
  geom_point(shape=21) + geom_line() + theme_bw() #+ scale_y_continuous(limits=c(0,450))


## Calculate diversity metrics

data = NA
presence = NA
proportions = NA
spp_row_start = NA
spp_row_end = NA

phytos_2019_spring_pres = phytos_2019_spring #copy data for pres/abs

for(i in 1:length(phytos_2019_spring_pres$Day)){ #for each day/replicate...
  for(s in 4:95){ #for each species...
    phytos_2019_spring_pres[i,s] = ifelse(
      phytos_2019_spring_pres[i,s] > 0,
      yes = 1, #convert to presence (1) or absence (0)
      no = 0 
    )
  }
}
phytos_2019_spring_pres$richness = NA
phytos_2019_spring_pres$richness = rowSums(phytos_2019_spring_pres[,4:95])

phytos_2019_spring_props = phytos_2019_spring

phytos_2019_spring$shannon = NA

for(i in 1:length(phytos_2019_spring_props$Day)){ #for every replicate on every sample day...
  for(s in 4:95){ # for each species / taxon... 
    if(phytos_2019_spring[i,s] != 0){ #for only species present...
      proportion = phytos_2019_spring[i,s] / phytos_2019_spring$biomass[i] #calculate biomass proportion (p_i)
      phytos_2019_spring_props[i,s] = proportion * log(proportion) #then calculate p_i*log(p_i)
    }
  }
}
phytos_2019_spring$shannon = -1*rowSums(phytos_2019_spring_props[,4:95]) #H' = -sum( p_i * log(p_i) )

phytos_2019_spring$richness = phytos_2019_spring_pres$richness
phytos_2019_spring$evenness = NA
for(i in 1:length(phytos_2019_spring$Day)){ #for every replicate on every sample day...
  phytos_2019_spring$evenness[i] = phytos_2019_spring$shannon[i] / log(phytos_2019_spring$richness[i])
} # ^ calculate evenness according to Lewandowska et al. 2014

phytos_2019_spring_NAs = phytos_2019_spring

for(i in 1:length(phytos_2019_spring_NAs$Day)){
  for(s in 4:length(phytos_2019_spring_NAs)){
    phytos_2019_spring_NAs[i,s] = ifelse(
      phytos_2019_spring_NAs[i,s] > 0,
      yes = phytos_2019_spring_NAs[i,s],
      no = NA
    )
  }
}
phytos_2019_spring_NAs$vegan_shannon = NA
for(i in 1:length(phytos_2019_spring_NAs$Day)){ #for every replicate on every sample day...
  all_spp = gather(phytos_2019_spring_NAs[i,4:95]) #turn data into long format
  present_spp = na.omit(all_spp$value) # remove NA values (aka absent taxa)
  phytos_2019_spring_NAs$vegan_shannon[i] = diversity(present_spp, index="shannon")
} # ^ calculate evenness according to Lewandowska et al. 2014

phytos_2019_spring$vegan_shannon = phytos_2019_spring_NAs$vegan_shannon
#summary(phytos_2019_spring$shannon==phytos_2019_spring$vegan_shannon)
  # ^ values match! 



phytos_2018_summer_pres = phytos_2018_summer_wide #copy data for pres/abs

for(i in 1:length(phytos_2018_summer_pres$DOY)){ #for each day/replicate...
  for(s in 4:59){ #for each species/taxon...
    phytos_2018_summer_pres[i,s] = ifelse(
      phytos_2018_summer_pres[i,s] > 0,
      yes = 1, #convert to present (1) or absent (0)
      no = 0
    )
  }
}
phytos_2018_summer_pres$richness = NA
phytos_2018_summer_pres$richness = rowSums(phytos_2018_summer_pres[,4:59])

phytos_2018_summer_props = phytos_2018_summer_wide

phytos_2018_summer_wide$shannon = NA

for(i in 1:length(phytos_2018_summer_props$DOY)){ #for every replicate on every sample day...
  for(s in 4:59){ # for each species / taxon... 
    if(phytos_2018_summer_wide[i,s] != 0){ #for only species present...
      proportion = phytos_2018_summer_wide[i,s] / phytos_2018_summer_wide$total_biomass[i] #calculate biomass proportion (p_i)
      phytos_2018_summer_props[i,s] = proportion * log(proportion) #then calculate p_i*log(p_i)
    }
  }
}
phytos_2018_summer_wide$shannon = -1*rowSums(phytos_2018_summer_props[,4:59]) #H' = -sum( p_i * log(p_i) )

phytos_2018_summer_wide$richness = phytos_2018_summer_pres$richness
phytos_2018_summer_wide$evenness = NA
for(i in 1:length(phytos_2018_summer_wide$DOY)){ #for every replicate on every sample day...
  phytos_2018_summer_wide$evenness[i] = phytos_2018_summer_wide$shannon[i] / log(phytos_2018_summer_wide$richness[i])
} # ^ calculate evenness according to Lewandowska et al. 2014

phytos_2018_summer_NAs = phytos_2018_summer_wide

for(i in 1:length(phytos_2018_summer_NAs$DOY)){
  for(s in 4:59){
    phytos_2018_summer_NAs[i,s] = ifelse(
      phytos_2018_summer_NAs[i,s] > 0,
      yes = phytos_2018_summer_NAs[i,s],
      no = NA
    )
  }
}
phytos_2018_summer_NAs$vegan_shannon = NA
for(i in 1:length(phytos_2018_summer_NAs$DOY)){ #for every replicate on every sample day...
  all_spp = gather(phytos_2018_summer_NAs[i,4:59]) #turn data into long format
  present_spp = na.omit(all_spp$value) # remove NA values (aka absent taxa)
  phytos_2018_summer_NAs$vegan_shannon[i] = diversity(present_spp, index="shannon")
} # ^ calculate evenness according to Lewandowska et al. 2014

phytos_2018_summer_wide$vegan_shannon = phytos_2018_summer_NAs$vegan_shannon
#summary(phytos_2018_summer_wide$shannon==phytos_2018_summer_wide$vegan_shannon)
# ^ values match! 



phytos_2021_summer_pres = phytos_2021_summer_wide #copy data for pres/abs

for(i in 1:length(phytos_2021_summer_pres$Day)){ #for each day/replicate...
  for(s in 5:65){ #for each species/taxon...
    phytos_2021_summer_pres[i,s] = ifelse(
      phytos_2021_summer_pres[i,s] > 0,
      yes = 1, #convert to present (1) or absent (0)
      no = 0
    )
  }
}
phytos_2021_summer_pres$richness = NA
phytos_2021_summer_pres$richness = rowSums(phytos_2021_summer_pres[,5:59])

phytos_2021_summer_props = phytos_2021_summer_wide

phytos_2021_summer_wide$shannon = NA

for(i in 1:length(phytos_2021_summer_props$Day)){ #for every replicate on every sample day...
  for(s in 5:65){ # for each species / taxon... 
    if(phytos_2021_summer_wide[i,s] != 0){ #for only species present...
      proportion = phytos_2021_summer_wide[i,s] / phytos_2021_summer_wide$total_biomass[i] #calculate biomass proportion (p_i)
      phytos_2021_summer_props[i,s] = proportion * log(proportion) #then calculate p_i*log(p_i)
    }
  }
}
phytos_2021_summer_wide$shannon = -1*rowSums(phytos_2021_summer_props[,5:65]) #H' = -sum( p_i * log(p_i) )

phytos_2021_summer_wide$richness = phytos_2021_summer_pres$richness
phytos_2021_summer_wide$evenness = NA
for(i in 1:length(phytos_2021_summer_wide$Day)){ #for every replicate on every sample day...
  phytos_2021_summer_wide$evenness[i] = phytos_2021_summer_wide$shannon[i] / log(phytos_2021_summer_wide$richness[i])
} # ^ calculate evenness according to Lewandowska et al. 2014

phytos_2021_summer_NAs = phytos_2021_summer_wide

for(i in 1:length(phytos_2021_summer_NAs$Day)){
  for(s in 5:65){
    phytos_2021_summer_NAs[i,s] = ifelse(
      phytos_2021_summer_NAs[i,s] > 0,
      yes = phytos_2021_summer_NAs[i,s],
      no = NA
    )
  }
}
phytos_2021_summer_NAs$vegan_shannon = NA
for(i in 1:length(phytos_2021_summer_NAs$Day)){ #for every replicate on every sample day...
  all_spp = gather(phytos_2021_summer_NAs[i,5:65]) #turn data into long format
  present_spp = na.omit(all_spp$value) # remove NA values (aka absent taxa)
  phytos_2021_summer_NAs$vegan_shannon[i] = diversity(present_spp, index="shannon")
} # ^ calculate evenness according to Lewandowska et al. 2014

phytos_2021_summer_wide$vegan_shannon = phytos_2021_summer_NAs$vegan_shannon
#summary(phytos_2021_summer_wide$shannon==phytos_2021_summer_wide$vegan_shannon)
# ^ values match! 


## exploratory plots

biomass_2018 <- ggplot(data = aggregate(total_biomass ~ DOY + treatment,
                                        data = phytos_2018_summer_wide,
                                        FUN = mean), 
                       aes(x=DOY,y=total_biomass,col=treatment,shape=treatment)) +
  geom_point() + geom_line() + theme_bw() + guides(fill="none") + 
  scale_colour_manual(values = c("cornflowerblue", "red4"))

biomass_2019 <- ggplot(data = aggregate(biomass ~ Day + Scenario,
                                        data = phytos_2019_spring,
                                        FUN = mean), 
                       aes(x=Day,y=biomass,col=Scenario,shape=Scenario))+
  geom_point() + geom_line() + theme_bw() + guides(fill="none") + 
  scale_colour_manual(values = c("cornflowerblue", "red4"))

biomass_2021 <- ggplot(data = aggregate(total_biomass ~ Day + Mesocosm,
                                        data = phytos_2021_summer_wide,
                                        FUN = mean), 
                       aes(x=Day,y=total_biomass,col=Mesocosm,shape=Mesocosm))+
  geom_point() + geom_line() + theme_bw() + guides(fill="none") + 
  scale_colour_manual(values = c("cornflowerblue", "red4","grey"))

richness_2018 <- ggplot(data = aggregate(richness ~ DOY + treatment,
                                         data = phytos_2018_summer_wide,
                                         FUN = mean), 
                        aes(x=DOY,y=richness,col=treatment,shape=treatment))+
  geom_point() + geom_line() + theme_bw() + guides(fill="none") + 
  scale_colour_manual(values = c("cornflowerblue", "red4"))

richness_2019 <- ggplot(data = aggregate(richness ~ Day + Scenario,
                                         data = phytos_2019_spring,
                                         FUN = mean), 
                        aes(x=Day,y=richness,col=Scenario,shape=Scenario))+
  geom_point() + geom_line() + theme_bw() + guides(fill="none") + 
  scale_colour_manual(values = c("cornflowerblue", "red4"))

richness_2021 <- ggplot(data = aggregate(richness ~ Day + Mesocosm,
                                         data =phytos_2021_summer_wide,
                                         FUN = mean), 
                        aes(x=Day,y=richness,col=Mesocosm,shape=Mesocosm))+
  geom_point() + geom_line() + theme_bw() + guides(fill="none") + 
  scale_colour_manual(values = c("cornflowerblue", "red4","grey"))

evenness_2018 <- ggplot(data = aggregate(evenness ~ DOY + treatment,
                                         data = phytos_2018_summer_wide,
                                         FUN = mean), 
                        aes(x=DOY,y=evenness,col=treatment,shape=treatment))+
  geom_point() + geom_line() + theme_bw() + guides(fill="none") + 
  scale_colour_manual(values = c("cornflowerblue", "red4"))

evenness_2019 <- ggplot(data = aggregate(evenness ~ Day + Scenario,
                                         data = phytos_2019_spring,
                                         FUN = mean), 
                        aes(x=Day,y=evenness,col=Scenario,shape=Scenario))+
  geom_point() + geom_line() + theme_bw() + guides(fill="none") + 
  scale_colour_manual(values = c("cornflowerblue", "red4"))

evenness_2021 <- ggplot(data = aggregate(evenness ~ Day + Mesocosm,
                                         data = phytos_2021_summer_wide,
                                         FUN = mean), 
                        aes(x=Day,y=evenness,col=Mesocosm,shape=Mesocosm))+
  geom_point() + geom_line() + theme_bw() + guides(fill="none") + 
  scale_colour_manual(values = c("cornflowerblue", "red4","grey"))


ggarrange(chl_2018,chl_2019,chl_2021,
          biomass_2018,biomass_2019,biomass_2021,
          richness_2018,richness_2019,richness_2021,
          evenness_2018,evenness_2019,evenness_2021,
          ncol=3,
          nrow = 4,
          common.legend = TRUE)




