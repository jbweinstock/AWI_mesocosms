## Combine + synthesize Food Webs group mesocosm data
## Date created: 05 May 2026
## Date updated: 05 May 2026

library(ggplot2)
library(zoo) #na.approx
library(vegan) #second way to calculate Shannon H'
library(tidyr)

# load data
phytos_2019_spring = read.csv("mesocosm_data/phytos_2019_ugC.csv")

phytos_2019_spring$biomass = rowSums(phytos_2019_spring[,4:95])

phytos_2019_spring_pres = phytos_2019_spring

for(i in 1:length(phytos_2019_spring_pres$Day)){
  for(s in 4:length(phytos_2019_spring_pres)){
    phytos_2019_spring_pres[i,s] = ifelse(
      phytos_2019_spring_pres[i,s] > 0,
      yes = 1,
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
  all_spp = gather(phytos_2019_spring_NAs[i,4:95])
  present_spp = na.omit(all_spp$value)
  phytos_2019_spring_NAs$vegan_shannon[i] = diversity(present_spp, index="shannon")
} # ^ calculate evenness according to Lewandowska et al. 2014

phytos_2019_spring$vegan_shannon = phytos_2019_spring_NAs$vegan_shannon
#summary(phytos_2019_spring$shannon==phytos_2019_spring$vegan_shannon)
  # ^ values match! 


