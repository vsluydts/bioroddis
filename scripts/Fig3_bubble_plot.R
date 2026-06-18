## Load libraries
library(tidyverse)
library(colorspace)
library(ggplot2)

## clean workspace
rm(list = ls()); gc()

## Input Data ----- 

dat<-readRDS("./data/all11.rds")
#dat<-readRDS("./data/bioroddis.rds")

#rename Myodes glareolus into its more recently appointed genus level name: Clethrionomys + remove few specimen without species name
dat %>% mutate(Species = fct_recode(Species, "Clethrionomys_glareolus" = "Myodes_glareolus")) %>% drop_na(Species) -> dat

## Pathogens names, summarised per genus or family -----

dat$Borrelia <- ifelse((dat$Borrelia_1+dat$Borrelia_2) > 0, 1,0)  
dat$Mycoplasma_ravipulmonis <- ifelse((dat$Mycoplasma_ravipulmonis_1 + dat$Mycoplasma_ravipulmonis_2) >0, 1, 0)
dat$Chlamydia <- ifelse((dat$Chlamydia_1+dat$Chlamydia_2) > 0, 1,0)
dat$Rickettsia <- ifelse((dat$Rickettsia_1+dat$Rickettsia_2) > 0, 1,0)
dat$Streptobacillus <- ifelse((dat$Streptobacillus_1 + dat$Streptobacillus_2)> 0, 1,0)
dat$Hanta <- ifelse((dat$IFA_PUUV +dat$IFA_DOBRV)> 0, 1,0)
dat$Lepto <- ifelse((dat$lepto.lipl32 + dat$Leptospira)>0, 1, 0)

dat$Mycoplasma1 <- (dat$Mycoplasma_ravipulmonis  + dat$Mycoplasma_penetrans + dat$Mycoplasma_haemo + dat$Mycoplasma_coco + 
                      dat$Mycoplasma_microti+dat$Mycoplasma_insons)
dat$Mycoplasma <- ifelse(dat$Mycoplasma1 > 0 , 1, 0)


pathogen.names <- c("Bartonella",  "Candidatus_Neoehrlichia",   "Anaplasma"    , "Francisella",
                   "Borrelia"    ,    "Orientia"    ,   "Sarcocystidae"  
                  , "Ehrlichia" ,   "Chlamydia"     ,  "Rickettsia"   ,   "Streptobacillus", 
                   "IFA_POX" ,"Hanta" , "IFA_LCMV" , "Lepto", "Mycoplasma")  

randomeffects<-c("Species","Country", "hab2")

needed<-c(randomeffects, pathogen.names)

dat  %>% dplyr::select(all_of(needed)) %>% mutate(Habitat = fct_relevel(hab2), Habitat = fct_recode(hab2, "Temperate forest" = "Forest", "Urban green space"="Urban Park")) -> dat.fig


## Figure ---- 

#names(dat.fig[,c(1,2,20,4:19)]) : selects the Species, Country, Habitat and all the pathogens of interest for plotting

#calculation of the prevalence per country/ per host species
dat.fig[,c(1,2,20,4:19)] %>% group_by(Country) %>% pivot_longer(!c(Species,Country, Habitat), names_to = "p_id", values_to = "count")->dat.fig2
dat.fig2 %>% group_by(Country, Habitat, Species, p_id) %>% summarize(Prevalence = sum(count, na.rm=T)/n(), log_cases = log(sum(count, na.rm=T)+1)) ->dat.fig.out

#rename the short pathogen abbreviations into their proper names for appearance on the plot
dat.fig.out %>% mutate(Host_Species = Species, Pathogen= fct_recode(p_id, "Orthopoxvirus" = "IFA_POX","Leptospira" = "Lepto" ,"Orthohantavirus" = "Hanta", "Mammarenavirus" = "IFA_LCMV", "Ca. Neoerhlichia" = "Candidatus_Neoehrlichia")) -> dat.fig.out

#plot the output as a bubble plot per country, with bubble sized to prevalence and for each habitat
ggplot(dat.fig.out, aes(x=Pathogen, y=Host_Species, size = Prevalence, color = Habitat)) +
  geom_point(alpha = .4) + coord_flip() + facet_grid(~Country)+
  scale_color_manual(values = c("Urban green space" = "#E34A33", "Temperate forest" = "#006837"))+
  scale_size_area()+ theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))

#same, but safe as object
p1<- ggplot(dat.fig.out, aes(x=Pathogen, y=Host_Species, size = Prevalence, color = Habitat)) +
  geom_point(alpha = .4) + coord_flip() + facet_grid(~Country)+
  scale_color_manual(values = c("Urban green space" = "#E34A33", "Temperate forest" = "#006837"))+
  scale_size_area()+ theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


#write to pdf
pdf(file = "Fig3_bubble_plot2.pdf",
    bg = "transparent", 
    width = 12, 
    height = 6)
par(family = "serif", mfrow = c(1,4))

p1
dev.off()


#write to svg
svg(file = "Fig3_bubble_plot2.svg",
    bg = "transparent", 
    width = 12, 
    height = 6)
par(family = "serif", mfrow = c(1,4))

p1
dev.off()
