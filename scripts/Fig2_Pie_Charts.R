

library(tidyverse)
library(Hmsc) #depends on package coda!
library(corrplot)
library(colorspace)
library(vioplot)
library(GGally)
library(writexl)
library(ggplot2)
library(PerformanceAnalytics)
library(grid)
library(gridExtra)
library(ggpie)
library("RColorBrewer")

rm(list = ls()); gc()

dat<-readRDS("./data/all11.rds")




dat %>% drop_na(Species) %>% droplevels() -> dat

dat %>% mutate(Species = fct_recode(Species, "Clethrionomys_glareolus" = "Myodes_glareolus")) %>% drop_na(Species)-> dat.new


dat.new$Cat.Urb <- ifelse(dat.new$Species  == "Apodemus_flavicollis" |
                             dat.new$Species  == "Crocidura_leucodon"|
                             dat.new$Species  == "Glis_glis"|
                             dat.new$Species  == "Micromys_minutus"|
                             dat.new$Species  == "Microtus_agrestis"|
                             dat.new$Species  == "Microtus_subterraneus"|
                             dat.new$Species  == "Sorex_minutus" , "Avoid", 
                           ifelse(dat.new$Species  == "Apodemus_agrarius" |
                                    dat.new$Species  == "Apodemus_sylvaticus"|
                                    dat.new$Species  == "Microtus_arvalis"|
                                    dat.new$Species  == "Clethrionomys_glareolus" , "Adapt", 
                                  ifelse(dat.new$Species  == "Crocidura_russula" | 
                                           dat.new$Species  == "Mus_musculus"|
                                           dat.new$Species  == "Rattus_norvegicus"|
                                           dat.new$Species  == "Sciurus_vulgaris" , "Dwell","na")))

dat.new$col <- ifelse(dat.new$Cat.Urb == "Avoid", "#238443", 
                    ifelse(dat.new$Cat.Urb == "Adapt", "#FFFFCC", "#D7301F"))

dat.new  %>% dplyr::select(Species, Cat.Urb)%>% distinct(Species, .keep_all = T) -> kleur

colfunc <- colorRampPalette(c("#D7301F", "#FFFFCC", "#238443"))

#colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
col=(colfunc(20))
plot(rep(1,20),col=(colfunc(20)), pch=19,cex=2)
plot(rep(1,5), col = col[1:5], pch=19, cex=2)

col2 <- c(col[7:10], col[14:20], col[c(1,3,5,6)])
kleur <- kleur[order(kleur$Cat.Urb),]
kleur$col2 <- col2

kleur$col <- ifelse(kleur$Cat.Urb == "Avoid", "#238443", 
                    ifelse(kleur$Cat.Urb == "Adapt", "#FFFFCC", "#D7301F"))

names(dat)
getwd()


col9 = brewer.pal(n = 9, name = "YlGn")
col9

dat.new %>% dplyr::select(Species) %>% unique() -> out

length(out[,1]) -> nb.cols
  
col15 <- colorRampPalette(brewer.pal(n = 9, name = "YlGn"))(nb.cols)
  
col15

out <- out[order(out$Species),]

cbind(out,col15) -> out2

head(out2)

out2<-as.data.frame(out2)

names(out2)<- c('Species', 'col')

head(out2)

col16 <- kleur[order(kleur$Species),"col"]
col15

dat.new %>% ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                      label_info = "count", label_type = "horizon",label_split = NULL, fill_color = col16,
                      label_size = 3, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkgreen")+
  theme(legend.position = "left")

dat.new %>% ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                    label_info = "count", label_type = "horizon",label_split = NULL, fill_color = col16,
                    label_size = 3, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkgreen")+
  theme(legend.position = "left") ->p1

p1 + guides(fill=guide_legend(title="Species"))

## ALL
kleur[kleur$Species == "Clethrionomys_glareolus", "col2"] <-  "#FCF4C2"

kleur[kleur$Species == "Apodemus_sylvaticus", "col2"] <- "#F8DEB0"

kleur[kleur$Species == "Microtus_arvalis", "col2"] <-  "#F4C89E"

kleur[kleur$Species == "Apodemus_agrarius", "col2"] <- "#F0B28C"
kleur[kleur$Species == "Rattus_norvegicus", "col2"] <- "#D7301F"
 kleur[kleur$Species == "Crocidura_russula", "col2"] <- "#E78767"

dat.new %>% filter(Habitat2 == "Forest" ) %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Forest" ) %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                                             label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                                             label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 4, donut.label.color = "darkgreen")+
  theme(legend.position = "left")-> p2

p2 + guides(fill=guide_legend(title="Species"))

dat.new %>% filter(Habitat2 == "Urban Park" ) %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Urban Park" ) %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                       label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                       label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 4, donut.label.color = "darkred")+
  theme(legend.position = "left")

##ALL

dat.new %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                       label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                       label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 4, donut.label.color = "darkgreen")+
  theme(legend.position = "left")


table(dat.new$hab2)

## Belgium
kleur

dat.new %>% filter(Habitat2 == "Forest" & Country == "Belgium") %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Forest" & Country == "Belgium") %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                                         label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                                         label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkgreen")+
  theme(legend.position = "none") -> b1

dat.new %>% filter(Habitat2 == "Urban Park" & Country == "Belgium") %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Urban Park" & Country == "Belgium") %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                                             label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                                             label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkred")+
  theme(legend.position = "none") -> b2

## France

dat.new %>% filter(Habitat2 == "Forest" & Country == "France") %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Forest" & Country == "France") %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                                             label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                                             label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkgreen")+
  theme(legend.position = "none") -> f1

dat.new %>% filter(Habitat2 == "Urban Park" & Country == "France") %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Urban Park" & Country == "France") %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                                                 label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                                                 label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkred")+
  theme(legend.position = "none") -> f2

## Germany

dat.new %>% filter(Habitat2 == "Forest" & Country == "Germany") %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Forest" & Country == "Germany") %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                                            label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                                            label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkgreen")+
  theme(legend.position = "none") -> g1

dat.new %>% filter(Habitat2 == "Urban Park" & Country == "Germany") %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Urban Park" & Country == "Germany") %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                                                label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                                                label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkred")+
  theme(legend.position = "none") -> g2





## Ireland

dat.new %>% filter(Habitat2 == "Forest" & Country == "Ireland") %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Forest" & Country == "Ireland") %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                                            label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                                            label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkgreen")+
  theme(legend.position = "none") -> I1

dat.new %>% filter(Habitat2 == "Urban Park" & Country == "Ireland") %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Urban Park" & Country == "Ireland") %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                                                label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                                                label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkred")+
  theme(legend.position = "none") -> I2




## Poland

dat.new %>% filter(Habitat2 == "Forest" & Country == "Poland") %>% distinct(Species)-> sp

kleur[kleur$Species %in% sp$Species, ] -> coltje
coltje[order(coltje$Species), "col2"] -> kleurtjes

dat.new %>% filter(Habitat2 == "Forest" & Country == "Poland") %>%  ggdonut(group_key = "Species", count_type = "full",r0 = 1.5,r1 = 5,
                                                                             label_info = "count", label_type = "horizon",label_split = NULL,fill_color = kleurtjes,
                                                                             label_size = 4, label_pos = "in", label_threshold = 10, donut.label.size = 3, donut.label.color = "darkgreen")+
  theme(legend.position = "none") -> p1


grid.arrange(b1, f1, g1, I1, p1, b2, f2, g2, I2, nrow = 2)

getwd()


pdf(file = "pie_all1.pdf",
    bg = "transparent", 
    width = 15, 
    height = 10)
par(family = "serif", mfrow = c(1,4))

#kleur <- out3$kleur
grid.arrange(b1, f1, g1, I1, p1, b2, f2, g2, I2, nrow = 2)


dev.off()

