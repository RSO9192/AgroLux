library(tidyverse)
library(readxl)
source("~/theme_publication.R")

#### FIG 2b ####

fig2b <- read_excel("../data/supp_tables_dataset_AgroLux_paper_v2.xlsx", sheet = 4, skip = 7, n_max = 49) %>% 
  group_by(Treatments, `Leaf*`) %>% 
  summarise(Mean=mean(`Mean**`)) 

fig2b %>% 
  group_by(Treatments) %>% 
  summarise(N=n(),  sd=sd(Mean), se=sd/sqrt(N), Mean=mean(Mean), ) %>% 
  mutate(se=se/1000, Mean=Mean/1000) %>% 
  mutate(Treatments = fct_relevel(Treatments, 
                            "WT + WT(GFP)", "LUX + WT(GFP)", "WT + LUX(GFP)")) %>%
ggplot(aes(Treatments, Mean)) +
  geom_bar(stat="identity", width = 0.6, color="black", fill="darkgreen", alpha=0.5)+
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se),width=0.5)+
  geom_jitter(data=fig2b, aes(Treatments, Mean/1000), size=3, alpha=0.6, width = 0.1)+
  theme_Publication(base_size = 12)+
  ylab("GFP (RFUx1000)")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggsave("../results/fig2b.svg", width = 7, height = 12, units = "cm")


#### FIG 2C ####

fig2c <- read_excel("../data/supp_tables_dataset_AgroLux_paper_v2.xlsx", sheet = 5, skip = 7, n_max = 49) %>% 
  group_by(Treatments, `Leaf*`) %>% 
  summarise(Mean=mean(`Mean – Background**`, na.rm = TRUE)) 

fig2c %>% 
  group_by(Treatments) %>% 
  summarise(N=n(),  sd=sd(Mean), se=sd/sqrt(N), Mean=mean(Mean)) %>% 
  mutate(se=se, Mean=Mean) %>% 
  mutate(Treatments = fct_relevel(Treatments, 
                                  "WT + WT(GFP)", "LUX + WT(GFP)", "WT + LUX(GFP)")) %>%
  ggplot(aes(Treatments, Mean)) +
  geom_bar(stat="identity", width = 0.6, color="black", fill="red", alpha=0.5)+
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se),width=0.5)+
  geom_jitter(data=fig2c, aes(Treatments, Mean), size=3, alpha=0.6, width = 0.1)+
  theme_Publication(base_size = 12)+
  ylab("Luminescence (RLU)")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggsave("../results/fig2c.svg", width = 7, height = 12, units = "cm")


#### FIG 
