library(tidyverse)
library(readxl)
source("~/theme_publication.R")

#### FIG 2b ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

ggsave("../results/fig2b.pdf", width = 7, height = 12, units = "cm")
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

ggsave("../results/fig2c.pdf", width = 7, height = 12, units = "cm")
ggsave("../results/fig2c.svg", width = 7, height = 12, units = "cm")


#### FIG 4b ####

fig4b <- read_excel("../data/supp_tables_dataset_AgroLux_paper_v2.xlsx", sheet = 8, skip = 7, n_max = 37) %>%
  select(-Mean) %>% 
 rename(Mean=`Mean – Background**`) 

fig4b %>% 
  group_by(Treatments, dpi) %>% 
  summarise(N=n(),  sd=sd(Mean), se=sd/sqrt(N), Mean=mean(Mean)) %>% 
  ggplot(aes(Treatments, Mean)) +
  geom_bar(stat="identity", width = 0.6, color="black", fill="red", alpha=0.5)+
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se),width=0.5)+
  geom_jitter(data=fig4b, aes(Treatments, Mean), size=3, alpha=0.6, width = 0.1)+
  theme_Publication(base_size = 12)+
  ylab("Luminescence (RLU)")+
  facet_wrap(~dpi) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggsave("../results/fig4b.pdf", width = 10, height = 12, units = "cm")
ggsave("../results/fig4b.svg", width = 10, height = 12, units = "cm")


# Groups for statistical significance

c_2dpi <- fig4b %>% 
  filter(Treatments=="Cf4+Avr4", dpi==2) %>% 
  select(Mean)

t1_2dpi <-  fig4b %>% 
  filter(Treatments=="Avr4", dpi==2) %>% 
  select(Mean)

t2_2dpi <-  fig4b %>% 
  filter(Treatments=="Cf4", dpi==2) %>% 
  select(Mean)

c_5dpi <- fig4b %>% 
  filter(Treatments=="Cf4+Avr4", dpi==5) %>% 
  select(Mean)

t1_5dpi <-  fig4b %>% 
  filter(Treatments=="Avr4", dpi==5) %>% 
  select(Mean)

t2_5dpi <-  fig4b %>% 
  filter(Treatments=="Cf4", dpi==5) %>% 
  select(Mean)

# significant. p.value 0.001. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c_2dpi), as.vector(t1_2dpi))

# significant. p.value 0.009. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c_2dpi), as.vector(t2_2dpi))

# significant. p.value 0.0002. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c_5dpi), as.vector(t1_5dpi))

# significant. p.value 0.0006. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c_5dpi), as.vector(t2_5dpi))
