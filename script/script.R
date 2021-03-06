library(tidyverse)
library(readxl)
library(lme4)
library(lemon)
library(nlme)
source("~/theme_publication.R")


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# figure 2B-C -------------------------------------------------------------

fig2b <-
  read_excel(
    "../data/supp_tables_dataset_AgroLux_paper_v5.xlsx",
    sheet = 3,
    skip = 7,
    n_max = 200
  ) %>%
  group_by(Treatments, Leaf, `Final OD`) %>%
  summarise(Mean = mean(Luminescence), OD=mean(`Lux OD`)) %>%
  mutate(
    Treatments = as.factor(Treatments))

fig2b %>% 
  group_by(`Final OD`, OD) %>% 
  summarise(Mean2=mean(Mean), sd=sd(Mean), N=n(), se=sd/sqrt(N)) %>% 
  mutate(Mean=Mean2) %>% 
  ggplot(aes(OD, Mean)) +
  geom_smooth(method = "lm", color="Red", se = FALSE)+
  geom_jitter(data=fig2b, size=2, alpha=0.5)+
  geom_point(size=4, alpha=0.8, color="Red3")+
  geom_errorbar(aes(ymin = Mean - se, ymax = Mean + se), width = 0.1)+
  facet_rep_wrap(.~`Final OD`, nrow = 2, repeat.tick.labels = "all")+
  theme_Publication(base_size = 12)

  ggsave(
    "../results/fig2b.pdf",
    width = 7,
    height = 12,
    units = "cm"
  )

ggsave(
  "../results/fig2b.svg",
  width = 7,
  height = 12,
  units = "cm"
)


# figure 2D -------------------------------------------------------------

fig2d <-
  read_excel(
    "../data/supp_tables_dataset_AgroLux_paper_v5.xlsx",
    sheet = 4,
    skip = 7,
    n_max = 200
  ) %>% 
  select(`Final OD`, Treatments, `Lux OD`, `CFU/cm2...15`) %>% 
  rename(Mean=`CFU/cm2...15`, OD=`Lux OD`) %>% 
  mutate(OD=as.factor(OD))


fig2d %>% 
  group_by(`Final OD`, OD) %>% 
  summarise(Mean2=mean(Mean), sd=sd(Mean), N=n(), se=sd/sqrt(N)) %>% 
  mutate(Mean=Mean2) %>% 
  ggplot(aes(OD, Mean)) +
  geom_bar(stat="identity",fill="gray", color="black")+
  geom_errorbar(aes(ymin = Mean - se, ymax = Mean + se), width = 0.05)+
  geom_jitter(data=fig2d, size=3, alpha=0.5, width = 0.08)+
  facet_rep_wrap(.~`Final OD`, nrow = 2, repeat.tick.labels = "all")+
  theme_Publication(base_size = 12)

ggsave(
  "../results/fig2d.pdf",
  width = 7,
  height = 12,
  units = "cm"
)

ggsave(
  "../results/fig2d.svg",
  width = 7,
  height = 12,
  units = "cm"
)



# figure 3B ---------------------------------------------------------------


fig3b <-
  read_excel(
    "../data/supp_tables_dataset_AgroLux_paper_v5.xlsx",
    sheet = 5,
    skip = 7,
    n_max = 42
  ) %>%
  group_by(Treatments, `Leaf*`) %>%
  summarise(Mean = mean(`Mean**`)) %>%
  mutate(
    Treatments = as.factor(Treatments),
    Treatments = fct_relevel(Treatments,
                             "WT + WT(GFP)", "LUX + WT(GFP)", "WT + LUX(GFP)")
  )

fig3b %>%
  group_by(Treatments) %>%
  summarise(
    N = n(),
    sd = sd(Mean),
    se = sd / sqrt(N),
    Mean = mean(Mean),
    
  ) %>%
  mutate(se = se / 1000, Mean = Mean / 1000) %>%
  ggplot(aes(Treatments, Mean)) +
  geom_bar(
    stat = "identity",
    width = 0.6,
    color = "black",
    fill = "darkgreen",
    alpha = 0.5
  ) +
  geom_errorbar(aes(ymin = Mean - se, ymax = Mean + se), width = 0.5) +
  geom_jitter(
    data = fig3b,
    aes(Treatments, Mean / 1000),
    size = 3,
    alpha = 0.6,
    width = 0.1
  ) +
  theme_Publication(base_size = 12) +
  ylab("Fluorescence (RFU x 1000)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "../results/fig3b.pdf",
  width = 7,
  height = 12,
  units = "cm"
)

ggsave(
  "../results/fig3b.svg",
  width = 7,
  height = 12,
  units = "cm"
)

# statistics

mod_3b <- lm(Mean~Treatments, data=fig3b)

plot(mod_3b)

anova(mod_3b)

stat_3b <- c(" Non significant. [F(2,18)=0.1797, p=0.1797]")

write.table(stat_3b, file = "../results/stat_fig3b.txt")

# figure 3C ---------------------------------------------------------------

fig3c <-
  read_excel(
    "../data/supp_tables_dataset_AgroLux_paper_v5.xlsx",
    sheet = 6,
    skip = 7,
    n_max = 42
  ) %>%
  group_by(Treatments, `Leaf*`) %>%
  summarise(Mean = mean(`Mean – Background**`, na.rm = TRUE)) %>%
  mutate(
    Treatments = as.factor(Treatments),
    Treatments = fct_relevel(Treatments,
                             "WT + WT(GFP)", "LUX + WT(GFP)", "WT + LUX(GFP)")
  )

fig3c %>%
  group_by(Treatments) %>%
  summarise(
    N = n(),
    sd = sd(Mean),
    se = sd / sqrt(N),
    Mean = mean(Mean)
  ) %>%
  mutate(se = se, Mean = Mean) %>%
  ggplot(aes(Treatments, Mean)) +
  geom_bar(
    stat = "identity",
    width = 0.6,
    color = "black",
    fill = "red",
    alpha = 0.5
  ) +
  geom_errorbar(aes(ymin = Mean - se, ymax = Mean + se), width = 0.3) +
  geom_jitter(
    data = fig3c,
    aes(Treatments, Mean),
    size = 3,
    alpha = 0.6,
    width = 0.1
  ) +
  theme_Publication(base_size = 12) +
  ylab("Luminescence (RLU)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "../results/fig3c.pdf",
  width = 7,
  height = 12,
  units = "cm"
)

ggsave(
  "../results/fig3c.svg",
  width = 7,
  height = 12,
  units = "cm"
)


# statistics

mod_3c<- lm(Mean~Treatments, data=fig3c)

plot(mod_3c)

anova(mod_3c)

stat_3c <- c(" Non significant. [F(1,12)=0.51, p=0.48]")

write.table(stat_3c, file = "../results/stat_fig3c.txt")


# figure 4c ---------------------------------------------------------------

fig4c <-
  read_excel(
    "../data/supp_tables_dataset_AgroLux_paper_v5.xlsx",
    sheet = 7,
    skip = 7,
    n_max = 100
  ) %>%
  mutate(Mean = `Mean – Background*`) %>%
  mutate(Mean = ifelse(Mean < 0, 0, Mean)) %>%
  filter(Treatments!="Ctrl") %>% 
  mutate(
    Treatments = as.factor(Treatments),
    Treatments = fct_relevel(Treatments, "Low", "High"),
    dpi=as.factor(dpi)
  ) %>% 
  group_by(Treatments, dpi, Plant) %>% 
  summarise(Mean=mean(Mean))


fig4c %>%
  group_by(Treatments, dpi) %>%
  summarise(
    N = n(),
    sd = sd(Mean),
    se = sd / sqrt(N),
    Mean = mean(Mean)
  ) %>%
  mutate(se = se, Mean = Mean) %>%
  ggplot(aes(dpi, Mean, fill = Treatments)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           color = "black") +
  geom_errorbar(
    aes(ymin = Mean - se, ymax = Mean + se),
    width = 0.3,
    size = 1,
    position = position_dodge(width = 0.9)
  ) +
  geom_point(
    data = fig4c,
    aes(dpi, Mean, color = Treatments),
    color = "black",
    position = position_dodge(width = 0.9),
    size = 3,
    alpha = 0.6
  ) +
  theme_Publication(base_size = 12) +
  ylab("Luminescence (RLU)") +
 theme(legend.position = "none") +
  scale_fill_manual(values = c("red", "orange"))


ggsave(
  "../results/fig4c.pdf",
  width = 10,
  height = 12,
  units = "cm"
)

ggsave(
  "../results/fig4c.svg",
  width = 10,
  height = 12,
  units = "cm"
)

# statistical significance at each DPIs

for (i in seq_along(1:4)) {
  
  C1 <- fig4c %>%
    filter(dpi == i, Treatments=="Low") %>% 
    select(Mean)
  
  T1 <- fig4c %>%
    filter(dpi == i, Treatments=="High") %>% 
    select(Mean)
  
  print(t.test(as.vector(C1$Mean), as.vector(T1$Mean)))
  
}


# non significant

p.value <-
  data.frame(
    dpi = 1:4,
    t.test = c(
      "p=0.03",
      "p=0.81",
      "p=0.31",
      "p=0.36"
    )
  )

write_csv(p.value, "../results/pvalues_fig4c.csv")


# figure 4d ---------------------------------------------------------------


fig4d <-
  read_excel(
    "../data/supp_tables_dataset_AgroLux_paper_v5.xlsx",
    sheet = 8,
    skip = 7,
    n_max = 200
  ) %>%
  group_by(Treatments, Plant, dpi) %>%
  summarise(Mean = mean(`Mean – Background*`, na.rm = TRUE)) %>%
  filter(Treatments != "Ctrl") %>%
  mutate(Mean = ifelse(Mean < 0, 0, Mean), Mean = Mean / 1000) %>%
  mutate(
    Treatments = as.factor(Treatments),
    Treatments = fct_relevel(Treatments, "Low", "High")
  )

fig4d %>%
  group_by(Treatments, dpi) %>%
  summarise(
    N = n(),
    sd = sd(Mean),
    se = sd / sqrt(N),
    Mean = mean(Mean)
  ) %>%
  mutate(se = se, Mean = Mean) %>%
  ggplot(aes(dpi, Mean, fill = Treatments)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           color = "black") +
  geom_errorbar(
    aes(ymin = Mean - se, ymax = Mean + se),
    width = 0.3,
    size = 1,
    position = position_dodge(width = 0.9)
  ) +
  geom_point(
    data = fig4d,
    aes(dpi, Mean, color = Treatments),
    color = "black",
    position = position_dodge(width = 0.9),
    size = 3,
    alpha = 0.6
  ) +
  theme_Publication(base_size = 12) +
  ylab("GFP (RFU X 1000)") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("red", "orange", "yellow")) +
  ylim(0, 20)



ggsave(
  "../results/fig4d.pdf",
  width = 10,
  height = 12,
  units = "cm"
)

ggsave(
  "../results/fig4d.svg",
  width = 10,
  height = 12,
  units = "cm"
)


# statistical significance at each DPIs

for (i in 2:4) {
  
  print(i)
  
  C1 <- fig4d %>%
    filter(dpi ==i, Treatments=="Low") %>% 
    select(Mean)
  
  T1 <- fig4d %>%
    filter(dpi == i, Treatments=="High") %>% 
    select(Mean)
  
  print(t.test(as.vector(C1$Mean), as.vector(T1$Mean)))
  
}


# significant at 4 dpi

p.value <-
  data.frame(
    dpi = 2:4,
    t.test = c(
      "p=0.11",
      "p=0.07",
      "p=0.00025"
    )
  )

write_csv(p.value, "../results/pvalues_fig4d.csv")


# figure 5b ---------------------------------------------------------------


fig5b <-
  read_excel(
    "../data/supp_tables_dataset_AgroLux_paper_v5.xlsx",
    sheet = 9,
    skip = 7,
    n_max = 37
  ) %>%
  select(-Mean) %>%
  rename(Mean = `Mean – Background**`)

fig5b %>%
  group_by(Treatments, dpi) %>%
  summarise(
    N = n(),
    sd = sd(Mean),
    se = sd / sqrt(N),
    Mean = mean(Mean)
  ) %>%
  ggplot(aes(Treatments, Mean)) +
  geom_bar(
    stat = "identity",
    width = 0.6,
    color = "black",
    fill = "red",
    alpha = 0.5
  ) +
  geom_errorbar(aes(ymin = Mean - se, ymax = Mean + se), width = 0.5) +
  geom_jitter(
    data = fig5b,
    aes(Treatments, Mean),
    size = 3,
    alpha = 0.6,
    width = 0.1
  ) +
  theme_Publication(base_size = 12) +
  ylab("Luminescence (RLU)") +
  facet_wrap( ~ dpi) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "../results/fig5b.pdf",
  width = 10,
  height = 12,
  units = "cm"
)
ggsave(
  "../results/fig5b.svg",
  width = 10,
  height = 12,
  units = "cm"
)


# Groups for statistical significance

c_2dpi <- fig5b %>%
  filter(Treatments == "Cf4+Avr4", dpi == 2) %>%
  select(Mean)

t1_2dpi <-  fig5b %>%
  filter(Treatments == "Avr4", dpi == 2) %>%
  select(Mean)

t2_2dpi <-  fig5b %>%
  filter(Treatments == "Cf4", dpi == 2) %>%
  select(Mean)

c_5dpi <- fig5b %>%
  filter(Treatments == "Cf4+Avr4", dpi == 5) %>%
  select(Mean)

t1_5dpi <-  fig5b %>%
  filter(Treatments == "Avr4", dpi == 5) %>%
  select(Mean)

t2_5dpi <-  fig5b %>%
  filter(Treatments == "Cf4", dpi == 5) %>%
  select(Mean)

# significant. p.value 0.001. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c_2dpi), as.vector(t1_2dpi))

# significant. p.value 0.009. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c_2dpi), as.vector(t2_2dpi))

# significant. p.value 0.0002. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c_5dpi), as.vector(t1_5dpi))

# significant. p.value 0.0006. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c_5dpi), as.vector(t2_5dpi))

p.value <-
  data.frame(
    Comparison = rep(c("Control-Avr4", "Control-Cf4"), 2),
    DPI = rep(c(2, 5), each = 2),
    p.value = c(0.001, 0.009, 0.0002, 0.0006)
  )

write_csv(p.value, "../results/pvalues_figure5b.csv")


# figure 6B ---------------------------------------------------------------


fig6b <-
  read_excel(
    "../data/supp_tables_dataset_AgroLux_paper_v5.xlsx",
    sheet = 12,
    skip = 7,
    n_max = 49
  ) %>%
  select(-Mean) %>%
  rename(Mean = `Mean – Background**`)

fig6b %>%
  mutate(Mean=Mean/100) %>% 
  group_by(Treatments, dpi) %>%
  summarise(
    N = n(),
    sd = sd(Mean),
    se = sd / sqrt(N),
    Mean = mean(Mean)
  ) %>%
  ggplot(aes(Treatments, Mean)) +
  geom_bar(
    stat = "identity",
    width = 0.6,
    color = "black",
    fill = "red",
    alpha = 0.5
  ) +
  geom_errorbar(aes(ymin = Mean - se, ymax = Mean + se), width = 0.5) +
  geom_jitter(
    data = fig6b,
    aes(Treatments, Mean/100),
    size = 3,
    alpha = 0.6,
    width = 0.1
  ) +
  theme_Publication(base_size = 12) +
  ylab("Luminescence (RLU X 100)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap( ~ dpi, ncol = 2)

ggsave(
  "../results/fig6b.pdf",
  width = 10,
  height = 12,
  units = "cm"
)

ggsave(
  "../results/fig6b.svg",
  width = 10,
  height = 12,
  units = "cm"
)


# Groups for statistical significance

c1_2dpi <- fig6b %>%
  filter(Treatments == "Cf4/Avr4", dpi == 2) %>%
  select(Mean)

c2_2dpi <- fig6b %>%
  filter(Treatments == "Cf9/Avr9", dpi == 2) %>%
  select(Mean)

t1_2dpi <-  fig6b %>%
  filter(Treatments == "Cf9/Avr4", dpi == 2) %>%
  select(Mean)

t2_2dpi <-  fig6b %>%
  filter(Treatments == "Cf4/Avr9", dpi == 2) %>%
  select(Mean)


c1_5dpi <- fig6b %>%
  filter(Treatments == "Cf4/Avr4", dpi == 5) %>%
  select(Mean)

c2_5dpi <- fig6b %>%
  filter(Treatments == "Cf9/Avr9", dpi == 5) %>%
  select(Mean)

t1_5dpi <-  fig6b %>%
  filter(Treatments == "Cf9/Avr4", dpi == 5) %>%
  select(Mean)

t2_5dpi <-  fig6b %>%
  filter(Treatments == "Cf4/Avr9", dpi == 5) %>%
  select(Mean)


# Comparisison Cf4/Avr4-Cf4/Avr9 2dpi. significant. p.value 6.211e-09. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c1_2dpi), as.vector(t2_2dpi))

# Comparisison Cf9/Avr9-Cf9/Avr4 2dpi. significant. p.value 3.782e-07. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c2_2dpi), as.vector(t1_2dpi))

# Comparisison Cf4/Avr4-Cf4/Avr9 5dpi. significant. p.value 0.0004221. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c1_5dpi), as.vector(t2_5dpi))

# Comparisison Cf9/Avr9-Cf9/Avr4 5dpi. significant. p.value 0.0003414. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c2_5dpi), as.vector(t1_5dpi))


p.value <-
  data.frame(
    Comparison = rep(c(
      "Cf4/Avr4-Cf4/Avr9", "Cf9/Avr9-Cf9/Avr4"
    ), 2),
    DPI = rep(c(2, 5), each = 2),
    p.value = c(6.211e-09, 3.782e-07, 0.0004221, 0.0003414)
  )


write_csv(p.value, "../results/pvalues_figure6b.csv")



# figure 6c ---------------------------------------------------------------

fig6c <-
  read_excel(
    "../data/supp_tables_dataset_AgroLux_paper_v5.xlsx",
    sheet = 13,
    skip = 7,
    n_max = 49
  ) %>%
  select(-Mean) %>%
  rename(Mean = `Mean – Background**`)

fig6c %>%
  mutate(Mean=Mean/1000) %>% 
  group_by(Treatments, dpi) %>%
  summarise(
    N = n(),
    sd = sd(Mean),
    se = sd / sqrt(N),
    Mean = mean(Mean)
  ) %>%
  ggplot(aes(Treatments, Mean)) +
  geom_bar(
    stat = "identity",
    width = 0.6,
    color = "black",
    fill = "red",
    alpha = 0.5
  ) +
  geom_errorbar(aes(ymin = Mean - se, ymax = Mean + se), width = 0.5) +
  geom_jitter(
    data = fig6c,
    aes(Treatments, Mean/1000),
    size = 3,
    alpha = 0.6,
    width = 0.1
  ) +
  theme_Publication(base_size = 12) +
  ylab("Fluorescence (RLU X 1000)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap( ~ dpi, ncol = 2)


ggsave(
  "../results/fig6c.pdf",
  width = 10,
  height = 12,
  units = "cm"
)

ggsave(
  "../results/fig6c.svg",
  width = 10,
  height = 12,
  units = "cm"
)


# Groups for statistical significance

c1_2dpi <- fig6c %>%
  filter(Treatments == "Cf4/Avr4", dpi == 2) %>%
  select(Mean)

c2_2dpi <- fig6c %>%
  filter(Treatments == "Cf9/Avr9", dpi == 2) %>%
  select(Mean)

t1_2dpi <-  fig6c %>%
  filter(Treatments == "Cf9/Avr4", dpi == 2) %>%
  select(Mean)

t2_2dpi <-  fig6c %>%
  filter(Treatments == "Cf4/Avr9", dpi == 2) %>%
  select(Mean)


c1_5dpi <- fig6c %>%
  filter(Treatments == "Cf4/Avr4", dpi == 5) %>%
  select(Mean)

c2_5dpi <- fig6c %>%
  filter(Treatments == "Cf9/Avr9", dpi == 5) %>%
  select(Mean)

t1_5dpi <-  fig6c %>%
  filter(Treatments == "Cf9/Avr4", dpi == 5) %>%
  select(Mean)

t2_5dpi <-  fig6c %>%
  filter(Treatments == "Cf4/Avr9", dpi == 5) %>%
  select(Mean)


# Comparisison Cf4/Avr4-Cf4/Avr9 2dpi. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c1_2dpi), as.vector(t2_2dpi))

# Comparisison Cf9/Avr9-Cf9/Avr4 2dpi. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c2_2dpi), as.vector(t1_2dpi))

# Comparisison Cf4/Avr4-Cf4/Avr9 5dpi. If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c1_5dpi), as.vector(t2_5dpi))

# Comparisison Cf9/Avr9-Cf9/Avr4 5dpi.  If we correct with Bonferroni, the critical p.value would be 0.025

t.test(as.vector(c2_5dpi), as.vector(t1_5dpi))

p.value <-
  data.frame(
    Comparison = rep(c(
      "Cf4/Avr4-Cf4/Avr9", "Cf9/Avr9-Cf9/Avr4"
    ), 2),
    DPI = rep(c(2, 5), each = 2),
    p.value = c( 0.03105,  0.0005889,  1.336e-06,  2.191e-07)
  )


write_csv(p.value, "../results/pvalues_fig6c.csv")



# figure 7b ---------------------------------------------------------------


fig7b <-
  read_excel(
    "../data/supp_tables_dataset_AgroLux_paper_v3.xlsx",
    sheet = 14,
    skip = 7,
    n_max = 49
  ) %>%
  select(-Mean) %>%
  rename(Mean = `Mean – Background**`)

fig7b %>%
  group_by(Treatments, dpi) %>%
  summarise(
    N = n(),
    sd = sd(Mean),
    se = sd / sqrt(N),
    Mean = mean(Mean)
  ) %>%
  ggplot(aes(Treatments, Mean)) +
  geom_bar(
    stat = "identity",
    width = 0.6,
    color = "black",
    fill = "red",
    alpha = 0.5
  ) +
  geom_errorbar(aes(ymin = Mean - se, ymax = Mean + se), width = 0.5) +
  geom_jitter(
    data = fig7b,
    aes(Treatments, Mean),
    size = 3,
    alpha = 0.6,
    width = 0.1
  ) +
  theme_Publication(base_size = 12) +
  ylab("Luminescence (RLU)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap( ~ dpi, ncol = 2)

ggsave(
  "../results/fig7b.pdf",
  width = 10,
  height = 12,
  units = "cm"
)

ggsave(
  "../results/fig7b.svg",
  width = 10,
  height = 12,
  units = "cm"
)


# statistics 2dpi

fig7b_2dpi <- fig7b %>% 
  filter(dpi==2)

mod_fig.7b <- aov(Mean~Treatments, data = fig7b_2dpi)

mod_out <- anova(mod_fig.7b)

(posthoc <- TukeyHSD(x=mod_fig.7b , 'Treatments', conf.level=0.95))

capture.output(mod_out,posthoc, file = "../results/stat_fig7b_2dpi.txt")


# statistics 5dpi

fig7b_5dpi <- fig7b %>% 
  filter(dpi==5)

mod_fig.7b <- aov(Mean~Treatments, data = fig7b_5dpi)

mod_out <- anova(mod_fig.7b)

(posthoc <- TukeyHSD(x=mod_fig.7b , 'Treatments', conf.level=0.95))

capture.output(mod_out,posthoc, file = "../results/stat_fig7b_5dpi.txt")

