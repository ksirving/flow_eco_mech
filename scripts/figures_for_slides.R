### figures - flow chart for slides

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)

LA11 <- read.csv("input_data/HecRas/hydraulic_ts_LA11.csv")

## work flow
##  species habitat curve
## rating curve - hydraulic and Q
## rating curve - probability and Q

## depth ranges and Q limits

hydraul <- LA11[,-1]
names(hydraul)
head(hydraul)
## select columns
## change some names
hydraul <- hydraul %>%
  rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow)

# nas <- which(complete.cases(hyd_dep) == FALSE)
# hyd_dep[nas,]

## convert unit from feet to meters 

## change names and transform ft to cm
hyd_dep <- hydraul %>%
  select(c(DateTime, Q, node, Shear..lb.sq.ft..LOB, Hydr..Depth..ft..LOB,Shear..lb.sq.ft..MC, Hydr..Depth..ft..MC, 
           Shear..lb.sq.ft..ROB, Hydr..Depth..ft..ROB)) %>%
  rename(shear_ft_LOB = Shear..lb.sq.ft..LOB, depth_ft_LOB = Hydr..Depth..ft..LOB, shear_ft_MC = Shear..lb.sq.ft..MC,
         depth_ft_MC = Hydr..Depth..ft..MC, shear_ft_ROB = Shear..lb.sq.ft..ROB, depth_ft_ROB = Hydr..Depth..ft..ROB) %>%
  mutate(depth_cm_LOB = (depth_ft_LOB*0.3048)*100,
         depth_cm_MC = (depth_ft_MC*0.3048)*100,
         depth_cm_ROB = (depth_ft_ROB*0.3048)*100) %>%
  # mutate(shear_pa_LOB = (shear_ft_LOB/0.020885),
  #        shear_pa_MC = (shear_ft_MC/0.020885),
  #        shear_pa_ROB = (shear_ft_ROB/0.020885)) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

head(hyd_dep)

## depth v Q rating curve 
hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num"))

hyd_dep <- hyd_dep %>% 
  filter(variable == "depth_cm_LOB") %>%
  rename(depth_cm = value)

labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")
png("figures/Application_curves/nodes/LA11_Depth_Q_main_channel.png", width = 800, height = 800)

ggplot(hyd_dep, aes(x = Q, y=depth_cm)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  # facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(text = element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Glendale Narrows (LA11): Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


### Prob v Q
## Typha adult patch 
load("/Users/katieirving/Documents/git/typha_model_application/input_data/typha_depth.RData")

### model

# head(depth)
# mean(na.omit(depth$depth_cm))

summary(dep_ptch_mdl <- lm(occurrence ~ depth_cm + I(depth_cm^2), data = depth))

new_data <- hyd_dep %>%
  mutate(prob_fit = predict(dep_ptch_mdl, newdata = hyd_dep, type="response")) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

range(new_data$prob_fit)

png("figures/Application_curves/Depth/LA11_adult_patch_depth_prob_Q_thresholds_main_channel.png", width = 800, height = 800)
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"))
  theme(text = element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Glendale Narrows (LA11): Probability ~ Q",
       y = "Probability of Occurrence",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

## depth and flow limits

head(new_data)
## high probability

load(file="root_interpolation_function.Rdata")


q_limit <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.75)
q_limit[1]

depth_limit <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 0.75)
depth_limit


### figures

png("figures/Application_curves/nodes/LA11_Depth_Q_main_channel_depth_limits.png", width = 800, height = 800)

ggplot(hyd_dep, aes(x = Q, y=depth_cm)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  # facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  geom_point(aes(y=depth_limit[1], x=q_limit[1]), color = "blue", size = 5) +
  geom_point(aes(y=depth_limit[2], x=q_limit[2]), color = "blue", size = 5) +
  theme(text = element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Glendale Narrows (LA11): Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

png("figures/Application_curves/Depth/LA11_adult_patch_depth_prob_Q_thresholds_main_channel_Q_limits.png", width =800, height = 800)
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"))
  geom_point(aes(y=0.75, x=q_limit[1]), color = "blue", size = 5) +
  geom_point(aes(y=0.75, x=q_limit[2]), color = "blue", size = 5) +
  theme(text = element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Glendale Narrows (LA11): Probability ~ Q",
       y = "Probability of Occurrence",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

Q_df <- as.data.frame(matrix(q_limit))
Q_df <- Q_df %>%
  rename(Q = V1)
D_df <- as.data.frame(matrix(depth_limit))
D_df <- D_df %>%
  rename(Depth = V1)

df <- cbind(Q_df, D_df)
df

write.csv(df, "results/limits/LA11_typha_adult_patch_Q_depth_limits.csv")

### LA20_2

LA20 <- read.csv("input_data/HecRas/hydraulic_ts_LA20_2.csv")

## work flow
##  species habitat curve
## rating curve - hydraulic and Q
## rating curve - probability and Q

## depth ranges and Q limits

hydraul <- LA20[,-1]
names(hydraul)
head(hydraul)
## select columns
## change some names
hydraul <- hydraul %>%
  rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow)

# nas <- which(complete.cases(hyd_dep) == FALSE)
# hyd_dep[nas,]

## convert unit from feet to meters 

## change names and transform ft to cm
hyd_dep <- hydraul %>%
  select(c(DateTime, Q, node, Shear..lb.sq.ft..LOB, Hydr..Depth..ft..LOB,Shear..lb.sq.ft..MC, Hydr..Depth..ft..MC, 
           Shear..lb.sq.ft..ROB, Hydr..Depth..ft..ROB)) %>%
  rename(shear_ft_LOB = Shear..lb.sq.ft..LOB, depth_ft_LOB = Hydr..Depth..ft..LOB, shear_ft_MC = Shear..lb.sq.ft..MC,
         depth_ft_MC = Hydr..Depth..ft..MC, shear_ft_ROB = Shear..lb.sq.ft..ROB, depth_ft_ROB = Hydr..Depth..ft..ROB) %>%
  mutate(depth_cm_LOB = (depth_ft_LOB*0.3048)*100,
         depth_cm_MC = (depth_ft_MC*0.3048)*100,
         depth_cm_ROB = (depth_ft_ROB*0.3048)*100) %>%
  # mutate(shear_pa_LOB = (shear_ft_LOB/0.020885),
  #        shear_pa_MC = (shear_ft_MC/0.020885),
  #        shear_pa_ROB = (shear_ft_ROB/0.020885)) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

head(hyd_dep)

## depth v Q rating curve 
hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num"))

hyd_dep <- hyd_dep %>% 
  filter(variable == "depth_cm_LOB") %>%
  rename(depth_cm = value)

labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")
png("figures/Application_curves/nodes/LA20_Depth_Q_main_channel.png", width = 800, height = 800)

ggplot(hyd_dep, aes(x = Q, y=depth_cm)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  # facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(text = element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Sepulveda (LA20): Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


### Prob v Q
## Typha adult patch 
load("/Users/katieirving/Documents/git/typha_model_application/input_data/typha_depth.RData")

### model

# head(depth)
# mean(na.omit(depth$depth_cm))

summary(dep_ptch_mdl <- lm(occurrence ~ depth_cm + I(depth_cm^2), data = depth))

new_data <- hyd_dep %>%
  mutate(prob_fit = predict(dep_ptch_mdl, newdata = hyd_dep, type="response")) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

range(new_data$prob_fit)

png("figures/Application_curves/Depth/LA20_adult_patch_depth_prob_Q_thresholds_main_channel.png", width = 800, height = 800)
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"))
  theme(text = element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Sepulveda (LA20): Probability ~ Q",
       y = "Probability of Occurrence",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

## depth and flow limits

head(new_data)
## high probability

load(file="root_interpolation_function.Rdata")


q_limit <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.75)
q_limit[1]

depth_limit <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 0.75)
depth_limit


### figures

png("figures/Application_curves/nodes/LA20_Depth_Q_main_channel_depth_limits.png", width = 800, height = 800)

ggplot(hyd_dep, aes(x = Q, y=depth_cm)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  # facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  geom_point(aes(y=depth_limit[1], x=q_limit[1]), color = "blue", size = 5) +
  geom_point(aes(y=depth_limit[2], x=q_limit[2]), color = "blue", size = 5) +
  theme(text = element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Sepulveda (LA20): Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

png("figures/Application_curves/Depth/LA20_adult_patch_depth_prob_Q_thresholds_main_channel_Q_limits.png", width =800, height = 800)
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"))
  geom_point(aes(y=0.75, x=q_limit[1]), color = "blue", size = 5) +
  geom_point(aes(y=0.75, x=q_limit[2]), color = "blue", size = 5) +
  theme(text = element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Sepulveda (LA20): Probability ~ Q",
       y = "Probability of Occurrence",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

Q_df <- as.data.frame(matrix(q_limit))
Q_df <- Q_df %>%
  rename(Q = V1)
D_df <- as.data.frame(matrix(depth_limit))
D_df <- D_df %>%
  rename(Depth = V1)

df <- cbind(Q_df, D_df)
df

write.csv(df, "results/limits/LA20_typha_adult_patch_Q_depth_limits.csv")


# Q limits ----------------------------------------------------------------

## depth, velocity & Q ranges for typha, willow & migration

## upload data

## depth >18cm
mig_depth_LA11 <- read.csv("output_data/F5_LA11_migration_depth_Q_limits.csv")
mig_depth_LA20 <- read.csv("output_data/F5_LA20_2_migration_depth_Q_limits.csv")
mig_depth_LA20
## velocity <3.1
mig_velocity_LA11 <- read.csv("output_data/F5_LA11_migration_velocity_Q_limits.csv")
mig_velocity_LA20 <- read.csv("output_data/F5_LA20_2_migration_velocity_Q_limits.csv")

## willow

willow_germ_depth_LA20 <- read.csv("/Users/katieirving/Documents/git/willow_model_application/output_data/W4_LA20_2_willow_germ_depth_Q_limits.csv")
willow_germ_depth_LA11 <- read.csv("/Users/katieirving/Documents/git/willow_model_application/output_data/W4_LA11_willow_germ_depth_Q_limits.csv")
willow_germ_depth_LA11

willow_seed_depth_LA11 <- read.csv("/Users/katieirving/Documents/git/willow_model_application/output_data/W1_LA11_seedling_depth_Q_limits.csv")
willow_seed_depth_LA20 <- read.csv("/Users/katieirving/Documents/git/willow_model_application/output_data/W1_LA20_2_seedling_depth_Q_limits.csv")

## typha

typha_seed_depth_LA20 <- read.csv("/Users/katieirving/Documents/git/typha_model_application/output_data/M3_LA20_2_Typha_seedling_Depth_Q_limits.csv")
typha_seed_depth_LA11 <- read.csv("/Users/katieirving/Documents/git/typha_model_application/output_data/M3_LA11_Typha_seedling_Depth_Q_limits.csv")
typha_seed_depth_LA20 

typha_seed_velocity_LA11 <- read.csv("/Users/katieirving/Documents/git/typha_model_application/output_data/M1_LA11_adult_patch_velocity_Q_limits.csv")
typha_seed_velocity_LA20 <- read.csv("/Users/katieirving/Documents/git/typha_model_application/output_data/M1_LA20_2_adult_patch_velocity_Q_limits.csv")
typha_seed_velocity_LA11
typha_adult_depth_LA20 <- read.csv("results/limits/LA20_typha_adult_patch_Q_depth_limits.csv")
typha_adult_depth_LA11 <- read.csv("results/limits/LA11_typha_adult_patch_Q_depth_limits.csv")

typha_adult_depth_LA20
typha_adult_depth_LA11


df <- read.csv("input_data/depth_flow_ex_species.csv")
df <- df %>%
  pivot_wider(names_from = "Node", values_from = "Flow..cfs.")
df
write.csv(df, "output_data/depth_flow_ex_wide.csv")
