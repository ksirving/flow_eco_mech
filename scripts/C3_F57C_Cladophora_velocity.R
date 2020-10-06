## cladophora


library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)
library(mgcv)
set.seed(435)
## 1km reach at 0.25m grids
## presence = 0.68 mean sd = 0.19
## absence = 0.41 mean, sd = 0.2
## n = 1000/0.25 = 4000

## 1km reach at 1m grids
## presence = 0.7 mean sd = 0.2
## absence = 0.44 mean, sd = 0.2
## n = 1000/1 = 1000
?rnorm
pres_velo25 <- rnorm(4000, 0.68, 0.19 )
abs_velo25 <- rnorm(4000, 0.41, 0.2 )

t.test(pres_velo25, abs_velo25)

dfp <- as.data.frame(pres_velo25) %>%
 rename(Velocity = pres_velo25) %>%
  mutate(Occurrence = 1, Species = "Cladophora glomerata")

dfa <- as.data.frame(abs_velo25) %>%
  rename(Velocity = abs_velo25) %>%
  mutate(Occurrence = 0, Species = "Cladophora glomerata")
  
df <- rbind(dfp, dfa)
head(df)

summary(glm(Occurrence~Velocity, data=df, family="binomial"))

png("figures/Final_curves/Velocity/C2_Cladophora_velocity_model_lower.png", width = 500, height = 600)


ggplot(data = df, mapping = aes(x = Velocity, y = Occurrence))+
  geom_point(size = 2)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  # scale_y_continuous(trans=log1p_trans()) +
  # scale_y_log10()+
  labs(x = "Velocity m/s", y = "Probability of Occurrence")+
  theme_classic()+
  # scale_y_continuous(limits=c(,100)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20))

dev.off()
