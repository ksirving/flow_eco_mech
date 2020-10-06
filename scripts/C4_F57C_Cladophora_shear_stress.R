## Cladophora shear stress

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

## 16.9 n m-3 - one event per year (Spring)

## upload hydraulic data

## soft bottom reaches

F57C <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("input_data/HecRas/hydraulic_ts_LA8.csv")
# LA11 <- read.csv("input_data/HecRas/hydraulic_ts_LA11.csv")
# LA20 <- read.csv("input_data/HecRas/hydraulic_ts_LA20_2.csv")

head(F57C)
names(F57C)
## convert lb sq ft to n m-2 = lb sq ft/0.020885

hyd_shear <- F57C[,c(2:4,7,11,15)]

hyd_shear <- hyd_shear %>%
  mutate(shear_pa_LOB = (Shear..lb.sq.ft..LOB/0.020885),
         shear_pa_MC = (Shear..lb.sq.ft..MC/0.020885),
         shear_pa_ROB = (Shear..lb.sq.ft..ROB/0.020885)) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(Q_ts.datetime), 1))

range(hyd_shear$shear_pa_ROB)
