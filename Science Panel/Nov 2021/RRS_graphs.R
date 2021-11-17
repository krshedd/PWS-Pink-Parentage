rm(list=ls())

library(tidyverse)
library(lubridate)
library(DT)
library(abind)
library(ggExtra)

setwd("H:/Programs_and_protocols/My_R")

(RRS <- read_csv("Science Panel/Nov 2021/RRS.csv"))

# RRS %>% 
#   ggplot(aes(x = BY2, y = RRS, colour = Sex)) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymax = Upper, ymin = Lower)) +
#   facet_grid(Stream ~ .) +
#   geom_hline(linetype = "dotted", yintercept = 1, colour = "black") +
#   theme_bw() +
#   ylab("RRS") +
#   xlab("Brood Year")

RRS %>% 
  mutate(BY2 = as.character(BY2)) %>% 
  ggplot(aes(x = BY2, y = RRS, ymax = Upper, ymin = Lower, fill = Sex)) +
  geom_col(position = position_dodge(), colour = "black") +
  geom_errorbar(position = position_dodge(), colour = "black") +
  facet_grid(Stream ~ Lineage, scales = "free_x") +
  geom_hline(linetype = "dotted", yintercept = 1, colour = "black") +
  theme_bw(base_size = 14) +
  ylab("RRS") +
  xlab("Run Year")

ggsave(filename = "Science Panel/Nov 2021/FIGURE_1-RRS_results_to_date.png", units = "in", width = 8, height = 8)

(RRS_odd <- read_csv("RRS_odd.csv"))

RRS_odd %>% 
  ggplot(aes(x = BY, y = RRS, ymax = Upper, ymin = Lower, fill = Sex)) +
  geom_col(position = position_dodge(), colour = "black") +
  geom_errorbar(position = position_dodge(), colour = "black") +
  facet_grid(Stream ~ .) +
  geom_hline(linetype = "dotted", yintercept = 1, colour = "black") +
  theme_bw() +
  ylab("RRS") +
  xlab("Brood Year")

(RRS_even <- read_csv("RRS_even.csv"))

RRS_even %>% 
  ggplot(aes(x = BY, y = RRS, ymax = Upper, ymin = Lower, fill = Sex)) +
  geom_col(position = position_dodge(), colour = "black") +
  geom_errorbar(position = position_dodge(), colour = "black") +
  facet_grid(Stream ~ .) +
  geom_hline(linetype = "dotted", yintercept = 1, colour = "black") +
  theme_bw() +
  ylab("RRS") +
  xlab("Brood Year")

(gRRS <- read_csv("gRRS.csv"))

gRRS %>% 
  ggplot(aes(x = BY, y = RRS, ymax = Upper, ymin = Lower)) +
  geom_col(position = position_dodge(), colour = "black", fill = "LightGrey") +
  geom_errorbar(position = position_dodge(), colour = "black") +
  facet_grid(Stream ~ Lineage) +
  geom_hline(linetype = "dotted", yintercept = 1, colour = "black") +
  theme_bw() +
  ylab("RRS") +
  xlab("Brood Year")