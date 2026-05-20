rm(list=ls())
library(ComplexUpset)
library(tidyverse)
library(ggbreak)
library(ggplot2)
library(dplyr)

df_raw <- read_csv("symptom.csv")


head(df_raw)
df <- df_raw %>%
  dplyr::select(-1) %>%                 
  rename(
    Asymptomatic = `asymptomatic`,
    WeightLoss   = `weight loss`,
    Sweats       = sweats,
    Fever        = fever,
    Hemoptysis   = hemoptysis,
    ChestPain    = `chest pain`,
    Cough     = cough,
  )


glimpse(df)

p <- upset(
  df,
  colnames(df),
  name = "Symptom",
  sort_intersections=FALSE,  
  sort_sets=FALSE,
  intersections = list(
    'Asymptomatic',
    'Cough',
    'ChestPain',
    'Hemoptysis',
    'Fever',
    c('Cough','ChestPain'),
    c('Cough','Hemoptysis'),
    c('Cough','Fever'),
    c('ChestPain','Fever'),
    c('Cough','Sweats'),
    c('Cough','WeightLoss'),
    c('ChestPain','WeightLoss'),
    c('ChestPain','Sweats'),
    c('Sweats','WeightLoss'),
    c('Cough','ChestPain','Fever'),
    c('Cough','ChestPain','Hemoptysis'),
    c('Cough','Hemoptysis','Sweats'),
    c('Cough','ChestPain','Sweats'),
    c('Cough','Hemoptysis','Fever'),
    c('Cough','Fever','WeightLoss'),
    c('Cough','Fever','Sweats'),
    c('Cough','Hemoptysis','WeightLoss'),
    c('Cough','ChestPain','WeightLoss'),
    c('Cough','ChestPain','Hemoptysis','Fever'),
    c('Cough','Hemoptysis','Sweats','WeightLoss'),
    c('Cough','Hemoptysis','Fever','Sweats')
    #'Outside of known sets'
  ),
  #min_size = 1,                       
  width_ratio = 0.2,                
  height_ratio = 0.7,              
  base_annotations = list(
    'Intersection size' = (
      intersection_size(
        text_colors = c(
          on_background = 'brown',
          on_bar = 'yellow'
        )
      ) +
        annotate(
          geom = 'text', x = Inf, y = Inf,
          label = paste('Total:', nrow(df)),
          vjust = 1, hjust = 1
        ) +
        ylab('Intersection size')
    
    )
  ),
  queries = list(
    upset_query(
      intersect = "Asymptomatic",
      color = "steelblue",
      fill = "steelblue"
    )
  )
)
p

ggsave("/Users/yll/Documents/scTB/4-结果图片/upset_plot.pdf", width = 10, height = 6)
