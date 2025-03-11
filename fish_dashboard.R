library(tidyverse)
library(shiny)
library(ggplot2)

df <- read.csv("SAU EEZ 826 v50-1.csv")
df <- df |> mutate(end_use_type = if_else(end_use_type== "", "Discard", end_use_type))

annual_chart <- function(group_col){
  grouped <- df |> group_by(year, {{group_col}}) |> 
    summarise(total = sum(tonnes)) |> drop_na()
  grouped |> ggplot(aes(x=year, y = total, color={{group_col}})) +
    geom_line()
}

annual_chart(end_use_type)  

species_ratio <- df |> group_by(scientific_name) |>
  summarise(total_ton = sum(tonnes)) |> 
  mutate(ratio = total_ton/sum(total_ton)*100) |> 
  select(-total_ton) |> 
  arrange(desc(ratio)) |> 
  slice(1:10)
sum(species_ratio$ratio)

species_ratio <- species_ratio |> add_row(scientific_name= "Other species", ratio = 100-sum(species_ratio$ratio))

species_ratio |> ggplot() +
  geom_col(aes(x="", y=ratio, fill=scientific_name))

