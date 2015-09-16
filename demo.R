######################################################
###############   Demonstration file   ###############
####### How to use DendriticSpineR package ###########
######################################################

## 1. Load required packages

library(DendriticSpineR)
library(ggplot2)
library(scales)

## 2. Set a folder with working direction

setwd("put here a full path to the folder with files with data")

#example
#setwd("H:\\R\\")

## 3. Choose a file *.csv or *.xlsx

file <- "put_here_a_name_of_file.csv"
#or
file <- "put_here_a_name_of_file.xlsx"

#example
#file <- "zebrane.csv"

## 4. Choose columns that you are interested about


animal_col_name <- "animal_column_name"
group_col_name <- "group_column_name"
spines_col_name <- "spines_number_column_name"
photo_col_name <- "photo_column_1_name" #it has default value, so you can omit it
#or
photo_col_name <- c("photo_column_1_name", "photo_column_2_name") #it has default value, so you can omit it
properties_col_name <- "properties_column_name"
#or
properties_col_name <- c("properties_column_1_name", "properties_column_1_name", ...)

#example
# animal_col_name <- "Animal"
# group_col_name <- "Group"
# spines_col_name <- "spine_number"
# photo_col_name <- "Photo_ID_abs" #it has default value, so you can omit it
##or
# photo_col_name <- c("Photo_ID_abs", "Photo_ID_rel") #it has default value, so you can omit it
# properties_col_name <- "length"
##or
# properties_col_name <- c("length","foot","head_width")

## 5. Load file with data

spines <- read_spines(file, animal_col_name = animal_col_name,
                      group_col_name = group_col_name,
                      spines_col_name = spines_col_name,
                      photo_col_name = photo_col_name,
                      properties_col_name = properties_col_name)

#Remember that in the Group column all elements should have the same pattern. If they do not
#the "x" will be added to new condition column. For example if there are "ko", "tg", "wt ko",
#"wt tg", you will get new columns: group with "ko", "tg", "wt" (first element of pattern) and
#condition with "ko", "tg", "x" (second element of pattern). So, to avoid this situation, you
#should edit file with data.

## 6. Summary of loaded file with data

summary(spines)

## 7. Drawing a (cumulative) density function plot

#a cumulative density function plot

plot_ecdf(spines, "put_here_spines_properties", TRUE, c(0, 2))

#example
# plot_ecdf(spines, "length")
# plot_ecdf(spines, c("length","foot","head_width"), TRUE, c(0, 3))

#a density function plot

plot_ecdf(spines, "put_here_spines_properties", FALSE, c(0, 2))

#example
# plot_ecdf(spines, "length", FALSE, c(0, 3))
# plot_ecdf(spines, c("length","foot","head_width"), FALSE, c(0, 3))




