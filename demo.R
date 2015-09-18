######################################################
###############   Demonstration file   ###############
####### How to use DendriticSpineR package ###########
######################################################

##
## 1. Load required package
##

library(DendriticSpineR)

##
## 2. Set a folder with working direction
##

# setwd("put here a full path to the folder with files with data")

#example
setwd("H:\\R\\")

#if you want use this demo file, just change folder in setwd() function

##
## 3. Choose a file *.csv or *.xlsx
##

# file <- "put_here_a_name_of_file.csv"
# #or
# file <- "put_here_a_name_of_file.xlsx"

#example
file <- "zebrane.csv"

#if you want use this demo file, just change file variable for your file with data

##
## 4. Choose columns that you are interested about
##

# animal_col_name <- "animal_column_name"
# group_col_name <- "group_column_name"
# spines_col_name <- "spines_number_column_name"
# photo_col_name <- "photo_column_rel_name" #it has default value, so you can omit it
# properties_col_name <- "properties_column_name"
# #or
# properties_col_name <- c("properties_column_1_name", "properties_column_1_name", ...)

#example
animal_col_name <- "Animal"
group_col_name <- "Group"
spines_col_name <- "spine_number"
photo_col_name <- "Photo_ID_rel" #it has default value, so you can omit it
properties_col_name <- "length"
#or
properties_col_name <- c("length","foot","head_width")

##
## 5. Load file with data
##

# spines <- read_spines(file, animal_col_name = "animal_col_name",
#                       group_col_name = "group_col_name",
#                       spines_col_name = "spines_col_name",
#                       photo_col_name = "photo_col_name",
#                       properties_col_name = "properties_col_name",
#                       ... )
#
# photo_col_name has default value, so you can omit it
# ... means other arguments like header, sep, sheet, etc.. Without them
#     you probably will not open a file and you get error

#example
spines <- read_spines(file, animal_col_name = animal_col_name,
                      group_col_name = group_col_name,
                      spines_col_name = spines_col_name,
                      photo_col_name = photo_col_name,
                      properties_col_name = properties_col_name,
                      header = TRUE, sep = ";")

#Remember that in the Group column all elements should have the same pattern. If they do not
#the "x" will be added to new condition column. For example if there are "ko", "tg", "wt ko",
#"wt tg", you will get new columns: group with "ko", "tg", "wt" (first element of pattern) and
#condition with "ko", "tg", "x" (second element of pattern). So, to avoid this situation, you
#should edit file with data.

##
## 6. Summary of loaded file with data
##

summary(spines)

##
## 7. Drawing a (cumulative) density function plot
##

#a cumulative density function plot

# plot_distributions(spines, property = "put_here_spines_property",
#   ecdf = TRUE, x_lim = c(0, 2))

#example
plot_distributions(spines, property = "length")
plot_distributions(spines, property = "length", ecdf = TRUE, x_lim = c(0, 3))

#a density function plot

# plot_distributions(spines, property = "put_here_spines_property,
#   ecdf = FALSE, x_lim = c(0, 2))

#example
plot_distributions(spines, property = "length", ecdf = FALSE)
plot_distributions(spines, property = "length", ecdf = FALSE, x_lim = c(0, 3))

##
## 8. Drawing a plot of animals
##

#a panel plot with densities for one property

# plot_animals(spines, property = "put_here_spines_property", box = FALSE)

#example
plot_animals(spines)
plot_animals(spines, property = "length", box = FALSE)

#a panel plot with boxplots for one property

# plot_animals(spines, property = "put_here_spines_property", box = TRUE)

#example
plot_animals(spines, property = "length", box = TRUE)

##
## 9. Drawing crossed effects plot
##

# plot_crossed_effects(spines, property = "put_here_spines_property",
#                      trans = put_here_transformation,
#                      inv = put_here_inverse_transformation,
#                      strat = "put_here_stratification",
#                      mixed = TRUE, addpoints = FALSE)

# You should not worry about transformations, because if you do not
# choose both transformations which should be used during working with
# data, the function will match them to chosen property by itself.

#example
plot_crossed_effects(spines, property = "length",
                     trans = log, inv = exp,
                     strat = "Animal",
                     mixed = FALSE)

plot_crossed_effects(spines, property = "length",
                     trans = log, inv = exp,
                     strat = "Animal:group",
                     mixed = TRUE)

plot_crossed_effects(spines, property = "length",
                     strat = "Animal",
                     mixed = TRUE, addpoints = TRUE)

##
## 10. Drawing diffogram
##

# ms <- model_spines(spines, property = "put_here_spines_property", trans = put_here_transformation,
#    photo_col_name = "photo_column_rel_name")
# diffogram(ms, logt = FALSE)

# You should not worry about transformation, because if you do not
# choose transformation which should be used during working with
# data, the function will match it to chosen property by itself.

#example
ms <- model_spines(spines, photo_col_name = "Photo_ID_rel")
diffogram(ms)
