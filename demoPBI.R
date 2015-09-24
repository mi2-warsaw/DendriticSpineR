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

setwd("~/Dropbox/Projekt Nencki/")

##
## 3. Choose a file *.csv or *.xlsx
##

##
## 4. Choose columns that you are interested about
##

#example
animal_col_name <- "Animal"
group_col_name <- "Group"
spines_col_name <- "spine_number"
photo_col_name <- "Photo_ID_rel" #it has default value, so you can omit it
properties_col_name <- "length"
#or
properties_col_name <- c("length","area","length_area_ratio", "length_width_ratio")

##
## 5. Load file with data
##

#example
file <- "kolce dendrytyczne myszy tg3-gm.csv"

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

plot_distributions(spines, property = "length")
plot_distributions(spines, property = "length", ecdf = TRUE, x_lim = c(0, 3))

#a density function plot

plot_distributions(spines, property = "length", ecdf = FALSE)
plot_distributions(spines, property = "length", ecdf = FALSE, x_lim = c(0, 3))

##
## 8. Drawing a plot of animals
##

plot_animals(spines)
plot_animals(spines, property = "length", box = FALSE)

#a panel plot with boxplots for one property

plot_animals(spines, property = "length", box = TRUE)

##
## 9. Drawing crossed effects plot
##

#example
plot_crossed_effects(spines, property = "length",
                     strat = "Animal",
                     mixed = FALSE)

plot_crossed_effects(spines, property = "length",
                     strat = "Animal",
                     mixed = TRUE, addpoints = TRUE)

##
## 10. Drawing diffogram
##

#example
ms <- model_spines(spines, photo_col_name = "Photo_ID_rel")
diffogram(ms)

##
## 11. Generating raport file
##

generate_raport_file(folder_path = getwd(), file_path = file, animal_col_name = animal_col_name,
                     group_col_name = group_col_name,
                     spines_col_name = spines_col_name,
                     photo_col_name = photo_col_name,
                     properties_col_name = properties_col_name,
                     header = TRUE, sep = ";")


##
## 12. Rendering raport
##

render_raport(file_path = paste0("raport-", Sys.Date(), ".Rmd"),
                                 destination_path = paste0("raport-", Sys.Date(), ".html"))



##
## 11. Generating raport file and rendering it at ones
##

make_raport(folder_path = getwd(), file_path = file, animal_col_name = animal_col_name,
            group_col_name = group_col_name,
            spines_col_name = spines_col_name,
            photo_col_name = photo_col_name,
            properties_col_name = properties_col_name,
            strat = "Animal", mixed = FALSE,
            header = TRUE, sep = ";")
