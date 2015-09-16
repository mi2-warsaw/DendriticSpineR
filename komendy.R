library(roxygen2)
devtools::check() #cleanup = FALSE
devtools::document()
devtools::build()
devtools::install()

library(dplyr)
library(ggplot2)
library(lme4)
library(lsmeans)
library(openxlsx)
library(scales)
library(stringi)
library(tidyr)
library(pbkrtest)

library(DendriticSpineR)

setwd("H:\\R\\dendryty\\przyklady danych i raportow")

file <- "zebrane.csv"
file <- "MMP-9 KO&TG.xlsx"

animal_col_name <- "Animal"
group_col_name <- "Group"
spines_col_name <- "spine_number"
properties_col_name <- "length" #c("length","foot","head_width")

##

spines <- read_spines(file, animal_col_name=animal_col_name,
                      group_col_name=group_col_name, spines_col_name=spines_col_name,
                      properties_col_name=properties_col_name)

##

summary(spines)

##

plot_ecdf(spines, "length", TRUE)

plot_ecdf(spines, "length", FALSE)

plot_ecdf(spines, c("length","foot","head_width"), TRUE)

plot_ecdf(spines, c("length","foot"), FALSE, c(0,3))

##

plot_distributions(spines, "length", box = FALSE)

plot_distributions(spines, "length", box = TRUE)

##

plot_crossed_effects(spines, property = "length",
                     trans = log, inv = exp,
                     strat = "Animal:group",
                     mixed = TRUE)

plot_crossed_effects(spines, property = c("length","foot"),
                     trans = log, inv = exp,
                     strat = "Animal",
                     mixed = FALSE)

##

# model_mieszany <- lmer(log(length) ~ Group + (1|Animal:group) + (1|Photo_ID_rel:Animal:group), data=spines)
# ms6 <- lsmeans(model_mieszany,  pairwise~Group, adjust="tukey")
# diffogram(ms6)

col_names <- colnames(spines)
formula_model <- as.formula(paste0("log(", "length", ") ~ ", col_names[2], " + (1|",
                                   col_names[1], ":group) + (1|", tail(photo_col_name, 1),
                                   ":", col_names[1], ":group)"))
mixed_model <- lmer(formula_model, data=spines)
ms <- lsmeans(mixed_model, pairwise~Group, adjust="tukey")
diffogram(ms)
