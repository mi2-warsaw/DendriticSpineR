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
properties_col_name <- "length" #c("length","foot")

spines <- read_spines(file, animal_col_name=animal_col_name,
                      group_col_name=group_col_name, spines_col_name=spines_col_name,
                      properties_col_name=properties_col_name)

summary(spines)

plot_ecdf(spines, "length", TRUE) +
  coord_cartesian(xlim=c(0,2)) + xlab("length")

plot_ecdf(spines, "length", FALSE) +
  coord_cartesian(xlim=c(0,2)) + xlab("length")

plot_ecdf(spines, c("length","foot"), TRUE) +
  coord_cartesian(xlim=c(0,2)) + xlab("length + foot")

plot_ecdf(spines, c("length","foot"), FALSE) +
  coord_cartesian(xlim=c(0,2)) + xlab("length + foot")

# plot_ecdf(c(spines$length,spines$foot), spines$Group, TRUE) +
#   coord_cartesian(xlim=c(0,2)) + xlab("length")
#
# plot_ecdf(spines$length, spines$Group, FALSE)+
#   coord_cartesian(xlim=c(0,2)) + xlab("length")

plot_crossed_effects(spines, var = "length",
                     trans = log, inv = exp,
                     f1="group", f2="condition", strat = "Animal:group",
                     mixed = TRUE)

# model_crossed_effects(spines, "length",
#                       trans = log, f1="group",
#                       f2="condition", strat = "Animal:group",
#                       mixed = TRUE)

model_mieszany <- lmer(log(length) ~ Group + (1|Animal:group) + (1|Photo_ID_rel:Animal:group), data=spines)

# lsmeans
ms6 <- lsmeans(model_mieszany,  pairwise~Group, adjust="tukey")

diffogram(ms6)
