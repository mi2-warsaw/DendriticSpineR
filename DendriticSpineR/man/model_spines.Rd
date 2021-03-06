% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/model_spines.R
\name{model_spines}
\alias{model_spines}
\title{lsmeans calculation}
\usage{
model_spines(spines, property = "length", trans = NA,
   photo_col_name = colnames(spines)[5])
}
\arguments{
\item{spines}{a data.frame of spines class}

\item{property}{a character with property variable; default: "length"}

\item{trans}{a transformation of the property variable before ANOVA; default: NA}

\item{strat}{a stratification (Animal); default: paste0(animal_col_name, ":group")}

\item{photo_col_name}{a name of column with photos; default: colnames(spines)[5]}

\item{inv}{an inverse transformation of the trans(property); default: NA}
}
\value{
a list from lsmeans function
}
\description{
Function \code{model_spines} calculates lsmeans function
on mixed model of spines data.
}

