
# 1. Basic Packages -------------------------------------------------------

# Note that packages not indispensable to execute R scripts are also included.

if (!require("pacman")) install.packages("pacman")

packages <- c("tidyverse",  "zeallot",  "car", "here",
              "data.table",  "modelsummary", "gt","knitr",  "kableExtra",  "gridExtra",
              "gtsummary","broom", "ggpubr","broom.helpers" ,
              "texreg",   "parallel", "GGally","corrplot","DataExplorer", "skimr",
              "maps","mapdata",   "lubridate",  "furrr", "future",
              "mediation" , "patchwork",  "psych",  "AER", "ivreg", "tools",
              "purrr", "tibble","osrm", "devtools", 'remotes', 
              "MatchIt", "cobalt",   "slider",    "coefplot", "dotwhisker",  "sensemakr",  
              "foreach", "doParallel",   "Matrix", "stringr","splines","marginaleffects",
              "extrafont",  "ggh4x","zoo", "scales",  "np", "plm", "locfit",
              "transport", "dgof","twosamples", "cramer", "T4transport","tidycensus", "sf",
              "did", "fixest","rgl","DIDmultiplegt" ,"DIDmultiplegtDYN",   "did2s",
              "bacondecomp",  "TwoWayFEWeights","panelView", "fect",  "PanelMatch")

pacman::p_load(char = packages) 


# 2 DCDH --------------------------------------------------------------

## 2.1 Check the latest ver. -----------------------------------------------

# update.packages("DIDmultiplegtDYN")
# packageVersion("DIDmultiplegtDYN")

# When you need to update
# remove.packages("DIDmultiplegtDYN")

# You might have to update previous packages since they are inter-related
# detach("package:DIDmultiplegt", unload = TRUE)
# detach("package:DIDmultiplegtDYN", unload = TRUE)


if (!require("DIDmultiplegtDYN")) devtools::install_github("chaisemartinPackages/did_multiplegt_dyn/R")
# devtools::install_version("DIDmultiplegtDYN", version = "2.0.0")
library(DIDmultiplegtDYN)


# for BJS imputation model
# original ver is Stata

# if (!require("didImputation")) devtools::install_github("CdfInnovLab/didImputation")
# library(didImputation)


# 3. Other Packages -------------------------------------------------------

if (!require("staggered"))devtools::install_github("jonathandroth/staggered")
library(staggered)

if (!require("DIDmultiplegtSTAT")) install_github("chaisemartinPackages/did_multiplegt_stat/R", force = TRUE)
library(DIDmultiplegtSTAT)

if (!require("HonestDiD")) remotes::install_github("asheshrambachan/HonestDiD")
library(HonestDiD)

if (!require("HonestDiDFEct")) devtools::install_github("lzy318/HonestDiDFEct")
library(HonestDiDFEct)

if (!require("TestMechs")) devtools::install_github("jonathandroth/TestMechs")
library(TestMechs)

# When installing TestMechs, the following packages might be corrupt.
# remove.packages("pkgbuild")
if (!require("pkgbuild")) install.packages("pkgbuild")
library("pkgbuild")

if (!require("lpinfer")) devtools::install_github("conroylau/lpinfer")
library("lpinfer")

# For lpinfer, you might also need to update other packages.

if (!require("scales")) install.packages("scales", force = TRUE)
library("scales")

# install.packages(c("parallelly", "rlang", "cli", "Rcpp"),  force = TRUE)
if (!require("parallelly")) install.packages("parallelly")
if (!require("rlang")) install.packages( "rlang")
if (!require("cli")) install.packages("cli")
if (!require("Rcpp")) install.packages("Rcpp")