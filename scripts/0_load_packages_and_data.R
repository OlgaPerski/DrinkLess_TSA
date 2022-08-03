# load required packages --------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pkgs <- c("here",
          "tidyverse",
          "lubridate",
          "ggplot2",
          "readxl",
          "gtsummary",
          "AER",
          "mgcv")

pacman::p_load(pkgs, character.only=T)

# load data ---------------------------------------------------------------

df_rq1a <- read_excel(here("data","RQ1_outcome_measures.xlsx"))
df_rq1b <- read_excel(here("data","RQ1_users.xlsx"))
df_rq2 <- read_excel(here("data","RQ2_downloads by download date.xlsx"))
df_rq3a <- read_excel(here("data","RQ3a_s-d and drinking characteristics by download date.xlsx"))
df_rq3be <- read_excel(here("data","RQ3b-e_aggregated by download date.xlsx"))




