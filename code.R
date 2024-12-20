
library(tidyverse)  # Pour la manipulation des données
library(haven)      # pour l'importation des données du format SPSS dans R
library(labelled)   # Pour les labelles des variables
library(expss)      # Pour le formatage des tableaux dans R
library(xlsx)       # pour exporter les données dans Excel
library(naniar)     # pour remplacer les données vides avec la fonction replace_with_na 
library(here)       # Pour le chemin des données
library(survey)     # Pour la manipulation des données
library(srvyr)      # pour la manipulation des données pondérées avec le style dplyr
library(rio)        # Pour l'importation des données
library(gtsummary)
library(gt)
library(janitor)



hl <- import(here("data/mics/hl.sav")) |> clean_names() |> as_tibble() # Importation de la base hl

hl <- hl |>
  mutate(
    red = case_when(
      (ed16a == ed10a) & (ed16b == ed10b) & !is.na(ed16a) ~ 1,
      !is.na(ed16a)   ~ 0
    ),
    drop = case_when(
      ed9 != 1 ~ 1,
      .default = 0
    )
  )


hl_design <- hl %>% 
  as_survey_design(weights = hhweight)


TR <- hl_design |> 
  group_by(ed16a,ed16b) |> 
  summarise(TR = survey_mean(red,na.rm = TRUE)*100)

PR <- hl_design |> 
  group_by(ed16a,ed16b) |> 
  summarise(PR = survey_mean(red,na.rm = TRUE)*100)

hl_design |> 
  group_by(ed16a) |> 
  summarise(ens1 = survey_mean(red,na.rm = TRUE)*100)


hl_design |> 
  group_by(ed16a,ed16b) |> 
  summarise(survey_mean(drop,na.rm = TRUE)*100)


wm <- import(here("data/mics/wm.sav")) |> 
  clean_names() |> 
  as_tibble() |>
  filter(wm17 == 1) |> 
  select(hh1,hh2,ln,wb14,wagem, mt6a:mt6i, mt4, mt5, welevel, wmweight,ma1, ma5)
# Importation de la base wm
mn <- import(here("data/mics/mn.sav")) |>
  clean_names() |>
  as_tibble() |> 
  filter(mwm17 == 1 ) |>
  select(hh1, hh2, ln, mwb14, mwagem, mmt6a:mmt6i, mmt4, mmt5, mwelevel, mnweight, mma1, mma5)
  # Importation de la base mn


base <- wm |> 
  left_join(mn)

base <- base |> 
  mutate(
    literate = case_when(
      MWB14 == 1| MWB14 == 2 | WB14 == 1 | WB14 ==2 ~ 0,
      MWB14==3 | WB14 == 3 ~ 1,
      welevel >= 2 & welevel <= 5 ~ 1,
      mwelevel >= 2 & mwelevel <= 5 ~ 1)
  )

base_design <- base %>% 
  as_survey_design(weights = hhweight)





