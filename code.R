
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
hl <- import(here("data/mics/hl.sav")) |> as_tibble() # Importation de la base hl
#wm <- import(here("data/wm.sav")) |> as_tibble() # Importation de la base wm
#mn <- import(here("data/mn.sav")) |> as_tibble() # Importation de la base mn
#fs <- import(here("data/fs.sav")) |> as_tibble() # Importation de la base fs
#ch <- import(here("data/ch.sav")) |> as_tibble() # Importation de la base ch

hl <- hl |> 
  mutate(
    NE1 = ifelse(ED10A == 1 & ED10B == 1 , 1,0),
    PA1 = if_else(schage == 6,1,0),
    NE2 = ifelse(ED10A == 1 & ED10B == 2 & ED16A == 1 & ED16B  == 1, 1,0),
    PA2 = if_else(schage == 7,1,0),
    NE3 = ifelse(ED10A == 1 & ED10B == 3  & ED16A == 1 & ED16B  == 2 , 1,0),
    PA3 = if_else(schage == 8,1,0),
    NE4 = ifelse(ED10A == 1 & ED10B == 4  & ED16A == 1 & ED16B  == 3, 1,0),
    PA4 = if_else(schage == 9,1,0),
    NE5 = ifelse(ED10A == 1 & ED10B == 5  & ED16A == 1 & ED16B  == 4, 1,0),
    PA5 = if_else(schage == 10,1,0),
    NE6 = ifelse(ED10A == 1 & ED10B == 6  & ED16A == 1 & ED16B  == 6, 1,0),
    PA6 = if_else(schage == 11,1,0),
    )

hl_design <- hl %>% 
  as_survey_design(weights = hhweight)

hl_design |>
  group_by(HL4) |> 
  summarise(
    TA6 = survey_total(NE6,na.rm = TRUE)/survey_total(PA6,na.rm = TRUE)*100
)



acces <-  function(niveauAn,classeAn,by){
  if (classeAn < 2){
    hl_design <- hl |> 
      mutate(
        NE= ifelse(ED10A == {{niveauAn}} & ED10B == {{classeAn}} , 1,0),
        PA = if_else(schage == {{classeAn}}+5,1,0)
      ) |>
      as_survey_design(weights = hhweight) 
      
  } else{
    hl_design <- hl |> 
      mutate(
        NE = ifelse(ED10A == {{niveauAn}} & ED10B == {{classeAn}} & ED16A == {{niveauAn}} & ED16B  == {{classeAn}}-1, 1,0),
        PA = if_else(schage == {{classeAn}}+5,1,0)) |>
      as_survey_design(weights = hhweight) 
  }
  
 hl_design |>
    group_by({{by}}) |> 
    summarise(
      "TA_{classeAn}" := survey_total(NE,na.rm = TRUE)/survey_total(PA,na.rm = TRUE)*100
    ) |>
   select(-contains("_se"),-1)  
   
  
}



prim_ens = cbind(acces(1,1,0),acces(1,2,0),acces(1,3,0),acces(1,4,0),acces(1,5,0),acces(1,6,0)) |>
  rownames_to_column("var") |> 
  mutate(var = if_else(var == 1,"Nationale",var))
  
prim_genre = cbind(acces(1,1,HL4),acces(1,2,HL4),acces(1,3,HL4),acces(1,4,HL4),acces(1,5,HL4),acces(1,6,HL4)) |> 
  rownames_to_column("var") |> 
  mutate(var = if_else(var == 1,"Masculin","Feminin"))


df <- rbind(prim_ens,prim_genre) 
profil <- data.table::transpose(df,keep.names = "Taux",make.names = "var") |> 
  mutate(Taux = case_when(
    Taux == "TA_1" ~ "CP1",
    Taux == "TA_2" ~ "CP2",
    Taux == "TA_3" ~ "CE1",
    Taux == "TA_4" ~ "CE2",
    Taux == "TA_5" ~ "CM1",
    Taux == "TA_6" ~ "CM2",
  ))
write.xlsx(profil,"profil.xlsx")

indic1 <- hl_design |>
  group_by(HH6) |> 
  summarise(
    TBS_prim = survey_total(ES_prim,na.rm = TRUE)/survey_total(PAS_prim,na.rm = TRUE)*100,
    TNS_prim = survey_total(ESAS_prim,na.rm = TRUE)/survey_total(PAS_prim,na.rm = TRUE)*100,
    TNSA_prim = survey_total(ESASA_prim,na.rm = TRUE)/survey_total(PAS_prim,na.rm = TRUE)*100,
    TBS_sec1 = survey_total(ES_sec1,na.rm = TRUE)/survey_total(PAS_sec1,na.rm = TRUE)*100,
    TNS_sec1 = survey_total(ESAS_sec1,na.rm = TRUE)/survey_total(PAS_sec1,na.rm = TRUE)*100,
    TNSA_sec1 = survey_total(ESASA_sec1,na.rm = TRUE)/survey_total(PAS_sec1,na.rm = TRUE)*100,
    TBS_sec2 = survey_total(ES_sec2,na.rm = TRUE)/survey_total(PAS_sec2,na.rm = TRUE)*100,
    TNS_sec2 = survey_total(ESAS_sec2,na.rm = TRUE)/survey_total(PAS_sec2,na.rm = TRUE)*100,
    TNSA_sec2 = survey_total(ESASA_sec2,na.rm = TRUE)/survey_total(PAS_sec2,na.rm = TRUE)*100) |>
  mutate(class = "urban/rural",levels = HH6) |> 
  select(TBS_prim,TNS_prim,TNSA_prim,TBS_sec1,TNS_sec1,TNSA_sec1,TBS_sec2,TNS_sec2,TNSA_sec2,class,levels)


hl <- hl |>
  mutate(
    
    age_school = case_when(
      schage>= 6 & schage <= 11 ~  1,
      schage>= 12 & schage <= 15 ~ 2,
      schage>= 16 & schage <= 18 ~ 3,
      .default = NA),
    
    PAS_prim = if_else(age_school == 1,1,0),
    PAS_col = if_else(age_school == 2,1,0),
    PAS_lyc = if_else(age_school == 3,1,0),
    
    ES_prim = if_else(ED9 == 1 & ED10A == 1,1,0),
    ES_col = if_else(ED9 == 1 & ED10A == 2,1,0),
    ES_lyc = if_else(ED9 == 1 & ED10A == 3,1,0),
    
    ESAS_prim = if_else(age_school == 1 & ED9 == 1 & ED10A == 1,1,0),
    ESAS_col = if_else(age_school == 2 & ED9 == 1 & ED10A == 2,1,0),
    ESAS_lyc = if_else(age_school == 3 & ED9 == 1 & ED10A == 3,1,0),
    
    ESASA_prim = if_else(age_school == 1 & ED9 == 1 & (ED10A == 1 | ED10A == 2),1,0),
    ESASA_col = if_else(age_school == 2 & ED9 == 1 & (ED10A == 2 | ED10A == 3),1,0),
    ESASA_lyc = if_else(age_school == 3 & ED9 == 1 & (ED10A == 3 | ED10A == 4 | ED10A == 4),1,0),
    
    
    NAR = case_when(
      age_school == 1 & ED9 == 1 & ED10A == 1 ~ 1,
      age_school == 2 & ED9 == 1 & ED10A == 2 ~ 1,
      age_school == 3 & ED9 == 1 & ED10A == 3 ~ 1,
      .default = 0
    )
  )

hl_design <- hl %>% 
  as_survey_design(weights = hhweight)




indic_sco = function(numerateur1,numerateur2,numerateur3,denominateur,niveau,by,class){
  
    hl_design |>
      group_by({{by}}) |> 
      summarise(
        'TBS_{niveau}' := survey_total({{numerateur1}},na.rm = TRUE)/survey_total({{denominateur}},na.rm = TRUE)*100,
        'TNS_{niveau}' := survey_total({{numerateur2}},na.rm = TRUE)/survey_total({{denominateur}},na.rm = TRUE)*100,
        'TNSA_{niveau}' := survey_total({{numerateur3}},na.rm = TRUE)/survey_total({{denominateur}},na.rm = TRUE)*100
      ) |>
      mutate(class = {{class}},levels = {{by}}) |> 
      select(-contains("_se"),-1)  
}


indic10 <- indic_sco(ES_prim,ESAS_prim,ESASA_prim,PAS_prim,"prim",0,"National")
indic11 <- indic_sco(ES_prim,ESAS_prim,ESASA_prim,PAS_prim,"prim",HL4,"Genre")
indic12 <- indic_sco(ES_prim,ESAS_prim,ESASA_prim,PAS_prim,"prim",HH6,"Urbain/Rural")
indic13 <- indic_sco(ES_prim,ESAS_prim,ESASA_prim,PAS_prim,"prim",HH7,"Region")
indic14 <- indic_sco(ES_prim,ESAS_prim,ESASA_prim,PAS_prim,"prim",windex5,"Richesse")



indic_prim = rbind(indic10,indic11,indic12,indic13,indic14)

indic20 <- indic_sco(ES_col,ESAS_col,ESASA_col,PAS_col,"col",0,"National")
indic21 <- indic_sco(ES_col,ESAS_col,ESASA_col,PAS_col,"col",HL4,"Genre")
indic22 <- indic_sco(ES_col,ESAS_col,ESASA_col,PAS_col,"col",HH6,"Urbain/Rural")
indic23 <- indic_sco(ES_col,ESAS_col,ESASA_col,PAS_col,"col",HH7,"Region")
indic24 <- indic_sco(ES_col,ESAS_col,ESASA_col,PAS_col,"col",windex5,"Richesse")



indic_sec1 = rbind(indic20,indic21,indic22,indic23,indic24)

indic30 <- indic_sco(ES_lyc,ESAS_lyc,ESASA_lyc,PAS_lyc,"lyc",0,"National")
indic31 <- indic_sco(ES_lyc,ESAS_lyc,ESASA_lyc,PAS_lyc,"lyc",HL4,"Genre")
indic32 <- indic_sco(ES_lyc,ESAS_lyc,ESASA_lyc,PAS_lyc,"lyc",HH6,"Urbain/Rural")
indic33 <- indic_sco(ES_lyc,ESAS_lyc,ESASA_lyc,PAS_lyc,"lyc",HH7,"Region")
indic34 <- indic_sco(ES_lyc,ESAS_lyc,ESASA_lyc,PAS_lyc,"lyc",windex5,"Richesse")



indic_sec2 = rbind(indic30,indic31,indic32,indic33,indic34)


df <- cbind(indic_prim |> select(-c(class,levels)),indic_sec1 |> select(-c(class,levels)),indic_sec2)



DT = data.table::data.table(x=1:3, y=c("a","b","c"))
data.table::transpose(DT, list.cols=FALSE)



grd2 <- import(here("data/pasec/PASEC2019_GRADE2.dta")) |> 
  clean_names() |> 
  filter(pays == 'TOGO') 

export(grd2, "data/pasec/tg_grd2.dta")

grd6 <- import(here("data/pasec/PASEC2019_GRADE6.dta")) |> 
  clean_names() |> 
  filter(pays == 'TOGO') 

export(grd6, "data/pasec/tg_grd6.dta")






