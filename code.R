
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


df_prim <- rbind(prim_ens,prim_genre) 
profil_prim <- data.table::transpose(df_prim,keep.names = "Classe",make.names = "var") |> 
  mutate(Classe = case_when(
    Classe == "TA_1" ~ "CP1",
    Classe == "TA_2" ~ "CP2",
    Classe == "TA_3" ~ "CE1",
    Classe == "TA_4" ~ "CE2",
    Classe == "TA_5" ~ "CM1",
    Classe == "TA_6" ~ "CM2",
  ))

sec1_ens <-  cbind(acces(2,1,0),acces(2,2,0),acces(2,3,0),acces(2,4,0)) |>
  rownames_to_column("var") |> 
  mutate(var = if_else(var == 1,"Nationale",var))

sec1_genre <-  cbind(acces(2,1,HL4),acces(2,2,HL4),acces(2,3,HL4),acces(2,4,HL4)) |> 
  rownames_to_column("var") |> 
  mutate(var = if_else(var == 1,"Masculin","Feminin"))


df_sec1 <- rbind(sec1_ens,sec1_genre) 
profil_sec1 <- data.table::transpose(df_sec1,keep.names = "Classe",make.names = "var") |> 
  mutate(Classe = case_when(
    Classe == "TA_1" ~ "6ieme",
    Classe == "TA_2" ~ "5ieme",
    Classe == "TA_3" ~ "4ieme",
    Classe == "TA_4" ~ "3ieme"
  ))



sec2_ens <-  cbind(acces(3,1,0),acces(3,2,0),acces(3,3,0)) |>
  rownames_to_column("var") |> 
  mutate(var = if_else(var == 1,"Nationale",var))

sec2_genre <-  cbind(acces(3,1,HL4),acces(3,2,HL4),acces(3,3,HL4)) |> 
  rownames_to_column("var") |> 
  mutate(var = if_else(var == 1,"Masculin","Feminin"))


df_sec2 <- rbind(sec2_ens,sec2_genre) 
profil_sec2 <- data.table::transpose(df_sec2,keep.names = "Classe",make.names = "var") |> 
  mutate(Classe = case_when(
    Classe == "TA_1" ~ "2nde",
    Classe == "TA_2" ~ "1ere",
    Classe == "TA_3" ~ "Tle"
  ))


profil <- rbind(profil_prim,profil_sec1,profil_sec2) |> 
  mutate(niveau = case_when(
    Classe %in% c("CP1","CP2","CE1","CE2","CM1","CM2") ~ "Primaire",
    Classe %in% c("6ieme","5ieme","4ieme","3ieme") ~ "College",
    Classe %in% c("2nde","1ere","Tle") ~ "Lycée"
  ), x = 1:13)

library(patchwork)
col_palette  <- paletteer::paletteer_d("nbapalettes::mavericks_retro")
profil |>
  mutate(Classe = fct_reorder(Classe, x)) |> 
  ggplot() +
  geom_point(aes(x = x,y = Nationale),size = 1.5,color = "#787878FF") +
  geom_line(aes(x = x,y = Nationale,colour = "#787878FF"),linewidth = 1) +
  geom_point(aes(x = x, y = Feminin),size = 1.5) +
  geom_line(aes(x = x, y = Feminin,colour = "#803342FF"),linewidth = 1) +
  geom_point(aes(x = x,y = Masculin),size = 1.5) +
  geom_line(aes(x = x,y = Masculin, colour = "#417839FF"),linewidth = 1) +
  geom_vline(xintercept = 6, linetype = "dotted", color = "#088BBEFF", size = 1) +
  geom_vline(xintercept = 10, linetype = "dotted", color = "#1BB6AFFF", size = 1) +
  scale_y_continuous(
    breaks = seq(10, 130, by = 20),
    limits = c(10, 130)
  ) +
  scale_x_continuous(
    breaks = seq(1, 13, by = 1),
    limits = c(1, 13),
    labels = c("CP1","CP2","CE1","CE2","CM1","CM2","6ieme","5ieme","4ieme","3ieme","2nde","1ere","Tle")
  ) +
  #scale_colour_manual(name = "Genre",values = col_palette,labels = c("Masculin", "National", "Feminin")) +
  scale_colour_manual(name = "Genre",values = c("#417839FF","#787878FF","#803342FF"),labels = c("Masculin", "National", "Feminin")) +
  labs(
    x = NULL,
    y = NULL#,
    #title = title_text,
    #subtitle = subtitle_text,
    #caption = caption_text,
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9)
  )


p1 <- profil |>
  filter(niveau == "Primaire") |> 
  mutate(Classe = fct_reorder(Classe, x)) |> 
  ggplot() +
  geom_point(aes(x = x,y = Nationale),size = 1.5,color = "#787878FF") +
  geom_line(aes(x = x,y = Nationale,color = "#787878FF"),linewidth = 1) +
  geom_point(aes(x = x, y = Feminin),size = 1.5,color = "#903030FF") +
  geom_line(aes(x = x, y = Feminin,color = "#903030FF"),linewidth = 1) +
  geom_point(aes(x = x,y = Masculin),size = 1.5,color = "#172869FF") +
  geom_line(aes(x = x,y = Masculin,color = "#172869FF"),linewidth = 1) +
  scale_y_continuous(
    breaks = seq(10, 130, by = 20),
    limits = c(10, 130)
  ) +
  scale_x_continuous(
    breaks = seq(1, 6, by = 1),
    limits = c(1, 6),
    labels = c("CP1","CP2","CE1","CE2","CM1","CM2")
  ) +
  scale_colour_manual(name = "Genre",values = c("#417839FF","#787878FF","#803342FF"),labels = c("Masculin", "National", "Feminin")) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

p2 <- profil |>
  filter(niveau == "College") |> 
  mutate(Classe = fct_reorder(Classe, x)) |> 
  ggplot() +
  geom_point(aes(x = x,y = Nationale),size = 1.5,color = "#787878FF") +
  geom_line(aes(x = x,y = Nationale,color = "#787878FF"),linewidth = 1) +
  geom_point(aes(x = x, y = Feminin),size = 1.5,color = "#903030FF") +
  geom_line(aes(x = x, y = Feminin,color = "#903030FF"),linewidth = 1) +
  geom_point(aes(x = x,y = Masculin),size = 1.5,color = "#172869FF") +
  geom_line(aes(x = x,y = Masculin,color = "#172869FF"),linewidth = 1) +
  scale_y_continuous(
    breaks = seq(10, 130, by = 20),
    limits = c(10, 130)
  ) +
  scale_x_continuous(
    breaks = seq(7, 10, by = 1),
    limits = c(7, 10),
    labels = c("6ieme","5ieme","4ieme","3ieme")
  ) +
  scale_colour_manual(name = "Genre",values = c("#417839FF","#787878FF","#803342FF"),labels = c("Masculin", "National", "Feminin")) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

p3 <- profil |>
  filter(niveau == "Lycée") |> 
  mutate(Classe = fct_reorder(Classe, x)) |> 
  ggplot() +
  geom_point(aes(x = x,y = Nationale),size = 1.5,color = "#787878FF") +
  geom_line(aes(x = x,y = Nationale,color = "#787878FF"),linewidth = 1) +
  geom_point(aes(x = x, y = Feminin),size = 1.5,color = "#903030FF") +
  geom_line(aes(x = x, y = Feminin,color = "#903030FF"),linewidth = 1) +
  geom_point(aes(x = x,y = Masculin),size = 1.5,color = "#172869FF") +
  geom_line(aes(x = x,y = Masculin,color = "#172869FF"),linewidth = 1) +
  scale_y_continuous(
    breaks = seq(10, 130, by = 20),
    limits = c(10, 130)
  ) +
  scale_x_continuous(
    breaks = seq(11, 13, by = 1),
    limits = c(11, 13),
    labels = c("2nde","1ere","Tle")
  ) +
  scale_colour_manual(name = "Genre",values = c("#417839FF","#787878FF","#803342FF"),labels = c("Masculin", "National", "Feminin")) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

p1 + p2 + p3 + plot_layout(widths = c(2,1.5,1),axes = "collect", guides = "collect") & theme(legend.position = "top") 



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


couverture <- df |> 
  mutate(etiquette = case_when(
    class=="National" & levels ==0 ~ "Togo",
    class=="Urbain/Rural"& levels ==1 ~ "Urbian",
    class=="Urbain/Rural"& levels ==2 ~ "Rural",
    class=="Region"& levels ==1 ~ "Maritime",
    class=="Region"& levels ==2 ~ "Plateaux",
    class=="Region"& levels ==3 ~ "Centrale",
    class=="Region"& levels ==4 ~ "Kara",
    class=="Region"& levels ==5 ~ "Savanes",
    class=="Region"& levels ==6 ~ "Lome-Comm",
    class=="Region"& levels ==7 ~ "Golfe-Urbain",
    class=="Richesse"& levels ==1 ~ "plus pauvres",
    class=="Richesse"& levels ==2 ~ "Second",
    class=="Richesse"& levels ==3 ~ "Moyen",
    class=="Richesse"& levels ==4 ~ "Quatrième",
    class=="Richesse"& levels ==5 ~ "plus riches",
    class=="Genre"& levels ==1 ~ "Masculin",
    class=="Genre"& levels ==2 ~ "Feminin",
    
  ))


df <- couverture |> 
  filter(class == "Region"| class == "National" ) |> 
  select(TNS_lyc,etiquette) |> 
  mutate(Togo = ifelse(etiquette == "Togo",TRUE,FALSE)) |> 
  mutate(etiquette = fct_reorder(etiquette, TNS_lyc)) 

  ggplot(df,aes(x = etiquette,y = TNS_lyc,fill = Togo))+
  geom_col(show.legend = FALSE) +
  scale_fill_manual(name = NULL, values = c("#787878FF", "#00B0F0"),labels(c("Fille","Garçon"))) +
  geom_text(
    data = df,
    mapping = aes(x = etiquette, y = TNS_lyc, label = round(TNS_lyc,2)),
    hjust = 1,
    nudge_x = -0.1,
    color = 'white',
    fontface = 'bold',
    size = 4.5
  ) +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        #panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()
  )


  
TNS_lyc <-  hl_design |>
    group_by(HH7,HL4) |> 
    summarise(
      TNS = survey_total(ESAS_col,na.rm = TRUE)/survey_total(PAS_col,na.rm = TRUE)*100
    ) |> 
    select(HH7,HL4,TNS)  |> 
    mutate(
      HH7 = case_when(
      HH7 == 1 ~ "Maritime",
      HH7 == 2 ~ "Plateaux",
      HH7 == 3 ~ "Centrale",
      HH7 == 4 ~ "Kara",
      HH7 == 5 ~ "Savanes",
      HH7 == 6 ~ "Lome-Comm",
      HH7 == 7 ~ "Golfe-Urbain"),
      HL4 = case_when(
      HL4 == 1 ~ "Masculin",
      HL4 == 2 ~ "Feminin")
    ) |> 
  mutate(
    HH7 = fct_reorder(HH7,TNS)
  )
  
    
  

ggplot(TNS_lyc, aes(x = TNS, y = HH7)) +
  geom_line() +
  geom_point(aes(color = HL4),size = 3,show.legend = FALSE) +
  scale_color_manual(name = NULL, values = c("#33658A", "#00B0F0")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8.5)
  )


