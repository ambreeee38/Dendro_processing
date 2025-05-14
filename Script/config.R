#### 1.Packages ####
if(TRUE) {

 library(dplyr) #dplyr permet de mieux manipuler les donnes contenues dans un/des dataframe(s) ou tibble(s).On y toruve les fonctions select, rename, relocate, filter...mais surtout mutate qui est très pratique pour créer des nouvelles colonnes (très utile dans notre cas lorsqu'on calcule des volumes, G, N.ha à partir de colonnes préexistantes). On y trouve aussi le pipe (%>%) qui permet d'enchainer plusieurs opérations à partir d'un même dataset ou d'une même colonne
 library(tidyverse) #tidyverse contient pas mal de fonctions très utliles (ggplot, readxl, readr, %>%). Il fait partie des packages de dplyr
 library(openxlsx) #permet d'utiliser la fonction write.xlsx très pratique pour enregistrer des dataframe de r vers excel
 library(readxl) #permet d'ouvrir un fichier Excel
 library(ggplot2) #pour les graph
library(viridis)
library(corrplot)
library(gridExtra)
library(patchwork)
library(lme4)
library(emmeans) #pour faire table d'anova
library(kableExtra) #pour faire les jolies tables de présentation des sorties de modèle
library(MuMIn) #pour faire AICc
library(MASS) #permet d'accéder à des fonctions que le package glm ne gère pas, comme pour binomial negative
library(gamlss)
library(glmmTMB)
library(vegan) #pour le jackknif indiactor
}


#### 2. Configuration du chemin de direction ####
if(TRUE){
  
  
  root_dir <- getwd()
  
  ## Définition des chemins d'accès aux inputs (soit les jeux de données avec lesquels nous travaillons)
  
  
  ##VENTOUX##
  Input_case_file <-file.path(root_dir, "Input")
  Input_case_file_ventoux <-file.path(Input_case_file, "Input_ventoux")
  Input_couvert_case_file <-file.path(Input_case_file, "Input_couvert")
  Input_dmh_case_file <-file.path(Input_case_file, "Input_dmh")
  Input_oiseaux_case_file <-file.path(Input_case_file, "Input_oiseaux")
  
  
  ##GIFFRE##
  Input_case_file <-file.path(root_dir, "Input")
  Input_case_file_giffre <-file.path(Input_case_file, "Input_giffre")
  Input_couvert_case_file <-file.path(Input_case_file, "Input_couvert")
  Input_dmh_case_file <-file.path(Input_case_file, "Input_dmh")
  Input_oiseaux_case_file <-file.path(Input_case_file, "Input_oiseaux")
  
  
  
  ## Définition des chemins d'accès output (soit là où les fichiers de sortie seront stockés)
  
  Output_file <-file.path(root_dir, "Output")
  
  ##VENTOUX##
  Output_file_ventoux <- file.path (Output_file, "Output_ventoux")
  
  
  ##GIFFRE##
  Output_file_giffre <- file.path (Output_file, "Output_giffre")
  
  
  ##BD GIFFRE + VENTOUX##
  Output_file_BD_merged <- file.path (Output_file, "Output_BD_merged")
  

}
