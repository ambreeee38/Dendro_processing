#Ce script est dédié à la création de fonctions permettant de généraliser et de
#centraliser les étapes du traitement du jeu de données. L'idée est d'avoir à la fin
#une seule fonction permettant de réaliser l'entièreté des opération en un clic.
#Les fonctions sont propres à chaque jeu de données (DMH, couvert et oiseaux) car ils
#ne contiennent pas les mêmes informations.

#/!\Il est important de vérifier que les colonnes du dataframe qu'on utilise aient le 
#même nom que le nom des colonnes utilisés dans les fonctions /!\

#Les fonctions se construisent toujours de la même manière :
#nom_de_la_fonction <- function (data) {blablabla return(data)}

#---------------------------------------------------------------------------------------
#------------------------------------FORET----------------------------------------------
#---------------------------------------------------------------------------------------
#------------------------------------
#Packages
#-----------------------------------

library(dplyr) #dplyr permet de mieux manipuler les donnes contenues dans un/des dataframe(s) ou tibble(s).On y toruve les fonctions select, rename, relocate, filter...mais surtout mutate qui est très pratique pour créer des nouvelles colonnes (très utile dans notre cas lorsqu'on calcule des volumes, G, N.ha à partir de colonnes préexistantes). On y trouve aussi le pipe (%>%) qui permet d'enchainer plusieurs opérations à partir d'un même dataset ou d'une même colonne
library(tidyverse) #tidyverse contient pas mal de fonctions très utliles (ggplot, readxl, readr, %>%). Il fait partie des packages de dplyr
library(openxlsx) #permet d'utiliser la fonction write.xlsx très pratique pour enregistrer des dataframe de r vers excel



#------------------------------------JEU DMH--------------------------------------------




#---------------------------------
#TYPOLOGIE FORESTIERE
#---------------------------------

#Dans les metadonnées, on dispose de la "forme" de la forêt (futaie régulière,
#irrégulière, ripisylve..). Le but est de les fusionner avec le jeu de données 
#DMH par placette.

#Dans Excel j'ai crée un nouveau jeu de données nommé "forme_forestiere" en copiant
#les colonnes "placette" et "forme" des métadonnées. La fonction ci-dessous permet 
#merge ce dataframe au jeu de données initial "Saisie_DMH_2024"et ce, par placette.

#On met les colonnes placettes au format "character" car il arrive que les colonnes
#placettes soient au format numérique dans un jeu de données et au format character
#dans l'autre. En faisant ça, on empêche les erreurs de jointure.

#La fonction left_join est issue du package dplyr et permet de réaliser une jointure
#dite "gauche" c'est à dire que toutes les lignes du jeux de données sont conservées 
#et uniquement les colonnes de "forme_forestiere" seront ajoutées au jeu de données
#initial s'il y a une correspondance.

#La fonction coalesce permet de garder uniqement une seule colonne forme car il se peut
#qu'on obtienne à la sortie, dans le jeu de données forme.x et forme.y. La fonction "select"
#les supprime et permet de ne conserver qu'une suele colonne "forme".

merge_forme_forestiere <- function(data) {
  # Harmoniser les types de "placette"
  data <- data %>%
    mutate(placette = as.character(placette))
  
  forme_forestiere <- forme_forestiere %>%
    mutate(placette = as.character(placette))
  
  # Effectuer la jointure
  data <- data %>%
    left_join(forme_forestiere, by = "placette")
  
  return(data)
}




#Cette fonction permet de convertir les 1,2,3.. par leur typologie associée afin d'avoir
#plus de clarté dans le jeu de données et pas uniquement des chiffres qui peuvent
#paraitre abstrait si l'on a pas la classification utilisée en tête.

#Ici mutate permet de modifier la colonne "forme" en character et remplace les chiffres
#par lur correspondance textuelle.


forme_foret<- function(data) {
  data <- data %>%
    mutate(
      forme = case_when(
        forme == 1 ~ "Futaie régulière",
        forme == 2 ~ "Futaie irrégulière",
        forme == 3 ~ "Taillis sous futaie",
        forme == 4 ~ "Taillis",
        forme == 5 ~ "Reboisement",
        forme == 6 ~ "Ripisylve",
        forme == 7 ~ "Milieu ouvert",
        TRUE ~ as.character(forme)  # Conserve les valeurs existantes si elles ne sont pas 1 à 6
      )
    )
  return(data)
}


#-----------------------------------
#TYPOLOGIE ESSENCE ARBRES
#-----------------------------------

#Les essences dans les jeux de données sont conservées sous forme de code pour plus
#de simplicité lors de la saisie des données. Ceci est une étape facultative si
#vous connaissez vos codes essences. Cette étape permet simplement de créer une nouvelle
#colonne associant les noms vernaculaires complets des essences à leur code.

#Pour cela, je reprend la classification des metadonnées et j'en fait un dataframe
#nommé ici "correspondance". Comme pour la fonction précédente, on left_join 
#ce tableau "correspondance" avec notre jeu de données. 

#Par ailleurs, dans cette même fonction, je demande à créer une nouvelle colonne nommée
#Typologie, permettant d'associer à chaque essence sa "typologie" = feuillu, conifère 
#ou indeterminée si l'essence n'a pas pu être clairement identifiée.



#Pour tout lister, j'avoue que l'IA m'a filé un petit coup de pouce :)




Typologie_essence <- function(data) {
  
  # Tableau de correspondance entre le code et l'espèce
  correspondance <- data.frame(
    Code = c("IND", "F.D", "ALI", "ALB", "ALT", "AUL", "AUB", "AUG", "AUV", "BOU", "BOP", "BOV", "CER", 
             "CHA", "CHT", "CHE", "CHP", "CHY", "CHR", "CHS", "CHX", "CHZ", "CHV", "CHL", "COM", "COR", 
             "CYT", "ERO", "ERC", "ERP", "ERS", "ERM", "FRE", "FRF", "FRC", "FRO", "HET", "MAR", "MER", 
             "NOI", "NOY", "ORM", "ORC", "ORT", "ORD", "PEU", "POI", "TRE", "TRO", "PLA", "ROB", "SAU", 
             "SAB", "SAM", "SOR", "TIL", "TIG", "TIP", "R.D", "CED", "CYP", "DOU", "EPI", "EPC", "EPS", 
             "GEN", "IFS", "MEE", "MEJ", "MEL", "PIN", "P.X.", "P.B.", "P.C.", "P.A.", "P.Z.", "P.L.", 
             "PCA", "PCO", "P.M.", "P.O.", "P.P.", "P.S.", "P.W.", "SAP", "S.C.", "S.N.", "S.V.", "S.P.", 
             "HOU", "PRU", "VIO", "SUR", "BUX"),
    Nom_essence = c("Espèces indéterminée", "Feuillu divers", "Alisier", "Alisier blanc", "Alisier torminal", "Aulne", 
                    "Aulne blanc", "Aulne glutineux", "Aulne vert", "Bouleau", "Bouleau blanc", "Bouleau verruqueux", 
                    "Cerisier", "Charme", "Châtaignier", "Chêne", "Chêne pédonculé", "Chêne pubescent", "Chêne rouge d'Amérique", 
                    "Chêne sessile", "Chêne sessile et pédonculé", "Chêne tauzin", "Chêne vert", "Chêne-liège", "Cornouiller mâle", 
                    "Cormier", "Cytise", "Érable à feuilles d'obier", "Érable champêtre", "Érable plane", "Érable sycomore", 
                    "Erable de Montpellier", "Frêne", "Frêne à fleurs", "Frêne commun", "Frêne oxyphylle", "Hêtre", "Marronnier", 
                    "Merisier", "Noisetier ou Coudrier", "Noyer", "Orme", "Orme champêtre", "Orme de montagne", "Orme lisse / diffus", 
                    "Peuplier", "Poirier sauvage", "Tremble", "Troene", "Platane", "Robinier faux-acacia", "Saule", "Saule blanc", 
                    "Saule Marsault", "Sorbier des oiseleurs", "Tilleul", "Tilleul à gdes feuilles", "Tilleul à petites feuilles", 
                    "Résineux divers", "Cèdre", "Cyprès", "Douglas", "Épicéa", "Épicéa commun", "Épicéa de Sitka", "Genévrier", 
                    "If commun", "Mélèze d'Europe", "Mélèze du Japon", "Mélèze hybride", "Pin", "Pin à crochets", "Pin brutia", 
                    "Pin cembro", "Pin d'Alep", "Pin de Salzmann", "Pin Laricio", "Pin laricio de Calabre", "Pin laricio de Corse", 
                    "Pin maritime", "Pin noir d'Autriche", "Pin pignon (parasol)", "Pin sylvestre", "Pin Weymouth", "Sapin", 
                    "Sapin de Céphalonie", "Sapin de Nordmann", "Sapin de Vancouver", "Sapin pectiné", "Houx", "Prunus divers", 
                    "Viorne sp.", "Sureau sp.", "Buis")
  )
  
  # Liste des essences par typologie
  feuillus <- c("Feuillu divers", "Alisier", "Alisier blanc", "Alisier torminal", 
                "Aulne", "Aulne blanc", "Aulne glutineux", "Aulne vert", "Bouleau", 
                "Bouleau blanc", "Bouleau verruqueux", "Cerisier", "Charme", "Châtaignier", 
                "Chêne", "Chêne pédonculé", "Chêne pubescent", "Chêne rouge d'Amérique", 
                "Chêne sessile", "Chêne sessile et pédonculé", "Chêne tauzin", "Chêne vert", 
                "Chêne-liège", "Cornouiller mâle", "Cormier", "Cytise", "Érable à feuilles d'obier", 
                "Érable champêtre", "Érable plane", "Érable sycomore", "Erable de Montpellier", 
                "Frêne", "Frêne à fleurs", "Frêne commun", "Frêne oxyphylle", "Hêtre", "Marronnier", 
                "Merisier", "Noisetier ou Coudrier", "Noyer", "Orme", "Orme champêtre", "Orme de montagne", 
                "Orme lisse / diffus", "Peuplier", "Poirier sauvage", "Tremble", "Troene", "Platane", 
                "Robinier faux-acacia", "Saule", "Saule blanc", "Saule Marsault", "Sorbier des oiseleurs", 
                "Tilleul", "Tilleul à gdes feuilles", "Tilleul à petites feuilles")
  
  coniferes <- c("Cèdre", "Cyprès", "Douglas", "Épicéa", "Épicéa commun", "Épicéa de Sitka", 
                 "Genévrier", "If commun", "Mélèze d'Europe", "Mélèze du Japon", "Mélèze hybride", 
                 "Pin", "Pin à crochets", "Pin brutia", "Pin cembro", "Pin d'Alep", "Pin de Salzmann", 
                 "Pin Laricio", "Pin laricio de Calabre", "Pin laricio de Corse", "Pin maritime", "Pin noir d'Autriche", 
                 "Pin pignon (parasol)", "Pin sylvestre", "Pin Weymouth", "Sapin", "Sapin de Céphalonie", 
                 "Sapin de Nordmann", "Sapin de Vancouver", "Sapin pectiné", "Résineux divers")
  
  # Fusion et ajout de la typologie avec mutate()
  data <- data %>%
    left_join(correspondance, by = "Code") %>%
    mutate(Typologie = case_when(
      Nom_essence %in% feuillus ~ "Feuillu",
      Nom_essence %in% coniferes ~ "Conifère",
      TRUE ~ "Indéterminée"
    ))
  
  return(data)
}

#------------------------------------
#DENSITE
#-----------------------------------


#CATEGORISER PB, MB, GB, TGB



#Categorisation des bois >30 cm DBH afin d'obtenir les gros bois (50à 60cm de diamètre)
#et tres gros bois ( >60 cm diamètre) pour calculer la densité de GB et TGB vivants et morts

#Le classement des bois a été fait selon Gaudin (1996)



classer_bois <- function(data, diam_col = "diam") {
  data <- data %>%
    mutate(classe_bois = case_when(
      .[[diam_col]] < 20 ~ "TPB",
      between(.[[diam_col]], 20, 25) ~ "PB",
      between(.[[diam_col]], 26, 45) ~ "MB",
      between(.[[diam_col]], 46, 60) ~ "GB",
      .[[diam_col]] > 60 ~ "TGB",
      TRUE ~ NA_character_  
    ))
  
  return(data)
}





#Calcul de la densite de gros bois vivant et tres gros bois vivant



#Le calcul a été réalisé pour chaque placette afin de filtrer dans la colonne classe_bois
#les "GB" et "TGB" pour les type.objet "BV". Le but est d'obtenir une densite a l'ha
#alors on divise par la surface de la placette (0.2157) pour avoir la densite/ha.
#Ce calcul est issu d'un simple produit en croix : 52 TGB et GB -> 0.2157 ha
#                                                   X TGB et GB -> 1 ha 

#Le calcul par placette est permis grâce à la fonction group_by.
#/!\ Quand on utilise la fonction group_by dans une fonction, il est important de
#utiliser la fonction "ungroup" à la fin de la fonction afin de dégrouper les parcelles



GBV <- function(data) {
  data <- data %>%
    group_by(placette) %>% 
    mutate(densite_GBV = sum(classe_bois %in% c("GB", "TGB") & type.objet == "BV", na.rm = TRUE)/0.1257) %>%
    ungroup()
  
  return(data)
}





#Calcul de la densite de gros bois mort et tres gros bois mort



#La même méthodologie a été utilisée pour les denistes de GBM et TGBM


GBM <- function(data) {
  data <- data %>%
    group_by(placette) %>% 
    mutate(densite_GBM = sum(classe_bois %in% c("GB", "TGB") & type.objet %in% c("BMD", "BMS"), na.rm = TRUE) / 0.1257) %>%
    ungroup()
  
  return(data)
}





#Calcul de la densite de BV >30 cm DBH




#Le calcul de la desnite de BV permet de caractériser un indice d'encombrement du milieu
#(milieu très dense ou pas du tout). Il est calculer selon la formule suivante afin
#d'obtenir une desnité par hectare: 
#Nb arbres >30cm DBH/ Surface placette soit Nb arbres >30cm DBH/0.2157.


#surface_placette_ha est définie en haut de la fonction. Cela évite de créer une colonne
#supplémentaire dans le tableau qui viendrait surcharger encore plus le jeu de données

#On calcule toujours à l'échelle de la placette grâce à la fonction group_by
#On exclut les BMS et BMD du calcul par la fonction "!(type.objet %in%)
#Ensuite, on désigne le nom de la nouvelle colonne (densite_BV) qui est égale au 
#nombre total de BV / surface de la placette. La fonction floor permet de n'avoir aucun
#chiffre après la virgule et donc un chiffre rond


densite_BV <- function(data) {
  surface_placette_ha <- 0.1257  
  data <- data %>%
    group_by(placette) %>%
    mutate(
      nombre_total_BV = sum(!(type.objet %in% c("BMS", "BMD"))),  
      densite_BV = floor(nombre_total_BV / surface_placette_ha) 
    ) %>%
    ungroup()
  
  return(data)
}





#Calcul de la densite de BMS 



#La même méthodologie a été utilisée pour calculer la desnité de BMS, en excluant
#cette fois les BV et BMD de la colonne type.objet


densite_BMS <- function (data){
  surface_placette_ha <- 0.1257
  data <- data %>%
    group_by(placette) %>%
    mutate(
      nombre_total_BMS = sum(!(type.objet %in% c("BV", "BMD"))), 
      densite_BMS = floor(nombre_total_BMS / surface_placette_ha) 
      
    ) %>%
    ungroup()
  
  return(data)
}
  




#Calcul desnite BMD


#La même méthodologie a été utilisée pour calculer la desnité de BMS, en excluant
#cette fois les BV et BMS de la colonne type.objet


densite_BMD <- function (data){
  surface_placette_ha <- 0.1257
  data <- data %>%
    group_by(placette) %>%
    mutate(
      nombre_total_BMD = sum(!(type.objet %in% c("BV", "BMS"))),  
      densite_BMD = floor(nombre_total_BMD / surface_placette_ha)  
    ) %>%
    ungroup()
  
  return(data)
}





#Calcul de la densite de BM totale



#La même méthodologie a été utilisée pour calculer la desnité de BM totale, en excluant
#cette fois les BV de la colonne type.objet


densite_BM_tot <- function (data) {
  surface_placette_ha <- 0.1257
  data<-data %>%
    group_by(placette) %>%
    mutate(
      nombre_total_BM = sum(type.objet != "BV"),  
      densite_BM_tot = floor(nombre_total_BM / surface_placette_ha)  
    ) %>%
    ungroup()
}







#Calcul de la densite de souches


#Le calcul de la desnite de souche permet d'avoir une indication sur l'exploitation
#forestiere (intensive ou non ?)

#La méthode utilisée est globalement la même, surface_placette_ha n'a pas été définie
#mais l'équation est divisée par 0.2157, cela revient au même.

#On calcule toujours par placette grâce à group_by

densite_souche <- function(data) {
  data <- data %>%
    group_by(placette) %>% 
    mutate(densite_souche = sum(Type.BM =="S" , na.rm = TRUE) / 0.1257) %>%
    ungroup()
  
  return(data)
}




#Calcul de la densite de chandelles


#On regroupe la densite de chandelles et d'arbres car leurs effectifs sont relativement
#faibles ce qui évite que leur poids soient "crasés individuellement.On fait donc la 
#somme des desnites de chandelles et d'arbres par placette



densite_VA <- function(data) {
  data <- data %>%
    group_by(placette) %>%
    mutate(
      densite_V = sum(Type.BM %in% "V", na.rm = TRUE) / 0.1257,
      densite_A = sum(Type.BM %in% "A", na.rm = TRUE) / 0.1257
    ) %>%
    ungroup()
  
  return(data)
}



#-----------------------------------
#VOLUME
#-----------------------------------

#Besoin de séparer les hauteurs et les longueurs de la colonne "hauteur/longueur" 
#donc création de 2 nouvelles colonnes, une "hauteur" associée aux BV et BMD
#et une "longueur" assoicée aux BMS.

#Les colonnes sont créées grâce à la fonction mutate
#NA_real permet de laisser les NA quand les lignes ne sont pas concernées par la 
#selection appliquée.
#Important de mettre BMD et BV dans le même case_when afin que les haueturs soient
#bien attribuées au deux et non pas à l'un qui sera remplacé par l'autre plus tard
#dans la fonction dans le cas où mutate se divise en 3 parties

ht_lg <- function(data) {
  data %>%
    mutate(
      hauteur = case_when(
        type.objet == "BMD" ~ `longueur / hauteur`,  # Donne la valeur de 'longueur / hauteur' quand type.objet == "BMD"
        type.objet == "BV" ~ `longueur / hauteur`,  # Donne la valeur de 'longueur / hauteur' quand type.objet == "BV"
        TRUE ~ NA_real_  # Met NA pour les autres types
      ),
      longueur = case_when(
        type.objet == "BMS" ~ `longueur / hauteur`,  # Donne la valeur de 'longueur / hauteur' quand type.objet == "BMS"
        TRUE ~ NA_real_  # Met NA pour les autres types
      )
    )
}







#Calcul de la densite propre à chaque arbre à l'ha


#On calcule une densite propre à chaque arbre à l'ha afin de pouvoir calculer la
#densite réelle à l'ha de chaque type d'arbres (BV, BM, BMS, BMD) pour pouvoir calculer
#les volumes

add_den_new <- function(data) {
  data %>%
    mutate(
      den_new = case_when(
        type.objet %in% c("BMD", "BMS", "BV") ~ 7.96,  # Vérifie si type.objet est "BMD" ou "BMS"
        TRUE ~ NA_real_  # Sinon, met NA
      )
    )
}



#Pour calculer le volume de BMD et BMS totaux par hectare = besoin de 3 étapes pour y arriver :


#(i) Calculer le volume de BMS/BMD par individu grâce aux formules :

#     Pour BMD_ind : (pi*40000*((diam)-(h/2-1.3))^2*h) #on intègre un facteur
#de correction (h/2-1.3). Le diamètre est en cm (corrigé par 40 000 qui permet la 
#conversion en m² car diamètre au carrée or on veut rayon au carré donc /2² = /4 et 
#10 000 pour la conversion de cm² en m²). La longueur est en m. 
#Cette formule s'appelle :  formule de Pressler

#     Pour BMS_ind : (pi / 40000 * (diam.med^2) * longueur) #volume d'un cylindre, pas de correction
#Même fonctionnement avec le 40 000. La hauteur est en m.



#(ii) Calculer le volume de BMS/BMD par individu par hectare grâce à la formule :
#     Pour BMD_ind_ha : Volume_BMD_ind*new_den
#     Pour BMS_ind_ha : volume_BMS_ind*new_den

#(iii) Calculer le volume total de BMS/BMD par hectare grâce à la formule :

#     Pour BMD_tot_ha : somme BMD_ind_ha pour chaque placette
#     Pour BMS_tot_ha : somme BMS_ind_ha pour chaque placette




#VOLUME BOIS MORT DEBOUT DU CUL




#Calcul du BMD par individu (volume en m3 par ind)

volume_BMD_ind <- function(data) {
  data<-data %>%
    mutate(
      vol_BMD_ind = case_when(
        type.objet == "BMD" & !is.na(diam) & !is.na(hauteur) ~ 
          (pi / 40000 * ((diam) - (hauteur / 2 - 1.3))^2)* hauteur, 
        type.objet == "BMD" & (is.na(diam) | is.na(hauteur)) ~ NA_real_,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      vol_BMD_ind = if_else(vol_BMD_ind == 0, NA_real_, vol_BMD_ind) 
    )
}




# Calcul du volume de BMD par individu par hectare

volume_BMD_ha <- function(data) {
  data<-data %>%
    mutate(
      vol_BMD_ha = vol_BMD_ind * den_new
    ) 
}



# Calcul du volume total de BMD par placette

volume_BMD_tot <- function(data) {
  data<-data %>%
    group_by(placette) %>%
    mutate(vol_BMD_tot = sum(vol_BMD_ha, na.rm = TRUE)) %>%  
    ungroup()  
}






#VOLUME BOIS MORT AU SOL




#Calcul volume BMS par individu

volume_BMS_ind <- function(data) {
  data <- data %>%
    mutate(
      vol_BMS_ind = case_when(
        type.objet == "BMS" & !is.na(diam.med) & !is.na(longueur) ~ 
          pi / 40000 * (diam.med^2) * longueur,  # Formule pour diam.med en cm et longueur en m
        type.objet == "BMS" & (is.na(diam.med) | is.na(longueur)) ~ NA_real_,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      vol_BMS_ind = if_else(vol_BMS_ind == 0, NA_real_, vol_BMS_ind)
    )
  
  return(data)  
}


test<-volume_BMS_ind(test)

#Calcul du volume de BMS par hectare

volume_BMS_ha <- function(data) {
  data<-data %>%
    mutate(
      vol_BMS_ha = vol_BMS_ind * den_new
    ) 
}



#Calcul du volume de BMS total à l'hectare

volume_BMS_tot <- function(data) {
  data<-data %>%
    group_by(placette) %>%
    mutate(vol_BMS_tot = sum(vol_BMS_ha, na.rm = TRUE)) %>%  
    ungroup() 
}







#VOLUME BOIS MORT TOTAL


#Pour calculer le volume total de bois mort, on fait la somme pour chaque placette
#(goup_by(placette)) des vol_BMS_ha + vol_BMD_ha. Ces valeurs sont des valeurs 
#individuelles de volumes extrapolées à l'hectare qui sont donc plus justes à 
#utiliser pour le calcul du volume de bois mort total  sur la parcelle


volume_BM_tot <- function(data) {
  data <- data %>%  # Assigner le résultat à `data`
    group_by(placette) %>%
    mutate(
      vol_BM_tot = sum(vol_BMS_ha, vol_BMD_ha, na.rm = TRUE)  
    ) %>%
    ungroup()
  
  return(data)  # Retourner explicitement `data`
}




#-----------------------------------
#DIVERSITE DMH
#-----------------------------------
diversite_dmh <- function(data) {
  data <- data %>%  
    group_by(placette) %>%
    mutate(diversite_DMH = n_distinct(code.DMH)) %>%
    ungroup()
  
  return(data)  
}

test <- diversite_dmh(test)  

#----------------------------------
#NETTOYAGE JEU DE DONNEES FINAL
#---------------------------------
nettoyage<-function(data){
  data <- data %>%
  select(-arbre, -Code, -diam, -longueur / hauteur, -Type.BM, -Remarque, -diam.ini, -diam.fin, -diam.med, -nombre_total_BV, -nombre_total_BMS, -nombre_total_BMD, -nombre_total_BM, -hauteur, -longueur, -den_new, -vol_BMD_ind, -vol_BMD_ha, -vol_BMS_ind, -vol_BMS_ha)
return(data)
}
#----------------------------------
#FONCTION FINALE POUR CALCULER VARIABLES STUCTURELLES
#----------------------------------


#Ici, ff_dmh est la fonction finale qui regroupe toutes les fonctions créées ci-dessus.
#Si elle est fonctionnelle et que les jeux de données ont bien les mêmes noms de
#données d'entrée, il suffit d'utiliser la ligne de code suivante :

#nom_du_jeu_donnees_final<-ff_dmh(nom_jeu_de_donnes_initial) 


ff_dmh <- function(data) {
  data<-merge_forme_forestiere(data)
  data<-forme_foret(data)
  data <- Typologie_essence(data)           # Applique la fonction culture_foret
  data <- classer_bois(data)           # Applique la fonction classer_bois
  data <- GBV(data)                    # Applique la fonction GBV
  data <- GBM(data)                    # Applique la fonction GBM
  data <- densite_BV(data)              # Applique la fonction densite_BV
  data <- densite_BMS(data)             # Applique la fonction densite_BMS
  data <- densite_BMD(data)             # Applique la fonction densite_BMD
  data <- densite_BM_tot(data)              # Applique la fonction densite_BM
  data <- densite_souche(data)
  data <- densite_VA(data)
  data <- ht_lg(data)                  # Applique la fonction ht_lg
  data <- add_den_new(data)             # Applique la fonction add_den_new
  data <- volume_BMD_ind(data)         # Applique la fonction volume_BMD_ind
  data <- volume_BMD_ha(data)          # Applique la fonction volume_BMD_ha
  data <- volume_BMD_tot(data)         # Applique la fonction volume_BMD_tot
  data <- volume_BMS_ind(data)         # Applique la fonction volume_BMS_ind
  data <- volume_BMS_ha(data)          # Applique la fonction volume_BMS_ha
  data <- volume_BMS_tot(data)         # Applique la fonction volume_BMS_tot
  data <- volume_BM_tot(data)          # Applique la fonction volume_BM_tot
  data <- nettoyage (data)
  
  

  return(data)
}



#Test de la fonction finale

test<-ff_dmh(Saisie_DMH_2024_corrige)

write.xlsx()





#------------------------------------------JEU COUVERT--------------------------------


#-----------------------------------
#TYPOLOGIE ESSENCE ARBRES
#-----------------------------------



Typologie_essence <- function(data) {
  
#Création d'un data.frame permettant d'associer à chaque code son nom vernaculaire en entier
  correspondance <- data.frame(
    Ess = c("IND", "FD", "ALI", "ALB", "ALT", "AUL", "AUB", "AUG", "AUV", "BOU", "BOP", "BOV", "CER", 
                "CHA", "CHT", "CHE", "CHP", "CHY", "CHR", "CHS", "CHX", "CHZ", "CHV", "CHL", "COM", "COR", 
                "CYT", "ERO", "ERC", "ERP", "ERS", "ERM", "FRE", "FRF", "FRC", "FRO", "HET", "MAR", "MER", 
                "NOI", "NOY", "ORM", "ORC", "ORT", "ORD", "PEU", "POI", "TRE", "TRO", "PLA", "ROB", "SAU", 
                "SAB", "SAM", "SOR", "TIL", "TIG", "TIP", "RD", "CED", "CYP", "DOU", "EPI", "EPC", "EPS", 
                "GEN", "IFS", "MEE", "MEJ", "MEL", "PIN", "PX", "PB", "PC", "PA", "PZ", "PL", 
                "PCA", "PCO", "P", "PO", "PP", "PS", "PW", "SAP", "SC", "SN", "SV", "SP", 
                "HOU", "PRU", "VIO", "SUR", "BUX", "PN","PH","OP","GE"),
    Nom_essence = c("Espèces indéterminée", "Feuillu divers", "Alisier", "Alisier blanc", "Alisier torminal", "Aulne", 
                    "Aulne blanc", "Aulne glutineux", "Aulne vert", "Bouleau", "Bouleau blanc", "Bouleau verruqueux", 
                    "Cerisier", "Charme", "Châtaignier", "Chêne", "Chêne pédonculé", "Chêne pubescent", "Chêne rouge d'Amérique", 
                    "Chêne sessile", "Chêne sessile et pédonculé", "Chêne tauzin", "Chêne vert", "Chêne-liège", "Cornouiller mâle", 
                    "Cormier", "Cytise", "Érable à feuilles d'obier", "Érable champêtre", "Érable plane", "Érable sycomore", 
                    "Erable de Montpellier", "Frêne", "Frêne à fleurs", "Frêne commun", "Frêne oxyphylle", "Hêtre", "Marronnier", 
                    "Merisier", "Noisetier ou Coudrier", "Noyer", "Orme", "Orme champêtre", "Orme de montagne", "Orme lisse / diffus", 
                    "Peuplier", "Poirier sauvage", "Tremble", "Troene", "Platane", "Robinier faux-acacia", "Saule", "Saule blanc", 
                    "Saule Marsault", "Sorbier des oiseleurs", "Tilleul", "Tilleul à gdes feuilles", "Tilleul à petites feuilles", 
                    "Résineux divers", "Cèdre", "Cyprès", "Douglas", "Épicéa", "Épicéa commun", "Épicéa de Sitka", "Genévrier", 
                    "If commun", "Mélèze d'Europe", "Mélèze du Japon", "Mélèze hybride", "Pin", "Pin à crochets", "Pin brutia", 
                    "Pin cembro", "Pin d'Alep", "Pin de Salzmann", "Pin Laricio", "Pin laricio de Calabre", "Pin laricio de Corse", 
                    "Pin maritime", "Pin noir d'Autriche", "Pin pignon (parasol)", "Pin sylvestre", "Pin Weymouth", "Sapin", 
                    "Sapin de Céphalonie", "Sapin de Nordmann", "Sapin de Vancouver", "Sapin pectiné", "Houx", "Prunus divers", 
                    "Viorne sp.", "Sureau sp.", "Buis","Pin noir","Pin d'Alep","Chêne pubescent","Genévrier commun")
  )
  
  # Fusionner le dataframe original avec la correspondance pour obtenir le nom des essences
  data <- data %>%
    left_join(correspondance, by = c("Ess" = "Ess"))
  
  # Ajouter la colonne 'Typologie' selon le nom de l'essence
  data$Typologie <- ifelse(data$Ess %in% c("Feuillu divers", "Alisier", "Alisier blanc", "Alisier torminal", 
                                                   "Aulne", "Aulne blanc", "Aulne glutineux", "Aulne vert", "Bouleau", 
                                                   "Bouleau blanc", "Bouleau verruqueux", "Cerisier", "Charme", "Châtaignier", 
                                                   "Chêne", "Chêne pédonculé", "Chêne pubescent", "Chêne rouge d'Amérique", 
                                                   "Chêne sessile", "Chêne sessile et pédonculé", "Chêne tauzin", "Chêne vert", 
                                                   "Chêne-liège", "Cornouiller mâle", "Cormier", "Cytise", "Érable à feuilles d'obier", 
                                                   "Érable champêtre", "Érable plane", "Érable sycomore", "Erable de Montpellier", 
                                                   "Frêne", "Frêne à fleurs", "Frêne commun", "Frêne oxyphylle", "Hêtre", "Marronnier", 
                                                   "Merisier", "Noisetier ou Coudrier", "Noyer", "Orme", "Orme champêtre", "Orme de montagne", 
                                                   "Orme lisse / diffus", "Peuplier", "Poirier sauvage", "Tremble", "Troene", "Platane", 
                                                   "Robinier faux-acacia", "Saule", "Saule blanc", "Saule Marsault", "Sorbier des oiseleurs", 
                                                   "Tilleul", "Tilleul à gdes feuilles", "Tilleul à petites feuilles"), 
                           "Feuillu", 
                           ifelse(data$Ess %in% c("Cèdre", "Cyprès", "Douglas", "Épicéa", "Épicéa commun", "Épicéa de Sitka", 
                                                          "Genévrier", "If commun", "Mélèze d'Europe", "Mélèze du Japon", "Mélèze hybride", 
                                                          "Pin", "Pin à crochets", "Pin brutia", "Pin cembro", "Pin d'Alep", "Pin de Salzmann", 
                                                          "Pin Laricio", "Pin laricio de Calabre", "Pin laricio de Corse", "Pin maritime", "Pin noir d'Autriche", 
                                                          "Pin pignon (parasol)", "Pin sylvestre", "Pin Weymouth", "Sapin", "Sapin de Céphalonie", 
                                                          "Sapin de Nordmann", "Sapin de Vancouver", "Sapin pectiné", "Résineux divers"), 
                                  "Conifère", "Indéterminée"))
  
  # Retourner le dataframe modifié
  return(data)
}

couvert<-Typologie_essence(couvert)


#----------------------------
#STRATIFICATION VERTICALE
#----------------------------

colonnes_strates <- function(data) {
  # Ajouter les nouvelles colonnes par addition
  data$strate_h <- data$a + data$b
  data$strat_sa <- data$c + data$d
  data$strat_a <- data$e + data$f
  data$strat_A <- data$g + data$h
  return(data)
}


#------------------------------------
#SURFACE TERRIERE
#-----------------------------------




#Fonction pour calculer la surface terrière par essence et pour chaque individu



st_essence <- function(data) {
  # Calcul de la surface de la placette en hectares
  surface_placette_ha <- 0.1257  # 1256.64 m² converti en ha
  
  data <- data %>%
    mutate(
      st_conifere = ifelse(Typologie == "Conifère", round((pi * (diam / 2)^2) / 10000, 2), 0),
      st_feuillu  = ifelse(Typologie == "Feuillu", round((pi * (diam / 2)^2) / 10000, 2), 0),
      st_BMS = ifelse (type.objet == "BMS", round ((pi * (diam.med / 2)^2) / 10000, 2), 0),
      st_BMD = ifelse (type.objet == "BMD", round ((pi * (diam / 2)^2) / 10000, 2), 0),
      st_conifere_ha = round(st_conifere / surface_placette_ha, 2),
      st_feuillu_ha  = round(st_feuillu / surface_placette_ha, 2),
      st_BMS_ha  = round(st_BMS / surface_placette_ha, 2),
      st_BMD_ha  = round(st_BMD / surface_placette_ha, 2),#conversion en ha pour chaque individu
    )
  
  return(data)
}






#Fonction pour calculer la ST totale par essence et par placette 



st_tot_essence <- function(data) {
  data_tot <- data %>%
    group_by(placette) %>%
    summarise(
      st_conifere_tot = round(sum(st_conifere_ha, na.rm = TRUE), 2),
      st_feuillu_tot  = round(sum(st_feuillu_ha, na.rm = TRUE), 2),
      st_BMS_tot = round(sum(st_BMS_ha, na.rm = TRUE), 2),
      st_BMD_tot = round(sum(st_BMD_ha, na.rm = TRUE), 2)
    ) %>%
    ungroup()
  
  # Ajout des valeurs agrégées à chaque ligne du dataset
  data <- data %>%
    left_join(data_tot, by = "placette")
  
  return(data)
}




#Fonction pour calculer la ST_totale /!\ PAS UTILISEE POUR LE MOMENT

st_totale <- function(data) {
  
  data_st_tot <- data %>%
    group_by(placette) %>%
    summarise(
      st_indetermine = sum(ifelse(Typologie == "Indéterminé", ((pi * (diam / 2)^2) / 10000) / 0.0314, 0), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Ajouter la surface des espèces indéterminées au dataset original
  data <- left_join(data, data_st_tot, by = "placette")
  
  # Calculer la surface terrière totale
  data <- data %>%
    mutate(st_totale = st_conifere + st_feuillu + st_indetermine)
  
  return(data)
}


#------------------------------
#DENSITE CANOPEE
#------------------------------


canopy <- function(data) {
  
  # Grouper les données par la colonne Placette et calcul de la moyenne pour chaque exposition
  data <- data %>%
    mutate(can_moy = rowMeans(select(., No, Su, Es, Ou), na.rm = TRUE)) %>%
    ungroup()  # Dé-groupe après l'opération
  
  # Calcul de la densité de la canopée par placette
  data <- data %>%
    group_by(Placette) %>%  # Grouper de nouveau par Placette pour le calcul de densité
    mutate(densite_canopy = 1.04 * mean(can_moy, na.rm = TRUE)) %>%
    ungroup()  # Dé-grouper après avoir appliqué le calcul
  return(data)
}

#----------------------------
#CALCUL DE LA RICHESSE SPECIFIQUE 
#-----------------------------


#

richesse_spe <- function(data) {
  data %>%
    group_by(placette) %>%           
    mutate(richesse_spe = n_distinct(Essence)) %>%  
    ungroup()                             
  
}

#-------------------------------------
#NETTOYAGE JEU DE DONNES COUVERT
#-------------------------------------
  
bd_couvert_propre<-function(data)  {
  
  # Ajouter la colonne 'observateur' en combinant les valeurs non manquantes des colonnes Eq1 à Eq4
  data <- data %>%
    mutate(observateur = apply(., 1, function(x) paste(na.omit(x[c("Eq1", "Eq2", "Eq3", "Eq4")]), collapse = " ")))
  
  # Supprimer les colonnes spécifiées (a, b, c, e, f, g, h)
  data <- data %>%
    select(-a, -b, -c, -d, -e, -f, -g, -h, -No, -Es, -Su, -Ou, -Ess, -Dia, -N, -Comment, -Tri, -Noteur, -Eq1, -Eq2, -Eq3, -Eq4)
  return(data)
}


#-----------------------------------
#FONCTION FINALE COUVERT
#-----------------------------------




ff_couvert<-function(data) {
  data<-colonnes_strates(data)
  data<-canopy (data)
  data<-bd_couvert_propre(data)
  
  return(data)
}





#--------------------------------------------------------------------------------------
#--------------------------------------PTIT BEC----------------------------------------
#--------------------------------------------------------------------------------------
#------------------------------
#CALCUL DE LA RICHESSE SPECIFIQUE PAR GUILDE
#-------------------------------



#NIDIFICATION


#cavicole
rs_cavi <- function(data) {
  data<-data %>%
    group_by(Placette) %>%           
    mutate(rs_cav = sum(Nidification == "Cavicole", na.rm = TRUE)) %>% 
    ungroup()
  
  return(data)
}



#sol
rs_sol <- function(data) {
  data<-data %>%
    group_by(Placette) %>%           
    mutate(rs_sol = sum(Nidification == "Sol", na.rm = TRUE)) %>% 
    ungroup()
  
  return(data)
}


#arboricole

rs_arb <- function(data) {
  data<-data %>%
    group_by(Placette) %>%           
    mutate(rs_arb = sum(Nidification == "Arboricole", na.rm = TRUE)) %>% 
    ungroup()
  
  return(data)
}



#ALIMENTATION


#insectivore
rs_ins <- function(data) {
  data<-data %>%
    group_by(Placette) %>%           
    mutate(rs_ins = sum(Alimentation == "Insectivore", na.rm = TRUE)) %>% 
    ungroup()
  
  return(data)
}


#omnivores
rs_omn <- function(data) {
  data<-data %>%
    group_by(Placette) %>%           
    mutate(rs_omn = sum(Alimentation == "Omnivore", na.rm = TRUE)) %>% 
    ungroup()
  
  return(data)
}




#herbivore
rs_herb <- function(data) {
  data<-data %>%
    group_by(Placette) %>%           
    mutate(rs_herb = sum(Alimentation == "Herbivore", na.rm = TRUE)) %>% 
    ungroup()
  
  return(data)
}


















#AU CAS OU


#------------------------------------
#SURFACE TERRIERE
#-----------------------------------

#Fonction pour calculer la surface terrière par essence et pour chaque individu



st_essence <- function(data) {
  # Calcul de la surface de la placette en hectares
  surface_placette_ha <- 0.1257  # 1256.64 m² converti en ha
  
  data <- data %>%
    mutate(
      st_conifere = ifelse(Typologie == "Conifère", round((pi * (diam / 2)^2) / 10000, 2), 0),
      st_feuillu  = ifelse(Typologie == "Feuillu", round((pi * (diam / 2)^2) / 10000, 2), 0),
      st_BMS = ifelse (type.objet == "BMS", round ((pi * (diam.med / 2)^2) / 10000, 2), 0),
      st_BMD = ifelse (type.objet == "BMD", round ((pi * (diam / 2)^2) / 10000, 2), 0),
      st_conifere_ha = round(st_conifere / surface_placette_ha, 2),
      st_feuillu_ha  = round(st_feuillu / surface_placette_ha, 2),
      st_BMS_ha  = round(st_BMS / surface_placette_ha, 2),
      st_BMD_ha  = round(st_BMD / surface_placette_ha, 2),#conversion en ha pour chaque individu
    )
  
  return(data)
}






#Fonction pour calculer la ST totale par essence et par placette 



st_tot_essence <- function(data) {
  data_tot <- data %>%
    group_by(placette) %>%
    summarise(
      st_conifere_tot = round(sum(st_conifere_ha, na.rm = TRUE), 2),
      st_feuillu_tot  = round(sum(st_feuillu_ha, na.rm = TRUE), 2),
      st_BMS_tot = round(sum(st_BMS_ha, na.rm = TRUE), 2),
      st_BMD_tot = round(sum(st_BMD_ha, na.rm = TRUE), 2)
    ) %>%
    ungroup()
  
  # Ajout des valeurs agrégées à chaque ligne du dataset
  data <- data %>%
    left_join(data_tot, by = "placette")
  
  return(data)
}




#Fonction pour calculer la ST_totale /!\ PAS UTILISEE POUR LE MOMENT

st_totale <- function(data) {
  
  data_st_tot <- data %>%
    group_by(placette) %>%
    summarise(
      st_indetermine = sum(ifelse(Typologie == "Indéterminé", ((pi * (diam / 2)^2) / 10000) / 0.0314, 0), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Ajouter la surface des espèces indéterminées au dataset original
  data <- left_join(data, data_st_tot, by = "placette")
  
  # Calculer la surface terrière totale
  data <- data %>%
    mutate(st_totale = st_conifere + st_feuillu + st_indetermine)
  
  return(data)
}
