  #Ce script est dédié à la création de fonctions permettant de généraliser et de
#centraliser les étapes du traitement des jeux de données pour les sites Ventoux et
#Haut Giffre. L'idée est d'avoir à la fin une seule fonction permettant de réaliser 
#l'entièreté des opération en un clic.
#Les fonctions sont propres à chaque jeu de données (DMH, couvert et oiseaux) car ils
#ne contiennent pas les mêmes informations.A la fin, un seul jeu de donné sera 
#obtenu.

#/!\ Il est important de vérifier que les colonnes du dataframe utilisé possèdent le 
#même nom que celui des colonnes utilisées dans les fonctions /!\

#Les fonctions se construisent toujours de la même manière :
#nom_de_la_fonction <- function (data) {blablabla return(data)}

#La configuration R est expliquée dans le jeu de donnée "config.R"

source(config.R)

 SITE = "ventoux"

#---------------------------------------------------------------------------------------
#----------------------------------I. FORET----------------------------------------------
#---------------------------------------------------------------------------------------

#### 1. JEU DMH ####


  ### 1.0. Definition des modalités d'entrée et de sortie des jeux de données DMH ###

if(TRUE) {
  
  ## ENTREE
  
  # Dossier contenant le jeu de données DMH
  Input_dmh_case_file <-file.path(Input_case_file_ventoux, "Input_dmh")
  
  # Deux .XLSX contenant les données DMH. sheet = Data permet de sélectionner la feuille "Data" parmi les 3 autres.  
  file_data_dmh <-read_excel(file.path(Input_dmh_case_file, "Saisie_DMH_2024_corrige.xlsx"), sheet = "Data")
  file_data_forme_forestiere<-file.path(Input_dmh_case_file, "forme_forestiere.xlsx")
  file_data_CodEss <- file.path (Input_dmh_case_file, "CodESS.xlsx")
  file_data_alt <- file.path (Input_dmh_case_file, "alt.xlsx")
  file_data_topo <- file.path (Input_dmh_case_file, "topo.xlsx")
  file_data_pente <- file.path (Input_dmh_case_file, "pente.xlsx")
  file_data_expo <- file.path (Input_dmh_case_file, "expo.xlsx")
  
  ## SORTIE
  
  # Un .XLSX contenant les données DMH propres
  file_output_dmh_filtered <- file.path(Output_file_ventoux, "data_dmh_filtered")
  if (!dir.exists(file_output_dmh_filtered)) {
    dir.create(file_output_dmh_filtered, recursive = TRUE)
  }
  file_output_dmh <- file.path(file_output_dmh_filtered,paste0(SITE,"_dmh_filtered.xlsx"))
 
  
  ## LECTURE DES JEUX DE DONNEES DANS L'ENVIRONNEMENT (file_data_dmh est déja chargé dans les lignes supérieures)
  forme_forestiere = read_excel(file_data_forme_forestiere) 
  CodEss = read_excel (file_data_CodEss)
  alt = read_excel (file_data_alt)
  topo = read_excel (file_data_topo)
  pente = read_excel (file_data_pente)
  expo = read_excel (file_data_expo)
  data_dmh<-file_data_dmh
  
  
  
}




  ### 1.1. TYPOLOGIE FORESTIERE ###


if(TRUE){
  
  #Dans les metadonnées, on dispose de la "forme" de la forêt (futaie régulière,
  #irrégulière, ripisylve..). Le but est de les fusionner avec le jeu de données 
  #DMH par placette.
  
  #Dans Excel, un nouveau jeu de données nommé "forme_forestiere" est crée en copiant
  #les colonnes "placette" et "forme" des métadonnées. La fonction ci-dessous permet 
  #merge ce dataframe au jeu de données initial "Saisie_DMH_2024"et ce, par placette.
  
  #On met les colonnes placettes au format "character" car il arrive que les colonnes
  #placettes soient au format numérique dans un jeu de données et au format character
  #dans l'autre. En faisant ça, on empêche les erreurs de jointure.
  
  #La fonction left_join est issue du package dplyr et permet de réaliser une jointure
  #dite "gauche". Toutes les lignes du jeux de données sont conservées 
  #et uniquement les colonnes de "forme_forestiere" seront ajoutées au jeu de données
  #initial s'il y a une correspondance.
  

  
  merge_forme_forestiere <- function(data) {
    data <- data %>%
      mutate(placette = as.character(placette))
    forme_forestiere <- forme_forestiere %>%
      mutate(placette = as.character(placette))
    data <- data %>%
      left_join(forme_forestiere, by = "placette")
    
    return(data)
  }
  
  
  
  
  #Cette fonction permet de convertir les 1,2,3.. par leur typologie associée afin d'avoir
  #plus de clarté dans le jeu de données et pas uniquement des chiffres qui peuvent
  #paraitre abstrait si l'on a pas la classification utilisée en tête.
  
  #Ici mutate permet de modifier la colonne "forme" en character et remplace les chiffres
  #par leur correspondance textuelle.
  
  
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
  
}
 

  ### 1.2. ESSENCES ARBRES ###

if(TRUE) {
  
  #Les essences dans les jeux de données sont conservées sous forme de code pour plus
  #de simplicité lors de la saisie des données. Ceci est une étape facultative si
  #vous connaissez vos codes essences. Cette étape permet simplement de créer une nouvelle
  #colonne associant les noms vernaculaires complets des essences à leur code.
  
  #Pour cela, nous reprenons la classification des metadonnées et en faisons un excel
  #nommé ici "CodEss". Comme pour la fonction précédente, on left_join 
  #ce tableau "CodeEss" avec notre jeu de données. 
  
  #Par ailleurs, la fonction suivante, nous permet de créer une nouvelle colonne nommée
  #Typologie, permettant d'associer à chaque essence sa "typologie" = feuillu, conifère 
  #ou indeterminée si l'essence n'a pas pu être clairement identifiée.
  
  
  Typologie_essence_code <- function (data) {
    data <-data%>%
      left_join(CodEss, by = "Code") %>%
      return (data)
  }
  
  

  
  Typologie_essence <- function(data) {

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
                   "Sapin de Nordmann", "Sapin de Vancouver", "Sapin pectiné", "Résineux divers", "Pin pignon")
    
    # Fusion et ajout de la typologie
      data <-data%>%
      mutate(Typologie = case_when(
        Nom_essence %in% feuillus ~ "Feuillu",
        Nom_essence %in% coniferes ~ "Conifère",
        TRUE ~ "Indéterminée"
      ))
    
    return(data)
  }
  
 
}

  ### 1.3. DENSITE ###


if(TRUE) {
  
  
  ## 1.3.1. CATEGORISER PB, MB, GB, TGB ##
  
  
   
  #Categorisation des bois > 30 cm DBH afin d'obtenir les gros bois (50 à 60cm de diamètre)
  #et tres gros bois ( > 60 cm diamètre) pour calculer la densité de GB et TGB vivants et morts
  
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
  
  
}
  
  
  ## 1.3.2. Calcul de la densite de Gros Bois Vivant et Tres Gros Bois Vivant ##
  
if(TRUE){
  
  
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
  
  
}
  
  
  ## 1.3.3. Calcul de la densite de Gros Bois Mort et Tres Gros Bois Mort ##
  
  if(TRUE){
    
    #La même méthodologie a été utilisée pour les densites de GBV et TGBV
    
    
  GBM <- function(data) {
      data <- data %>%
        group_by(placette) %>% 
        mutate(densite_GBM = sum(classe_bois %in% c("GB", "TGB") & type.objet %in% c("BMD", "BMS"), na.rm = TRUE) / 0.1257) %>%
        ungroup()
      
      return(data)
    }
    
  }
  

  ## 1.3.4. Calcul de la densite de BV >30 cm DBH ##
  
  
  if(TRUE) {
    
    #Le calcul de la densite de BV permet de caractériser un indice d'encombrement du milieu
    #(milieu très dense ou pas du tout). Il est calculer selon la formule suivante afin
    #d'obtenir une desnité par hectare: 
    #Nb arbres > 30cm DBH / Surface placette soit Nb arbres > 30cm DBH/0.2157.
    
    
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
    
    
  }  
 
    
  ## 1.3.5. Calcul de la densite de BMS ##
    
    if(TRUE) {
      
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
      
    }
    
    
    
  ## 1.3.6. Calcul de la densite de BMD ##

if(TRUE) {
  
  
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
  
  
}
    

  ## 1.3.7. Calcul de la densite de BM TOTALE ##


if(TRUE) {
  
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
  
  
  
}
    

    
  
  ## 1.3.8. Calcul de la densite de souches ##
    
    
if(TRUE) {
  
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
  
}
    
    
    
  ## 1.3.9.Calcul de la densite de chandelles ##

    
if(TRUE) {
  
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
  
  
  
}

    
    
    

 ### 1.4. VOLUME ###

  ## 1.4.1. Séparation des colonnes hauteur / longueur ##

if (TRUE) {
  
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
  
}

 ## 1.4.2. Calcul de la densite propre à chaque arbre à l'ha ##


 if(TRUE) {
   
   
   #On calcule une densite propre à chaque arbre à l'ha afin de pouvoir calculer la
   #densite réelle à l'ha de chaque type d'arbres (BV, BM, BMS, BMD) afin de pouvoir calculer
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
   
   
 }

 

  ## 1.4.3. Calcul des volumes de BMD et BMS  ##

if(TRUE) {
  
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
  
  
   ## 1.4.3.1 Calcul du volume de BMD ##
  
      ## 1.4.3.1.1. Calcul du BMD par individu (m3/ind) ##
  
  volume_BMD_ind <- function(data) {
    data<-data %>%
      mutate(
        vol_BMD_ind = case_when(
          type.objet == "BMD" & !is.na(diam) & !is.na(hauteur) ~ 
            (pi / 40000 * ((diam) - (hauteur / 2 - 1.3))^2)* hauteur
        )
      ) 
  } 

  ## 1.4.3.1.2. Calcul du BMD par individu par ha (m3/ind/ha) ##
  
  volume_BMD_ha <- function(data) {
    data<-data %>%
      mutate(
        vol_BMD_ha = vol_BMD_ind * den_new
      ) 
  }
  
    ## 1.4.3.1.3.  Calcul du volume total de BMD à l'ha pour toutes les placettes ( BMD tot m3/ha) ##
  
  volume_BMD_tot <- function(data) {
    data<-data %>%
      group_by(placette) %>%
      mutate(vol_BMD_tot = sum(vol_BMD_ha, na.rm = TRUE)) %>%  
      ungroup()  
  }
 
  
  ## 1.4.3.2 Calcul du volume de BMS ##
  
  
    ## 1.4.3.2.1 Calcul du BMS par individu (m3/ind) ##
  
  volume_BMS_ind <- function(data) {
    data <- data %>%
      mutate(
        vol_BMS_ind = case_when(
          type.objet == "BMS" & !is.na(diam.med) & !is.na(longueur) ~ 
            pi / 40000 * (diam.med^2) * longueur,
          type.objet == "BMS" & (is.na(diam.med) | is.na(longueur)) ~ NA_real_,
          TRUE ~ NA_real_  # Pour les objets qui ne sont pas BMS
        )
      )
    return(data)  
  }
  
  
   ## 1.4.3.2.2 Calcul du BMS par individu par ha (m3/ind/ha) ##
  
  volume_BMS_ha <- function(data) {
    data<-data %>%
      mutate(
        vol_BMS_ha = vol_BMS_ind * den_new
      ) 
  }
  
   ## 1.4.3.2.3  Calcul du volume total de BMS à l'ha pour toutes les placettes ( BMS tot m3/ha) ##
  
  volume_BMS_tot <- function(data) {
    data<-data %>%
      group_by(placette) %>%
      mutate(vol_BMS_tot = sum(vol_BMS_ha, na.rm = TRUE)) %>%  
      ungroup() 
  }
  
}


  ## 1.4.4. Calcul du volume de BM total  ##

if(TRUE) {
  
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
  
  
}

 ## 1.4.5. Calcul du volume de chandelles ##
 
 if(TRUE) {
   
 
   ## 1.4.5.1. Calcul de chandelles par individu (m3/ind) ##
 
 volume_chand_ind <- function(data) {
   data <- data %>%
     mutate(
       vol_chandelle_ind = case_when(
         Type.BM == "V" & !is.na(diam) & !is.na(hauteur) ~ 
           (pi / 40000 * ((diam) - (hauteur / 2 - 1.3))^2) * hauteur
       )
     )
   
   return(data)
 }
 
 


 ## 1.4.3.1.2 Calcul du BMD par individu par ha (m3/ind/ha) ##
 
 volume_chand_ha <- function(data) {
   data<-data %>%
     mutate(
       vol_chandelle_ha = vol_chandelle_ind * den_new
     ) 
 }


 ## 1.4.3.1.3  Calcul du volume total de BMD à l'ha pour toutes les placettes ( BMD tot m3/ha) ##
 
 volume_chand_tot <- function(data) {
   data<-data %>%
     group_by(placette) %>%
     mutate(vol_chandelle_tot = sum(vol_chandelle_ha, na.rm = TRUE)) %>%  
     ungroup()  
 }
 
}


 ### 1.5. DENDROMICROHABITATS ###

   ## 1.5.1. Diversité DMH

if(TRUE) {
  
  #On calcule la diversité des dendromicrohabitats afin de répondre à l'hypothèse
  #suivante : une plus grande diversité de dendromicrohabitats favorisera
  #la richesse spécifique globale et la richesse spécifique des espèces cavicoles
  #et insectivores en offrant une grande diversité de niches écologiques à ces
  #espèces. 
  #Il s'agit d'une analyse de divesité gamma (inter-parcelle).
  
  
  diversite_dmh <- function(data) {
    data <- data %>%  
      group_by(placette) %>%
      mutate(diversite_DMH = n_distinct(code.DMH)) %>%  # Calculer la diversité sans division
      ungroup()
    
    return(data)
  }
  

}

   ## 1.5.2. Densité de DMH / ha ##

if(TRUE) {
  
   ## 1.5.2.1 Calcul du nombre total de DMH par parcelle ##
  
  #On calcule la densité de DMH/ha afin de répondre à l'hypothèse suivante :
  #Plus la densité de DMH/ha est importante, plus les espèces spécialistes 
  #sont favorisées dans le milieu par rapport aux généralistes avec une 
  #augmentation de la disponibilité en ressources alimentaires et habitats.
  
  #Pour calculer cette desnité par hectare, il faut, dans un premier temps,
  #calculer le nombre total de DMH sur la parcelle.
  
  densite_dmh_parcelle <- function(data) {
    data <- data %>%
      mutate(nb_DMH_ligne = ifelse(is.na(code.DMH), 0,
                                   str_count(code.DMH, "-") + 1)) %>%  # compte les DMH dans chaque ligne
      group_by(placette) %>%
      mutate(nb_DMH = sum(nb_DMH_ligne)) %>%
      ungroup()
    
    return(data)
  }
  


    ## 1.5.2.2. Calcul du nombre de DMH par hectare (densité par ha)

densite_dmh_ha <-function (data) {
  data <- data %>%
    group_by(placette) %>%
    mutate (densite_dmh_ha = nb_DMH/0.1257)%>%
    ungroup()
  
  return(data)
}

}

  ### 1.6. TAUX DE DECOMPOSITION MOYEN ###

if(TRUE) {
  
  #On veut calculer le taux de décomposition moyen du bois mort par placette car c'est
  #un bon indicateur de biodiversité (un taux de décompositon important sur la 
  #placette favoriserait les espèces insectivores et cavicoles).
  
  #Dans le jeu de données, la colonne stade.decomp contient deux valeurs : une relative
  #à l'écorce et l'autre relative au stade décomposition. Pour étudier ce dernier,
  #nous choisissons donc d'étudier la deuxième valeur. Or, le jeu de données, dans
  #notre cas, contient des valeurs en format texte alors queles données à traiter 
  #doivent être au format numérique. Ainsi, les données situéesaprès le "-" sont
  #converties au format numérique pour être traitées.
  
  #Ensuite, une moyenne de ces valeurs est réalisée à l'échelle de la placette avec un
  #chiffre après la virgule qui est conservé.
  
 
  
  
  decompo <- function(data) {
    data <- data %>%
      mutate(stade.decomp = as.character(stade.decomp)) %>%
      mutate(decompo = sub(".*-(\\d+)", "\\1", stade.decomp)) %>%
      mutate(decompo = ifelse(decompo == "" | !grepl("^\\d+$", decompo), NA, decompo)) %>%
      mutate(decompo = as.numeric(decompo)) 
    data <- data %>%
      group_by(placette) %>%
      mutate(decompo_moyen = ifelse(all(is.na(decompo)), NA, round(mean(decompo, na.rm = TRUE), 1))) %>%
      ungroup()
    
    return(data)
  }
 
  
}
 
 ### 1.7. ALTITUDE ###
 
 
 if(TRUE){
   
   #Dans les metadonnées, on dispose de l'altitude des placettes. Le but est de les fusionner avec le 
   #jeu de données DMH par placette.
   
   #Dans Excel, un nouveau jeu de données nommé "alt" est crée en copiant
   #les colonnes "placette" et "forme" des métadonnées. La fonction ci-dessous permet 
   #merge ce dataframe au jeu de données initial "Saisie_DMH_2024"et ce, par placette.
   
   #On met les colonnes placettes au format "character" car il arrive que les colonnes
   #placettes soient au format numérique dans un jeu de données et au format character
   #dans l'autre. En faisant ça, on empêche les erreurs de jointure.
   
   #La fonction left_join est issue du package dplyr et permet de réaliser une jointure
   #dite "gauche". Toutes les lignes du jeux de données sont conservées 
   #et uniquement les colonnes de "forme_forestiere" seront ajoutées au jeu de données
   #initial s'il y a une correspondance.
   
   
   
   merge_altitude <- function(data) {
     data <- data %>%
       mutate(placette = as.character(placette))
     alt <- alt %>%
       mutate(placette = as.character(placette))
     data <- data %>%
       left_join(alt, by = "placette")
     
     return(data)
   }
 }
 test <- merge_altitude (data_dmh)
 ### 1.8. TOPOGRAPHIE ###
 
 if(TRUE){
   
   #Dans les metadonnées, on dispose de la topographie des placettes. Le but est de les fusionner avec le 
   #jeu de données DMH par placette.
   
   #Dans Excel, un nouveau jeu de données nommé "topo" est crée en copiant
   #les colonnes "placette" et "topo" des métadonnées. La fonction ci-dessous permet 
   #merge ce dataframe au jeu de données initial "Saisie_DMH_2024"et ce, par placette.
   
   #On met les colonnes placettes au format "character" car il arrive que les colonnes
   #placettes soient au format numérique dans un jeu de données et au format character
   #dans l'autre. En faisant ça, on empêche les erreurs de jointure.
   
   #La fonction left_join est issue du package dplyr et permet de réaliser une jointure
   #dite "gauche". Toutes les lignes du jeux de données sont conservées 
   #et uniquement les colonnes de "forme_forestiere" seront ajoutées au jeu de données
   #initial s'il y a une correspondance.
   
   
   
   merge_topo<- function(data) {
     data <- data %>%
       mutate(placette = as.character(placette))
     topo <- topo %>%
       mutate(placette = as.character(placette))
     data <- data %>%
       left_join(topo, by = "placette")
     
     return(data)
   }
  

   
   #Cette fonction permet de convertir les 1,2,3.. par leur typologie associée afin d'avoir
   #plus de clarté dans le jeu de données et pas uniquement des chiffres qui peuvent
   #paraitre abstrait si l'on a pas la classification utilisée en tête.
   
   #Ici mutate permet de modifier la colonne "topo" en character et remplace les chiffres
   #par leur correspondance textuelle.
   
   
   forme_topo<- function(data) {
     data <- data %>%
       mutate(
         topo = case_when(
           topographie == 1 ~ "Terrain plat",
           topographie == 2 ~ "Sommet",
           topographie == 3 ~ "Haut versant",
           topographie == 4 ~ "Bas versant",
           topographie == 5 ~ "Mi versant",
           topographie == 6 ~ "Depression ouverte",
           topographie == 7 ~ "Depression fermee",
           TRUE ~ as.character(topographie)  # Conserve les valeurs existantes si elles ne sont pas 1 à 6
         )
       )
     return(data)
   }
 
 }
 
 ### 1.9. PENTE ###
 
 if(TRUE){
   
   #Dans les metadonnées, on dispose de la pente des placettes. Le but est de les fusionner avec le 
   #jeu de données DMH par placette.
   
   #Dans Excel, un nouveau jeu de données nommé "pente" est crée en copiant
   #les colonnes "placette" et "pente" des métadonnées. La fonction ci-dessous permet 
   #merge ce dataframe au jeu de données initial "Saisie_DMH_2024"et ce, par placette.
   
   #On met les colonnes placettes au format "character" car il arrive que les colonnes
   #placettes soient au format numérique dans un jeu de données et au format character
   #dans l'autre. En faisant ça, on empêche les erreurs de jointure.
   
   #La fonction left_join est issue du package dplyr et permet de réaliser une jointure
   #dite "gauche". Toutes les lignes du jeux de données sont conservées 
   #et uniquement les colonnes de "forme_forestiere" seront ajoutées au jeu de données
   #initial s'il y a une correspondance.
   
   
   
   merge_pente<- function(data) {
     data <- data %>%
       mutate(placette = as.character(placette))
     pente <- pente %>%
       mutate(placette = as.character(placette))
     data <- data %>%
       left_join(pente, by = "placette")
     
     return(data)
   }
   
 }
 
 ### 1.10. EXPOSITION ###
 
 if(TRUE){
   
   #Dans les metadonnées, on dispose de l'exposition des placettes. Le but est de les fusionner avec le 
   #jeu de données DMH par placette.
   
   #Dans Excel, un nouveau jeu de données nommé "exposition" est crée en copiant
   #les colonnes "placette" et "exposition" des métadonnées. La fonction ci-dessous permet 
   #merge ce dataframe au jeu de données initial "Saisie_DMH_2024"et ce, par placette.
   
   #On met les colonnes placettes au format "character" car il arrive que les colonnes
   #placettes soient au format numérique dans un jeu de données et au format character
   #dans l'autre. En faisant ça, on empêche les erreurs de jointure.
   
   #La fonction left_join est issue du package dplyr et permet de réaliser une jointure
   #dite "gauche". Toutes les lignes du jeux de données sont conservées 
   #et uniquement les colonnes de "forme_forestiere" seront ajoutées au jeu de données
   #initial s'il y a une correspondance.
   
   
   
   merge_expo<- function(data) {
     data <- data %>%
       mutate(placette = as.character(placette))
     expo <- expo %>%
       mutate(placette = as.character(placette))
     data <- data %>%
       left_join(expo, by = "placette")
     
     return(data)
   }
   
 }

   ### 1.11. NETTOYAGE JEU DE DONNEES ###

if(TRUE) {
  
    ## 1.11.1. Suppression des colonnes qui ne sont plus indispensables dans l'analyse de données ##
  
  nettoyage <- function(data){
    data <- data %>%
      dplyr::select (-arbre, 
             -Code, 
             -diam, 
             -`longueur / hauteur`,   
             -Type.BM, 
             -Remarque, 
             -diam.ini, 
             -diam.fin, 
             -diam.med, 
             -nombre_total_BV, 
             -nombre_total_BMS, 
             -nombre_total_BMD, 
             -nombre_total_BM, 
             -hauteur, 
             -longueur, 
             -den_new, 
             -vol_BMD_ind, 
             -vol_BMD_ha, 
             -vol_BMS_ind, 
             -vol_BMS_ha, 
             -nb_DMH_ligne,
             -nb_DMH,
             -code.DMH,
             -decompo,
             -Nom_essence,
             -Typologie,
             -classe_bois,
             -stade.decomp,
             -type.objet,
             -vol_chandelle_ind,
             -topographie,
             -vol_chandelle_ha
             )
    
    return(data)
  }
  
  ## 1.7.2. Garder une ligne par placette ##
  
  #Dans cette fonction, nous souhaitons conserver uniquement une ligne par placette
  #afin d'avoir un jeu de donées propre (et non pas une ligne pour chaque arbre comme
  #il s'agissait dans le jeu de données initial). 
  
  #Par ailleurs, nous en profitons pour mettre le jeu de données au propre en gardant
  #des chiffres ronds pour les densités (dire qu'il y a 7.5 arbres par ha n'a pas 
  #beaucoup de sens). Par ailleurs, nous arrondissons à deux chiffres après la virgule
  #pour les volumes afin de ne pas avoir des données du type 52.3669523665.
  
  #Pour cela, nous nommons un objet qui contient toutes les colonnes auxquelles nous
  #souhaitons appliquer ces réductions décimales. Nous les appelons ensuite afin de 
  #leur appliquer la fonction floor déjà utilisée précédemment ou bien "round . , 2"
  #qui permet de garder deux chiffres après la virgule.
  
  ligne <- function(data) {
    data <- data %>%
      distinct(placette, .keep_all = TRUE)
    colonnes_densite <- c("densite_GBV", "densite_GBM", "densite_BMS", "densite_BV", "densite_BMD", "densite_BM_tot", "densite_souche", "densite_A", "densite_V","densite_dmh_ha", "decompo_moyen")
    colonnes_volumes <- c("vol_BMD_tot", "vol_BMS_tot", "vol_BM_tot")  
    data <- data %>%
      mutate(across(all_of(colonnes_densite), ~ floor(.)))  
    data <- data %>%
      mutate(across(all_of(colonnes_volumes), ~ round(., 2)))  
    
    return(data)
  }
  

}

   ### 1.12. FONCTION FINALE POUR CALCULER VARIABLES STUCTURELLES ###

if(TRUE) {

#Ici, ff_dmh est la fonction finale qui regroupe toutes les fonctions créées ci-dessus.
#Si elle est fonctionnelle et que les jeux de données ont bien les mêmes noms de
#données d'entrée, il suffit d'utiliser la ligne de code suivante :

#nom_du_jeu_donnees_final<-ff_dmh(nom_jeu_de_donnes_initial) 


ff_dmh <- function(data) {
  data <-merge_forme_forestiere(data)
  data <-forme_foret(data)
  data <-Typologie_essence_code(data)
  data <- Typologie_essence(data)           # Applique la fonction culture_foret
  data <- merge_altitude(data)
  data <- merge_topo(data)
  data <- forme_topo (data)
  data <- merge_pente(data)
  data <- merge_expo(data)
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
  data <- volume_chand_ind(data)
  data <- volume_chand_ha(data)
  data <- volume_chand_tot(data)
  data <- volume_BM_tot(data)          # Applique la fonction volume_BM_tot
  data <- diversite_dmh (data)
  data <- densite_dmh_parcelle (data)
  data <- densite_dmh_ha (data)
  data <- decompo (data)
  data <- nettoyage (data)
  data <- ligne (data)
  return(data)
}



  ## 1.7.1. Application de la fonction finale au jeu de données initial et enregistrement

dmh_filtered<-ff_dmh(data_dmh)
write.xlsx(dmh_filtered, file = file_output_dmh)


}
 
 
#### 2. JEU COUVERT ####




   ### 2.0. Definition des modalités d'entrée et de sortie des jeux de données DMH ###

if(TRUE) {
  
  ## ENTREE
  
  # Dossier contenant le jeu de données DMH
  Input_couvert_case_file <-file.path(Input_case_file_ventoux, "Input_couvert")
  
  # Deux .XLSX contenant les données DMH. sheet = Data permet de sélectionner la feuille "Data" parmi les 3 autres.  
  file_data_couvert <-read_excel(file.path(Input_couvert_case_file, "Saisie_couvert_2024_corrige.xlsx"))
  file_data_CodEss_couvert <-read_excel(file.path(Input_couvert_case_file, "CodEss.xlsx"))
  data_couvert<-file_data_couvert
  
  
  ## SORTIE
  
  # Un .XLSX contenant les données DMH propres
  file_output_couvert_filtered <- file.path(Output_file_ventoux, "data_couvert_filtered")
  if (!dir.exists(file_output_couvert_filtered)) {
    dir.create(file_output_couvert_filtered, recursive = TRUE)
  }
  file_output_couvert <- file.path(file_output_couvert_filtered, paste0(SITE, "_couvert_filtered.xlsx"))
  
  
}

   ### 2.1. ESSENCES ARBRES ###

if(TRUE) {
  
  #Comme pour le premier jeu de données "DMH", on associe à chaque code essence, son
  #essence en nom vernaculaire afin de mieux s'y reprérer lorsqu'on ne connaît pas le 
  #code en question.
  
  essence_code <- function (data) {
    data <-data%>%
      left_join(CodEss, by = "Code") %>%
      return (data)
  }

  
  Typologie_essence_2 <- function(data) {
    
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
                   "Sapin de Nordmann", "Sapin de Vancouver", "Sapin pectiné", "Résineux divers", "Pin pignon")
    
    # Fusion et ajout de la typologie
    data <- data %>%
      mutate(Typologie = case_when(
        Nom_essence %in% feuillus ~ "Feuillu",
        Nom_essence %in% coniferes ~ "Conifère",
        TRUE ~ "Indéterminée"
      ))
    
    return(data)
  }
  
  
}
 


   ### 2.2. STRATIFICATION VERTICALE ###

if(TRUE) {
  
  #Pour traiter la stratification verticale de la forêt, les données initiales de
  #hauteur de couvert ont été simplement remplacées par des lettres (0-25 : a, 25-50 : b
  #50-1 : c....). Ceci a été réalisé afin de limiter les erreurs potentielles
  #générées dans le code du fait que les noms des colonnes soient des chiffres.
  
  #La fonction ci-dessous permet de regrouper les différents étages de végétation 
  #en différentes classes comme suit :
  
  #                     Strate herbacée : 0.- 50 cm (a + b)
  #                     Strate sous-arbustive : 50 cm - 2 m (c + d)
  #                     Strate arbustive : 2 m - 8 m (e + f)
  #                     Strate arborée : > 8 m (g+h)
  
  #D'après GILLET (2000).
  
  #Les % de recouvrement des différentes strates ont donc été additionnés ce qui donne
  #parfois des pourcentages > 100%. Un indice supérieur à 100% signifie que les deux 
  #strates se chevauchent. Ainsi un % > à 100% signifie que la stratification verticale
  #est importante.
  
  colonnes_strates <- function(data) {
    data$strate_h <- data$a + data$b
    data$strat_sa <- data$c + data$d
    data$strat_a <- data$e + data$f
    data$strat_A <- data$g + data$h
    return(data)
  }
  

}


   ### 2.3. SURFACE TERRIERE ###


if(TRUE) {
  
  #Dans le jeu de données, la colonne N correspond à la surface terrière de chaque arbre.
  #Plus haut, nous avons associé à chaque arbre une classification conifère/feuillu. 
  #Ainsi,nous sommes en mesure de définir une surface terrière propre à chaque 
  #classification.L'idée est d'ensuite convertir cette surface terrière en % d'occupation 
  #placette.
  
  #Dans le jeu de données initial, la colonne N était en caractère, une conversion en
  #numérique a donc été réalisée avec la fonction as.numeric.
  #On fait la somme des surfaces terrières par typologie et par placette (group_by 
  #placette et typologie). Un nouveau data.frame est crée contenant les placettes et
  #les surfaces terrières par classification. La fonction pivot.wider a permis d'
  #obtenir une donnée par placette (au lieu d'avoir 51 : 12.5 51 : 12.5, 51 : 12.5...
  #on a une fois la donnée.)
  
  
  #Pour la conversion en %, la surface terriere totale de la placette est calculée
  #en sommant l'ensemble des surfaces terrieres. la fonction across(starts_with("st_"))
  #permet de sélectionner les colonnes dont le nom commence par "st_" (permet de limiter 
  #la sélection manuelle). rowSums(..., na.rm = TRUE) permet de faire la somme des 
  #de chauqe ligne à partir des colones spécifiées et ce, pour chaque placette. Avec
  #la fonction mutate, une nouvelle colonne est également ajoutée dans le tableau de 
  #donnée contenant la surface terriere total de chaque parcelle.
  
  #Ensuite, la conversion en % est effectuée. Pour cela, la surface terriere de chaque
  #classification est divisée par 100. Grâce à la fonction mutate, deux nouvelles 
  #colonnes sont crées "pct_st_conifere" et "pct_st_feuillu". La fonction round permet 
  #d'avoir un chiffre après la virgule.
  
  #Ensuite, on merge ce nouveau data.frame au jeu de données initial grâce à 
  #"left_join". Le names_prefix permet de faire suivre le préfixe "st_" par la 
  #classification associée à la donnée de st.
  
  #/!\ Suppression des NA qui n'en sont pas mais qui correspondent en réalité à des 0,
  #ce qui fausse les calculs.
  
  st_cf <- function(data) {
    data <- data %>%
      mutate(N = as.numeric(N))
    
    st_data <- data %>%
      group_by(placette, Typologie) %>%
      summarise(surface_terriere = sum(N, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = Typologie, values_from = surface_terriere, names_prefix = "st_") %>%
      mutate(across(starts_with("st_"), ~ replace_na(., 0))) %>%
      mutate(st_total = rowSums(across(starts_with("st_")), na.rm = TRUE)) %>%
      mutate(across(starts_with("st_"), ~ round((. / st_total) * 100, 1), .names = "pct_{.col}"))
    
    # Calcul des richesses spécifiques
    rs_data <- data %>%
      group_by(placette) %>%
      summarise(
        rs_feuillu = n_distinct(Nom_essence[Typologie == "Feuillu" & !is.na(Nom_essence)]),
        rs_conifere = n_distinct(Nom_essence[Typologie == "Conifère" & !is.na(Nom_essence)])
      )
    
    data <- data %>%
      left_join(st_data, by = "placette") %>%
      left_join(rs_data, by = "placette")
    
    return(data)
  }
  
  
  
  #Création d'une colonne déterminant le type de forêt (feuillu, mixte ou conifère). D'après l'IGN forestier,
  #Les forêts sont considérées comme "pures" dès lors que la présence d'un type (conifère ou feuillu) est 
  # > 75%. Le reste est considéré comme forêt mixte.
  

  
  typo_foret <- function(data) {
  data <- data %>%
    mutate(type_foret = case_when(
      pct_st_Conifère > 75 ~ "conifère",
      pct_st_Feuillu > 75 ~ "feuillu",
      TRUE ~ "mixte"
    ))
  }
  
}


   ### 2.4. DENSITE CANOPEE ###


 if(TRUE) {
   
   #La densité de la canopée a été calculée grâce à un densiomètre convexe. Les mesures
   #ont été prises aux 4 points cardinaux. Les valeurs sont pour 96, donc besoin de faire
   #*1.04 pour avoir les valeurs en %. Les valeurs ont donc d'abord été additionnées et
   #*#divisées par 4 afin d'en obtenir une moyenne. Cette moyenne a été ensuite multipliée
   # par 1.04. On groupe les données une première fois pour la moyenne, on les dégroupe
   #avant de les regrouper par placette pour faire le calcul *1.04 puis de les 
   #dégrouper de nouveau.
   
   #Les données obtenues sont la desnité du couvert arboré en %.
   
   canopy <- function(data) {
     data <- data %>%
       mutate(can_moy = rowMeans(across(c(No, Su, Es, Ou)), na.rm = TRUE)) %>%
       ungroup() %>%
       group_by(placette) %>%
       mutate(densite_canopy = 1.04 * mean(can_moy, na.rm = TRUE)) %>%
       ungroup()
     
     return(data)
   }
   
 }




   ### 2.5. CALCUL DE LA RICHESSE SPECIFIQUE ### 
 
     ## 2.5.1. Richesse spécifique totale ##

if(TRUE) {
  
  richesse_spe <- function(data) {
    data %>%
      group_by(placette) %>%           
      mutate(richesse_spe_arbre = n_distinct(Nom_essence)) %>%  
      ungroup()                             
    
  }
  
}



   ### 2.6. NETTOYAGE JEU DE DONNES COUVERT ###


    ## 2.6.1. Nettoyage du jeu de données ##

if (TRUE) {
  
  #Comme pour le jeu de données "dmh", un nettoyage s'impose afin de ne garder que les
  #colonnes qui nous intéressent. La fonction select a été utilisée pour ceci. Par ailleurs,
  #une colonne "observateur" a été ajoutée en fusionnant les colonnes "eq1", "eq 2", "eq3",
  #et "eq4" afin de pouvoir tenir compte du biais observateur lors des analyses futures.
  
  #Enfin, une seule ligne par placette a été conservée afin d'avoir un jeu de données 
  #propre.
  
  bd_couvert_propre<-function(data)  {
    data <- data %>%
      mutate(observateur = apply(., 1, function(x) paste(na.omit(x[c("Eq1", "Eq2", "Eq3", "Eq4")]), collapse = " ")))
    data <- data %>%
      dplyr :: select(-a, 
             -b, 
             -c, 
             -d, 
             -e, 
             -f, 
             -g, 
             -h, 
             -No, 
             -Es, 
             -Su, 
             -Ou, 
             -Code, 
             -Dia, 
             -can_moy,
             -N, 
             -Comment, 
             -Tri, 
             -Noteur, 
             -Eq1, 
             -Eq2, 
             -Eq3, 
             -Eq4, 
             -H_Arr, 
             -H_Dep, 
             -P.A.oto,
             -AP.O.,
             -st_Conifère,
             -st_Feuillu,
             -st_Indéterminée,
             -st_total,
             -pct_st_Indéterminée,
             -pct_st_total,-Typologie,-Nom_essence)
    return(data)
  }
  
  
}


  ## 2.6.2. Conservation d'une ligne par placette ##


if(TRUE) {
  
  ligne_couvert <- function(data) {
    data <- data %>%
      distinct(placette, .keep_all = TRUE)
    return(data)
  }
  
}


  ### 2.7. FONCTION FINALE COUVERT ###

if(TRUE) {
  
  ff_couvert<-function(data) {
    data<- essence_code (data)
    data<-Typologie_essence_2(data)
    data<-colonnes_strates(data)
    data<- st_cf(data)
    data<- typo_foret(data)
    data<-canopy (data)
    data<-richesse_spe(data)
    data<-bd_couvert_propre(data)
    data<-ligne_couvert(data)
    
    return(data)
  }

  ## 2.7.1. Application de la fonction finale au jeu de données initial et enregistrement
  
  couvert_filtered<-ff_couvert(data_couvert)
  write.xlsx(couvert_filtered, file = file_output_couvert)
  
}
 
 
#### 3. FUSION DES JEUX DE DONNEES COUVERT ET DMH ####

   ### 3.0. Definition des modalités de sortie des jeux de données couvert_dmh_merged ###

if(TRUE) {
  
  ## SORTIE
  
  # Un .XLSX contenant les données DMH propres
  file_output_merged <- file.path(Output_file_ventoux, "data_merged")
  if (!dir.exists(file_output_merged)) {
    dir.create(file_output_merged, recursive = TRUE)
  }
  file_output_merged <- file.path(file_output_merged, paste0(SITE,"_couvert_dmh_merged.xlsx"))
  
  
}


   ### 3.1. FUSION DE DATA_DMH ET DATA_COUVERT

if(TRUE) {
  
  #La colonne placette doit être en numérique. Or elle était en caractère dans l'un 
  #et en "double" dans l'autre ce qui empêche la fusion. Ainsi, nous les convertissons
  #toutes les deux en numérique.
  
  couvert_filtered$placette <- as.numeric(couvert_filtered$placette)
  dmh_filtered$placette <- as.numeric(dmh_filtered$placette)
  ventoux_couvert_dmh_merged <- left_join(couvert_filtered, dmh_filtered, by = "placette")
  write.xlsx(ventoux_couvert_dmh_merged, file = file_output_merged)
  
}







#--------------------------------------------------------------------------------------
#--------------------------------------II. CUICUI--------------------------------------
#--------------------------------------------------------------------------------------

   #### 4. JEU OISEAUX ####


   ### 4.0. Definition des modalités d'entrée et de sortie des jeux de données DMH ###

if(TRUE) {
  
  ## ENTREE
  
  # Dossier contenant les jeux de données à manipuler sur les oiseaux
  Input_bird_case_file <-file.path(Input_case_file_ventoux, "Input_oiseaux")
  
  # Trois .XLSX contenant les données DMH. sheet = Data permet de sélectionner la feuille "Data" parmi les 3 autres. 
  file_data_avonet<-file.path(Input_bird_case_file, "avonet.xlsx")
  file_data_nidification<-file.path(Input_bird_case_file, "Nidification.xlsx")
  file_data_sites_sp<-file.path(Input_bird_case_file, "sites_sp.xlsx")
  file_data_code_latin<-file.path(Input_bird_case_file, "code_latin.xlsx")
  file_data_bird <-read_excel(file.path(Input_bird_case_file, "Saisie_oiseaux_2024.xlsx"), sheet = "DATA")
  file_data_spe <- file.path(Input_bird_case_file, "specialisation.xlsx")
  
  data_bird<- file_data_bird
  
  ## SORTIE
  
  # Un .XLSX contenant les données bird propres
  file_output_bird_filtered <- file.path(Output_file_ventoux, "data_bird_filtered")
  if (!dir.exists(file_output_bird_filtered)) {
    dir.create(file_output_bird_filtered, recursive = TRUE)
  }
  file_output_bird <- file.path(file_output_bird_filtered, paste0(SITE,"_bird_filtered.xlsx"))
  
  ## LECTURE DES JEUX DE DONNEES DANS L'ENVIRONNEMENT.
  
  #Ici, avonet1 est un jeu de donnée créée en récupérant la colonne "sp_latin"
  #de la classification avonet. Les colonnes spécialisation et alimentation de 
  #cette même classification ont été copiées et collées dans ce nouveau data.frame
  #Grâce à cette fcontion, on appelle ce jeu de données afin, plus tard, de le
  #merge avec notre jeu de données initial.
  avonet = read_excel(file_data_avonet) 
  
  #Chargement du deuxième jeu de données "Nidification" qui va permettre de 
  #créer le jeu de données final. Les données de nidification sont issues de
  #la base de données "Life charcteristics of European birds".
  nidification=read_excel(file_data_nidification)
  
  #Chargement du jeu de données créée sur le terrain recensant les noms 
  #vernaculaires et les codes STOC des espèces identifiées sur le site du Ventoux
  sp_sites = read_excel (file_data_sites_sp)
  
  #Chargement du jeu de données associant les codes STOC des espèces à leur
  #nom latin.
  
  code_stoc_latin = read_excel (file_data_code_latin)
  
  #Chargement du jeu de données associant la spécialisation des espèces à leur
  #nom latin.
  
  specialisation = read_excel (file_data_spe)

}

   ### 4.1. CREATION DU JEU DE DONNEES INITIAL (NIDIFICATION, SPECIALISATION, ALIMENTATION + SAISIE_OISEAUX_2024)


    ## 4.1.1. Ajout des noms vernaculaires  aux codes du jeu de données "Saisie_2024_Oiseaux" ##


if(TRUE) {

  
  #Dans cette partie, il s'agit de construire notre jeu de données "oiseaux" final. 
  #Pour cela, un classeur excel (sp_sites) recensant les espèces vues et entendues sur le
  #site du ventoux et y associant leur code_stoc  a été créée. Dans notre jeu de
  #données initial (data_bird), les espèces sont recensées uniquement via leur
  #nom de code STOC. Pour s'y retrouver parmi les accronymes latin, nous allons
  #grâce à la fonction suivant, fusionner les deux tableaux avec pour variable
  #commune les codes STOC. Ceci permet ainsi d'avoir les noms vernaculaires
  #associés aux codes STOC dans notre jeu de données final. 
  

 merge_verna_stoc <- function (data) {
   data <-data%>%
   left_join(sp_sites, by = "code_stoc") %>%
   return (data)
 }

}

    ## 4.1.2. Ajout des noms latins  aux codes du jeu de données "Saisie_2024_Oiseaux" ##

if(TRUE) {
  
  #Les jeux de données contenant les informations de nidification, d'alimentation et 
  #de spéicialisation des espèces sont associées aux noms latins des espèces.
  #Ainsi, pour pouvoir fusionner les deux tableaux avec notre tableau de 
  #données "data_bird", il est nécesaire d'y ajouter une colonne "nom_latin".
  #Pour cela, nous avons créer un jeu de données (code_latin) associant les
  #noms latins au code STOC des espèces. Ce jeu de données contient les
  #informations pour toutes les espèces européennes. Or, grâce à la fusion par 
  #les code STOC, nous aurons dans notre data_bird, uniquement les espèces 
  #nous concernant.
  
  
  merge_latin_stoc <- function (data) {
    data <-data%>%
      left_join(code_stoc_latin, by = "code_stoc") %>%
      return (data)
  }
}


   ## 4.1.3. Ajout des modes de nidification, d'alimentation et la spécialisation des espèces à data_bird

if(TRUE) {
  
  ## 4.1.3.1. Ajout des guildes de nidification ##
  
  #Désormais, nous disposons de 3 jeux de données : data_bird, avonet et 
  #nidification. Les données des deux derniers sont associées au nom latin des
  #espèces. Ainsi, nous allons pouvoir les fusionner à data_bird grâce à la
  #colonne "sp_latin".
  
  #/!\ Certaines espèces manquantes dans la BD nidification des oiseaux européens
  #ont été ajoutées manuellement au jeu de données grâce au guide ornitho 
  #Delachaux. Les voici :
  
          #Mésange huppée : Cavicole
          #Verdier d'Europe : Arboricole
          #Linotte mélodieuse : Arboricole
  
  
  merge_guilde_nidification <- function (data) {
    data <-data%>%
      left_join(nidification, by = "sp_latin") %>%
      return (data)
  }


  ## 4.1.3.2. Ajout des guildes d'alimentation ##
  
  #Pour l'alimentation, 3 classification ont été créées : Omnivore, herbivore
  #et insectivore. 
  
  # Les insectivores sont les espèces dont le régime alimentaire est composée à 
  #minimum 70% d'insectes (invertébrés aquatiques ou terrestres).Ainsi, le 
  #rougequeue noir qui est un prédateur aquatiquea été catégorisé en insectivore.
  
  #Les omnivores sont les espèces se nourrissant autant d'insectes, que de vertébrés
  #que de graines.
  
  #Les herbivores regroupent les espèces frugivores, nectarivores et herbivores
  #dont le régime alimentaire est composé à au moins 70% des produits associés.
  #(d'après Avonet).
  
  
  merge_guilde_alim<- function (data) {
    data <-data%>%
      left_join(avonet, by = "sp_latin") %>%
      return (data)
  }

  

 ## 4.1.3.3. Ajout des guildes de spécialisation ##
 
 
 #Pour la spécialisation, il y a au total 2 grandes guildes tirées de Bouvet & al (2016) :
 
 # (i) Spécialistes forestières : Espèces inféodée au milieu forestier
 

 # (ii) Généralistes : Espèces n'ayant pas de lifestyle particulièrement lié au milieu forestier
 
 
  merge_guilde_spe<- function (data) {
    data <-data%>%
      left_join(specialisation, by = "sp_latin") %>%
      return (data)
  }
}

  ### 4.2. CALCUL DE L'ABONDANCE PAR GUILDE ###


    ## 4.2.1. NIDIFICATION ##

if (TRUE) {
  
  
     # 4.1.1.1 Cavicole
  
  ab_cavi <- function(data) {
    data<-data %>%
      group_by(placette) %>%           
      mutate(ab_cav = sum(Nidification == "Cavicole", na.rm = TRUE)) %>% 
      ungroup()
    
    return(data)
  }
  
  
  
   # 4.1.1.2 Sol
  ab_sol <- function(data) {
    data<-data %>%
      group_by(placette) %>%           
      mutate(ab_sol = sum(Nidification == "Sol", na.rm = TRUE)) %>% 
      ungroup()
    
    return(data)
  }
  
  
   # 4.1.1.3 Arboricole
  
  ab_arb <- function(data) {
    data<-data %>%
      group_by(placette) %>%           
      mutate(ab_arb = sum(Nidification == "Arboricole", na.rm = TRUE)) %>% 
      ungroup()
    
    return(data)
  }
  
  
}


   ## 4.2.2. ALIMENTATION ##

if (TRUE) {
  
  
   # 4.2.2.1. Insectivore
  
  ab_ins <- function(data) {
    data<-data %>%
      group_by(placette) %>%           
      mutate(ab_ins = sum(alimentation == "Insectivore", na.rm = TRUE)) %>% 
      ungroup()
    
    return(data)
  }
  
  
   # 4.2.2.2. Omnivores
  ab_omn <- function(data) {
    data<-data %>%
      group_by(placette) %>%           
      mutate(ab_omn = sum(alimentation == "Omnivore", na.rm = TRUE)) %>% 
      ungroup()
    
    return(data)
  }
  
  
  
  
   # 4.2.2.3. Herbivore
  ab_herb <- function(data) {
    data<-data %>%
      group_by(placette) %>%           
      mutate(ab_herb = sum(alimentation == "Herbivore", na.rm = TRUE)) %>% 
      ungroup()
    
    return(data)
  }
  
  
}

   ## 4.2.3. Spécialisation ##

 ab_gen <- function(data) {
   data<-data %>%
     group_by(placette) %>%           
     mutate(ab_gen = sum(specialisation == "G", na.rm = TRUE)) %>% 
     ungroup()
   
   return(data)
   
 }
 
   ab_specialist <- function(data) {
     data<-data %>%
       group_by(placette) %>%           
       mutate(ab_specialist = sum(specialisation == "F", na.rm = TRUE)) %>% 
       ungroup()
     
     return(data)
  
   }
 

   ### 4.3. CALCUL DE LA RICHESSE SPECIFIQUE PAR GUILDE ###



    ## 4.3.1. Nidification ##


if(TRUE) {
  

  rs_nidif <- function(data) {
    rs_values <- data %>%
      group_by(placette, Nidification) %>%
      summarise(rs = n_distinct(nom_vernaculaire), .groups = "drop") %>%
      pivot_wider(names_from = Nidification, values_from = rs, names_prefix = "rs_", values_fill = list(rs = 0))
    data <- data %>%
      left_join(rs_values, by = "placette")
    
    return(data)
  }
}


    ## 4.3.2. Alimentation ##


if(TRUE) {
  
  rs_alim <- function(data) {
    rs_values <- data %>%
      group_by(placette, alimentation) %>%
      summarise(rs = n_distinct(nom_vernaculaire), .groups = "drop") %>%
      pivot_wider(names_from = alimentation, values_from = rs, names_prefix = "rs_", values_fill = list(rs = 0))  
    
    data <- data %>%
      left_join(rs_values, by = "placette")
    
    return(data)
  }

}


    ## 4.3.3. Spécialisation ##


if(TRUE) {
  
  rs_spe <- function(data) {
    rs_values <- data %>%
      group_by(placette, specialisation) %>%
      summarise(rs = n_distinct(nom_vernaculaire), .groups = "drop") %>%
      pivot_wider(names_from = specialisation, values_from = rs, names_prefix = "rs_", values_fill = list(rs = 0))  
    
    data <- data %>%
      left_join(rs_values, by = "placette")
    
    return(data)
  }
}

  
   ### 4.4. CALCUL DE LA RICHESSE SPECIFIQUE GLOBALE PAR PLACETTE ###


if(TRUE) {
  
  rs_bird <- function(data) {
    data <- data %>%
      group_by(placette) %>%
      mutate(rs_tot = n_distinct(nom_vernaculaire)) %>%
      ungroup() 
    
    return(data)
  }
  
}

 
    ### 4.5. NETTOYAGE DU JEU DE DONNEES OISEAUX ###



      ## 4.5.1. Nettoyage du jeu de données pour tableau 1 ligne par parcelle ##

        ## 4.5.1.1. Nettoyage

if (TRUE) {
  
  #Comme pour les jeux de données précédents, un nettoyage s'impose afin de ne garder que les
  #colonnes qui nous intéressent. La fonction select a été utilisée pour ceci. 
  
  #La colonne observateur a été supprimée car c'est toujours la même personne qui a effectué
  #les points d'écoute (Thais).
  
  #Enfin, une seule ligne par placette a été conservée afin d'avoir un jeu de données 
  #propre.
  
  bd_oiseaux_propre<-function(data)  {
    data <- data %>%
    dplyr ::  select(-Obs,
             -Date,
             -Pluie,
             -Vent,
             -Visibilite,
             -Neige,
             -Nuage,
             -code_stoc,
             -`0-5min`,
             -`5-10min`,
             -`10-15min`,
             -`15-20min`,
             -rs_NA,
             -Type_Contact,
             -Classe_distance,
             -sp_latin,
             -nom_vernaculaire,
             -Nidification,
             -alimentation,
             -rs_NA.x,
             -rs_NA.y,
             -specialisation
             )
    return(data)
  }
  
}

   ## 4.5.1.2. Conservation d'une seule ligne par placette ##

if(TRUE) {
  
  ligne_bird <- function(data) {
    data <- data %>%
      distinct(placette, .keep_all = TRUE)
    return(data)
  }
  
}

  ## 4.5.1.3. Fonction finale 1 ##

if(TRUE) {
  
  ff_oiseaux1<-function(data) {
    data<-merge_verna_stoc (data)
    data<-merge_latin_stoc(data)
    data<- merge_guilde_nidification(data)
    data<-merge_guilde_alim (data)
    data<-merge_guilde_spe (data)
    data<-ab_cavi(data)
    data<-ab_ins(data)
    data<-ab_arb(data)
    data<-ab_sol(data)
    data<-ab_herb(data)
    data<-ab_omn(data)
    data<-ab_specialist(data)
    data<-ab_gen(data)
    data<-rs_spe(data)
    data<-rs_alim(data)
    data<-rs_nidif(data)
    data<-rs_bird(data)
    data<-bd_oiseaux_propre(data)
    data<-ligne_bird(data)
    
    return(data)
  }
  
  bird_filtered<-ff_oiseaux1(data_bird)
  write.xlsx(bird_filtered, file = file_output_bird)
  
}
 
 
 

   #### 5. BD FINALE (1) ####
 
        ### 5.1. Définition des paramètres de sortie ###

if(TRUE) {
  
  
  ## SORTIE
  
  file_output_merged <- file.path(Output_file_ventoux,"data_merged")
  if (!dir.exists(file_output_merged)) {
    dir.create(file_output_merged, recursive = TRUE)
  }
  file_output_merged_BD1 <- file.path(file_output_merged,paste0(SITE, "_BD1.xlsx"))
  
  
}

        ### 5.2. Fusion de data_dmh_couvert et data_bird ###

if(TRUE) {
  
  BD1 <- left_join(bird_filtered, ventoux_couvert_dmh_merged, by = "placette")
  write.xlsx(BD1, file = file_output_merged_BD1)
  
}


   #### 6. BD FINALE (2) ####
 
    ### 6.1. Définition des paramètres de sortie ###

 if(TRUE) {
   
   
   ## SORTIE
   
   file_output_merged <- file.path(Output_file_ventoux,"data_merged")
   if (!dir.exists(file_output_merged)) {
     dir.create(file_output_merged, recursive = TRUE)
   }
   file_output_merged_BD2 <- file.path(file_output_merged,paste0(SITE, "_BD2.xlsx"))
   
   
 }

   ### 5.2. Fusion de data_dmh_couvert et data_bird ###
 
 if(TRUE) {
   
   BD2 <- left_join(bird_filtered, couvert_dmh_merged, by = "placette")
   write.xlsx(BD2, file = file_output_merged_BD2)
   
 }









#AU CAS OU


 
 
 
 
 
 
 
 
 ####AU CAS OU ####
 

#SURFACE TERRIERE


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



latin <- function(data) {
  especes <- data.frame(
    sp_verna = c("Accenteur mouchet", "Alouette des champs", "Alouette lulu", "Autour des Palombes", "Bec croisé des sapins", 
                 "Bruant zizi", "Buse variable", "Chardonneret élégant", "Choucar des tours", "Chouette hulotte", 
                 "Circaète Jean-le-Blanc", "Corneille noire", "Coucou gris", "Etourneau sansonnet", "Fauvette à tête noire", 
                 "Fauvette barbillarde", "Fauvette passerinette", "Fauvette pitchou", "Geai des chênes", "Gobemouche gris", 
                 "Gobemouche noir", "Grimpereau des bois", "Grimpereau des jardins", "Grive draine", "Grive musicienne", 
                 "Linotte mélodieuse", "Loriot d'Europe", "Merle à plastron", "Merle noir", "Mésange charbonnière", 
                 "Mésange nonnette", "Mésange à queue longue", "Mésange bleue", "Mésange huppée", "Mésange noire", 
                 "Pic épeiche", "Pic noir", "Pic vert", "Pie bavarde", "Pigeon ramier", "Pinson des arbres", 
                 "Pouillot de Bonnelli", "Pouillot véloces", "Roitelet à triple bandeau", "Rossignol philomène", 
                 "Rouge-gorge commun", "Rougequeue à front blanc", "Rougequeue noir", "Serin cini", "Sittelle torchepot", 
                 "Tourterelle des bois", "Tourterelle turque", "Traquet motteux", "Troglodyte mignon", 
                 "Venturon montagnard", "Verdier d'Europe"),
    sp_latin = c("Prunella modularis", "Alauda arvensis", "Lullula arborea", "Accipiter gentilis", "Loxia curvirostra", 
                 "Emberiza cirlus", "Buteo buteo", "Carduelis carduelis", "Turdus merula", "Strix aluco", 
                 "Circaetus gallicus", "Corvus corone", "Cuculus canorus", "Sturnus vulgaris", "Sylvia atricapilla", 
                 "Sylvia nisoria", "Sylvia cantillans", "Sylvia undata", "Garrulus glandarius", "Muscicapa striata", 
                 "Muscicapa parva", "Certhia familiaris", "Certhia brachydactyla", "Turdus viscivorus", "Turdus philomelos", 
                 "Linaria cannabina", "Oriolus oriolus", "Turdus torquatus", "Turdus merula", "Parus major", 
                 "Poecile palustris", "Aegithalos caudatus", "Cyanistes caeruleus", "Lophophanes cristatus", 
                 "Periparus ater", "Dendrocopos major", "Dryocopus martius", "Picus viridis", "Pica pica", 
                 "Columba palumbus", "Fringilla coelebs", "Phylloscopus bonelli", "Phylloscopus collybita", 
                 "Regulus ignicapillus", "Luscinia megarhynchos", "Erithacus rubecula", "Phoenicurus phoenicurus", 
                 "Phoenicurus ochruros", "Serinus serinus", "Sitta europaea", "Streptopelia turtur", "Streptopelia decaocto", 
                 "Oenanthe oenanthe", "Troglodytes troglodytes", "Emberiza cia", "Chloris chloris"),
    code_STOC <- c("PRUMOD", "ALAARV", "LULARB", "ACCGEN", "LOXCUR", "EMBCIR", "BUTBUT", "CARCAR", 
                   "TURMER", "STRALU", "CIRGAL", "CORCOR", "CUCCAN", "STUVUL", "SYLATR", "SYLNIS", 
                   "SYLCAN", "SYLUND", "GARGLA", "MUSSTR", "CERFAM", "CERBRA", "TURVIS", 
                   "TURPHI", "ORIORI", "TURTOR", "PARMAJ", "AEGCAU", "PARCAE", "PARCRI", 
                   "DENMAJ", "DRYMAR", "PICVIR", "PICPIC", "COLPAL", "FRICOE", "PHYBON", 
                   "PHYCOL", "REGIGN", "LUSMEG", "ERIRUB", "PHOPHO", "PHOOCH", "SERSER", 
                   "SITEUR", "STRTUR", "STRDEC", "OENOEN", "TROTRO", "EMBCIA", "CARCHL")
    
  )
  
  data <- data %>%
    left_join(especes, by = c("sp_latin"))  # Correction de la clé de jointure
  
  return(data)
}


Typologie_essence <-function(data)

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

 

#JEU COUVERT




essence <- function(data) {
  
  
  correspondance <- data.frame(
    Ess = c("IND", "F.D", "ALI", "ALB", "ALT", "AUL", "AUB", "AUG", "AUV", "BOU", "BOP", "BOV", "CER", 
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
    left_join(correspondance, by = "Ess") %>%
    mutate(Typologie = case_when(
      Nom_essence %in% feuillus ~ "Feuillu",
      Nom_essence %in% coniferes ~ "Conifère"
    ))
  
  return(data)
}






diversite_dmh <- function(data) {
  # Séparer les codes de DMH en lignes distinctes pour chaque arbre
  data_separee <- data %>%
    separate_rows(code.DMH, sep = ",") %>%
    mutate(code.DMH = str_trim(code.DMH)) %>%
    
    # Créer une nouvelle colonne combinant l'essence, le type de bois et le type de DMH
    mutate(type_complet = paste(Nom_essence, Typologie, code.DMH, sep = " - ")) %>%
    
    # Compter le nombre d'occurrences de chaque type complet par placette, tout en gardant 'code.DMH'
    count(placette, type_complet, code.DMH) %>%
    
    # Calculer la proportion de chaque type complet par placette
    group_by(placette) %>%
    mutate(proportion = n / sum(n)) %>%
    ungroup() %>%
    
    # Calcul de l'indice de Shannon pour chaque placette
    group_by(placette) %>%
    summarise(
      diversite_DMH = ifelse(sum(!is.na(code.DMH)) == 0, NA, -sum(proportion * log2(proportion), na.rm = TRUE))
    ) %>%
    ungroup()
  
  # Ajouter l'indice de Shannon calculé à l'ensemble du jeu de données initial
  data <- data %>%
    left_join(data_separee, by = "placette")
  
  return(data)
}


test3 <- diversite_dmh (test)


alt$placette <- as.character(alt$placette)
Saisie_DMH_2024_corrige$placette <- as.character(Saisie_DMH_2024_corrige$placette)


# Faire un inner join pour ne garder que les placettes communes
Saisie_DMH_2024_corrige <- inner_join(Saisie_DMH_2024_corrige, alt, by = "placette")


write.xlsx(Saisie_DMH_2024_corrige, file="Saisie_DMH_2024_corrige.xlsx")

