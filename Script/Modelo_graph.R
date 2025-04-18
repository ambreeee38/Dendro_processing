#-----------------------------------------------------------------------------------------------------------
#---------------------------------------------I. VENTOUX-------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

  #Une fois le jeu de données obtenu, il est important d'analyser la "qualité" des variables explicatives.
#Pour cela, 4 choses sopnt à vérifier : 

#(i) Rechercher graphiquement les valeurs aberrantes, les identifier et les associer ou non à des erreurs
#de manip, de mesure ou bien s'il s'agit simplement d'exceptions.

#(ii) Etudier l'hétérogénéité des valeurs

#(iii) Regarder les corrélations entre variables grâce à une matrice de corrélation. Selon Zuur 2010,deux
#variables sont considérées comme corrélées quand "l'indice de corrélation" est > 70. /!\ Toutefois,
#la corrrélation entre deux variables peut être conservée mais globalement, une variable sur les deux
#est choisie en tant que variable explicative, au risque sinon d'expliquer deux fois la même chose dans
#notre modèle.

#(iv) Analyser les intéractions entre variables. Ceci est une étape importante afin de ne pas mettre
#des intéractions qui n'ont aucun sens dans nos modélisations et que nous sommes incapables d'expliquer.
#Par ailleurs, il est important d'étudier les intéractions de notre variable explicative en fonction
#de chaque variable réponse afin de ne pas passer à côté d'une intéraction potentielle qui pourrait expliquer
#une observation où la tendance générale est nulle alors qu'en réalité, l'influence des variables est
#bien réelle. Dans notre cas, les intéractions sont assez limités, bien qu'il puisse y en avoir notamment 
#sur les variables liées à l'altitude et à l'ouverture de la canopée (potentiellement)



 #### 1. Matrice de corrélation entre les variables ####

if(TRUE) {
  
  #Une matrice de corrélation est réalisée pour voir si des variables ont des corrélations supérieures à 0.8
  #entre elles. A ce compte là; simplement une variable sur les deux seraient gardées selon la littérature.
  
  
  
  
  cor_matrix <- cor(ventoux_BD1[, c("rs_tot", "nb_DMH_BV", "strate_h","strat_sa","strat_a","strat_A", "densite_GBM",
                                    "densite_GBV", "densite_BMS","densite_BV","densite_canopy","densite_souche",
                                    "densite_dmh_ha", 
                                    "diversite_DMH" ,"vol_BM_tot", "vol_BMD_tot","vol_BMS_tot","vol_chandelle_tot","pct_st_Conifère",
                                     "pct_st_Feuillu", 
                                    "decompo_moyen", "alt", "expo",
                                    "pente")],use="complete.obs")
  
  
  # Visualisation de la matrice

colormap <- viridis(200, option = "C")  
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.9, tl.col = "black", addCoef.col = "black",
         col = colormap, cl.cex = 0.9, number.cex = 0.7, 
         insig = "blank", diag = FALSE)
#Il y a de nombreuses corrélation dans cette matrice, il va donc falloir éliminer des variables afin de
#sélectionner celles qui semblent le plus pertinentes dans le cadre de notre étude ou bien, toutes les 
#conserver mais bien veiller à ne pas toutes les mettre dans le même modèle !! 
}

#### 2. Recherche des outliers et des intéractions potentielles entre variables par visualisation graphique (method = "lm") ####

if(TRUE) {
 ### 2.1. Guildes et variables liées au bois mort ###


  
  #Réaménagement du jeu de données en créant une colonne "Richness_nidification" qui reprend
  #les 3 sous groupes/placette. Ainsi, nous avons, par exemple, pour la placette 39 rs_Cavicole,
  #rs_Arboricole, rs_Sol, et "Richness_nid_ass" pour la richesse spécifique associée sur la
  #placette. Ceci permet de faire des graphiques avec l'ensemble des guildes sur le même
  #graph.


        ## 2.1.1. Volume bois mort total ##

if(TRUE) {
  
  
          # 2.1.1.1.1 Bois mort total et nidification #
  #------------------------------------------------------------#
  
  
  ventoux_BD_nid <- ventoux_BD1 %>%
    pivot_longer(cols = c(rs_Cavicole, rs_Arboricole, rs_Sol), 
                 names_to = "Richness_nid", 
                 values_to = "Richness_nid_ass") 
  
  ventoux_BD_nid$Richness_nid <- factor(ventoux_BD_nid$Richness_nid, levels = c("rs_Cavicole", "rs_Arboricole", "rs_Sol"))
  
  
  cb_palette_nid <- c("rs_Cavicole" = "#E69F00",
                  "rs_Arboricole" = "#8fd175", 
                  "rs_Sol" = "brown")        
  

 nid_bm <- ggplot(ventoux_BD_nid, aes(x = vol_BM_tot, y = Richness_nid_ass, color = Richness_nid)) +
    geom_point(alpha = 0.7, size = 2) +  
    geom_smooth(method = "lm", se = TRUE, linewidth =1.2, alpha=0.2) +  
    labs(title = "Richesse spécifique en fonction du volume BM total",
         x = "Volume BM total",
         y = "Richesse spécifique des groupes de nidification") +
    scale_color_manual(values = cb_palette, 
                       labels = c("Cavicole", "Arboricole", "Sol")) +
    theme_minimal(base_size = 14) +  
    theme(legend.title = element_blank(),  
          legend.position = "top",  # Déplace la légende en haut
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank())  # Supprime les petites lignes de grille

  
 
 
        # # 2.1.1.1.1 Bois mort total et alimentation #
 #------------------------------------------------------------#
 
 
  
  ventoux_BD_alim <- ventoux_BD1 %>%
    pivot_longer(cols = c(rs_Herbivore, rs_Omnivore, rs_Insectivore), 
                 names_to = "Richness_alim", 
                 values_to = "Richness_alim_ass") 
  
  ventoux_BD_alim$Richness_alim <- factor(ventoux_BD_alim$Richness_alim, levels = c("rs_Insectivore", "rs_Omnivore", "rs_Herbivore"))
  
  
  cb_palette_alim <- c("rs_Insectivore" = "burlywood4",
                  "rs_Omnivore" = "yellow", 
                  "rs_Herbivore" = "olivedrab3")        
  
  
alim_bm <-  ggplot(ventoux_BD_alim, aes(x = vol_BM_tot, y = Richness_alim_ass, color = Richness_alim)) +
    geom_point(alpha = 0.7, size = 2) +  
    geom_smooth(method = "lm", se = TRUE, size = 1.2, alpha=0.2) +  
    labs(title = "Richesse spécifique des groupes alimentaires en fonction du volume BMS total",
         x = "Volume BM total",
         y = "Richesse spécifique des groupes alimentaires") +
    scale_color_manual(values = cb_palette_alim, 
                       labels = c("Insectivore", "Omnivore","Herbivore" )) +
    theme_minimal(base_size = 14) +  
    theme(legend.title = element_blank(),  
          legend.position = "top",  # Déplace la légende en haut
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank())  # Supprime les petites lignes de grille



# Fusion des plots pour apparaitre sur la même feuille "plots" (grâce au package patchwork)


nid_bm + alim_bm 



#Pas de valeurs aberrantes pour le volume de bois mort total.Petite réserve sur le volume à près de 80/85
#m3/ha et 70 m3/ha => probablement de gros volumes assez exceptionnels mais sûrement pas d'erreurs de 
#mesures.
#Absence de corrélation entre les variables.
  
}

   
      # 2.1.2. Densité bois mort total #

if(TRUE) {
  
        # 2.1.2.1. Densite bois mort total pour les groupes de nidification      
  #-----------------------------------------------------------------------------#
 
 nid_bm_densite <- ggplot(ventoux_BD_nid, aes(x = densite_BM_tot, y = Richness_nid_ass, color = Richness_nid)) +
   geom_point(alpha = 0.7, size = 2) +  
   geom_smooth(method = "lm", se = TRUE, size = 1.2, alpha=0.2) +  
   labs(title = "Richesse spécifique en fonction de la densité totale de bois mort",
        x = "Densité BM total",
        y = "Richesse spécifique des groupes de nidification") +
   scale_color_manual(values = cb_palette, 
                      labels = c("Cavicole", "Arboricole", "Sol")) +
   theme_minimal(base_size = 14) +  
   theme(legend.title = element_blank(),  
         legend.position = "top",  # Déplace la légende en haut
         panel.grid.major = element_line(color = "gray85"),  
         panel.grid.minor = element_blank())  # Supprime les petites lignes de grille
 

 
  
        # 2.1.2.2. Bois mort total en fonction des groupes alimentaires
 #-----------------------------------------------------------------------------#
  
 alim_bm_densite <- ggplot(ventoux_BD_alim, aes(x = densite_BM_tot, y = Richness_alim_ass, color = Richness_alim)) +
   geom_point(alpha = 0.7, size = 2) +  
   geom_smooth(method = "lm", se = TRUE, size = 1.2, alpha=0.2) +  
   labs(title = "Richesse spécifique en fonction de la densité totale de bois mort",
        x = "Densité BM total",
        y = "Richesse spécifique des groupes alimentaires") +
   scale_color_manual(values = cb_palette_alim, 
                      labels = c("Insectivore", "Omnivore", "Herbivore")) +
   theme_minimal(base_size = 14) +  
   theme(legend.title = element_blank(),  
         legend.position = "top",  # Déplace la légende en haut
         panel.grid.major = element_line(color = "gray85"),  
         panel.grid.minor = element_blank())  # Supprime les petites lignes de grille
 
  nid_bm_densite + alim_bm_densite
  
  
  #Les valeurs 200 et ~ 350 BM/ha sont seules dans ces extrêmes et correspondent sans doute à des valeurs
  #exceptionnelles => quand même demander l'avis à Yoan et Thais. Sinon, aucune valeur aberrantes et
  #pas de corrélation.
  
}


       # 2.1.3. Taux de décomposition moyen du bois sur la placette #

if(TRUE) {
  
       # 2.1.3.1. Decomposition moyen du bois pour les groupes nidification et alimentaires#
  #---------------------------------------------------------------------------------------------#
  
  #Permet d'associer les bonnes couleurs aux "richness_nid" car sinon les attributs par ordre
 #alphabétique
  ventoux_BD_nid$Richness_nid <- factor(ventoux_BD_nid$Richness_nid, levels = c("rs_Cavicole", "rs_Arboricole", "rs_Sol"))
  
  #Modification du taux de décomposition moyen en classes plus parlantes que 1,2,3, 4
  #Permet de renommer les numéros comme on le souhaite
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate(decompo_moyen_label = case_when(
      decompo_moyen == 1 ~ "Non décomposé",
      decompo_moyen == 2 ~ "Décomposition légère",
      decompo_moyen == 3 ~ "Décomposition avancée",
      decompo_moyen == 4 ~ "Décomposition très avancée",
      TRUE ~ as.character(decompo_moyen)
    ))
  
  
  #Permet de filtrer les NA du jeu de données afin qu"ils n'apparaissent pas sur le graphique
  ventoux_BD_nid <- ventoux_BD_nid %>%
    filter(!is.na(decompo_moyen_label)) 
  
  
  #R classe les variables en ordre alphabétique  sur l'axe des x, cette fonction permet
  #de les classer dans l'ordre que l'on souhaite
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate(decompo_moyen_label = factor(decompo_moyen_label, 
                                        levels = c("Non décomposé", "Décomposition légère", "Décomposition avancée","Décomposition très avancée")))  # Remplace les autres niveaux si besoin
  
  
  # Filtrer les NA pour éviter les erreurs dans le graphique
  ventoux_BD_nid <- ventoux_BD_nid %>%
    filter(!is.na(decompo_moyen_label), !is.na(Richness_nid_ass), !is.na(Richness_nid))
  
  nid_decompo <- ggplot(ventoux_BD_nid, aes(x = decompo_moyen, y = Richness_nid_ass, color = Richness_nid)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
    labs(title = "Tendance de la richesse spécifique selon le taux de décomposition du bois mort",
         x = "Taux de décomposition du bois",
         y = "Richesse spécifique") +
    scale_color_manual(values = cb_palette_nid, 
                       labels = c("Cavicole", "Arboricole", "Sol")) +
    scale_x_continuous(
      breaks = c(1, 2, 3, 4),
      labels = c("Non décomposé", "Décomposition légère", "Décomposition avancée", "Décomposition très avancée")
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1))
  
  
  
  
      # 2.1.3.2. Decomposition moyen du bois pour les groupes alimentaires #
 #----------------------------------------------------------------------------#
  
  
  
 # Permet d'associer les bonnes couleurs aux "richness_nid" car sinon les attributs par ordre
 # alphabétique
 ventoux_BD_alim$Richness_alim <- factor(ventoux_BD_alim$Richness_alim, levels = c("rs_Insectivore", "rs_Omnivore", "rs_Herbivore"))
 
 # Modification du taux de décomposition moyen en classes plus parlantes que 1, 2, 3, 4
 # Permet de renommer les numéros comme on le souhaite
 ventoux_BD_alim <- ventoux_BD_alim %>%
   mutate(decompo_moyen_label = case_when(
     decompo_moyen == 1 ~ "Non décomposé",
     decompo_moyen == 2 ~ "Décomposition légère",
     decompo_moyen == 3 ~ "Décomposition avancée",
     decompo_moyen == 4 ~ "Décomposition très avancée",
     TRUE ~ as.character(decompo_moyen)
   ))
 
 # Permet de filtrer les NA du jeu de données afin qu'ils n'apparaissent pas sur le graphique
 ventoux_BD_alim <- ventoux_BD_alim %>%
   filter(!is.na(decompo_moyen_label)) 
 
 # R classe les variables en ordre alphabétique sur l'axe des x, cette fonction permet
 # de les classer dans l'ordre que l'on souhaite
 ventoux_BD_alim <- ventoux_BD_alim %>%
   mutate(decompo_moyen_label = factor(decompo_moyen_label, 
                                       levels = c("Non décomposé", "Décomposition légère", "Décomposition avancée", "Décomposition très avancée")))  # Remplace les autres niveaux si besoin
 
 
 alim_decompo <- ggplot(ventoux_BD_alim, aes(x = decompo_moyen, y = Richness_alim_ass, color = Richness_alim)) +
   geom_point(alpha = 0.7, size = 2) +
   geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
   labs(title = "Tendance de la richesse spécifique selon le taux de décomposition du bois mort",
        x = "Taux de décomposition du bois",
        y = "Richesse spécifique") +
   scale_color_manual(values = cb_palette_alim, 
                      labels = c("Insectivore", "Omnivore", "Herbivore")) +
   scale_x_continuous(
     breaks = c(1, 2, 3, 4),
     labels = c("Non décomposé", "Décomposition légère", "Décomposition avancée", "Décomposition très avancée")
   ) +
   theme_minimal(base_size = 14) +
   theme(legend.position = "top",  
         panel.grid.major = element_line(color = "gray85"),  
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 40, hjust = 1))
 
  #Apparition des 2 plots sur la même page
  
   nid_decompo + alim_decompo
   
   
   
   #Valeurs de décompo définies selon le protocole donc pas de variables aberrantes attendues et pas de 
   #corrélation.
}


      # 2.1.4. Densite souche (indice de défrichement) #


if(TRUE) {
  
     # 2.1.4.1. Densite souche selon groupes nidification #
  
  #Ce graphique permet de visualiser l'impact de l'intensité du défrichement sur les
  #communuatés d'oiseaux.
  
  
  # Définition des classes de défrichement
  
  define_classes <- function(densite_souche) {
    case_when(
      densite_souche == 0 ~ "Aucun défrichement",
      densite_souche > 0 & densite_souche <= 90 ~ "Défrichement léger",
      densite_souche > 90 & densite_souche <= 180 ~ "Défrichement modéré",
      densite_souche > 180 ~ "Défrichement intense"
    )
  }
  
  
  
  # Ajout de la colonne de classification
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate(defrichement_class = define_classes(densite_souche))
  
  # Réordonne les niveaux du facteur pour garantir l'ordre souhaité
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate(defrichement_class = factor(defrichement_class, 
                                       levels = c("Aucun défrichement", 
                                                  "Défrichement léger", 
                                                  "Défrichement modéré", 
                                                 "Défrichement intense")))
  
  
  # Regroupement par classe de défrichement et guilde écologique et calcul de la moyenne
  #de richesse spécifique
  richesse_moyenne <- ventoux_BD_nid %>%
    group_by(defrichement_class, Richness_nid) %>%
    summarise(Richesse_moyenne = mean(Richness_nid_ass, na.rm = TRUE), .groups = "drop")
  
  
  #Filtration des NA
  ventoux_BD_nid <- ventoux_BD_nid %>% 
    filter(!is.na(Richness_nid_ass), !is.na(defrichement_class), !is.na(Richness_nid))
  
  nid_souche <- ggplot(ventoux_BD_nid, aes(x = densite_souche, y = Richness_nid_ass, color = Richness_nid)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
    labs(title = "Tendance de la richesse spécifique selon la densité de souches",
         x = "Densité de souches (indicateur de défrichement)",
         y = "Richesse spécifique moyenne par placette") +
    scale_color_manual(values = cb_palette_nid) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top",
          panel.grid.major = element_line(color = "gray85"),
          panel.grid.minor = element_blank())
  
  
  

      # 2.1.1.3.2. Densite souche selon groupes alimentaires #
  
  #Ce graphique permet de visualiser l'impact de l'intensité du défrichement sur les
  #communuatés d'oiseaux.
  
  
  # Définition des classes de défrichement
  
  define_classes <- function(densite_souche) {
    case_when(
      densite_souche == 0 ~ "Aucun défrichement",
      densite_souche > 0 & densite_souche <= 90 ~ "Défrichement léger",
      densite_souche > 90 & densite_souche <= 180 ~ "Défrichement modéré",
      densite_souche > 180 ~ "Défrichement intense"
    )
  }
  
  
  
  # Ajout de la colonne de classification
  ventoux_BD_alim <- ventoux_BD_alim %>%
    mutate(defrichement_class = define_classes(densite_souche))
  
  # Réordonne les niveaux du facteur pour garantir l'ordre souhaité
  # #Permet d'associer les bonnes couleurs aux "richness_nid" car sinon les attributs par ordre
  #alphabétique
  ventoux_BD_alim <- ventoux_BD_alim %>%
    mutate(defrichement_class = factor(defrichement_class, 
                                       levels = c("Aucun défrichement", 
                                                  "Défrichement léger", 
                                                  "Défrichement modéré", 
                                                  "Défrichement intense")))
  
  # Regroupement par classe de défrichement et guilde écologique avec calcul de la moyenne
  #de richesse spécifique par placette
  richesse_moyenne2 <- ventoux_BD_alim %>%
    group_by(defrichement_class, Richness_alim) %>%
    summarise(Richesse_moyenne = mean(Richness_alim_ass, na.rm = TRUE), .groups = "drop")
  
  #Filtration des NA
  ventoux_BD_alim <- ventoux_BD_alim %>% 
    filter(!is.na(Richness_alim_ass), !is.na(defrichement_class), !is.na(Richness_alim))
  
  
  
  alim_souche <- ggplot(ventoux_BD_alim, aes(x = densite_souche, y = Richness_alim_ass, color = Richness_alim)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
    labs(title = "Tendance de la richesse spécifique selon la densité de souches",
         x = "Densité de souches (indicateur de défrichement)",
         y = "Richesse spécifique") +
    scale_color_manual(values = cb_palette_alim) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top",
          panel.grid.major = element_line(color = "gray85"),
          panel.grid.minor = element_blank())
  
  
  
  #Apparition des graph sur la meme page plot
  nid_souche + alim_souche
 
  
  # Pas de valeurs aberrantes ni de corrélation
}


      ### 2.2. Visualisation graphique guildes et variables liées à la canopée ###

if(TRUE) {
  
  #Pour la desnité de la canopée, des classes sont effectuées, définies selon l'IGN BD foret :
  
  #(i) Milieu ouvert (Couvert < 10%)
  #(ii) Forêt ouverte (10% < densité canopée < 40%)
  #(iii) Forêt fermée (Couvert > 40%)
  
  
  
  
  #NIDIFICATION#
  #------------#
  
  
  
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate (ouverture_canope = case_when(
      densite_canopy < 10 ~ "Milieu ouvert",
      densite_canopy >= 10 & densite_canopy <= 40 ~ "Foret ouverte",
      densite_canopy > 40 ~ "Foret fermee"
    ))
  
  # Calcul de la richesse spécifique moyenne par placette
  effectifs_moyens_canopy <- ventoux_BD_nid %>%
    group_by(ouverture_canope, Richness_nid) %>%
    summarise(Richness_moyenne_canopy = mean(Richness_nid_ass), .groups = "drop")
  
  nid_canope <- ggplot(ventoux_BD_nid, aes(x = densite_canopy, y = Richness_nid_ass, color = Richness_nid)) +
    geom_point(alpha = 0.7, size = 2) +  # Nuage de points
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +  # Tendance linéaire
    labs(title = "Tendance de la richesse spécifique selon la densité de canopée",
         x = "Densité de la canopée (%)",
         y = "Richesse spécifique moyenne par placette") +
    scale_color_manual(values = cb_palette_nid) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top",
          panel.grid.major = element_line(color = "gray85"),
          panel.grid.minor = element_blank())
  

  #ALIMENTATION#
  #------------#
  
  
  
  
  ventoux_BD_alim <- ventoux_BD_alim %>%
    mutate (ouverture_canope = case_when(
      densite_canopy < 10 ~ "Milieu ouvert",
      densite_canopy >= 10 & densite_canopy <= 40 ~ "Foret ouverte",
      densite_canopy > 40 ~ "Foret fermee"
    ))
  
  
  # Calcul de la richesse spécifique moyenne par placette
  effectifs_moyens_canopy2 <- ventoux_BD_alim %>%
    group_by(ouverture_canope, Richness_alim) %>%
    summarise(Richness_moyenne_canopy = mean(Richness_alim_ass), .groups = "drop")
  
  alim_canope <- ggplot(ventoux_BD_alim, aes(x = densite_canopy, y = Richness_alim_ass, color = Richness_alim)) +
    geom_point(alpha = 0.7, size = 2) +  # Nuage de points
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +  # Tendance linéaire
    labs(title = "Tendance de la richesse spécifique selon la densité de canopée",
         x = "Densité de la canopée (%)",
         y = "Richesse spécifique") +
    scale_color_manual(values = cb_palette_alim) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top",
          panel.grid.major = element_line(color = "gray85"),
          panel.grid.minor = element_blank())
  
  
  
  nid_canope + alim_canope
         
} 

     
      ### 2.3. Visualisation graphique guildes et variables liées aux DMH###

if(TRUE) {
  
         ## 2.3.1. Densite DMH ##
  
  
  
  #NIDIFICATION#
  #------------#
  
  
  
  nid_dmh_density <- ggplot(ventoux_BD_nid, aes(x = densite_dmh_ha, y = Richness_nid_ass, color = Richness_nid)) +
    geom_point(alpha = 0.7, size = 2) +  
    geom_smooth(method = "lm", se = TRUE, size = 1.2, alpha=0.2) +  
    labs(title = "Richesse spécifique en fonction de la densite de DMH/ha",
         x = "Densite DMH/ha",
         y = "Richesse spécifique des groupes de nidification") +
    scale_color_manual(values = cb_palette, 
                       labels = c("Cavicole", "Arboricole", "Sol")) +
    theme_minimal(base_size = 14) +  
    theme(legend.title = element_blank(),  
          legend.position = "top",  # Déplace la légende en haut
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank())  # Supprime les petites lignes de grille
  
  
  
  
  #ALIMENTATION#
  #------------#
  
  

  alim_dmh_density <- ggplot(ventoux_BD_alim, aes(x = densite_dmh_ha, y = Richness_alim_ass, color = Richness_alim)) +
    geom_point(alpha = 0.7, size = 2) +  
    geom_smooth(method = "lm", se = TRUE, size = 1.2, alpha=0.2) +  
    labs(title = "Richesse spécifique en fonction de la densite de DMH/ha",
         x = "Densite DMH/ha",
         y = "Richesse spécifique des groupes de nidification") +
    scale_color_manual(values = cb_palette_alim, 
                       labels = c("Insectivore", "Omnivore", "Herbivore")) +
    theme_minimal(base_size = 14) +  
    theme(legend.title = element_blank(),  
          legend.position = "top",  # Déplace la légende en haut
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank())  # Supprime les petites lignes de grille
  
  nid_dmh_density + alim_dmh_density
  
  
  #pas de corrélation ni de variables aberrantes
  
           ## 2.3.2. Diversite DMH ##
  
  
  
  #NIDIFICATION#
  #------------#
  
  
  
  nid_dmh_diversity <- ggplot(ventoux_BD_nid, aes(x = diversite_DMH, y = Richness_nid_ass, color = Richness_nid)) +
    geom_point(alpha = 0.7, size = 2) +  
    geom_smooth(method = "lm", se = TRUE, size = 1.2, alpha=0.2) +  
    labs(title = "Richesse spécifique en fonction de la diversité de DMH",
         x = "Diversité DMH",
         y = "Richesse spécifique des groupes de nidification") +
    scale_color_manual(values = cb_palette, 
                       labels = c("Cavicole", "Arboricole", "Sol")) +
    theme_minimal(base_size = 14) +  
    theme(legend.title = element_blank(),  
          legend.position = "top",  # Déplace la légende en haut
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank())  # Supprime les petites lignes de grille
  
  
  
  
  #ALIMENTATION#
  #------------#
  
  
  
  alim_dmh_diversity <- ggplot(ventoux_BD_alim, aes(x = diversite_DMH, y = Richness_alim_ass, color = Richness_alim)) +
    geom_point(alpha = 0.7, size = 2) +  
    geom_smooth(method = "lm", se = TRUE, size = 1.2, alpha=0.2) +  
    labs(title = "Richesse spécifique en fonction de la diversite de DMH",
         x = "Diversité DMH",
         y = "Richesse spécifique des groupes de nidification") +
    scale_color_manual(values = cb_palette_alim, 
                       labels = c("Insectivore", "Omnivore", "Herbivore")) +
    theme_minimal(base_size = 14) +  
    theme(legend.title = element_blank(),  
          legend.position = "top",  # Déplace la légende en haut
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank())  # Supprime les petites lignes de grille
  
  nid_dmh_diversity + alim_dmh_diversity
  
  
  #pas de corrélation ni de variables aberrantes
}


      ### 2.4. Visualisation graphique guildes et variables liées à la stratification verticale###

if(TRUE) {
  
  #Pour cette visualisation graphique, on utilise le jeu de données "ventoux_BD1" auquel on va 
  #faire un pivot_longer pour obtenir par placette le taux de recouvrement de chaque strate.
  #Ensuite, on tracera les graph de rs_cavi, rs_arboricole, rs_sol... en fonction de ces 
  #stratifications. Ce qui fait un total de 8 graphiques (1 strate*8 rs_). Si on utilisait ventoux_BD_nid
  #ou alim cela ferait près de 12 graphiques pour avoir la richesse spécifique de chaque
  #guilde en fonction de chaque strate (4 strates * 3 guildes)
  
  
  ventoux_BD_strate <- ventoux_BD1 %>%
    pivot_longer(cols = c(strate_h, strat_sa, strat_a, strat_A), 
                 names_to = "stratification", 
                 values_to = "pct_recouvrement") 
  
  ventoux_BD_strate$stratification<- factor(ventoux_BD_strate$stratification, levels = c("strate_h", "strat_sa", "strat_a", "strat_A"))
  
  
  cb_palette_strate <- c("strate_h" = "#E69F00",
                      "strat_sa" = "#8fd175", 
                      "strat_a" = "brown",
                      "strat_A" = "green")   
  
  
  
  
  
  #RS_CAVICOLE#
  #-----------#
  
  
  
  
  cavi_strat <- ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Cavicole, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique cavicole et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique cavicole",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels
  
  
  
  
  
  #RS_ARBORICOLE#
  #-------------#
  
  
  
  
  arboricole_strat <- ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Arboricole, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique arboricole et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique arboricole",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels
  
  
    
  
  
  
  #RS_SOL#
  #------#
  
  
  
  
  sol_strat <- ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Sol, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique des nicheurs au sol et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique nicheurs au sol",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels
  
  
  
  
  #RS_INSECTIVORES#
  #---------------#
  
  
  
  ins_strat <-ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Insectivore, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique insectivores et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique insectivores",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels
  
  
  
  
  
  #RS_OMNIVORES#
  #------------#
  
  
  
  omn_strat <-ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Omnivore, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique omnivore et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique omnivore",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels
  
  
  
  
  
  
  
  #RS_HERBIVORE#
  #------------#
  
  
  
  
  herb_strat <-ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Herbivore, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique herbivore et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique herbivore",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des label
  
  
  ((cavi_strat | arboricole_strat | sol_strat) / (ins_strat | herb_strat | omn_strat)) +
    plot_annotation(title = "Relations entre la richesse spécifique et le recouvrement des strates",
                    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
  
  #% donc pas de valeurs aberrantes et ne semble pas y avoir de corrélation sauf peut être entre strate_h
  #et strate_A pour herbivore, sol et cavicole. 
  
}
 


     ### 2.5. Visualisation graphique guildes et variables liées à la compososition forestière ###

if(TRUE) {
  
          ## 2.5.1. st_conifères et guildes alimentaires ##
  
  ventoux_BD_compo <- ventoux_BD1 %>%
    pivot_longer(cols = c(pct_st_Conifère, pct_st_Feuillu), 
                 names_to = "composition", 
                 values_to = "pct_compo") 
  
  ventoux_BD_compo$composition<- factor(ventoux_BD_compo$composition, levels = c("pct_st_Conifère","pct_st_Feuillu"))
  
  
  cb_palette_compo <- c("pct_st_Conifère" = "darkgreen",
                         "pct_st_Feuillu" = "#8fd175")
  
  
  
  ggplot(ventoux_BD_nid, aes(x = pct_st_Conifère, y = Richness_nid_ass, color = Richness_nid)) +
    geom_point(alpha = 0.6) +  # Points de la relation
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire
    scale_color_manual(values = cb_palette_nid, 
                       labels = c("Cavicole", "Arboricole", "Sol")) +
    theme_minimal() +
    labs(
      title = "Richesse spécifique des guildes alimentaires selon la composition forestière",
      x = "Pourcentage de conifere",
      y = "Richesse spécifique",
      color = "Composition"
    ) 
  
  ggplot(ventoux_BD_nid, aes(x = pct_st_Feuillu, y = Richness_nid_ass, color = Richness_nid)) +
    geom_point(alpha = 0.6) +  # Points de la relation
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire
    scale_color_manual(values = cb_palette_nid, 
                       labels = c("Cavicole", "Arboricole", "Sol")) +
    theme_minimal() +
    labs(
      title = "Richesse spécifique des guildes alimentaires selon la composition forestière",
      x = "Pourcentage de feuillu",
      y = "Richesse spécifique",
      color = "Composition"
    ) 
  
  #Pas de valeurs aberrantes, par contre peut etre corrélation entre composition feuillu confiere avedc
  #des effets opposés ?
}

}

#### 3. Modélisation ####


#### 3.1. HYPOTHESE 1 : La diversité des dendromicrohabitats à l'échelle parcellaire favorise la 
#richesse spécifique globale des oiseaux  indépendemment de l'altitude ####


hist(ventoux_BD1$rs_tot,
     main="Histogramme de la variable rs_tot (réponse)",
     xlab="rs_tot",
     col="lightblue",
     breaks=20)


### 3.1.1. Modèle nul

if(TRUE) {
summary(model_nul <- glm(rs_tot ~ 1,
                 family=poisson, data = ventoux_BD1))

}

  ## 3.1.2. Modèle complet et selection descendante par StepAIC (selection d emodèle par l'AIC) ###

if(TRUE) {
  
  ##MODELE COMPLET##
  
  summary(model_complet <- glmmTMB(rs_tot ~ pct_st_Conifère+alt + densite_canopy + diversite_DMH,
                       family=poisson,data= ventoux_BD1))
  
  
  ##STEPAIC##
  
  modele_reduit <- stepAIC(model_complet, direction = "backward")
  summary(modele_reduit)
  
  
 
  
  #La sélection ee modèle par StepAIC selectionne comme modèle final celui-ci :
  
  #                          rs_tot ~ alt * pct_st_conifere 
  
  
  ##CALCUL DE LA DISPERSION##
  
  phi7 <- modele_reduit$deviance / modele_reduit$df.residual #phi = 0.45
  
  #On a de la sous dispersion qu'il est mportant de tenir compte. Pour la corriger, on pourrait faire
  #une negative binomiale mais comme dit Yoan "c'est comme enfoncer un clou avec un marteau-piqueur."
  #Par ailleurs, il me semble que la negative.binomiale corrige la surdispersion.
  #La correction est trop forte, alors il faut tenir compte d'un autre mode de correction qui est celle
  #de Conwell et Maxwell (compois).Ainsi, on peut essayer avec le modèle complet en tenant compte
  #de cette correction 
  
  
  ##CORRECTION DE LA SOUS DISPERSION##
  
  mod_compois <- glmmTMB(
    rs_tot ~ alt + pct_st_Conifère + densite_dmh_ha + densite_canopy ,
    family = compois(link = "log"),
    data = ventoux_BD1
  )
  summary(mod_compois)
}


 ## 3.1.3. Modéle par selection descendante et famille compois (Maxwell et Conwall)

   ## 3.1.3.1. Diversite_dmh (qualitatif) ##

if(TRUE) {
  
  ##MODELE NUL##
  mod_compois_rstot_nul <- glmmTMB(
    rs_tot ~ 1,
    family = compois(link = "log"),
    data = ventoux_BD1
  )
  summary(mod_compois_rstot_nul)
  
  
  ##MODELE FULL##
  mod_compois_rstot_full <- glmmTMB(
    rs_tot ~ alt + pct_st_Conifère + diversite_DMH + densite_canopy + forme + vol_BM_tot + densite_GBV + densite_souche,
    family = compois(link = "log"),
    data = ventoux_BD1
  )
  summary(mod_compois_rstot_full)
  
  ##SELECTION DESCENDANTE##
  
     #SANS DENSITE_SOUCHE#
  mod_compois_rstot_ds <- glmmTMB(
    rs_tot ~ alt + pct_st_Conifère + diversite_DMH + densite_canopy + forme + densite_GBV,
    family = compois(link = "log"),
    data = ventoux_BD1
  )
  summary(mod_compois_rstot_ds)
  
  
  #SANS VOLUME BOIS MORT TOTAL#
  mod_compois_rstot_vbm <- glmmTMB(
    rs_tot ~ alt + pct_st_Conifère + diversite_DMH + densite_canopy + forme + densite_GBV,
    family = compois(link = "log"),
    data = ventoux_BD1
  )
  summary(mod_compois_rstot_vbm)
  
  
  #SANS DENSITE GROS BOIS VIVANT#
  mod_compois_rstot_gbv <- glmmTMB(
    rs_tot ~ alt + pct_st_Conifère + diversite_DMH + densite_canopy + forme,
    family = compois(link = "log"),
    data = ventoux_BD1
  )
  summary(mod_compois_rstot_gbv)
  
  
  #SANS CONIFERE#
  mod_compois_rstot_coni <- glmmTMB(
    rs_tot ~ alt  + diversite_DMH + densite_canopy + forme,
    family = compois(link = "log"),
    data = ventoux_BD1
  )
  summary(mod_compois_rstot_coni)
  #/!\ AIC qui réaugmente quand suppression de la variable pct_st_conifère, même si p.value >0.05.
  #Donc le modèle conservant les conifère est le meilleur modèle. Il faut maintenant tester ses
  #paramètres pour voir s'il fit bien les données grâce à DHARMa.
  
  
  
  ##MODELE FINAL##
mod_compois1 <- glmmTMB(
  rs_tot ~ alt + pct_st_Conifère + diversite_DMH + densite_canopy + forme,
  family = compois(link = "log"),
  data = ventoux_BD1
)
summary(mod_compois1)

mod_compois1_diag <- simulateResiduals(mod_compois1)
plot(mod_compois1_diag)


}


  #### 3.2. Modélo rs_cavicole ####

if(TRUE) {

  ## 3.1.1. Histogramme de la distribution de la variable réponse
  
  
  
  #On trace l'histogramme de la distribution de la variable réponse afin de voir si elle suit une distribution
  #normale. Dans notre cas, la distribution est plutot jolie.
  

hist(ventoux_BD1$rs_Cavicole,
     main="Histogramme de la variable rs_cavicole (réponse)",
     xlab="rs_cavi",
     col="lightblue",
     breaks=20)

  hist(ventoux_BD1$rs_tot)
  
  
  
  ##MODELE NUL##
  summary(model_nul <- glmmTMB(rs_Cavicole ~ 1,
                   family=compois (link="log"), data = ventoux_BD1))
  
  
  
  ##SELECTION DESCENDANTE##

  
  model_cavi1 <-glmmTMB(rs_Cavicole ~  densite_GBV +densite_GBM+  pct_st_Conifère
                        +alt  + diversite_DMH + densite_souche + decompo_moyen,
                        family=compois(link = "log"), data = ventoux_BD1)
  
  
  summary(model_cavi1)
  
  #Suppression altitude => p.value = 0.43
  #AIC = 207.6

  
  
  
  model_cavi2 <-glmmTMB(rs_Cavicole ~  densite_GBV +densite_GBM+  pct_st_Conifère
                        + diversite_DMH + densite_souche + decompo_moyen,
                        family=compois(link = "log"), data = ventoux_BD1)
  
  
  
  summary(model_cavi2)
  
  #Suppression de densite_souche (p.value = 0.39)
  #AIC = 206.2
  
  
  
  
  model_cavi3 <-glmmTMB(rs_Cavicole ~  densite_GBV +densite_GBM+  pct_st_Conifère
                        + diversite_DMH + decompo_moyen,
                        family=compois(link = "log"), data = ventoux_BD1)
  
  
  
  summary(model_cavi3)
  
  
  #Suppresion de pct_st_conifere (p.value = 0.62)
  #AIC = 204.9
  
  
  
  
  
  model_cavi4 <-glmmTMB(rs_Cavicole ~  densite_GBV +densite_GBM
                        + diversite_DMH  + decompo_moyen,
                        family=compois(link = "log"), data = ventoux_BD1)
  
  
  summary(model_cavi4)
  
  
  #Suppression de densite_GBM (p.value = 0.62)
  #AIC = 203.2
  
  
  model_cavi5 <-glmmTMB(rs_Cavicole ~  densite_GBV 
                        + diversite_DMH  + decompo_moyen,
                        family=compois(link = "log"), data = ventoux_BD1)
  
  
  summary(model_cavi5)
  
  
  #Suppression de densite_GBV (p.value = 0.23)
  #AIC=201.4
  
  
  
  model_cavi6 <-glmmTMB(rs_Cavicole ~  diversite_DMH  + decompo_moyen,
                        family=compois(link = "log"), data = ventoux_BD1)
  
  
  summary(model_cavi6)
  
  
  ##MODEL FINAL##
  model_full_cavi <- glmmTMB(rs_Cavicole ~  diversite_DMH + decompo_moyen ,
                             
                             family=compois(link = "log"), data = ventoux_BD1)
  summary(model_full_cavi)
  
  #Suppression de decompo_moyen (p.value =0.27)
  #AIC = 200.7
  #MODELE FINAL ?
  
  
}




### 3.3. Modélisation rs_sol ###

if(TRUE) {
  


  #On trace l'histogramme de la distribution de la variable réponse afin de voir si elle suit une distribution
  #normale. Dans notre cas, la distribution est plutot jolie.
  
  
  hist(ventoux_BD1$rs_Sol,
       main="Histogramme de la variable rs_sol (réponse)",
       xlab="rs_sol",
       col="lightblue",
       breaks=20)
  
  hist(ventoux_BD1$rs_tot)
  
  
  
  ##MODELE NUL##
  
  summary(model_nul <- glm(rs_Sol ~ 1,
                   family=poisson, data = ventoux_BD1))
  
  summary(model_1 <- glmmTMB(rs_Insectivore ~ pct_st_Conifère +  diversite_DMH + vol_BM_tot ,
                           family=compois, data = ventoux_BD1))
  
  summary(model_1b <- glmmTMB(rs_Sol ~ pct_st_Conifère + alt + diversite_DMH ,
                         family=genpois(link = "log"), data = ventoux_BD1))  
  
  ##MODEL FULL##
  
  model_full_sol <- glm(rs_Sol ~ densite_canopy + diversite_DMH + densite_dmh_ha + strate_h +
                          strat_sa + pct_st_Conifère * alt + densite_BV ,
                         
                         family=poisson, data = ventoux_BD1)
  
  
  ##STEPAIC##
  
  modele_reduit_sol <- stepAIC(model_full_sol, direction = "both")
  summary(modele_reduit_cavi)
  
}


### 3.4. Modélisation rs_arboricole ###

if(TRUE) {
  
  
  
  #On trace l'histogramme de la distribution de la variable réponse afin de voir si elle suit une distribution
  #normale. Dans notre cas, la distribution est plutot jolie.
  
  
  hist(ventoux_BD1$rs_Arboricole,
       main="Histogramme de la variable rs_arboricole (réponse)",
       xlab="rs_cavi",
       col="lightblue",
       breaks=20)
  
  
  
  ##MODELE NUL##
  
  summary(model_nul <- glm(rs_Arboricole ~ 1,
                   family=poisson, data = ventoux_BD1))
  
  ##MODEL FULL##
  
  model_full_sol <- glm(rs_Arboricole ~ densite_canopy + diversite_DMH + densite_dmh_ha + strate_h +
                          strat_sa + pct_st_Conifère * alt + densite_BV ,
                        
                        family=poisson, data = ventoux_BD1)
  
  ##STEPAIC##
  
  modele_reduit_cavi <- stepAIC(model_full_cavi, direction = "both")
  summary(modele_reduit_cavi)
  
}



#### 4. Visualisation graphique variable réponse/variable explicative ayant un effet ####




#### GRAPH AU CAS OU POUR 4.####
#--------------------------------#

 #BOXPLOT ET BARPLOT DE LA RICHESSE SPECIFIQUE DES GUILDES DE NIDIFICATION EN FONCTION DE LA DENSITE DE 
#BOIS TOTAL 

if(TRUE) {
  
  #BOXPLOT#
  #-------#
  
  
  
  
  
  classes_densite_bm <- function(densite_BM_tot) {
    case_when(
      densite_BM_tot == 0 ~ "0 bois mort/ha",
      densite_BM_tot > 0 & densite_BM_tot <= 90 ~ "1-90 bois morts/ha",
      densite_BM_tot > 90 & densite_BM_tot <= 180 ~ "91-180 bois morts/ha",
      densite_BM_tot > 180 & densite_BM_tot <= 270 ~ "181-270 bois morts/ha",
      densite_BM_tot > 270 & densite_BM_tot <= 375 ~ "271-375 bois morts/ha"
    )
  }
  
  # Exemple d'application à un jeu de données
  ventoux_BD_nid <- ventoux_BD1 %>%
    mutate(classes_bm1 = classes_densite_bm(densite_BM_tot))
  
  # Réordonne les niveaux du facteur pour garantir l'ordre souhaité
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate(classes_bm1 = factor(classes_bm1, 
                                levels = c("0 bois mort/ha", 
                                           "1-90 bois morts/ha", 
                                           "91-180 bois morts/ha", 
                                           "181-270 bois morts/ha",
                                           "271-375 bois morts/ha")))
  
  
  # Regroupement par classe de défrichement et guilde écologique et calcul de la moyenne
  #de richesse spécifique
  richesse_moyenne <- ventoux_BD_nid %>%
    group_by(classes_bm1, Richness_nid) %>%
    summarise(Richesse_moyenne = mean(Richness_nid_ass, na.rm = TRUE), .groups = "drop")
  
  
  #Filtration des NA
  ventoux_BD_nid <- ventoux_BD_nid %>% 
    filter(!is.na(Richness_nid_ass), !is.na(classes_bm1), !is.na(Richness_nid))
  
  #Boxplot
  nid_bm_densite <- ggplot(ventoux_BD_nid, aes(x = classes_bm1, y = Richness_nid_ass, fill = Richness_nid)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Ajoute un boxplot et cache les outliers
    labs(title = "Richesse spécifique selon la densite de bois mort",
         x = "Classe de densite de bois mort",
         y = "Richesse spécifique") +
    scale_fill_manual(values = cb_palette_nid) +  # Palette de couleurs
    scale_color_manual(values = cb_palette_nid) +  # Même couleur pour les points
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1))  # Rotation pour lisibilité
  
  nid_bm_densite
  
  
  
  
  
  
  
  #BARPLOT#
  #-------#
  
  
  
  
  # Fonction pour créer les classes de densité de bois mort
  classes_densite_bm <- function(densite_BM_tot) {
    case_when(
      densite_BM_tot == 0 ~ "0 bois mort/ha",
      densite_BM_tot > 0 & densite_BM_tot <= 90 ~ "1-90 bois morts/ha",
      densite_BM_tot > 90 & densite_BM_tot <= 180 ~ "91-180 bois morts/ha",
      densite_BM_tot > 180 & densite_BM_tot <= 270 ~ "181-270 bois morts/ha",
      densite_BM_tot > 270 & densite_BM_tot <= 375 ~ "271-375 bois morts/ha"
    )
  }
  
  # Application à un jeu de données
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate(classes_bm1 = classes_densite_bm(densite_BM_tot))
  
  # Réorganiser les niveaux du facteur pour garantir l'ordre souhaité
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate(classes_bm1 = factor(classes_bm1, 
                                levels = c("0 bois mort/ha", 
                                           "1-90 bois morts/ha", 
                                           "91-180 bois morts/ha", 
                                           "181-270 bois morts/ha",
                                           "271-375 bois morts/ha")))
  
  # Regroupement par classe de densité de bois mort et guilde écologique pour calculer la richesse moyenne
  richesse_moyenne <- ventoux_BD_nid %>%
    group_by(classes_bm1, Richness_nid) %>%
    summarise(Richesse_moyenne = mean(Richness_nid_ass, na.rm = TRUE), .groups = "drop")
  
  # Filtrer les données pour enlever les NA
  ventoux_BD_nid <- ventoux_BD_nid %>%
    filter(!is.na(Richness_nid_ass), !is.na(classes_bm1), !is.na(Richness_nid))
  
  
  
  
  
  nid_bm_densite_barplot <- ggplot(richesse_moyenne, aes(x = classes_bm1, y = Richesse_moyenne, fill = Richness_nid)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 1) +  
    labs(title = "Richesse spécifique selon la densité de bois mort",
         x = "Classe de densité de bois mort",
         y = "Richesse spécifique moyenne par placette") +
    scale_fill_manual(values = cb_palette_nid) +  
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1))  
  
  nid_bm_densite
  
}

#BOXPLOT DE LA RICHESSE SPECIFIQUE DES GROUPES DE NIDIFICATION EN FONCTION DE LA DENSITE DE BOIS MORT #

if(TRUE) {
  
  classes_densite_bm <- function(densite_BM_tot) {
    case_when(
      densite_BM_tot == 0 ~ "0 bois mort/ha",
      densite_BM_tot > 0 & densite_BM_tot <= 90 ~ "1-90 bois morts/ha",
      densite_BM_tot > 90 & densite_BM_tot <= 180 ~ "91-180 bois morts/ha",
      densite_BM_tot > 180 & densite_BM_tot <= 270 ~ "181-270 bois morts/ha",
      densite_BM_tot > 270 & densite_BM_tot <= 375 ~ "271-375 bois morts/ha"
    )
  }
  
  # Exemple d'application à un jeu de données
  ventoux_BD_alim <- ventoux_BD_alim %>%
    mutate(classes_bm1 = classes_densite_bm(densite_BM_tot))
  
  # Réordonne les niveaux du facteur pour garantir l'ordre souhaité
  ventoux_BD_alim <- ventoux_BD_alim %>%
    mutate(classes_bm1 = factor(classes_bm1, 
                                levels = c("0 bois mort/ha", 
                                           "1-90 bois morts/ha", 
                                           "91-180 bois morts/ha", 
                                           "181-270 bois morts/ha",
                                           "271-375 bois morts/ha")))
  
  
  # Regroupement par classe de défrichement et guilde écologique et calcul de la moyenne
  #de richesse spécifique
  richesse_moyenne2 <- ventoux_BD_alim %>%
    group_by(classes_bm1, Richness_alim) %>%
    summarise(Richesse_moyenne = mean(Richness_alim_ass, na.rm = TRUE), .groups = "drop")
  
  
  #Filtration des NA
  ventoux_BD_alim <- ventoux_BD_alim %>% 
    filter(!is.na(Richness_alim_ass), !is.na(classes_bm1), !is.na(Richness_alim))
  
  #Boxplot
  alim_bm_densite <- ggplot(ventoux_BD_alim, aes(x = classes_bm1, y = Richness_alim_ass, fill = Richness_alim)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Ajoute un boxplot et cache les outliers
    labs(title = "Richesse spécifique selon la densite de bois mort",
         x = "Classe de densite de bois mort",
         y = "Richesse spécifique moyenne par placette") +
    scale_fill_manual(values = cb_palette_alim) +  # Palette de couleurs
    scale_color_manual(values = cb_palette_alim) +  # Même couleur pour les points
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1))  # Rotation pour lisibilité
  
  
}

#BOXPLOT DE LA RICHESSE SPECIFIQUE DES GROUPES DE NIDIFICATION EN FONCTION DU TAUX DE DECOMPOSITION
#DU BOIS MORT 

if(TRUE) {
  
  # Permet d'associer les bonnes couleurs aux "richness_nid" car sinon les attributs par ordre
  # alphabétique
  ventoux_BD_alim$Richness_alim <- factor(ventoux_BD_alim$Richness_alim, levels = c("rs_Insectivore", "rs_Omnivore", "rs_Herbivore"))
  
  # Modification du taux de décomposition moyen en classes plus parlantes que 1, 2, 3, 4
  # Permet de renommer les numéros comme on le souhaite
  ventoux_BD_alim <- ventoux_BD_alim %>%
    mutate(decompo_moyen_label = case_when(
      decompo_moyen == 1 ~ "Non décomposé",
      decompo_moyen == 2 ~ "Décomposition légère",
      decompo_moyen == 3 ~ "Décomposition avancée",
      decompo_moyen == 4 ~ "Décomposition très avancée",
      TRUE ~ as.character(decompo_moyen)
    ))
  
  # Permet de filtrer les NA du jeu de données afin qu'ils n'apparaissent pas sur le graphique
  ventoux_BD_alim <- ventoux_BD_alim %>%
    filter(!is.na(decompo_moyen_label)) 
  
  # R classe les variables en ordre alphabétique sur l'axe des x, cette fonction permet
  # de les classer dans l'ordre que l'on souhaite
  ventoux_BD_alim <- ventoux_BD_alim %>%
    mutate(decompo_moyen_label = factor(decompo_moyen_label, 
                                        levels = c("Non décomposé", "Décomposition légère", "Décomposition avancée", "Décomposition très avancée")))  # Remplace les autres niveaux si besoin
  
  
  
  # Calcul de la richesse spécifique moyenne par placette
  effectifs_moyens <- ventoux_BD_alim %>%
    group_by(decompo_moyen_label, Richness_alim) %>%
    summarise(Richness_moyenne = mean(Richness_alim_ass), .groups = "drop")
  
  # Boxplot
  alim_decompo <- ggplot(ventoux_BD_alim, aes(x = decompo_moyen_label, y = Richness_alim_ass, fill = Richness_alim)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Ajoute un boxplot et cache les outliers
    labs(title = "Richesse spécifique des groupes alimentaires selon le taux de décomposition du bois mort",
         x = "Taux de décomposition du bois",
         y = "Richesse spécifique moyenne par placette") +
    scale_fill_manual(values = cb_palette_alim, 
                      labels = c("Insectivore", "Omnivore", "Herbivore")) +
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1))  # Rotation pour lisibilité
}

#BOXPLOT DE LA RICHESSE SPECIFIQUE DES GROUPES DE NIDIFICATION EN FONCTION DE LA DENSITE DE SOUCHE#

if(TRUE) {
  
  #Ce graphique permet de visualiser l'impact de l'intensité du défrichement sur les
  #communuatés d'oiseaux.
  
  
  # Définition des classes de défrichement
  
  define_classes <- function(densite_souche) {
    case_when(
      densite_souche == 0 ~ "Aucun défrichement",
      densite_souche > 0 & densite_souche <= 90 ~ "Défrichement léger",
      densite_souche > 90 & densite_souche <= 180 ~ "Défrichement modéré",
      densite_souche > 180 ~ "Défrichement intense"
    )
  }
  
  
  
  # Ajout de la colonne de classification
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate(defrichement_class = define_classes(densite_souche))
  
  # Réordonne les niveaux du facteur pour garantir l'ordre souhaité
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate(defrichement_class = factor(defrichement_class, 
                                       levels = c("Aucun défrichement", 
                                                  "Défrichement léger", 
                                                  "Défrichement modéré", 
                                                  "Défrichement intense")))
  
  
  # Regroupement par classe de défrichement et guilde écologique et calcul de la moyenne
  #de richesse spécifique
  richesse_moyenne <- ventoux_BD_nid %>%
    group_by(defrichement_class, Richness_nid) %>%
    summarise(Richesse_moyenne = mean(Richness_nid_ass, na.rm = TRUE), .groups = "drop")
  
  
  #Filtration des NA
  ventoux_BD_nid <- ventoux_BD_nid %>% 
    filter(!is.na(Richness_nid_ass), !is.na(defrichement_class), !is.na(Richness_nid))
  
  #Boxplot
  nid_souche <- ggplot(ventoux_BD_nid, aes(x = defrichement_class, y = Richness_nid_ass, fill = Richness_nid)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Ajoute un boxplot et cache les outliers
    labs(title = "Richesse spécifique selon le niveau de défrichement",
         x = "Classe de défrichement",
         y = "Richesse spécifique moyenne par placette") +
    scale_fill_manual(values = cb_palette_nid) +  # Palette de couleurs
    scale_color_manual(values = cb_palette_nid) +  # Même couleur pour les points
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1))  # Rotation pour lisibilité
  
}

#BOXPLOT DE LA RICHESSE SPECIFIQUE DES GROUPES DE NIDIFICATION EN FONCTION DE LA DENSITE DE SOUCHE#

if(TRUE) {
  

define_classes <- function(densite_souche) {
  case_when(
    densite_souche == 0 ~ "Aucun défrichement",
    densite_souche > 0 & densite_souche <= 90 ~ "Défrichement léger",
    densite_souche > 90 & densite_souche <= 180 ~ "Défrichement modéré",
    densite_souche > 180 ~ "Défrichement intense"
  )
}



# Ajout de la colonne de classification
ventoux_BD_alim <- ventoux_BD_alim %>%
  mutate(defrichement_class = define_classes(densite_souche))

# Réordonne les niveaux du facteur pour garantir l'ordre souhaité
# #Permet d'associer les bonnes couleurs aux "richness_nid" car sinon les attributs par ordre
#alphabétique
ventoux_BD_alim <- ventoux_BD_alim %>%
  mutate(defrichement_class = factor(defrichement_class, 
                                     levels = c("Aucun défrichement", 
                                                "Défrichement léger", 
                                                "Défrichement modéré", 
                                                "Défrichement intense")))

# Regroupement par classe de défrichement et guilde écologique avec calcul de la moyenne
#de richesse spécifique par placette
richesse_moyenne2 <- ventoux_BD_alim %>%
  group_by(defrichement_class, Richness_alim) %>%
  summarise(Richesse_moyenne = mean(Richness_alim_ass, na.rm = TRUE), .groups = "drop")

#Filtration des NA
ventoux_BD_alim <- ventoux_BD_alim %>% 
  filter(!is.na(Richness_alim_ass), !is.na(defrichement_class), !is.na(Richness_alim))


#Boxplot
alim_souche <- ggplot(ventoux_BD_alim, aes(x = defrichement_class, y = Richness_alim_ass, fill = Richness_alim)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA)  +  
  labs(title = "Richesse spécifique selon le niveau de défrichement",
       x = "Classe de défrichement",
       y = "Richesse spécifique moyenne par placette") +
  scale_fill_manual(values = cb_palette_alim) +  
  theme_minimal(base_size = 14) +  
  theme(legend.position = "top",  
        panel.grid.major = element_line(color = "gray85"),  
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1)) 

}

#BOXPLOT DE LA RICHESSE SPECIFIQUE DES GROUPES DE NIDIFICATION ET ALIMENTAIRES EN FONCTION DE LA 
#DENSITE DE LA CANOPEE#

if(TRUE) {
  #Pour la desnité de la canopée, des classes sont effectuées, définies selon l'IGN BD foret :
  
  #(i) Milieu ouvert (Couvert < 10%)
  #(ii) Forêt ouverte (10% < densité canopée < 40%)
  #(iii) Forêt fermée (Couvert > 40%)
  
  
  
  
  #NIDIFICATION#
  #------------#
  
  
  
  ventoux_BD_nid <- ventoux_BD_nid %>%
    mutate (ouverture_canope = case_when(
      densite_canopy < 10 ~ "Milieu ouvert",
      densite_canopy >= 10 & densite_canopy <= 40 ~ "Foret ouverte",
      densite_canopy > 40 ~ "Foret fermee"
    ))
  
  # Calcul de la richesse spécifique moyenne par placette
  effectifs_moyens_canopy <- ventoux_BD_nid %>%
    group_by(ouverture_canope, Richness_nid) %>%
    summarise(Richness_moyenne_canopy = mean(Richness_nid_ass), .groups = "drop")
  
  nid_canope<- ggplot(effectifs_moyens_canopy, aes(x= ouverture_canope, y= Richness_moyenne_canopy, fill= Richness_nid))+
    geom_bar(stat = "identity", position = "dodge", alpha = 1) +  
    labs(title = "Richesse spécifique selon la densite de la canopé",
         x = "Ouverture de la canopée",
         y = "Richesse spécifique moyenne") +
    scale_fill_manual(values = cb_palette_nid) +  
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1)) 
  
  
  
  
  #ALIMENTATION#
  #------------#
  
  
  
  
  ventoux_BD_alim <- ventoux_BD_alim %>%
    mutate (ouverture_canope = case_when(
      densite_canopy < 10 ~ "Milieu ouvert",
      densite_canopy >= 10 & densite_canopy <= 40 ~ "Foret ouverte",
      densite_canopy > 40 ~ "Foret fermee"
    ))
  
  
  # Calcul de la richesse spécifique moyenne par placette
  effectifs_moyens_canopy2 <- ventoux_BD_alim %>%
    group_by(ouverture_canope, Richness_alim) %>%
    summarise(Richness_moyenne_canopy = mean(Richness_alim_ass), .groups = "drop")
  
  alim_canope <- ggplot(effectifs_moyens_canopy2, aes(x= ouverture_canope, y= Richness_moyenne_canopy, fill= Richness_alim))+
    geom_bar(stat = "identity", position = "dodge", alpha = 1) +  
    labs(title = "Richesse spécifique selon la densite de la canopé",
         x = "Ouverture de la canopée",
         y = "Richesse spécifique moyenne") +
    scale_fill_manual(values = cb_palette_alim) +  
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1)) 
  
}





# (i) La richesse spécifique totale est plus importante dans une forêt à composition mixte

if(TRUE) {   
  
  summary(tot1 <- glm(rs_tot ~ type_foret*alt,
                      family=negative.binomial, data=ventoux_BD1))
  
  phi1 <- summary(tot1)negative.binomial()phi1 <- summary(tot1)$deviance / summary(tot1)$df.residual #phi = 0.61
  
}

#(ii) La richesse spécifique totale est plus importante avec la densité de Gros Bois Vivant

if(TRUE) {
  
  summary(tot2 <- glm(rs_tot ~ densite_GBV,
                      family=poisson, data=ventoux_BD1))
  
  phi2 <- summary(tot2)$deviance / summary(tot2)$df.residual #phi = 0.62
}

# (iii) La richesse spécifique totale diminue avec une forte densité d'arbres vivants

if(TRUE) {
  
  summary(tot3 <- glm(rs_tot ~ densite_BV,
                      family=poisson, data=ventoux_BD1))
  
  phi3 <- summary(tot3)$deviance / summary(tot3)$df.residual #phi = 0.58
}

# (iv) La richesse spécifique totale diminue avec une forte densité de souches

if(TRUE) {
  
  summary(tot4 <- glm(rs_tot ~ densite_souche,
                      family=poisson, data=ventoux_BD1))
  
  phi4 <- summary(tot4)$deviance / summary(tot4)$df.residual #phi = 0.62
}

# (v) La richesse spécifique totale varie selon le type d'exploitation

if(TRUE) {
  
  summary(tot5 <- glm(rs_tot ~ forme,
                      family=poisson, data=ventoux_BD1))
  
  phi5 <- summary(tot5)$deviance / summary(tot5)$df.residual #phi = 0.52
}


# (vi) La richesse spécifique totale augmente lorsqu'il y a une grande richesse spécifique arborée

if(TRUE) {
  
  summary(tot6 <- glm(rs_tot ~ richesse_spe_arbre,
                      family=poisson, data=ventoux_BD1))
  
  phi6 <- summary(tot6)$deviance / summary(tot6)$df.residual #phi = 0.63
}


