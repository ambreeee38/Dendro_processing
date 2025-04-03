#-----------------------------------------------------------------------------------------------------------
#---------------------------------------------I. VENTOUX-------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

 #### 1. Matrice de corrélation entre les variables ####

if(TRUE) {
  
  #Une matrice de corrélation est réalisée pour voir si des variables ont des corrélations supérieures à 0.8
  #entre elles. A ce compte là; simplement une variable sur les deux seraient gardées selon la littérature.
  
  #Dans notre cas, aucune variable n'est réellement corrélée, le maximum étant à 0.7
  
  
  cor_matrix <- cor(ventoux_BD1[, c("strate_h","strat_sa","strat_a","strat_A",
                                    "densite_canopy","densite_GBV","densite_GBM","densite_BV","densite_BMS",
                                    "densite_BMD","vol_BMS_tot", "vol_BMD_tot","vol_BM_tot","diversite_DMH",
                                    "densite_dmh_ha","decompo_moyen")],use="complete.obs")
  
  #Fonction permettant de voir le nombre de corrélation > 0.8 (/!\ CHIFFRE A VERIFIER /!\)
  
  cor_matrix[upper.tri(cor_matrix, diag = FALSE) & abs(cor_matrix) > 0.75]
  
  # Visualisation de la matrice

colormap <- viridis(200, option = "C")  
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.9, tl.col = "black", addCoef.col = "black",
         col = colormap, cl.cex = 0.9, number.cex = 0.7, 
         insig = "blank", diag = FALSE)

}

 #### 2. VISUALISATION GRAPHIQUE ####
   
 ### 2.1. Visualisation graphique guildes et variables liées au bois mort ###


  
  #Réaménagement du jeu de données en créant une colonne "Richness_nidification" qui reprend
  #les 3 sous groupes/placette. Ainsi, nous avons, par exemple, pour la placette 39 rs_Cavicole,
  #rs_Arboricole, rs_Sol, et "Richness_nid_ass" pour la richesse spécifique associée sur la
  #placette. Ceci permet de faire des graphiques avec l'ensemble des guildes sur le même
  #graph.


        ## 2.1.1. Volume bois mort total ##

if(TRUE) {
  
          # 2.1.1.1.1 Bois mort total et nidification #
  
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
    geom_smooth(method = "lm", se = TRUE, size = 1.2, alpha=0.2) +  
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

  nid_bm
 
        # # 2.1.1.1.1 Bois mort total et alimentation #
  
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
  
}

   
      # 2.1.2. Densité bois mort total #

if(TRUE) {
  
        # 2.1.1.2.1. Densite bois mort total pour les groupes de nidification
  
  
  #BOXPLOT#
  
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
  ventoux_BD_nid <- ventoux_BD_nid %>%
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
  
  
 #GRAPHIQUE REGRESSION LINEAIRE#
 
 
 ventoux_BD_nid$Richness_nid <- factor(ventoux_BD_nid$Richness_nid, levels = c("rs_Cavicole", "rs_Arboricole", "rs_Sol"))
 
 
 cb_palette_nid <- c("rs_Cavicole" = "#E69F00",
                     "rs_Arboricole" = "#8fd175", 
                     "rs_Sol" = "brown")        
 
 
 nid_bm <- ggplot(ventoux_BD_nid, aes(x = densite_BM_tot, y = Richness_nid_ass, color = Richness_nid)) +
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
 
 nid_bm
 
  
        # 2.1.1.2.2. Bois mort total en fonction des groupes alimentaires
  
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
  
 (nid_bm_densite_barplot + nid_bm_densite) /nid_bm
  
  
}


       # 2.1.3. Taux de décomposition moyen du bois sur la placette #

if(TRUE) {
  
       # 2.1.1.2.1. Decomposition moyen du bois pour les groupes nidification #
  
  
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
  
  
  # Calcul de la richesse spécifique MOYENNE par placette
  effectifs_moyens <- ventoux_BD_nid %>%
    group_by(decompo_moyen_label, Richness_nid) %>%
    summarise(Richness_moyenne = mean(Richness_nid_ass), .groups = "drop")
  
  # Filtrer les NA pour éviter les erreurs dans le graphique
  ventoux_BD_nid <- ventoux_BD_nid %>%
    filter(!is.na(decompo_moyen_label), !is.na(Richness_nid_ass), !is.na(Richness_nid))
  
  # Création du boxplot
  nid_decompo <- ggplot(ventoux_BD_nid, aes(x = decompo_moyen_label, y = Richness_nid_ass, fill = Richness_nid)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Affiche un boxplot sans les outliers
    labs(title = "Distribution de la richesse spécifique selon le taux de décomposition du bois mort",
         x = "Taux de décomposition du bois",
         y = "Richesse spécifique moyenne par placette") +
    scale_fill_manual(values = cb_palette_nid, 
                      labels = c("Cavicole", "Arboricole", "Sol")) +
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1))  # Rotation pour lisibilité
  
 nid_decompo
  
  
      # 2.1.1.2.2. Decomposition moyen du bois pour les groupes alimentaires #
  
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
 
 
  #Apparition des 2 plots sur la même page
  
   nid_decompo + alim_decompo
}


      # 2.1.4. Densite souche (indice de défrichement) #


if(TRUE) {
  
     # 2.1.1.3.1. Densite souche selon groupes nidification #
  
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
  
  
  #Apparition de sgraph sur la meme page plot
  nid_souche + alim_souche
  
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
  
  
  
  
  ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Cavicole, color = stratification)) +
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
  
  
  
  
  ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Arboricole, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique cavicole et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique arboricole",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels
  
  
    
  
  
  
  #RS_SOL#
  #------#
  
  
  
  
  ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Sol, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique cavicole et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique nicheurs au sol",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels
  
  
  
  
  #RS_INSECTIVORES#
  #---------------#
  
  
  
  ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Insectivore, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique cavicole et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique insectivores",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels
  
  
  
  
  
  #RS_OMNIVORES#
  #------------#
  
  
  
  ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Omnivore, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique cavicole et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique omnivore",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels
  
  
  
  
  
  
  
  #RS_HERBIVORE#
  #------------#
  
  
  
  
  ggplot(ventoux_BD_strate, aes(x = pct_recouvrement, y = rs_Herbivore, color = stratification)) +
    geom_point(alpha = 0.6) +  # Ajout des points pour visualiser les données
    geom_smooth(method = "lm", se = TRUE) +  # Régression linéaire avec intervalle de confiance
    theme_minimal() +
    labs(
      title = "Relation entre la richesse spécifique cavicole et le recouvrement des strates",
      x = "Pourcentage de recouvrement",
      y = "Richesse spécifique herbivore",
      color = "Stratification"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des label
  
  
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
  
  
  
  ggplot(ventoux_BD_compo, aes(x = pct_recouvrement, y = rs_Cavicole, color = stratification)) +
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
  
}
