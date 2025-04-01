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


  ### 2.1. Visualisation graphique nidification ###
   
    ## 2.1.1. Visualisation graphique nidification et variables liées au bois mort ##


  
  #Réaménagement du jeu de données en créant une colonne "Richness_nidification" qui reprend
  #les 3 sous groupes/placette. Ainsi, nous avons, par exemple, pour la placette 39 rs_Cavicole,
  #rs_Arboricole, rs_Sol, et "Richness_nid_ass" pour la richesse spécifique associée sur la
  #placette. Ceci permet de faire des graphiques avec l'ensemble des guildes sur le même
  #graph.
  
        # 2.1.1.1. Bois mort total #

if(TRUE) {
  
  ventoux_BD1 <- ventoux_BD1 %>%
    pivot_longer(cols = c(rs_Cavicole, rs_Arboricole, rs_Sol), 
                 names_to = "Richness_nid", 
                 values_to = "Richness_nid_ass") 
  
  ventoux_BD1$Richness_nid <- factor(ventoux_BD1$Richness_nid, levels = c("rs_Cavicole", "rs_Arboricole", "rs_Sol"))
  
  
  cb_palette <- c("rs_Arboricole" = "#8fd175",
                  "rs_Cavicole" = "#E69F00", 
                  "rs_Sol" = "brown")        
  

  ggplot(ventoux_BD1, aes(x = vol_BM_tot, y = Richness_nid_ass, color = Richness_nid)) +
    geom_point(alpha = 0.7, size = 2) +  
    geom_smooth(method = "lm", se = TRUE, size = 1.2, alpha=0.2) +  
    labs(title = "Richesse spécifique en fonction du volume BMS total",
         x = "Volume BMS total",
         y = "Richesse spécifique") +
    scale_color_manual(values = cb_palette, 
                       labels = c("Cavicole", "Arboricole", "Sol")) +
    theme_minimal(base_size = 14) +  
    theme(legend.title = element_blank(),  
          legend.position = "top",  # Déplace la légende en haut
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank())  # Supprime les petites lignes de grille

}
  
  
    # 2.1.1.2. Taux de décomposition moyen du bois sur la placette #

if(TRUE) {
  
  
  #Modification du taux de décomposition moyen en classes plus parlantes que 1,2,3, 4
  
  
  
  #Permet d'associer les bonnes couleurs aux "richness_nid" car sinon les attributs par ordre
 #alphabétique
  ventoux_BD1$Richness_nid <- factor(ventoux_BD1$Richness_nid, levels = c("rs_Cavicole", "rs_Arboricole", "rs_Sol"))
  
  
  #Permet de renommer les numéros comme on le souhaite
  ventoux_BD1 <- ventoux_BD1 %>%
    mutate(decompo_moyen_label = case_when(
      decompo_moyen == 1 ~ "Non décomposé",
      decompo_moyen == 2 ~ "Décomposition légère",
      decompo_moyen == 3 ~ "Décomposition avancée",
      decompo_moyen == 4 ~ "Décomposition très avancée",
      TRUE ~ as.character(decompo_moyen)
    ))
  
  
  #Permet de filtrer les NA du jeu de données afin qu"ils n'apparaissent pas sur le graphique
  ventoux_BD1 <- ventoux_BD1 %>%
    filter(!is.na(decompo_moyen_label)) 
  
  
  #R classe les variables en ordre alphabétique  sur l'axe des x, cette fonction permet
  #de les classer dans l'ordre que l'on souhaite
  ventoux_BD1 <- ventoux_BD1 %>%
    mutate(decompo_moyen_label = factor(decompo_moyen_label, 
                                        levels = c("Non décomposé", "Décomposition légère", "Décomposition avancée","Décomposition très avancée")))  # Remplace les autres niveaux si besoin
  
  
  # Calcul des effectifs par catégorie
  effectifs <- ventoux_BD1 %>%
    group_by(decompo_moyen_label, Richness_nid) %>%
    summarise(Richness_total = sum(Richness_nid_ass), .groups = "drop")
  
  # Barplot 
  ggplot(effectifs, aes(x = decompo_moyen_label, y = Richness_total, fill = Richness_nid)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 1) +  
    geom_text(aes(label = Richness_total), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, 
              size = 4) +  # Ajoute les effectifs au-dessus des barres
    labs(title = "Richesse spécifique selon le taux de décomposition du bois mort",
         x = "Taux de décomposition du bois",
         y = "Richesse spécifique") +
    scale_fill_manual(values = cb_palette, 
                      labels = c("Cavicole", "Arboricole", "Sol")) +
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1))  
  
  
}


   # 2.1.1.3. Densite souche (indice de défrichement) #

if(TRUE) {
  
  #Ce graphique permet de visualiser l'impact de l'intensité du défrichement sur les
  #communuatés d'oiseaux.
  
  
  # Définition des classes de défrichement
  
  define_classes <- function(densite_souche) {
    case_when(
      densite_souche == 0 ~ "Aucun défrichement",
      densite_souche > 0 & densite_souche <= 50 ~ "Défrichement léger",
      densite_souche > 50 & densite_souche <= 120 ~ "Défrichement modéré",
      densite_souche > 120 & densite_souche <= 180 ~ "Défrichement intense",
      densite_souche > 180 ~ "Défrichement très intense"
    )
  }
  
  # Ajout de la colonne de classification
  ventoux_BD1 <- ventoux_BD1 %>%
    mutate(defrichement_class = define_classes(densite_souche))
  
  # Réordonne les niveaux du facteur pour garantir l'ordre souhaité
  # #Permet d'associer les bonnes couleurs aux "richness_nid" car sinon les attributs par ordre
  #alphabétique
  ventoux_BD1 <- ventoux_BD1 %>%
    mutate(defrichement_class = factor(defrichement_class, 
                                       levels = c("Aucun défrichement", 
                                                  "Défrichement léger", 
                                                  "Défrichement modéré", 
                                                  "Défrichement intense", 
                                                  "Défrichement très intense")))
  
  # Regroupement par classe de défrichement et guilde écologique
  richesse_totale <- ventoux_BD1 %>%
    group_by(defrichement_class, Richness_nid) %>%
    summarise(Richesse_total = sum(Richness_nid_ass, na.rm = TRUE), .groups = "drop")
  
  # Création du barplot
  ggplot(richesse_totale, aes(x = defrichement_class, y = Richesse_total, fill = Richness_nid)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 1) +  
    geom_text(aes(label = Richesse_total), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
    labs(title = "Richesse spécifique totale selon les guildes écologiques et le niveau de défrichement",
         x = "Classe de défrichement",
         y = "Richesse spécifique totale") +
    scale_fill_manual(values = cb_palette) +
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top",  
          panel.grid.major = element_line(color = "gray85"),  
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1))
  
}

# 2.1.1.4.  #
