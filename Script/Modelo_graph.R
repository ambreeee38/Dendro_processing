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


ventoux_long <- ventoux_BD1 %>%
  pivot_longer(cols = c(rs_Cavicole, rs_Arboricole, rs_Sol), 
               names_to = "Type", values_to = "Richesse")


cb_palette <- c("rs_Cavicole" = "#E69F00",   # Orange
                "rs_Arboricole" = "#8fd175", # Bleu
                "rs_Sol" = "brown")        # Vert

# Graphique
ggplot(ventoux_long, aes(x = vol_BMS_tot, y = Richesse, color = Type)) +
  geom_point(alpha = 0.7, size = 2) +  
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +  
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





