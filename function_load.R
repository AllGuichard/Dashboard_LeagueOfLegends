library(fmsb)

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 1.3,
                                        caxislabels = NULL, title = NULL){
  radarchart(
    data, axistype = 1,
    # Personnaliser le polygone
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Personnaliser la grille
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Personnaliser l'axe
    axislabcol = "grey", 
    # Étiquettes des variables
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = NULL, title = title, 
    
  )
}