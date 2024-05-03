rm(list=ls())
# Chargement de la librairie ggplot2
library(ggplot2)

# Fonction pour calculer les points successifs d'une séquence
valse_folle <- function(angle_degres, pas, n_sequences, facteur_affaiblissement) {
  # Conversion de l'angle en radians
  angle_radians <- angle_degres * pi / 180
  
  # Initialisation du vecteur de points
  points <- numeric(0) + 0i
  
  # Initialisation du point courant et du vecteurs de points
  point_courant <- 0 + 0i
  points <- c(point_courant)
  
  # Initialisation de l'angle courant
  angle_courant <- angle_radians
  
  # Création d'un ggplot vide
  p <- ggplot() +
    coord_equal() +
    theme_minimal() +
    labs(
      title = "Représentation graphique de la valse",
      x = "",
      y = ""
    )
  
  # Boucle sur les séquences
  for (seq in 1:n_sequences) {
    # Boucle sur les pas de la séquence
    for (i in seq_along(pas)) {
      p_i <- pas[i]
      # Calcul du nouvel angle pour ce pas
      angle_courant <- angle_courant + angle_radians
      
      # Calcul du nouveau point
      nouveau_point <- point_courant + (p_i * exp(1i * angle_courant))
      
      # Ajout du nouveau point au vecteur de points
      points <- c(points, nouveau_point)
      
      # Mise à jour du point courant
      point_courant <- nouveau_point
      
      # Création d'un tibble avec les nouvelles données
      new_data <- tibble(
        x = Re(points),
        y = Im(points)
      )
      
      # Mise à jour du ggplot avec les nouvelles données
      p <- p +
        geom_point(data = new_data, aes(x = x, y = y)) +
        geom_path(data = new_data, aes(x = x, y = y))
      
      # Affichage du ggplot mis à jour
      print(p)
      
      # Délai entre chaque itération
      Sys.sleep(0.1)
    }
    
    # Application du facteur d'affaiblissement pour la séquence suivante
    pas <- pas * facteur_affaiblissement
  }
  
  # Conversion des points complexes en coordonnées (x, y)
  coordonnees <- tibble(
    x = Re(points),
    y = Im(points)
  )
  
  # Affichage du graphique final
  print(ggplot(coordonnees, aes(x = x, y = y)) +
          geom_path() +
          geom_point() +
          coord_equal() +
          theme_minimal() +
          labs(
            title = "Représentation graphique de la valse",
            x = "",
            y = ""
          ))
}

# Exemple d'utilisation de la fonction
angle_degres <- 90 # Angle entre chaque pas
pas <- c(1, 3, 2)  # Séquence de pas
n_sequences <- 5  # Nombre de séquences à enchaîner
facteur_affaiblissement <- 1.5  # Facteur d'affaiblissement entre les séquences

valse_folle(90, c(1, 2, 1), 5, 0.9)
