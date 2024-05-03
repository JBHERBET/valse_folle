Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")


library(dplyr)
library(plotly)
library(ggplot2)
# install.packages("gganimate")
library("gganimate")

# Fonction pour calculer les points successifs d'une séquence
retourner_coordonnees <- function(angle_degres, pas, n_sequences, facteur_affaiblissement) {
  # Conversion de l'angle en radians
  angle_radians <- angle_degres * pi / 180

  # Initialisation du vecteur de points
  points <- numeric(0) + 0i

  # Initialisation du point courant et du vecteurs de points
  point_courant <- 0 + 0i
  points <- c(point_courant)

  # Initialisation de l'angle courant
  angle_courant <- angle_radians

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

    }
    # Application du facteur d'affaiblissement pour la séquence suivante
    pas <- pas * facteur_affaiblissement
  }

  # Conversion des points complexes en coordonnées (x, y)
  coordonnees <- tibble(
    x = Re(points),
    y = Im(points)
  )

  return(coordonnees)
}

# Exemple d'utilisation de la fonction
angle_degres <- 90 # Angle entre chaque pas
pas <- c(1, 3, 2)  # Séquence de pas
n_sequences <- 5  # Nombre de séquences à enchaîner

table_coordonnees <- retourner_coordonnees(112, c(1, 3,5,3), 400, 1)
# table_coordonnees <- retourner_coordonnees(115, c(3,4,3,2,4,3,4,5,4,3,3,1), 800, 1)
table_coordonnees <- retourner_coordonnees(115, c(3,4,3,2,4,3,4,5,4,3,3,2), 800, 1)

table_coordonnees <- table_coordonnees %>%
  mutate(numero_frame = row_number()) %>% as.data.frame()



# Créer le plot
p <- ggplot(table_coordonnees, aes(x, y)) +
  geom_point() +
  geom_path(color ="red") +
  transition_reveal(numero_frame)+
  theme_minimal()

animate(plot = p,duration =200)

