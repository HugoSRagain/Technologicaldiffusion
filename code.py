# Installer les packages nécessaires
install.packages("igraph")
install.packages("plotly")

# Charger les bibliothèques
library(igraph)
library(plotly)

# Paramètres de base
A <- 1.5  # Facteur de productivité totale
alpha <- 0.3  # Élasticité du capital
beta <- 0.7  # Élasticité du travail
gamma <- 0.5  # Pondération de la diffusion d'innovation

# Génération d'un réseau social aléatoire (nœuds et connexions)
N <- 20  # Nombre de nœuds
p <- 0.2  # Probabilité de connexion entre nœuds
G <- erdos.renyi.game(N, p, type = "gnp", directed = FALSE)

# Calcul des centralités de degré pour chaque nœud
centrality <- degree(G)

# Initialisation des probabilités d'adoption d'innovation P_i(t) (aléatoires entre 0 et 1)
set.seed(42)  # Pour la reproductibilité
P <- runif(N, min = 0, max = 1)

# Capital et travail sur des plages de valeurs pour la simulation
K_values <- seq(10, 100, length.out = 50)  # Capital
L_values <- seq(50, 200, length.out = 50)  # Travail

# Fonction de production
production_function <- function(K, L, P, centrality, gamma) {
  # Calcul de la somme pondérée des probabilités d'adoption et des centralités
  innovation_term <- sum(P * centrality)
  # Calcul de la production
  Y <- A * (K^alpha) * (L^beta) * (innovation_term^gamma)
  return(Y)
}

# Matrice pour stocker les niveaux de production Y en fonction de K et L
Y_matrix <- matrix(0, nrow = length(K_values), ncol = length(L_values))

# Calcul de la production pour chaque combinaison de K et L
for (i in 1:length(K_values)) {
  for (j in 1:length(L_values)) {
    Y_matrix[i, j] <- production_function(K_values[i], L_values[j], P, centrality, gamma)
  }
}

# Visualisation 3D avec plotly
plot_ly(x = K_values, y = L_values, z = Y_matrix) %>%
  add_surface() %>%
  layout(scene = list(xaxis = list(title = 'Capital (K)'),
                      yaxis = list(title = 'Labor (L)'),
                      zaxis = list(title = 'Output (Y)')),
         title = "Output function with diffusion of innovation")
