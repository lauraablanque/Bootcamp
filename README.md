#Entrenamiento 

# 0) Paquetes 
library(ggplot2)
library(cluster)

# 1) Carga del dataset 
dataset <- read.csv("C:/Users/laura/Downloads/Mall_Customers.csv")

# Vista rápida
head(dataset)
dim(dataset)
str(dataset)
summary(dataset)

# 2) Limpieza / preparación 
names(dataset) <- c("CustomerID", "Genre", "Age", "Income", "Spending")

# Codificar Genre: 1 = Male, 0 = Female
dataset$GenreNum <- ifelse(tolower(dataset$Genre) == "male", 1, 0)

# Comprobar NA
colSums(is.na(dataset))

# 3) Normalización (scale) 
dataset_scaled <- scale(dataset[, c("Income", "Spending")])

# 4) K-Means 
# 4.1 Método del codo (k = 2 a 6)
set.seed(123)

wss <- sapply(2:6, function(k) {
  kmeans(dataset_scaled, centers = k, nstart = 25)$tot.withinss
})

plot(2:6, wss, type = "b",
     main = "Método del codo (K-Means)",
     xlab = "Número de clusters (k)",
     ylab = "Tot.WithinSS")

# 4.2 Entrenar modelo final (elige k según el codo)
k_opt <- 5  
set.seed(123)
kmeans_model <- kmeans(dataset_scaled, centers = k_opt, nstart = 25)

# Asignar clusters al dataset
dataset$cluster_kmeans <- kmeans_model$cluster
table(dataset$cluster_kmeans)

# 5) Clustering jerárquico 
# Distancia euclídea
d <- dist(dataset_scaled, method = "euclidean")

# hclust con ward.D
hclust_model <- hclust(d, method = "ward.D")

# Dendrograma
plot(hclust_model, labels = FALSE,
     main = "Dendrograma - Clustering jerárquico (ward.D)")

# Cortar dendrograma en k clusters (usa el mismo k para comparar)
h_opt <- k_opt
dataset$cluster_hier <- cutree(hclust_model, k = h_opt)
table(dataset$cluster_hier)

# 6) Evaluación con silueta 
# Silueta K-Means
sil_km <- silhouette(as.integer(dataset$cluster_kmeans), d)
avg_sil_km <- mean(sil_km[, "sil_width"])

# Silueta jerárquico
sil_hc <- silhouette(as.integer(dataset$cluster_hier), d)
avg_sil_hc <- mean(sil_hc[, "sil_width"])

cat("\n===== SILUETA PROMEDIO =====\n")
cat("K-Means:      ", round(avg_sil_km, 3), "\n")
cat("Jerárquico:   ", round(avg_sil_hc, 3), "\n")

best_model <- ifelse(avg_sil_km >= avg_sil_hc, "K-Means", "Jerárquico")
cat("Modelo seleccionado:", best_model, "\n\n")

# 7) Visualización de clusters
# 7.1 K-Means (Income vs Spending)
p1 <- ggplot(dataset, aes(x = Income, y = Spending, color = factor(cluster_kmeans))) +
  geom_point(size = 2, alpha = 0.85) +
  labs(title = paste("Clusters K-Means (k =", k_opt, ")"),
       x = "Annual Income (k$)", y = "Spending Score (1-100)",
       color = "Cluster") +
  theme_minimal()

print(p1)

# 7.2 Jerárquico (Income vs Spending)
p2 <- ggplot(dataset, aes(x = Income, y = Spending, color = factor(cluster_hier))) +
  geom_point(size = 2, alpha = 0.85) +
  labs(title = paste("Clusters Jerárquicos (k =", h_opt, ")"),
       x = "Annual Income (k$)", y = "Spending Score (1-100)",
       color = "Cluster") +
  theme_minimal()

print(p2)

# 8) Análisis descriptivo de segmentos

cat("===== PERFILES PROMEDIO POR CLUSTER (K-Means) =====\n")
profile_km <- aggregate(cbind(Age, Income, Spending) ~ cluster_kmeans, data = dataset, mean)
print(profile_km)

cat("\n===== PERFILES PROMEDIO POR CLUSTER (Jerárquico) =====\n")
profile_hc <- aggregate(cbind(Age, Income, Spending) ~ cluster_hier, data = dataset, mean)
print(profile_hc)

# 9) Interpretación básica
cat("\n===== INTERPRETACIÓN (GUIA) =====\n")
cat("- Segmentos con Income alto y Spending alto: clientes premium / alto valor.\n")
cat("- Income alto y Spending bajo: potencial para estrategias de activación/cross-selling.\n")
cat("- Income bajo y Spending alto: compradores impulsivos o sensibles a promociones.\n")
cat("- Income bajo y Spending bajo: segmento de menor impacto.\n")

cat("\nFin del script.\n")

# RESULTADOS Y CONCLUSIONES
# El método del codo sugiere un número óptimo de clusters igual a k_opt.
# El modelo K-Means presenta una silueta promedio de avg_sil_km,
# mientras que el modelo jerárquico obtiene avg_sil_hc.
# El modelo seleccionado es best_model por presentar mayor calidad de clusters.
# Se identifican segmentos diferenciados por nivel de ingresos y comportamiento de gasto,
# como clientes premium (alto ingreso y alto gasto) y clientes de bajo impacto.
# Bootcamp
