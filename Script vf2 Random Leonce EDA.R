library(dplyr)
library(haven)
library(tidyverse)
library(dplyr)
library(readr)
setwd("C:/Users/SWEED/Downloads/Léonce EDA/Data Léonce EDA")
datarandleonce <- read.csv(file="Data Random Leonce EDA.csv", h=TRUE, dec=",", sep=";")
view(datarandleonce)
datarandleonce <- datarandleonce %>%
  mutate(
    chweight = as.numeric(gsub(",", ".", chweight)),
    Couv_vaccinale = as.factor(Couv_vaccinale)
  )

str(datarandleonce$Couv_vaccinale)
attach(datarandleonce)
str(Couv_vaccinale)

str(datarandleonce$Couv_vaccinale_g)
table(datarandleonce$Couv_vaccinale_g)
table(datarandleonce$Couv_vaccinale)

library(randomForest)
str(Couv_vaccinale)
table(Couv_vaccinale)

# A- Modèle de régression Random Forest avec pondération (régression sur Couv_vaccinale_g)

data_class1 <- datarandleonce %>%
  select(-c(Couv_vaccinale, ID, CPN, Nb_enft_men, Age_CM, Fecondite, Age_Mere)) %>%
  na.omit()
model_rf1 <- randomForest(Couv_vaccinale_g ~ ., data = data_class1, case.weigths=datarandleonce$chweight, importance = TRUE, na.action = na.omit)
print(model_rf1)

# B- Modèle de classification Random Forest avec pondération (régression sur Couv_vaccinale)

#Supprimer les variables non-nécessaires
data_class <- datarandleonce %>%
  select(-c(Couv_vaccinale_g, ID, CPN, Nb_g_enft_men, Age_g_CM, Age_g_Mere, chweight, Fecondite_g)) %>%
  na.omit()
#Lancer le modèle random Forest (classification)
model_rf2 <- randomForest(
  Couv_vaccinale ~ .,
  data = data_class,
  case.weights = data_class$chweight,
  importance = TRUE
)

#Lancer le modèle random Forest (classification)
model_rf2 <- randomForest(
  Couv_vaccinale ~ .,
  data = data_class,
  case.weights = data_class$chweight,
  importance = TRUE
)

# C- Améliorer la prédiction de Random Forest
plot(model_rf2$err.rate[, 1], type = "l", xlab = "Nombre d'arbres", ylab = "erreur OOB")

#Ntree approprié = 150

# Modèle random forest finalement retenu (classification)

model_rf2 <- randomForest(
  Couv_vaccinale ~ .,
  data = data_class,
  case.weights = data_class$chweight,
  ntree = 150,
  importance = TRUE
)
par(mfrow = c(1, 2))

#Ntree final 
plot(model_rf2$err.rate[, 1], type = "l", xlab = "Nombre d'arbres", ylab = "erreur OOB", main = "Ntree du modele final")

# D- Sortir les résultats finaux du modèle en fonction des variables

print(model_rf2)

out <- capture.output(print(model_rf2))

cat(out, sep = "\n")

writeLines(out, "resume_model_rf2.txt")

graphics.off()
while(dev.cur() > 1) dev.off()

importance(model_rf2)
varImpPlot(model_rf2, main = "Classement Brut des déterminants de la Couverture vaccinale selon le modèle décisionnel")

model_rf2$importance[order(model_rf2$importance[, 1], decreasing = TRUE), ]

out <- capture.output(model_rf2$importance[order(model_rf2$importance[, 1], decreasing = TRUE), ])
cat(out, sep = "\n")
writeLines(out, "Valeur_propre_Model_rf_2.csv")

varImpPlot(model_rf2, type = 2, n.var = 10, main = "Top 10 déterminants les plus importants")

# E- Colorier les variables par niveau d'importance

library(ggplot2)
library(patchwork)

#1- Du bleu et du gris

# Extraire l’importance des variables
imp <- importance(model_rf2, type = 2)
imp_df <- data.frame(
  Variable = rownames(imp),
  Importance = imp[, 1]
)

# Garder les 10 plus importantes et colorier
imp_df <- imp_df %>%
  arrange(desc(Importance)) %>%
  mutate(Top = ifelse(row_number() <= 10, "Top 10", "Autre"))

ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Top)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Top 10" = "steelblue", "Autre" = "gray")) +
  labs(title = "Importance des variables - Classification",
       x = "Déterminants de la Couverture vaccincale", y = "Importance (Mean Decrease Gini)") +
  theme_minimal()

#2- Du bleu dégradé progressif

imp_df <- imp_df %>%
  arrange(Importance)  # ici on garde du plus petit au plus grand pour le dégradé

ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(
    low = "#a6cee3",  # bleu clair
    high = "#08306b"  # bleu foncé
  ) +
  labs(
    title = "Importance des variables explicatives - Classification",
    x = "Déterminants de la Couverture vaccincale",
    y = "Importance (Mean Decrease Gini)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # facultatif : retire la légende
    plot.title = element_text(face = "bold", size = 14)
  )

#3- Une double échelle bleu dégradé prossif et orange dégradé prossif

library(ggplot2)
library(dplyr)

imp <- importance(model_rf2, type = 2)
imp_df <- data.frame(
  Variable = rownames(imp),
  Importance = imp[, 1]
)

imp_df <- imp_df %>%
  arrange(desc(Importance)) %>%
  mutate(
    Groupe = ifelse(row_number() <= 10, "Déterminants critiques", "Déterminants modérés"),
    Rank = ifelse(Groupe == "Déterminants critiques",
                  rank(-Importance),  # pour avoir un dégradé croissant
                  rank(Importance))   # pour avoir un dégradé croissant
  )

n_crit <- sum(imp_df$Groupe == "Déterminants critiques")
n_mod  <- sum(imp_df$Groupe == "Déterminants modérés")

couleurs_crit <- colorRampPalette(c("#08306b" , "#c6dbef"))(n_crit)
couleurs_mod  <- colorRampPalette(c("#fee6ce", "#e6550d"))(n_mod)

imp_df <- imp_df %>%
  group_by(Groupe) %>%
  arrange(Groupe, Rank) %>%
  mutate(Couleur = ifelse(Groupe == "Déterminants critiques", 
                          couleurs_crit[Rank],
                          couleurs_mod[Rank])) %>%
  ungroup()

ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", aes(fill = Variable), show.legend = FALSE) +
  scale_fill_manual(values = setNames(imp_df$Couleur, imp_df$Variable)) +
  coord_flip() +
  facet_wrap(~Groupe, scales = "free_y", ncol = 1, strip.position = "top") +
  labs(
    title = "Importance des variables explicatives (avec nuances internes)",
    x = "Déterminants de la Couverture vaccincale",
    y = "Importance (Mean Decrease Gini)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10)
  )

#4- Une double échelle bleu dégradé prossif et orange dégradé prossif
#Couleur bleue (steelblue)
library(ggplot2)
library(dplyr)

imp <- importance(model_rf2, type = 2)
imp_df <- data.frame(
  Variable = rownames(imp),
  Importance = imp[, 1]
)

imp_df <- imp_df %>%
  arrange(desc(Importance)) %>%
  mutate(
    Groupe = ifelse(row_number() <= 10, "Déterminants critiques", "Déterminants modérés"),
    Rank = ifelse(Groupe == "Déterminants critiques",
                  rank(-Importance),  # pour avoir un dégradé croissant
                  rank(Importance))   # pour avoir un dégradé croissant
  )

n_crit <- sum(imp_df$Groupe == "Déterminants critiques")
n_mod  <- sum(imp_df$Groupe == "Déterminants modérés")

couleurs_crit <- colorRampPalette(c("#4682B4", "#a6cee3"))(n_crit)
couleurs_mod  <- colorRampPalette(c("#fee6ce", "#e6550d"))(n_mod)

imp_df <- imp_df %>%
  group_by(Groupe) %>%
  arrange(Groupe, Rank) %>%
  mutate(Couleur = ifelse(Groupe == "Déterminants critiques", 
                          couleurs_crit[Rank],
                          couleurs_mod[Rank])) %>%
  ungroup()

ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", aes(fill = Variable), show.legend = FALSE) +
  scale_fill_manual(values = setNames(imp_df$Couleur, imp_df$Variable)) +
  coord_flip() +
  facet_wrap(~Groupe, scales = "free_y", ncol = 1, strip.position = "top") +
  labs(
    title = "Importance des variables explicatives (avec nuances internes)",
    x = "Déterminants de la Couverture vaccincale",
    y = "Importance (Mean Decrease Gini)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10)
  )


# F- Voir les relations entre les déterminants critiques et la Couverture vaccinale
attach(data_class)
str(data_class)

par(mfrow = c(2, 2))

plot(Couv_vaccinale ~ Age_enft, data = data_class, main = "Âge de l'enfant")
plot(Couv_vaccinale ~ Age_CM, data = data_class, main = "Âge du chef de ménage")
plot(Couv_vaccinale ~ Age_Mere, data = data_class, main = "Âge de la mère")
plot(Couv_vaccinale ~ Fecondite, data = data_class, main = "Fécondité")


par(mfrow = c(1, 1))

m=margin(model_rf2)

# Extraire le premier arbre pour exemple

install.packages(c("rpart", "rpart.plot"))
library(rpart, rpart.plot)

library(partykit)

getTree(model_rf2, k = 1, labelVar = TRUE)

sapply(data_class, class)

data_class[] <- lapply(data_class, function(x) if (is.character(x)) as.factor(x) else x)

tree_model <- ctree(Couv_vaccinale ~ Age_enft + Age_CM + Age_Mere + Carnet + Depart_resid + Fecondite + ethnie_CM + Nvo_vie + Nb_enft_men + Decideur_soins , data = data_class)
plot(tree_model)

# Couleurs et optimisation
library(partykit)

ctrl <- ctree_control(mincriterion = 0.95,  # seuil de test statistique (1 = arbre plus petit)
                      minsplit = 20,        # nombre minimal d'observations pour faire une division
                      maxdepth = 5)         # profondeur max de l'arbre

tree_model <- ctree(Couv_vaccinale ~ Age_enft + Age_CM + Age_Mere + Carnet + Depart_resid + Fecondite + ethnie_CM + Nvo_vie + Nb_enft_men + Decideur_soins, data = data_class)

plot(tree_model,
     gp = gpar(fill = c("lightblue", "lightgreen", "lightpink", "lightyellow")),  # couleurs alternées pour les noeuds
     tp_args = list(fill = "orange"),  # couleur des feuilles
     ep_args = list(col = "brown"),   # couleur des étiquettes
     ip_args = list(fontsize = 10),   # taille du texte des noeuds internes
     terminal_panel = node_terminal)  # affichage plus détaillé en terminal (optionnel)
)

plot(tree_model,
     gp = gpar(cex = 0.8,  # taille du texte
               fill = c("lightblue", "lightgreen", "lightpink", "lightyellow")),  # couleurs des noeuds
     tp_args = list(fill = "orange"),
     main = "Extraction d'un arbre de la forêt aléatoire"
)

graphics.off()

view(datarandleonce)
view(data_class)
