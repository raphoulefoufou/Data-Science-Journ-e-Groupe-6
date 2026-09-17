library(rpart)

tree.fit <- rpart(DIFF ~ R32 + R17 + R7 + R8 + R22 + TOF + AGE, data = farm_train, method = "class")
# Commencons par faire un arbre sur l'ensemble du modele
par(xpd=NA)#sinon le texte est parfois rogné
plot(tree.fit)
text(tree.fit, digits=3)

library(rpart.plot)

rpart.plot(
  tree.fit, 
  type = 2,           # Affiche les règles sous chaque nœud
  extra = 104,        # Affiche les pourcentages et effectifs par classe
  shadow.col = "gray",
  main = "Arbre de Décision — Prédiction de Défaillance"
)
# ICI on fait du surraprentissage on va donc elaguer c a dire suppriemr les branches inutiles.


# Élagage avec un paramètre de complexité plus strict
tree.pruned <- prune(tree.fit, cp = 0.02)

# Affichage du nouvel arbre
rpart.plot(
  tree.pruned, 
  type = 2, 
  extra = 104, 
  box.palette = "RdYlGn",
  main = "Arbre de Décision Élagué"
)
# Ici on creer le fichier de prediction 
farms_test$DIFF <- ifelse(predict(tree.pruned, newdata = farms_test, type = "class") == "Saine", 1, 0)

head(farms_test[, c("ID", "DIFF")]) 
write.csv(farms_test[, c("ID", "DIFF")], "soumission_arbre2.csv", row.names = FALSE)