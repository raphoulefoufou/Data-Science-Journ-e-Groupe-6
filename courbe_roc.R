library(pROC)

prob_train <- predict(tree.pruned, newdata = farm_train, type = "prob")[, "Défaillante"]

roc_obj <- roc(farm_train$DIFF, prob_train)
auc_value <- auc(roc_obj)

cat("AUC Train :", round(auc_value, 4), "\n")
plot(roc_obj, main = paste("Courbe ROC - AUC =", round(auc_value, 3)))