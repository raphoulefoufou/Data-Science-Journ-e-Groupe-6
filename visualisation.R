farm_train <- read.csv2('farms_train.csv',sep=',')
View(farm_train)

library(dplyr)


# 1. Nettoyage des données 
farm_train <- farm_train %>%
  mutate(
    across(c(R7, R8, R17, R22, R32), ~ as.numeric(gsub(",", ".", .))),
    DIFF = factor(DIFF, levels = c(0, 1), labels = c("Défaillante", "Saine")),
    TOF = factor(TOF)
  )

glimpse(farm_train)
library(tidyr)
library(dplyr)


farm_train_long <- farm_train %>%
  pivot_longer(
    cols = c(R7, R8, R17, R22, R32),
    names_to = "Ratio",
    values_to = "Valeur"
  )

# Creation box plot 
ggplot(farm_train_long, aes(x = DIFF, y = Valeur, fill = DIFF)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  facet_wrap(~ Ratio, scales = "free_y") +  # Permet à chaque ratio d'avoir son propre axe Y
  theme_minimal() +
  labs(
    title = "Comparaison des ratios financiers selon la santé de l'exploitation",
    x = "Santé de l'exploitation",
    y = "Valeur du ratio"
  )

liste <- c("R7", "R8", "R17", "R22", "R32")

for (var in liste) {
  cat(" Test t de Student pour la variable :", var, "\n")
  formule <- as.formula(paste(var, "~ DIFF"))
    print(t.test(formule, data = farm_train))
}
# Ici les grands gagnants sont R32 R17 R7 

ggplot(farm_train, aes(x = DIFF, y = AGE, fill = DIFF)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution de l'âge selon la santé de l'exploitation",
    x = "Santé", y = "Âge"
  )

# 2. Test t de Student
t.test(AGE ~ DIFF, data = farm_train)




table_tof <- table(farm_train$DIFF, farm_train$TOF)
print(table_tof)

# 2. Visualisation des proportions par type d'exploitation
ggplot(farm_train, aes(x = factor(TOF), fill = DIFF)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Proportion de défaillances par type d'exploitation (TOF)",
    x = "Type d'exploitation (TOF)",
    y = "Proportion"
  )

# 3. Test du Khi-deux
chisq.test(table_tof)