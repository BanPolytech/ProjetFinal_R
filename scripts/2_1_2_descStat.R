###########################################################################
#
# Formation MBA ESG - Big Data Chief Data Officer
#
# Projet Final : Prédicton du pays de destination des gens sur Airbnb
#
# Formateur : Xue ZHAO
# Mail : xue.zhao@protonmail.com
#
#  Etudiants :  Amine ALIANE, Meriam HAMMOUDA, 
#               Imed Eddine BOUSSOUF, Khaoula ELMOUTAMID
#               Lina SAIDANE, Esteban GOBERT
#
#  Mails : amine_aliane@hotmail.fr
#         hammouda.meriam@yahoo.fr
#         imededdine.boussouf@yahoo.com
#         khaoula.elmoutamid@gmail.com
#         lina.saidane96@gmail.com
#         esteban.gobert@me.com
#
###########################################################################

### 2. Statistiques descriptives de données : "train_users": ---------------------------
# stats basiques sur toutes les données, stats univariées sur chaque colonnes
real_train_users %>%
  select(-id) %>%
  dfSummary(round.digits = 2,
          na.col = FALSE,
          varnumbers = FALSE,
          plain.ascii = TRUE)

# A * Variables quantitatives: ---------------------------------
# les statistiques descriptives univariées : la distribution

# age
real_train_users %>%
  ggplot(aes(x=age)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# signup_flow
real_train_users %>%
  ggplot(aes(x=signup_flow)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# les statistiques descriptives bivariées : corrélations entre des variables (Une matrice de corrélation) : 

# boite a moustache avec gender et age
real_train_users %>%
  ggplot(aes(x=gender, y=age)) +
  geom_boxplot(fill="#69b3a2", alpha=0.8)

# matrice de corrélation
mcor <- real_train_users %>%
  select(age, signup_flow) %>%
  cor(use = "complete.obs")
mcor

# la corrélation avec la variable à expliquer (facultatif : 0.5 pt en plus) : Test de Student // ANOVA

#student test par âge / gender
t.test(real_train_users$age ~ real_train_users$gender)

# p est grand (16%) donc le resultat du test n'est pas significatif, on ne peut pas rejeter l'hypothèse
# nulle d'égalité des moyennes des 2 groupes.


# B * Variables qualitatives: ---------------------------------
# les statistiques descriptives univariées : la distribution

dotchart(sort(table(real_train_users$first_browser)))

# les corrélations entre des variables (Une matrice de corrélation)
tab_genre <- table(real_train_users$country_destination, real_train_users$gender)
tab_genre

mosaicplot(tab_genre)

# la corrélation avec la variable à expliquer (facultatif : 0.5 pt en plus) : Test d’indépendance

# Quel pays est le plus apprécié (destination) entre les hommes et les femmes ?
chisq.test(tab_genre)
chisq.residuals(tab_genre)
#L’interprétation des résidus est la suivante :
# si la valeur du résidu pour une case est inférieure à -2, alors il y a une sous-représentation de cette case dans le tableau : les effectifs sont significativement plus faibles que ceux attendus sous l’hypothèse d’indépendance
# à l’inverse, si le résidu est supérieur à 2, il y a sur-représentatation de cette case
# si le résidu est compris entre -2 et 2, il n’y a pas d’écart à l’indépendance significatif

saveRDS(real_train_users, "R_data/train_users.RDS")