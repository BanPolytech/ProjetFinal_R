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

## 4- Description statistique sur les indicateurs créés--------

train_dataset %>%
  select(-c(dureeMax,id)) %>%
  dfSummary()

# A * Variables quantitatives: ---------------------------------
# les statistiques descriptives univariées : la distribution
train_dataset %>%
  ggplot(aes(x=NbActions)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

train_dataset %>%
  ggplot(aes(x=NbActions_ActionsTypes)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

train_dataset %>%
  ggplot(aes(x=NbActions_ActionsTypes_ActionDetails)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

train_dataset %>%
  ggplot(aes(x=NbDevices)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

train_dataset %>%
  ggplot(aes(x=dureeTotal)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

train_dataset %>%
  ggplot(aes(x=dureeMax)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

train_dataset %>%
  ggplot(aes(x=dureeMedian)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# les statistiques descriptives bivariées : corrélations entre des variables (Une matrice de corrélation) :
# boite a moustache avec gender et age
train_dataset %>%
  ggplot(aes(x=gender, y=age)) +
  geom_boxplot(fill="#69b3a2", alpha=0.8)

# matrice de corrélation
mcor2 <- train_dataset %>%
  select(age, signup_flow, NbActions, NbActions_ActionsTypes, NbActions_ActionsTypes_ActionDetails, NbDevices, dureeTotal, dureeMedian, dureeMax) %>%
  cor(use = "complete.obs")
mcor2


##couleur par genre
train_dataset %>%
  select(gender, age, signup_flow, NbActions, NbActions_ActionsTypes, NbActions_ActionsTypes_ActionDetails, NbDevices, dureeTotal, dureeMedian, dureeMax) %>%
  ggpairs(columns = 2:10,ggplot2::aes(colour = gender))

##couleur par country_destination
train_dataset %>%
  select(country_destination, age, signup_flow, NbActions, NbActions_ActionsTypes, NbActions_ActionsTypes_ActionDetails, NbDevices, dureeTotal, dureeMedian, dureeMax) %>%
  ggpairs(columns = 2:10, ggplot2::aes(colour = country_destination))

# la corrélation avec la variable à expliquer (facultatif : 0.5 pt en plus) : Test de Student // ANOVA
# par âge / genre
t.test(train_dataset$age ~ train_dataset$gender)

# B * Variables qualitatives: ---------------------------------
# les statistiques descriptives univariées : la distribution
dotchart(sort(table(train_dataset$first_browser)))

# les corrélations entre des variables (Une matrice de corrélation)
tab_genre2 <- table(train_dataset$country_destination, train_dataset$gender)
tab_genre2

# corrélation entre les devices type joints entre les tables
tab_device <- table(train_dataset$first_device_type, train_dataset$mostFreqDeviceType)
tab_device

# corrélation entre l'age des utilisateur et les devices utilisé
train_dataset %>%
  ggplot(aes(x=mostFreqDeviceType, y=age, fill=mostFreqDeviceType)) +
  geom_boxplot(alpha = 0.8) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

# la corrélation avec la variable à expliquer (facultatif : 0.5 pt en plus) : Test d’indépendance
# Quel pays est le plus apprécié (destination) entre les hommes et les femmes ?
chisq.test(tab_genre2)
chisq.residuals(tab_genre2)

chisq.test(tab_device)
chisq.residuals(tab_device)

mosaicplot(tab_device, las = 4, shade = TRUE)
#L’interprétation des résidus est la suivante :
# si la valeur du résidu pour une case est inférieure à -2, alors il y a une sous-représentation de cette case dans le tableau :
# les effectifs sont significativement plus faibles que ceux attendus sous l’hypothèse d’indépendance
# à l’inverse, si le résidu est supérieur à 2, il y a sur-représentatation de cette case
# si le résidu est compris entre -2 et 2, il n’y a pas d’écart à l’indépendance significatif