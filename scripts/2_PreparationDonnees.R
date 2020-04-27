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
#               Imed Eddine BOUSSOUF, Khaoula ELMOUTAMID,
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

# Charger les packages:---------------------------------------------
library(dplyr)
library(ggplot2)
library(Amelia)
library(outliers)
library(VIM)
library(summarytools)
library(questionr)
library(ellipse)
library(RColorBrewer)
library(GGally)

# Chargement des fonctions sources
source("function/prepareData.R")

# Step n°2: Préparation des données --------------------------

## 1- Descrition statistique et Nettoyage de données:--------

### 1. Description de la qualité des données "train_users": ---------------------------

# On enleve les clients qui n'ont pas de prediction de pays
real_train_users <- train_users %>% filter(country_destination != 'NDF')

# pour détecter les valeurs abberantes pour les variables catégorielles nous 
# avons besoin de la fonction levels() ou la fonction table() qui nous 
# permettra d'avoir le nombre de fois de chaque donnée qualitative et de savoir
# s'il y a des valeurs abberantes qualitatives ou pas (exemple si il y a des 
# des erreurs de saisie ou pas(ex Google et Googl). 


# On observe les variables dans leur ensemble pour detecter les valeurs aberrantes, extremes ou manquantes
dfSummary(real_train_users, round.digits = 2)

## les variables catégorielles
table(real_train_users$gender, useNA = 'always')
table(real_train_users$signup_method, useNA = 'always')
table(real_train_users$language, useNA = 'always')
table(real_train_users$affiliate_channel, useNA = 'always')
table(real_train_users$affiliate_provider,useNA = 'always')
table(real_train_users$first_affiliate_tracked, useNA = 'always') 
table(real_train_users$signup_app, useNA = 'always')
table(real_train_users$first_device_type, useNA = 'always')
table(real_train_users$first_browser, useNA = 'always')
table(real_train_users$country_destination, useNA = 'always')

## les variables quantitatives
ggplot(data = real_train_users,aes(y = age)) +
  geom_boxplot(fill = "green",color = "blue",outlier.colour = "red") +
  xlab("Boxplot - age")
# Présence évidente de variable aberrantes/extremes

ggplot(data = train_users,aes(y = signup_flow)) +  
  geom_boxplot(fill = "green",color = "blue",outlier.colour = "red") +
  xlab("Boxplot - signup flow")
# Aprés visualistaion de la variable signup_flow nous allons dire qu'il n y a pas des valeurs 
# abberantes car c'est tout à fait possible qu'un utilisateur entende parler de airbnb dans 25 sites

## les variables date
ggplot(real_train_users, aes(x=date_account_created)) +
  geom_histogram()

ggplot(real_train_users, aes(x=timestamp_first_active)) +
  geom_histogram()

ggplot(real_train_users, aes(x=date_first_booking)) +
  geom_histogram()
# Pas de valeur aberrante

# A * Variable aberrantes: ---------------------------------

## âge
# On considère comme aberrantes les valeurs pour âge  < 18 et âge >= 90
real_train_users <- real_train_users %>%
  filter(age >= 18 & age < 90 | is.na(age))

# B * Variables rare: --------------------------------------

## first_browser
table(real_train_users$first_browser, useNA = 'always')
ggplot(real_train_users, aes(x=real_train_users$first_browser)) + geom_bar()

# Garder les modalités chrome, firefox, IE, Safari et mobile Safari et grouper 
# tout le reste dans une modalité “autre” qu’on va filtrer pendant la phase d’apprentissage du modèle.
# On testera le modèle ultérieurement sur cette modalité écartée.
browser_extrem <- names(which(table(real_train_users$first_browser) < 1000))
browser <- names(which(table(real_train_users$first_browser) >= 1000))

# on garde les valeurs extremes de cote
real_train_users_extremes <- real_train_users %>%
  filter(first_browser %in% browser_extrem)

real_train_users <- real_train_users %>%
  filter(first_browser %in% browser | is.na(first_browser))

## affiliate_provider
table(real_train_users$affiliate_provider)
ggplot(real_train_users, aes(x=real_train_users$affiliate_provider)) + geom_bar()
# On garde les modalités “direct”, “google” et on regroupe tout le reste dans la modalité “others”
providers <- names(which(table(real_train_users$affiliate_provider) < 2000))
provider_tokeep <- names(which(table(real_train_users$affiliate_provider) >= 2000))

real_train_users <- real_train_users %>% 
  mutate(affiliate_provider = if_else(affiliate_provider %in% providers, "other", affiliate_provider))

## gender
summary(real_train_users)
table(real_train_users$gender, useNA = 'always')
ggplot(real_train_users, aes(x=real_train_users$gender)) + geom_bar()
# Suppression des lignes “OTHER” (0,14%)
real_train_users <- real_train_users %>% filter(gender != "OTHER" | is.na(gender))

## first_device_type
table(real_train_users$first_device_type)
ggplot(real_train_users, aes(x=real_train_users$first_device_type)) + geom_bar()
## regroupement possible entre les différentes type de device

## language
language_extrem <- names(which(table(real_train_users$language) < 1000))
language_tokeep <- names(which(table(real_train_users$language) >= 1000))
real_train_users <- real_train_users %>%
  filter(language %in% language_tokeep | is.na(language))

ggplot(real_train_users, aes(x=real_train_users$language)) + geom_bar()
# il n'y a plus qu'une seule valeur possible, la colonne ne nous sert plus

## affiliate_channel
ac_extr <- names(which(table(real_train_users$affiliate_channel) < 1000))
ac_tokeep <- names(which(table(real_train_users$affiliate_channel) >= 1000))
real_train_users <- real_train_users %>% 
  mutate(affiliate_channel = if_else(affiliate_channel %in% ac_extr, "other", affiliate_channel))

ggplot(real_train_users, aes(x=real_train_users$affiliate_channel)) + geom_bar()

## first_affiliate_tracked
fat_extr <- names(which(table(real_train_users$first_affiliate_tracked) < 1000))
fat_tokeep <- names(which(table(real_train_users$first_affiliate_tracked) >= 1000))
real_train_users <- real_train_users %>% 
  mutate(first_affiliate_tracked = if_else(first_affiliate_tracked %in% fat_extr, "tracked-other", first_affiliate_tracked))



# C * Variables manquantes: ---------------------------------

missingVals <- as.data.frame(sapply(real_train_users, FUN = "countMissingVals"))
colnames(missingVals) <- "Nombre de valeur manquante"
missingVals

missmap(real_train_users, main = "Valeurs manquantes contre celles observées")

## gender
# Distribuer le genre sur les NAs en fonction de la proportion de MALE et FEMALE
setDT(real_train_users)[, gender := sample_fill_na(real_train_users$gender)]

## age
# par la mediane
library(zoo)
real_train_users$ageMedian <- na.aggregate(real_train_users$age, FUN = median)

# Plus proche voisin en clustering (KNN)
res <- kNN(sample,
           variable = c("age_tranche", "age"),
           dist_var = c("age_tranche", "gender", "signup_method", "affiliate_provider", "affiliate_channel", "first_device_type", "first_browser", "signup_app", "first_affiliate_tracked", "country_destination"),
           k = 23)

res2 <- kNN(real_train_users,
            variable = c("age_tranche","first_affiliate_tracked","first_browser"),
            dist_var = c("age_tranche", "gender", "signup_method", "affiliate_provider", "affiliate_channel", "first_device_type", "first_browser", "signup_app", "first_affiliate_tracked", "country_destination"),
            k = 5)
#####  KNN dure 5min pour un sample de 500 lignes
##### KNN pas forcément adapté pour cette volumétrie

# par locf
real_train_users$age <- na.locf(real_train_users$age)
## on retiendra cette methode d'imputation car elle ne brise pas la distribution

# first_affiliate_tracked
table(real_train_users$first_affiliate_tracked, useNA = "always")
ggplot(real_train_users, aes(x=real_train_users$first_affiliate_tracked)) + geom_bar()

real_train_users$first_affiliate_tracked <- na.locf(real_train_users$first_affiliate_tracked)

# first_browser
table(real_train_users$first_browser, useNA = "always")
ggplot(real_train_users, aes(x=real_train_users$first_browser)) + geom_bar()

real_train_users$first_browser <- na.locf(real_train_users$first_browser)

### 2. Statistiques descriptives de données : "train_users": ---------------------------
# stats basiques sur toutes les données, stats univariées sur chaque colonnes
dfSummary(real_train_users,
          round.digits = 2,
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


## 2- Traitement du dataset sessions:--------

### 1. Agréger le dataset sessions à la maille d'utilisateurs
user_sessions <- sessions %>%
  group_by(user_id) %>%
  summarise(
            # Le nombre d'action uniques
            NbActions = n_distinct(action),
            # Le nombre de combinaisons d'action + action type uniques
            NbActions_ActionsTypes = n_distinct(action, action_type),
            # Le nombre de combinaisons d'action + action_type + action_detail uniques
            NbActions_ActionsTypes_ActionDetails = n_distinct(action, action_type, action_detail),
            # Le nombre de device_type unique
            NbDevices = n_distinct(device_type),
            # La durée totale de toutes les actions
            dureeTotal = sum(secs_elapsed, na.rm = TRUE))
  

### 1(bis)

# L'action la plus fréquente
action_most_freq <- sessions %>%
  group_by(user_id) %>%
  count(action) %>%
  slice((which.max(n))) %>%
  mutate(mostFreqAction = action) %>%
  select(-c(n,action))

# La combinaison d'action + action type la plus fréquente
action_actionType_most_freq <- sessions %>%
  group_by(user_id) %>%
  count(action, action_type) %>%
  slice((which.max(n))) %>%
  mutate(mostFreqAction_ActionType = paste(action, action_type)) %>%
  select(-c(n,action,action_type))

# La combinaison d’ action + action type + action_detail la plus fréquente
action_actionType_actionDetail_most_freq <- sessions %>%
  group_by(user_id) %>%
  count(action, action_type, action_detail) %>%
  slice((which.max(n))) %>%
  mutate(mostFreqAction_ActionType_ActionDetail = paste(action, action_type, action_detail)) %>%
  select(-c(n,action,action_type,action_detail))

# Le device_type le plus fréquent
deviceType_most_freq <- sessions %>%
  group_by(user_id) %>%
  count(device_type) %>%
  slice((which.max(n))) %>%
  mutate(mostFreqDeviceType = device_type) %>%
  select(-c(n,device_type))

# Le maximum de la durée des sessions
session_max_length <- sessions %>%
  group_by(user_id) %>%
  summarise(dureeMax = max(secs_elapsed, na.rm = TRUE))

# La médiane de la durée des sessions
session_median_length <- sessions %>%
  group_by(user_id) %>%
  summarise(dureeMedian = median(secs_elapsed, na.rm = TRUE))

## user_sessions 
user_sessions <- inner_join(user_sessions, action_most_freq, by = "user_id")
user_sessions <- inner_join(user_sessions, action_actionType_most_freq, by = "user_id")
user_sessions <- inner_join(user_sessions, action_actionType_actionDetail_most_freq, by = "user_id")
user_sessions <- inner_join(user_sessions, deviceType_most_freq, by = "user_id")
user_sessions <- inner_join(user_sessions, session_max_length, by = "user_id")
user_sessions <- inner_join(user_sessions, session_median_length, by = "user_id")

saveRDS(user_sessions, "R_data/user_sessions.RDS")


## 3- Jointures--------
# jointure entre l’agrégation des sessions (user_sessions) et le dataset d’entrainement (train_users)
train_dataset <- inner_join(real_train_users, user_sessions, by = c("id" = "user_id"))

# jointure avec le dataset countries
train_dataset <- left_join(train_dataset, countries, by = "country_destination")

# jointure avec le dataset age_gender_bkts

## Nous allons classer les valeurs par tranche conformément aux tranches décrites
## dans le jeu de données age_gender_bkts
real_train_users <- real_train_users %>%
  mutate(age_tranche = cut_width(age,
                                 width = 5,
                                 boundary = 90,
                                 labels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89")))

age_gender_bkts$gender <- toupper(age_gender_bkts$gender)
train_dataset <- left_join(train_dataset, age_gender_bkts, by = c("age_tranche" = "age_bucket", "gender" = "gender", "country_destination" = "country_destination"))

saveRDS(train_dataset, "R_data/train_dataset.RDS")

## 4- Description statistique sur les indicateurs créés--------

summary(train_dataset)

train_dataset %>%
  select(-dureeMax) %>%
  dfSummary()

dfSummary(train_dataset,
          round.digits = 2,
          plain.ascii = FALSE)
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
# par âge / country destination
t.test(train_dataset$age ~ train_dataset$gender)

# B * Variables qualitatives: ---------------------------------
# les statistiques descriptives univariées : la distribution
dotchart(sort(table(real_train_users$first_browser)))

# les corrélations entre des variables (Une matrice de corrélation)
tab_genre2 <- table(train_dataset$country_destination, train_dataset$gender)
tab_genre2

mosaicplot(tab_genre2)

# la corrélation avec la variable à expliquer (facultatif : 0.5 pt en plus) : Test d’indépendance

# Quel pays est le plus apprécié (destination) entre les hommes et les femmes ?
chisq.test(tab_genre2)
chisq.residuals(tab_genre2)
#L’interprétation des résidus est la suivante :
# si la valeur du résidu pour une case est inférieure à -2, alors il y a une sous-représentation de cette case dans le tableau : les effectifs sont significativement plus faibles que ceux attendus sous l’hypothèse d’indépendance
# à l’inverse, si le résidu est supérieur à 2, il y a sur-représentatation de cette case
# si le résidu est compris entre -2 et 2, il n’y a pas d’écart à l’indépendance significatif