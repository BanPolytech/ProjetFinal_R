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
library(readr)
library(ggplot2)
library(Amelia)
library(outliers)
library(VIM)

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
# Pas de valeur aberrantes

# A * Variable aberrantes: ---------------------------------

## âge
# On considère comme aberrantes les valeurs pour âge  < 18 et âge > 90
real_train_users <- real_train_users %>%
  filter(age >= 18 & age < 90 | is.na(age))

#la variable age est tres disparate, nous allons classer les valeurs par tranche conformément aux tranches décrites
# dans le jeu de données age_gender_bkts

real_train_users <- real_train_users %>%
  mutate(age_tranche = cut_width(age,
                                 width = 5,
                                 boundary = 90,
                                 labels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89")))

table(real_train_users$age_tranche, useNA = "always")

# B * Variables rare: --------------------------------------

## first_browser
table(real_train_users$first_browser, useNA = 'always')
ggplot(real_train_users, aes(x=real_train_users$first_browser)) + geom_bar()

# Garder les modalités chrome, firefox, IE, Safari et mobile Safari et grouper 
# tout le reste dans une modalité “autre” qu’on va filtrer pendant la phase d’apprentissage du modèle.
# On testera le modèle ultérieurement sur cette modalité écartée.
browser_extrem <- names(which(table(real_train_users$first_browser) < 2500))
browser <- names(which(table(real_train_users$first_browser) >= 2500))

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

ggplot(real_train_users, aes(x=real_train_users$first_device_type)) + geom_bar()
ggplot(data = real_train_users,aes(x = first_device_type, y = count(first_device_type))) +  
  geom_boxplot(fill = "green",color = "blue",outlier.colour = "red") +
  xlab("Boxplot - first_device_type")
## language

language_extrem <- names(which(table(real_train_users$language) < 1000))
language_tokeep <- names(which(table(real_train_users$language) >= 1000))
real_train_users <- real_train_users %>%
  filter(language %in% language_tokeep | is.na(language))

ggplot(real_train_users, aes(x=real_train_users$language)) + geom_bar()
# il n'y a plus qu'une seule valeur possible, la colonne ne nous sert plus

##

ggplot(real_train_users, aes(x=real_train_users$affiliate_channel)) + geom_bar()

ggplot(data = real_train_users,aes(y = affiliate_channel)) +  
  geom_boxplot(fill = "green",color = "blue",outlier.colour = "red") +
  xlab("Boxplot - affiliate_channel")

# C * Variables manquantes: ---------------------------------

missingVals <- as.data.frame(sapply(real_train_users, FUN = "countMissingVals"))
colnames(missingVals) <- "Nombre de valeur manquante"
missingVals

missmap(real_train_users, main = "Valeurs manquantes contre celles observées")

## gender
# Distribuer le genre sur les NAs en fonction de la proportion de MALE et FEMALE
setDT(real_train_users)[, gender := sample_fill_na(real_train_users$gender)]

## age, first_affiliate_tracked, first_browser
# Plus proche voisin en clustering (KNN)
sample <- sample_n(real_train_users, 500)

table(sample$first_affiliate_tracked, useNA = "always")
table(sample$first_browser, useNA = "always")

missingVals2 <- as.data.frame(sapply(sample, FUN = "countMissingVals"))
colnames(missingVals2) <- "Nombre de valeur manquante"
missingVals2

res <- kNN(sample, variable = c("age"), k = 5)

res2 <- kNN(real_train_users, c("age_tranche","first_affiliate_tracked","first_browser"), k = 5)

table(res2$age_tranche)

missingVals3 <- as.data.frame(sapply(res2, FUN = "countMissingVals"))
colnames(missingVals3) <- "Nombre de valeur manquante"
missingVals3

table(res2$first_affiliate_tracked, useNA = "always")
table(res2$first_browser, useNA = "always")

table(real_train_users$country_destination)

#####  KNN dure 5min pour un sample de 500 lignes

### 2. Statistiques descriptives de données : "train_users": ---------------------------

# A * Variables quantitatives: ---------------------------------
library(summarytools)
library(knitr)

dfSummary(real_train_users$age, round.digits = 2)


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

train_dataset <- inner_join(train_users, user_sessions, by = "user_id")