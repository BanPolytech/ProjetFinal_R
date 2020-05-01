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

# On fixe le wd pour le sourcage
wd <- getwd()
if(grepl("/scripts", wd, fixed = TRUE)) {
  setwd("..")
}

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
library(zoo)
library(janitor)

# Chargement des fonctions sources
source("function/prepareData.R")

# Step n°2: Préparation des données --------------------------

## 1- Descrition statistique et Nettoyage de données:--------

### 1. Description de la qualité des données "train_users": ---------------------------

# On enleve les clients qui n'ont pas de prediction de pays
train_users_sansNDF <- train_users %>% filter(country_destination != 'NDF')

# pour détecter les valeurs abberantes pour les variables catégorielles nous 
# avons besoin de la fonction levels() ou la fonction table() qui nous 
# permettra d'avoir le nombre de fois de chaque donnée qualitative et de savoir
# s'il y a des valeurs abberantes qualitatives ou pas (exemple si il y a des 
# des erreurs de saisie ou pas(ex Google et Googl). 


# On observe les variables dans leur ensemble pour detecter les valeurs aberrantes, extremes ou manquantes
#dfSummary(train_users_sansNDF, round.digits = 2)

## les variables catégorielles
cat_var <- names(train_users_sansNDF[, sapply(train_users_sansNDF, class) == 'character'])
cat_var <- cat_var[-1]
observe_table(train_users_sansNDF, cat_var)

## les variables quantitatives
#ggplot(data = train_users_sansNDF,aes(y = age)) +
#  geom_boxplot(fill = "green",color = "blue",outlier.colour = "red") +
#  xlab("Boxplot - age")
# Présence évidente de variable aberrantes/extremes

#ggplot(data = train_users_sansNDF,aes(y = signup_flow)) +  
#  geom_boxplot(fill = "green",color = "blue",outlier.colour = "red") +
#  xlab("Boxplot - signup flow")
# Aprés visualistaion de la variable signup_flow nous allons dire qu'il n y a pas des valeurs 
# abberantes car c'est tout à fait possible qu'un utilisateur entende parler de airbnb dans 25 sites

## les variables date
#ggplot(train_users_sansNDF, aes(x=date_account_created)) +
#  geom_histogram()

#ggplot(train_users_sansNDF, aes(x=timestamp_first_active)) +
#  geom_histogram()

#ggplot(train_users_sansNDF, aes(x=date_first_booking)) +
#  geom_histogram()
# Pas de valeur aberrante

# A * Variable aberrantes: ---------------------------------
# On garde train_users_sansNDF
real_train_users <- train_users_sansNDF

## âge
# On considère comme aberrantes les valeurs pour âge  < 18 et âge >= 90
real_train_users <- real_train_users %>%
  filter(age >= 18 & age < 90 | is.na(age))

# B * Variables rare: --------------------------------------

## first_browser
#table(real_train_users$first_browser, useNA = 'always')
#ggplot(real_train_users, aes(x=real_train_users$first_browser)) + geom_bar()

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
#table(real_train_users$affiliate_provider)
#ggplot(real_train_users, aes(x=real_train_users$affiliate_provider)) + geom_bar()
# On garde les modalités “direct”, “google” et on regroupe tout le reste dans la modalité “others”
providers <- names(which(table(real_train_users$affiliate_provider) < 2000))
provider_tokeep <- names(which(table(real_train_users$affiliate_provider) >= 2000))

real_train_users <- real_train_users %>% 
  mutate(affiliate_provider = if_else(affiliate_provider %in% providers, "other", affiliate_provider))

## gender
#summary(real_train_users)
#table(real_train_users$gender, useNA = 'always')
#ggplot(real_train_users, aes(x=real_train_users$gender)) + geom_bar()
# Suppression des lignes “OTHER” (0,14%)
real_train_users <- real_train_users %>% filter(gender != "OTHER" | is.na(gender))

## first_device_type
#table(real_train_users$first_device_type)
#ggplot(real_train_users, aes(x=first_device_type)) + geom_bar()
## Regroupement de "Android Phone", "Android Tablet" et "Desktop (Other)" en "Other/Unknown"
real_train_users <- real_train_users %>% 
  mutate(first_device_type = if_else(first_device_type %in% c("Android Phone", "Android Tablet", "Desktop (Other)"), "Other/Unknown", first_device_type))

## language
language_extrem <- names(which(table(real_train_users$language) < 1000))
language_tokeep <- names(which(table(real_train_users$language) >= 1000))
real_train_users <- real_train_users %>%
  filter(language %in% language_tokeep | is.na(language))

#ggplot(real_train_users, aes(x=language)) + geom_bar()
# il n'y a plus qu'une seule valeur possible, la colonne ne nous sert plus

## affiliate_channel
ac_extr <- names(which(table(real_train_users$affiliate_channel) < 1000))
ac_tokeep <- names(which(table(real_train_users$affiliate_channel) >= 1000))
real_train_users <- real_train_users %>% 
  mutate(affiliate_channel = if_else(affiliate_channel %in% ac_extr, "other", affiliate_channel))

#ggplot(real_train_users, aes(x=real_train_users$affiliate_channel)) + geom_bar()

## first_affiliate_tracked
fat_extr <- names(which(table(real_train_users$first_affiliate_tracked) < 1000))
fat_tokeep <- names(which(table(real_train_users$first_affiliate_tracked) >= 1000))
real_train_users <- real_train_users %>% 
  mutate(first_affiliate_tracked = if_else(first_affiliate_tracked %in% fat_extr, "tracked-other", first_affiliate_tracked))



# C * Variables manquantes: ---------------------------------
real_train_users_NA <- real_train_users
missingVals <- as.data.frame(sapply(real_train_users, FUN = "countMissingVals"))
colnames(missingVals) <- "Nombre de valeur manquante"
# missingVals

## missmap(real_train_users, main = "Valeurs manquantes contre celles observées")

## gender
# Distribuer le genre sur les NAs en fonction de la proportion de MALE et FEMALE
setDT(real_train_users)[, gender := sample_fill_na(real_train_users$gender)]

## age
# par la mediane

real_train_users$ageMedian <- na.aggregate(real_train_users$age, FUN = median)

# Plus proche voisin en clustering (KNN)
#res <- kNN(sample,
#           variable = c("age_tranche", "age"),
#           dist_var = c("age_tranche", "gender", "signup_method", "affiliate_provider", "affiliate_channel", "first_device_type", "first_browser", "signup_app", "first_affiliate_tracked", "country_destination"),
#           k = 23)

#res2 <- kNN(real_train_users,
#            variable = c("age_tranche","first_affiliate_tracked","first_browser"),
#            dist_var = c("age_tranche", "gender", "signup_method", "affiliate_provider", "affiliate_channel", "first_device_type", "first_browser", "signup_app", "first_affiliate_tracked", "country_destination"),
#            k = 5)
#####  KNN dure 5min pour un sample de 500 lignes
##### KNN pas forcément adapté pour cette volumétrie

# par locf
real_train_users$age <- na.locf(real_train_users$age)
## on retiendra cette methode d'imputation car elle ne brise pas la distribution

# first_affiliate_tracked
#table(real_train_users$first_affiliate_tracked, useNA = "always")
#ggplot(real_train_users, aes(x=real_train_users$first_affiliate_tracked)) + geom_bar()

real_train_users$first_affiliate_tracked <- na.locf(real_train_users$first_affiliate_tracked)

# first_browser
#table(real_train_users$first_browser, useNA = "always")
#ggplot(real_train_users, aes(x=real_train_users$first_browser)) + geom_bar()

real_train_users$first_browser <- na.locf(real_train_users$first_browser)

