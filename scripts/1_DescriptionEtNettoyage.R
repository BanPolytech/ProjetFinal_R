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
library(vim)

# Chargement des fonctions sources
source("function/prepareData.R")

# Step n°1: Initialisation d'un projet R -------------------------

## 1- La création d'un projet R avec les répertoires: inputs, outputs, scripts, r_data, functions
# --> script "makeFolders.R" à la racine du dossier de travail

## 2- Importation des données:----------------------------------

countries <- read_csv("inputs/countries.csv",
                      na = "",
                      col_types = cols(destination_km2 = col_number(), 
                                       destination_language = col_character(), 
                                       distance_km = col_number(), language_levenshtein_distance = col_number(), 
                                       lat_destination = col_number(), lng_destination = col_number()))

sessions <- read_csv("inputs/sessions.csv",
                     na = "",
                     col_types = cols(secs_elapsed = col_number()))

age_gender_bkts <- read_csv("inputs/age_gender_bkts.csv",
                            na = "",
                            col_types = cols(population_in_thousands = col_number(), 
                                             year = col_number()))
#Strictement, une année n'est pas une date donc on garde le format numérique

train_users <- read_csv("inputs/train_users.csv",
                        na = c("-unknown-", "", ""),
                        col_types = cols(age = col_number(), 
                                         date_account_created = col_date(format = "%Y-%m-%d"), 
                                         date_first_booking = col_date(format = "%Y-%m-%d"), 
                                         signup_flow = col_number(), timestamp_first_active = col_number()))
#Les variables comportant la valeur "-unknown-" sont transformées en NA (notamment dans la colonne age)

test_users <- read_csv("inputs/test_users.csv",
                       na = "",
                       col_types = cols(age = col_number(), 
                                        date_account_created = col_date(format = "%Y-%m-%d"), 
                                        date_first_booking = col_date(format = "%Y-%m-%d"), 
                                        signup_flow = col_number()))

# Step n°2: Préparation des données --------------------------

## 1- Descrition statistique et Nettoyage de données:--------

### 1. Description de la qualité des données "train_users": ---------------------------

# A * Variable manquantes et rare: ---------------------------------

# Pour détecter les valeurs manquantes et rares il suffit d'utiliser la fonction summary() 
# celle-ci permet de compter le nombre des valeurs manquantes


real_train_users <- train_users %>% filter(country_destination != 'NDF')

# Pour les variables numerics et dates 
summary(train_users)

missingVals <- as.data.frame(sapply(train_users, FUN = "countMissingVals"))
colnames(missingVals) <- "Nombre de valeur manquante"
missingVals

missmap(train_users, main = "Valeurs manquantes contre celles observées")

# Résulats : 
# Nous avons 99152 (la majorité) valeurs manquantes dans la variable date_first_booking
# ce nombre de valeur manquantes me parrait acceptable et comprhénsible du fait que ce n'est pas
# tous ceux qui consulte le site résevent. 
# Nous avons aussi 70172 de valeurs manquantes dans la variable age. 
# 6216 NA dans first_affiliate_tracked

# B* LA détection des valeurs abberantes et extremes : --------------

# pour détecter les valeurs abberantes pour les variables catégorielles nous 
# avons besoin de la fonction levels() ou la fonction table() qui nous 
# permettra d'avoir le nombre de fois de chaque donnée qualitative et de savoir
# s'il y a des valeurs abberantes qualitatives ou pas (exemple si il y a des 
# des erreurs de saisie ou pas(ex Google et Googl). 

table(train_users$gender)
table(train_users$signup_method)
table(train_users$language)
table(train_users$affiliate_channel)
table(train_users$affiliate_provider)
table(train_users$first_affiliate_tracked) 
table(train_users$signup_app)
table(train_users$first_device_type)
table(train_users$first_browser)
table(train_users$country_destination)




# pour les variables numerics et dates :

summary(train_users)

pinf = 0.025 # fixe le percentile de limite inférieure
psup = 0.975# fixe le percentile de limite supérieure
k = 3

binf <- median(train_users$age, na.rm = TRUE) - k * mad(train_users$age, na.rm = TRUE) # calcule la borne inf de l'intervalle
binf

bsup <- median(train_users$age, na.rm = TRUE) + k * mad (train_users$age, na.rm = TRUE) # calcule la borne sup de l'intervalle
bsup

outlier_idx <- which(train_users$age < binf | train_users$age > bsup)
outlier_idx

table(train_users$age)

grubbs.test(train_users$age)

# Nous remarqueons rapidement que pour la variable age il existe des variables extremes
# telles que 2014 et 1
# Nous remarquons que pour signup_flow il existe une valeur extreme 25. 
# Cependant summary ne permet pas de ressortir toutes la variables extremes mais juste montrer 
# qu'il existe des valeurs extremes. 
# pour cette raison nous allons utiliser la méthode de visualisation pour bien repérer les 
# valeurs extremes: 

# visualisation pour la variable age : 
ggplot(data = train_users,aes(y = age)) +  
  geom_boxplot(fill = "green",color = "blue",outlier.colour = "red") +
  xlab("Boxplot - age") +
  theme(
    axis.title.y = element_blank()
    ,axis.text.x = element_blank()
    ,axis.ticks.x = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_line(color = "lightgray"))

# Notre boxplot nous montre les interquantilles et l'exsistence de valeurs aberrantes
# Pour sortir ces valeurs on utilise abboxplot$out 

abboxplot <- boxplot(train_users$age,range=1,5)
abboxplot$out

# la visualistaion de la variable age montre beaucoup de valeurs extremes et
# abberantes.ces valeurs contiennents même des ages qui sont censé etre toloérer
# tels que : 58 et 59 donc je ne sais pas si je dois les enlever ou pas? 

nb_browser <- train_users %>% summarise_all()
nb_browser


# Visualisation pour la variable signup_flow: 
ggplot(data = train_users,aes(y = signup_flow)) +  
  geom_boxplot(fill = "green",color = "blue",outlier.colour = "red") +
  xlab("Boxplot - signup flow") +
  theme(
    axis.title.y = element_blank()
    ,axis.text.x = element_blank()
    ,axis.ticks.x = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_line(color = "lightgray"))
# Aprés visualistaion de la variable signup_flow nous allons dire qu'il n y a pas des valeurs 
# abberantes car c'est tout à fait possible qu'un utilisateur entend parler de airbnb dans 25 sites


# Retraitement des données manquantes/ abberantes / extremes:----------

# Retraitement des valeurs manquantes: --------------------------

# Pour rappel : nous avons des données manquantes sur les variables Age, date_first_booking
# et first_affiliate_tracked. 

# Premierement remplacer les vides dans first_affiliate_tracked par NA:

train_users$first_affiliate_tracked [train_users$first_affiliate_tracked ==""] <- NA

# Aprés consultattion de la variable on remarque que les valeurs manquantes de celle-ci 
# souvent il passe directement au site donc il n y a pas une vraie compagne qui les a tracké
# c'est pour ça il convient de remplacer NA par unknown c'est la modalité la plus proche 

train_users$first_affiliate_tracked [train_users$first_affiliate_tracked == "NA"] <- "-untracked-"

## 2- Traitement du dataset sessions:--------

### 1. Agréger le dataset sessions à la maille d'utilisateurs

groupe <- sessions %>%
  select(user_id, action) %>%
  group_by(user_id) %>%
  summarise(UNIQUEA = sum(action))

res <- aggregate(sessions$user_id, by = list(sessions$action), FUN = unique)
print(res)

table(sessions$action)

unique(sessions$user_id)
