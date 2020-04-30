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
library(readr)

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
                     na = c("-unknown-",""),
                     col_types = cols(secs_elapsed = col_number()))
#Les variables comportant la valeur "-unknown-" sont transformées en NA

age_gender_bkts <- read_csv("inputs/age_gender_bkts.csv",
                            na = "",
                            col_types = cols(population_in_thousands = col_number(), 
                                             year = col_number()))
#Strictement, une année n'est pas une date donc on garde le format numérique

train_users <- read_csv("inputs/train_users.csv",
                        na = c("-unknown-", ""),
                        col_types = cols(age = col_number(), 
                                         date_account_created = col_date(format = "%Y-%m-%d"), 
                                         date_first_booking = col_date(format = "%Y-%m-%d"), 
                                         signup_flow = col_number(), timestamp_first_active = col_number()))
#Les variables comportant la valeur "-unknown-" sont transformées en NA (notamment dans la colonne age)

test_users <- read_csv("inputs/test_users.csv",
                       na = c("-unknown-", ""),
                       col_types = cols(age = col_number(), 
                                        date_account_created = col_date(format = "%Y-%m-%d"), 
                                        date_first_booking = col_date(format = "%Y-%m-%d"), 
                                        signup_flow = col_number(), timestamp_first_active = col_number()))
