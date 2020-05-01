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

## 3- Jointures--------
# jointure entre l’agrégation des sessions (user_sessions) et le dataset d’entrainement (train_users)
train_dataset <- inner_join(real_train_users, user_sessions, by = c("id" = "user_id"))

# jointure avec le dataset countries
train_dataset <- left_join(train_dataset, countries, by = "country_destination")

# jointure avec le dataset age_gender_bkts
## Nous allons classer les valeurs par tranche conformément aux tranches décrites
## dans le jeu de données age_gender_bkts
train_dataset <- train_dataset %>%
  mutate(age_tranche = cut_width(age,
                                 width = 5,
                                 boundary = 90,
                                 labels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89")))

age_gender_bkts$gender <- toupper(age_gender_bkts$gender)
train_dataset <- left_join(train_dataset, age_gender_bkts, by = c("age_tranche" = "age_bucket", "gender" = "gender", "country_destination" = "country_destination"))

saveRDS(train_dataset, "R_data/train_dataset.RDS")