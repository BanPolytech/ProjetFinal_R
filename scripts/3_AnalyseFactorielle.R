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

library("mnlogit")
library("nnet")
library("MASS")

# Step n°3: Analyse factorielle & Modalisations --------------------------

## Multinomial logistic regression
mymodel <- nnet::multinom(as.factor(country_destination) ~ . -id
                          -date_account_created - timestamp_first_active
                          -date_first_booking, data = head(real_train_users, 100))
summary(mymodel)
# Selection le meilleur modèle par stepwise regression
mymodel_trivial <- "~1" #On définit un modèle trivial réduit à la constante
mymodel_temp <- nnet::multinom(as.factor(country_destination) ~ 1, data = head(real_train_users, 100))
mymodel_stepwise <- MASS::stepAIC(mymodel_temp,
                                  scope = list(lower = mymodel_trivial, upper = mymodel),
                                  trace = TRUE, data = head(real_train_users, 100), direction = "both")
summary(mymodel_stepwise)