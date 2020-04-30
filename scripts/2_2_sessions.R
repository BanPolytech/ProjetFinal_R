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
##prétraitement de la colonne device_type pour match les valeurs de la colonne "first_device" du dataset train_users
sessions <- sessions %>%
  mutate(device_type = recode(device_type, `iPad Tablet` = "iPad"))

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