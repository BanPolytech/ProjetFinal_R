# countMissingVals : Fait la somme des valeurs manquante pour un objet x
countMissingVals <- function(x) {
  sum(is.na(x))
}