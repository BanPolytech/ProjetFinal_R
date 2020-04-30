# countMissingVals : Fait la somme des valeurs manquante pour un objet x
countMissingVals <- function(x) {
  sum(is.na(x))
}

# sample_fill_na
sample_fill_na = function(x) {
  x_na = is.na(x)
  x[x_na] = sample(x[!x_na], size = sum(x_na), replace = TRUE)
  return(x)
}

# table on columns in df
observe_table = function(df, colnames) {
  for (name in colnames) {
    print(table(df[name], useNA = 'always'))
  }
}