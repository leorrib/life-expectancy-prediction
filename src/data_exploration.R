
de.find_values <- function (dataset, value) {
  for (i in 1:ncol(dataset)) {
    ifelse(is.na(value), 
           S <- sum(is.na(dataset[,i])), S <- sum(dataset[,i] == value))
    msg <- paste('The column', colnames(dataset)[i], 
                 'has', S, 'values equal to', value, sep = ' ')
    if (S != 0) {
      print(msg) 
    }
  }
}

de.get_corr_coef_predictor_vars <- function(df, var_target, cutoff) {
  for (j in 1:ncol(df)) {
    for (i in j:nrow(df)) {
      if (abs(df[i, j]) > cutoff & 
          i != j &
          colnames(df)[j] != var_target &
          rownames(df)[i] != var_target) {
        print(paste('Corr coef between ', 
                    colnames(df)[j],
                    ' and ',
                    rownames(df)[i],
                    ': ',
                    df[i, j],
                    sep = ''))
      }
    }
  } 
}

de.get_corr_coef_w_target_var <- function(df, var_target, cutoff) {
  for (j in 1:ncol(df)) {
    for (i in j:nrow(df)) {
      if (abs(df[i, j]) > cutoff & 
          i != j &
          (colnames(df)[j] == var_target | rownames(df)[i] == var_target)) {
        print(paste('Corr coef between ', 
                    colnames(df)[j],
                    ' and ',
                    rownames(df)[i],
                    ': ',
                    df[i, j],
                    sep = ''))
      }
    }
  } 
}

de.build_corr_coef_list <- function (df, metodo) {
  cors <- lapply(metodo, function(x) {
      cor(raw_data[,-c(1, 3, 21)], method = x)
    }
  )
  return(cors)
}

de.get_random_row_indexes <- function (df, percent) {
  index <- 1:nrow(df)
  trainFrame <- sample(index, trunc(length(index)*(percent/100)))
  return(trainFrame)
}