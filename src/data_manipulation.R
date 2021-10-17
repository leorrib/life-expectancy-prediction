dm.range_divide <- function (df, col, nlevels, extrapolation) {
  ranges <- seq(from = min(df[, col]) - extrapolation, 
                to = max(df[, col]) + extrapolation, 
                length.out = nlevels + 1)
  df[, paste(col, "_range", sep = "") ] <- cut(df[, col], breaks = ranges, 
                                               order_result = ordered)
  return(df)
}


dm.factorize_cols <- function (df, cols) {
  for (i in cols) {
    df[, i] <- as.factor(df[, i])
  }
  return(df)
}

dm.drop_cols <- function (df, cols) {
  df <- df[,-which(names(df) %in% cols)]
  return(df)
}

dm._normalize <- function (x) {
  return((x - min(x))/(max(x) - min(x)))
}

dm.normalize_cols <- function(df, cols) {
  for (col in cols) {
    df[, col] <- dm._normalize(df[, col])
  }
  return(df)
}

dm.zscale_cols <- function(df, cols) {
  for (col in cols) {
    df[, col] <- scale(df[, col])
  }
  return(df)
}

