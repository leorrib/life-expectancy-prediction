dv.plot_multiple_bars <- function (df, set_of_cols, col_target) {  
  lapply(set_of_cols, function (x) {
      ggplot(df, aes(x)) +
        geom_bar(color = 'cyan3', fill = 'cyan3') +
        theme(text = element_text(size=14)) +
        facet_grid(paste(x, '~', col_target)) +
        xlab('') + 
        ylab('number of clients') +
        ggtitle(paste("Health insurance cost per", x))
    }
  )
}

dv.plot_multiple_bars_II <- function (
  df, set_of_cols, col_target, title, xlabel, ylabel
  ) {  
  lapply(set_of_cols, function (x) {
    ggplot(df, aes(col_target)) +
      geom_bar(color = 'cyan3', fill = 'cyan3') +
      theme(text = element_text(size=14)) +
      facet_grid(paste(x, '~', col_target)) +
      xlab(xlabel) + 
      ylab(ylabel) +
      ggtitle(paste(title, x, sep = ' '))
    }
  )
}

dv.plot_corr_coeffs <- function (x, labs, title) {
  plot( 
    levelplot(x,
              main = paste(title, labs),
              scales = list(x = list(rot = 90), cex = 0.75),
              xlab = '',
              ylab = '')
  )
}

dv.plot_scatter <- function (X, label) {
  ggplot(raw_data, aes_string(x = X, y = 'Life.expectancy')) +
    geom_point(aes_string(colour = 'Life.expectancy'), alpha = 0.1) +
    scale_colour_gradient(low = 'green', high = 'blue') +
    geom_smooth(method = 'loess') +
    ggtitle(label) +
    theme(text = element_text(size = 12))
}

dv.get_auc <- function (predition) {
  auc_ROCR <- performance(predition, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  return(auc_ROCR)
}