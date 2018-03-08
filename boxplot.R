#install.packages("dplyr")
#install.packages("sna")
#install.packages("matrixStats")

library('sna')
library('dplyr')
library('matrixStats')

process_table <- function(path) {
  f <- read.csv(path,
                stringsAsFactors = FALSE,
                header = TRUE,
                sep = '\t')
  rownames(f) <- f$id
  f[1] <- NULL
  m <- as.matrix(f)
  return (m[order(as.numeric(row.names(m))), ])
}

add_column <- function(df, colName, values) {
  df <- cbind(df, values )
  colnames(df)[length(colnames(df))] <- colName
  return(df)
}

merge_by_id <- function(a, b){
  c = merge(a, b, by=0, all=TRUE)
  row.names(c) <- as.numeric(c[, 'Row.names'])
  c[, 'Row.names'] <- NULL
  c <- c[order(as.numeric(row.names(c))), ]
  return (c)
}

norm <- function(df) {
  # normalisation
  return(scale(df)) # zscore
  
  #r01 <- function(x) {
  #  (x - min(x)) / (max(x) - min(x))
  #}
  #return(apply(df, 2, r01)) # minmax
}

r01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


colQ1s <- function(data) sapply(data, quantile, probs=c(0.25), na.rm = TRUE)
colQ3s <- function(data) sapply(data, quantile, probs=c(0.75), na.rm = TRUE)

# loading datasets
g <- process_table('data_geo.csv')
c <- process_table('data_components.csv')

c_all_cadasters <- merge_by_id(g, c)

out = g

# clipping test data
c_test = c[seq(1, nrow(c), 3), ]
g_test = g[seq(1, nrow(g), 3), ]

# clipping training data
c_train = c[ !(row.names(c) %in% row.names(c_test)), ]
g_train = g[ !(row.names(g) %in% row.names(g_test)), ]
g_train <- norm(g_train)

colnames(g) <- c('slope', 'srtm', 'tpi', 'twi', 'coarse', 'stream')


out <- cbind(out, component = row.names(out) %in% row.names(c))
out <- out[,1, drop=FALSE]

for (comp in colnames(c)) {
  # center values for each component in training dataset
  print(comp)
  comp_data = g[row.names(g) %in% rownames(c_train[c_train[,comp] == 1,]), ]
  
  c_means = colMedians(comp_data)
  names(c_means) <- paste('median_', colnames(g), sep="")
  
  c_maxs = colMaxs(comp_data)
  names(c_maxs) <- paste('max_', colnames(g), sep="")
  
  c_mins = colMins(comp_data)
  names(c_mins) <- paste('min_', colnames(g), sep="")
  
  c_q1s = apply(comp_data[, ], 2, quantile, probs=c(0.25), na.rm = TRUE)
  names(c_q1s) <- paste('q1_', colnames(g), sep="")
  
  c_q3s = apply(comp_data[, ], 2, quantile, probs=c(0.75), na.rm = TRUE)
  names(c_q3s) <- paste('q3_', colnames(g), sep="")
  
  
  for (c_mean in names(c_means)) {
    out <- add_column(out, paste(comp, c_mean, sep="_"), c_means[c_mean])
  }
  for (c_max in names(c_maxs)) {
    #out <- add_column(out, paste(comp, c_max, sep="_"), c_maxs[c_max])
  }
  for (c_min in names(c_mins)) {
    #out <- add_column(out, paste(comp, c_min, sep="_"), c_mins[c_min])
  }
  for (c_q1 in names(c_q1s)) {
    out <- add_column(out, paste(comp, c_q1, sep="_"), c_q1s[c_q1])
  }
  for (c_q3 in names(c_q3s)) {
    out <- add_column(out, paste(comp, c_q3, sep="_"), c_q3s[c_q3])
  }
}

write.csv(t(out)[,1], file="out/boxplots2.csv")
