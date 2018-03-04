#install.packages("dplyr")
#install.packages("sna")
#install.packages("matrixStats")

library('sna')
library('dplyr')
library('matrixStats')

out_file = 'out/blank.csv'
head_no = 0
no_iters = 10


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

# loading datasets
g <- process_table('data_geo.csv')
c <- process_table('data_components.csv')

c_all_cadasters <- merge_by_id(g, c)

out = g

g_norm <- norm(g)

# clip g based on ids in c
g <- g_norm[rownames(c), ]

# clipping test data
c_test = c[seq(1, nrow(c), 3), ]
g_test = g[seq(1, nrow(g), 3), ]

# clipping training data
c_train = c[ !(row.names(c) %in% row.names(c_test)), ]
g_train = g[ !(row.names(g) %in% row.names(g_test)), ]

# c <- as.data.frame(c)
# c$id <- rownames(c)

# assign 0 for cadasters without localities
# c_copy <- data.frame(id = rownames(g))
# c2 <- dplyr::left_join(c_copy, c, by = "id")
# rownames(c2) <- c2$id
# c2[is.na(c2)] <- 0
# c2$id <- NULL
# c <- as.matrix(c2)

# subsetting - testing purposes
if (head_no > 0) {
  c_train <- head(c_train, head_no)
  g_train <- head(g_train, head_no)
  c_test <- head(c_test, head_no)
  g_test <- head(g_test, head_no)
}

g_train <- norm(g_train)


write(
  c('slope', 'srtm', 'tpi', 'twi', 'coarse', 'stream', 'hdist'),
  file = out_file,
  ncolumns = 7,
  append = FALSE,
  sep = ', '
)



#ws = find_hd(c_train, g_train)
#ws = c(0.18,	0.9,	0.22,	0.06,	0.98,	0.1)

#man
#ws = c(0.22,	1,	0.23,	0,	0.9,	0)

#can
#ws = c(0, 0.64, 0.87, 0, 0.56, 0.85)
#ws = c(0.29, 0.19, 0.72, 0.16, 1, 0.94)
#ws = c(0.28, 0.21, 0.73, 0.1, 1, 0.95)
ws = c(0.25, 0.26, 0.73, 0.1, 1, 0.95)

ws = ws / sum(ws)

colnames(g_norm) <- c('n_slope', 'n_srtm', 'n_tpi', 'n_twi', 'n_coarse', 'n_stream')
names(ws) <- c('n_slope', 'n_srtm', 'n_tpi', 'n_twi', 'n_coarse', 'n_stream')
out = merge_by_id(out, g_norm)
out <- cbind(out, test = row.names(out) %in% row.names(g_test))
out <- cbind(out, component = row.names(out) %in% row.names(c))

occurences <- data.frame()

for (comp in colnames(c)) {
  no_occ = nrow(na.omit(c_train[c_train[, comp] != 0,]))
  occurences <- c(occurences, no_occ)
}

names(occurences) <-colnames(c)

for (comp in colnames(c)) {
  # center values for each component in training dataset
  cms = colMeans(g_norm[row.names(g_norm) %in% rownames(c_train[c_train[,comp] == 1,]), ])
  #cms = colMedians(g_norm[row.names(g_norm) %in% rownames(c_train[c_train[,comp] == 1,]), ])
  names(cms) <- colnames(g_norm)
  
  # add empty column
  #out <- add_column(out, '', '')
  
  # distances from central values
  d = 0 # unweighed
  dw = 0 # weights used
  
  for (cm in names(cms)) {
    out <- add_column(out, paste(comp, cm, sep=""), cms[cm])
    
    diff = abs(cms[cm] - out[cm])
    #d = d + 1/6 * diff
    #dw = dw + ws[cm] * diff
    
    # euclidian distance in multidimensional space
    d = sqrt(d ^ 2 + (1/6 * diff) ^ 2 ) 
    dw = sqrt(dw ^ 2 + (ws[cm] * diff) ^ 2 )
  }
  
  
  
  out <- add_column(out, paste(comp, '_dist', sep=""), d )
  #out <- add_column(out, paste(comp, '_dist_w', sep=""), dw)
  
  # space for predictions
  #out <- add_column(out, paste(comp, '_pred', sep=""), '')
  #out <- add_column(out, paste(comp, '_pred_w', sep=""), '')
  
  #out <- add_column(out, paste(comp, '_predicted', sep=""), '')
  #out <- add_column(out, paste(comp, '_confusion', sep=""), '')
  
  #out[(out[,comp] == 1) & (out_t[,paste(comp, '_predicted', sep="")] == 1),][,paste(comp, '_confusion', sep="")] <- '11'
  #out[(out[,comp] == 0) & (out_t[,paste(comp, '_predicted', sep="")] == 0),][,paste(comp, '_confusion', sep="")] <-'10'
  #out[(out[,comp] == 1) & (out_t[,paste(comp, '_predicted', sep="")] == 0),][,paste(comp, '_confusion', sep="")] <- '00'
  #out[(out[,comp] == 0) & (out_t[,paste(comp, '_predicted', sep="")] == 1),][,paste(comp, '_confusion', sep="")] <- '01'
}

for (comp in colnames(c)) {
  out <- add_column(out, comp, c_all_cadasters[,comp])
}

for (comp in colnames(c)) {
  #out <- add_column(out, paste(comp, '_pred_w', sep=""), '')
  out <- add_column(out, paste(comp, '_pred', sep=""), '')
}


out[, 'id'] = rownames(out)

# only testing data
out <- out[out[,'test'] == TRUE,]

# bind occurences
out <- bind_rows(data.frame(occurences), out)

# removing all columns used for own geographical variables 
out <- out[, -grep("srtm$", colnames(out))]
out <- out[, -grep("twi$", colnames(out))]
out <- out[, -grep("tpi$", colnames(out))]
out <- out[, -grep("stream$", colnames(out))]
out <- out[, -grep("coarse$", colnames(out))]
out <- out[, -grep("slope$", colnames(out))]
#write.csv(out, file="cadasters_distances.csv")

# order columns 
#out <- out[ , order(names(out))]
write.csv(out, file="out/test/median_euclid_t.csv")

#out_t <- out_t[, -grep("srtm$", colnames(out_t))]
#out_t <- out_t[, -grep("twi$", colnames(out_t))]
#out_t <- out_t[, -grep("tpi$", colnames(out_t))]
#out_t <- out_t[, -grep("stream$", colnames(out_t))]
#out_t <- out_t[, -grep("coarse$", colnames(out_t))]
#out_t <- out_t[, -grep("slope$", colnames(out_t))]

#write.csv(out_t, file="out_test.csv")

#colMeans(g[row.names(g) %in% rownames(c[c[,"X800"] == 1,]), ])

#test_table = merge(c_test, g_test, by=0, all=TRUE)

# testing data
# c <- matrix(c(1,0, 1,0, 1,1, 0,1),ncol=2,byrow=TRUE)
# g <- matrix(c(
#   6,2,3,
#   5,2,1,
#   5,5,4,
#   1,4,1
# ),ncol=3,byrow=TRUE)
# g <- matrix(c(1.1,1.1,1, 1,1,1, 0.8,1,0, 1,1,0),ncol=3,byrow=TRUE)
