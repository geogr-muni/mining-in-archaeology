#install.packages("dplyr")
#install.packages("sna")

library('sna')
library('dplyr')

out_file = 'out/blank.csv'
head_no = 0
no_iters = 10

table_dist <- function(a) {
  tab = as.matrix(dist(a))
  return (round(tab / max(tab), 6))
}

do_htable <- function(a, b) {
  h = array(dim = c(2, dim(a)[2], dim(a)[1]))
  
  h[1, , ] <- a
  h[2, , ] <- b
  h
}

# a for components
# b for geographical variables
find_hd <- function(a, b) {
  ad = table_dist(a)
  
  best_hd = 10000000000
  best_coeffs = c(rep(NA, times = 6))
  
  hd <- function(b) {
    bd = table_dist(b)
    as.double(hdist(do_htable(ad, bd), normalize=TRUE, mode='graph', g1=c(1), g2=(2)))
    #as.double(gcor(do_htable(ad, bd), mode='graph', g1=c(1), g2=(2)))
  }
  
  vs = c(1:6)
  
  # for (v in vs) {
  #   best_v = 99999999
  #   #print(v  )
  #   for (w in weights) {
  #     b2 = b
  #     b2[,v] = b2[,v] * w
  #     hd_value = hd(b2)
  #     #print(hd_value)
  #     #cat("\n")
  #     
  #     
  #     if (hd_value < best_v) {
  #       best_v = hd_value
  #       best_coeffs[v] = w
  #     }
  #   }
  # }
  
  
  processed = 0
  start_time = proc.time()
  
  # weights = 1:no_iters / no_iters
  # weights = c(0.2, 0.8)
  one = c(1)
  all = c(0:100/100)
  
  ws1 = c(23:25/100)
  ws2 = c(65:67/100)
  ws3 = c(60:67/100)
  ws4 = c(52:57/100)
  ws5 = c(68:72/100)
  ws6 = c(76:82/100)
  no_iters = length(ws1) * length(ws2) * length(ws3) * length(ws4) * length(ws5) * length(ws6)
  
  for (w1 in ws1) {
    for (w2 in ws2) {
      for (w3 in ws3) {
        for (w4 in ws4) {
          one_iter_time = ((proc.time() - start_time)['elapsed'] / processed)
          print((no_iters - processed) * one_iter_time / 60)
          
          for (w5 in ws5) {
            for (w6 in ws6) {
              # cat("\n")
              b2 = b
              b2[, 1] = b2[, 1] * w1
              b2[, 2] = b2[, 2] * w2
              b2[, 3] = b2[, 3] * w3
              b2[, 4] = b2[, 4] * w4
              b2[, 5] = b2[, 5] * w5
              b2[, 6] = b2[, 6] * w6
              
              hd_value = hd(b2)
              # print(hd_value)
              write(
                c(w1, w2, w3, w4, w5, w6, hd_value) ,
                file = out_file,
                ncolumns = 7,
                append = TRUE,
                sep = ', '
              )
              
              processed = processed + 1
              
              if (hd_value < best_hd) {
                best_hd = hd_value
                best_coeffs = c(w1, w2, w3, w4, w5, w6)
              }
            }
          }
        }
      }
    }
  }
  
  return(best_coeffs)
}

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
ws = c(0.23,	0.67,	0.6,	0.55,	0.69,	0.78)
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
  out <- add_column(out, comp, c_all_cadasters[,comp])
}

names(occurences) <-colnames(c)


for (comp in colnames(c)) {
  # mean values for each component in training dataset
  cms = colMeans(g_norm[row.names(g_norm) %in% rownames(c_train[c_train[,comp] == 1,]), ])
  
  dist = 0
  dist_w = 0
  
  for (cm in names(cms)) {
    out <- add_column(out, paste(comp, cm, sep=""), cms[cm])
    
    diff = abs(cms[cm] - out[cm])
    dist = dist + 1/6 * diff
    dist_w = dist_w + ws[cm] * diff
  }
  
  
  out <- add_column(out, paste(comp, '_dist', sep=""), dist )
  #out <- add_column(out, paste(comp, '_dist_w', sep=""), dist_w)
  
  # space for predictions
  #out <- add_column(out, paste(comp, '_pred', sep=""), '')
  
  #out <- add_column(out, paste(comp, '_predicted', sep=""), '')
  #out <- add_column(out, paste(comp, '_confusion', sep=""), '')
}

for (comp in colnames(c)) {
  #out <- add_column(out, paste(comp, '_pred_w', sep=""), '')
  out <- add_column(out, paste(comp, '_pred', sep=""), '')
}

out <- bind_rows(data.frame(occurences), out)
out[, 'id'] = rownames(out)

# only testing data
out <- out[out[,'test'] == TRUE,]

# bind occurences

out <- out[, -grep("srtm$", colnames(out))]
out <- out[, -grep("twi$", colnames(out))]
out <- out[, -grep("tpi$", colnames(out))]
out <- out[, -grep("stream$", colnames(out))]
out <- out[, -grep("coarse$", colnames(out))]
out <- out[, -grep("slope$", colnames(out))]
#write.csv(out, file="cadasters_distances.csv")

# order columns 
#out <- out[ , order(names(out))]
write.csv(out, file="out/test_2.csv")

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
