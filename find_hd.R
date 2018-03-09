#install.packages("dplyr")
#install.packages("sna")
#install.packages("matrixStats")

library('sna')
library('dplyr')
library('matrixStats')

out_file = 'out/can/7.csv'
head_no = 0
no_iters = 10

table_dist <- function(a, method) {
  # method for constrution of distance tables
  #tab = as.matrix(dist(a) )
  tab = as.matrix(dist(a, method=method) )
  tab[is.na(tab)] <- 0
  return (round(tab / max(tab), 10))
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
  ad = table_dist(a, 'canberra')
  
  best_hd = 10000000000
  best_coeffs = c(rep(NA, times = 6))
  
  hd <- function(b) {
    bd = table_dist(b, 'euclidian')
    as.double(hdist(do_htable(ad, bd), normalize=TRUE, mode='graph', g1=c(1), g2=(2)))
    #as.double(gcor(do_htable(ad, bd), mode='graph', g1=c(1), g2=(2)))
  }

  processed = 0
  start_time = proc.time()

  # weights = 1:no_iters / no_iters
  # weights = c(0.2, 0.8)
  one = c(1)
  all = c(0:100/100)
  #ws = c(0.23,	0.67,	0.6,	0.55,	0.69,	0.78)
  
  ws1 = c(28:30/100) # slope 125
  ws2 = c(18:21/100) # srtm 29
  ws3 = c(71:73/100) # tpi 117
  ws4 = c(10:16/100) # twi 66
  ws5 = c(98:100/100) # coarse
  ws6 = c(93:95/100) # water
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
g <- process_table('./data/geo.csv')
c <- process_table('/data/components.csv')

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

ws = find_hd(c_train, g_train)