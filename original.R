library('sna')

inc <- function (x) {
  eval.parent(substitute(x <- x + 1))
}

getHdist <- function (f1, dm2) {
  fm1 = as.matrix(f1)
  dm1 = as.matrix(dist(fm1))
  
  if (max(dm1) == 0) return(0)
  
  dm1 = dm1/max(dm1)
  a <- array(dim=c(2,dim(dm1)[2],dim(dm1)[1]))
  a[1,,] <- dm1
  a[2,,] <- dm2
  
  return(hdist(a, mode="graph")[,1][2])
}

f1 <- read.csv("cadasters_sequences.csv", stringsAsFactors=FALSE, header=TRUE)
f2 <- read.csv("cadasters_normalised.csv", stringsAsFactors=FALSE, header=TRUE)

rownames(f1) <- f1$id
f1[1] <- NULL

rownames(f2) <- f2$id
f2[1] <- NULL

nCols = dim(f2)[2]

fm1 = as.matrix(f1)
dm1 = as.matrix(dist(fm1))

iterValues <- c(0:niter)/niter

write(c('slope','srtm','tpi','twi','coarse','stream','hdist'), file='hdists.csv', ncolumns=7, append=FALSE, sep=', ')

iterations = 0:100/100

for (i1 in iterations) {
  for (i2 in iterations) {
    for (i3 in iterations) {      
      for (i4 in iterations) {
        for (i5 in iterations) {
          for (i6 in iterations) {
            
            n = inc(n)
            fc = f2
            fc[1] = fc[1] * i1
            fc[2] = fc[2] * i2
            fc[3] = fc[3] * i3
            fc[4] = fc[4] * i4
            fc[5] = fc[5] * i5
            fc[6] = fc[6] * i6
            
            outRow <- c(i1, i2, i3, i4, i5, i6, getHdist(fc, dm1))
            write(outRow , file='hdists.csv', ncolumns=7, append=TRUE, sep=', ')
          }
        }
      }
    }
  }
}