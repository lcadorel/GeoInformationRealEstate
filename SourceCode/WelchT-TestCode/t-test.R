# Compute the p-value for Welch's t-test

Welch <- function(X1, s1, n1, X2, s2, n2)
{
  r1 <- s1*s1/n1
  r2 <- s2*s2/n2
  t <- (X1 - X2) / sqrt(r1 + r2)
  df <- (r1 + r2)*(r1 + r2) / ( r1*r1/(n1 - 1) + r2*r2/(n2 - 1) )
  #cat("t = ", t, "\n")
  #cat("d.f. = ", df, "\n")
  p <- pt(-abs(t), df)
  #cat("p-value = ", p, "\n")
  p
}

d <- read.table("cmp.txt", header = TRUE)
for(i in 1:dim(d)[1])
  for(j in i:dim(d)[1])
    if(i!=j)
    {
      cat(as.character(d$label[i]), " vs. ", as.character(d$label[j]), ": ",
        Welch(as.double(d$avg[i]), as.double(d$std[i]), d$n[i],
              as.double(d$avg[j]), as.double(d$std[j]), d$n[j]), "\n")
    }

