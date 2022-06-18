test_ch <- function(){
  # Rパッケージ_Cohen's d & Hedges' g_ver.0.0.1

  dat <- read.csv(file.choose(), header=T)

  m1 <- mean(dat[,2])
  m2 <- mean(dat[,3])
  n1 <- length(dat[,2])
  n2 <- length(dat[,3])
  s1 <- sd(dat[,2])
  s2 <- sd(dat[,3])

  # Cohen's d
  Sp1 <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2))
  d <- abs(m1 - m2)/Sp1

  # Hedges' g
  sp2 <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
  g <- abs(m1 - m2)/sp2

  return(c("Cohen's d"=d,"Hedges' g"=g))
}
