#### Examen synthèse Capstone
### A1 - Probabilités - convolutions, fft
###

### Convolution directe (2 v.a.)
directconvo<-function(ff1,ff2)
{
    # convolution de deux fns de masses de probabilite
    l1<-length(ff1)
    l2<-length(ff2)
    ffs<-ff1[1]*ff2[1]
    smax<-l1+l2-2
    ff1<-c(ff1,rep(0,smax-l1+1))
    ff2<-c(ff2,rep(0,smax-l2+1))
    for(i in 1:smax)
    {
        j<-i+1
        ffs<-c(ffs,sum(ff1[1:j]*ff2[j:1]))
    }
    return(ffs)
}

### Convolution directe (n v.a.)
directconvo.nrisks<-function(matff=rbind(...))
{
    # convolution de n fns de masses de probabilite
    # supports de longueur egale -- sinon ajouter des 0
    # utiliser rbind pour mettre ensemble les vecteurs de
    # fns de masses de probabilite
    nbrisks<-dim(matff)[1]
    ffs<-matff[1,]
    for (i in 2:nbrisks)
    {
        ffx<-matff[i,]
        ffs<-directconvo(ffs,ffx)
    }
    return(ffs)
}

### Algo récursif de De Pril (n v.a. i.i.d.)
recur.nrisks<-function(ff,nn=5,smax=100)
{
    # convolution de n fns de masses de probabilite avec
    # elle-meme
    # premier algorihtme de DePril
    ll<-length(ff)
    ffs<-ff[1]^nn
    ff<-c(ff,rep(0,smax-ll+1))
    for (i in 1:smax)
    {
        j<-i+1
        ffs<-c(ffs,(1/ff[1])*sum(ff[2:j]*ffs[i:1]*((nn+1)*(1:i)/i-1)))
    }
    return(ffs)
}

#### Panjer
panjer.poisson<-function(lam,ff,smax)
{
    # Algorithme de Panjer
    # Loi de M : Poisson
    # Loi de B : discrete
    aa<-0
    bb<-lam
    ll<-length(ff)
    ffs<-exp(lam*(ff[1]-1))
    ff<-c(ff,rep(0,smax-ll+1))
    for (i in 1:smax)
    {
        j<-i+1
        ffs<-c(ffs,(1/(1-aa*ff[1]))*sum(ff[2:j]*ffs[i:1]*(bb*(1:i)/i+aa)))
    }
    return(ffs)
}


panjer.binom<-function(nn,qq,ff,smax)
{
    # Algorithme de Panjer
    # Loi de M : Binomiale
    # Loi de B : discrete
    aa<- -qq/(1-qq)
    bb<- -(nn+1)*aa
    ll<-length(ff)
    ffs<-(1-qq+qq*ff[1])^nn
    ff<-c(ff,rep(0,smax-ll+1))
    for (i in 1:smax)
    {
        j<-i+1
        ffs<-c(ffs,(1/(1-aa*ff[1]))*sum(ff[2:j]*ffs[i:1]*(bb*(1:i)/i+aa)))
    }
    return(ffs)
}

panjer.nbinom1<-function(rr,qq,ff,smax)
{
    # Algorithme de Panjer
    # Loi de M : Binomiale negative
    # Loi de B : discrete
    aa<-1-qq
    bb<-aa*(rr-1)
    ll<-length(ff)
    ffs<-(qq/(1-(1-qq)*ff[1]))^rr
    ff<-c(ff,rep(0,smax-ll+1))
    for (i in 1:smax)
    {
        j<-i+1
        ffs<-c(ffs,(1/(1-aa*ff[1]))*sum(ff[2:j]*ffs[i:1]*(bb*(1:i)/i+aa)))
    }
    return(ffs)
}


# Lemme 2020 ----

# X1 ~ BinComp(n = 6, q = 0.3) B1 ~ Exp(b1 = 0.12)
# X2 ~ GeoComp(q = 0.4) B2 ~ Exp(b2 = 0.48)
# S = X1 + X2, de façon exacte


n1 <- 6
q1 <- 0.3
bet1 <- 0.12
q2 <- 0.4
bet2 <- 0.48
xx <- 0:(2^13 - 1)
### K: binomiale composée avec loi de sévérité geo essais(qq)
qq <- bet1 / bet2
fb <- c(0, qq * (1 - qq)^(1:(2^13 - 1) - 1))
fbt <- fft(fb)
fkt <- ((1 - q1) + q1 * fbt)^n1
fk <- Re(fft(fkt, inverse = TRUE)) / 2^13

## Loi de L: convolution de K et M2 (m2 est une géo(q2))

fm2 <- dgeom(xx, q2)
fm2t <- fft(fm2)

flt <- fkt * fm2t
fl <- Re(fft(flt, inverse = TRUE))/2^13

FS <- function(x, fs, bet2) fl[1] + sum(fl[-1] * pgamma(x, xx[-1], bet2))

FS(50, fl, bet2)


# X1 ~ Pcomp(lam1 = 10) B1 ~ Erl(alp1 = 7, bet1 = 0.12)
# X2 ~ Pcomp(lam2 = 5) B2 ~ Erl(alp2 = 6, bet2 = 0.8)
# S = X1 + X2, de façon exacte

lam1 <- 10
alp1 <- 7
bet2 <- 0.12

lam2 <- 5
alp2 <- 6
bet2 <- 0.8

qq <- bet1 / bet2
# BN essais
fj <- c(rep(0, alp1), dnbinom(head(xx, -alp1), alp1, qq))
fi <- numeric(2^13)
fi[alp2 + 1] <- 1

fkt <- exp(lam1 * (fft(fj) - 1))
fk <- Re(fft(fkt, inverse = TRUE)) / 2^13

fht <- exp(lam2 * (fft(fi) - 1))

flt <- fkt * fht
fl <- Re(fft(flt, inverse = TRUE)) / 2^13

FS(50, fl, bet2)



## Exemple typique

# X1 ~ PComp(lam = 10) B1 ~ Exp(bet1 = 0.12)
# X2 ~ BinComp(n = 25, p = 0.45) B2 ~ Exp(bet2 = 0.8)
# S = X1 + X2

lam1 <- 10
bet1 <- 0.12

n <- 25
p <- 0.45
bet2 <- 0.8
qq <- bet1 / bet2
## Espérances
EX1 <- lam1 * (1 / bet1)
EX2 <- n * p * (1 / bet2)
EXS <- EX1 + EX2
cbind(EX1, EX2, EXS)

## Variances
VX1 <- lam1 * (1 / bet1^2) + lam1 * (1 / bet1)^2
VX2 <- n * p * (1 / bet2^2) + n * p * (1 - p) * (1 / bet2)^2
VXS <- VX1 + VX2
cbind(VX1, VX2, VXS)

## Survie et autres calculs

xx <- 0:(2^13 - 1)

fj <- c(0, qq * (1 - qq)^(1:(2^13 - 1) - 1))
fjt <- fft(fj)
fkt <- exp(lam1 * (fjt - 1))

fi <- numeric(2^13)
fi[2] <- 1

fit <- fft(fi)
fht <- fft(dbinom(xx, n, p))

fst <- fkt * fht
fs <- Re(fft(fst, inverse = TRUE)) / 2^13
sum(fs)
sum(xx * fs)
sum(xx^2 * fs) - sum(xx * fs)^2

FS <- function(x) fs[1] + sum(fs[-1] * pgamma(x, xx[-1], bet2))
FS(200)
