# sd <- 1; source('/users/kzth541/util/R/tks.R'); v6 <- v
#q()
# Rscript ~/util/R/tks.R -input ~/apps/tks/Tams.data/pd.xml -output tmp.1.out -nrep 4 -model 'PA.Meropenem'

# TODO: cluster results

if (0) {
  # reading tabular data
  options(width=180)
  fn <- 'SA516.TopX996.txt'
  fn <- 'SA516.TopX830.txt'
  fn <- '/users/kzth541/apps/tks/Tams.data/PA.Levo/PA.Levo.txt.orig'
  fn <- '/users/kzth541/apps/tks/Tams.data/AB.Amik/tmp'
  x <- read.table(fn, header=F, sep='\t')
  times <- x[1, 3:10]
  z <- as.character(x$V2)[2:10]
  C <- numeric(length(z))
  for (i in 1:length(z)) {
    #C[i] <- strsplit(strsplit(z[i], 'x')[[1]], 'X')[[1]][2] 
    C[i] <- strsplit(z[i], 'x')[[1]]
  }
  C <- as.numeric(C)
  #n <- names(x)
  #substring(n, 1, 1) <- ''  
  #times <- as.numeric(n)
  #C <- as.numeric(row.names(x))

  # convert from xls to xml

  #summary(x)
  times <- x[,1]
  nc <- ncol(x)
  z <- names(x)[2:nc]
  x <- log10(x[,2:ncol(x)])
  x <- round(x, digits=2)
  out <- 'tmp'
  out <- '/users/kzth541/apps/tks/Tams.data/PA.Levo/PA.Levo.txt'
  cat('times=', as.numeric(times), '\n', file=out)
  cat('C=', C, '\n', file=out, append=T)
  #write.table(t(x), file=out, quote=F, row.names=F, col.names=F, sep='\t', append=T)
  write.table(x[2:10, 3:10], file=out, quote=F, row.names=F, col.names=F, sep='\t', append=T)
  system('more tmp')
  times <- c(0, 2, 4, 6, 24)
  C <- c(0, 2^(-2:4))
}

if (0) {
  png('/users/kzth541/apps/tks/Tams.data/AB.Amik/AB.Amik.png')
  x <- get.xml.data('/users/kzth541/apps/tks/Tams.data/AB.Amik/AB.Amik.xml')
  m <- x$exp.data$StDev > 0
  plot(x$exp.data$log10CFU[m], x$exp.data$StDev[m], main='AB.Amik', xlab='log10(CFU/mL)', ylab='StDev')
  text(5, 0.85, sprintf('Mean(StDev) = %4.2f', mean(x$exp.data$StDev[m])), cex=1.5, adj=c(0,0))
  text(5, 0.75, sprintf('SD(StDev) = %4.2g', sd(x$exp.data$StDev[m])), cex=1.5, adj=c(0,0))
  dev.off(dev.cur())
}

if (0) {
  fn <- '/users/kzth541/apps/tks/Tams.data/PA.Gent/log2.nrep.8/VT2005_2.txt'
  lst <- list()
  for (i in 0:3) {
    fn <- sprintf('/users/kzth541/apps/tks/Tams.data/PA.Gent/log2.nrep.9/VT2005_%d.txt', i)
    cat('fn =', fn, '\n')
    lst[[as.character(i)]] <- x <- read.table(fn, header=T)
  }
  for (i in 0:3) {
    x <- lst[[as.character(i)]]
    m <- 1:(nrow(x)/2)
    cat(round(sd(x[m,c('Kg', 'Kk', 'H')]), digits=2), '\n')
  }
  round(cov(x[m,c('Kg', 'Kk', 'H')]), digits=2)
  #plot(density(x$RMSE))
  #pairs(log10(x[m,c('Kg', 'Kk', 'H')]), diag.panel=function(x){par(new=T); plot(density(x), main=''); rug(x)})
  pairs(x[m,c('Kg', 'Kk', 'H')], diag.panel=function(x){par(new=T); plot(density(x), main=''); rug(x)})
  # mean values for PA.Gent (sdo=0.25)
  png('~/apps/tks/img/PA.Gent.zcov.png')
  #pairs(x[m,c('Kg', 'Kk', 'H')], diag.panel=function(x){par(new=T); plot(density(x), main=''); rug(x)})
  y <- x[m,c('Kg', 'Kk', 'H')]
  z <- mvrnorm(1024, mu=mean(y), Sigma=cov(y))
  pairs(z, diag.panel=function(x){par(new=T); plot(density(x), main=''); rug(x)})
  #pairs(z, diag.panel=function(x){par(new=T); plot(density(x), main=''); rug(x)},
  #         panel = function(x, zc) {par(new=T); points(zc$medoids, col='red', pch=19, cex=3)} )
  #pairs(z, diag.panel=function(x){par(new=T); plot(density(x), main=''); points(zc$medoids[,sc], col='red', pch=19, cex=3); rug(x)})
  dev.off(dev.cur())

  library(cluster)
  zc <- pam(z, k=3)
  yc <- pam(y, k=3)
  round(yc$medoids, digits=2)
  png('~/apps/tks/img/PA.Gent.cl23.png')
  sc <- c(2,3); plot(z[,sc], type='p'); points(zc$medoids[,sc], col='red', pch=19, cex=3)
  dev.off(dev.cur())
  eh <- ellipsoidhull(z)

  cl <- zc$medoids
  for (i in 1:3) {
    png(sprintf('~/apps/tks/img/PA.Gent.cl%d.png', i))
    Kg <- cl[i,'Kg']; Kk=cl[i,'Kk']; H <- cl[i,'H']
    plot.D.over.Kg(Kg=Kg, Kk=Kk, H=H, CL=1, C.50=1, C.cr=6.72717, dd.max=322.9042, ylab='Daily dose [mg]', plt.3D=FALSE)
    dev.off(dev.cur())
  }
  plot.D.over.Kg(Kg=1.2, Kk=10.1, H=2.0, CL=1, C.50=1, C.cr=6.72717, dd.max=322.9042, ylab='Daily dose [mg]', plt.3D=FALSE)
  plot.D.over.Kg(Kg=0.5, Kk=4.4, H=3, CL=1, C.50=1, C.cr=6.72717, dd.max=322.9042, ylab='Daily dose [mg]', plt.3D=FALSE)
  png(sprintf('~/apps/tks/img/PA.Gent.cl-ex-2.png'))
  plot.D.over.Kg(Kg=1.04, Kk=10.7, H=1.04, CL=1, C.50=1, C.cr=6.72717, dd.max=322.9042, ylab='Daily dose [mg]', plt.3D=FALSE)
  dev.off(dev.cur())
}

if (0) {
  input <- "/users/kzth541/apps/tks/Tams.data/PA.Gent/PA.Gent.xml"
  input <- "/users/kzth541/apps/tks/Tams.data/PA.Levo/PA.Levo.xml"
  m <- get.xml.data(input)
  x <- m$exp.data$times;
  y1 <- m$exp.data$log10CFU[1,]
  y3 <- m$exp.data$log10CFU[3,]
  y6 <- m$exp.data$log10CFU[6,]
  png('/users/kzth541/apps/tks/Tams.data/PA.Levo/PA.Levo.StDev.2.png')
  n <- length(y1); nrep <- 64; StDev <- 2^(-2)
  plot(x, y1, ylim=c(0,10), type='n', main=sprintf('PA.Levo: StDev = %g', StDev), xlab='time [h]',
      ylab='log10(CFU/mL)')
  for (i in 1:nrep) lines(x, y1+rnorm(n, sd=StDev))
  for (i in 1:nrep) lines(x, y3+rnorm(n, sd=StDev))
  for (i in 1:nrep) lines(x, y6+rnorm(n, sd=StDev))
  points(x, y1, pch=19, cex=2, col='red')
  points(x, y3, pch=19, cex=2, col='green')
  points(x, y6, pch=19, cex=2, col='pink')
  dev.off(dev.cur())
}

AB.Amik <- function (mkpng=FALSE, rdir='/users/kzth541/apps/tks/Tams.data', id='AB.Amik') {
  # for AB.Amik
  mkpng <- FALSE
  dir <- sprintf('%s/%s', rdir, id)
  x <- read.table(sprintf('%s/%s/VT2005_2.txt', rdir, id), header=T)
  m <- 1:(nrow(x)/2)
  pnames <- c('Kg','Kk','C50k','H')
  if (mkpng) png(sprintf('%s/%s.pairs.png', dir, id))
  pairs(x[m,pnames], diag.panel=function(x){par(new=T); plot(density(x), main=''); rug(x)})
  if (mkpng) dev.off(dev.cur())
  p.mean <- mean(x[m,pnames])
  p.sd <- sd(x[m,pnames])
  p.cov <- cov(x[m,pnames])
  cat('Mean=\n'); print(round(p.mean, digits=2))
  cat('SD=\n'); print(round(p.sd, digits=2))
  cat('COV=\n'); print(round(p.cov, digits=2))
  nc <- 4
  xc <- pam(x[m,], nc)
  parms <- x2par(as.data.frame(xc$medoids), 1:nc)
  input <- sprintf('%s/%s/%s.xml', rdir, id, id)
  model <- get.xml.data(input, dt=2^(-6))
  #for (i in 1:nc) {
    p <- as.numeric(parms[i,])
    names(p) <- names(parms)
    model$parms <- p
    if (mkpng) png(sprintf('%s/%s.cluster-%d.png', dir, id, i))
    model$solver <- 'bernoulli'
    yp.b <- plot.fit(model, main=sprintf('%s: cluster: %d', id, i))
    model$solver <- 'ode'
    yp.o <- plot.fit(model, main=sprintf('%s: cluster: %d', id, i))
    plot(yp.o, yp.b, main=sprintf('dt = %g', model$dt))
    if (mkpng) dev.off(dev.cur())
    if (mkpng) png(sprintf('%s/%s.2D-%d.png', dir, id, i))
    #plot.D.over.Kg(Kg=p['Kg'], Kk=p['Kk'], C.50=p['C50k'], H=p['H'], CL=380, t.half=3, cuts=9, dd.max=6200)
    if (mkpng) dev.off(dev.cur())
    else system('sleep 9')
  }
  vt.parms <- c(N0=10^6.815, Nmax=10^8.82, Kg=0.55, Kk=27.81, C50k=1.56, H=3.06, beta=50.09, tau=0.0265)
  model$parms <- vt.parms
  if (mkpng) png(sprintf('%s/%s.VTreport.png', dir, id))
  yp <- plot.fit(model, main=sprintf('%s: VT', id))
  if (mkpng) dev.off(dev.cur())

  if (mkpng) png(sprintf('%s/%s.VTreport-2D.png', dir, id))
  p <- vt.parms
  plot.D.over.Kg(Kg=p['Kg'], Kk=p['Kk'], C.50=p['C50k'], H=p['H'], CL=380, t.half=3, cuts=9, dd.max=6200)
  if (mkpng) dev.off(dev.cur())
}

png('Tams.data/AB.Amik/AB.Amik.C50.png')
t=1:24; C=32; C50k=1.19; tau=2.189e-02; beta=37.46; y=C50k*(1+beta*(1-exp(-C*tau*t)))
plot(t, y, type='l', ylab='C50', main='AB.Amik', ylim=c(0,45))
xt <- 17; yt <- 10; dy <- 2.5; adj <- c(0, 1); cex=1.5
text(xt, yt, sprintf('C50k = %g', C50k), adj=adj, cex=cex)
text(xt, yt-dy, sprintf('beta = %g', beta), adj=adj, cex=cex)
text(xt, yt-2*dy, sprintf('tau = %g', tau), adj=adj, cex=cex)
for (C in c(1,4,8,16)) {
  y <- C50k*(1+beta*(1-exp(-C*tau*t)))
  lines(t, y)
}

dev.off(dev.cur())
