tp1_exo1 = function(x) {
  
  x = sort(x);
  
  nb_classe = round(1 + log2(length(x)));
  borne_basse = x[1] - 0.025*(x[length(x)] - x[1]);
  borne_haute = x[length(x)] + 0.025*(x[length(x)] - x[1]);
  
  par(mfrow=c(3,3));
  
  histx = hist(x, prob=T, breaks=seq(borne_basse, borne_haute, length = nb_classe+1), main = "Classe de meme largeur");
  lines(histx$mids, histx$density, lwd=3);
  
  histx = hist(x, prob=T, breaks=c(borne_basse, quantile(x,seq(1,nb_classe-1)/nb_classe),borne_haute), main="Classe de meme effectif");
  lines(histx$mids, histx$density, lwd=3);
  
  plot(ecdf(x), main="Fonction de repartition empririque");
  
  size = length(x);
  
  plot(x[1:(size-1)],log(1-seq(1:(size-1))/size), main="Graphe de probabilite exponentiel");
  abline(lm(log(1-seq(1:(size-1))/size) ~ x[1:(size-1)]), col = "red")
  abline(v=0);
  abline(h=0);
  
  plot(x[1:(size-1)], qnorm(seq(1:(size-1))/size), main="Graphe de probabilite normal");
  abline(lm(qnorm(seq(1:(size-1))/size) ~ x[1:(size-1)]), col = "red")
  abline(v=0);
  abline(h=0);
  
  plot(x[1:(size-1)], qunif(seq(1:(size-1))/size), main="Graphe de probabilite uniforme");
  abline(lm(qunif(seq(1:(size-1))/size) ~ x[1:(size-1)]), col = "red")
  abline(v=0);
  abline(h=0);
}

simulation = function(lambdaest) {
  
  lambda = lambdaest;
  results = c();
  n = 30;
  
  for(i in 1:10000) {
    x = rexp(n, lambda);
    results = c(results, 1/mean(x));
  }
  
  print(paste("Moyenne biaisee", mean(results)));
  print(paste("Moyenne", mean(results)*(n-1)/n));
  
}


donnees <- c(0.22, 0.24, 0.29, 0.29, 0.33, 0.47, 0.85, 1.14, 1.50, 1.51, 1.64, 1.96, 2.27, 2.44, 2.75, 2.99, 3.15, 3.85, 4.66, 5.04)
tp1_exo1(donnees)
plot(donnees)

#1/estmation = moyenne
moyDonnees = sum(donnees)
lambdaEst <- 20/moyDonnees
thetaEst = 1/lambdaEst

print(lambdaEst)

# On cherche maintenant le vrai lambda on va chercher le biais grace à la simulation
simulation(lambdaEst)

w <- rexp(1000, rate=lambdaEst)
w = sort(w)
plot(w)