dimeta2 <- function(resmca,l,n,dim=1:resmca$call$ncp) {
  eta2 <- matrix(nrow=length(l),ncol=length(dim))
  sub <- rep(TRUE,times=nrow(resmca$ind$coord))
  if(class(resmca)[1]=='csMCA') sub <- resmca$call$subcloud
  for(i in 1:length(dim)) {
    for(j in 1:length(l)) eta2[j,dim[i]] <- summary(lm(resmca$ind$coord[,dim[i]]~l[[j]][sub],weights=resmca$call$row.w[sub]))$r.squared
   }
  rownames(eta2) <- n
  colnames(eta2) <- colnames(resmca$var$contrib)[dim]
  return(round(100*eta2,1))
  }