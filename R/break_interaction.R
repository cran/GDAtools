break_interaction <- function(resmca, v1, v2) {
  
  type <- attr(resmca,'class')[1]
  
  if(type=="bcMCA") {
    wt <- resmca$mycall$row.w
  } else if(type=="csMCA") {
    wt <- resmca$call$row.w[resmca$call$subcloud]
  } else {
    wt <- resmca$call$row.w
  }
  
  if(type=="stMCA") type <- resmca$call$input.mca
  if(type=="csMCA") {
    v1 <- v1[resmca$call$subcloud]
    v2 <- v2[resmca$call$subcloud]
  }
  if(type=="multiMCA") {
    if(class(resmca$my.mca[[1]])[1]=="csMCA") {
      v1 <- v1[resmca$my.mca[[1]]$call$subcloud]
      v2 <- v2[resmca$my.mca[[1]]$call$subcloud]
    }
  }
  
  v1 <- factor(v1)
  v2 <- factor(v2)
  v12 <- interaction(v1, v2)
  
  ind <- as.data.frame(resmca$ind$coord)
  
  ## mean points of v1 X v2
  coord <- agg.wtd.mean(ind, v12, wt)
  coord$v1 <- factor(rep(levels(v1), nlevels(v2)))
  coord$v2 <- factor(unlist(lapply(levels(v2), function(x) rep(x, nlevels(v1)))))
  coord$weight <- as.numeric(descriptio::weighted.table(v12, weights = wt))
  
  ## additive cloud
  # new <- expand.grid(levels(v1), levels(v2))
  # names(new) <- c("v1", "v2")
  # add <- sapply(1:ncol(ind), function(x) stats::predict(stats::lm(coord[,x] ~ coord$v1 + coord$v2, weights = coord$weight), new))
  # add <- as.data.frame(add)
  # names(add) <- names(ind)
  # add <- cbind.data.frame(add, coord[,c("v1","v2")])
  
  ## v1 X v2
  sup12 <- supvar(resmca, v12)
  B7 <- sup12$var["between",]
  
  ## between v1
  B1 <- supvar(resmca, v1)$var["between",]
  B2 <- B7 - B1
  
  ## between v2
  B3 <- supvar(resmca, v2)$var["between",]
  B4 <- B7 - B3
  
  ## additive
  r2 <- sapply(1:ncol(ind), function(x) summary(stats::lm(coord[,x] ~ coord$v1 + coord$v2, weights = coord$weight))$r.squared)
  B5 <- sup12$var["between",]*r2
  B6 <- B7 - B5
  B8 <- B6 / B7
  
  res <- rbind.data.frame(B1, B2, B3, B4, B5, B6, B7, B8)
  res <- round(res, 4)
  variance <- c("between v1",
                "v2 within-v1",
                "between v2",
                "v1 within-v2",
                "additive",
                "interaction",
                "v1 x v2",
                "ratio interaction")
  res <- data.frame(variance, res)
  rownames(res) <- NULL
  # res[8,-1] <- round(res[8,-1], 3)
  
  return(res)
}
