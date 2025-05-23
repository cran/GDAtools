ggadd_ellipses <- function(p, resmca, var, sel=1:nlevels(var), axes=c(1,2), level=0.05, label=TRUE, label.size=3, size=0.5, points=TRUE, legend='right') {

  subvar <- factor(var)
  
  type <- attr(resmca,'class')[1]
  if(type=="stMCA") type <- resmca$call$input.mca

  if(type=="bcMCA") {
    wt <- resmca$mycall$row.w
  } else {
    wt <- resmca$call$row.w
  }  
  
  if(type=="csMCA") subvar <- factor(var[resmca$call$subcloud])

  if(type=="multiMCA") {
    if(class(resmca$my.mca[[1]])[1]=="csMCA") subvar <- factor(var[resmca$my.mca[[1]]$call$subcloud])
  }
  
  ecoord <- as.data.frame(resmca$ind$coord[,axes])
  names(ecoord) <- c('axeX','axeY')
  ecoord$var <- subvar
  ecoord <- ecoord[subvar %in% levels(subvar)[sel],]
  ecoord$var <- factor(ecoord$var)

  ccoord <- agg.wtd.mean(resmca$ind$coord[,axes], var, wt)
  names(ccoord) <- c('axeX','axeY')
  ccoord$categories <- rownames(ccoord)
  ccoord <- ccoord[sel,]

  pfin <- p + ggplot2::stat_ellipse(data=ecoord, ggplot2::aes(x=.data$axeX, y=.data$axeY, colour=.data$var), level = level, type='norm', size = size)
              
  if(points) pfin <- pfin + ggplot2::geom_point(data=ecoord, ggplot2::aes(x=.data$axeX, y=.data$axeY, colour=.data$var), size = 0.5, alpha = 0.6)
  
  if(label) { pfin <- pfin + ggplot2::geom_text(key_glyph='blank', data=ccoord, ggplot2::aes(x=.data$axeX, y=.data$axeY, label=.data$categories, colour=.data$categories), size=label.size) 
  } else { pfin <- pfin + ggplot2::geom_point(data=ccoord, ggplot2::aes(x=.data$axeX, y=.data$axeY, colour=.data$categories), shape=8, size=3) }

  pfin <- pfin + ggplot2::guides(color = ggplot2::guide_legend(title="")) +
                 ggplot2::theme(legend.position = legend)

  pfin

}
