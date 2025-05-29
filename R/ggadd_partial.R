ggadd_partial <- function(p, 
                          resmca,
                          var,
                          controls,
                          excl = NULL,
                          axes = c(1,2),
                          col = "black",
                          textsize = 4,
                          lines = TRUE,
                          dashes = TRUE,
                          legend = "right",
                          force = 1, 
                          max.overlaps = Inf) {
  
  controls <- as.data.frame(controls)
  
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
    var <- var[resmca$call$subcloud]
    controls <- controls[resmca$call$subcloud,]
  }
  if(type=="multiMCA") {
    if(class(resmca$my.mca[[1]])[1]=="csMCA") {
      var <- var[resmca$my.mca[[1]]$call$subcloud]
      controls <- controls[resmca$my.mca[[1]]$call$subcloud,]
    }
  }

  var <- factor(var)
  controls <- sapply(controls, factor)
  
  wvar <- descriptio::weighted.table(var, weights = wt)
  
  ind <- as.data.frame(resmca$ind$coord)[,axes]
  
  # main effects
  coord_main <- agg.wtd.mean(ind, var, wt)
  coord_main$cat <- rownames(coord_main)
  coord_main$type <- rep("main", nrow(coord_main))

  # partial effects
  new <- replicate(nlevels(var), data.frame(var, controls), simplify = FALSE)
  new <- do.call("rbind.data.frame", new)
  new$var <- unlist(sapply(levels(var), function(x) rep(x, length(var)), simplify = FALSE))
  res <- list()
  for(i in 1:ncol(ind)) {
    model <- lm(dim ~ ., weights = wt, data = data.frame(dim = ind[,i], var, controls))
    pred <- stats::predict(model, new, type = "response")
    res[[i]] <- agg.wtd.mean(pred, new$var, rep(wt, nlevels(var)))
  }
  coord_partial <- do.call("cbind.data.frame", res)
  names(coord_partial) <- names(ind)
  coord_partial$cat <- rownames(coord_partial)
  coord_partial$type <- rep("part", nrow(coord_partial))

  # bond main and partial effects
  coord <- rbind.data.frame(coord_main, coord_partial)
  names(coord)[1:2] <- c('axeX','axeY')
  coord <- coord[!coord$cat %in% excl,]
  coord$cat <- factor(coord$cat)
  coord$type <- factor(coord$type)

  p <- p + ggplot2::geom_point(data = coord, 
                               ggplot2::aes(alpha = .data$type),
                               color = col) +
    ggrepel::geom_text_repel(key_glyph = 'blank', 
                             data = coord, 
                             ggplot2::aes(alpha = .data$type,
                                          label = .data$cat),
                             color = col,
                             size = textsize,
                             force = force, 
                             max.overlaps = max.overlaps) +
    ggplot2::scale_alpha_discrete("effect", range = c(1, 0.3))
  
  if(lines) p <- p +
    ggplot2::geom_path(data=coord,
                       ggplot2::aes(x = .data$axeX,
                                    y = .data$axeY,
                                    alpha = .data$type,
                                    group = .data$type),
                       color = col)
  
  if(dashes) p <- p +
    ggplot2::geom_path(data = coord, 
                       ggplot2::aes(x = .data$axeX,
                                    y = .data$axeY,
                                    group = .data$cat),
                       color = "darkgray", 
                       linetype = "dashed")
  
  p <- p + 
    ggplot2::theme(legend.position = legend)
  
  p
}
