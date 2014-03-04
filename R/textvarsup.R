textvarsup <- function(resmca,var,sel=1:nlevels(var),axes=c(1,2),col='black',app=0,vname=NULL) {
   vs <- varsup(resmca,var)
   xy <- vs$coord[sel,axes]
   #lev <- sel
   #if(is.null(sel)) lev <- 1:length(levels(var))
   if(is.null(vname)) texte <- levels(var)[sel] else texte <- paste(vname,levels(var),sep='.')[sel]
   #for(i in 1:length(sel)) text(xy[sel[i],],texte[sel[i]],col=col,cex=cex)
   prop <- round(vs$weight/sum(vs$weight)*2+0.5,1)[sel]
   if(app==0) text(xy,texte,col=col,cex=1)
   if(app==1) text(xy,texte,col=col,cex=prop)
   if(app==2) {
      points(xy,pch=17,cex=prop)
      text(xy,texte,pos=3,col=col)
      }
   }
