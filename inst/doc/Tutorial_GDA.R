## ----setup, echo=FALSE, cache=FALSE-------------------------------------------
library(knitr)
library(rmdformats)

oldpar <- par() 
oldoptions <- options()

## Global options
options(max.print="75")
opts_chunk$set(echo=TRUE,
	             cache=FALSE,
               prompt=FALSE,
               tidy=FALSE,
               comment=NA,
               message=FALSE,
               warning=FALSE)
opts_knit$set(width=75)

## ----load---------------------------------------------------------------------
library(GDAtools)
data(Taste)
str(Taste)

## ----nb_na--------------------------------------------------------------------
sapply(Taste[,1:11], function(x) sum(x=="NA"))

## ----see_junk-----------------------------------------------------------------
getindexcat(Taste[,1:11])

## ----mca----------------------------------------------------------------------
mca <- speMCA(Taste[,1:11], excl=c(3,6,9,12,15,18,21,24,27,30,33))

## ----inertia------------------------------------------------------------------
modif.rate(mca)$modif

## ----cloud_ind----------------------------------------------------------------
ggcloud_indiv(mca)

## ----cloud_ind_contour--------------------------------------------------------
ggcloud_indiv(mca, col="lightgray", density="contour")

## ----cloud_ind_hex------------------------------------------------------------
ggcloud_indiv(mca, density="hex", hex.bin=10)

## ----cloud_var----------------------------------------------------------------
ggcloud_variables(mca, shapes=FALSE, legend="none")

## ----tabcontrib1, eval=FALSE--------------------------------------------------
#  tabcontrib(mca, dim=1)

## ----tabcontrib1bis, echo=FALSE-----------------------------------------------
knitr::kable(tabcontrib(mca, dim=1), row.names=FALSE)

## ----tabcontrib2, eval=FALSE--------------------------------------------------
#  tabcontrib(mca, dim=2)

## ----tabcontrib2bis, echo=FALSE-----------------------------------------------
knitr::kable(tabcontrib(mca, dim=2), row.names=FALSE)

## ----cloud_varsup, warning=FALSE----------------------------------------------
p <- ggcloud_variables(mca, shapes=FALSE, col="lightgray")
p <- ggadd_supvar(p, mca, Taste$Age, col="dodgerblue3", shape=NULL)
p <- ggadd_supvar(p, mca, Taste$Educ, col="tomato", shape=NULL)
ggadd_supvar(p, mca, Taste$Gender, col="seagreen", shape=NULL)

## ----dimeta2------------------------------------------------------------------
dimeta2(mca, Taste[,c("Gender","Age","Educ")])

## ----condesc1, eval=FALSE-----------------------------------------------------
#  dimdescr(mca, vars=Taste[,c("Gender","Age","Educ")])$dim.1$categories

## ----condesc1bis, echo=FALSE--------------------------------------------------
knitr::kable(dimdescr(mca, vars=Taste[,c("Gender","Age","Educ")])$dim.1$categories, row.names=FALSE)

## ----condesc2, eval=FALSE-----------------------------------------------------
#  condesc(mca$ind$coord[,2], Taste[,c("Gender","Age","Educ")])$categories

## ----condesc2bis, echo=FALSE--------------------------------------------------
knitr::kable(condesc(mca$ind$coord[,2], Taste[,c("Gender","Age","Educ")])$categories, row.names=FALSE)

## ----varsup-------------------------------------------------------------------
varsup(mca, Taste$Educ)

## ----educ_ellipses------------------------------------------------------------
p <- ggcloud_indiv(mca, col='lightgrey')
ggadd_kellipses(p, mca, Taste$Educ, label=FALSE)

## ----educ_ellipse-------------------------------------------------------------
ggadd_kellipses(p, mca, Taste$Educ, sel=4, legend="none")

## ----educ_contour-------------------------------------------------------------
ggadd_density(p, mca, var=Taste$Educ, cat="High", density="contour")

## ----educ_area----------------------------------------------------------------
ggadd_density(p, mca, var=Taste$Educ, cat="High", density="area", ellipse=TRUE)

## ----educ_corr----------------------------------------------------------------
ggadd_corr(p, mca, var=Taste$Educ, cat="High", xbins=20, ybins=20)

## ----chull--------------------------------------------------------------------
d <- dist(mca$ind$coord[,c(1,2)])
hca <- hclust(d, "ward.D2")
cluster <- factor(cutree(hca, 3))
ggadd_chulls(p, mca, cluster)

## ----interaction--------------------------------------------------------------
p <- ggcloud_variables(mca, col='lightgrey', shapes=FALSE)
ggadd_interaction(p, mca, Taste$Gender, Taste$Age, col=c("tomato3","dodgerblue3"), legend="none")

## ----typic--------------------------------------------------------------------
dimtypicality(mca, Taste[,c("Gender","Age","Educ")], dim=c(1,2), max.pval=0.05)

## ----varsup_educ--------------------------------------------------------------
vseduc <- varsup(mca, Taste$Educ)
vseduc$pval[,c(1,2)]

## ----educ_conc_ellipses-------------------------------------------------------
p <- ggcloud_indiv(mca, col='lightgrey')
ggadd_ellipses(p, mca, Taste$Educ, level=0.05, label=FALSE)

## ----homog_test1--------------------------------------------------------------
ht <- homog.test(mca, Taste$Educ)
ht$dim.1$p.values

## ----homog_test2--------------------------------------------------------------
ht$dim.2$p.values

## ----csa----------------------------------------------------------------------
csa <- csMCA(Taste[,1:11], Taste$Educ=="High", excl=c(3,6,9,12,15,18,21,24,27,30,33))

## ----csa_cloud_var------------------------------------------------------------
ggcloud_variables(csa, shapes=FALSE, legend="none")

## ----csa_tabcontrib1, eval=FALSE----------------------------------------------
#  tabcontrib(csa, dim=1)

## ----csa_tabcontrib1bis, echo=FALSE-------------------------------------------
knitr::kable(tabcontrib(csa, dim=1), row.names=FALSE)

## ----csa_tabcontrib2, eval=FALSE----------------------------------------------
#  tabcontrib(csa, dim=2)

## ----csa_tabcontrib2bis, echo=FALSE-------------------------------------------
knitr::kable(tabcontrib(csa, dim=2), row.names=FALSE)

## ----st_mca-------------------------------------------------------------------
stmca <- stMCA(mca, control=list(Taste$Educ))
ggcloud_variables(stmca, shapes=FALSE, legend="none")

## ----mfa----------------------------------------------------------------------
mca1 <- speMCA(Taste[,1:5],excl=c(3,6,9,12,15))
mca2 <- speMCA(Taste[,6:11],excl=c(3,6,9,12,15,18))
mfa <- multiMCA(list(mca1,mca2))
ggcloud_variables(mfa, shapes=FALSE, legend="none")

