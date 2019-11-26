### MASTER SCRIPT: TANGED AND BACKED POINT ANALYSIS ###

### AUTHOR: CHRISTIAN STEVEN HOGGARD ###

### NOTES ###
### THIS SCRIPT WAS DESIGNED TO EXAMINE AND ANALYSE THE LARGE CORPUS
### OF TANGED AND BACKED POINT ANALYSIS COLLATED BY MEMBERS OF THE
### LAPADIS/CLIOARCH TEAM (AARHUS UNIVERSITY). IT INCLUDES THE RAW
### OUTLINE DATA, MASTER DATABASES AND THE RPHYLIP OUTPUTS (THESE 
### CAN ALSO BE PRODUCED THROUGH THE CODE BELOW).

### THIS SCRIPT IS UNPUBLISHED AND IS NOT YET STORED ON A GITHUB
### REPOSITORY OR OPEN SCIENCE FRAMEWORK PROJECT PAGE

### R VERSION: X86_64_W64-MINGW32 (3.6.1) ###
### R SUBVERSION REVISION NUMBER: 76782 ###
### ARCHITECTURE: X86_64 ###

### LAST MODIFIED: 26/09/2019

#################################################################

### PACKAGES ###

if(!require("tidyverse")) install.packages('tidyverse', repos='http://cran.us.r-project.org')
if(!require("Momocs")) install.packages('geomorph', repos='http://cran.us.r-project.org')
if(!require("ggtree")) install.packages('ggtree', repos='http://cran.us.r-project.org')
if(!require("ape")) install.packages('ape', repos='http://cran.us.r-project.org')
if(!require("Rphylip")) install.packages('Rphylip', repos='http://cran.us.r-project.org')
if(!require("tidytree")) install.packages('tidytree', repos='http://cran.us.r-project.org')
if(!require("phytools")) install.packages('phytools', repos='http://cran.us.r-project.org')
if(!require("kohonen")) install.packages('kohonen', repos='http://cran.us.r-project.org')
if(!require("cowplot")) install.packages('cowplot', repos='http://cran.us.r-project.org')

### STAGE 1: DATA IMPORTING ###

backedpoints <- import_tps("TPS_BP_18_10_2019.tps", curves = TRUE) ### import .tps file
tangedpoints <- import_tps("TPS_TP_27_09_2019.tps", curves = TRUE) ### import .tps file
backed.points <- read.csv("backed.points.csv", header = TRUE, sep = ",", encoding = "UTF-8", row.names = 1)
tanged.points <- read.csv("tanged.points.csv", header = TRUE, sep = ",", encoding = "UTF-8", row.names = 1)

### STAGE 2: DATA TIDYING ###

backed.points$Site <- as.character(backed.points$Site)
backed.points$Context <- as.character(backed.points$Context)

tanged.points$Site <- as.character(tanged.points$Site)
tanged.points$Context <- as.character(tanged.points$Context)

### STAGE 3A: OUTLINE FILE CREATION ###

outlinebp <- Out(backedpoints$coo, fac = backed.points)
outlinebp <- filter(outlinebp, Burinated=="N") ### removal of burinated examples
outlinebp <- filter(outlinebp, OTT=="Y") ### removal of cutting edges less than two-thirds
summary(outlinebp$ATU)
summary(outlinebp$ATU2)
summary(outlinebp$NAT)

outlinebpatu  <- filter(outlinebp, !ATU %in% "Unspecified", !ATU %in% "Multiple")
outlinebpatu2 <- filter(outlinebp, !ATU2 %in% "Unspecified")
outlinebpNAT  <- filter(outlinebp, !NAT %in% "Backed Point (Unspecified)")
summary(outlinebpatu$ATU)
summary(outlinebpatu2$ATU2)
summary(outlinebpNAT$NAT)

outlinetp <- Out(tangedpoints$coo, fac = tanged.points)
outlinetp <- filter(outlinetp, Burinated=="N") ### removal of burinated examples
outlinetpatu  <- filter(outlinetp, !ATU %in% "Unspecified")  
outlinetpatu2 <- filter(outlinetp, !ATU2 %in% "Unspecified") 
outlinetpNAT  <- filter(outlinetp, !NAT %in% "Tanged Point (Unspecified)")
summary(outlinetp$ATU)
summary(outlinetp$ATU2)
summary(outlinetp$NAT)

### STAGE 3B: EXAMINATION OF DATA (FOLLOWING SCREENING) ###

unique(outlinebp$Site) 
unique(outlinetp$Site)

unique(outlinebpatu$Site)
unique(outlinebpatu2$Site)
unique(outlinebpNAT$Site)

unique(outlinetpatu$Site)
unique(outlinetpatu2$Site)
unique(outlinetpNAT$Site)

unique(outlinebp$Context)
unique(outlinetp$Context)

unique(outlinebpatu$Context)
unique(outlinebpatu2$Context)
unique(outlinebpNAT$Context)

unique(outlinetpatu$Site)
unique(outlinetpatu2$Site)
unique(outlinetpNAT$Site)

unique(outlinebp$Country)
unique(outlinetp$Country)

unique(outlinebpatu$Country)
unique(outlinebpatu2$Country)
unique(outlinebpNAT$Country)

unique(outlinetpatu$Country)
unique(outlinetpatu2$Country)
unique(outlinetpNAT$Country)

summary(outlinebp$Country)
summary(outlinetp$Country)

summary(outlinebpatu$Country)
summary(outlinebpatu2$Country)
summary(outlinebpNAT$Country)

summary(outlinetpatu$Country)
summary(outlinetpatu2$Country)
summary(outlinetpNAT$Country)

summary(outlinebp$ATU)
summary(outlinetp$ATU)

summary(outlinebpatu$ATU)
summary(outlinetpatu$ATU)

summary(outlinebp$ATU2)
summary(outlinetp$ATU2)

summary(outlinebpatu2$ATU2)
summary(outlinetpatu2$ATU2)

summary(outlinebp$NAT)
summary(outlinetp$NAT)

summary(outlinebpNAT$NAT)
summary(outlinetpNAT$NAT)

### STAGE 4: OUTLINE ALTERATION ###

outlinebp$coo[which(outlinebp$Retouch.position=="Right")] <- lapply(outlinebp$coo[which(outlinebp$Retouch.position=="Right")], coo_flipy) ### flip outline
outlinebp$coo[which(outlinebp$Retouch.position=="Right")] <- lapply(outlinebp$coo[which(outlinebp$Retouch.position=="Right")], coo_rev) ### reverse landmark order

outlinebpatu$coo[which(outlinebpatu$Retouch.position=="Right")] <- lapply(outlinebpatu$coo[which(outlinebpatu$Retouch.position=="Right")], coo_flipy) ### flip outline
outlinebpatu$coo[which(outlinebpatu$Retouch.position=="Right")] <- lapply(outlinebpatu$coo[which(outlinebpatu$Retouch.position=="Right")], coo_rev) ### reverse landmark order

outlinebpatu2$coo[which(outlinebpatu2$Retouch.position=="Right")] <- lapply(outlinebpatu2$coo[which(outlinebpatu2$Retouch.position=="Right")], coo_flipy) ### flip outline
outlinebpatu2$coo[which(outlinebpatu2$Retouch.position=="Right")] <- lapply(outlinebpatu2$coo[which(outlinebpatu2$Retouch.position=="Right")], coo_rev) ### reverse landmark order

outlinebpNAT$coo[which(outlinebpNAT$Retouch.position=="Right")] <- lapply(outlinebpNAT$coo[which(outlinebpNAT$Retouch.position=="Right")], coo_flipy) ### flip outline
outlinebpNAT$coo[which(outlinebpNAT$Retouch.position=="Right")] <- lapply(outlinebpNAT$coo[which(outlinebpNAT$Retouch.position=="Right")], coo_rev) ### reverse landmark order

### STAGE 5: OUTLINE NORMALISATION ###

outlinebp     <- coo_centre(outlinebp) ### centre outlines
outlinebpatu  <- coo_centre(outlinebpatu) ### centre outlines
outlinebpatu2 <- coo_centre(outlinebpatu2) ### centre outlines
outlinebpNAT  <- coo_centre(outlinebpNAT) ### centre outlines
outlinetp     <- coo_centre(outlinetp) ### centre outlines
outlinetpatu  <- coo_centre(outlinetpatu) ### centre outlines
outlinetpatu2 <- coo_centre(outlinetpatu2) ### centre outlines
outlinetpNAT  <- coo_centre(outlinetpNAT) ### centre outlines

outlinebp     <- coo_scale(outlinebp) ### scale outlines
outlinebpatu  <- coo_scale(outlinebpatu) ### scale outlines
outlinebpatu2 <- coo_scale(outlinebpatu2) ### scale outlines
outlinebpNAT  <- coo_scale(outlinebpNAT) ### scale outlines
outlinetp     <- coo_scale(outlinetp) ### scale outlines
outlinetpatu  <- coo_scale(outlinetpatu) ### scale outlines
outlinetpatu2 <- coo_scale(outlinetpatu2) ### scale outlines
outlinetpNAT  <- coo_scale(outlinetpNAT) ### scale outlines

### STAGE 6: OUTLINE VISUALISATION ###

stack(outlinebp, title = "All BP Examples (n = 1670)") ### bp: visualisation (stack)
stack(outlinebpatu, title = "ATU BP Examples (n = 909)") ### bp: visualisation (stack)
stack(outlinebpatu2, title = "ATU2 BP Examples (n = 450)") ### bp: visualisation (stack)
stack(outlinebpNAT, title = "NAT BP Examples (n = 522)") ### bp: visualisation (stack)
stack(outlinetp, title = "All Examples TP (n = 571)") ### tp: visualisation (stack)
stack(outlinetpatu, title = "ATU Examples TP (n = 422)") ### tp: visualisation (stack)
stack(outlinetpatu2, title = "ATU2 Examples TP (n = 376)") ### tp: visualisation (stack)
stack(outlinetpNAT, title = "NAT Examples TP (n = 260)") ### tp: visualisation (stack)

panel(outlinebp) ### bp: visualisation (panel)
panel(outlinebpatu) ### bp: visualisation (panel)
panel(outlinebpatu2) ### bp: visualisation (panel)
panel(outlinebpNAT) ### bp: visualisation (panel)
panel(outlinetp) ### tp: visualisation (panel)
panel(outlinetpatu) ### tp: visualisation (panel)
panel(outlinetpatu2) ### tp: visualisation (panel)
panel(outlinetpNAT) ### tp: visualisation (panel)

### STAGE 7: HARMONIC CALIBRATION ###

calibrate_deviations_efourier(outlinebp) ### bp: harmonic check
calibrate_deviations_efourier(outlinebpatu) ### bp: harmonic check
calibrate_deviations_efourier(outlinebpatu2) ### bp: harmonic check
calibrate_deviations_efourier(outlinebpNAT) ### bp: harmonic check
calibrate_deviations_efourier(outlinetp) ### tp: harmonic check
calibrate_deviations_efourier(outlinetpatu) ### tp: harmonic check
calibrate_deviations_efourier(outlinetpatu2) ### tp: harmonic check
calibrate_deviations_efourier(outlinetpNAT) ### tp: harmonic check

calibrate_harmonicpower_efourier(outlinebp) ### bp: harmonic check
calibrate_harmonicpower_efourier(outlinebpatu) ### bp: harmonic check
calibrate_harmonicpower_efourier(outlinebpatu2) ### bp: harmonic check
calibrate_harmonicpower_efourier(outlinebpNAT) ### bp: harmonic check
calibrate_harmonicpower_efourier(outlinetp) ### tp: harmonic check
calibrate_harmonicpower_efourier(outlinetpatu) ### tp: harmonic check
calibrate_harmonicpower_efourier(outlinetpatu2) ### tp: harmonic check
calibrate_harmonicpower_efourier(outlinetpNAT) ### tp: harmonic check

calibrate_reconstructions_efourier(outlinebp) ### bp: harmonic check
calibrate_reconstructions_efourier(outlinebpatu) ### bp: harmonic check
calibrate_reconstructions_efourier(outlinebpatu2) ### bp: harmonic check
calibrate_reconstructions_efourier(outlinebpNAT) ### bp: harmonic check
calibrate_reconstructions_efourier(outlinetp) ### tp: harmonic check
calibrate_reconstructions_efourier(outlinetpatu) ### tp: harmonic check
calibrate_reconstructions_efourier(outlinetpatu2) ### tp: harmonic check
calibrate_reconstructions_efourier(outlinetpNAT) ### tp: harmonic check

### STAGE 8: EFOURIER FILE CREATION ###

efourierbp <- efourier(outlinebp, nb.h = 24,  norm = TRUE) ### bp: 24 harmonics for 99.9%
efourierbpatu <- efourier(outlinebpatu, nb.h = 24,  norm = TRUE) ### bp: 24 harmonics for 99.9%
efourierbpatu2 <- efourier(outlinebpatu2, nb.h = 26,  norm = TRUE) ### bp: 26 harmonics for 99.9%
efourierbpNAT <- efourier(outlinebpNAT, nb.h = 24,  norm = TRUE) ### bp: 24 harmonics for 99.9%
efouriertp <- efourier(outlinetp, nb.h = 31, norm = TRUE) ### tp: 31 harmonics for 99.9%
efouriertpatu <- efourier(outlinetpatu, nb.h = 31,  norm = TRUE) ### tp: 31 harmonics for 99.9%
efouriertpatu2 <- efourier(outlinetpatu2, nb.h = 31,  norm = TRUE) ### tp: 31 harmonics for 99.9%
efouriertpNAT <- efourier(outlinetpNAT, nb.h = 31,  norm = TRUE) ### tp: 31 harmonics for 99.9%

### STAGE 9: PCA FILE CREATION ###

pcabp <- PCA(efourierbp) ### bp: creation of PCA
pcabpatu <- PCA(efourierbpatu) ### bp: creation of PCA
pcabpatu2 <- PCA(efourierbpatu2) ### bp: creation of PCA
pcabpNAT <- PCA(efourierbpNAT) ### bp: creation of PCA
pcatp <- PCA(efouriertp) ### tp: creation of PCA
pcatpatu <- PCA(efouriertpatu) ### tp: creation of PCA
pcatpatu2 <- PCA(efouriertpatu2) ### tp: creation of PCA
pcatpNAT <- PCA(efouriertpNAT) ### tp: creation of PCA

### STAGE 10: PCA VISUALISATION ###

PCcontrib(pcabp)
PCcontrib(pcabpatu)
PCcontrib(pcabpatu2)
PCcontrib(pcabpNAT)
PCcontrib(pcatp)
PCcontrib(pcatpatu)
PCcontrib(pcatpatu2)
PCcontrib(pcatpNAT)

plot(pcabp, xax = 1, yax = 2, pos.shp = "XY", rug = FALSE, zoom = 1, lwd.shp = 1, size.shp = 0.5, amp.shp = 0.5, title = "")
plot(pcabpatu, xax = 1, yax = 2, pos.shp = "XY", rug = FALSE, zoom = 1, lwd.shp = 1, size.shp = 0.5, amp.shp = 0.5, title = "")
plot(pcabpatu2, xax = 1, yax = 2, pos.shp = "XY", rug = FALSE, zoom = 1, lwd.shp = 1, size.shp = 0.5, amp.shp = 0.5, title = "")
plot(pcabpNAT, xax = 1, yax = 2, pos.shp = "XY", rug = FALSE, zoom = 1, lwd.shp = 1, size.shp = 0.5, amp.shp = 0.5, title = "")
plot(pcatp, xax = 1, yax = 2, pos.shp = "XY", rug = FALSE, zoom = 1, lwd.shp = 1, size.shp = 0.5, amp.shp = 0.5, title = "") 
plot(pcatpatu, xax = 1, yax = 2, pos.shp = "XY", rug = FALSE, zoom = 1, lwd.shp = 1, size.shp = 0.5, amp.shp = 0.5, title = "")
plot(pcatpatu2, xax = 1, yax = 2, pos.shp = "XY", rug = FALSE, zoom = 1, lwd.shp = 1, size.shp = 0.5, amp.shp = 0.5, title = "")
plot(pcatpNAT, xax = 1, yax = 2, pos.shp = "XY", rug = FALSE, zoom = 1, lwd.shp = 1, size.shp = 0.5, amp.shp = 0.5, title = "")

plot(pcabp, xax = 1, yax = 2, fac = pcabp$ATU, rug = FALSE, title = "", points = FALSE, zoom = 2, morphospace = FALSE, chull = FALSE, chull.filled = FALSE, morphospaces = FALSE, ellipses = TRUE, conf.ellipses = 0.66) 
plot(pcabp, xax = 1, yax = 2, fac = pcabp$ATU2, rug = FALSE, title = "", points = FALSE, rect.labelsgroups = TRUE, zoom = 2.5, cex.labelsgroups = 0.9, morphospace = TRUE, size.shp = 0.5, nr.shp = 8, chull = FALSE, chull.filled = FALSE, ellipses = TRUE, conf.ellipses = 0.33)
plot(pcabp, xax = 1, yax = 2, fac = pcabp$NAT, rug = FALSE, title = "", points = FALSE, zoom = 2, morphospace = TRUE, size.shp = 0.5, nr.shp = 8, chull = FALSE, chull.filled = FALSE, morphospaces = FALSE, ellipses = TRUE, conf.ellipses = 0.33)
plot(pcatp, xax = 1, yax = 2, fac = pcatp$ATU, rug = FALSE, title = "", points = FALSE, zoom = 4, morphospace = FALSE, chull = FALSE, chull.filled = FALSE, morphospaces = FALSE, ellipses = TRUE, conf.ellipses = 0.66) 
plot(pcatp, xax = 1, yax = 2, fac = pcatp$ATU2, rug = FALSE, title = "", points = FALSE, rect.labelsgroups = TRUE, zoom = 2,  cex.labelsgroups = 0.9, morphospace = TRUE, size.shp = 0.5, nr.shp = 12, chull = FALSE, chull.filled = FALSE, ellipses = TRUE, conf.ellipses = 0.33)
plot(pcatp, xax = 1, yax = 2, fac = pcatp$NAT, rug = FALSE, title = "", points = FALSE, zoom = 2.5, morphospace = TRUE, size.shp = 0.5, nr.shp = 8, chull = FALSE, chull.filled = FALSE, morphospaces = FALSE, ellipses = TRUE, conf.ellipses = 0.33)

plot(pcabpatu, xax = 1, yax = 2, fac = pcabpatu$ATU, rug = FALSE, title = "", points = FALSE, zoom = 2, morphospace = FALSE, chull = FALSE, chull.filled = FALSE, morphospaces = FALSE, ellipses = TRUE, conf.ellipses = 0.66) 
plot(pcabpatu2, xax = 1, yax = 2, fac = pcabpatu2$ATU2, rug = FALSE, title = "", points = FALSE, zoom = 2, morphospace = FALSE, chull = FALSE, chull.filled = FALSE, morphospaces = FALSE, ellipses = TRUE, conf.ellipses = 0.66) 
plot(pcabpNAT, xax = 1, yax = 2, fac = pcabpNAT$NAT, rug = FALSE, title = "", points = FALSE, zoom = 2, morphospace = FALSE, chull = FALSE, chull.filled = FALSE, morphospaces = FALSE, ellipses = TRUE, conf.ellipses = 0.66) 
plot(pcatpatu, xax = 1, yax = 2, fac = pcatpatu$ATU, rug = FALSE, title = "", points = FALSE, zoom = 2, morphospace = FALSE, chull = FALSE, chull.filled = FALSE, morphospaces = FALSE, ellipses = TRUE, conf.ellipses = 0.66) 
plot(pcatpatu2, xax = 1, yax = 2, fac = pcatpatu2$ATU2, rug = FALSE, title = "", points = FALSE, zoom = 2, morphospace = FALSE, chull = FALSE, chull.filled = FALSE, morphospaces = FALSE, ellipses = TRUE, conf.ellipses = 0.66) 
plot(pcatpNAT, xax = 1, yax = 2, fac = pcatpNAT$NAT, rug = FALSE, title = "", points = FALSE, zoom = 2, morphospace = FALSE, chull = FALSE, chull.filled = FALSE, morphospaces = FALSE, ellipses = TRUE, conf.ellipses = 0.66) 

### STAGE 11: MANOVA AND PAIRWISE ANALYSES ###

MANOVA(pcabp, fac = pcabp$fac$ATU)     ### BP: MANOVA (ATU)
MANOVA(pcatp, fac = pcatp$fac$ATU)     ### TP: MANOVA (ATU)
MANOVA_PW(pcabp, fac = pcabp$fac$ATU)  ### BP-PW: MANOVA (ATU)
MANOVA_PW(pcatp, fac = pcatp$fac$ATU)  ### TP-PW: MANOVA (ATU)

MANOVA(pcabp, fac = pcabp$fac$ATU2)    ### BP: MANOVA (ATU2)
MANOVA(pcatp, fac = pcatp$fac$ATU2)    ### TP: MANOVA (ATU2)
MANOVA_PW(pcabp, fac = pcabp$fac$ATU2) ### BP-PW: MANOVA (ATU2)
MANOVA_PW(pcatp, fac = pcatp$fac$ATU2) ### TP-PW: MANOVA (ATU2)

MANOVA(pcabp, fac = pcabp$fac$NAT)     ### BP: MANOVA (NAT)
MANOVA(pcatp, fac = pcatp$fac$NAT)     ### TP: MANOVA (NAT)
MANOVA_PW(pcabp, fac = pcabp$fac$NAT)  ### BP-PW: MANOVA (NAT)
MANOVA_PW(pcatp, fac = pcatp$fac$NAT)  ### TP-PW: MANOVA (NAT)

MANOVA(pcabpatu, fac = pcabpatu$fac$ATU)       ### BP: MANOVA (ATU EXC UNSPECIFIED)
MANOVA(pcatpatu, fac = pcatpatu$fac$ATU)       ### TP: MANOVA (ATU EXC UNSPECIFIED)
MANOVA_PW(pcabpatu, fac = pcabpatu$fac$ATU)    ### BP-PW: MANOVA (ATU EXC UNSPECIFIED)
MANOVA_PW(pcatpatu, fac = pcatpatu$fac$ATU)    ### TP-PW: MANOVA (ATU EXC UNSPECIFIED)

MANOVA(pcabpatu2, fac = pcabpatu2$fac$ATU2)    ### BP: MANOVA (ATU2 EXC UNSPECIFIED)
MANOVA(pcatpatu2, fac = pcatpatu2$fac$ATU2)    ### TP: MANOVA (ATU2 EXC UNSPECIFIED)
MANOVA_PW(pcabpatu2, fac = pcabpatu2$fac$ATU2) ### BP-PW: MANOVA (ATU2 EXC UNSPECIFIED)
MANOVA_PW(pcatpatu2, fac = pcatpatu2$fac$ATU2) ### TP-PW: MANOVA (ATU2 EXC UNSPECIFIED)

MANOVA(pcabpNAT, fac = pcabpNAT$fac$NAT)      ### BP: MANOVA (NAT EXC UNSPECIFIED)
MANOVA(pcatpNAT, fac = pcatpNAT$fac$NAT)      ### TP: MANOVA (NAT EXC UNSPECIFIED)
MANOVA_PW(pcabpNAT, fac = pcabpNAT$fac$NAT)   ### BP-PW: MANOVA (NAT EXC UNSPECIFIED)
MANOVA_PW(pcatpNAT, fac = pcatpNAT$fac$NAT)   ### TP-PW: MANOVA (NAT EXC UNSPECIFIED)

### STAGE 12: FURTHER EXPLORATORY ANALYSIS ###

PCcontrib(pcabp)
PCcontrib(pcabpatu)
PCcontrib(pcabpatu2)
PCcontrib(pcabpNAT)
PCcontrib(pcatp)
PCcontrib(pcatpatu)
PCcontrib(pcatpatu2)
PCcontrib(pcatpNAT)

scree(pcabp, nax = 1:30) ### BP: SCREE TABLE
scree_plot(pcabp, nax = 1:30) ### BP: SCREE PLOT

scree(pcatp, nax = 1:30) ### TP: SCREE TABLE
scree_plot(pcatp, nax = 1:30) ### TP: SCREE PLOT

scree(pcabpatu, nax = 1:30) ### BP (ATU EXC UNSPECIFIED) SCREE TABLE 
scree_plot(pcabpatu, nax = 1:30) ### BP (ATU EXC UNSPECIFIED) SCREE PLOT

scree(pcatpatu, nax = 1:30) ### TP (ATU EXC UNSPECIFIED) SCREE TABLE 
scree_plot(pcatpatu, nax = 1:30) ### TP (ATU EXC UNSPECIFIED) SCREE PLOT 

scree(pcabpatu2, nax = 1:30) ### BP (ATU2 EXC UNSPECIFIED) SCREE TABLE 
scree_plot(pcabpatu2, nax = 1:30) ### BP (ATU2 EXC UNSPECIFIED) SCREE PLOT 

scree(pcatpatu2, nax = 1:30) ### TP (ATU2 EXC UNSPECIFIED) SCREE TABLE 
scree_plot(pcatpatu2, nax = 1:30) ### TP (ATU2 EXC UNSPECIFIED) SCREE PLOT 

scree(pcabpNAT, nax = 1:30) ### BP (NAT EXC UNSPECIFIED) SCREE TABLE
scree_plot(pcabpNAT, nax = 1:30) ### BP (NAT EXC UNSPECIFIED) SCREE PLOT

scree(pcatpNAT, nax = 1:30) ### TP (NAT EXC UNSPECIFIED) SCREE TABLE
scree_plot(pcatpNAT, nax = 1:30) ### TP (NAT EXC UNSPECIFIED) SCREE PLOT

### STAGE 13: RPHYLIP TREE CONSTRUCTION ###
### NOTE: FOR RCONTML TO WORK PHYLIP (FELSENSTEIN 1989, 2013) MUST ALSO BE INSTALLED ###
### SEE ATTACHED TREE FILES TO AVOID THE RCONTML PROCEDURE ### 

bppcamatrix <- as.matrix(pcabp$x[, 1:26])
tppcamatrix <- as.matrix(pcatp$x[, 1:30])
bppcaatumatrix <- as.matrix(pcabpatu$x[, 1:26])
tppcaatumatrix <- as.matrix(pcatpatu$x[, 1:29])
bppcaatu2matrix <- as.matrix(pcabpatu2$x[, 1:26])
tppcaatu2matrix <- as.matrix(pcatpatu2$x[, 1:29])
bppcaNATmatrix <- as.matrix(pcabpNAT$x[, 1:26])
tppcaNATmatrix <- as.matrix(pcatpNAT$x[, 1:27])

### bpml     <- Rcontml(bppcamatrix, path = NULL, global = FALSE, random.order = TRUE, random.addition = 1) ### BP: MAXIMUM LIKELIHOOD TREE
### tpml     <- Rcontml(tppcamatrix, path = NULL, global = FALSE, random.order = TRUE, random.addition = 1) ### TP: MAXIMUM LIKELIHOOD TREE
### bpatuml  <- Rcontml(bppcaatumatrix, path = NULL, global = FALSE, random.order = TRUE, random.addition = 1) ### BP (ATU EXCLUDING UNSPECIFIED: MAXIMUM LIKELIHOOD TREE
### tpatuml  <- Rcontml(tppcaatumatrix, path = NULL, global = FALSE, random.order = TRUE, random.addition = 1) ### TP (ATU EXCLUDING UNSPECIFIED: MAXIMUM LIKELIHOOD TREE
### bpatu2ml <- Rcontml(bppcaatu2matrix, path = NULL, global = FALSE, random.order = TRUE, random.addition = 1) ### BP (ATU2 EXCLUDING UNSPECIFIED: MAXIMUM LIKELIHOOD TREE
### tpatu2ml <- Rcontml(tppcaatu2matrix, path = NULL, global = FALSE, random.order = TRUE, random.addition = 1) ### TP (ATU2 EXCLUDING UNSPECIFIED: MAXIMUM LIKELIHOOD TREE
### bpNATml  <- Rcontml(bppcaNATmatrix, path = NULL, global = FALSE, random.order = TRUE, random.addition = 1) ### BP (NAT EXCLUDING UNSPECIFIED: MAXIMUM LIKELIHOOD TREE
### tpNATml  <- Rcontml(tppcaNATmatrix, path = NULL, global = FALSE, random.order = TRUE, random.addition = 1) ### TP (NAT EXCLUDING UNSPECIFIED: MAXIMUM LIKELIHOOD TREE

### Log-likelihood values ###
### bpml: 192788
### bpatuml: 103299
### bpatu2ml: 50459
### bpNATml: 59377
### tpml: 66990
### tpatuml: 49978
### tpatu2ml: 44487
### tpNATml: 28097

### write.tree(bpml, file = "bptree")
### write.tree(tpml, file = "tptree")
### write.tree(bpatuml, file = "bpatutree")
### write.tree(tpatuml, file = "tpatutree")
### write.tree(bpatu2ml, file = "bpatu2tree")
### write.tree(tpatu2ml, file = "tpatu2tree")
### write.tree(bpNATml, file = "bpNATtree")
### write.tree(tpNATml, file = "tpNATtree")

bpml <- read.tree(file = "bp_phylip/bptree")         ### FILE READ (ALTERNATIVE)
tpml <- read.tree(file = "tp_phylip/tptree")         ### FILE READ (ALTERNATIVE)
bpatuml <- read.tree(file = "bpatu_phylip/bpatutree")   ### FILE READ (ALTERNATIVE)
tpatuml <- read.tree(file = "tpatu_phylip/tpatutree")   ### FILE READ (ALTERNATIVE)
bpatu2ml <- read.tree(file = "bpatu2_phylip/bpatu2tree") ### FILE READ (ALTERNATIVE)
tpatu2ml <- read.tree(file = "tpatu2_phylip/tpatu2tree") ### FILE READ (ALTERNATIVE)
bpNATml <- read.tree(file = "bpNAT_phylip/bpNATtree")   ### FILE READ (ALTERNATIVE)
tpNATml <- read.tree(file = "tpNAT_phylip/tpNATtree")   ### FILE READ (ALTERNATIVE)

bpml <- reroot(bpml, 924)         ### REROOT (TO THE OLDEST SPECIMEN)
tpml <- reroot(tpml, )            ### REROOT (TO THE OLDEST SPECIMEN)
bpatuml <- reroot(bpatuml, 285)   ### REROOT (TO THE OLDEST SPECIMEN)
tpatuml <- reroot(tpatuml, XXX)   ### REROOT (TO THE OLDEST SPECIMEN)
bpatu2ml <- reroot(bpatu2ml, XXX) ### REROOT (TO THE OLDEST SPECIMEN)
tpatu2ml <- reroot(tpatu2ml, XXX) ### REROOT (TO THE OLDEST SPECIMEN)
bpNATml <- reroot(bpNATml, XXX)   ### REROOT (TO THE OLDEST SPECIMEN)
tpNATml <- reroot(tpNATml, XXX)   ### REROOT (TO THE OLDEST SPECIMEN)

### FOR A MORE EXTENSIVE ANALYSIS ALL SHAPES DEEMED TO BE FROM THE OLDEST HORIZON
### SHOULD BE EXPLORED (ITERATIVE VISUALISATION OF ALL POSSIBLE REROOTS)

### STAGE 14: RPHYLIP TREE VISUALISATION ###

ggtree(bpml)     ### GGTREE LAYOUT
ggtree(tpml)     ### GGTREE LAYOUT
ggtree(bpatuml)  ### GGTREE LAYOUT
ggtree(tpatuml)  ### GGTREE LAYOUT
ggtree(bpatu2ml) ### GGTREE LAYOUT
ggtree(tpatu2ml) ### GGTREE LAYOUT
ggtree(bpNATml)  ### GGTREE LAYOUT
ggtree(tpNATml)  ### GGTREE LAYOUT

ggtree(bpml) + theme_tree() + geom_nodepoint()
ggsave("images/bpml.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")
ggtree(tpml) + theme_tree() + geom_nodepoint() 
ggsave("images/tpml.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")
ggtree(bpatuml) + theme_tree() + geom_nodepoint()
ggsave("images/bpatuml.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")
ggtree(tpatuml) + theme_tree() + geom_nodepoint()
ggsave("images/tpatuml.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")
ggtree(bpatu2ml) + theme_tree() + geom_nodepoint()
ggsave("images/bpatu2ml.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")
ggtree(tpatu2ml) + theme_tree() + geom_nodepoint()
ggsave("images/tpatu2ml.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")
ggtree(bpNATml) + theme_tree() + geom_nodepoint()
ggsave("images/bpNAT.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")
ggtree(tpNATml) + theme_tree() + geom_nodepoint()
ggsave("images/tpNAT.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")

###############

bpmldata <- select(pcabp$fac, FN, Site, ATU, ATU2, NAT) %>% droplevels()
summary(bpmldata)
tpmldata <- select(pcatp$fac, FN, Site, ATU, ATU2, NAT) %>% droplevels()
summary(tpmldata)
bpatumldata <- select(pcabpatu$fac, FN, ATU) %>% droplevels()
summary(bpatumldata)
tpatumldata <- select(pcatpatu$fac, FN, ATU) %>% droplevels()
summary(tpatumldata)
bpatu2mldata <- select(pcabpatu2$fac, FN, ATU2) %>% droplevels()
summary(bpatu2mldata)
tpatu2mldata <- select(pcatpatu2$fac, FN, ATU2) %>% droplevels()
summary(tpatu2mldata)
bpNATmldata <- select(pcabpNAT$fac, FN, NAT) %>% droplevels()
summary(bpNATmldata)
tpNATmldata <- select(pcatpNAT$fac, FN, NAT) %>% droplevels()
summary(tpNATmldata)

ggtree(bpml) %<+% bpmldata + geom_tiplab(aes(colour = ATU, align = TRUE), size = 3) + theme_minimal()
ggtree(bpml) %<+% bpmldata + geom_tippoint(aes(colour = ATU)) + theme_minimal()
ggtree(bpml) %<+% bpmldata + geom_tiplab(aes(colour = ATU2, align = TRUE), size = 3) + theme_minimal()
ggtree(bpml) %<+% bpmldata + geom_tippoint(aes(colour = ATU2)) + theme_minimal()
ggtree(bpml) %<+% bpmldata + geom_tiplab(aes(colour = NAT, align = TRUE), size = 3) + theme_minimal()
ggtree(bpml) %<+% bpmldata + geom_tippoint(aes(colour = NAT)) + theme_minimal()

ggtree(tpml) %<+% tpmldata + geom_tiplab(aes(colour = ATU, align = TRUE), size = 3) + theme_minimal()
ggtree(tpml) %<+% tpmldata + geom_tippoint(aes(colour = ATU)) + theme_minimal()
ggtree(tpml) %<+% tpmldata + geom_tiplab(aes(colour = ATU2, align = TRUE), size = 3) + theme_minimal()
ggtree(tpml) %<+% tpmldata + geom_tippoint(aes(colour = ATU2)) + theme_minimal()
ggtree(tpml) %<+% tpmldata + geom_tiplab(aes(colour = NAT, align = TRUE), size = 3) + theme_minimal()
ggtree(tpml) %<+% tpmldata + geom_tippoint(aes(colour = NAT)) + theme_minimal()

ggtree(bpatuml) %<+% bpatumldata + geom_tiplab(aes(colour = ATU, align = TRUE), size = 3) + theme_minimal() + scale_color_manual(values = c("#D43F3A", "#EEA236", "#5BB85C", "#D8099C", "#46B8DA", "#357EBD", "#9632B8", "#767676", "#8F3931", "#340E20"))
ggtree(bpatuml) %<+% bpatumldata + geom_tippoint(aes(colour = ATU), size = 2) + theme_minimal() + scale_color_manual(values = c("#D43F3A", "#EEA236", "#5BB85C", "#D8099C", "#46B8DA", "#357EBD", "#9632B8", "#767676", "#8F3931", "#340E20"))
ggsave("images/bpatu_tree.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")

ggtree(bpatu2ml) %<+% bpatu2mldata + geom_tiplab(aes(colour = ATU2, align = TRUE), size = 3) + theme_minimal()
ggtree(bpatu2ml) %<+% bpatu2mldata + geom_tippoint(aes(colour = ATU2)) + theme_minimal()
ggsave("images/bpatu2_tree.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")

ggtree(bpNATml) %<+% bpNATmldata + geom_tiplab(aes(colour = NAT, align = TRUE), size = 3) + theme_minimal()
ggtree(bpNATml) %<+% bpNATmldata + geom_tippoint(aes(colour = NAT)) + theme_minimal()
ggsave("images/bpNAT_tree.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")

ggtree(tpatuml) %<+% tpatumldata + geom_tiplab(aes(colour = ATU, align = TRUE), size = 3) + theme_minimal() + scale_color_manual(values = c("#D43F3A", "#EEA236", "#5BB85C", "#D8099C", "#46B8DA", "#357EBD", "#9632B8", "#767676"))
ggtree(tpatuml) %<+% tpatumldata + geom_tippoint(aes(colour = ATU)) + theme_minimal() + scale_color_manual(values = c("#D43F3A", "#EEA236", "#5BB85C", "#D8099C", "#46B8DA", "#357EBD", "#9632B8", "#767676"))
ggsave("images/tpatu_tree.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")

ggtree(tpatu2ml) %<+% tpatu2mldata + geom_tiplab(aes(colour = ATU2, align = TRUE), size = 3) + theme_minimal()
ggtree(tpatu2ml) %<+% tpatu2mldata + geom_tippoint(aes(colour = ATU2)) + theme_minimal()
ggsave("images/tpatu2_tree.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")

ggtree(tpNATml) %<+% tpNATmldata + geom_tiplab(aes(colour = NAT, align = TRUE), size = 3) + theme_minimal()
ggtree(tpNATml) %<+% tpNATmldata + geom_tippoint(aes(colour = NAT)) + theme_minimal() + scale_color_manual(values=c("#a6cee3","#b2df8a","#33a02c","#fb9a99","#1f78b4","#e31a1c","#fdbf6f","#ff7f00","#6a3d9a","#ffff99","#cab2d6")) 
ggsave("images/tpNAT_tree.tiff", plot = last_plot(), dpi = 300, height = 250, width = 300, units = "mm")

#####



ggtree(tpml) %<+% tpmldata + geom_point2(aes(shape = ATU, subset=(ATU %in% "Tanged Point Technocomplex")), size = 2, colour = "#bd284c") + labs(title = "ATU: Tanged Point Technocomplex")
ggtree(tpml) %<+% tpmldata + geom_point2(aes(shape = NAT, subset=(NAT %in% "Bromme Point")), size = 2, colour = "#28bd33") + labs(title = "NAT: Bromme Point")
ggtree(bpml) %<+% bpmldata + geom_point2(aes(shape = NAT, subset=(NAT %in% "Appelscha Point")), size = 2, colour = "#a6cee3") + labs(title = "NAT: Appelscha Point")

tp1 <- ggtree(tpml) %<+% tpmldata + geom_point2(aes(shape = ATU, subset=(ATU %in% "Bromme")), size = 2, colour = "#ed980d") + labs(title = "Bromme")
tp2 <- ggtree(tpml) %<+% tpmldata + geom_point2(aes(shape = ATU, subset=(ATU %in% "Upper Magdalenian")), size = 2, colour = "#ed980d") + labs(title = "Upper Magdalenian")
tp3 <- ggtree(tpml) %<+% tpmldata + geom_point2(aes(shape = ATU, subset=(ATU %in% "Ahrensburgian")), size = 2, colour = "#ed980d") + labs(title = "Ahrensburgian")
tp4 <- ggtree(tpml) %<+% tpmldata + geom_point2(aes(shape = ATU, subset=(ATU %in% "Lyngby")), size = 2, colour = "#ed980d") + labs(title = "Lyngby")
tp5 <- ggtree(tpml) %<+% tpmldata + geom_point2(aes(shape = ATU, subset=(ATU %in% "Federmesser")), size = 2, colour = "#ed980d") + labs(title = "Federmesser")
tp6 <- ggtree(tpml) %<+% tpmldata + geom_point2(aes(shape = ATU, subset=(ATU %in% "Azilian")), size = 2, colour = "#ed980d") + labs(title = "Azilian")
tp7 <- ggtree(tpml) %<+% tpmldata + geom_point2(aes(shape = ATU, subset=(ATU %in% "Unspecified")), size = 2, colour = "#ed980d") + labs(title = "Unspecified")

multiplot(tp1, tp2, tp3, tp4,tp5, tp6, tp7, ncol = 2) ### multiplot of ATU

bpml2 <- rename_taxa(bpml, pcabp$fac, FN, Site) ### RENAMING TIP LABELS (FN TO SITE)
tpml2 <- rename_taxa(tpml, pcatp$fac, FN, Site) ### RENAMING TIP LABELS (FN TO SITE)
ggtree(bpml2) + geom_tiplab(size = 1)
ggtree(tpml2) + geom_tiplab(size = 1.5)

### STAGE 15: TANGLEGRAM FORMATION ###

plot_grid(ggtree(bpml), ggtree(tpml), ncol=2, labels=c("BP", "TP"))

#### METHOD #1: COPHYLO FUNCTION (PHYTOOLS)
cmatrix <- inner_join(bpmldata, tpmldata, by = "Site")
cmatrix <- select(cmatrix, ArtefactBP = FN.x, ArtefactTP = FN.y)
cophyloml <- cophylo(bpml, tpml, assoc = cmatrix, rotate = TRUE)
plot(cophyloml)

#################################################################

### OTHER FUNCTIONS ###

### FILTERING BY A PARTICULAR ARTEFACT TYPE OR ORDER ###

malauriepoints <- filter(outlinebp, Named_Artefact_Type=="Malaurie Point")
germanypoints <- filter(outlinebp, Country=="Germany")

### FILTERING BY QUANTITY ###

outlinebp <- at_least(outlinebp, "ATU", 20) ### groups with at least 20 sample in ATU

### SELF-ORGANISING MAPS ###

grid.som <- somgrid(xdim = 8, ydim = 8, topo = "hexagonal")
bpx <- scale(pcabp$x)
tpx <- scale(pcatp$x)
summary(bpx)
summary(tpx)

set.seed(222)
bpmap <- som(bpx, grid = grid.som, alpha = c(0.05, 0.01), radius = 1)
bpmap
plot(bpmap, type = "changes")
plot(bpmap,  main = "Self Organising Map (SOM) of PC variance")
plot(bpmap, type = "count") # counts in each node
plot(bpmap, main = "Self Organising Map (SOM) of PC variance", type = "mapping") # counts in each node

tpmap <- som(tpx, grid = grid.som, alpha = c(0.05, 0.01), radius = 1)
tpmap
plot(tpmap, type = "changes")
plot(tpmap,  main = "Self Organising Map (SOM) of PC variance")
plot(tpmap, type = "count") # counts in each node
plot(tpmap, main = "Self Organising Map (SOM) of PC variance", type = "mapping") # counts in each node

### Making a holotype (case study) tree

### Extract all examples, combine as an out, extract mean shapes and then tree
### Bromme Point	                    Bromme
### Ahrensburgian Point	              Stellmoor
### Hamburgian (Classic) Point	      Meindorf
### Hamburgian (Havelte) Point	      Havelte Holtingerzand
### Grensk Point	                    Grensk
### Creswellian Point	                Siegerswoude
### Cheddar Point	                    Aveline's Hole
### Federmesser Point	                Stoksbjerg Vest
### Lyngby Point	                    Nørre Lyngby
### Appelscha Point 	                Appelscha
### Azilian Point	                    Les Chaloignes
### Blanchère Point	                  Blanchères
### Kalbe Point	                      Berlin-Tegel
### Malaurie Point	                  Champ-Chalatras
### Petersfels Point	                Borneck-Bornwisch
### Ranis Point	                      Wehlen
### Taucha Point	                    Taucha
### Tjonger Point	                    Usselo
### Westerbeck Point	                Westerbeck
### Zonhoven Point	                  Zonhoven-Molenheide
### Altinovo Point	                  Borovka
### Krasnosillya Point	              Krasnosillya
### Teyjat Point	                    Bois-Ragot

Hamburgian <- import_tps("TPS_Hamburgian.tps", curves = TRUE) ### import .tps file
hamburgian.points <- read.csv("hamburgian.points.csv", header = TRUE, sep = ",", encoding = "UTF-8", row.names = 1)
Hamburgian <- Out(Hamburgian$coo, fac = hamburgian.points)

Bro  <- filter(outlinetp, Site=="Bromme")
Bro  <- select(Bro, Site) %>% mutate(Holotype= "Bromme Point")
Ahr  <- filter(outlinetp, Site=="Stellmoor")
Ahr  <- select(Ahr, Site) %>% mutate(Holotype= "Ahrensburgian Point")
Gre  <- filter(outlinetp, Site=="Grensk")
Gre  <- select(Gre, Site) %>% mutate(Holotype= "Grensk Point")
Cre  <- filter(outlinebp, Site=="Siegerswoude")
Cre  <- select(Cre, Site) %>% mutate(Holotype= "Creswellian Point")
Che  <- filter(outlinebp, Site=="Aveline's Hole")
Che  <- select(Che, Site) %>% mutate(Holotype= "Cheddar Point")
Fed  <- filter(outlinebp, Site=="Stoksbjerg Vest")
Fed  <- select(Fed, Site) %>% mutate(Holotype= "Federmesser Point")
HamC <- filter(Hamburgian, Site=="Meindorf")
HamC <- select(HamC, Site) %>% mutate(Holotype= "Hamburgian (Classic) Point")
HamH <- filter(Hamburgian, Site=="Havelte Holtingerzand")
HamH <- select(HamH, Site) %>% mutate(Holotype= "Hamburgian (Havelte) Point")
Lyn  <- filter(outlinetp, Site=="Nørre Lyngby")
Lyn  <- select(Lyn, Site) %>% mutate(Holotype= "Lyngby Point")
App  <- filter(outlinebp, Site=="Appelscha")
App  <- select(App, Site) %>% mutate(Holotype= "Appelscha Point")
Azi  <- filter(outlinebp, Site=="Les Chaloignes")
Azi  <- select(Azi, Site) %>% mutate(Holotype= "Azillian Point")
Bla  <- filter(outlinebp, Site=="Blanchères")
Bla  <- select(Bla, Site) %>% mutate(Holotype= "Blanchere Point")
Mal  <- filter(outlinebp, Site=="Champ-Chalatras")
Mal  <- select(Kab, Site) %>% mutate(Holotype= "Malarie Point")
Pet  <- filter(outlinebp, Site=="Borneck-Bornwisch")
Pet  <- select(Pet, Site) %>% mutate(Holotype= "Petersfels Point")
Ran  <- filter(outlinebp, Site=="Wehlen")
Ran  <- select(Ran, Site) %>% mutate(Holotype= "Ranis Point")
Tau  <- filter(outlinebp, Site=="Taucha")
Tau  <- select(Tau, Site) %>% mutate(Holotype= "Taucha Point")
Tjo  <- filter(outlinebp, Site=="Usselo")
Tjo  <- select(Tjo, Site) %>% mutate(Holotype= "Tjonger Point")
Wes  <- filter(outlinebp, Site=="Westerbeck")
Wes  <- select(Wes, Site) %>% mutate(Holotype= "Westerbeck Point")
Zon  <- filter(outlinebp, Site=="Zonhoven-Molenheide")
Zon  <- select(Zon, Site) %>% mutate(Holotype= "Zonhoven Point")
Alt  <- filter(outlinetp, Site=="Borovka")
Alt  <- select(Alt, Site) %>% mutate(Holotype= "Altinovo Point")
Kra  <- filter(outlinetp, Site=="Krasnosillya")
Kra  <- select(Kra, Site) %>% mutate(Holotype= "Krasnosillya Point")
Tey  <- filter(outlinetp, Site=="Bois-Ragot")
Tey  <- select(Tey, Site) %>% mutate(Holotype= "Teyjat Point")

holo <- combine(Tey, Fed, Che, Ahr, Bro, Cre, Kra, Alt, Zon, Wes, Tjo, Tau, Ran, Pet, Mal, Bla, Azi, App, Lyn, HamC, HamH)
holo$fac$Holotype<-as.factor(holo$fac$Holotype)
holo <- coo_centre(holo)
holo <- coo_scale(holo)
calibrate_harmonicpower_efourier(holo)
efourierholo <- efourier(holo, nb.h = 27, norm = TRUE)

PCAholo <- PCA(efourierholo, center = TRUE, fac = Holotype)
holomeans <- mshapes(PCAholo, fac = "Holotype")
holomatrix <- holomeans$x
holoml  <- Rcontml(holomatrix, path = NULL, global = TRUE, random.order = TRUE, random.addition = 50) 
### write.tree(holoml, file = "holomltree")
### holoml <- read.tree(file = "holo_phylip/holomltree")
ggtree(holoml) + geom_tiplab(size = 3.5)

