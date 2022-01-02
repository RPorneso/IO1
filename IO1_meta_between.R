#### PART 0. PRE-PROCESSING.

## clear environment

rm(list = ls())

## clear console

cat("\f") 

## install package and library

install.packages("readxl")
install.packages("lme4")
install.packages("dplyr")
install.packages("metafor")

library(metafor)
library(lme4)
library(readxl)
library(dplyr)

#### PART 1. EXPERIENTIAL LEARNING EFFECT ON SOFT SKILLS (BET SUBJ)

## load multiple excel sheets

for (i in 1:10){
  outcome <- read_excel("Desktop/ralph/EL_BETGRPS.xlsx",i)
  assign(paste0("outcome",i),outcome)
}

rm(outcome)

## perform meta-analysis for each outcome variable

outcome1 <- escalc(measure="SMDH", n1i=n_exp, n2i=n_cont, m1i=mean_exp, m2i=mean_cont, 
                   sd1i=sd_exp, sd2i=sd_cont, data=outcome1)
res1 <- rma(yi, vi, data=outcome1)
resmv1 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome1)

outcome2 <- escalc(measure="SMDH", n1i=n_exp, n2i=n_cont, m1i=mean_exp, m2i=mean_cont, 
                   sd1i=sd_exp, sd2i=sd_cont, data=outcome2)
res2 <- rma(yi, vi, data=outcome2)
resmv2 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome2)

outcome3 <- escalc(measure="SMDH", n1i=n_exp, n2i=n_cont, m1i=mean_exp, m2i=mean_cont, 
                   sd1i=sd_exp, sd2i=sd_cont, data=outcome3)
res3 <- rma(yi, vi, data=outcome3)
resmv3 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome3)

outcome4 <- escalc(measure="SMDH", n1i=n_exp, n2i=n_cont, m1i=mean_exp, m2i=mean_cont, 
                   sd1i=sd_exp, sd2i=sd_cont, data=outcome4)
res4 <- rma(yi, vi, data=outcome4)
resmv4 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome4)

outcome5 <- escalc(measure="SMDH", n1i=n_exp, n2i=n_cont, m1i=mean_exp, m2i=mean_cont, 
                   sd1i=sd_exp, sd2i=sd_cont, data=outcome5)
res5 <- rma(yi, vi, data=outcome5)
resmv5 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome5)

outcome6 <- escalc(measure="SMDH", n1i=n_exp, n2i=n_cont, m1i=mean_exp, m2i=mean_cont, 
                   sd1i=sd_exp, sd2i=sd_cont, data=outcome6)
res6 <- rma(yi, vi, data=outcome6)
resmv6 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome6)

##effect size per study (update to show all when required)

round(aggregate(yi ~ Study, data=outcome1, FUN=function(x) c(mean=mean(x), 
      sd=sd(x), min=min(x), max=max(x))), 3)
effsizestudy <- round(aggregate(yi ~ Study, data=outcome1, 
                                FUN=function(x) c(mean=mean(x), 
                                                  sd=sd(x), min=min(x), 
                                                  max=max(x))), 3)
effsizestudy

## forest plot with text

dd <- c(0,diff(outcome1$Study))
dd[dd > 0] <- 1
rows <- (1:res1$k) + cumsum(dd)
par(tck=-.02)
forest(resmv1, xlim=c(-5,5), ylim=c(-3,11),
       ilab=cbind(formatC(outcome1$mean_cont,digits=2,format='f'), 
                  formatC(outcome1$mean_exp,digits=2,format='f')),
       ilab.xpos=c(-1.9,-1.2),
       slab=paste(outcome1$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-1.9), 10.1, cex = 0.8, bquote(bold("Pre")))
text(c(-1.2), 10.1, cex = 0.8, bquote(bold("Post")))
text(-5, -2.1, pos=4, cex=0.75, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv1$QE, digits=2, format="f")), 
        ", df = ", .(res1$k - res1$p),", p = ", 
        .(formatC(resmv1$QEp, digits=2, format="f")),
        "; "))))
text(-5, -3.1, pos=4, cex=0.75, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res1$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res1$tau2, digits=2, format="f"))))))
title("Experiential Learning on Analytical Skills (bet. Subjects)")


dd <- c(0,diff(outcome2$Study))
dd[dd > 0] <- 1
rows <- (1:res2$k) + cumsum(dd)
par(tck=-.02)
forest(resmv2, xlim=c(-7,8.5), ylim=c(-3,7),
       ilab=cbind(formatC(outcome2$mean_cont,digits=2,format='f'), 
                  formatC(outcome2$mean_exp,digits=2,format='f')),
       ilab.xpos=c(-1.9,-1),
       slab=paste(outcome2$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-1.9), 6.1, cex = 0.8, bquote(bold("Pre")))
text(c(-1), 6.1, cex = 0.8, bquote(bold("Post")))
text(-7, -2.1, pos=4, cex=0.75, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv2$QE, digits=2, format="f")), 
        ", df = ", .(res2$k - res2$p),", p = ", 
        .(formatC(resmv2$QEp, digits=2, format="f")),
        "; "))))
text(-7, -3.1, pos=4, cex=0.75, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res2$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res2$tau2, digits=2, format="f"))))))
title("Experiential Learning on Communication (bet. Subjects)")

dd <- c(0,diff(outcome3$Study))
dd[dd > 0] <- 1
rows <- (1:res3$k) + cumsum(dd)
par(tck=-.02)
forest(resmv3, xlim=c(-7,7), ylim=c(-3,6),
       ilab=cbind(formatC(outcome3$mean_cont,digits=2,format='f'), 
                  formatC(outcome3$mean_exp,digits=2,format='f')),
       ilab.xpos=c(-2.9,-2),
       slab=paste(outcome3$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-2.9), 5.1, cex = 0.85, bquote(bold("Pre")))
text(c(-2), 5.1, cex = 0.85, bquote(bold("Post")))
text(-7, -2.1, pos=4, cex=0.75, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv3$QE, digits=2, format="f")), 
        ", df = ", .(res3$k - res3$p),", p = ", 
        .(formatC(resmv3$QEp, digits=2, format="f")),
        "; "))))
text(-7, -3.1, pos=4, cex=0.75, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res3$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res3$tau2, digits=2, format="f"))))))
title("Experiential Learning on Flexibility (bet. Subjects)")

dd <- c(0,diff(outcome4$Study))
dd[dd > 0] <- 1
rows <- (1:res4$k) + cumsum(dd)
par(tck=-.02)
forest(resmv4, xlim=c(-10,10), ylim=c(-3,10),
       ilab=cbind(formatC(outcome4$mean_cont,digits=2,format='f'), 
                  formatC(outcome4$mean_exp,digits=2,format='f')),
       ilab.xpos=c(-3.9,-2.7),
       slab=paste(outcome4$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-3.9), 9.1, cex = 0.85, bquote(bold("Pre")))
text(c(-2.7), 9.1, cex = 0.85, bquote(bold("Post")))
text(-10, -2.1, pos=4, cex=0.75, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv4$QE, digits=2, format="f")), 
        ", df = ", .(res4$k - res4$p),", p = ", 
        .(formatC(resmv4$QEp, digits=2, format="f")),
        "; "))))
text(-10, -3.1, pos=4, cex=0.75, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res4$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res4$tau2, digits=2, format="f"))))))
title("Experiential Learning on Organizational Skills (bet. Subjects)",
      cex.main=1)

dd <- c(0,diff(outcome5$Study))
dd[dd > 0] <- 1
rows <- (1:res5$k) + cumsum(dd)
par(tck=-.02)
forest(resmv5, xlim=c(-7,6), ylim=c(-3,7),
       ilab=cbind(formatC(outcome5$mean_cont,digits=2,format='f'), 
                  formatC(outcome5$mean_exp,digits=2,format='f')),
       ilab.xpos=c(-2.7,-1.8),
       slab=paste(outcome5$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-2.7), 6.1, cex = 0.85, bquote(bold("Pre")))
text(c(-1.8), 6.1, cex = 0.85, bquote(bold("Post")))
text(-7, -2.1, pos=4, cex=0.75, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv5$QE, digits=2, format="f")), 
        ", df = ", .(res5$k - res5$p),", p = ", 
        .(formatC(resmv5$QEp, digits=2, format="f")),
        "; "))))
text(-7, -3.1, pos=4, cex=0.75, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res5$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res5$tau2, digits=2, format="f"))))))
title("Experiential Learning on Personal Skills (bet. Subjects)")

dd <- c(0,diff(outcome6$Study))
dd[dd > 0] <- 1
rows <- (1:res6$k) + cumsum(dd)
par(tck=-.02)
forest(resmv6, xlim=c(-32,35), ylim=c(-3,8),
       ilab=cbind(formatC(outcome6$mean_cont,digits=2,format='f'), 
                  formatC(outcome6$mean_exp,digits=2,format='f')),
       ilab.xpos=c(-11.5,-6.8),
       slab=paste(outcome6$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-11.5), 7.1, cex = 0.85, bquote(bold("Pre")))
text(c(-6.8), 7.1, cex = 0.85, bquote(bold("Post")))
text(-32, -2.1, pos=4, cex=0.75, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv6$QE, digits=2, format="f")), 
        ", df = ", .(res6$k - res6$p),", p = ", 
        .(formatC(resmv6$QEp, digits=2, format="f")),
        "; "))))
text(-32, -3.1, pos=4, cex=0.75, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res6$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res6$tau2, digits=2, format="f"))))))
title("Experiential Learning on Social Skills (bet. Subjects)")
