#### PART 0. PRE-PROCESSING.

## clear environment

rm(list = ls())

## clear console

cat("\f") 

## install package and library

install.packages("readxl")
install.packages("metafor")
install.packages("lme4")
install.packages("dplyr")

library(metafor)
library(lme4)
library(readxl)
library(dplyr)


#### PART 1. EXPERIENTIAL LEARNING EFFECT ON SOFT SKILLS (WITHIN SUBJ)

## load multiple excel sheets

for (i in 1:10){
  outcome <- read_excel("Desktop/ralph/EL_on_Life_Skills_within_subj.xlsx",i)
  assign(paste0("outcome",i),outcome)
}

## perform meta-analysis for each outcome variable

outcome1 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                   ri=ri, ni=ni, data=outcome1)
res <- rma(yi, vi, data=outcome1)
resmv <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome1)

outcome2 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                   ri=ri, ni=ni, data=outcome2)
res2 <- rma(yi, vi, data=outcome2)
resmv2 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome2)

outcome3 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                   ri=ri, ni=ni, data=outcome3)
res3 <- rma(yi, vi, data=outcome3)
resmv3 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome3)

outcome3 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                   ri=ri, ni=ni, data=outcome3)
res3 <- rma(yi, vi, data=outcome3)
resmv3 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome3)

outcome4 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                   ri=ri, ni=ni, data=outcome4)
res4 <- rma(yi, vi, data=outcome4)
resmv4 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome4)

outcome5 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                   ri=ri, ni=ni, data=outcome5)
res5 <- rma(yi, vi, data=outcome5)
resmv5 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome5)

outcome6 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                   ri=ri, ni=ni, data=outcome6)
res6 <- rma(yi, vi, data=outcome6)
resmv6 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome6)

outcome7 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                   ri=ri, ni=ni, data=outcome7)
res7 <- rma(yi, vi, data=outcome7)
resmv7 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome7)

outcome8 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                   ri=ri, ni=ni, data=outcome8)
res8 <- rma(yi, vi, data=outcome8)
resmv8 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome8)

outcome9 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                   ri=ri, ni=ni, data=outcome9)
res9 <- rma(yi, vi, data=outcome9)
resmv9 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome9)

outcome10 <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
                    ri=ri, ni=ni, data=outcome10)
res10 <- rma(yi, vi, data=outcome10)
resmv10 <- rma.mv(yi, vi, random = ~ 1 | Study, data=outcome10)

## forest plots with text

dd <- c(0,diff(outcome2$Study))
dd[dd > 0] <- 1
rows <- (1:res2$k) + cumsum(dd)
par(tck=-.02)
forest(resmv2,xlim=c(-5,4), ylim=c(-3,12),
       ilab=cbind(formatC(outcome2$m_pre,digits=2,format='f'), 
                  formatC(outcome2$m_post,digits=2,format='f')),
       ilab.xpos=c(-1.9,-1.2),
       slab=paste(outcome2$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-1.9), 11.1, cex = 0.8, bquote(bold("Pre")))
text(c(-1.2), 11.1, cex = 0.8, bquote(bold("Post")))
text(-5, -2.1, pos=4, cex=0.75, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv2$QE, digits=2, format="f")), 
        ", df = ", .(res2$k - res2$p),", p = ", 
        .(formatC(resmv2$QEp, digits=2, format="f")),
        "; "))))
text(-5, -3.1, pos=4, cex=0.75, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res2$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res2$tau2, digits=2, format="f"))))))
title("Effect of Experiential Learning on Career-related Skills")

dd <- c(0,diff(outcome3$Study))
dd[dd > 0] <- 1
rows <- (1:res3$k) + cumsum(dd)
par(tck=-.02)
forest(resmv3,xlim=c(-5,3.5), ylim=c(-3,21),
       ilab=cbind(formatC(outcome3$m_pre,digits=2,format='f'), 
                  formatC(outcome3$m_post,digits=2,format='f')),
       ilab.xpos=c(-1.9,-1.2),
       slab=paste(outcome3$Authors),cex=0.65, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-1.9), 20.2, cex = 0.65, bquote(bold("Pre")))
text(c(-1.2), 20.2, cex = 0.65, bquote(bold("Post")))
text(-5, -2.1, pos=4, cex=0.65, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv3$QE, digits=2, format="f")), 
        ", df = ", .(res3$k - res3$p),", p = ", 
        .(formatC(resmv3$QEp, digits=2, format="f")),
        "; "))))
text(-5, -3.1, pos=4, cex=0.65, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res3$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res3$tau2, digits=2, format="f"))))))
title("Effect of Experiential Learning on Communication Skills")

dd <- c(0,diff(outcome4$Study))
dd[dd > 0] <- 1
rows <- (1:res4$k) + cumsum(dd)
par(tck=-.02)
forest(resmv4, xlim=c(-7.5,4), ylim=c(-3,6),
       ilab=cbind(formatC(outcome4$m_pre,digits=2,format='f'), 
                  formatC(outcome4$m_post,digits=2,format='f')),
       ilab.xpos=c(-2.8,-2),
       slab=paste(outcome4$Authors),cex=0.9, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-2.8), 5.1, cex = 0.9, bquote(bold("Pre")))
text(c(-2), 5.1, cex = 0.9, bquote(bold("Post")))
text(-7.5, -2.1, pos=4, cex=0.9, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv4$QE, digits=2, format="f")), 
        ", df = ", .(res4$k - res4$p),", p = ", 
        .(formatC(resmv4$QEp, digits=2, format="f")),
        "; "))))
text(-7.5, -3.1, pos=4, cex=0.9, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res4$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res4$tau2, digits=2, format="f"))))))
title("Effect of Experiential Learning on Emotion Regulation")

dd <- c(0,diff(outcome5$Study))
dd[dd > 0] <- 1
rows <- (1:res5$k) + cumsum(dd)
par(tck=-.02)
forest(resmv5, xlim=c(-7,5.5), ylim=c(16,-3),
       ilab=cbind(formatC(outcome5$m_pre,digits=2,format='f'), 
                  formatC(outcome5$m_post,digits=2,format='f')),
       ilab.xpos=c(-2.5,-1.7),
       slab=paste(outcome5$Authors),cex=0.75, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-2.5), 15.1, cex = 0.75, bquote(bold("Pre")))
text(c(-1.7), 15.1, cex = 0.75, bquote(bold("Post")))
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
title("Effect of Experiential Learning on Flexibility")

dd <- c(0,diff(outcome6$Study))
dd[dd > 0] <- 1
rows <- (1:res6$k) + cumsum(dd)
par(tck=-.02)
forest(resmv6, xlim=c(-6.5,5.5), alim=c(-1,2.5), ylim=c(15,-3),
       ilab=cbind(formatC(outcome6$m_pre,digits=2,format='f'), 
                  formatC(outcome6$m_post,digits=2,format='f')),
       ilab.xpos=c(-2.5,-1.7),
       slab=paste(outcome6$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-2.5), 14.1, cex = 0.8, bquote(bold("Pre")))
text(c(-1.7), 14.1, cex = 0.8, bquote(bold("Post")))
text(-6.5, -2.1, pos=4, cex=0.8, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv6$QE, digits=2, format="f")), 
        ", df = ", .(res6$k - res6$p),", p = ", 
        .(formatC(resmv6$QEp, digits=2, format="f")),
        "; "))))
text(-6.5, -3.1, pos=4, cex=0.8, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res6$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res6$tau2, digits=2, format="f"))))))
title("Effect of Experiential Learning on Organization Skills")

dd <- c(0,diff(outcome7$Study))
dd[dd > 0] <- 1
rows <- (1:res7$k) + cumsum(dd)
par(tck=-.02)
forest(resmv7,xlim=c(-6.5,4.5), ylim=c(-3,22),
       ilab=cbind(formatC(outcome7$m_pre,digits=2,format='f'), 
                  formatC(outcome7$m_post,digits=2,format='f')),
       ilab.xpos=c(-2.8,-2),
       slab=paste(outcome7$Authors),cex=0.68, header="Author(s) and Year",
       rows=rows)
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")
text(c(-2.8), 21.1, cex = 0.68, bquote(bold("Pre")))
text(c(-2), 21.1, cex = 0.68, bquote(bold("Post")))
text(-6.5, -2.1, pos=4, cex=0.68, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv7$QE, digits=2, format="f")), 
        ", df = ", .(res7$k - res7$p),", p = ", 
        .(formatC(resmv7$QEp, digits=2, format="f")),
        "; "))))
text(-6.5, -3.1, pos=4, cex=0.68, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res7$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res7$tau2, digits=2, format="f"))))))
title("Effect of Experiential Learning on Personal Skills")

dd <- c(0,diff(outcome8$Study))
dd[dd > 0] <- 1
rows <- (1:res8$k) + cumsum(dd)
par(tck=-.02)
forest(resmv8, xlim=c(-6.5,4.5), ylim=c(-3,15),
       ilab=cbind(formatC(outcome8$m_pre,digits=2,format='f'), 
                  formatC(outcome8$m_post,digits=2,format='f')),
       ilab.xpos=c(-2.5,-1.7),
       slab=paste(outcome8$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-2.5), 14.1, cex = 0.8, bquote(bold("Pre")))
text(c(-1.7), 14.1, cex = 0.8, bquote(bold("Post")))
text(-6.5, -2.1, pos=4, cex=0.8, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv8$QE, digits=2, format="f")), 
        ", df = ", .(res8$k - res8$p),", p = ", 
        .(formatC(resmv8$QEp, digits=2, format="f")),
        "; "))))
text(-6.5, -3.1, pos=4, cex=0.8, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res8$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res8$tau2, digits=2, format="f"))))))
title("Effect of Experiential Learning on Self-regulation")

dd <- c(0,diff(outcome9$Study))
dd[dd > 0] <- 1
rows <- (1:res9$k) + cumsum(dd)
par(tck=-.02)
forest(resmv9, xlim=c(-6.5,4.5), ylim=c(-3,19),
       ilab=cbind(formatC(outcome9$m_pre,digits=2,format='f'), 
                  formatC(outcome9$m_post,digits=2,format='f')),
       ilab.xpos=c(-2.5,-1.7),
       slab=paste(outcome9$Authors),cex=0.7, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-2.5), 18.1, cex = 0.7, bquote(bold("Pre")))
text(c(-1.7), 18.1, cex = 0.7, bquote(bold("Post")))
text(-6.5, -2.1, pos=4, cex=0.7, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv9$QE, digits=2, format="f")), 
        ", df = ", .(res9$k - res9$p),", p = ", 
        .(formatC(resmv9$QEp, digits=2, format="f")),
        "; "))))
text(-6.5, -3.1, pos=4, cex=0.7, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res9$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res9$tau2, digits=2, format="f"))))))
title("Effect of Experiential Learning on Social Skills")

dd <- c(0,diff(outcome10$Study))
dd[dd > 0] <- 1
rows <- (1:res10$k) + cumsum(dd)
par(tck=-.02)
forest(resmv10, xlim=c(-6.5,5), ylim=c(-3,6),
       ilab=cbind(formatC(outcome10$m_pre,digits=2,format='f'), 
                  formatC(outcome10$m_post,digits=2,format='f')),
       ilab.xpos=c(-2.5,-1.5),
       slab=paste(outcome10$Authors),cex=0.9, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
text(c(-2.5), 5.1, cex = 0.9, bquote(bold("Pre")))
text(c(-1.5), 5.1, cex = 0.9, bquote(bold("Post")))
text(-6.5, -2.1, pos=4, cex=0.9, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv10$QE, digits=2, format="f")), 
        ", df = ", .(res10$k - res10$p),", p = ", 
        .(formatC(resmv10$QEp, digits=2, format="f")),
        "; "))))
text(-6.5, -3, pos=4, cex=0.9, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res10$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res10$tau2, digits=2, format="f"))))))
title("Effect of Experiential Learning on Soft Skills (compound)")


#### PART 2. EXPERIENTIAL LEARNING EFFECT ON SOFT SKILLS (BETWEEN SUBJ)



#### PART 3. EXPERIENTIAL LEARNING EFFECT ON SOFT SKILLS (WITHIN & BETWEEN SUBJ)



#### PART 4. CAREER PLANNING EFFECT ON SOFT SKILLS (WITHIN SUBJ)



#### PART 5. CAREER PLANNING EFFECT ON SOFT SKILLS (WITHIN & BETWEEN SUBJ)


