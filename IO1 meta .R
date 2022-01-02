## clear environment

rm(list = ls())

## clear console

cat("\f") 

## install package and library

install.packages("readxl")
install.packages("metafor")
install.packages("lme4")
install.packages("dplyr")
install.packages("XLConnect") # not working, no java

library(metafor)
library(lme4)
library(readxl)
library(dplyr)
library(XLConnect) # cannot use this, no java

# if required, install updates and dowload dev version

install.packages("remotes")
remotes::install_github("wviechtb/metafor")

## load tables repeated measures only

exp_analytical <- read_excel("Desktop/exp_analytical.xlsx")
View(exp_analytical)   

## calculating t from p val

pval <- 0.00000000001
ni <- 392
ti = qt(pval/2, df=ni-1, lower.tail=FALSE)

## calculate standard mean change

exp_analytical <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, ri=ri, ni=ni, data=exp_analytical)

## effect size per study

# add col for study number and rearrange

study <- c(1,2,2,3,3,3,4)
exp_analytical$new_study <- study
#exp_analytical[11] <- NULL
exp_analytical = exp_analytical[,c(11,1,2,3,4,5,6,7,8,9,10)]
names(exp_analytical)[1] <- "Study"

# exp_ana <- exp_analytical[2:11]

round(aggregate(yi ~ Study, data=exp_analytical, FUN=function(x) c(mean=mean(x), 
    sd=sd(x), min=min(x), max=max(x))), 3)

effsizestudy <- round(aggregate(yi ~ Study, data=exp_analytical, 
                                FUN=function(x) c(mean=mean(x), 
                                                  sd=sd(x), min=min(x), 
                                                  max=max(x))), 3)

## get meta analysis results

res <- rma(yi, vi, data=exp_analytical)
print(res, digits=3)

resmv <- rma.mv(yi, vi, random = ~ 1 | Study, data=exp_analytical)
print(resmv, digits=3)

## forest plot

# forest(res, slab=paste(exp_analytical$Authors)) # regular without multi-level modeling
# 
# dd <- c(0,diff(exp_analytical$Study))
# dd[dd > 0] <- 1
# rows <- (1:res$k) + cumsum(dd)
# par(tck=-.01, mgp = c(1.6,.2,0))
# 
# forest(resmv, 
#        order=order(exp_analytical$Study),
#        slab=paste(exp_analytical$Authors),cex=0.8, header=TRUE,rows=rows) 
# abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 
# text(-4.8, -1.65, pos=4, cex=0.75, bquote(italic(
#   paste("Q = ",
#         .(formatC(resmv$QE, digits=2, format="f")), 
#         ", p = ", 
#         .(formatC(resmv$QEp, digits=2, format="f")),
#         "; ", I^2, " = ",
#         .(formatC(res$I2, digits=1, format="f")), 
#         "; ", tau^2 == 
#           .(formatC(res$tau2, digits=2, format="f"))))))

# text(-4.79, -1.78, pos=4, cex=0.70, bquote(italic(
#   paste("Heterogeneity: ",
#         I^2, " = ",
#         .(formatC(res$I2, digits=1, format="f")), 
#         ", ", tau^2 == 
#           .(formatC(res$tau2, digits=2, format="f"))))))

# forest(resmv, 
#            predict = TRUE, 
#            print.tau2 = TRUE,
#            leftcols = exp_analytical$Author)
# abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")

# tabletext <- cbind("","Study","Abushakra et al.", "Hains-Wesson & Ji","Hains-Wesson & Ji","Jackson","Jackson","Jackson","Klegeris")

# forest(resmv, slab=paste(exp_analytical$Authors),  
#        header = TRUE) 
# abline(h = rows[c(2, 4, 6)]-1.4, lty="dotted")
# text(-4.8, -1.65, pos=4, cex=0.75, bquote(italic(
#   paste("Q = ",
#         .(formatC(resmv$QE, digits=2, format="f")), 
#         ", p = ", 
#         .(formatC(resmv$QEp, digits=2, format="f")),
#         "; ", I^2, " = ",
#         .(formatC(res$I2, digits=1, format="f")), 
#         "; ", tau^2 == 
#           .(formatC(res$tau2, digits=2, format="f"))))))
# text(-4.8, -2.65, pos=4, cex=0.75, bquote(italic(
#   paste(I^2, " = ",
#         .(formatC(res$I2, digits=1, format="f")), 
#         "; ", tau^2 == 
#           .(formatC(res$tau2, digits=2, format="f"))))))

######

dd <- c(0,diff(exp_analytical$Study))
dd[dd > 0] <- 1
rows <- (1:res$k) + cumsum(dd)
par(tck=-.02)

forest(resmv, xlim=c(-5,6), ylim=c(-3,13),
       slab=paste(exp_analytical$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") # WORKING!!!!
text(-5, -2.1, pos=4, cex=0.75, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv$QE, digits=2, format="f")), 
        ", df = ", .(res$k - res$p),", p = ", 
        .(formatC(resmv$QEp, digits=2, format="f")),
        "; "))))
text(-5, -3.1, pos=4, cex=0.75, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res$tau2, digits=2, format="f"))))))
title("Effect of Experiential Learning on Analytical Skills")

## load multiple excel sheets

for (i in 1:10){
  outcome <- read_excel("Desktop/EL_on_Life_Skills_within_subj.xlsx",i)
  assign(paste0("outcome",i),outcome)
}

## perform meta-analysis for each outcome variable

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





################################################################################
################################################################################

## trial lapply

outcomeslist <- list(outcome2=outcome2, outcome3=outcome3, outcome4=outcome4)

outcome <- lapply(outcomeslist,escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
              ri=ri, ni=ni, data=outcomeslist))

## trial for loops

outcomes <- c(1:4)

for (i in outcomes){
  mydata <- paste0("outcome",i)
  mydata <- escalc(measure="SMCC", m1i=m_post, m2i=m_pre, sd1i=sd_post, sd2i = sd_pre, 
         ri=ri, ni=ni, data=mydata)
  res0 <- rma(yi, vi, data=mydata)
  assign(paste0("res",i),res0)
  resmv0 <- rma.mv(yi, vi, random = ~ 1 | Study, data=mydata)
  assign(paste0("resmv",i),resmv0)
}

## to add in for or lapply when it works

dd <- c(0,diff(exp_analytical$Authors))
dd[dd > 0] <- 1
rows <- (1:res$k) + cumsum(dd)
par(tck=-.02)

forest(resmv, xlim=c(-5,6), ylim=c(-3,13),
       slab=paste(exp_analytical$Authors),cex=0.8, header="Author(s) and Year",
       rows=rows) 
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") # WORKING!!!!
text(-5, -2.1, pos=4, cex=0.75, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv$QE, digits=2, format="f")), 
        ", p = ", 
        .(formatC(resmv$QEp, digits=2, format="f")),
        "; "))))
text(-5, -3.1, pos=4, cex=0.75, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res$tau2, digits=2, format="f"))))))