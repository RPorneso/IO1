exp_analytical <- read_excel("Desktop/exp_analytical.xlsx")

study <- c(1,2,2,3,3,3,4)
exp_analytical$new_study <- study
exp_analytical = exp_analytical[,c(9,1,2,3,4,5,6,7,8)]
names(exp_analytical)[1] <- "Study"
names(exp_analytical)[2] <- "Authors"

dd <- c(0,diff(exp_analytical$Study))
dd[dd > 0] <- 1
rows <- (1:res$k) + cumsum(dd)
par(tck=-.03)

forest(resmv, xlim=c(-6,5), ylim=c(-3,13), 
       ilab=cbind(formatC(exp_analytical$m_pre,digits=2,format='f'), 
                  formatC(exp_analytical$m_post,digits=2,format='f')),
       ilab.xpos=c(-2.2,-1.2),
       slab=paste(exp_analytical$Authors), 
       cex=0.8, header="Author(s) and Year",
       rows=rows) 
text(c(-2.2), 12.2, cex = 0.8, bquote(bold("Pre")))
text(c(-1.2), 12.2, cex = 0.8, bquote(bold("Post")))



abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") # WORKING!!!!

text(-6, -2.1, pos=4, cex=0.75, bquote(italic(
  paste("Heterogeneity: Q = ",
        .(formatC(resmv$QE, digits=2, format="f")), 
        ", df = ", .(res$k - res$p),", p = ", 
        .(formatC(resmv$QEp, digits=2, format="f")),
        "; "))))
text(-6, -3.1, pos=4, cex=0.75, bquote(italic(
  paste(I^2, " = ",
        .(formatC(res$I2, digits=1, format="f")), 
        "; ", tau^2 == 
          .(formatC(res$tau2, digits=2, format="f"))))))

title("Effect of Experiential Learning on Analytical Skills")

