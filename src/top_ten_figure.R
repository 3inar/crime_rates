library(ProjectTemplate); load.project()
library(plyr)

rates_2016 <- norwegian_crime %>% filter(crime_type == "¬ Vold og mishandling", year=="2016") %>%
  transmute(place, population, rate=reports/population, reports)

rates_2016 <- cbind(rates_2016, stats(rates_2016))

# make top 10, plot it
top10 <- top_n(rates_2016, 10, ebayes_rate) %>% arrange(ebayes_rate)
top10[8, "place"] = "Porsanger"

pdf(file="top_ten.pdf", width=6, height=5)
op <- par(mar = c(4,10,2,2) + 0.1)
n_c <- nrow(top10)
plot(c(range(select(top10, rate, ebayes_rate, ebayes_lower, ebayes_upper))), c(0, n_c), type="n",
     yaxt="n", bty="n", ylab="", xlab="", main="Ten most violent towns in 2016",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)
axis(2, at=1:n_c, labels=top10$place, las=1, tick=F, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)
points(top10$ebayes_rate, 1:n_c, pch=19)
# points(top10$lc, 1:n_c, pch="|", cex = 0.5)
# points(top10$uc, 1:n_c, pch="|", cex = 0.5)

plyr::a_ply(1:n_c, 1, function(i) {
  obs <- top10[i, ]
  lines(c(obs$ebayes_upper, obs$ebayes_lower), c(i,i), lwd=2)
})
points(top10$rate, 1:n_c, pch=20, col="red")
dev.off()
