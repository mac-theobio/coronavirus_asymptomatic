library(emdbook)
library(ggplot2); theme_set(theme_bw())

r <- 1/7
G_s <- 8
kappa_s <- 0.5
kappa_a <- 0.5

z <- seq(0.01, 0.99, by=0.01)
relG <- exp(seq(log(0.5), log(2), by=0.005))

qodds <- apply2d(
  "*", 
  z/(1-z),
  (1 + kappa_s * r * G_s)^(1/kappa_s)/(1 + kappa_a * r * relG * G_s)^(1/kappa_a)
)

qvals <- qodds/(1+qodds)

figdata <- data.frame(
  qdiff=c(qvals)-rep(z, length(relG)),
  z=rep(z, length(relG)),
  relG=rep(relG, each=length(z))
)

zexample <- 0.5
relg1 <- 0.55
q1 <- (1 + kappa_s * r * G_s)^(1/kappa_s)/(1 + kappa_a * r * relg1 * G_s)^(1/kappa_a)
q1/(1+q1)

relg2 <- 1.8
q2 <- (1 + kappa_s * r * G_s)^(1/kappa_s)/(1 + kappa_a * r * relg2 * G_s)^(1/kappa_a)
q2/(1+q2)

g0 <- ggplot(figdata) +
  geom_raster(aes(relG, z, fill=qdiff)) +
  geom_contour(aes(relG, z, z=qdiff), col="white") +
  scale_x_log10("Relative mean asymptomatic generation interval, $G_a/G_s$", expand=c(0, 0),
                     breaks=c(0.5, 1, 2)) +
  scale_y_continuous("Intrinsic proportion of asymptomatic transmission, $z$", expand=c(0, 0),
                     limits=c(0, 1),
                     breaks=0:10/10) +
  scale_fill_gradientn("$q-z$", colors=c("black", "#8a0072", "#cf2661", "#f66d4e", "#ffb34a", "#f9f871", "#f5f3b5"),
                       limits=c(-0.2, 0.2),
                       breaks=c(-0.2, -0.1, 0, 0.1, 0.2) ) +
  ggtitle("Differences in the realized and the intrinsic\nproportions of asymptomatic transmission") +
  theme(
    legend.key.height = unit(1.5, "cm")
  )

tikz(file = "figheatmap_diff.tex", width = 5, height = 4, standAlone = T)
print(g0)
dev.off()
tools::texi2dvi('figheatmap_diff.tex', pdf = T, clean = T)
