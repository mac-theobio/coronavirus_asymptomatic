library(emdbook)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))

r <- 1/7
G_s <- 8
G_a <- 8
kappa_s <- 0.5

z <- seq(0.01, 0.99, by=0.005)
relkappa <- exp(seq(log(0.5), log(2), by=0.005))

qodds <- apply2d(
  "*", 
  z/(1-z),
  (1 + kappa_s * r * G_s)^(1/kappa_s)/(1 + kappa_s * relkappa * r * G_a)^(1/(kappa_s * relkappa))
)

qvals <- qodds/(1+qodds)

figdata <- data.frame(
  q=c(qvals),
  z=rep(z, length(relkappa)),
  relkappa=rep(relkappa, each=length(z))
)

g0 <- ggplot(figdata) +
  geom_raster(aes(relkappa, z, fill=q)) +
  geom_contour(aes(relkappa, z, z=q), col="white") +
  scale_x_log10(expression(paste("Relative asymptomatic generation-interval dispersion, ", italic(kappa[a]/kappa[s]))), expand=c(0, 0),
                breaks=c(0.5, 1, 2),
                limits=c(0.5, 2)) +
  scale_y_continuous(expression(paste("Intrinsic proportion of asymptomatic transmission, ", italic(z))), expand=c(0, 0),
                     limits=c(0, 1),
                     breaks=0:10/10) +
  scale_fill_gradientn(colors=c("black", "#8a0072", "#cf2661", "#f66d4e", "#ffb34a", "#f9f871", "#f5f3b5"),
                       limits=c(0, 1),
                       breaks=0:5*2/10) +
  ggtitle("A. Relevance of asymptomatic cases") +
  theme(
    legend.title = element_blank(),
    legend.key.height = unit(1.8, "cm")
  )

ggsave("figheatmap_kappa.pdf", g0, width=5.3, height=4.3)
