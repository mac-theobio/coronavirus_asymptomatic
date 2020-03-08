library(emdbook)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))

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
  q=c(qvals),
  z=rep(z, length(relG)),
  relG=rep(relG, each=length(z))
)

example_p <- 0.18
example_R_a <- 0.5
example_R_s <- 1
example_z <- example_p * example_R_a/(1-example_p)
example_qodds <- example_z/(1-example_z) *  (1 + kappa_s * r * G_s)^(1/kappa_s)/(1 + kappa_a * r * relG * G_s)^(1/kappa_a)
example_q <- example_qodds/(1+example_qodds)
range(example_q)

example_qodds2 <- example_z/(1-example_z) *  (1 + kappa_s * r * G_s)^(1/kappa_s)/(1 + kappa_a * r * G_s)^(1/kappa_a)
example_q2 <- example_qodds2/(1+example_qodds2)

g0 <- ggplot(figdata) +
  geom_raster(aes(relG, z, fill=q)) +
  geom_contour(aes(relG, z, z=q), col="white") +
  # geom_hline(yintercept=example_z, col="red", lwd=1) +
  geom_point(aes(x=1, y=example_z, fill=example_q2), size=5, col="white", shape=1, stroke=1) +
  scale_x_log10(expression(paste("Relative mean asymptomatic generation interval, ", italic(G[a]/G[s]))), expand=c(0, 0),
                     breaks=c(0.5, 1, 2)) +
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

ggsave("figheatmap.pdf", g0, width=5.3, height=4.3)
