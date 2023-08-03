library(cowplot)

load("differential abundance.R")
load("beta_diversity.R")

plot_grid(fran_plot, ma_plot0+ theme(legend.position = "none"), ma_plot+ theme(legend.position = "none"),
          fran_volc, mavolc + theme(legend.position = "none") , both_bar, labels=c("A", "B", "C", "D", "E", "F"), rows = 2)
ggsave("figure3_pcoa_da.pdf", height = 10, width = 15)
