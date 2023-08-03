library(vegan)
library(cowplot)
library(viridis)
ps_maginator_f0 = filter_taxa(ps_maginator, function(x) sum(x) > 0, TRUE)
ma_ordination0 = ordinate(ps_maginator_f0, "PCoA", "jsd")
ma_dist0 = distance(ps_maginator_f0, "jsd")
ma_plot0 = plot_ordination(ps_maginator_f0, ma_ordination0, color = "Control")  +
  scale_color_manual(values = c("brown2", "deepskyblue1"), labels = c("IBD", "Control")) + geom_point(size=3)+ theme_bw()+ labs(color = "Disease status") +
  annotate("label", x = 0.4, y = -0.20,
           label = paste0("\n Permanova R^2: ", round(ma0_permanova_fit$aov.tab[1,5], 4)*100, "%\nP: ", round(ma0_permanova_fit$aov.tab[1,6],4) ), color = "black", size = 5,
           hjust = 1, vjust = 0) +
  xlim(-0.25, 0.4) + ylim(-0.2, 0.4)
ma_plot0
ma0_permanova_fit = adonis(ma_dist0 ~ sample_data(ps_maginator_f0)$Control)
ma0_permanova_fit


ps_maginator_f = filter_taxa(ps_maginator, function(x) sum(x) > .1, TRUE)
ma_ordination = ordinate(ps_maginator_f, "PCoA", "jsd")
ma_dist = distance(ps_maginator_f, "jsd")
ma_permanova_fit = adonis(ma_dist ~ sample_data(ps_maginator_f)$Control)

ma_plot = plot_ordination(ps_maginator_f, ma_ordination, color = "Control")  +
  scale_color_manual(values = c("brown2", "deepskyblue1"),labels = c("IBD", "Control"))  + geom_point(size=3) + theme_bw() + labs(color = "Disease status")+
  annotate("label", x = 0.4, y = -0.20,
           label = paste0("\n Permanova R^2: ", round(ma_permanova_fit$aov.tab[1,5], 4)*100, "%\nP: ", round(ma_permanova_fit$aov.tab[1,6],4) ), color = "black", size = 5,
           hjust = 1, vjust = 0)+
  xlim(-0.25, 0.4) + ylim(-0.2, 0.4)


fran_ordination = ordinate(ps_fran, "PCoA", "jsd")
fran_dist = distance(ps_fran, "jsd")
fran_plot =plot_ordination(ps_fran, fran_ordination, color = "Control") +
  scale_color_manual(values = c("brown2", "deepskyblue1"), labels = c("IBD", "Control")) + geom_point(size=3)+ theme_bw() + labs(color = "Disease status")+
  annotate("label", x = 0.4, y = -0.20,
           label = paste0("\n Permanova R^2: ", round(fran_permanova_fit$aov.tab[1,5], 4)*100, "%\nP: ", round(fran_permanova_fit$aov.tab[1,6],4) ), color = "black", size = 5,
           hjust = 1, vjust = 0)+
  xlim(-0.25, 0.4) + ylim(-0.2, 0.4)
fran_permanova_fit = adonis(fran_dist ~ sample_data(ps_fran)$Control)

