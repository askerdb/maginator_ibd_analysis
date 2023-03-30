library(vegan)
library(cowplot)
library(viridis)
ps_maginator_f0 = filter_taxa(ps_maginator, function(x) sum(x) > 0, TRUE)
ma_ordination0 = ordinate(ps_maginator_f0, "PCoA", "jsd")
ma_dist0 = distance(ps_maginator_f0, "jsd")
ma_plot0 = plot_ordination(ps_maginator_f0, ma_ordination0, color = "Control")  + scale_color_viridis(discrete=TRUE)
ma0_permanova_fit = adonis(ma_dist0 ~ sample_data(ps_maginator_f0)$Control)
ma0_permanova_fit


ps_maginator_f = filter_taxa(ps_maginator, function(x) sum(x) > .1, TRUE)
ma_ordination = ordinate(ps_maginator_f, "PCoA", "jsd")
ma_dist = distance(ps_maginator_f, "jsd")
ma_plot = plot_ordination(ps_maginator_f, ma_ordination, color = "Control")  + scale_color_viridis(discrete=TRUE)
ma_permanova_fit = adonis(ma_dist ~ sample_data(ps_maginator_f)$Control)
ma_permanova_fit

fran_ordination = ordinate(ps_fran, "PCoA", "jsd")
fran_dist = distance(ps_fran, "jsd")
fran_plot =plot_ordination(ps_fran, fran_ordination, color = "Control") + scale_color_viridis(discrete=TRUE)
fran_permanova_fit = adonis(fran_dist ~ sample_data(ps_fran)$Control)

