library(DAtest)
library(ggplot2)

#Original data differential abundance
#Run discovery DA with wilcoxons test
fran_discovery = DAtest::DA.wil(subset_samples(ps_fran, Validation == F), "Control",relative=FALSE)
fran_discovery_sig = subset(fran_discovery, pval.adj < 0.05)


#Run validation DA
fran_validation = DAtest::DA.wil(subset_samples(ps_fran, Validation == T), "Control",relative=FALSE)
fran_validation_sig = subset(fran_validation, pval.adj < 0.05)
fran_validation_sig$Label = "Original"

#Overlap significant results
fran_both = merge(fran_discovery_sig, fran_validation_sig, by = "Feature")
ggplot(fran_both, aes(x = log2FC.x, log2FC.y)) + geom_point()

fran_both_all = merge(fran_discovery, fran_validation, by = "Feature")
#All the cases I have looked at, p-values of NA indicate no presence in the validation cohort. Should we check this systematically?
fran_both_all$Validated_significant =fran_both_all$pval.adj.x < 0.05 & fran_both_all$pval.adj.y < 0.05
fran_both_all$Validated_significant[is.na(fran_both_all$Validated_significant)] = FALSE
fran_volc = ggplot(fran_both_all, aes(x = log2FC.x, y = -log10(pval.x), color = Validated_significant)) + geom_point(size = 3, alpha = 0.9) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  theme_bw() + scale_color_manual(values = c("darkolivegreen4", "cornflowerblue")) +
  xlab("Log2(FC)") + ylab("-log10(p-value)")


#MAGinator differential abundance test
#Run discovery DA with wilcoxons test
ma_discovery = DAtest::DA.wil(subset_samples(ps_maginator, Validation == F), "Control",relative=FALSE)
ma_discovery_sig = subset(ma_discovery, pval.adj < 0.05)
ggplot(ma_discovery)

#Run validation DA
ma_validation = DAtest::DA.wil(subset_samples(ps_maginator, Validation == T), "Control",relative=FALSE)
ma_validation_sig = subset(ma_validation, pval.adj < 0.05)
ma_validation_sig$Label = "MAGinator"

#Overlap significant results
ma_both = merge(ma_discovery_sig, ma_validation_sig, by = "Feature")
ggplot(ma_both, aes(x = log2FC.x, log2FC.y)) + geom_point()

ma_both_all = merge(ma_discovery, ma_validation, by = "Feature")

ma_both_all$Validated_significant =ma_both_all$pval.adj.x < 0.05 & ma_both_all$pval.adj.y < 0.05
ma_both_all$Validated_significant[is.na(ma_both_all$Validated_significant)] = FALSE
mavolc = ggplot(ma_both_all, aes(x = log2FC.x, y = -log10(pval.x), color = Validated_significant)) + geom_point(size = 3, alpha = 0.9) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  theme_bw() + scale_color_manual(values = c("darkolivegreen4", "cornflowerblue")) +
  xlab("Log2(FC)") + ylab("-log10(p-value)")


#plot barplot
both_bar = ggplot(rbind(fran_validation_sig[,c("Feature", "Label")], ma_validation_sig[,c("Feature", "Label")]), aes(x = Label, fill = Label)) +
  geom_bar() + xlab("Number of validated discoveries") + ylab("Count") + scale_fill_manual(values=c("darkblue", "darkred")) + theme_bw()


#Try to create some kind of tree?

