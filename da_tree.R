library(ggtree)
library(treeio)


files = Sys.glob("data/fransoza_trees/trees/*")
da_tree_results_df = lapply(files, function(x){
  print(x)
  tree_path = x
  tree_name = sub(".nwk", "",strsplit(tree_path,"/" )[[1]][4])
  tree_taxa = data.frame(tax_table(ps_maginator))[tree_name,]$Species
  tree <- read.tree(x)
  tree_co = cophenetic(tree)
  tree_co_f = tree_co[-which(colnames(tree_co) == "Outgroup"), -which(colnames(tree_co) == "Outgroup")]
  tree_co_meta = merge(data.frame(Sample = colnames(tree_co)), franmeta_ena, by.x = "Sample", by.y = "experiment_accession", sort = F)
  if (length(table(tree_co_meta$Control) | nrow(tree_co_meta) < 15) == 1) return(data.frame(P = 1, R2 = 0, Cluster = x, Samples = nrow(tree_co_meta), taxa = tree_taxa))
  res = adonis(tree_co_f  ~ tree_co_meta$Control, permutations = 99999)
  resdf = data.frame(P = res$aov.tab[1,6], R2 = res$aov.tab[1,4], Cluster = x, Samples = nrow(tree_co_meta), taxa = tree_taxa)
  print(res)
  return(resdf)
})
da_tree_results_dfdf = do.call(rbind, da_tree_results_df)
da_tree_results_dfdf$Padj = p.adjust(da_tree_results_dfdf$P, method = "fdr")
save(da_tree_results_dfdf, file = "data/da_tree_results.Rdata")
ggplot(da_tree_results_dfdf, aes(y = -log10(P), x = R2, color = Padj < 0.05)) + geom_point()

tree_path = "data/fransoza_trees/trees/Cluster1882.nwk"
tree_name = sub(".nwk", "",strsplit(tree_path,"/" )[[1]][4])
tree_taxa = data.frame(tax_table(ps_maginator))[tree_name,]$Species
tree <- read.tree(tree_path)
ggtree_obj <- ggtree(tree, layout = "circular", branch.length = "none")
ggtree_obj$data = merge(ggtree_obj$data, franmeta_ena, by.x ="label", by.y = "experiment_accession", all.x =T)
ggtree_obj_ann = ggtree_obj + geom_tippoint(aes(color = Control, size = 5))#geom_tiplab(aes(color = Control, label = Control))
ggtree_obj_ann




treepcoa = pcoa(tree_co_f)
plot(treepcoa$vectors[,1:2])


