library(tidyverse)
library(ggtree)
library (ggplot2)
library(ggrepel)
library (ape)

tree  <- read.tree("Tree_genoma_OK.treefile")
tree
plot(tree)
### reRoot the tree
edit(tree) #To see the name of the external group correctly
ggtree(tree) + geom_tiplab()
rooted.tree <- root(tree, which(tree$tip.label == "Euphausia_pacifica"))
ggtree(rooted.tree) + geom_tiplab()

####### pops information
library("readxl")
library(viridis)
info <- as.data.frame(read_excel ("clados_info.xlsx"))

# For the clade group
dat4 <- info %>% select(c("ID", "family","name_ok","grupo"))
dat4 <- aggregate(.~grupo, dat4, FUN=paste, collapse=",")
clades <- lapply(dat4$ID, function(x){unlist(strsplit(x,split=","))})
names(clades) <- dat4$grupo
tr <- groupOTU(rooted.tree, clades, "grupo")
grupo <- NULL

####### Color pallete - viridis Type=inferno
pal <- c("Euphausiacea"= "#fcffa4", "Stomatopoda"="#f5db4c","Dendrobrachiata"="#fcae12","Caridea"="#f78410","Stenopodidea"="#e65d2f","Axiidea"= "#e15635","Polychelidae"="#cb4149","Astacidea"= "#a92e5e","Achelata"="#9c179e","Gebiidea"= "#5f136e","Anomura"="#0d0887","Brachyura"= "#000004")

library(treeio)


######## Plot tree circular
plot_tree <- ggtree(tr,aes(color=grupo),layout = "circular", branch.length="none", size=1.25) +
  geom_tippoint(aes(color=grupo), size=2) + 
  geom_label2(aes(label=label, subset = !is.na(as.numeric(label)) & as.numeric(label) > 70), size= 1.5)+
  geom_tiplab( hjust = -.10, size=2, align=TRUE) + 
  geom_treescale(offset=0.15)+
  scale_colour_manual(values = pal,name="")+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
   theme(legend.position = c(1.1, 0.5))

plot_tree

#ggsave("plot_tree.pdf", width = 40, height = 30, units = "cm")
#ggsave("plot_tree2.pdf", width = 30, height = 20, units = "cm")

##### Plot big tree
plot_tree2 <- ggtree(tr,aes(color=grupo), size=1) +
  geom_tippoint(aes(color=grupo), size=2) + 
  geom_tiplab(size=4) + 
  geom_treescale(offset=0.15)+
  geom_nodelab(label = rooted.tree,
               geom = 'label')+
  scale_colour_manual(values = pal,name="")+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  theme(legend.position = c(1.1, 0.5))
plot_tree2


 


