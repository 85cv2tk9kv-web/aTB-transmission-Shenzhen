# GenePair Bao'an dataset 

library(tidyverse); library(BactDating); library(MetBrewer)
library(ape); library(phytools); library(stringr)
library(MetBrewer); library(parallel); library(mgcv)
library(cowplot); library(ggplot2); library(ggtree);
library(cmdstanr); library(binom); library(adegenet)

data <- # load dataset with strain IDs, diagnosis date (in decimal format) 

# create a vector of tip dates
tip.dates.vect <- data$diagnosis_date
names(tip.dates.vect) <- data$ID_strain

treeML <- # load ML treefile 
tree1 <- ape::multi2di(treeML)

genome_length <- # see length of MSA

# convert edge lengths to number of substitutions, NOT subs per site
tree1$edge.length <- round(tree1$edge.length * genome_length)

# fit tree
result <- bactdate(tree1, 
                   tip.dates.vect,
                   initMu = 0.5, 
                   nbIts = 100,# 000,
                   updateMu = F,
                   updateRoot = T,
                   model = 'strictgammaR', 
                   useCoalPrior = F, 
                   showProgress = T)

# SOURCE FUNCTIONS
source(file = 'Phylowave_pkg/2_Functions/2_1_Index_computation_20240909.R')
source(file = 'Phylowave_pkg/2_Functions/2_2_Lineage_detection_20240909.R')
source(file = 'Phylowave_pkg/2_Functions/2_3_Lineage_fitness_20240909.R')

# Tree info
tree       <- result$tree
names_seqs <- result$tree$tip.label
n_seq      <- length(names_seqs)
times_seqs <- leafDates(tree)

min_year <- floor(result$tree$root.time)
min_year_samp <- year(min(tip.dates.vect) - 1) 
max_year <- year(max(tip.dates.vect))

# Mutation rate 
mutation_rate = 0.5/genome_length # mutations per site per year
# Parameters for the index
timescale = 30 # based on what authors used for TB
wind = 1 # window of time on which to search for samples in the population

# distance matrix
genetic_distance_mat = dist.nodes.with.names(tree)

# internal node timing
nroot = length(tree$tip.label) + 1 ## Root number
distance_to_root = genetic_distance_mat[nroot,]
root_height = times_seqs[which(names_seqs == names(distance_to_root[1]))] - 
  distance_to_root[1]
nodes_height = root_height + distance_to_root[n_seq+(1:(n_seq-1))]

# Prep output dataset 
dataset_with_nodes <- data.frame(ID = c(1:n_seq, n_seq+(1:(n_seq-1))), 
                                 name_seq = c(names_seqs, n_seq+(1:(n_seq-1))),
                                 time = c(times_seqs, nodes_height),
                                 is.node = c(rep('no', (n_seq)), 
                                             rep('yes', (n_seq-1)))) 

# CALCULATE INDEX
dataset_with_nodes$index = compute.index(time_distance_mat = genetic_distance_mat, 
                                         timed_tree = tree, 
                                         time_window = wind,
                                         metadata = dataset_with_nodes, 
                                         mutation_rate = mutation_rate,
                                         timescale = timescale,
                                         genome_length = genome_length)
# end