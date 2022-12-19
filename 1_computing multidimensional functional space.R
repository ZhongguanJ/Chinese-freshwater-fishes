## compute functional space

rm(list = ls())


library(tidyverse)

trait <- read_csv("data/functional traits.csv")
iucn <- read_csv("data/fish_iucn_level.csv") 
trait_category <- read.csv('data/trait category.csv', header= TRUE)

## remove extincted species
iucn <- iucn %>%
  filter(!iucn_level %in% c("EX", "RE"))

trait <- trait %>%
  filter(species %in% iucn$species) %>%
  mutate(Diet = as.factor(Diet),
         Habitat  = as.factor(Habitat),
         Body  = as.factor(Body)) %>%
  tibble::column_to_rownames(var = "species")


#computing distances between species based on functional traits
sp_dist <- mFD::funct.dist(
  sp_tr = trait,
  tr_cat = trait_category,
  metric = "gower",
  scale_euclid = "scale_center",
  ordinal_var = "classic",
  weight_type = "equal",
  stop_if_NA = TRUE)


# Compute multidimensional functional spaces and assess their quality
fspaces_quality <- mFD::quality.fspaces(
  sp_dist = sp_dist,
  maxdim_pcoa = 10,
  deviation_weighting = "absolute",
  fdist_scaling = FALSE,
  fdendro = "average")

# determine how many dimensions have lowest deviations
fspaces_quality[[1]]

sp_faxes_coord <- fspaces_quality$details_fspaces$sp_pc_coord[, 1:6]

save(sp_faxes_coord, file = "data/fish_functional_axes.RDATA")

