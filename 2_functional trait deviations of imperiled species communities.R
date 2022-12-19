

rm(list = ls())


# load packages
needed_libs <- c("tidyverse","ggplot2", "ggridges", "cowplot", "ggstance")

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1])) {   
    install.packages(p, dep = TRUE)
  }
  require(p, character.only = TRUE)
}

sapply(needed_libs, usePackage)
rm(usePackage)


spedis <- read_csv("data/species site.csv")
region <- read_csv("data/region_names.csv") 
iucn <- read_csv("data/fish_iucn_level.csv") 
load("data/fish_functional_axes.RDATA")


## remove extincted species
iucn <- iucn %>%
  filter(!iucn_level %in% c("EX", "RE"))

trait_5pc <- sp_faxes_coord %>% 
  as.data.frame() %>%
  rownames_to_column(var = "species") %>%
  as_tibble()

# combine distribution, trait, IUCN status 
dat <- spedis %>% 
  pivot_longer(col = sp1:sp1591, names_to = "species", values_to = "presence") %>%
  filter(presence == 1 & species %in% trait_5pc$species) %>%
  left_join(trait_5pc) %>%
  left_join(region, by = c("region" = "region_ID")) %>%
  rename(region_iucn = region, region = region_name) %>%
  relocate(region_iucn, region, iucn) %>%
  dplyr::select(-presence)

# total, red and green species richness for the China and each region 
sprich_region <- dat %>% 
  group_by(region, iucn) %>%
  summarise(sprich = n_distinct(species)) %>% 
  pivot_wider(names_from = iucn, values_from = sprich, names_prefix = "sprich_", values_fill = 0)

dat <- dat %>%
  left_join(sprich_region)


## A rarefaction null model 
# random sample the same number of species with the red species from species pool (total) for each area 
# replicate 200 times, which will be used to calculate the distance between centroids of these species' trait space and that of species pool

dat_rare <- NULL
for(i in 1:200){
  dat_temp <- dat %>% 
    filter(iucn != "green" & sprich_red > 0) %>%
    group_by(region, iucn) %>%
    sample_n(size = sprich_red) %>%
    mutate(replication = i)
  
  dat_rare <- bind_rows(dat_rare, dat_temp)
}

# for red species, we only need on replication, that is the observed species composition 
dat_rare <- dat_rare %>%
  filter(!(iucn == "red" & replication > 1)) %>%
  relocate(replication)

# calculate the mean values of trait PCs for each replication and observed species compositions
dat_rare_mean <- dat_rare %>% 
  group_by(region, iucn, replication) %>%
  summarise(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3),
            PC4 = mean(PC4),
            PC5 = mean(PC5)) %>%
  ungroup()

# the mean values of trait PCs of all species in each area
dat_mean <- dat %>% 
  filter(iucn == "total") %>%
  group_by(region) %>%
  summarise(t_PC1 = mean(PC1),
            t_PC2 = mean(PC2),
            t_PC3 = mean(PC3),
            t_PC4 = mean(PC4),
            t_PC5 = mean(PC5)) %>%
  ungroup()

# the distance between trait centroids of the observed red or random species assemblages and that of species pool
dat_ft_dist <- dat_rare_mean %>%
  left_join(dat_mean) %>%
  mutate(ft_dist = sqrt((PC1 - t_PC1)^2 + (PC2 - t_PC2)^2 + (PC3 - t_PC3)^2 + (PC4 - t_PC4)^2 + (PC5 - t_PC5)^2)) %>%
  dplyr::select(-c(PC1:t_PC5))

# set levels of reiion (put the China in the first positioni)
dat_ft_dist <- dat_ft_dist %>%
  mutate(region = factor(region, levels = c("China", "Ayeyarwady", "EastRiv", "Haihe", "Hainan", "Honghe", "Huaihe", 
                                            "LancangRiv","NortheastRiv", "Nujiang", "PearlRiv", "Qinzhang", "SouthRiv", 
                                            "Taiwan", "Xinjiang", "YangtzeRiv", "YarlungZangbo", "YellowRiv")))

## generate figures
ggplot(dat_ft_dist  %>% filter(iucn == "total")) +
  facet_wrap(~ region, scales = "free", nrow = 3) +
  geom_density_ridges_gradient(aes(x = ft_dist, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.0, 0, 0.00,0.00)) +
  geom_vline(data = dat_ft_dist  %>% filter(iucn == "red"), 
             aes(xintercept = ft_dist), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Functional distance", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 10),
        strip.text.x = element_text(size = 8),
        strip.background = element_blank(),
        strip.text = element_text(angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)

ggsave("results/Functionial_distance.png", unit="mm", width=180, height=120)

