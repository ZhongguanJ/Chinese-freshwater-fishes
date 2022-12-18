
library(tidyverse)
library(ggridges)


spedis <- read_csv("data/species site.csv")
trait <- read_csv("data/functional traits.csv")
region <- read_csv("data/region_names.csv") 

# combine distribution, trait, IUCN status 
dat <- spedis %>% 
  #rename(region = X1) %>%
  pivot_longer(col = sp1:sp1591, names_to = "species", values_to = "presence") %>%
  filter(presence == 1) %>%
  left_join(trait) %>%
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
# replicate 200 times, which can be used to calculate the distribution of mean trait values of random samples 
dat_rare <- NULL
for(i in 1:200){
  dat_temp <- dat %>% 
    filter(iucn != "green" & sprich_red > 0) %>%
    group_by(region, iucn) %>%
    sample_n(size = sprich_red[1]) %>%
    mutate(replication = i)

  dat_rare <- bind_rows(dat_rare, dat_temp)
}



# for red species, we only need on replication, that is the observed species composition 
dat_rare <- dat_rare %>%
  filter(!(iucn == "red" & replication > 1)) %>%
  relocate(replication)


## calculate the mean values of traits for each replication
# log-transform trait values
dat_rare_mean <- dat_rare %>% 
  group_by(region, iucn, replication) %>%
  summarise(Max = mean(Max),
            Trophic = mean(Trophic),
            Growth = mean(Growth),
            Age = mean(Age),
            Length = mean(Length),
            Longevity = mean(Longevity)) %>%
  ungroup()

# the observed mean trait values for red species
dat_mean_red <- dat_rare_mean %>% 
  filter(iucn == "red")

# the mean trait values of random samples from species pool
dat_mean_rare_total <- dat_rare_mean %>% 
  filter(iucn == "total")


## For category variables, calculate proportion of species for each level 
# diet
dat_diet_prop <- dat_rare %>% 
  group_by(region, iucn, replication, Diet, sprich_red) %>% 
  summarise(sprich_group = n_distinct(species)) %>%
  mutate(pspp_group = 100*sprich_group/sprich_red) %>% 
  ungroup() %>%
  complete(replication, Diet, iucn, region, fill = list(sprich_group = 0, pspp_group =0))

dat_prop_diet_red <- dat_diet_prop %>% 
  filter(iucn == "red" & replication == 1) %>% 
  distinct()

dat_prop_diet_total <- dat_diet_prop %>% 
  filter(iucn != "red") %>%
  group_by(region, Diet) %>%
  summarise(diet_mean = mean(pspp_group),
            diet_q2.5 = quantile(pspp_group, probs = 0.025),
            diet_q97.5 = quantile(pspp_group, probs = 0.975)) %>%
  ungroup() %>%
  # combine with observed value
  left_join(dat_prop_diet_red %>% dplyr::select(region, Diet, pspp_group)) %>%
  mutate(signif = ifelse(pspp_group < diet_q2.5 | pspp_group > diet_q97.5, "*", "")) %>% 
  mutate(pspp_group_max = ifelse(diet_q97.5 > pspp_group, diet_q97.5, pspp_group))


# Habitat
dat_habitat_prop <- dat_rare %>% 
  group_by(region, iucn, replication, Habitat, sprich_red) %>% 
  summarise(sprich_group = n_distinct(species)) %>%
  mutate(pspp_group = 100*sprich_group/sprich_red) %>% 
  ungroup() %>%
  complete(replication, Habitat, iucn, region, fill = list(sprich_group = 0, pspp_group =0))

dat_prop_habitat_red <- dat_habitat_prop %>% 
  filter(iucn == "red" & replication == 1) %>% 
  distinct()

dat_prop_habitat_total <- dat_habitat_prop %>% 
  filter(iucn != "red") %>%
  group_by(region, Habitat) %>%
  summarise(habitat_mean = mean(pspp_group),
            habitat_q2.5 = quantile(pspp_group, probs = 0.025),
            habitat_q97.5 = quantile(pspp_group, probs = 0.975)) %>%
  ungroup() %>%
  # combine with observed value
  left_join(dat_prop_habitat_red %>% dplyr::select(region, Habitat, pspp_group)) %>%
  mutate(signif = ifelse(pspp_group < habitat_q2.5 | pspp_group > habitat_q97.5, "*", "")) %>% 
  # change the label
  # mutate(Habitat = ifelse(Habitat == "River-lake migratory", "RL-migratory", Habitat)) %>% 
  mutate(pspp_group_max = ifelse(habitat_q97.5 > pspp_group, habitat_q97.5, pspp_group))


# Body
dat_body_prop <- dat_rare %>% 
  group_by(region, iucn, replication, Body, sprich_red) %>% 
  summarise(sprich_group = n_distinct(species)) %>%
  mutate(pspp_group = 100*sprich_group/sprich_red) %>% 
  ungroup() %>%
  complete(replication, Body, iucn, region, fill = list(sprich_group = 0, pspp_group =0))

dat_prop_body_red <- dat_body_prop %>% 
  filter(iucn == "red" & replication == 1) %>% 
  distinct()

dat_prop_body_total <- dat_body_prop %>% 
  filter(iucn != "red") %>%
  group_by(region, Body) %>%
  summarise(body_mean = mean(pspp_group),
            body_q2.5 = quantile(pspp_group, probs = 0.025),
            body_q97.5 = quantile(pspp_group, probs = 0.975)) %>%
  ungroup() %>%
  # combine with observed value
  left_join(dat_prop_body_red %>% dplyr::select(region, Body, pspp_group)) %>%
  mutate(signif = ifelse(pspp_group < body_q2.5 | pspp_group > body_q97.5, "*", "")) %>% 
  # change the label
  # mutate(Body = ifelse(Body == "Dorsoventrally flattened", "Flattened", Body)) %>% 
  mutate(pspp_group_max = ifelse(body_q97.5 > pspp_group, body_q97.5, pspp_group))



###############
## Generate figures

###############
## figure: compare mean trait values of observed red species and random samples from species pool at the national scale

# Maximum body length
plot_max <- ggplot(dat_mean_rare_total %>% filter(region == "China")) +
  geom_density_ridges_gradient(aes(x = Max, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region == "China"), aes(xintercept = Max), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Maximum body length (cm)", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))
  
# Longevity
plot_longevity <- ggplot(dat_mean_rare_total %>% filter(region == "China")) +
  geom_density_ridges_gradient(aes(x = Longevity, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region == "China"), aes(xintercept = Longevity), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Longevity (yr)", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

# Growth rate
plot_growth <- ggplot(dat_mean_rare_total %>% filter(region == "China")) +
  geom_density_ridges_gradient(aes(x = Growth, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region == "China"), aes(xintercept = Growth), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Growth rate", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

# Length at maturation
plot_length <- ggplot(dat_mean_rare_total %>% filter(region == "China")) +
  geom_density_ridges_gradient(aes(x = Length, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region == "China"), aes(xintercept = Length), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Length at maturation (cm)", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

# Age at maturation
plot_age <- ggplot(dat_mean_rare_total %>% filter(region == "China")) +
  geom_density_ridges_gradient(aes(x = Age, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region == "China"), aes(xintercept = Age), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Age at maturation (yr)", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

# Trophic level
plot_trophic <- ggplot(dat_mean_rare_total %>% filter(region == "China")) +
  geom_density_ridges_gradient(aes(x = Trophic, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region == "China"), aes(xintercept = Trophic), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Trophic level", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))


# Habitat preference
plot_habitat <- ggplot(dat_prop_habitat_total %>% filter(region == "China")) +
  geom_linerange(aes(xmin = habitat_q2.5, xmax = habitat_q97.5, y = fct_rev(Habitat))) +
  geom_point(aes(x = habitat_mean, y = Habitat), size =1.5) +
  geom_point(aes(x = pspp_group, y = Habitat), shape = 17, colour = "red", size =1.5) +
  geom_text(aes(x = pspp_group_max, y = Habitat, label = signif), nudge_x = 2, size = 2.8) +
  labs(x = "Habitat preference", y = "") + 
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))


# Diet pattern
plot_diet <- ggplot(dat_prop_diet_total %>% filter(region == "China")) +
  geom_linerange(aes(xmin = diet_q2.5, xmax = diet_q97.5, y = fct_rev(Diet))) +
  geom_point(aes(x = diet_mean, y = Diet), size =1.5) +
  geom_point(aes(x = pspp_group, y = Diet), shape = 17, colour = "red", size =1.5) +
  geom_text(aes(x = pspp_group_max, y = Diet, label = signif), nudge_x = 2, size = 2.8) +
  labs(x = "Diet pattern", y = "") + 
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

# Body shape
plot_body <- ggplot(dat_prop_body_total %>% filter(region == "China")) +
  geom_linerange(aes(xmin = body_q2.5, xmax = body_q97.5, y = fct_rev(Body))) +
  geom_point(aes(x = body_mean, y = Body), size =1.5) +
  geom_point(aes(x = pspp_group, y = Body), shape = 17, colour = "red", size =1.5) +
  geom_text(aes(x = pspp_group_max, y = Body, label = signif), nudge_x = 2, size = 2.8) +
  labs(x = "Body shape", y = "") + 
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))


cowplot::plot_grid(plot_max, plot_longevity, plot_growth, 
                   plot_length, plot_age, plot_trophic,
                   plot_habitat, plot_diet, plot_body,
                   nrow = 3, labels = c(letters[1:9]), label_size = 10)

ggsave(file = "results/trait_obs_exp_China.png", 
       height = 180, width = 180, units = "mm")




########

# Maximum body length
ggplot(dat_mean_rare_total %>% filter(region != "China")) +
  facet_wrap(~ region, scales = "free") +
  geom_density_ridges_gradient(aes(x = Max, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.0, 0, 0.00,0.00)) +
  geom_vline(data = dat_mean_red %>% filter(region != "China"), aes(xintercept = Max), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Maximum body length (cm)", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(angle = 0, hjust = 0.5))

ggsave(file = "results/Ragional_trait_obs_exp_max_body_length.png", 
       height = 180, width = 180, units = "mm")


# Longevity
ggplot(dat_mean_rare_total %>% filter(region != "China")) +
  facet_wrap(~ region, scales = "free") +
  geom_density_ridges_gradient(aes(x = Longevity, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region != "China"), aes(xintercept = Longevity), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Longevity (yr)", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(angle = 0, hjust = 0.5))

ggsave(file = "results/Ragional_trait_obs_exp_longevity.png", 
       height = 180, width = 180, units = "mm")


# Growth rate
ggplot(dat_mean_rare_total %>% filter(region != "China")) +
  facet_wrap(~ region, scales = "free") +
  geom_density_ridges_gradient(aes(x = Growth, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region != "China"), aes(xintercept = Growth), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Growth rate", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(angle = 0, hjust = 0.5))

ggsave(file = "results/Ragional_trait_obs_exp_growth_rate.png", 
       height = 180, width = 180, units = "mm")


# Length at maturation
ggplot(dat_mean_rare_total %>% filter(region != "China")) +
  facet_wrap(~ region, scales = "free") +
  geom_density_ridges_gradient(aes(x = Length, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region != "China"), aes(xintercept = Length), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Length at maturation (cm)", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(angle = 0, hjust = 0.5))

ggsave(file = "results/Ragional_trait_obs_exp_length_maturation.png", 
       height = 180, width = 180, units = "mm")


# Age at maturation
ggplot(dat_mean_rare_total %>% filter(region != "China")) +
  facet_wrap(~ region, scales = "free") +
  geom_density_ridges_gradient(aes(x = Age, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region != "China"), aes(xintercept = Age), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Age at maturation (yr)", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(angle = 0, hjust = 0.5))

ggsave(file = "results/Ragional_trait_obs_exp_age_maturation.png", 
       height = 180, width = 180, units = "mm")


# Trophic level
ggplot(dat_mean_rare_total %>% filter(region != "China")) +
  facet_wrap(~ region, scales = "free") +
  geom_density_ridges_gradient(aes(x = Trophic, y = iucn, fill = stat(quantile)), 
                               quantiles = c(0.025, 0.975), calc_ecdf = T) + 
  scale_y_discrete(labels = NULL, expand = c(0.01, 0, 0,0)) +
  geom_vline(data = dat_mean_red %>% filter(region != "China"), aes(xintercept = Trophic), linetype = 1, size = 0.8, color = "red") +
  labs(x = "Trophic level", y = "") + 
  scale_fill_manual(name = 'Posterior probability',
                    values = c('#cccccc',  '#969696', '#cccccc')) +
  theme_classic() +
  theme(legend.position = "n",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(angle = 0, hjust = 0.5))

ggsave(file = "results/Ragional_trait_obs_exp_Trophic_level.png", 
       height = 180, width = 180, units = "mm")


# Habitat preference
ggplot(dat_prop_habitat_total %>% filter(region != "China")) +
  facet_wrap(~ region, scales = "fixed") +
  geom_linerange(aes(xmin = habitat_q2.5, xmax = habitat_q97.5, y = fct_rev(Habitat))) +
  geom_point(aes(x = habitat_mean, y = Habitat), size =1.5) +
  geom_point(aes(x = pspp_group, y = Habitat), shape = 17, colour = "red", size =1.5) +
  geom_text(aes(x = pspp_group_max, y = Habitat, label = signif), nudge_x = 2, size = 2.8) +
  labs(x = "Habitat preference", y = "") + 
  theme_bw() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file = "results/Ragional_trait_obs_exp_habitat_preference.png", 
       height = 180, width = 180, units = "mm")


# Diet pattern
ggplot(dat_prop_diet_total %>% filter(region != "China")) +
  facet_wrap(~ region, scales = "fixed") +
  geom_linerange(aes(xmin = diet_q2.5, xmax = diet_q97.5, y = fct_rev(Diet))) +
  geom_point(aes(x = diet_mean, y = Diet), size =1.5) +
  geom_point(aes(x = pspp_group, y = Diet), shape = 17, colour = "red", size =1.5) +
  geom_text(aes(x = pspp_group_max, y = Diet, label = signif), nudge_x = 2, size = 2.8) +
  labs(x = "Diet pattern", y = "") + 
  theme_bw() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file = "results/Ragional_trait_obs_exp_diet_pattern.png", 
       height = 180, width = 180, units = "mm")


# Body shape
ggplot(dat_prop_body_total %>% filter(region != "China")) +
  facet_wrap(~ region, scales = "fixed") +
  geom_linerange(aes(xmin = body_q2.5, xmax = body_q97.5, y = fct_rev(Body))) +
  geom_point(aes(x = body_mean, y = Body), size =1.5) +
  geom_point(aes(x = pspp_group, y = Body), shape = 17, colour = "red", size =1.5) +
  geom_text(aes(x = pspp_group_max, y = Body, label = signif), nudge_x = 2, size = 2.8) +
  labs(x = "Body shape", y = "") + 
  theme_bw() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file = "results/Ragional_trait_obs_exp_body_shape.png", 
       height = 180, width = 180, units = "mm")

