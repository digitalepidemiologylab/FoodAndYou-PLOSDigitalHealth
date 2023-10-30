library(patchwork)
library(tidyverse)


fig5data <- read_csv("../../data/fig5/figure_5.csv")

energy <- fig5data[fig5data$var == "energy_kcal_eaten", ]
meat <- fig5data[fig5data$var == "meat_eaten", ]
dairy <- fig5data[fig5data$var == "dairy_eaten", ]
water <- fig5data[fig5data$var == "water_eaten", ]


menuch = read_csv("../../data/fig5/menu_ch.csv", show_col_types = FALSE)
menuch[, which(sapply(menuch, class)=='numeric')] = apply(menuch[, which(sapply(menuch, class)=='numeric')], 2, round, 2)
menuch = data.frame(menuch)
menuch[, 'dairy_eaten'] = menuch[, 'dairy_eaten']*100
menuch[, 'water_eaten'] = menuch[, 'water_eaten']*100

#####################################################################

#
# special case: fruits and veggies
#

sub = subset(long, var == 'fiveperday')

# split the data into sublists
subsub = split(sub ,sub[['strata_value']])

fay_fiveamtag = sapply(subsub, function(x){
  
  # get proportion of participants eating more than 600 g of fruits and vegetables a day
  n_above = nrow(x[x[, 'value']>600,])
  n_total = nrow(x)
  prop = n_above/n_total
  
  # round and prettify
  prop = pretty_percentages(prop)
  # 
  # calculate 95% CI
  ci = round(PropCIs::scoreci(x = n_above, n = n_total, conf.level = 0.95)$conf.int[1:2]*100, 2)
  lci = ci[1]
  uci = ci[2]
  # # put the stuff together
  # temp = data.frame(prop = prop,
  #            ci = ci)
  temp = data.frame(n_above, n_total, prop, lci, uci)
  
  return(temp)
})

fay_fiveamtag = data.frame(t(fay_fiveamtag))
fay_fiveamtag = data.frame(apply(fay_fiveamtag, 2, function(x) as.numeric(unlist(x))))
fay_fiveamtag[, 'cat'] = names(subsub)

# select the relevant variables for fruits and veggies
menuch = menuch[menuch[, "cat"] %in% labs, c('cat', 'prop_vegfru_eaten', 'n')]

# calculate the 95% ci
menuch[, 'n_above'] = menuch[, 'n']*(menuch[, "prop_vegfru_eaten"]/100)
menuch[, 'lci'] = sapply(1:nrow(menuch), function(i) round(PropCIs::scoreci(x = menuch[i, 'n_above'], n = menuch[i, 'n'], conf.level = 0.95)$conf.int[1]*100, 2))
menuch[, 'uci'] = sapply(1:nrow(menuch), function(i) round(PropCIs::scoreci(x = menuch[i, 'n_above'], n = menuch[i, 'n'], conf.level = 0.95)$conf.int[2]*100, 2))

# throw all the stuff in a df for ggplot
comp = data.frame(cat = c(fay_fiveamtag[, 'cat'], menuch[, "cat"]),
                  value = c(fay_fiveamtag[, 'prop'], menuch[, "prop_vegfru_eaten"]),
                  lci = c(fay_fiveamtag[, "lci"], menuch[, "lci"]),
                  uci = c(fay_fiveamtag[, "uci"], menuch[, "uci"]),
                  study = c(rep('Food and You', length(labs)), rep('MenuCH', length(labs))))
comp[, "cat"] = factor(comp[, "cat"],
                       ordered = T,
                       levels = labs,
                       labels = labs)



#####################################################################

# we concatenate them
fy <- 
  bind_rows(
    energy %>% mutate(variable = "Energy", unit = "kcal per day"),
    meat %>% mutate(variable = "Meat", unit = "g per day"),
    dairy %>% mutate(variable = "Dairy", unit = "g per day"),
    water %>% mutate(variable = "Water", unit = "ml per day"),
  ) %>% 
  mutate(variable = variable %>% factor(., levels = unique(variable)))

variables <- fy %>% select(variable, unit) %>% distinct()

# we define the strata + some cosmetic changes of the names
strata <- 
  energy %>% select(strata, strata_value) %>% distinct() %>% 
  mutate(
    Strata = 
      strata %>% 
      str_to_sentence() %>% 
      factor(., levels = c("Dataset", "Gender","Language","Age_group")) %>% 
      fct_recode(., `-` = "Dataset", `Age group` = "Age_group")
  ) %>% 
  arrange(Strata, strata_value) %>%
  mutate(
    Strata_value = strata_value %>% str_to_sentence(),
    Strata_value = Strata_value %>% factor(., levels = unique(Strata_value))
  )

fy <- fy %>% left_join(strata, by = join_by(strata, strata_value))

# menuCH values

menuch <- 
  menuch %>% 
  rename(strata_value = cat) %>% 
  dplyr::filter(strata_value %in% strata$strata_value)

menuch_long <- 
  menuch %>% select(-prop_vegfru_eaten) %>% 
  pivot_longer(-strata_value, values_to = "mean", names_to = "variable") %>% 
  mutate(
    variable = 
      variable %>% 
      str_remove("_eaten") %>% str_remove("_kcal") %>% 
      str_to_sentence() %>% 
      factor(., levels = variables$variable)
  ) %>% 
  left_join(strata, by = join_by(strata_value)) %>% 
  left_join(variables, by = join_by(variable))

menuch_long <- 
  menuch_long %>% 
  arrange(variable) %>% 
  mutate(
    Variable = str_c(variable, "\n[",unit,"]"),
    Variable = Variable %>% factor(., levels = unique(Variable))
  )


plot_comparison <- function(fy, menuch_long, selected_variable, strata_colors){
  
  fy_var <- fy %>% filter(variable == selected_variable)
  menuch_long_var <- menuch_long %>% filter(variable == selected_variable)
  fy_var_summary <- 
    fy_var %>% 
    group_by(Strata, Strata_value) %>% 
    summarize(mean = mean(value), .groups = "drop")
  fy_var_trimmed <- 
    fy_var %>% 
    group_by(Strata, Strata_value) %>% 
    mutate(upper_limit = quantile(value, 0.99)) %>% 
    ungroup() %>% 
    filter(value < upper_limit)
  
  ggplot(fy_var_trimmed, aes(x = Strata_value)) +
    geom_violin(aes(y = value, fill = Strata_value), col = NA, alpha = 0.5) +
    geom_point(data = fy_var_summary, aes(y = mean, col = Strata_value), size = 4) +
    geom_point(data = menuch_long_var, aes(y = mean), col = "black") +
    facet_grid(. ~ Strata, scales = "free", space = "free_x") +
    expand_limits(y = 0) +
    theme_light() +
    theme(
      strip.text.y = element_text(angle = 0, hjust = 0, color = "black"),
      strip.text.x = element_text(angle = 0, hjust = 0.5, color = "black"),
      strip.background = element_rect(color = NA, fill = "gray80"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_fill_manual(breaks = levels(fy_var$Strata_value), values = strata_colors) +
    scale_color_manual(breaks = levels(fy_var$Strata_value),values = strata_colors) +
    guides(fill = "none", color = "none") +
    xlab("") + ylab("") +  # ylab((fy_var$unit) %>% unique()) +
    ggtitle(str_c(selected_variable, " (",(fy_var$unit) %>% unique(),")")) # ggtitle(selected_variable)
  
}



strata_colors_3 <- 
  c("gray","khaki3","khaki4","orangered1","orangered3", 
    "lightskyblue1","lightskyblue2","lightskyblue3","lightskyblue4"  ) 

plots <- 
  purrr::map(
    .x = variables$variable, .f = plot_comparison, 
    fy = fy, menuch_long = menuch_long, strata_colors = strata_colors_3
  )


### Fruits and Veggies

fruitsveggie <- comp #fig5data[fig5data$var == "fiveperday", ]

fruitsveggie <- 
  fruitsveggie %>% 
  mutate(strata_value = cat) %>% 
  left_join(strata, by = join_by(strata_value))

g_Veggies <- 
  ggplot(fruitsveggie, aes(x = Strata_value, y=value, fill = study)) +
  geom_bar(stat= 'identity', position = 'dodge') +
  geom_errorbar(
    aes(ymin = lci, ymax = uci), 
    position=position_dodge(width=0.9), width = 0.2
  ) +
  xlab('Strata') +
  ylab('Proportion of participants [%]') +
  scale_fill_manual("Study", values = c('orange1', 'gray40')) +
  facet_grid(. ~ Strata, scales = "free", space = "free") +
  theme_light() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, color = "black"),
    strip.text.x = element_text(angle = 0, hjust = 0.5, color = "black"),
    strip.background = element_rect(color = NA, fill = "gray80"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggtitle('≥ 5 Portions fruits and vegetables per day') +
  xlab("")

### Microbiota

pcoa <- read_csv(file = "../../data/fig5/vegdist_mds.csv")

pcoa$id <- pcoa[,1]  %>% unlist()
pcoa <- pcoa %>% select(id, Axis.1, Axis.2)

pcoa <- 
  pcoa %>% 
  mutate(
    tmp = str_replace(id, "\\|.*", "") %>% str_replace(.,"_"," (") %>% str_c(., ")"),
    Study = 
      case_when(
        str_detect(id, "^FAY") ~ "Food and You", 
        TRUE ~ tmp 
      ),
    Study = Study %>% factor(., levels = unique(Study)) 
  )


study_colors <- c("black", "turquoise3", "gold","rosybrown1", "slategray3")

g_pcoa <- 
  ggplot(pcoa %>% arrange(fct_rev(Study)), #  %>% filter(Study == "Food and You"),
         aes(x = Axis.1, y = Axis.2, col = Study)) +
  geom_point(size = 0.75) +
  scale_color_manual(values = study_colors) +
  xlab("PCoA 1") +
  ylab("PCoA 2") +
  coord_fixed() + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
#coord_fixed(ratio = pcoa_res$values$Rel_corr_eig[2]/pcoa_res$values$Rel_corr_eig[1])


g_pcoa_panels <- 
  ggplot(pcoa %>% arrange(fct_rev(Study)), #  %>% filter(Study == "Food and You"),
         aes(x = Axis.1, y = Axis.2, col = Study)) +
  geom_point(data = pcoa %>% select(-Study), col = "gray90", size = 0.5) +
  geom_point(size = 0.25) +
  facet_wrap(Study ~ ., nrow = 3) +
  scale_color_manual(values = study_colors) +
  guides(col = "none") +
  xlab("PCoA 1") +
  ylab("PCoA 2") +
  coord_fixed() 
#  coord_fixed(ratio = pcoa_res$values$Rel_corr_eig[2]/pcoa_res$values$Rel_corr_eig[1])





fig_5 <- 
  plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] +
  g_Veggies + g_pcoa + 
  plot_layout(
    design = "
    15
    25
    36
    46
    "
  )



ggsave(plot = fig_5, filename = "../../images/fig_5.png",
       height = 12, width = 17, unit = "cm", scale = 2.2)

