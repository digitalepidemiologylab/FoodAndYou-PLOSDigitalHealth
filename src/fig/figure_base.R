


plot_png <- function(path, padding){
  image <- magick::image_read(path)
  image_ratio <-
    magick::image_info(image)$height / magick::image_info(image)$width 
  
  ggplot() + 
    coord_fixed(ratio = image_ratio) +
    ggpubr::background_image(image) + 
    theme_minimal() +
    theme(plot.margin = ggplot2::margin(t = padding, l = padding, r = 0, b = padding, unit = "pt"))
  
}


######## FIGURE 3 ######


process_fig3_data <- function(df){
  df %>% 
    pivot_longer(
      cols = -days, 
      names_to = "var_raw",
      values_to = "count"
    ) %>% 
    mutate(
      cohort = 
        ifelse(str_detect(var_raw, "_B"),"B", "C") %>% 
        factor(., levels = c("B","C")),
      var = str_remove(var_raw, "\\_[BC].*"),
      variable = 
        case_when(
          var == "cgm" ~ "CGM",
          var == "kcal" ~ "Food intake",
          str_detect(var, "pa") ~ "Activities",
          str_detect(var, "survey") ~ "Survey",
          str_detect(var, "sample") ~ "Other"
        ) %>% 
        factor(
          ., levels = c("CGM","Food intake", "Activities",
                        "Survey","Other")
        ),
      group = 
        case_when(
          str_detect(var_raw, "all") ~ "all",
          (var == "kcal") & str_detect(var_raw, "ok") ~ "≥ 1000 kcal",
          (var == "cgm") & str_detect(var_raw, "ok") ~ "≥ 90% time points",
          str_detect(var_raw, "sleep") ~ "Sleep",
          str_detect(var_raw, "activity") ~ "Physical activity",
          str_detect(var_raw, "morning") ~ "Morning",
          str_detect(var_raw, "evening") ~ "Evening",
          str_detect(var_raw, "gdrink") ~ "Glucose drink",
          str_detect(var_raw, "sbreakfast") ~ "Standardized breakfast",
          str_detect(var_raw, "mb") ~ "Stool sample (gut microbiota)",
          TRUE ~ ""
        )
    )
}



get_cohort_colors <- function(cohort) {
  if (cohort == "B") colors <- c("orange", "darkorange3")
  if (cohort == "C") colors <- c("purple1","purple4")
  colors
}


plot_fig3_distr <- function(df_long, variable, cohort) {
  
  selected_variable <- variable
  selected_cohort <- cohort
  
  days_max <- ifelse(selected_cohort == "B", 28, 40)
  study_dur <- ifelse(selected_cohort == "B", 14, 28)
  
  data <- 
    df_long %>% 
    filter(variable == selected_variable, cohort == selected_cohort) %>% 
    mutate(
      x = (days %>% as.integer()) - 1, x = pmin(x, days_max + 1),
      Cohort = str_c("cohort ", cohort)
           )
  
  colors <- get_cohort_colors(cohort = selected_cohort)
  
  breaks <- c(seq(0, days_max, by = 7), days_max, days_max + 1) %>% unique()
  labels <- c(seq(0, days_max, by = 7), days_max, "+") %>% unique()
  
  g <- 
    ggplot(data, aes(x = x, y = count, fill = group)) + 
    geom_vline(xintercept = study_dur, col = "gray80", 
               alpha = 0.4, linewidth = 4) +
    geom_bar(stat = "identity", position = "dodge", width = 0.8) +
    ylab("Nb of participants") +
    xlab("Total nb of days") +
    scale_fill_manual("", values = colors) +
    scale_x_continuous(
      breaks = breaks, labels = labels, minor_breaks = 0:days_max,
      limits = c(-0.6, days_max + 1 + 0.6)
    ) +
    coord_cartesian(xlim = c(0.5, days_max + 0.5)) +
#    facet_grid(Cohort + variable  ~ .)  +
    ggtitle(str_c(selected_variable, " - Cohort ", selected_cohort)) +
    theme(
      legend.position = c(0.05,1), 
      legend.justification = c(0,1),
      legend.background = 
        element_rect(color = "gray80", fill = "white", linewidth = 0.3),
      legend.title = element_blank(),
      strip.background = element_rect(fill = "gray", color = "white"),
      strip.text = element_text(color = "gray20", face = 2),   
      plot.title = element_text(size = 11)
      )
  
  if (length(unique(data$group)) == 1) {
    g <- g + guides(fill = "none")
  }
  
  g
}

plot_fig3_samples <- function(df_long, variable, cohort){
  
  selected_variable <- variable
  selected_cohort <- cohort
  
  data <- 
    df_long %>% 
    filter(
      variable == selected_variable, 
      cohort == selected_cohort, 
      !is.na(count)
    ) %>% 
    mutate(Number = days)
  
  color <- get_cohort_colors(cohort = selected_cohort)[1]
  
  ggplot(data, aes(x = Number, y = count)) +
    geom_bar(stat = "identity", fill = color) +
    facet_grid(. ~ str_wrap(group, width = 12), scales = "free", space = "free") +
    ylab("Nb of participants") +
    theme(strip.background = element_rect(fill = "gray60"))
  
  
}




######## FIGURE 4 ######

food_binning_from_fig4data <- function(fig4_data) {
  
  # Filter out relevant columns
  food_data <- fig4_data %>% select(hr, starts_with("food_"))
  
  # Melt the data
  melted_data <- food_data %>%
    gather(key = "eaten_at_day", value = "n", -hr) %>%
    mutate(eaten_at_day = sub("food_", "", eaten_at_day)) %>%
    rename(eaten_at_hour = hr) %>%
    arrange(eaten_at_hour, eaten_at_day) %>%
    filter(!is.na(n))
  
  # Ensure that the day names are lowercase
  melted_data$eaten_at_day <- tolower(melted_data$eaten_at_day)
  
  # Convert the day names to ordered factor with lowercase levels
  day_levels_lower <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
  melted_data$eaten_at_day <- factor(melted_data$eaten_at_day, levels = day_levels_lower, ordered = TRUE)
  
  # Arrange data by the ordered factor
  melted_data <- melted_data %>%
    arrange(eaten_at_hour, eaten_at_day)
  
  # Convert the first letter to uppercase
  melted_data$eaten_at_day <- tools::toTitleCase(as.character(melted_data$eaten_at_day))
  
  # Convert back to an ordered factor with uppercase levels
  day_levels_upper <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  melted_data$eaten_at_day <- factor(melted_data$eaten_at_day, levels = day_levels_upper, ordered = TRUE)
  
  return(melted_data)
}


process_food_timing_data <- function(raw_food_timing_data) {
  raw_food_timing_data %>% 
    mutate(
      day_type = 
        case_when(
          ((eaten_at_day == "Fri") & (eaten_at_hour >= 17)) |
            ((eaten_at_day == "Sat")) |
            ((eaten_at_day == "Sun") & (eaten_at_hour < 17)) ~
            "Weekend",
          TRUE ~ "Weekday"
        )
    )
}

get_weekday_colors <- function(nb = c(2, 7)){
  weekday_colors <- 
    c("gray50","gray40","gray30","gray20","black",
      "steelblue1", "steelblue2", "steelblue3")
  if (nb == 2) res <- weekday_colors[c(5,7)] else res <- weekday_colors[-7]
  res
}


plot_food_timing_distribution <- function(food_timing_data){
  ggplot(food_timing_data, 
         aes(x = eaten_at_hour, y = n, fill = eaten_at_day, group = eaten_at_day)
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_color_manual("", values = get_weekday_colors(7)) +
    scale_fill_manual("", values = get_weekday_colors(7)) +
    scale_x_continuous(
      "Time of the day [h]", breaks = seq(0, 24, by = 4), limits = c(-0.1, 24.1)
    ) +
    ylab("Average # of logged dishes")
}


pivot_longer_day_type <- function(fig4_data) {
  fig4_data %>% 
    pivot_longer(
      cols = -c(hr, starts_with("food")), 
      names_to = c("variable","day_type"), 
      names_pattern = "(.*)_(w.)", 
      names_transform = list(
        day_type = ~ readr::parse_factor(.x, levels = c("wk", "we")) %>% 
          fct_recode(weekday = "wk", weekend = "we")
      ),
      values_to = "value"
    ) %>% 
    pivot_wider(
      id_cols = c(hr, day_type),
      names_from = variable, values_from = value
    ) 
  # it feels silly to do a pivot_wider after a pivot_longer. There is probably a way to do a single pivot_longer, but that works for now.
}


plot_glucose_timing_distribution <- function(glucose_timing_data){
  ggplot(glucose_timing_data, aes(x = hr)) +
    geom_ribbon(
      aes(ymin = cgm_mean - cgm_stdv, 
          ymax = cgm_mean + cgm_stdv,
          fill = day_type, col = day_type), 
      alpha = 0.2, linewidth = 0.1
    ) +
    geom_line(aes(y = cgm_mean, col = day_type),linewidth = 1) +
    scale_color_manual("", values = get_weekday_colors(nb = 2)) +
    scale_fill_manual("",values = get_weekday_colors(nb = 2)) +
    scale_x_continuous(
      "Time of the day [h]", breaks = seq(0, 24, by = 4), limits = c(-0.1, 24.1)
    ) +
    ylab('Glucose [mmol/l]') 
}


plot_sleep_start_and_end <- function(sleep_timing_data){
  
  sleep_timing_data <- 
    sleep_timing_data %>% 
    select(hr, day_type, starts_with("sleep_s")) %>% 
    pivot_longer(
      cols = starts_with("sleep_s"), 
      names_to = "time_type", 
      names_transform = list(
        time_type = ~ recode(.x, sleep_strt = "Falling asleep", sleep_stop = "Waking up") %>% 
          readr::parse_factor(., levels = c("Waking up", "Falling asleep")) 
      ),
      values_to = "f"
      ) %>% 
    group_by(day_type) %>% 
    mutate(scaled_f = f/sum(f)) %>% 
    ungroup() %>% 
    arrange(time_type, day_type, hr)
    
  time_res <- diff(sleep_timing_data$hr)[1]
  
  ggplot(sleep_timing_data %>% filter(f > 0) , 
         aes(x = hr, y = scaled_f, fill = day_type, color = day_type)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.5, width = time_res) +
    scale_x_continuous(
      'Time of the day [h]', 
      breaks = seq(0, 24, by = 4), 
      labels = seq(0, 24, by = 4),
      limits = c(-0.6, 23 + 0.6)
    ) +
    scale_fill_manual("", values = get_weekday_colors(nb = 2)) +
    scale_color_manual("", values = get_weekday_colors(nb = 2)) +
    ylab("Fraction of logs") +
    facet_grid(time_type ~ .) +
    theme(
      panel.background = element_rect(color = "black", fill = "transparent"),
      panel.spacing = unit(0, "pt")
    )
}



plot_sleep_duration <- function(sleep_timing_data) {
  sleep_timing_data <- 
    sleep_timing_data %>% 
    group_by(day_type) %>% 
    mutate(f = sleep_len/sum(sleep_len, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(day_type, hr)
  
  time_res <- diff(sleep_timing_data$hr)[1]
  
  ggplot(sleep_timing_data %>%  filter(!is.na(sleep_len)) , 
         aes(x = hr, y = f, fill = day_type, color = day_type)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.5, width = time_res) +
    scale_x_continuous(
      'Time of the day [h]', 
      breaks = seq(0, 24, by = 2), 
      labels = seq(0, 24, by = 2)
    ) +
    scale_fill_manual("", values = get_weekday_colors(nb = 2)) +
    scale_color_manual("", values = get_weekday_colors(nb = 2)) +
    ylab("Fraction of logs") 
}


process_peaks_exp_data <- function(peaks_exp_data){
  
  peaks_exp_data[, "threshold"] <- paste0(peaks_exp_data[, "threshold"], 'th percentile')
  peaks_exp_data[, 'prop_matching'] <- 1 - peaks_exp_data[, "prop_zeros"]
  peaks_exp_data[, 'key'] <- 
    dplyr::recode(
      peaks_exp_data[, 'key'], 
      'normal'='just before peak', 
      'zero_model' = '2h before peak\n(control)'
    )
  
  peaks_exp_data %>% as_tibble()
}

plot_peak_exp <- function(peaks_exp_data, thresh = 90){
  
  peaks_exp_data_th <- 
    peaks_exp_data %>% 
    filter(threshold == str_c(thresh, "th percentile"))
  
  n_participants <- 
    peaks_exp_data_th %>% 
    filter(key == "just before peak") %>% 
    nrow()
  
  t_test <- 
    t.test(data = peaks_exp_data_th, prop_matching ~ key, alternative = "less")
  sign <- cut(t_test$p.value, breaks = c(-1, 0.001, 0.01, 0.05, 0.1, 1), labels = c("***","**","*",".",""))
  
  
  ggplot(peaks_exp_data_th, 
         aes(x = key, y = prop_matching, fill = key)) + 
    scale_x_discrete() +
    # annotate(x = Inf, y = Inf, geom = "text", 
    #          hjust = 1, vjust = 1,
    #          label = str_c("n = ",n_participants)) +
    annotate(x = 1.5, y = Inf, geom = "text",
             hjust = 0.5, vjust = 1,
             label = sign) +
    geom_boxplot(outlier.size = 0.5) +
    xlab("") +
    ylab('Prop. of CGM peaks\npreceded by a food intake') +
    scale_fill_manual(values = c("gray", "purple")) +
    theme(legend.position = "none") +
    theme(strip.background = element_blank())
  
  
}
