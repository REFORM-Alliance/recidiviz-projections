rm(list = ls())


####Read in Libraries####
library(tidyverse)
library(data.table)
library(here)
library(janitor)
library(DBI)
library(RPostgres)
library(dbplyr)
library(scales)
library(ggrepel)
library(ggbump)


####Read in Data####
##Raw Recidiviz Projections
recidiviz_df <- 
  "data-raw" %>% 
  here("recidiviz-projections-total.csv") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  mutate(across(c("life_years", "people_impacted", 
                  starts_with("lower_ci"), 
                  starts_with("upper_ci")), 
                ~.x %>% 
                  str_replace_all(",", "") %>% 
                  as.numeric())) %>% 
  mutate(across(where(is.character), 
                ~.x %>% 
                  na_if(""))) %>% 
  rename("lower_ci_life_years" = "lower_ci",
         "upper_ci_life_years" = "upper_ci", 
         "lower_ci_people_impacted" = "lower_ci_2",
         "upper_ci_people_impacted" = "upper_ci_2") %>% 
  pivot_longer(cols = c(starts_with("lower_ci"), 
                        starts_with("upper_ci"), 
                        "life_years", "people_impacted"), 
               names_to = "names", 
               values_to = "values") %>% 
  mutate(ci_flag = 
           names %>% 
           str_extract("lower_ci|upper_ci") %>% 
           replace_na("estimate"),
         names = 
           names %>% 
           str_extract("life_years|people_impacted")) %>% 
  pivot_wider(names_from = ci_flag, 
              values_from = values) %>% 
  rename("estimate_type" = "names") %>% 
  relocate("estimate", .before = "lower_ci")


####Visualizations####
##Estimates by State
state_viz_by_estimate_time <- 
  recidiviz_df %>%
  filter(estimate_type == "people_impacted") %>%
  group_by(estimate_time_frame, state) %>%
  summarize(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(estimate_time_frame = str_replace_all(estimate_time_frame, 
                                               "ten years", "10-Year Estimate") %>%
           str_replace_all("Sep-25", "September 2025 Estimate") %>% 
           str_replace_all("^September 2025$", "September 2025 Estimate")) %>%
  left_join(data.frame(state = state.name, state_abb = state.abb), by = "state") %>%
  na.omit() %>%
  filter(estimate_time_frame == "September 2025 Estimate") %>%
  arrange(state_abb) %>%  
  mutate(state_abb = factor(state_abb, levels = rev(state_abb))) %>%
  
  mutate(total = sum(estimate),
         pct = estimate / total,
         is_small = pct < 0.08) %>%
  
  # Calculate stacked positions manually
  arrange(desc(state_abb)) %>%
  mutate(y_pos = cumsum(estimate) - estimate/2) %>%
  
  ggplot(aes(x = factor(estimate_time_frame), y = estimate, fill = state_abb)) +
  geom_bar(stat = "identity", color = "white", width = 0.25) +
  coord_flip() +
  
  # # Labels for larger segments (inside the bars)
  # geom_text(
  #   data = . %>% filter(!is_small),
  #   aes(y = y_pos, label = paste0(state_abb, ":\n", comma(estimate))),
  #   color = "black",
  #   size = 3
  # ) +
  # # Labels for smaller segments (with leader lines)
  # geom_text_repel(
  #   data = . %>% filter(is_small),
  #   aes(y = y_pos, label = paste0(state_abb, ":\n", comma(estimate))),
  #   direction = "x",
  #   hjust = 0,
  #   segment.color = "gray40",
  #   segment.size = 0.3,
  #   min.segment.length = 0,
  #   size = 3,
  #   xlim = c(1.3, NA),
  #   force = 1
  # ) +
  
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    y = "Estimate (People Impacted)",
    title = "Estimated People Impacted by State, as of September 2025",
    fill = "State"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(here("plots", "state_viz_by_estimate_time.png"), plot = state_viz_by_estimate_time, width = 8, height = 6, dpi = 300)

# ##Sankey Diagram
# sankey_plot <- 
#   recidiviz_df %>%
#   filter(estimate_type == "people_impacted") %>%
#   group_by(estimate_time_frame, state) %>%
#   summarize(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
#   ungroup() %>%
#   mutate(estimate_time_frame = str_replace_all(estimate_time_frame, 
#                                                "ten years", "10-Year Estimate") %>%
#            str_replace_all("September 2025", "September 2025 Estimate")) %>%
#   left_join(data.frame(state = state.name, state_abb = state.abb), by = "state") %>%
#   na.omit() %>%
#   # filter(estimate_time_frame == "September 2025 Estimate") %>%
#   arrange(state_abb) %>% 
#   ggplot(aes(x = estimate_time_frame, y = estimate, color = state_abb, value = estimate)) +
#   geom_bump(aes(group = state), smooth = 8, size = 2) +  # smooth gives you the "Sankey" flow feel
#   geom_point(size = 4) +
#   scale_x_discrete(expand = expansion(add = c(0.2, 0.2))) +
#   labs(
#     title = "Sankey Bump Chart: Change by State",
#     x = "",
#     y = "Estimate Value"
#   ) +
#   theme_minimal(base_size = 13)
