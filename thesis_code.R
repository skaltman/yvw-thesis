
## @knitr params_helpers

library(tidyverse)
library(readxl)
library(langcog)
library(knitr)

# Parameters
three_toy_path <- "~/Dropbox/YouVWorld/Data/YouVWorldData_forSophie.xlsx"
three_toy_sheet <- "DataUpdated"

two_toy_path <- "~/Dropbox/YouVWorld/TwoToys/data/YouvWorld_TwoToys_SB.xlsx"
two_toy_sheet <- "Data"

modified_path <- "~/Dropbox/YouVWorld/TwoToys/new_version_data/YouVWorld_modified_toy_updated.xlsx"
modified_sheet <- "Data"

# Global variables
fig_num_counter <- 1

# Helper functions
get_summ_stat <- function(data, stat, summary_var, ..., num_digits = 2){
  summary_var <- enquo(summary_var)
  stat <- enquo(stat)
  
  data %>% 
    filter(...) %>% 
    summarise(mean = round(mean(!!summary_var), num_digits),
              sd = round(sd(!!summary_var)), num_digits) %>% 
    pull(!!stat)
}

get_percentage_female <- function(data, ..., num_digits = 0){
  data %>% 
    filter(gender == "F", ...) %>% 
    summarise(percentage = round(n() / num_participants * 100, 0)) %>% 
    pull(percentage)
}

get_num_excluded <- function(data, exclude_code_var, ...){
  exclude_code <- enquo(exclude_code_var)
  
  data %>% 
    filter(`exclude?` == "yes") %>% 
    count(!!exclude_code) %>% 
    spread(!!exclude_code, nn) %>% 
    select(...) %>% 
    mutate(total = as.integer(rowSums(.))) %>% 
    rename_all(funs(str_replace_all(., " ", "_")))
}

format_data <- function(data) {
  data %>% 
    rename_all(funs(str_replace(., "1st", "first"))) %>% 
    rename(exclude = `exclude?`) %>% 
    mutate(condition = case_when(condition == "wrong action" ~ "Broken Button",
                                 condition == "broken toy" ~ "Broken Toy",
                                 condition == "person" ~ "Broken Button",
                                 condition == "toy" ~ "Broken Toy"),
           condition = fct_rev(condition),
           HelpfulCategory = str_to_title(HelpfulCategory) %>% 
             str_trim(side = "both"),
           firstBehaviorCode = str_to_lower(firstBehaviorCode),
           firstChoice = case_when(firstChoice == "confed" ~ "Confederate's toy",
                                   firstChoice == "other" ~ "Other toy",
                                   firstChoice == "tray" ~ "Other toy",
                                   firstChoice == "toy" ~ "Confederate's toy"),
           firstChoiceNum = as.integer(firstChoice == "Confederate's toy"),
           flip = str_detect(firstBehaviorCode, "flip"),
           age = as.double(age)) 
}

# Results section helper functions

# Returns a tibble with the number + percentage of children in each condition, variable group
format_for_between_stats <- function(data, variable, variable_option) {
  variable <- enquo(variable)
  
  data %>%
    group_by(condition, !!variable) %>%
    summarise(n = n()) %>%
    mutate(percentage = n/sum(n) * 100) %>%
    ungroup() %>% 
    filter(!!variable == variable_option) %>% 
    unite("stat", condition, !!variable) %>% 
    mutate(num_bb_other = .$n[[1]],
           num_bt_other = .$n[[2]]) %>% 
    select(-n) %>% 
    spread(stat, percentage) %>% 
    mutate_all(~ round(., 0))
}

# Returns a p-value from a fisher's exact test 
get_fishers_p_value <- function(table, digits = 4) {
  fisher.test(table)[[1]] %>% round(digits)
}

# Returns stats (num, percentages, p-value) for response data
between_condition_response <- function(data) {
  choices <-
    data %>% 
    format_for_between_stats(variable = firstChoice, 
                             variable_option = "Other toy") %>% 
    rename(perc_bt_other = `Broken Toy_Other toy`,
           perc_bb_other = `Broken Button_Other toy`) %>% 
    mutate(p_value = get_fishers_p_value(
      table(data$condition, 
            data$firstChoice)))
}

# Returns stats (num, percentages, p-value) for helpfulness data
between_condition_helpful <- function(data) {
  helpful <- 
    data %>% 
    format_for_between_stats(variable = helpfulCategory,
                             variable_option = "Helpful") %>% 
    rename(perc_bb_helpful = `Broken Button_Helpful`,
           perc_bt_helpful = `Broken Toy_Helpful`) %>% 
    mutate(p_value = get_fishers_p_value(
      table(data$condition, 
            data$helpfulCategory)))
}

# Within condition stats helper functions
get_binom_p_value <- function(k, n, digits = 4) {
  binom.test(k, n, alternative = "two.sided")$p.value %>% 
    round(digits)
}

format_for_within_stats <- function(data, variable, variable_option) {
  variable = enquo(variable)
  
  data %>% 
    group_by(condition, !!variable) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    filter(!!variable == variable_option) %>% 
    unite("stat", condition, !!variable) %>% 
    spread(stat, n)
}

within_condition_response <- function(data, num_bt, num_bb) {
  three_toy_tidy %>% 
    format_for_within_stats(variable = firstChoice, 
                            variable_option = "Other toy") %>% 
    rename(num_bb_other = `Broken Button_Other toy`,
           num_bt_other = `Broken Toy_Other toy`) %>% 
    mutate(p_value_bt = get_binom_p_value(num_bt_other, num_bt),
           p_value_bb = get_binom_p_value(num_bb_other, num_bb))
}

within_condition_helpful <- function(data, num_bt, num_bb) {
  data %>% 
    format_for_within_stats(variable = helpfulCategory, 
                            variable_option = "Helpful") %>% 
    rename(num_bb_help = `Broken Button_Helpful`,
           num_bt_help = `Broken Toy_Helpful`) %>% 
    mutate(num_total_help = num_bb_help + num_bt_help,
           perc_total_help = num_total_help / (num_bt + num_bb) %>% round(0)) %>% 
    mutate(p_value_bt = get_binom_p_value(num_bt_help, num_bt),
           p_value_bb = get_binom_p_value(num_bb_help, num_bb))
}

# Plot functions

# Returns a tibble that includes bootstrapped CI for the specified variable
# Variable option is the reference value of the specified variable (i.e., "Other toy")
get_bootstrapped_ci <- function(data, variable, variable_option) {
  variable <- enquo(variable)
  
  data %>%
    mutate(var_num = if_else(!!variable == variable_option, 1, 0)) %>%
    group_by(condition) %>%
    multi_boot_standard(col = "var_num") %>%
    mutate(Confederate = 1 - mean) %>% 
    ungroup()
}

# Plots the response plot. Captions with figure_num
response_plot <- function(data) {
  fig_num_counter <<- fig_num_counter + 1
  
  data %>% 
    get_bootstrapped_ci(firstChoice, "Confederate's toy") %>% 
    gather(key, val, mean, Confederate) %>% 
    ggplot(aes(x = condition, y = val, fill = key)) +
    geom_col(position = "fill", width = .7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .1) +
    geom_hline(yintercept = .5, linetype = "dashed") +
    scale_fill_solarized(name = "Target toy of first response", 
                         breaks = c("Confederate", "mean"),
                         labels = c("Other toy", "Confederate's toy")) +
    theme_minimal() + 
    labs(x = "Condition",
         y = "Proportion of children",
         title = "Response by condition",
         caption = str_c("Figure", fig_num_counter, sep = " ")) +
    coord_fixed(ratio = 2/1)
}

help_plot <- function(data) {
  fig_num_counter <<- fig_num_counter + 1
  
  data %>% 
    get_bootstrapped_ci(helpfulCategory, "Helpful") %>% 
    mutate(unhelpful = 1 - mean) %>% 
    gather(key = "measure", value = "val", mean, unhelpful) %>% 
    mutate(measure = forcats::fct_rev(as.factor(measure))) %>% 
    ggplot(aes(x = condition, y = val, fill = measure)) +
    geom_col(width = .7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .1) +
    geom_hline(yintercept = .5, linetype = "dashed") +
    scale_fill_manual(breaks = c("mean", "unhelpful"), 
                      labels = c("Successful", "Unsuccessful"), 
                      name = "Success of helping behavior",
                      values = c("#dddddd", "#addd8e"))  +
    labs(x = "Condition",
         y = "Proportion of children",
         title = "Success of help by condition",
         caption = str_c("Figure", fig_num_counter, sep = " ")) +
    theme_minimal() +
    coord_fixed(ratio = 2/1)
}

# Read in data
three_toy <- 
  read_xlsx(path = three_toy_path, 
            sheet = three_toy_sheet) %>% 
  rename(excludeCode = exclude_code)   # want all excludeCode variables named the same thing

two_toy <- 
  read_xlsx(path = two_toy_path, 
            sheet = two_toy_sheet)

modified <- 
  read_xlsx(path = modified_path, 
            sheet = modified_sheet)