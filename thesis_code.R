# Libraries
library(readxl)
library(tidyverse)
library(langcog)
library(broom)
library(knitr)

#----------------------------------------------------------------------------------
# PARAMETERS

three_toy_path <- "~/Dropbox/YouVWorld/Data/YouVWorldData_forSophie.xlsx"
three_toy_sheet <- "DataUpdated"

two_toy_path <- "~/Dropbox/YouVWorld/TwoToys/data/YouvWorld_TwoToys_SB.xlsx"
two_toy_sheet <- "Data"

modified_path <- "~/Dropbox/YouVWorld/TwoToys/new_version_data/YouVWorld_modified_toy_updated.xlsx"
modified_sheet <- "Data"

# COLORS FOR PLOTS
response_colors <- c("#dc322f", "#268bd2")
helpfulness_colors <- c("#dddddd", "#addd8e")
flip_colors <- c("#ffdb69", "#f08226")
predicted_colors <- c("#dddddd", "#8856a7")

# Global variables
# Counts the number of figures so that you don't have to hand caption them
fig_num_counter <- 1

#----------------------------------------------------------------------------------
# AUTOMATED FIGURE NUMBER COUNTING

caption_figure <- function() {
  fig_num_counter <<- fig_num_counter + 1
  return(fig_num_counter)
}

#----------------------------------------------------------------------------------
# READ IN DATA

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
#----------------------------------------------------------------------------------
# FORMAT DATA FUNCTION -- MAKES SURE ALL DATA FRAMES HAVE SAME VARIABLE NAMES, ETC.

format_data <- function(data) {
  data %>% 
    rename_all(funs(str_replace(., "1st", "first"))) %>% 
    rename(exclude = `exclude?`) %>% 
    mutate(condition = case_when(condition == "wrong action" ~ "Broken Button",
                                 condition == "broken toy" ~ "Broken Toy",
                                 condition == "person" ~ "Broken Button",
                                 condition == "toy" ~ "Broken Toy"),
           condition = fct_relevel(condition, "Broken Toy", "Broken Button"),
           #condition = fct_rev(condition),
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

#----------------------------------------------------------------------------------
# SUMMARY STAT HELPER FUNCTIONS

# Returns a tibble filtered by ... (can be nothing if you want) with either the mean or sd of stat
get_summ_stat <- function(data, stat, summary_var, ..., num_digits = 2){
  summary_var <- enquo(summary_var)
  stat <- enquo(stat)
  
  data %>% 
    filter(...) %>% 
    summarise(mean = round(mean(!!summary_var), num_digits),
              sd = round(sd(!!summary_var), num_digits)) %>% 
    pull(!!stat)
}

# Returns the percentage of female participants in data, filtered by ... if supplied
get_percentage_female <- function(data, ..., num_digits = 0){
  data %>% 
    filter(gender == "F", ...) %>% 
    summarise(percentage = round(n() / num_participants * 100, 0)) %>% 
    pull(percentage)
}

# Returns a with various summary statistics -- mean and sd for full data and by condition, percentage female, num participants and num per condition
summary_tibble <- function(data) {
  tibble(
    num_participants = data %>% nrow(),
    num_bb = data %>% filter(condition == "Broken Button") %>% nrow(),
    num_bt = data %>% filter(condition == "Broken Toy") %>% nrow(),
    perc_female = get_percentage_female(data),
    mean_age = data %>% get_summ_stat(mean, age),
    sd_age = data %>% get_summ_stat(sd, age),
    mean_age_bt = data %>% get_summ_stat(mean, age, condition == "Broken Toy"),
    mean_age_bb = data %>% get_summ_stat(mean, age, condition == "Broken Button"),
    sd_age_bt = data %>% get_summ_stat(sd, age, condition == "Broken Toy"),
    sd_age_bb = data %>% get_summ_stat(sd, age, condition == "Broken Button"),
    num_bing = data %>% filter(str_detect(location, "Bing")) %>% nrow(),
    num_jmz = data %>% filter(location == "JMZ") %>% nrow(),
    mean_age_bing = data %>% get_summ_stat(mean, age, str_detect(location, "Bing")),
    mean_age_jmz = data %>% get_summ_stat(mean, age, location == "JMZ"),
    sd_age_bing = data %>% get_summ_stat(sd, age, str_detect(location, "Bing")),
    sd_age_jmz = data %>% get_summ_stat(sd, age, location == "JMZ")
  )
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

#----------------------------------------------------------------------------------
# BETWEEN CONDITION HELPER FUNCTIONS

# Returns a tibble with the number + percentage of children in each condition, variable group
format_for_between_stats <- function(data, variable, variable_option) {
  variable <- enquo(variable)
  
  data %>%
    mutate(condition = fct_recode(as.factor(condition), 
                                  bt = "Broken Toy",
                                  bb = "Broken Button")) %>% #this makes it easier to access variables later -- no longer have a space
    group_by(condition, !!variable) %>%
    summarise(n = n()) %>%
    mutate(percentage = n/sum(n) * 100) %>%
    ungroup() %>% 
    filter(!!variable == variable_option) %>% 
    unite("stat", condition, !!variable) %>% 
    mutate(num_bb = .$n[[2]],
           num_bt = .$n[[1]]) %>% 
    select(-n) %>% 
    spread(stat, percentage) %>% 
    mutate_all(~ round(., 0))
}

# Returns a p-value from a fisher's exact test conducted on the table
get_fishers_p_value <- function(table, digits = 4) {
  fisher.test(table)[[1]] %>% round(digits)
}

# Returns a tibble with 5 variables -- num_bb, num_bt, perc_bb, perc_bt, and p_value
# num_bb and num_bt are the number of children with variable == variable_option in each condition (e.g., firstChoice == "Confederate's toy")
# perc_bb and perc_bt are num_bb and num_bt divided by the number of children in each condition
# p_value is the p-value from a fisher's exact test that tests for differences in `variable` by condition
# This will work for first reponse, helpfulness, correctness, etc.
between_condition_stats <- function(data, variable, variable_option, digits = 4) {
  variable <- enquo(variable)
  
  data %>%
    format_for_between_stats(variable = !!variable,
                             variable_option = variable_option) %>%
    rename_at(vars(contains(variable_option)), 
              funs(str_c("perc_", str_extract(., "[a-z][a-z]")))) %>% 
    mutate(p_value =
             get_fishers_p_value(
               table(data$condition, data %>% pull(!!variable)),
               digits = digits))
}

between_condition_helpful <- function(data, digits = 4) {
  between_condition_stats(data, helpfulCategory, "Helpful", digits = digits)
}

between_condition_response <- function(data, digits = 4) {
  between_condition_stats(data, firstChoice, "Other toy", digits = digits)
}

between_condition_correct <- function(data, digits = 4) {
  between_condition_stats(data, firstChoiceCorrect, "1", digits = digits)
}

between_condition_flip <- function(data, digits = 4) {
  between_condition_stats(data, flip, "TRUE", digits = digits)
}

#----------------------------------------------------------------------------------
# WITHIN CONDITION STATS HELPER FUNCTIONS

get_binom_p_value <- function(k, n, digits = 4, alternative = "two.sided") {
  binom.test(k, n, alternative = alternative)$p.value %>% 
    round(digits)
}

format_for_within_stats <- function(data, variable, variable_option) {
  variable = enquo(variable)
  
  data %>% 
    mutate(condition = fct_recode(as.factor(condition), 
                                  bt = "Broken Toy",
                                  bb = "Broken Button")) %>% #this makes it easier to access variables later -- no longer have a space
    group_by(condition, !!variable) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    filter(!!variable == variable_option) %>% 
    unite("stat", condition, !!variable) %>% 
    spread(stat, n)
}

within_condition_stats <- function(data, variable, variable_option, digits = 4) {
  total_bt <- data %>% filter(condition == "Broken Toy") %>% nrow()
  total_bb <- data %>% filter(condition == "Broken Button") %>% nrow()
  
  variable = enquo(variable)
  
  data %>% 
    format_for_within_stats(variable = !!variable,
                            variable_option = variable_option) %>% 
    rename_all(funs(str_c("num_", str_extract(., "[a-z][a-z]")))) %>% 
    mutate(num_total = num_bb + num_bt,
           perc_total = num_total / (total_bt + total_bb) %>% round(0),
           p_value_bt = get_binom_p_value(num_bt, total_bt, digits = digits),
           p_value_bb = get_binom_p_value(num_bb, total_bb, digits = digits)) 
}

within_condition_helpful <- function(data, digits = 4) {
  within_condition_stats(data, helpfulCategory, "Helpful", digits = digits)
}

within_condition_response <- function(data, digits = 4) {
  within_condition_stats(data, firstChoice, "Other toy", digits = digits)
}

within_condition_flip <- function(data, digits = 4) {
  within_condition_stats(data, flip, "TRUE", digits = digits)
}

#----------------------------------------------------------------------------------
# FORMAT DATA

three_toy_tidy <-
  three_toy %>% 
  format_data() %>% 
  filter(exclude != "yes",
         between(ageBucket, 2, 3),
         n != 109) %>% 
  mutate(firstChoiceCorrect = as.integer(firstChoiceCorrect)) %>% 
  select(-contains("second"),
         -contains("third"),
         -contains("fourth"),
         -contains("language"),
         -contains("?"),
         -contains("child's choice matches")) %>% 
  rename(helpfulCategory = HelpfulCategory)

two_toy_tidy <-
  two_toy %>% 
  format_data() %>% 
  filter(exclude == "no") %>% 
  mutate(helpfulCategory = HelpfulCategory,
         firstChoiceCorrect = as.integer(firstChoiceCorrect))

modified_tidy <-
  modified %>% 
  format_data() %>% 
  filter(exclude != "yes") %>% 
  rename(helpfulCategory = HelpfulCategory) %>% 
  mutate(as_predicted = 
           case_when(
            condition == "Broken Toy" & firstChoice == "Other toy" ~ 1,
            condition == "Broken Button" & firstChoice == "Confederate's toy" ~ 1,
            TRUE ~ 0
           )
         ) %>% 
  filter(n != 109,
         n != 110)

#----------------------------------------------------------------------------------
# PLOT HELPER FUNCTIONS

# Returns a tibble that includes bootstrapped CI for the specified variable
# Variable option is the reference value of the specified variable (i.e., "Other toy")
get_bootstrapped_ci <- function(data, variable, variable_option) {
  variable <- enquo(variable)
  
  data %>%
    mutate(var_num = if_else(!!variable == variable_option, 1, 0)) %>%
    group_by(condition) %>%
    multi_boot_standard(col = "var_num") %>%
    mutate(kids_in_variable_option = 1 - mean) %>% 
    ungroup()
}

proportion_by_condition_plot <- function(data, variable, variable_option_1, colors, title, legend_name, labels) {
  fig_num_counter <<- fig_num_counter + 1
  variable <- enquo(variable)
  
  data %>% 
    get_bootstrapped_ci(!!variable, variable_option_1) %>% 
    gather(key, val, mean, kids_in_variable_option) %>% 
    ggplot(aes(x = condition, y = val, fill = key)) +
    geom_col(position = "fill", width = .7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .1) +
    geom_hline(yintercept = .5, linetype = "dashed") +
    scale_fill_manual(name = legend_name, 
                      values = colors,
                      breaks = c("kids_in_variable_option", "mean"),
                      labels = labels) +
    theme_minimal() + 
    labs(x = "Condition",
         y = "Proportion of children",
         title = title,
         caption = str_c("Figure", fig_num_counter, sep = " ")) +
    coord_fixed(ratio = 2/1)
}

plot_responses <- function(data) {
  proportion_by_condition_plot(data, 
                               firstChoice, 
                               "Confederate's toy", 
                               response_colors, 
                               "Response by condition", 
                               "Target toy of first response", 
                               c("Other toy", "Confederate's toy"))
}

plot_helpfulness <- function(data) {
  proportion_by_condition_plot(data, 
                               helpfulCategory, 
                               "Helpful", 
                               helpfulness_colors, 
                               "Success of helping behavior by condition", 
                               "Success of helping behavior", 
                               c("Unsuccessful", "Successful"))
}

plot_flip <- function(data) {
  proportion_by_condition_plot(data,
                               flip,
                               TRUE,
                               flip_colors,
                               "Button type targeted by condition",
                               "Target button of first response",
                               c("Obvious", "Non-obvious"))
}

plot_predicted <- function(data) {
  proportion_by_condition_plot(data, 
                               as_predicted, 
                               1, 
                               predicted_colors, 
                               "", 
                               "Target toy matches prediction", 
                               c("No", "Yes"))
}
