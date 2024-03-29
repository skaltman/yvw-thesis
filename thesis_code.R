# Libraries
library(readxl)
library(tidyverse)
library(langcog)
library(broom)
library(knitr)
library(citr)

#----------------------------------------------------------------------------------
# PARAMETERS

three_toy_path <- "~/Dropbox/YouVWorld/Data/YouVWorldData_forSophie.xlsx"
three_toy_sheet <- "DataUpdated"

two_toy_path <- "~/Dropbox/YouVWorld/TwoToys/data/YouvWorld_TwoToys_SB.xlsx"
two_toy_sheet <- "Data"

modified_path <- "~/Dropbox/YouVWorld/TwoToys/new_version_data/YouVWorld_modified_toy_updated.xlsx"
modified_sheet <- "Data"

sr_path <- "~/Dropbox/YouVWorld/allExperimentsAnalysis/data/YouVWorld_SocialReferenceData.xlsx"

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

three_toy_sr <- 
  read_xlsx(path = sr_path, sheet = "three-toy") %>% 
  rename_all(funs(str_replace_all(., " ", "_")))

two_toy_sr <-
  read_xlsx(path = sr_path, sheet = "two-toy") %>% 
  rename_all(funs(str_replace_all(., " ", "_")))
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

format_sr_data <- function(data, original_data) {
  data %>% 
    filter_at(vars(orient_body, orient_toy, gaze, point, verbal), 
              all_vars(!is.na(.))) %>% 
    left_join(original_data %>% select(videoName, condition, age), 
              by = "videoName") %>% 
    group_by(condition) %>% 
    mutate(condition_count = n()) %>%
    mutate_at(vars(verbal, gaze, point, contains("orient")), funs(as.numeric)) %>% 
    ungroup() %>% 
    gather(key = "behavior", 
           value = "demonstrates", 
           verbal, 
           gaze, 
           point, 
           orient_body, 
           orient_toy) %>% 
    select(videoName,
           age,
           condition,
           behavior,
           demonstrates,
           condition_count) %>% 
    group_by(condition, condition_count, behavior) %>% 
    summarise(num_demonstrates = sum(demonstrates)) %>% 
    ungroup() %>% 
    mutate(percentage = num_demonstrates / condition_count)
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
  num_participants <- data %>% nrow()
  
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
         title = title) + #,
         #caption = str_c("Figure", fig_num_counter, sep = " ")) +
    coord_fixed(ratio = 2/1)
}

plot_responses <- function(data, title_prefix = "") {
  proportion_by_condition_plot(data, 
                               firstChoice, 
                               "Confederate's toy", 
                               response_colors, 
                               str_c(title_prefix, "Response by condition"), 
                               "Target toy of first response", 
                               c("Other toy", "Confederate's toy"))
}

plot_helpfulness <- function(data, title_prefix = "") {
  proportion_by_condition_plot(data, 
                               helpfulCategory, 
                               "Helpful", 
                               helpfulness_colors, 
                               str_c(title_prefix, "Success of helping behavior by condition"), 
                               "Success of helping behavior", 
                               c("Unsuccessful", "Successful"))
}

plot_flip <- function(data, title_prefix = "") {
  proportion_by_condition_plot(data,
                               flip,
                               TRUE,
                               flip_colors,
                               str_c(title_prefix, "Button type targeted by condition"),
                               "Target button of first response",
                               c("Obvious", "Non-obvious"))
}

plot_predicted <- function(data, title_prefix = "") {
  proportion_by_condition_plot(data, 
                               as_predicted, 
                               1, 
                               predicted_colors, 
                               str_c(title_prefix, "Target matches prediction by condition"), 
                               "Target toy matches prediction", 
                               c("No", "Yes"))
}

plot_social_referencing <- function(data, title_prefix = "") {
  fig_num_counter <<- fig_num_counter + 1
  
  data %>% 
    mutate(behavior = str_to_title(str_replace_all(behavior, "_", "\n")),
           behavior = fct_relevel(behavior, 
                                  "Orient\nToy", 
                                  "Orient\nBody", 
                                  "Point", 
                                  "Gaze",
                                  "Verbal")
    ) %>% 
    ggplot(aes(behavior, percentage)) +
    geom_col() +
    facet_grid(~ condition) +
    theme_minimal() +
    # theme(axis.text.x = element_text(angle = 45)) +
    labs(x = "Behavior",
         y = "Percentage of children",
         title = str_c(title_prefix, "Social referencing behavior"),
         caption = str_c("Figure", fig_num_counter, sep = " ")) +
    coord_cartesian(ylim = c(0, 1)) 
}

plot_change_type <- function(data, title_prefix = "") {
  data %>% 
    mutate(change_toy = firstChoice == "Other toy",
           behavior_group = 
             case_when(
               change_toy & flip ~ "Change toy\nand action",
               change_toy ~ "Change\ntoy",
               flip ~ "Change\naction",
               TRUE ~ "Imitate"
              )
           ) %>% 
      group_by(condition, behavior_group) %>% 
      summarise(n = n()) %>% 
      mutate(percentage = n / sum(n)) %>% 
      ungroup() %>% 
      mutate(color = (condition == "Broken Toy" & str_detect(behavior_group, "toy")) |
               (condition == "Broken Button" & str_detect(behavior_group, "action"))) %>% 
      ggplot(aes(behavior_group, percentage, fill = color)) +
      geom_col() +
      facet_grid(~condition) +
      scale_fill_manual(breaks = c(TRUE, FALSE),
                        values = c("#dddddd", "#dc322f"),
                        labels = c("Yes", "No"),
                        name = "Helpful response") +
      coord_cartesian(ylim = c(0, 1)) +
      theme_minimal() +
      labs(x = "Behavior type",
           y = "Proportion of children",
           title = str_c(title_prefix, "title filler until i think of a title")) #add title in here
}

#----------------------------------------------------------------------------------
# CROSS EXPERIMENT ANALYSIS

shared_vars <- c("videoName", 
                 "n",
                 "condition",
                 "age",
                 "gender",
                 "firstChoice",
                 "firstBehaviorCode",
                 "helpfulCategory"
)

combined <-
    rbind(
      three_toy_tidy %>% select(shared_vars) %>% mutate(experiment = 1),
      two_toy_tidy %>% select(shared_vars) %>% mutate(experiment = 2),
      modified_tidy %>% select(shared_vars) %>% mutate(experiment = 3)
    )

all_twos <-
  combined %>% 
  filter(age %/% 1 == 2)

all_threes <-
  combined %>% 
  filter(age %/% 1 == 3)

#----------------------------------------------------------------------------------
# AGE AS PREDICTOR GLMS

choice_glm <- function(data) {
  glm(firstChoice ~ age + condition, 
      data = 
        data %>% 
        mutate(firstChoice = firstChoice == "Other toy")
      )  %>% 
    tidy() %>% 
    mutate(response = "First response")
}


correctness_glm <- function(data) {
  glm(firstChoiceCorrect ~ age, family = "binomial", 
      data = 
        data %>% 
        mutate(firstChoiceCorrect = as.integer(firstChoiceCorrect))) %>% 
    tidy() %>% 
    mutate(response = "Correctness")
}

helpfulness_glm <- function(data) {
  glm(helpfulCategory ~ age, family = "binomial", 
      data = 
        data %>% 
        mutate(helpfulCategory = helpfulCategory == "Helpful")) %>% 
    tidy() %>% 
    mutate(response = "Helpfulness")
}

flip_glm <- function(data) {
  glm(flip ~ age, family = "binomial", data = data) %>% 
    tidy() %>% 
    mutate(response = "Targets non-obvious button")
}

get_age_p_value <- function(glm_tibble) {
  glm_tibble %>% 
    filter(term == "age") %>% 
    pull(p.value)
}

tidy_all_glms <- function(data) {
  choice_glm(data) %>%
    rbind(correctness_glm(data)) %>% 
    rbind(helpfulness_glm(data)) %>% 
    rbind(flip_glm(data)) %>% 
    mutate(term = 
             term %>% 
             str_replace_all("[:punct:]", "") %>% 
             str_to_title()) %>% 
    mutate_if(is.numeric, funs(round), digits = 2) %>% 
    rename_all(str_to_title) %>% 
    rename(SE = Std.error,
           p_value = P.value)
}

create_glm_tibble <- function(data1, data2, data3) {
  tidy_all_glms(data1) %>% 
    mutate(Exp = 1) %>% 
    rbind(tidy_all_glms(data2) %>% mutate(Exp = 2)) %>% 
    rbind(tidy_all_glms(data3) %>% mutate(Exp = 3)) %>% 
    select(Experiment = Exp, everything())
}

create_p_value_tibble <- function(data) {
  tibble(
    choice = get_age_p_value(choice_glm(data)),
    correctness = get_age_p_value(correctness_glm(data)),
    helpfulness = get_age_p_value(helpfulness_glm(data)),
    flip = get_age_p_value(flip_glm(data))
  )
}


  
