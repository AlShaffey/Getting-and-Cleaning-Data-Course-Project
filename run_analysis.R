## Holds the instructions required to achieve the Getting and Cleaning Data 
## Course Project.

## Please, use the file in a newly opened RStudio session.
rstudioapi::restartSession("conclusion <- sum_up(); conclusion")

## Loads a package and installs it if it's not installed.
load_up <- function(package_name){
  substituted <- substitute(package_name)
  characterized <- as.character(substituted)
  
  installed_packages <- installed.packages() 
  installed_package_names <- rownames(installed_packages)
  is_installed <- characterized %in% installed_package_names
  
  if (!is_installed) {
    install.packages(characterized)
  }
  library(characterized, character.only = T)
}
##

## collects, cleans, and summarizes the retrieved data sets.
sum_up <- function(){
  
  print("Reads sets.")
  
  ## Reads sets.
  load_up(readr)
  
  train <- read_delim(
    "./UCI HAR Dataset/train/X_train.txt"
    ,
    " "
    ,
    col_names = F
    ,
    col_types =  cols(.default = "c")
  )
  test <- 
    read_delim(
      "./UCI HAR Dataset/test/X_test.txt"
      ,
      " "
      ,
      col_names = F
      ,
      col_types =  cols(.default = "c")
    )
  ##
  
  print("Adds an is_train (T for Train / F for Test) identifier.")
  
  ## Adds an is_train (T for Train / F for Test) identifier.
  load_up(dplyr)
  
  mutated_train <- mutate(train, is_train = T)
  mutated_test <- mutate(test, is_train = F)
  ##
  
  print("Unions the training and the test sets to create one data set.")
  
  ## Unions the training and the test sets to create one data set.
  experiments <- rbind(mutated_train, mutated_test)
  ##
  
  print("Reads features.")
  
  ## Reads features. 
  features <- read_delim(
    "./UCI HAR Dataset/features.txt"
    ,
    " "
    ,
    col_names = F
    ,
    col_types =  cols(.default = "c")
  )
  ##
  
  print("Names experiments with features.")
  
  ## Names experiments with features.
  name_experiments <- function(experiments, features){ 
    features_names <- features[[2]]
    all_features_names <- c(features_names, "is_train")
    names(experiments) <- all_features_names
    experiments
  }
  ##
  
  experiments = name_experiments(experiments, features)
  
  print("Extracts only the measurements on the mean and standard deviation for each measurement.")
  
  ## Extracts only the measurements on the mean and standard deviation for each 
  ## measurement.
  mean_and_std <- 
    select(experiments, matches("Mean\\(\\)|std\\(\\)|is_train", ignore.case = T))
  ##
  
  print("Reads activity labels and set-activities.")
  
  ## Reads activity labels and set-activities. 
  train_activities <- 
    read_delim(
      "./UCI HAR Dataset/train/y_train.txt"
      ,
      " "
      ,
      col_names = F
      ,
      col_types =  cols(.default = "n")
    )
  test_activities <- 
    read_delim(
      "./UCI HAR Dataset/test/y_test.txt"
      , 
      " "
      ,
      col_names = F
      ,
      col_types =  cols(.default = "n")
    )
  ##
  
  print("Unions the training and test activities and joins the experiments with their activities.")
  
  ## Unions the training and test activities and joins the experiments with
  ## their activities.
  experiment_activities <- rbind(train_activities, test_activities)
  experiment_with_activities <- cbind(mean_and_std, experiment_activities)
  ##
  
  ## Merges the activities with the experiments.
  # experiment_merged_activities <- 
  #   inner_join(experiment_with_activities, activity_labels, by = "X1", copy = T)
  ##
  
  print("Labels the data set with descriptive variable names.")
  
  ## Labels the data set with descriptive variable names.
  label <- function(experiment_with_activities){
    load_up(stringr)
    
    ## Time.
    time_pattern <- "(?i)t([bg])"
    renamed_experiments <- rename_with(
      experiment_with_activities
      ,
      function(b){str_replace(b, time_pattern, "time\\1")}
      ,
      matches(time_pattern)
    )
    ## Frequency.
    frequency_pattern <- "(?i)f([bg])"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){str_replace(b, frequency_pattern, "frequency\\1")}
      ,
      matches(frequency_pattern)
    )
    ## Body.
    body_pattern <- "(?i)(Body)+"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){str_replace(b, body_pattern, "%body")}
      ,
      matches(body_pattern)
    )
    ## Gravity.
    gravity_pattern <- "(?i)gravity"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){str_replace(b, gravity_pattern, "%gravity")}
      ,
      matches(gravity_pattern)
    )
    ## Accelerometer.
    accelerometer_pattern <- "(?i)Acc"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){str_replace(b, accelerometer_pattern, "~accelerometer")}
      ,
      matches(accelerometer_pattern)
    )
    ## Gyroscope.
    gyroscope_pattern <- "(?i)Gyro"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){str_replace(b, gyroscope_pattern, "~gyroscope")}
      ,
      matches(gyroscope_pattern)
    )
    ## Mean.
    mean_pattern <- "(?i)-*mean[\\(*\\)*]*-*"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){str_replace_all(b, mean_pattern, "!mean")}
      ,
      matches(mean_pattern)
    )
    ## Standard deviation.
    standard_deviation_pattern <- "(?i)-*std[\\(*\\)*]*-*"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){
        str_replace_all(b, standard_deviation_pattern, "!standarddeviation")}
      ,
      matches(standard_deviation_pattern)
    )
    ## X axes.
    x_axe_pattern <- "(?i)(\\(x|x)$"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){
        str_replace(b, x_axe_pattern, "@x")}
      ,
      matches(x_axe_pattern)
    )
    ## Y axes.
    y_axe_pattern <- "(?i)(\\(y|y)$"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){
        str_replace(b, y_axe_pattern, "@y")}
      ,
      matches(y_axe_pattern)
    )
    ## Z axes.
    z_axe_pattern <- "(?i)(\\(z|z)$"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){
        str_replace(b, z_axe_pattern, "@z")}
      ,
      matches(z_axe_pattern)
    )
    ## Jerk.
    jerk_pattern <- "(?i)Jerk(.*$)"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){
        str_replace(b, jerk_pattern, "\\1#jerk")}
      ,
      matches(jerk_pattern)
    )
    ## Magnitude.
    magnitude_pattern <- "(?i)Mag(.*$)"
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){
        str_replace(b, magnitude_pattern, "\\1$magnitude")}
      ,
      matches(magnitude_pattern)
    )
    ## Activity id.
    renamed_experiments <- rename(renamed_experiments, activity_id = X1)
    ## Missing axe.
    missing_axe_pattern <- "(?i)((mean|standarddeviation)(?!@))"
    missing_axe_part <- "NA"
    axe_replacement_pattern = paste0("\\1@", missing_axe_part)
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){
        str_replace(b, missing_axe_pattern, axe_replacement_pattern)}
      ,
      matches(missing_axe_pattern, perl = T)
    )
    ##
    ## Missing jerk.
    jerk_separator <- "#"
    missing_jerk_pattern <- 
      paste0("(?i)((@([xyz]|", missing_axe_part, "))(?!", jerk_separator, "))")
    missing_jerk_part = "#NA"
    jerk_replacement_pattern = paste0("\\1", missing_jerk_part)
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){
        str_replace(b, missing_jerk_pattern, jerk_replacement_pattern)}
      ,
      matches(missing_jerk_pattern, perl = T)
    )
    ##
    ## Missing Magnitude.
    missing_magnitude_pattern <- 
      paste0("(?i)((jerk|", missing_jerk_part, ")(?!\\$))")
    renamed_experiments <- rename_with(
      renamed_experiments
      ,
      function(b){
        str_replace(b, missing_magnitude_pattern, "\\1$NA")}
      ,
      matches(missing_magnitude_pattern, perl = T)
    )
    ##
    renamed_experiments
  }
  ##
  
  renamed_experiments <- label(experiment_with_activities)
  
  print("Reads subjects.")
  
  ## Reads subjects.
  train_subjects <- 
    read_delim(
      "./UCI HAR Dataset/train/subject_train.txt"
      ,
      " "
      ,
      col_names = F
      ,
      col_types =  cols(.default = "n")
    )
  test_subjects <- 
    read_delim(
      "./UCI HAR Dataset/test/subject_test.txt"
      ,
      " "
      ,
      col_names = F
      ,
      col_types =  cols(.default = "n")
    )
  ##
  
  print("Unions the training and test subjects and joins the experiments with their subjects.")
  
  ## Unions the training and test subjects and joins the experiments with their 
  ## subjects.
  subjects <- rbind(train_subjects, test_subjects)
  experiments_with_subjects <- cbind(renamed_experiments, subjects)
  ##
  
  print("Renames the subject column to be subject id.")
  
  ## Renames the subject column to be subject id.
  experiments_subject_id <- rename(experiments_with_subjects, subject_id = X1)
  ##
  
  print("Tidies the experiments.")
  
  ## Tidies the experiments.
  load_up(tidyr)
  
  tidy_experiments <- pivot_longer(
    experiments_subject_id
    ,
    !is_train & !activity_id & !subject_id
    ,
    c("domain", "signal_for", "sensor", "estimate", "axe", "jerk", "magnitude")
    ,
    names_sep = "[%~!@#?$?]"
  )
  ##
  
  print("Applies factor on the experiment variables.")
  
  ## Applies factor on the experiment variables .
  factorized <- mutate(
    tidy_experiments
    ,
    domain = as.factor(domain)
    ,
    signal_for = as.factor(signal_for)
    ,
    sensor = as.factor(sensor)
    ,
    estimate = as.factor(estimate)
    ,
    axe = as.factor(axe)
    ,
    jerk = jerk == "jerk"
    ,
    magnitude = magnitude == "magnitude"
  )
  ##
  
  print("Groups.")
  
  ## Groups.
  groups <- group_by(
    factorized
    , 
    subject_id
    ,
    activity_id
    ,
    domain
    ,
    signal_for
    ,
    sensor
    ,
    estimate
    ,
    axe
    ,
    jerk
    ,
    magnitude
  )
  ##
  
  print("Summarizes.")
  
  ## Summarize.
  summary <- summarise(
    groups
    ,
    average = mean(as.numeric(value))
  )
  result <- summary
  ##
  View(result)
  result
}
##