# Getting-and-Cleaning-Data-Course-Project

This repo contains the files related ot the assignment of Getting and Cleaning Data, Week 4, Getting and Cleaning Data Course Project Coursera course.

The repo has two main components, the data sets, which is mentioned pretty enough by the assignment page, and the analysis file.

The analysis file performs the below:

Holds the instructions required to achieve the Getting and Cleaning Data Course Project.

Please, use the file in a newly opened RStudio session.

To avoid problems when installing packages due to pre-loaded dependencies, it's a good practice to use a fresh r session, therefore, the script uses R Studio APIs to force restarting R session whenever it's sourced.

To add more seamless experience the scripts runs itself immediately after the R session restart, by calling the method "sum_up()" which done everything required by the assignment underneath, and assign the result in a variable called "conclusion" to avoid losing the cashing of the output for later on processing, then it flushes the variable to the console to see the result implicitly.

The script exposes a method called "load_up" which loads a package and installs it if it's not installed, to make sure that the script will be reproducible on any environment without being bothered by the dependencies.

The method can be used out of the script to load/install any library (yet from CRAN) by the passing unquoted library name to it.


The method "sum_up" itself can be called explicitly any time not only by relying on the auto-execution after the restart of the R session when the script is sourced.

It collects, cleans, and summarizes the retrieved data sets.

Since the operation is somehow time-consuming - not so much -, the method doesn't leave you alone, but instead, it prints out what it does step by step while it's being executed.

It does the following in order:

Reads sets and this happens in a lazy manner where nothing is being red until it's really needed to preserve the time and memory resources.

Loads the "readr" library, also it loads the required libraries - lazily - just when they are needed for the above-mentioned reasons.

Adds an is_train (T for Train / F for Test) identifier.

Loads "dplyr"

Unions the training and the test sets to create one data set.

Reads features.

Names experiments with features.

Extracts only the measurements on the mean and standard deviation for each measurement.

Reads activity labels and set-activities.

Unions the training and test activities and joins the experiments with their activities.

Labels the data set with descriptive variable names.

Loads "stringr"

Reads subjects.

Unions the training and test subjects and joins the experiments with their subjects.

Renames the subject column to be subject id.

Tidies the experiments.

Loads "tidyr"

Applies factor on the experiment variables.

Applies grouping.

Summarizes the result.

Views the result.

Returns the result.