This code tidies up the data from the "Human Activity Recognition Using Smartphones Dataset"

I used the data.table package for all the handling

First a function is defined which reads in all the data from a folder (and optionally subfolders) and assings them to a logical object name in the global environment.
Data from different files are merged by rbinding and assignments within the dataset (df).
Factor labels for the 'activity' are added, extracted from the file 'activity_labels.txt'
Only features which contain 'mean' or 'std' are selected

In the last step, a new data.table is created (df2), which summarizes the data from df, by taking the mean of the selected features for each subject and activity
