The analysis is run by run_analysis.R
This code assumes the current directory contains

the input data files 
subject_test.txt,x_test.txt,y_test.txt
the input data files 
subject_train.txt,x_train.txt,y_train.txt

The code reads in the data an calculates a tidy data set (in the data.frame) called train_testdat 
which it saves in the data file train_testdatt.txt�

The variables are 
" Index""; original data set index
" Data-set": whether the original data was the training data set of the test data set
" Subject": the number of the test subject
" Activity": The test activity; ("Walking","Walking_upstairs",
              "Walking_downstairs","Sitting","Standing","Laying")
" Mean": The mean of the original 516 value data vector
" Sdev": The standard deviation of the original 516 value data vector

The code creates a 2nd tidy data set  which it saves in the data file 
�acti_subject.txt�

This is a condensed data set which contains the mean and standard deviation of the data summarized for each test subject and for each activity category.