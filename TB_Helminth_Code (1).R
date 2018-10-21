# I couldn't read in the Excel file so I save the original file as CSV
A = read.csv('/Users/kristianeschenburg/Desktop/TB_Helminth.csv',header=TRUE);

# Split the characterized StudyID at hyphen "-"
# If you look at the variables when you first load the table, StudyID is a "factor" 
# -- an indicator variable -- not a string, integer, boolean (basic variable types)
# strsplit only operates on characters / strings
splStudyID = strsplit(as.character(A$StudyID),"-");

# Re-assign first part of study ID, save as factor again
# sapply 
A$StudyID = as.factor(sapply(splStudyID,"[",1));

# this is kind of a hackjob, but for each unique StudyID, "aggregate" will take a data.frame (but in this
# case a vector) of the SchistosomaPositive values, input this vector into function(x), and then perform
# defined computation on it -- if any of the SchistosomaPositive values are == 1, then it will return 1
# otherwise it returns a 0
S = aggregate(SchistosomaPositive ~ StudyID,data = A, function(x) {
  
  # compute number of ones
  s = sum(x == 1);
  
  # is number is positive (> 0), return 1, else return 0
  if (s > 0) {
    return(1);
  } else {
    return(0);
  }
});


QFT = read.csv('/Users/kristianeschenburg/Desktop/TB_Helminth_QFT.csv',header=TRUE);
