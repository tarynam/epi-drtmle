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
})





