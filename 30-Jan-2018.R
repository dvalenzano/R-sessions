# Power analysis for mouse survival cohort study

library(pwr)

mouse0 = 791 #this is the average survival (not median) for control mice as per data shared by Sebastian
mouse1 = 791*1.15 #this is the survival of a group that lives 15% longer than the control mice 
sd_mouse0 = 192 #data from Sebastian's xcel sheet
sd_mouse1 = sd_mouse0 #assuming same standard deviation for either group

Cohen.d = (mouse1-mouse0)/sqrt(((sd_mouse0^2)+(sd_mouse1^2))/2)

pwr.t.test(
  n = NULL,                  # Observations in _each_ group
  d = Cohen.d,            
  sig.level = 0.05,          # Type I probability
  power = 0.80,              # 1 minus Type II probability
  type = "two.sample",       # Change for one- or two-sample
  alternative = "two.sided"
)

#Two-sample t test power calculation 

#n = 42.0881
#d = 0.6179687
#sig.level = 0.05
#power = 0.8
#alternative = two.sided

#NOTE: n is number in *each* group

# CONCLUSION: 42 mice are sufficient to detect 15% difference in average survival with a powere of 80%
# This power analysis was performed following this link: 
# http://rcompanion.org/rcompanion/b_02.html
