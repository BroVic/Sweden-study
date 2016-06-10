# Plot all the predicted outputs in one view
source("multiplot.R") 

if (!exists("p6"))              # check if objects are in workspace
  source("viol-emp_regr.R")

multiplot(p1, p2, p3, p4, p5, p6, cols = 3)

# Thanking Winston Chang for multiplot()
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/