# Plot all the predicted outputs in one view
source("multiplot.R") 

multiplot(p1, p4, p2, p5, p3, p6, cols = 3)

# Thanking Winston Chang for multiplot()
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/