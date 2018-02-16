#A simple plot histogram
library(reshape2)
library(ggplot2)
require(dplyr)
df_index_100 <- as.data.frame(df_index_100)
# 5)  Create box plots and quartile plots -----
##
# quintiles <- sapply(df0[3:16], function(x) split(x, cut(x, quantile(x, prob = 0:10 / 10, names = FALSE), include = TRUE)))

#=== Code for 26th Dec =====
##Cleaned up and removed old code
########### ---------- Create box plots and quintile plots ----------- #########

#back looking at counties color
# First, individual histograms for the CWBI and the Subindexes
ggplot(df_index_100, aes(CWB_Index)) + 
  geom_histogram(bins = 5, color = "black", fill = "gray") + 
  labs(title = "Child Well-Being Index", x = "Index Score", y = "Number of Census Tracts")
 
ggplot2::ggplot(df_index_100, aes(ChildZ)) + 
  geom_histogram(bins = 5, color = "black", fill = "lightgray") + 
  labs(title = "Child Sub-Index", x = "Sub-Index Score", y = "Number of Census Tracts")

ggplot(df_index_100, aes(FamilyZ)) + 
  geom_histogram(bins = 5, color = "black", fill = "lightgray") + 
  labs(title = "Family Sub-Index", x = "Sub-Index Score", y = "Number of Census Tracts")

ggplot(df_index_100, aes(CommunityZ)) + 
  geom_histogram(bins = 5, color = "black", fill = "lightgray") + 
  labs(title = "Community Sub-Index", x = "Sub-Index Score", y = "Number of Census Tracts")

# One plot that shows all four, from https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# Looks best once you Zoom the plot)
library(gridExtra)
hist_CWB <- ggplot(df_index_100, aes(CWB_Index)) + 
  geom_histogram(bins = 5, color = "black", fill = "gray") + 
  labs(title = "Child Well-Being Index", x = "Index Score", y = "Number of Census Tracts")
hist_Child <- ggplot(df_index_100, aes(ChildZ)) + 
  geom_histogram(bins = 5, color = "black", fill = "lightgray") + 
  labs(title = "Child Sub-Index", x = "Sub-Index Score", y = "Number of Census Tracts")
hist_Family <- ggplot(df_index_100, aes(FamilyZ)) + 
  geom_histogram(bins = 5, color = "black", fill = "lightgray") + 
  labs(title = "Family Sub-Index", x = "Sub-Index Score", y = "Number of Census Tracts")
hist_Community <- ggplot(df_index_100, aes(CommunityZ)) + 
  geom_histogram(bins = 5, color = "black", fill = "lightgray") + 
  labs(title = "Community Sub-Index", x = "Sub-Index Score", y = "Number of Census Tracts")

grid.arrange(
  grobs = list(hist_CWB, hist_Child, hist_Family, hist_Community),
  widths = c(2, 1), 
  layout_matrix = cbind(c(1),
                        c(2, 3, 4))
)



# Code for 31 Dec

# Define the quintile bounds for each index and put them in a dataframe for reference
quintile_CWBI <- quantile(df_index_100$CWB_Index, seq(0, 1, by = 0.2))
quintile_Child <- quantile(df_index_100$ChildZ, seq(0, 1, by = 0.2))
quintile_Family <- quantile(df_index_100$FamilyZ, seq(0, 1, by = 0.2))
quintile_Community <- quantile(df_index_100$CommunityZ, seq(0, 1, by = 0.2))

quintile_df <- data.frame(quintile_CWBI, quintile_Child, quintile_Family, quintile_Community)
# These values match the 'Improved Data' tab, cells O1590:U1596


# Just getting a simple boxplot of quintile bounds with no data points

boxplot(quintile_df$quintile_CWBI, quintile_df$quintile_Child, quintile_df$quintile_Family, quintile_df$quintile_Community,
        names = c("Child Well-Being", "Child", "Family", "Community"),
        main = "Comparison of Quintile Bounds vs. Quartile Bounds",
        ylab = "Index Score")

# but I think there's a better way... boxplots are explicitly for quartiles and 
# using them for quintiles won't make a lot of sense (to me)

# An approach to graphing quintiles as described by https://stackoverflow.com/questions/46628958/making-a-specific-quantile-plot-in-r
# This example uses the CWB_Index column values from df_index_100

d1 <- data_frame(x = df_index_100[ , 3])

breaks <- seq(min(d1$x), max(d1$x), length.out = 50)
quantiles <- quantile(d1$x, seq(0, 1, 0.2))
quantiles2 <- sapply(quantiles, function(x) breaks[which.min(abs(x - breaks))])

d1$bar <- as.numeric(as.character(cut(d1$x, breaks, na.omit((breaks + dplyr::lag(breaks)) / 2))))
d1$fill <- cut(d1$x, quantiles2, na.omit((quantiles2 + dplyr::lag(quantiles2)) / 2))

d1_plot <- ggplot(d1, aes(bar, y = 1, fill = fill)) +
  geom_col(position = 'stack', col = 1, show.legend = FALSE, width = diff(breaks)[1]) + 
  labs(title = "Child Well-Being Index", 
       x = "Distribution of Index Scores, Colored by Quintile", y = "Count of Census Tracts", 
       fill = "Quintile Breakpoint") + 
  scale_fill_manual(values = c("darkred", "red", "orange", "yellow", "green"))
d1_plot

# Here's the same plot but with the Child Sub-Index
d2 <- data_frame(x = df_index_100[ , 4])

breaks <- seq(min(d2$x), max(d2$x), length.out = 50)
quantiles <- quantile(d2$x, seq(0, 1, 0.2))
quantiles2 <- sapply(quantiles, function(x) breaks[which.min(abs(x - breaks))])

d2$bar <- as.numeric(as.character(cut(d2$x, breaks, na.omit((breaks + dplyr::lag(breaks)) / 2))))
d2$fill <- cut(d2$x, quantiles2, na.omit((quantiles2 + dplyr::lag(quantiles2)) / 2))

d2_plot <- ggplot(d2, aes(bar, y = 1, fill = fill)) +
  geom_col(position = 'stack', col = 1, show.legend = FALSE, width = diff(breaks)[1]) + 
  labs(title = "Child Sub-Index", 
       x = "Distribution of Index Scores, Colored by Quintile", y = "Count of Census Tracts", 
       fill = "Quintile Breakpoint") + 
  scale_fill_manual(values = c("darkred", "red", "orange", "yellow", "green"))
d2_plot

# Here's the same plot but with the Family Sub-Index
d3 <- data_frame(x = df_index_100[ , 5])

breaks <- seq(min(d3$x), max(d3$x), length.out = 50)
quantiles <- quantile(d3$x, seq(0, 1, 0.2))
quantiles2 <- sapply(quantiles, function(x) breaks[which.min(abs(x - breaks))])

d3$bar <- as.numeric(as.character(cut(d3$x, breaks, na.omit((breaks + dplyr::lag(breaks)) / 2))))
d3$fill <- cut(d3$x, quantiles2, na.omit((quantiles2 + dplyr::lag(quantiles2)) / 2))

d3_plot <- ggplot(d3, aes(bar, y = 1, fill = fill)) +
  geom_col(position = 'stack', col = 1, show.legend = FALSE, width = diff(breaks)[1]) + 
  labs(title = "Family Sub-Index", 
       x = "Distribution of Index Scores, Colored by Quintile", y = "Count of Census Tracts", 
       fill = "Quintile Breakpoint") + 
  scale_fill_manual(values = c("darkred", "red", "orange", "yellow", "green"))
d3_plot

# Here's the same plot but with the Community Sub-Index
d4 <- data_frame(x = df_index_100[ , 4])

breaks <- seq(min(d4$x), max(d4$x), length.out = 50)
quantiles <- quantile(d4$x, seq(0, 1, 0.2))
quantiles2 <- sapply(quantiles, function(x) breaks[which.min(abs(x - breaks))])

d4$bar <- as.numeric(as.character(cut(d4$x, breaks, na.omit((breaks + dplyr::lag(breaks)) / 2))))
d4$fill <- cut(d4$x, quantiles2, na.omit((quantiles2 + dplyr::lag(quantiles2)) / 2))

d4_plot <- ggplot(d4, aes(bar, y = 1, fill = fill)) +
  geom_col(position = 'stack', col = 1, show.legend = FALSE, width = diff(breaks)[1]) + 
  labs(title = "Community Sub-Index", 
       x = "Distribution of Index Scores, Colored by Quintile", y = "Count of Census Tracts", 
       fill = "Quintile Breakpoint") + 
  scale_fill_manual(values = c("darkred", "red", "orange", "yellow", "green"))
d4_plot

# Now to get all four of those on one plot -- it looks better after Zoom
library(gridExtra)
grid.arrange(
  grobs = list(d1_plot, d2_plot, d3_plot, d4_plot),
  widths = c(2, 1), 
  layout_matrix = cbind(c(1),
                        c(2, 3, 4))
)

