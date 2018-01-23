tmp <- tempfile()
Rprof()
test <- melt(df,  id.vars = "weave_ct2010")
g <- ggplot(test,aes(x=value))
g <- g + geom_histogram()
g <- g + facet_wrap(~weave_ct2010)

#Plot histogram
 
hist(wt)
hist(mpg)
hist(disp)