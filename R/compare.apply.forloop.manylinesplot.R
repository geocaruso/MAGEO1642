#apply and for loop compared for plotting lines per group
#apply quite quicker

# Generate sample data
set.seed(1)
nc<-25
ny<-15
df <- data.frame(
  country = factor(rep(LETTERS[1:nc], each = ny)),
  year = rep(2000:2014, nc),
  xvar = rnorm(ny*nc)
)

#empty plot
plot(NA, xlim = range(df$year), ylim = range(df$xvar))

# with for loop
system.time(
  for (x in split(df, df$country)) {
  lines(x$year, x$xvar, col = x$country[1])
  }
)

# with apply 
system.time(
lapply(split(df, df$country), function(x) {
  lines(x$year, x$xvar, col = x$country[1])
})
)

