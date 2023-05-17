#apply and for loop compared for graphic
#apply +-2 times quicker

# Generate sample data
set.seed(1)
df <- data.frame(
  COUNTRY = rep(LETTERS[1:25], each = 15),
  YEAR = rep(2000:2014, 25),
  METRIC = rnorm(375)
)

df$ctryfctr <- factor(df$COUNTRY)
plot(NA, xlim = range(df$YEAR), ylim = range(df$METRIC))


# Measure execution time of for loop
start_time_for <- Sys.time()
for (x in split(df, df$COUNTRY)) {
  lines(x$YEAR, x$METRIC, col = x$ctryfctr[1])
}
end_time_for <- Sys.time()
execution_time_for <- end_time_for - start_time_for

# Measure execution time of apply implementation
start_time_apply <- Sys.time()
lapply(split(df, df$COUNTRY), function(x) {
  lines(x$YEAR, x$METRIC, col = x$ctryfctr[1])
})
end_time_apply <- Sys.time()
execution_time_apply <- end_time_apply - start_time_apply

# Compare execution times
execution_time_for
execution_time_apply
