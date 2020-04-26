library(ggplot2)

last_month <- as.Date(covid_data$Date, format = "%m/%d")
df <- data.frame(
  date = last_month,
  price = runif(length(covid_data$Date))
)
base <- ggplot(df, aes(date, price)) +
  geom_line()

# The date scale will attempt to pick sensible defaults for
# major and minor tick marks. Override with date_breaks, date_labels
# date_minor_breaks arguments.
base + scale_x_date(date_labels = "%b %d")
base + scale_x_date(date_breaks = "1 week", date_labels = "%W")
base + scale_x_date(date_minor_breaks = "1 day")
base + scale_x_date(date_breaks = "3 days", date_labels = "%m/%d")

# Set limits
base + scale_x_date(limits = c(Sys.Date() - 7, NA))
