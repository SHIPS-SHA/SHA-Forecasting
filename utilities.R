# Helper functions
is_date <- function(df, str) {
  str_s <- as.name(str)
  summarise(df, is.Date(!!str_s) | is.POSIXct(!!str_s)) %>% pull
}

parse_date <- function(df, str, format, tz = "America/Regina") {
  str_s <- as.name(str)
  mutate(df, !!str_s := as.POSIXct(strptime(!!str_s,
                                            format = format)))
}

# Date-time formats to try on data
datetime_formats <- c(
  "%m/%d/%Y %H:%M",
  "%m/%d/%Y %H%M",
  "%m/%d/%Y",
  "%Y/%m/%d %H:%M",
  "%Y/%m/%d %H%M",
  "%Y/%m/%d",
  "%Y%m%d %H:%M",
  "%Y%m%d %H%M",
  "%Y%m%d"
)

# Default values for variables
date_col <- "Date"
y_col <- "Actual"
