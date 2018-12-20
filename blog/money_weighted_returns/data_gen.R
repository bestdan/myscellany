
source("calcEarnings.R")


percent_change <- function(x) {(x/lag(x)) - 1}

growth_data <-
  thisTicker %>%
  quantmod::getSymbols(., from = "1900-01-01", auto.assign = FALSE) %>%
  as.data.frame() %>%
  mutate(rdate = as.Date(row.names(.)),
         year = year(rdate),
         month = month(rdate),
         returns = na.fill(percent_change(VTSMX.Adjusted),0),
         returns_cumul = cumprod(returns + 1)) %>%
  ungroup() %>%
  group_by(year, month) %>%
  mutate(first_day_of_month = min(rdate),
         deposit_date = ifelse(rdate == first_day_of_month,
                               initial_deposit, NA)) %>%
  ungroup() %>%
  select(rdate, contains(".Adjusted"), returns, returns_cumul, deposit_date)



value_streams <- growth_data %>%
  group_by(deposit_date)


depositDates <- growth_data %>%
  filter(!is.na(deposit_date)) %>%
  select(rdate, deposit_date)

depositDates <- as.xts(depositDates$deposit_date, order.by = depositDates$rdate)

thisResult <- calcEarnings(as.xts(growth_data$returns, order.by =
                                    growth_data$rdate),
                           depositDates)

veMat <- as.data.frame(thisResult$valueEarningMat)
veMat$rdate <- as.Date(row.names(veMat))

valueStreams <- thisResult$valueStreams

value_streams_long <-
  valueStreams %>%
  broom::tidy() %>%
  mutate(rdate = index) %>%
  group_by(series)


value_streams_long <-
  value_streams_long %>%
  group_by(series) %>%
  mutate(last_value = dplyr::last(value),
         gain_bool = ifelse(last_value > initial_deposit,
                            TRUE,
                            FALSE),
         gain_cat = cut(last_value, breaks = c(-Inf,
                                               initial_deposit * 0.9,
                                               initial_deposit * 1.1,
                                               Inf)),
         min_value = min(value, na.rm = T),
         min_value_bool = ifelse(min_value > initial_deposit,
                                 TRUE,
                                 FALSE),
         min_value_cat = cut(min_value, breaks = c(-Inf,
                                                   initial_deposit * 0.5,
                                                   initial_deposit * 0.8,
                                                   initial_deposit,
                                                   Inf))
  )


value_streams_dayaligned <- value_streams_long %>%
    filter(!is.na(value)) %>%
    mutate(days = 1:n())

save.image(file = "data")

