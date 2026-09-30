# Thermal metrics functions

library(data.table)
library(slider)
library(lubridate)

# Main wrapper: accepts data.table or data.frame input and returns a data.table
fnc_compute_metrics <- function(stdata,
                                species = "Chinook",
                                lsc = "AMS",
                                year_range = NULL,
                                st_col = "value",
                                site_col = "SiteCode",
                                date_col = "Date",
                                n_threads = data.table::getDTthreads()){
  # set data.table threads
  data.table::setDTthreads(n_threads)

  # Standardize input to data.table (work on a copy)
  if (!data.table::is.data.table(stdata)) st_dt <- data.table::as.data.table(stdata) else st_dt <- data.table::copy(stdata)

  # Ensure required columns exist and have expected names
  site_nm <- rlang::as_name(rlang::ensym(site_col))
  date_nm <- rlang::as_name(rlang::ensym(date_col))
  st_nm <- rlang::as_name(rlang::ensym(st_col))

  if (!site_nm %in% names(st_dt)) stop("site column not found in stdata: ", site_nm)
  if (!date_nm %in% names(st_dt)) stop("date column not found in stdata: ", date_nm)
  if (!st_nm %in% names(st_dt)) stop("value column not found in stdata: ", st_nm)

  # normalize column names to SiteCode/Date/value for internal processing
  if (site_nm != "SiteCode") data.table::setnames(st_dt, site_nm, "SiteCode")
  if (date_nm != "Date") data.table::setnames(st_dt, date_nm, "Date")
  if (st_nm != "value") data.table::setnames(st_dt, st_nm, "value")

  # ensure Date class
  st_dt[, Date := as.Date(Date)]

  # build combos (species × year × life stage)
  combos <- expand.grid(Species = species, LHS_Code = lsc, Year = year_range, stringsAsFactors = FALSE)
  combos <- as.data.table(combos)
  combos <- merge(combos, periodicity, by.x = c("Species","LHS_Code"), by.y = c("Species","LHS_Code"), all.x = TRUE)
  combos[, Start := as.Date(paste0(Year + Year_begin, "-", Month_day_begin), format = "%Y-%d-%b")]
  combos[, End := as.Date(paste0(Year + Year_end, "-", Month_day_end), format = "%Y-%d-%b")]
  combos <- combos[, .(Species, LHS_Code, Year, Start, End, Pref_hi, Pref_lo, Thresh_hi, Thresh_lo)]
  combos <- unique(combos)

  
  # compute metrics grouped by combo
  res_dt <- overlaps[ , {
    ord <- order(Date)
    Date_ord <- Date[ord]
    val <- value[ord]

    n_days <- as.integer(as.numeric(End[1] - Start[1]) + 1)
    days_obs <- sum(!is.na(val))
    days_missing <- sum(is.na(val))
    prop_missing <- mean(is.na(val))
    longest_na_gap <- {r <- rle(is.na(val)); if(all(!r$values)) 0L else max(r$lengths[r$values], na.rm = TRUE)}
    longest_na_gap_prop <- ifelse(n_days > 0, longest_na_gap / n_days, NA_real_)
    missingness_score <- prop_missing + longest_na_gap_prop

    temp_mean <- mean(val, na.rm = TRUE)
    temp_sd <- sd(val, na.rm = TRUE)
    temp_var <- var(val, na.rm = TRUE)
    temp_range <- ifelse(all(is.na(val)), NA_real_, max(val, na.rm = TRUE) - min(val, na.rm = TRUE))
    q05 <- as.numeric(quantile(val, 0.05, na.rm = TRUE, names = FALSE))
    q25 <- as.numeric(quantile(val, 0.25, na.rm = TRUE, names = FALSE))
    q50 <- as.numeric(quantile(val, 0.50, na.rm = TRUE, names = FALSE))
    q75 <- as.numeric(quantile(val, 0.75, na.rm = TRUE, names = FALSE))
    q95 <- as.numeric(quantile(val, 0.95, na.rm = TRUE, names = FALSE))

    date_max <- if(all(is.na(val))) as.Date(NA) else Date_ord[which.max(val)]
    doy_max <- if(!is.na(date_max)) as.numeric(lubridate::yday(date_max)) else NA_real_
    date_min <- if(all(is.na(val))) as.Date(NA) else Date_ord[which.min(val)]
    doy_min <- if(!is.na(date_min)) as.numeric(lubridate::yday(date_min)) else NA_real_

    days_above_thresh <- sum(!is.na(val) & val > Thresh_hi[1], na.rm = TRUE)
    days_below_thresh <- sum(!is.na(val) & val < Thresh_lo[1], na.rm = TRUE)
    degree_days <- sum(pmax(val - 0, 0), na.rm = TRUE)
    cumulative_heat <- sum(pmax(val - Thresh_hi[1], 0), na.rm = TRUE)

    days_in_range <- sum(!is.na(val) & val > Thresh_lo[1] & val < Thresh_hi[1], na.rm = TRUE)

    exceed_1st <- metric_first_week_exceed(Date_ord, val, Thresh_hi[1])

    vs <- ifelse(!is.na(val) & val > Thresh_hi[1], 1L, 0L)
    r <- rle(vs)
    max_consec_above <- if(all(r$values == 0)) 0L else max(r$lengths[r$values == 1], na.rm = TRUE)
    runs <- r$lengths[r$values == 1]
    median_consec_above <- if(length(runs) == 0) 0L else as.integer(median(runs))
    n_heat_events <- sum(r$values & r$lengths >= 3, na.rm = TRUE)

    diffs <- if(length(val) < 2) NA_real_ else diff(val)
    max_daily_increase <- if(all(is.na(diffs))) NA_real_ else max(diffs, na.rm = TRUE)

    # single-pass sliding series using slider::slide_index_dbl (calendar 7-day windows)
    roll_min <- slider::slide_index_dbl(val, Date_ord, ~min(.x, na.rm = TRUE), .before = 6, .complete = TRUE)
    roll_mean <- slider::slide_index_dbl(val, Date_ord, ~mean(.x, na.rm = TRUE), .before = 6, .complete = TRUE)
    roll_max <- slider::slide_index_dbl(val, Date_ord, ~max(.x, na.rm = TRUE), .before = 6, .complete = TRUE)
    roll_range <- roll_max - roll_min

    min_7d <- if(all(is.na(roll_min))) NA_real_ else min(roll_min, na.rm = TRUE)
    mean_7d <- if(all(is.na(roll_mean))) NA_real_ else mean(roll_mean, na.rm = TRUE)
    max_7d <- if(all(is.na(roll_max))) NA_real_ else max(roll_max, na.rm = TRUE)
    mean_7d_min <- if(all(is.na(roll_min))) NA_real_ else mean(roll_min, na.rm = TRUE)
    mean_7d_max <- if(all(is.na(roll_max))) NA_real_ else mean(roll_max, na.rm = TRUE)
    min_7d_mean <- if(all(is.na(roll_mean))) NA_real_ else min(roll_mean, na.rm = TRUE)
    max_7d_mean <- if(all(is.na(roll_mean))) NA_real_ else max(roll_mean, na.rm = TRUE)
    median_weekly_range <- if(all(is.na(roll_range))) NA_real_ else median(roll_range, na.rm = TRUE)

    .(n_days = n_days, days_obs = days_obs, days_missing = days_missing, prop_missing = prop_missing,
      longest_na_gap = longest_na_gap, longest_na_gap_prop = longest_na_gap_prop, missingness_score = missingness_score,
      temp_mean = temp_mean, temp_sd = temp_sd, temp_var = temp_var, temp_range = temp_range,
      q05 = q05, q25 = q25, q50 = q50, q75 = q75, q95 = q95,
      date_max = date_max, doy_max = doy_max, date_min = date_min, doy_min = doy_min,
      days_above_thresh = days_above_thresh, days_below_thresh = days_below_thresh,
      degree_days = degree_days, cumulative_heat = cumulative_heat,
      days_in_range = days_in_range, exceed_1st = exceed_1st,
      max_consec_above = max_consec_above, median_consec_above = median_consec_above, n_heat_events = n_heat_events,
      max_daily_increase = max_daily_increase,
      min_7d = min_7d, mean_7d = mean_7d, max_7d = max_7d, mean_7d_min = mean_7d_min,
      mean_7d_max = mean_7d_max, min_7d_mean = min_7d_mean, max_7d_mean = max_7d_mean,
      median_weekly_range = median_weekly_range)

  }, by = .(Species, LifeHistory, Lifestage, Year, Start, End, Thresh_hi, Thresh_lo, SiteCode)]

  # Ensure combos with no observations are still present (NA metrics)
  out_dt <- merge(combos_expanded, res_dt, by = c("Species","LifeHistory","Lifestage","Year","Start","End","Thresh_hi","Thresh_lo","SiteCode"), all.x = TRUE)

  return(out_dt)
}

# Helper: first week with sustained exceedance (7-day window)
metric_first_week_exceed <- function(date, value, thresh){
  if (length(date) == 0) return(as.Date(NA))
  df_date <- as.Date(date)
  exceed <- ifelse(!is.na(value) & value > thresh, 1L, 0L)
  sums <- slider::slide_index_int(exceed, df_date, ~sum(.x, na.rm = TRUE), .before = 6, .complete = TRUE)
  idx <- which(sums > 0)
  if(length(idx) == 0) return(as.Date(NA))
  return(df_date[min(idx)])
}
