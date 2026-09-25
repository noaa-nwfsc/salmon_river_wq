# Thermal metrics functions
# Refactored to data.table + parallel for performance
# Generated script 2026-07-30 (refined for type consistency)

library(data.table)
library(lubridate)
library(zoo)
library(parallel)

# Main wrapper: computes a table of metrics for each Site × Year × LHS_Code
fnc_compute_metrics <- function(emp.dt,
                                periodicity,
                                LHS_Code,
                                species = "Chinook",
                                year_range,
                                st_col = "value",
                                site_col = "SiteCode",
                                date_col = "Date",
                                days_window = 7,
                                sustain_count = 7,
                                nworkers = NULL,
                                debug = FALSE) {
  # emp.dt: data.table or data.frame with full grid of daily records for each site
  # periodicity: table describing life-stage periods (must contain Year_begin, Month_day_begin, Year_end, Month_day_end, Pref_hi, Pref_lo, Thresh_hi, Thresh_lo)
  # LHS_Code: character vector of life-stage codes to evaluate

  emp.dt <- as.data.table(copy(emp.dt))
  emp.dt[, (date_col) := as.IDate(get(date_col))]
  setorderv(emp.dt, c(site_col, date_col))

  combos <- as.data.table(expand.grid(Species = species, LHS_Code = LHS_Code, Year = year_range, stringsAsFactors = FALSE))
  combos <- merge(combos, as.data.table(periodicity), by = c("Species","LHS_Code"), all.x = TRUE)
  combos[, Start := as.IDate(parse_date_time(paste(Year + Year_begin, Month_day_begin), orders = c("Y-d-b","Y-b-d","Y-m-d")))]
  combos[, End := as.IDate(parse_date_time(paste(Year + Year_end, Month_day_end),   orders = c("Y-d-b","Y-b-d","Y-m-d")))]
  combos <- combos[!is.na(Start) & !is.na(End)]
  if (nrow(combos) == 0) return(data.table())

  if (is.null(nworkers)) nworkers <- max(1, detectCores(logical = FALSE) - 1)
  # prepare a template with consistent column types (character/id + numeric + Date)
  site_name <- site_col
  template <- data.table(
    tmp_site = character(),
    n_days = numeric(), days_obs = numeric(), days_missing = numeric(), prop_missing = numeric(),
    longest_na_gap = numeric(), longest_na_gap_prop = numeric(), missingness_score = numeric(),
    temp_mean = numeric(), temp_sd = numeric(), temp_var = numeric(), temp_range = numeric(),
    q05 = numeric(), q25 = numeric(), q50 = numeric(), q75 = numeric(), q95 = numeric(),
    date_max = as.Date(character()), doy_max = numeric(), date_min = as.Date(character()), doy_min = numeric(),
    days_above_thresh = numeric(), days_below_thresh = numeric(),
    degree_days = numeric(), cumulative_heat = numeric(), days_in_range = numeric(),
    exceed_1st = as.Date(character()),
    max_consec_above = numeric(), median_consec_above = numeric(), n_heat_events = numeric(),
    max_daily_increase = numeric(),
    min_7d = numeric(), mean_7d = numeric(), max_7d = numeric(),
    mean_7d_min = numeric(), mean_7d_max = numeric(),
    min_7d_mean = numeric(), max_7d_mean = numeric(),
    median_weekly_range = numeric()
  )
  setnames(template, 'tmp_site', site_name)

  # worker-safe function
  process_one <- function(combo_row) {
    combo <- as.list(combo_row)
    d0 <- emp.dt[get(date_col) >= combo$Start & get(date_col) <= combo$End]
    if (nrow(d0) == 0) return(template[0])

    out <- d0[, {
      v <- get(st_col)
      dtv <- as.IDate(get(date_col))

      n_days <- as.integer(as.integer(combo$End - combo$Start) + 1)
      days_obs <- .N
      days_missing <- sum(is.na(v))
      prop_missing <- if (.N > 0) mean(is.na(v)) else NA_real_
      r_na <- rle(is.na(v))
      longest_na_gap <- if (all(!r_na$values)) 0L else max(r_na$lengths[r_na$values], na.rm = TRUE)
      longest_na_gap_prop <- if (n_days > 0) longest_na_gap / n_days else NA_real_
      missingness_score <- as.numeric(prop_missing) + as.numeric(longest_na_gap_prop)

      temp_mean <- mean(v, na.rm = TRUE)
      temp_sd <- sd(v, na.rm = TRUE)
      temp_var <- var(v, na.rm = TRUE)
      temp_range <- if (all(is.na(v))) NA_real_ else (max(v, na.rm = TRUE) - min(v, na.rm = TRUE))
      q05 <- as.numeric(quantile(v, 0.05, na.rm = TRUE, names = FALSE))
      q25 <- as.numeric(quantile(v, 0.25, na.rm = TRUE, names = FALSE))
      q50 <- as.numeric(quantile(v, 0.50, na.rm = TRUE, names = FALSE))
      q75 <- as.numeric(quantile(v, 0.75, na.rm = TRUE, names = FALSE))
      q95 <- as.numeric(quantile(v, 0.95, na.rm = TRUE, names = FALSE))

      if (all(is.na(v))) {
        date_max <- as.IDate(NA); doy_max <- NA_real_
        date_min <- as.IDate(NA); doy_min <- NA_real_
      } else {
        i_max <- which.max(v); i_min <- which.min(v)
        date_max <- as.IDate(dtv[i_max]); date_min <- as.IDate(dtv[i_min])
        doy_max <- as.numeric(yday(as.Date(date_max))); doy_min <- as.numeric(yday(as.Date(date_min)))
      }

      days_above_thresh <- sum(!is.na(v) & v > combo$Thresh_hi, na.rm = TRUE)
      days_below_thresh <- sum(!is.na(v) & v < combo$Thresh_lo, na.rm = TRUE)
      degree_days <- sum(pmax(v - 0, 0), na.rm = TRUE)
      cumulative_heat <- sum(pmax(v - combo$Thresh_hi, 0), na.rm = TRUE)
      days_in_range <- sum(!is.na(v) & v > combo$Pref_lo & v < combo$Pref_hi, na.rm = TRUE)

      exceed <- as.integer(!is.na(v) & v > combo$Thresh_hi)
      exceed_first <- as.IDate(NA)
      if (length(exceed) >= days_window) {
        s <- zoo::rollapply(exceed, width = days_window, FUN = sum, align = "right", fill = NA, na.rm = FALSE)
        idx <- which(s >= sustain_count)
        if (length(idx) > 0) exceed_first <- as.IDate(dtv[idx[1]])
      }

      vs <- ifelse(!is.na(v) & v > combo$Thresh_hi, 1L, 0L)
      r2 <- rle(vs)
      max_consec_above <- if (all(r2$values == 0)) 0L else max(r2$lengths[r2$values == 1], na.rm = TRUE)
      runs <- if (any(r2$values == 1)) r2$lengths[r2$values == 1] else integer(0)
      median_consec_above <- if (length(runs) == 0) 0L else as.integer(median(runs))
      n_heat_events <- sum(r2$values & r2$lengths >= 3, na.rm = TRUE)
      max_daily_increase <- if (length(v) < 2) NA_real_ else max(diff(v), na.rm = TRUE)

      if (all(is.na(v))) {
        min_7d <- mean_7d <- max_7d <- mean_7d_min <- mean_7d_max <- min_7d_mean <- max_7d_mean <- NA_real_
        median_weekly_range <- NA_real_
      } else {
        roll_min <- zoo::rollapply(v, width = days_window, FUN = function(x) min(x, na.rm = TRUE), align = "right", fill = NA)
        roll_mean <- zoo::rollapply(v, width = days_window, FUN = function(x) mean(x, na.rm = TRUE), align = "right", fill = NA)
        roll_max <- zoo::rollapply(v, width = days_window, FUN = function(x) max(x, na.rm = TRUE), align = "right", fill = NA)
        min_7d <- if (all(is.na(roll_min))) NA_real_ else min(roll_min, na.rm = TRUE)
        mean_7d <- if (all(is.na(roll_mean))) NA_real_ else mean(roll_mean, na.rm = TRUE)
        max_7d <- if (all(is.na(roll_max))) NA_real_ else max(roll_max, na.rm = TRUE)
        mean_7d_min <- if (all(is.na(roll_min))) NA_real_ else mean(roll_min, na.rm = TRUE)
        mean_7d_max <- if (all(is.na(roll_max))) NA_real_ else mean(roll_max, na.rm = TRUE)
        min_7d_mean <- if (all(is.na(roll_mean))) NA_real_ else min(roll_mean, na.rm = TRUE)
        max_7d_mean <- if (all(is.na(roll_mean))) NA_real_ else max(roll_mean, na.rm = TRUE)
        w_range <- zoo::rollapply(v, width = days_window, FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE), align = "right", fill = NA)
        median_weekly_range <- if (all(is.na(w_range))) NA_real_ else median(w_range, na.rm = TRUE)
      }

      list(
        n_days = as.numeric(n_days), days_obs = as.numeric(days_obs), days_missing = as.numeric(days_missing), prop_missing = as.numeric(prop_missing),
        longest_na_gap = as.numeric(longest_na_gap), longest_na_gap_prop = as.numeric(longest_na_gap_prop), missingness_score = as.numeric(missingness_score),
        temp_mean = as.numeric(temp_mean), temp_sd = as.numeric(temp_sd), temp_var = as.numeric(temp_var), temp_range = as.numeric(temp_range),
        q05 = as.numeric(q05), q25 = as.numeric(q25), q50 = as.numeric(q50), q75 = as.numeric(q75), q95 = as.numeric(q95),
        # return dates as numeric (seconds since epoch) to ensure consistent atomic types across groups
        date_max = as.numeric(as.Date(as.character(date_max)), units = "secs"), doy_max = as.numeric(doy_max),
        date_min = as.numeric(as.Date(as.character(date_min)), units = "secs"), doy_min = as.numeric(doy_min),
        days_above_thresh = as.numeric(days_above_thresh), days_below_thresh = as.numeric(days_below_thresh),
        degree_days = as.numeric(degree_days), cumulative_heat = as.numeric(cumulative_heat), days_in_range = as.numeric(days_in_range),
        exceed_1st = as.numeric(as.Date(as.character(exceed_first)), units = "secs"),
        max_consec_above = as.numeric(max_consec_above), median_consec_above = as.numeric(median_consec_above), n_heat_events = as.numeric(n_heat_events),
        max_daily_increase = as.numeric(max_daily_increase),
        min_7d = as.numeric(min_7d), mean_7d = as.numeric(mean_7d), max_7d = as.numeric(max_7d),
        mean_7d_min = as.numeric(mean_7d_min), mean_7d_max = as.numeric(mean_7d_max),
        min_7d_mean = as.numeric(min_7d_mean), max_7d_mean = as.numeric(max_7d_mean),
        median_weekly_range = as.numeric(median_weekly_range)
      )
    }, by = site_col]

    if (nrow(out) > 0) {
      out[, `:=`(Species = combo$Species, LHS_Code = combo$LHS_Code, Year = combo$Year)]
    }
    return(out[])
  }

  # run in parallel (use parLapplyLB even if nworkers=1 to keep behavior consistent)
  cl <- makeCluster(nworkers)
  clusterEvalQ(cl, { library(data.table); library(lubridate); library(zoo) })
  clusterExport(cl, varlist = c("emp.dt","date_col","site_col","st_col","days_window","sustain_count","template"), envir = environment())
  combos_list <- split(combos, seq_len(nrow(combos)))
  res_list <- parLapplyLB(cl, combos_list, process_one)
  stopCluster(cl)

  res_dt <- rbindlist(res_list, fill = TRUE)
  if (nrow(res_dt) > 0) {
    # ensure date columns are Date class
    if ("date_max" %in% names(res_dt)) res_dt[, date_max := as.Date(date_max)]
    if ("date_min" %in% names(res_dt)) res_dt[, date_min := as.Date(date_min)]
    if ("exceed_1st" %in% names(res_dt)) res_dt[, exceed_1st := as.Date(exceed_1st)]
  }
  return(res_dt[])
}

# # metric helpers
# metric_weekly_stat <- function(date, value, stat = c("min","mean","max","mean_min","mean_max","min_mean","max_mean"), days_window = 7) {
#   stat <- match.arg(stat)
#   df <- data.table(date = as.IDate(date), value = value)
#   setorder(df, date)
#   if (nrow(df) == 0) return(NA_real_)
#   if (all(is.na(df$value))) return(NA_real_)
#   roll_min <- zoo::rollapply(df$value, width = days_window, FUN = function(x) min(x, na.rm = TRUE), align = "right", fill = NA)
#   roll_mean <- zoo::rollapply(df$value, width = days_window, FUN = function(x) mean(x, na.rm = TRUE), align = "right", fill = NA)
#   roll_max <- zoo::rollapply(df$value, width = days_window, FUN = function(x) max(x, na.rm = TRUE), align = "right", fill = NA)
#   switch(stat,
#          min = if(all(is.na(roll_min))){NA_real_}else min(roll_min, na.rm = TRUE),
#          mean = if(all(is.na(roll_mean))){NA_real_}else mean(roll_mean, na.rm = TRUE),
#          max = if(all(is.na(roll_max))){NA_real_}else max(roll_max, na.rm = TRUE),
#          mean_min = if(all(is.na(roll_min))){NA_real_}else mean(roll_min, na.rm = TRUE),
#          mean_max = if(all(is.na(roll_max))){NA_real_}else mean(roll_max, na.rm = TRUE),
#          min_mean = if(all(is.na(roll_mean))){NA_real_}else min(roll_mean, na.rm = TRUE),
#          max_mean = if(all(is.na(roll_mean))){NA_real_}else max(roll_mean, na.rm = TRUE)
#   )
# }
# 
# metric_first_week_exceed <- function(date, value, thresh, days_window = 7, sustain_count = 7) {
#   df <- data.table(date = as.IDate(date), value = value)
#   setorder(df, date)
#   if (nrow(df) == 0) return(as.Date(NA))
#   exceed <- as.integer(!is.na(df$value) & df$value > thresh)
#   if (length(exceed) < days_window) return(as.Date(NA))
#   s <- zoo::rollapply(exceed, width = days_window, FUN = sum, align = "right", fill = NA, na.rm = FALSE)
#   idx <- which(s >= sustain_count)
#   if (length(idx) == 0) return(as.Date(NA))
#   return(as.Date(df$date[idx[1]]))
# }
