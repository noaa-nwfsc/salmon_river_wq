# Thermal metrics functions
# Refactored to data.table + parallel for performance
# Generated script 2026-07-30 (refined for type consistency); updated 10/2/26 (removed date fields, added new weekly timing metrics)
# Ensured nearly complete year coverage

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
                                min_coverage = 0.9,
                                nworkers = NULL,
                                debug = FALSE) {
  # emp.dt: data.table or data.frame with full grid of daily records for each site
  # periodicity: table describing life-stage periods (must contain Year_begin, Month_day_begin, Year_end, Month_day_end, Pref_hi, Pref_lo, Thresh_hi, Thresh_lo)
  # LHS_Code: character vector of life-stage codes to evaluate

  emp.dt <- as.data.table(copy(emp.dt))
  emp.dt[, (date_col) := as.IDate(get(date_col))]
  setorderv(emp.dt, c(site_col, date_col))

  # Build evaluation combos and compute numeric Start/End dates for each Year × LHS
  combos <- as.data.table(expand.grid(Species = species, LHS_Code = LHS_Code, Year = year_range, stringsAsFactors = FALSE))
  combos <- merge(combos, as.data.table(periodicity), by = c("Species","LHS_Code"), all.x = TRUE)
  combos[, Start := as.IDate(parse_date_time(paste(Year + Year_begin, Month_day_begin), orders = c("Y-d-b","Y-b-d","Y-m-d")))]
  combos[, End := as.IDate(parse_date_time(paste(Year + Year_end, Month_day_end),   orders = c("Y-d-b","Y-b-d","Y-m-d")))]
  combos <- combos[!is.na(Start) & !is.na(End)]
  if (nrow(combos) == 0) return(data.table())

  if (is.null(nworkers)) nworkers <- max(1, detectCores(logical = FALSE) - 1)

  # prepare a template with consistent column types (character/id + numeric)
  site_name <- site_col
  template <- data.table(
    tmp_site = character(),
    n_days = numeric(), days_obs = numeric(), days_missing = numeric(), prop_missing = numeric(),
    longest_na_gap = numeric(), longest_na_gap_prop = numeric(), missingness_score = numeric(),
    met_mean = numeric(), met_sd = numeric(), met_var = numeric(), met_range = numeric(),
    q05 = numeric(), q25 = numeric(), q50 = numeric(), q75 = numeric(), q95 = numeric(),
    doy_max = numeric(), doy_min = numeric(),
    days_above = numeric(), days_below = numeric(),
    degree_days = numeric(), cumulative_heat = numeric(), days_in_range = numeric(),
    exceed_1st = numeric(), exceed_1st_week = numeric(),
    max_consec_inrange = numeric(), mean_consec_inrange = numeric(), 
    max_consec_above = numeric(), mean_consec_above = numeric(), 
    max_consec_below = numeric(), mean_consec_below = numeric(),
    n_heat_events = numeric(),
    max_daily_increase = numeric(),
    below_1st_week= numeric(),
    min_7d_min = numeric(), mean_7d_mean = numeric(), max_7d_max = numeric(),
    mean_7d_min = numeric(), mean_7d_max = numeric(),
    min_7d_mean = numeric(), max_7d_mean = numeric(),
    median_weekly_range = numeric(),
    max_7d_week = numeric(), min_7d_week = numeric()
  )
  setnames(template, 'tmp_site', site_name)

  # worker-safe function: compute metrics for a single combo (Species, LHS_Code, Year)
  process_one <- function(combo_row) {
    combo <- as.list(combo_row)
    d0 <- emp.dt[get(date_col) >= combo$Start & get(date_col) <= combo$End]
    if (nrow(d0) == 0) return(template[0])

      out <- d0[, {
      v <- get(st_col)
      dtv <- as.IDate(get(date_col))

      n_days <- as.integer(as.integer(combo$End - combo$Start) + 1)
      days_obs <- .N
      prop_coverage <- if (n_days > 0) as.numeric(days_obs / n_days) else NA_real_

      if (!is.na(prop_coverage) && (
        prop_coverage < min_coverage ||
          min(dtv, na.rm = TRUE) > combo$Start ||
          max(dtv, na.rm = TRUE) < combo$End
      )) {
        NULL
      } else {
        days_missing <- sum(is.na(v))
        prop_missing <- if (.N > 0) mean(is.na(v)) else NA_real_
        r_na <- rle(is.na(v))
        # longest run of missing days
        longest_na_gap <- if (all(!r_na$values)) 0L else max(r_na$lengths[r_na$values], na.rm = TRUE)
        longest_na_gap_prop <- if (n_days > 0) longest_na_gap / n_days else NA_real_
        missingness_score <- as.numeric(prop_missing) + as.numeric(longest_na_gap_prop)

      # Central tendency and dispersion metrics
      met_mean <- mean(v, na.rm = TRUE)
      met_sd <- sd(v, na.rm = TRUE)
      met_var <- var(v, na.rm = TRUE)
      met_range <- if (all(is.na(v))) NA_real_ else (max(v, na.rm = TRUE) - min(v, na.rm = TRUE))
      q05 <- as.numeric(quantile(v, 0.05, na.rm = TRUE, names = FALSE))
      q25 <- as.numeric(quantile(v, 0.25, na.rm = TRUE, names = FALSE))
      q50 <- as.numeric(quantile(v, 0.50, na.rm = TRUE, names = FALSE))
      q75 <- as.numeric(quantile(v, 0.75, na.rm = TRUE, names = FALSE))
      q95 <- as.numeric(quantile(v, 0.95, na.rm = TRUE, names = FALSE))

      # Day-of-year for absolute max and min values
      if (all(is.na(v))) {
        doy_max <- NA_real_
        doy_min <- NA_real_
      } else {
        i_max <- which.max(v); i_min <- which.min(v)
        doy_max <- as.numeric(yday(as.Date(dtv[i_max])))
        doy_min <- as.numeric(yday(as.Date(dtv[i_min])))
      }

      # Counts relative to thresholds and preference bounds
      days_above <- sum(!is.na(v) & v > combo$Thresh_hi, na.rm = TRUE)  # days exceeding high threshold
      days_below <- sum(!is.na(v) & v < combo$Thresh_lo, na.rm = TRUE)  # days below low threshold
      degree_days <- sum(pmax(v - 0, 0), na.rm = TRUE)                          # cumulative positive degree-days above 0
      cumulative_heat <- sum(pmax(v - combo$Thresh_hi, 0), na.rm = TRUE)        # heat above high threshold
      days_in_range <- sum(!is.na(v) & v > combo$Pref_lo & v < combo$Pref_hi, na.rm = TRUE) # days inside preferred range

      # Identify first sustained exceedance (using a rolling window)
      exceed <- as.integer(!is.na(v) & v > combo$Thresh_hi)
      exceed_first <- as.IDate(NA)
      exceed_1st_week <- NA_real_
      if (length(exceed) >= days_window) {
        s <- zoo::rollapply(exceed, width = days_window, FUN = sum, align = "right", fill = NA, na.rm = FALSE)
        idx <- which(s >= sustain_count)
        if (length(idx) > 0) exceed_first <- as.IDate(dtv[idx[1]])
        # first full week (all days in window above threshold)
        full_week_idx <- which(s == days_window)
        if (length(full_week_idx) > 0) {
          exceed_1st_week <- lubridate::yday(as.Date(dtv[full_week_idx[1]]))
        }
      }
      exceed_1st <- lubridate::yday(exceed_first)

      # Run-length based metrics for consecutive days above high threshold
      vs_above <- ifelse(!is.na(v) & v > combo$Thresh_hi, 1L, 0L)
      r_above <- rle(vs_above)
      max_consec_above <- if (all(r_above$values == 0)) 0L else max(r_above$lengths[r_above$values == 1], na.rm = TRUE)
      runs_above <- if (any(r_above$values == 1)) r_above$lengths[r_above$values == 1] else integer(0)
      mean_consec_above <- if (length(runs_above) == 0) 0L else as.integer(mean(runs_above))
      # count heat events defined as runs above Thresh_hi of length >= 3
      n_heat_events <- if (length(r_above$values) == 0) 0L else sum(r_above$values & r_above$lengths >= 3, na.rm = TRUE)

      # Run-length metrics for consecutive days below low threshold
      vs_below <- ifelse(!is.na(v) & v < combo$Thresh_lo, 1L, 0L)
      r_below <- rle(vs_below)
      max_consec_below <- if (all(r_below$values == 0)) 0L else max(r_below$lengths[r_below$values == 1], na.rm = TRUE)
      runs_below <- if (any(r_below$values == 1)) r_below$lengths[r_below$values == 1] else integer(0)
      mean_consec_below <- if (length(runs_below) == 0) 0L else as.integer(mean(runs_below))

      max_daily_increase <- if (length(v) < 2) NA_real_ else max(diff(v), na.rm = TRUE) # largest day-to-day jump

      # Run-length metrics for periods inside the preferred range
      vs_inrange <- ifelse(!is.na(v) & v > combo$Pref_lo & v < combo$Pref_hi, 1L, 0L)
      r_inrange <- rle(vs_inrange)
      max_consec_inrange <- if (all(r_inrange$values == 0)) 0L else max(r_inrange$lengths[r_inrange$values == 1], na.rm = TRUE)
      runs_inrange <- if (any(r_inrange$values == 1)) r_inrange$lengths[r_inrange$values == 1] else integer(0)
      mean_consec_inrange <- if (length(runs_inrange) == 0) 0L else as.integer(mean(runs_inrange))

      # first week consistently below Thresh_lo (returns DOY of the window's last day)
      first_week_blw <- as.IDate(NA)
      below_1st_week<- NA_real_
      below_flag <- as.integer(!is.na(v) & v < combo$Thresh_lo)
      if (length(below_flag) >= days_window) {
        sbl <- zoo::rollapply(below_flag, width = days_window, FUN = sum, align = "right", fill = NA, na.rm = FALSE)
        idxb <- which(sbl == days_window)
        if (length(idxb) > 0) {
          first_week_blw <- as.IDate(dtv[idxb[1]])
          below_1st_week<- lubridate::yday(as.Date(first_week_blw))
        }
      }

      # 7-day rolling summaries: min, mean, max and range across each 7-day window
      if (all(is.na(v))) {
        min_7d_min <- mean_7d_mean<- max_7d_max <- mean_7d_min <- mean_7d_max <- min_7d_mean <- max_7d_mean <- NA_real_
        median_weekly_range <- NA_real_
        max_7d_week <- NA_real_
        min_7d_week <- NA_real_
      } else {
        roll_min <- zoo::rollapply(v, width = days_window, FUN = function(x) min(x, na.rm = TRUE), align = "right", fill = NA)
        roll_mean <- zoo::rollapply(v, width = days_window, FUN = function(x) mean(x, na.rm = TRUE), align = "right", fill = NA)
        roll_max <- zoo::rollapply(v, width = days_window, FUN = function(x) max(x, na.rm = TRUE), align = "right", fill = NA)
        min_7d_min <- if (all(is.na(roll_min))) NA_real_ else min(roll_min, na.rm = TRUE)      # coldest 7-day window value
        mean_7d_mean<- if (all(is.na(roll_mean))) NA_real_ else mean(roll_mean, na.rm = TRUE)  # average of 7-day means
        max_7d_max<- if (all(is.na(roll_max))) NA_real_ else max(roll_max, na.rm = TRUE)      # hottest 7-day window value
        mean_7d_min <- if (all(is.na(roll_min))) NA_real_ else mean(roll_min, na.rm = TRUE)
        mean_7d_max <- if (all(is.na(roll_max))) NA_real_ else mean(roll_max, na.rm = TRUE)
        min_7d_mean <- if (all(is.na(roll_mean))) NA_real_ else min(roll_mean, na.rm = TRUE)
        max_7d_mean <- if (all(is.na(roll_mean))) NA_real_ else max(roll_mean, na.rm = TRUE)
        w_range <- zoo::rollapply(v, width = days_window, FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE), align = "right", fill = NA)
        median_weekly_range <- if (all(is.na(w_range))) NA_real_ else median(w_range, na.rm = TRUE)

        # first day (DOY) of the hottest 7-day window: find window with maximum roll_max and return its first day
        if (all(is.na(roll_max))) {
          max_7d_week <- NA_real_
        } else {
          idx_max <- which(roll_max == max(roll_max, na.rm = TRUE))[1]
          start_idx <- idx_max - days_window + 1
          if (start_idx >= 1) {
            max_7d_week <- as.numeric(lubridate::yday(as.Date(dtv[start_idx])))
          } else {
            # if the window extends before the period, return the first available day
            max_7d_week <- as.numeric(lubridate::yday(as.Date(dtv[1])))
          }
        }

        # first day (DOY) of the coldest 7-day window: find window with minimum roll_min and return its first day
        if (all(is.na(roll_min))) {
          min_7d_week <- NA_real_
        } else {
          idx_min <- which(roll_min == min(roll_min, na.rm = TRUE))[1]
          start_idx2 <- idx_min - days_window + 1
          if (start_idx2 >= 1) {
            min_7d_week <- as.numeric(lubridate::yday(as.Date(dtv[start_idx2])))
          } else {
            min_7d_week <- as.numeric(lubridate::yday(as.Date(dtv[1])))
          }
        }
      }

      # return a named list of metrics (cast to numeric to ensure consistent column types)
      list(
        n_days = as.numeric(n_days), days_obs = as.numeric(days_obs), days_missing = as.numeric(days_missing), prop_missing = as.numeric(prop_missing),
        longest_na_gap = as.numeric(longest_na_gap), longest_na_gap_prop = as.numeric(longest_na_gap_prop), missingness_score = as.numeric(missingness_score),
        met_mean = as.numeric(met_mean), met_sd = as.numeric(met_sd), met_var = as.numeric(met_var), met_range = as.numeric(met_range),
        q05 = as.numeric(q05), q25 = as.numeric(q25), q50 = as.numeric(q50), q75 = as.numeric(q75), q95 = as.numeric(q95),
        doy_max = as.numeric(doy_max), doy_min = as.numeric(doy_min),
        days_above = as.numeric(days_above), days_below = as.numeric(days_below),
        degree_days = as.numeric(degree_days), cumulative_heat = as.numeric(cumulative_heat), 
        days_in_range = as.numeric(days_in_range),
        exceed_1st = as.numeric(exceed_1st),
        exceed_1st_week = as.numeric(exceed_1st_week),
        max_consec_above = as.numeric(max_consec_above), 
        mean_consec_above = as.numeric(mean_consec_above), 
        max_consec_below = as.numeric(max_consec_below),
        mean_consec_below = as.numeric(mean_consec_below),
        mean_consec_inrange = as.numeric(mean_consec_inrange), 
        max_consec_inrange = as.numeric(max_consec_inrange),
        n_heat_events = as.numeric(n_heat_events),
        max_daily_increase = as.numeric(max_daily_increase),
        below_1st_week= as.numeric(below_1st_week),
        min_7d_min = as.numeric(min_7d_min), mean_7d_mean= as.numeric(mean_7d_mean), max_7d_max = as.numeric(max_7d_max),
        mean_7d_min = as.numeric(mean_7d_min), mean_7d_max = as.numeric(mean_7d_max),
        min_7d_mean = as.numeric(min_7d_mean), max_7d_mean = as.numeric(max_7d_mean),
        median_weekly_range = as.numeric(median_weekly_range),
        max_7d_week = as.numeric(max_7d_week), min_7d_week = as.numeric(min_7d_week)
      )
    }}, by = site_col]

    if (nrow(out) > 0) {
      out[, `:=`(Species = combo$Species, LHS_Code = combo$LHS_Code, Year = combo$Year)]
    }
    return(out[])
  }

  # run in parallel (use parLapplyLB even if nworkers=1 to keep behavior consistent)
  cl <- makeCluster(nworkers)
  clusterEvalQ(cl, { library(data.table); library(lubridate); library(zoo) })
  clusterExport(cl, varlist = c("emp.dt","date_col","site_col","st_col","days_window","sustain_count","min_coverage","template"), envir = environment())
  combos_list <- split(combos, seq_len(nrow(combos)))
  res_list <- parLapplyLB(cl, combos_list, process_one)
  stopCluster(cl)

  res_dt <- rbindlist(res_list, fill = TRUE)

  return(res_dt[])
}