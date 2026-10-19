# Scoring logic for the fake review detector.
# Four red flags per review, score = how many flags it trips (0 to 4).
#   1. unusually short   : z-score of log(length) below -1.5
#   2. review burst      : Poisson tail P(X >= reviews that day) below 0.01
#   3. few helpful votes : z-score of helpful votes below -1
#   4. perfect rating    : five stars

make_reference <- function(df) {
  genuine <- if ("label" %in% names(df) && any(df$label == "genuine")) df[df$label == "genuine", ] else NULL
  list(
    mu_len  = mean(log(df$length)),
    sd_len  = sd(log(df$length)),
    mu_help = mean(df$helpful),
    sd_help = sd(df$helpful),
    # normal daily rate: mean of genuine reviews if labels exist, otherwise the median
    lambda  = if (!is.null(genuine)) mean(genuine$per_day) else max(median(df$per_day), 0.5)
  )
}

score_reviews <- function(df, ref = make_reference(df)) {
  out <- df
  out$z_len   <- (log(df$length) - ref$mu_len) / ref$sd_len
  out$burst_p <- ppois(df$per_day - 1, ref$lambda, lower.tail = FALSE)
  out$z_help  <- (df$helpful - ref$mu_help) / ref$sd_help
  out$flag_short <- out$z_len  < -1.5
  out$flag_burst <- out$burst_p < 0.01
  out$flag_help  <- out$z_help < -1
  out$flag_five  <- df$rating == 5
  out$score <- out$flag_short + out$flag_burst + out$flag_help + out$flag_five
  out
}

check_columns <- function(df) {
  need <- c("rating", "length", "per_day", "helpful")
  miss <- setdiff(need, names(df))
  if (length(miss)) stop("The file is missing these columns: ", paste(miss, collapse = ", "))
  for (n in need) df[[n]] <- suppressWarnings(as.numeric(df[[n]]))
  df <- df[stats::complete.cases(df[, need]), ]
  df <- df[df$length > 0, ]
  if (nrow(df) < 20) stop("Need at least 20 usable rows to estimate what is normal.")
  df
}

# The bundled sample: 900 genuine and 100 fake reviews, simulated with a fixed seed
# so every run gives the same 1,000 rows (same data as R/fake_review_detection.R).
simulate_reviews <- function(seed = 2840) {
  set.seed(seed)
  make_genuine <- function(n) data.frame(
    rating  = sample(1:5, n, replace = TRUE, prob = c(0.07, 0.08, 0.15, 0.30, 0.40)),
    length  = round(rlnorm(n, meanlog = 4.3, sdlog = 0.55)),
    per_day = rpois(n, lambda = 1.2),
    helpful = rpois(n, lambda = 3),
    label   = "genuine")
  make_fake <- function(n) data.frame(
    rating  = sample(1:5, n, replace = TRUE, prob = c(0.04, 0.02, 0.04, 0.10, 0.80)),
    length  = round(rlnorm(n, meanlog = 3.4, sdlog = 0.40)),
    per_day = rpois(n, lambda = 6),
    helpful = rpois(n, lambda = 0.6),
    label   = "fake")
  d <- rbind(make_genuine(900), make_fake(100))
  d$length <- pmax(d$length, 3)
  d <- d[sample(nrow(d)), ]
  d$id <- seq_len(nrow(d))
  rownames(d) <- NULL
  d[, c("id", "rating", "length", "per_day", "helpful", "label")]
}
