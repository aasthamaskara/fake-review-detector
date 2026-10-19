# ------------------------------------------------------------
# Unmasking Fake Online Reviews
# Probability and statistical anomaly detection in R
# Aastha Maskara (25BCE2840), Rayna (25BCE0651)
#
# Note: no real scraped dataset is used. The reviews are simulated
# so the true labels (genuine / fake) are known, which lets us check
# how well the statistical methods actually do.
# Only base R is needed.
# ------------------------------------------------------------

set.seed(2840)
dir.create("out", showWarnings = FALSE)

# ---------- 1. build the review data ----------
n_genuine <- 900
n_fake    <- 100
n         <- n_genuine + n_fake

make_genuine <- function(n) {
  data.frame(
    rating      = sample(1:5, n, replace = TRUE, prob = c(0.07, 0.08, 0.15, 0.30, 0.40)),
    length      = round(rlnorm(n, meanlog = 4.3, sdlog = 0.55)),   # words
    per_day     = rpois(n, lambda = 1.2),                           # reviews by that user's product that day
    helpful     = rpois(n, lambda = 3),
    label       = "genuine"
  )
}
make_fake <- function(n) {
  data.frame(
    rating      = sample(1:5, n, replace = TRUE, prob = c(0.04, 0.02, 0.04, 0.10, 0.80)),
    length      = round(rlnorm(n, meanlog = 3.4, sdlog = 0.40)),
    per_day     = rpois(n, lambda = 6),
    helpful     = rpois(n, lambda = 0.6),
    label       = "fake"
  )
}

reviews <- rbind(make_genuine(n_genuine), make_fake(n_fake))
reviews$length <- pmax(reviews$length, 3)
reviews <- reviews[sample(nrow(reviews)), ]
reviews$id <- seq_len(nrow(reviews))
rownames(reviews) <- NULL

cat("=== 1. DATA OVERVIEW ===\n")
cat("Total reviews:", nrow(reviews), "\n")
print(table(reviews$label))
cat("\nFirst few rows:\n")
print(head(reviews))

# ---------- 2. descriptive statistics ----------
cat("\n=== 2. DESCRIPTIVE STATISTICS ===\n")
desc <- function(x) c(mean = mean(x), median = median(x), sd = sd(x),
                      min = min(x), max = max(x))
print(round(sapply(reviews[, c("rating", "length", "per_day", "helpful")], desc), 2))

cat("\nBy group (mean and sd):\n")
agg_mean <- aggregate(cbind(rating, length, per_day, helpful) ~ label, reviews, mean)
agg_sd   <- aggregate(cbind(rating, length, per_day, helpful) ~ label, reviews, sd)
print(cbind(label = agg_mean$label, round(agg_mean[, -1], 2)))
cat("\nStandard deviations:\n")
print(cbind(label = agg_sd$label, round(agg_sd[, -1], 2)))

# ---------- 3. distributions (plots) ----------
png("out/fig1_rating_distribution.png", width = 900, height = 520, res = 110)
tab <- prop.table(table(reviews$label, reviews$rating), margin = 1)
barplot(tab, beside = TRUE, col = c("#e45756", "#4c78a8"),
        main = "Star rating distribution: genuine vs fake",
        xlab = "Star rating", ylab = "Proportion of reviews",
        legend.text = rownames(tab), args.legend = list(x = "topleft", bty = "n"))
invisible(dev.off())

png("out/fig2_length_boxplot.png", width = 900, height = 520, res = 110)
boxplot(length ~ label, data = reviews, col = c("#e45756", "#4c78a8"),
        main = "Review length (words)", ylab = "Words", xlab = "")
invisible(dev.off())

# ---------- 4. hypothesis testing ----------
cat("\n=== 3. HYPOTHESIS TESTS (alpha = 0.05) ===\n")

# 4a. do fake reviews have a different mean length?
t_len <- t.test(length ~ label, data = reviews)
cat("\n[t-test] H0: mean length is equal for genuine and fake\n")
print(t_len)

# 4b. are 5-star ratings more common among fake reviews?
five <- table(reviews$label, reviews$rating == 5)
cat("\n[two-proportion test] share of 5-star reviews\n")
print(five)
pt5 <- prop.test(five[, "TRUE"], rowSums(five))
print(pt5)

# 4c. is rating independent of label?
chi <- chisq.test(table(reviews$label, reviews$rating))
cat("\n[chi-square] H0: rating is independent of review type\n")
print(chi)

# 4d. helpful votes: non-parametric because counts are skewed
w <- wilcox.test(helpful ~ label, data = reviews)
cat("\n[Wilcoxon rank-sum] helpful votes\n")
print(w)

# ---------- 5. probability: how unusual is a burst of reviews? ----------
cat("\n=== 4. POISSON PROBABILITY OF A REVIEW BURST ===\n")
lambda_hat <- mean(reviews$per_day[reviews$label == "genuine"])
cat("Estimated normal rate (genuine), lambda =", round(lambda_hat, 3), "per day\n")
for (k in c(4, 6, 8)) {
  p <- ppois(k - 1, lambda_hat, lower.tail = FALSE)
  cat(sprintf("P(X >= %d in a day) = %.5f\n", k, p))
}
reviews$burst_p <- ppois(reviews$per_day - 1, lambda_hat, lower.tail = FALSE)

png("out/fig3_poisson_burst.png", width = 900, height = 520, res = 110)
hist(reviews$per_day[reviews$label == "genuine"], breaks = seq(-0.5, 16.5, 1),
     freq = FALSE, col = rgb(0.30, 0.47, 0.65, 0.6), border = "white",
     main = "Reviews per day against the Poisson model",
     xlab = "Reviews in a day", ylim = c(0, 0.4))
hist(reviews$per_day[reviews$label == "fake"], breaks = seq(-0.5, 16.5, 1),
     freq = FALSE, col = rgb(0.89, 0.34, 0.34, 0.6), border = "white", add = TRUE)
lines(0:16, dpois(0:16, lambda_hat), type = "b", pch = 19, lwd = 2)
legend("topright", c("genuine", "fake", "Poisson fit"),
       fill = c(rgb(0.30, 0.47, 0.65, 0.6), rgb(0.89, 0.34, 0.34, 0.6), NA),
       border = NA, pch = c(NA, NA, 19), lty = c(NA, NA, 1), bty = "n")
invisible(dev.off())

# ---------- 6. anomaly detection ----------
cat("\n=== 5. ANOMALY DETECTION ===\n")

# z-scores on log(length) (length is right skewed, log makes it roughly normal)
loglen <- log(reviews$length)
reviews$z_len   <- (loglen - mean(loglen)) / sd(loglen)
reviews$z_help  <- (reviews$helpful - mean(reviews$helpful)) / sd(reviews$helpful)

# IQR rule on reviews per day
q   <- quantile(reviews$per_day, c(0.25, 0.75))
iqr <- q[2] - q[1]
upper_fence <- q[2] + 1.5 * iqr
cat("Reviews/day IQR fence (upper):", upper_fence, "\n")

# individual flags
reviews$flag_short <- reviews$z_len  < -1.5
reviews$flag_burst <- reviews$burst_p < 0.01
reviews$flag_help  <- reviews$z_help < -1
reviews$flag_five  <- reviews$rating == 5

# combined suspicion score = how many red flags a review trips
reviews$score <- with(reviews, flag_short + flag_burst + flag_help + flag_five)
reviews$predicted <- ifelse(reviews$score >= 3, "fake", "genuine")

cat("\nSuspicion score vs true label:\n")
print(table(score = reviews$score, truth = reviews$label))

# ---------- 7. how good is it? ----------
cm <- table(truth = factor(reviews$label, levels = c("fake", "genuine")),
            predicted = factor(reviews$predicted, levels = c("fake", "genuine")))
cat("\n=== 6. CONFUSION MATRIX (score >= 3 means flagged) ===\n")
print(cm)

TP <- cm["fake", "fake"];       FN <- cm["fake", "genuine"]
FP <- cm["genuine", "fake"];    TN <- cm["genuine", "genuine"]
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1        <- 2 * precision * recall / (precision + recall)
accuracy  <- (TP + TN) / sum(cm)
cat(sprintf("\nAccuracy : %.3f\nPrecision: %.3f\nRecall   : %.3f\nF1 score : %.3f\n",
            accuracy, precision, recall, f1))

# bayes: given a review is flagged, chance it is really fake
prior_fake <- n_fake / n
cat(sprintf("\nPrior P(fake) = %.2f, posterior P(fake | flagged) = %.3f\n",
            prior_fake, precision))

# what happens if we change the cut-off?
cat("\nEffect of the cut-off on precision and recall:\n")
for (cut in c(2, 3, 4)) {
  flagged <- reviews$score >= cut
  tp <- sum(flagged & reviews$label == "fake")
  fp <- sum(flagged & reviews$label == "genuine")
  fn <- sum(!flagged & reviews$label == "fake")
  cat(sprintf("cut-off >= %d : precision %.3f, recall %.3f, flagged %d\n",
              cut, tp / (tp + fp), tp / (tp + fn), sum(flagged)))
}

# ---------- 8. score plot ----------
png("out/fig4_suspicion_score.png", width = 900, height = 520, res = 110)
tb <- table(reviews$label, reviews$score)
barplot(tb, beside = TRUE, col = c("#e45756", "#4c78a8"),
        main = "Suspicion score by true review type",
        xlab = "Number of red flags tripped", ylab = "Number of reviews",
        legend.text = rownames(tb), args.legend = list(x = "topright", bty = "n"))
invisible(dev.off())

# ---------- 9. top suspicious reviews ----------
cat("\n=== 7. TOP 10 MOST SUSPICIOUS REVIEWS ===\n")
ord <- order(-reviews$score, reviews$burst_p)
print(head(reviews[ord, c("id", "rating", "length", "per_day", "helpful", "score", "label")], 10),
      row.names = FALSE)

write.csv(reviews, "out/reviews_scored.csv", row.names = FALSE)
cat("\nDone. Plots and scored data saved in the out/ folder.\n")
