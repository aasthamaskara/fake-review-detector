# Fake Review Detector

A small R Shiny app that checks online reviews for signs of being fake. It is the working part of our Probability and Statistics project, "Unmasking Fake Online Reviews: A Probability and Statistical Anomaly Detection Approach Using R".

By Aastha Maskara (25BCE2840) and Rayna (25BCE0651).

## What it does

Every review gets checked against four red flags:

1. Unusually short: the z-score of log(length) is below -1.5.
2. Review burst: the Poisson tail probability of that many reviews in one day is below 0.01.
3. Few helpful votes: the z-score of helpful votes is below -1.
4. Perfect rating: five stars.

The suspicion score is the number of flags a review trips, from 0 to 4. A score of 3 or more is marked as fake (you can change that cut-off in the app).

The app has two tabs:

- Check one review: type in a rating, length, reviews that day and helpful votes, and see the verdict, which flags were tripped and where the review sits on the Poisson curve.
- Analyse a CSV: upload your own file (or use the built-in simulated sample) and get the number flagged, a score chart, the ten most suspicious rows and a download of the scored file. If the file has a `label` column (genuine or fake) it also shows precision and recall.

## Run it

You need R (4.0 or newer) and the shiny package.

```r
install.packages("shiny")
```

Then, from the folder where you downloaded this repo:

```r
shiny::runApp("app")
```

Or run it straight from GitHub without downloading anything by hand:

```r
shiny::runGitHub("fake-review-detector", "aasthamaskara", subdir = "app")
```

## Files

```
app/app.R                 the Shiny app (interface and server)
app/scoring.R             scoring functions (z-scores, Poisson tail, flags) and the simulated sample data
R/fake_review_detection.R the full command line analysis used in the report
```

`R/fake_review_detection.R` runs the whole analysis with base R: summary statistics, four hypothesis tests, the Poisson burst probabilities, the suspicion score and the confusion matrix. Run it with `Rscript R/fake_review_detection.R` and it writes four plots and a scored CSV into an `out/` folder.

## Example

A 14 word, five star review, written on a day when its product got 7 reviews, with no helpful votes, trips all four flags and scores 4 out of 4. A 95 word, four star review on a day with 1 review and 3 helpful votes trips none. On the full sample, a cut-off of 3 flags 49 reviews and 48 of them are fake (precision 98.0%, recall 48.0%).

## Limits

The data is simulated, so the numbers show that the method works on data built to have these patterns. Real reviews will be messier, and the thresholds would need tuning on real labelled data. A high score means a review looks unusual. It does not prove the review is fake.
