# Fake Review Detector (Shiny)
# Aastha Maskara (25BCE2840), Rayna (25BCE0651)
# Run from the repo folder:  shiny::runApp("app")

library(shiny)
source("scoring.R")

sample_df <- simulate_reviews()
sample_ref <- make_reference(sample_df)

flag_names <- c("Unusually short", "Review burst", "Few helpful votes", "Perfect rating")

css <- "
body { font-family: Calibri, Arial, sans-serif; color: #1E2330; }
.title-bar { background: #1E2330; color: white; padding: 18px 24px; margin: -15px -15px 20px -15px; }
.title-bar h2 { margin: 0; font-family: Cambria, Georgia, serif; }
.title-bar p { margin: 4px 0 0 0; color: #F2B84B; }
.verdict { padding: 16px 20px; border-radius: 8px; color: white; font-size: 22px; font-weight: bold; margin-bottom: 12px; }
.verdict small { display: block; font-size: 14px; font-weight: normal; margin-top: 4px; }
.v-fake { background: #E45756; } .v-maybe { background: #F2B84B; color: #1E2330; } .v-ok { background: #4C78A8; }
.stat { background: #F1F3F6; border-radius: 8px; padding: 12px 16px; margin-bottom: 10px; }
.stat b { font-size: 24px; font-family: Cambria, Georgia, serif; display: block; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  div(class = "title-bar",
      h2("Fake Review Detector"),
      p("Probability and statistical anomaly detection, written in R")),
  tabsetPanel(
    tabPanel("Check one review",
      br(),
      sidebarLayout(
        sidebarPanel(width = 4,
          selectInput("rating", "Star rating", choices = 5:1, selected = 5),
          numericInput("len", "Review length (words)", value = 14, min = 1, max = 2000),
          numericInput("perday", "Reviews this product got that day", value = 7, min = 0, max = 500),
          numericInput("helpful", "Helpful votes on the review", value = 0, min = 0, max = 500),
          sliderInput("cut1", "Flag as fake at this many red flags or more", min = 1, max = 4, value = 3, step = 1),
          helpText("Normal behaviour is learned from the bundled sample of 1,000 simulated reviews.")
        ),
        mainPanel(width = 8,
          uiOutput("verdict"),
          fluidRow(
            column(6, tableOutput("flag_table")),
            column(6, plotOutput("pois_plot", height = "260px"))
          )
        )
      )
    ),
    tabPanel("Analyse a CSV",
      br(),
      sidebarLayout(
        sidebarPanel(width = 4,
          checkboxInput("use_sample", "Use the bundled sample data", value = TRUE),
          conditionalPanel("!input.use_sample",
            fileInput("file", "Upload a CSV", accept = ".csv"),
            helpText("Needs columns: rating, length, per_day, helpful. An optional label column (genuine / fake) adds accuracy results.")),
          sliderInput("cut2", "Flag as fake at this many red flags or more", min = 1, max = 4, value = 3, step = 1),
          downloadButton("dl", "Download scored CSV")
        ),
        mainPanel(width = 8,
          uiOutput("err"),
          fluidRow(
            column(3, uiOutput("s_total")), column(3, uiOutput("s_flag")),
            column(3, uiOutput("s_prec")), column(3, uiOutput("s_rec"))
          ),
          plotOutput("score_plot", height = "280px"),
          h4("Most suspicious reviews"),
          tableOutput("top_table")
        )
      )
    ),
    tabPanel("About",
      br(),
      p("Each review is checked against four red flags and gets a suspicion score from 0 to 4."),
      tags$ol(
        tags$li("Unusually short: the z-score of log(length) is below -1.5."),
        tags$li("Review burst: the Poisson tail probability of that many reviews in a day is below 0.01."),
        tags$li("Few helpful votes: the z-score of helpful votes is below -1."),
        tags$li("Perfect rating: five stars.")
      ),
      p("The bundled data is simulated (900 genuine and 100 fake reviews), so the true labels are known."),
      p("This tool is a screening aid. A high score means the review looks unusual, not that it is proven fake.")
    )
  )
)

server <- function(input, output, session) {

  one <- reactive({
    validate(need(!is.na(input$len) && input$len >= 1, "Enter a review length of at least 1 word."),
             need(!is.na(input$perday) && input$perday >= 0, "Enter reviews per day (0 or more)."),
             need(!is.na(input$helpful) && input$helpful >= 0, "Enter helpful votes (0 or more)."))
    d <- data.frame(rating = as.numeric(input$rating), length = input$len,
                    per_day = input$perday, helpful = input$helpful)
    score_reviews(d, sample_ref)
  })

  output$verdict <- renderUI({
    r <- one(); sc <- r$score; cut <- input$cut1
    cls <- if (sc >= cut) "v-fake" else if (sc == cut - 1 && cut > 1) "v-maybe" else "v-ok"
    msg <- if (sc >= cut) "Looks fake" else if (cls == "v-maybe") "Borderline" else "Looks normal"
    div(class = paste("verdict", cls), msg,
        tags$small(sprintf("Suspicion score %d out of 4 (flagged at %d or more)", sc, cut)))
  })

  output$flag_table <- renderTable({
    r <- one()
    data.frame(
      Flag = flag_names,
      Value = c(sprintf("z = %.2f", r$z_len), sprintf("P = %.5f", r$burst_p),
                sprintf("z = %.2f", r$z_help), sprintf("%d stars", r$rating)),
      Tripped = ifelse(c(r$flag_short, r$flag_burst, r$flag_help, r$flag_five), "Yes", "No")
    )
  }, striped = TRUE, width = "100%")

  output$pois_plot <- renderPlot({
    r <- one(); k <- 0:15; lam <- sample_ref$lambda
    cols <- rep("#4C78A8", length(k)); cols[k == min(r$per_day, 15)] <- "#E45756"
    bp <- barplot(dpois(k, lam), names.arg = k, col = cols, border = "white",
            main = sprintf("Normal reviews per day (Poisson, lambda = %.2f)", lam),
            xlab = "Reviews in a day", ylab = "Probability", cex.main = 0.95)
    points(bp[min(r$per_day, 15) + 1], max(dpois(k, lam)) * 0.04, pch = 17, col = "#E45756", cex = 1.6)
    legend("topright", bty = "n", legend = sprintf("P(%d or more) = %.5f", r$per_day, r$burst_p))
  })

  err_msg <- reactiveVal(NULL)

  data_in <- reactive({
    err_msg(NULL)
    if (input$use_sample) return(sample_df)
    req(input$file)
    d <- tryCatch(check_columns(read.csv(input$file$datapath, stringsAsFactors = FALSE)),
                  error = function(e) { err_msg(conditionMessage(e)); NULL })
    req(d)
    d
  })

  scored <- reactive({
    d <- data_in()
    s <- score_reviews(d)
    s$flagged <- s$score >= input$cut2
    s
  })

  output$err <- renderUI({
    try(data_in(), silent = TRUE)
    msg <- err_msg()
    if (!is.null(msg)) div(class = "verdict v-fake", "Could not read the file", tags$small(msg))
  })

  stat <- function(value, label) div(class = "stat", tags$b(value), label)
  has_label <- reactive({ "label" %in% names(scored()) && all(c("fake", "genuine") %in% scored()$label) })

  output$s_total <- renderUI({ stat(format(nrow(scored()), big.mark = ","), "reviews") })
  output$s_flag  <- renderUI({ stat(sum(scored()$flagged), "flagged as fake") })
  output$s_prec  <- renderUI({
    s <- scored()
    if (!has_label()) return(stat("n/a", "precision (no label column)"))
    tp <- sum(s$flagged & s$label == "fake"); fp <- sum(s$flagged & s$label == "genuine")
    stat(if (tp + fp == 0) "n/a" else sprintf("%.1f%%", 100 * tp / (tp + fp)), "precision")
  })
  output$s_rec <- renderUI({
    s <- scored()
    if (!has_label()) return(stat("n/a", "recall (no label column)"))
    tp <- sum(s$flagged & s$label == "fake"); fn <- sum(!s$flagged & s$label == "fake")
    stat(if (tp + fn == 0) "n/a" else sprintf("%.1f%%", 100 * tp / (tp + fn)), "recall")
  })

  output$score_plot <- renderPlot({
    s <- scored()
    if (has_label()) {
      tb <- table(factor(s$label, levels = c("fake", "genuine")), factor(s$score, levels = 0:4))
      barplot(tb, beside = TRUE, col = c("#E45756", "#4C78A8"), border = NA,
              main = "Suspicion score by true review type", xlab = "Red flags tripped", ylab = "Reviews",
              legend.text = rownames(tb), args.legend = list(x = "topright", bty = "n"))
    } else {
      barplot(table(factor(s$score, levels = 0:4)), col = "#4C78A8", border = NA,
              main = "Suspicion score", xlab = "Red flags tripped", ylab = "Reviews")
    }
  })

  top_cols <- c("rating", "length", "per_day", "helpful", "score")
  output$top_table <- renderTable({
    s <- scored(); s <- s[order(-s$score, s$burst_p), ]
    cols <- c(intersect("id", names(s)), top_cols, intersect("label", names(s)))
    head(s[, cols, drop = FALSE], 10)
  }, striped = TRUE, digits = 0, width = "100%")

  output$dl <- downloadHandler(
    filename = function() "scored_reviews.csv",
    content = function(file) write.csv(scored(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
