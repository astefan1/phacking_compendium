library(shinydashboard)

# ==============================================================================
# Define header
# ==============================================================================
header <- dashboardHeader(title = "Effects of p-Hacking")

# ==============================================================================
# Define sidebar
# ==============================================================================
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "strategies",
    HTML("<p> <strong> <center> p-Hacking Strategies </strong> </center>"),
    menuItem("Composite Scores", tabName = "compositeScores",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Exploit Covariates", tabName = "exploitCovariates",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Exploit Cut-Offs", tabName = "exploitCutoffs",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Favorable Imputation", tabName = "favorableImputation",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Incorrect Rounding", tabName = "incorrectRounding",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Optional Stopping", tabName = "optionalStopping",
              icon = icon("stats",lib = "glyphicon")),
    menuItem("Outlier Exclusion", tabName = "outlierExclusion",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Selecting Effects", tabName = "selectEffects",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Selective Reporting DV", tabName = "selectiveReportingDV",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Selective Reporting IV", tabName = "selectiveReportingIV",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Exploiting Stat. Analysis", tabName = "statAnalysis",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Subgroup Analysis", tabName = "subgroupAnalysis",
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Variable Transformation", tabName = "variableTransformation",
             icon = icon("stats",lib = "glyphicon"))
  )
)

# ==============================================================================
# Define body
# ==============================================================================

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "compositeScores",
            fluidRow(box(width = 12, status = "primary",
                         h3("Composite scores / Scale redefinition"),
                         HTML("One of the variables in the statistical test is a composite score (e.g., mean of item scores). If the initial result is not significant, items are removed from the score to obtain a statistical significant result.")
            )),
            fluidRow(column(width=8,
                            fixedRow(box(width = 6, status = "primary",
                                         plotOutput("compScoresPlot1")),
                                     box(width = 6, status = "primary",
                                         plotOutput("compScoresPlot2"))),
                            fixedRow(box(width=6, status = "primary",
                                         plotOutput("compScoresPlot3")),
                                     box(width=6, status = "primary",
                                         h4("False positive rate (p-hacked)"),
                                         textOutput("compScoresFPHack"),
                                         h4("False positive rate (original)"),
                                         textOutput("compScoresFPOrig"))),
                            fixedRow(box(width=6, status = "primary",
                                         plotOutput("compScoresPlot4")),
                                     box(width=6, status = "primary",
                                         plotOutput("compScoresPlot5")))),
                     column(width=4,
                            box(width=12, status = "primary", height=750, background = "navy",
                                h4("Simulation settings"),
                                numericInput("nobsCompScores", label = "Number of observations", value = 30, min = 10, max = 1000),
                                numericInput("ncompvCompScores", label = "Number of variables in composite score", value = 5, min = 3, max = 20),
                                uiOutput("uindeleteCompScores"),
                                sliderInput("rcompCompScores", label = "Correlation between composite score variables", min = 0, max = 0.99, value = 0.8, step = 0.01),
                                radioButtons("strategyCompScores", label = "p-hacking strategy", choiceNames = list("First significant", "Smallest", "Smallest significant"), choiceValues = list("firstsig", "smallest", "smallest.sig")),
                                numericInput("alphaCompScores", label = "Significance level", value = 0.05, min = 0.001, max = 0.2),
                                numericInput("iterCompScores", label = "Simulation iterations", value = 1000, min = 10, max = 10000),
                                actionButton("calcCompScores", "Start simulation"))))),
    tabItem(tabName = "exploitCovariates",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Exploiting covariates"),
                         HTML("Different combinations of covariates are introduced in the statistical analysis until a significant result is obtained.")
            ))),
    tabItem(tabName = "exploitCutoffs",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Exploiting arbitrary cutoff values"),
                         HTML("Continuous independent variables are split into categories, such that, for example, high scorers and low scorers are compared to each other.")
            ))),
    tabItem(tabName = "favorableImputation",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Favorable imputation of missing values"),
                         HTML("Missing values are imputed in a way that is favorable for statistical significance.")
            ))),
    tabItem(tabName = "incorrectRounding",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Incorrect rounding"),
                         HTML("If the p-value is close to significance, a significant p-value is reported (i.e., the true p-value is rounded downwards).")
            ))),
    tabItem(tabName = "optionalStopping",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Optional stopping / Data peeking"),
                         HTML("Results of the statistical test are inspected during data collection. Data collection is stopped as soon as a significant result has been obtained.")
            ))),
    tabItem(tabName = "outlierExclusion",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Outlier exclusion"),
                         HTML("Outliers are excluded from the dataset with the goal to obtain a significant result.")
            ))),
    tabItem(tabName = "selectEffects",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Selective reporting of effects"),
                         HTML("In statistical models that contain multiple independent variables (e.g., ANOVA, multiple linear regression), only significant effects are reported.")
            ))),
    tabItem(tabName = "selectiveReportingDV",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Selective reporting of the dependent variable"),
                         HTML("The dataset contains several dependent variables. The p-value is hacked by recomputing the statistical test with several dependent variables and reporting a significant test result.")
            ))),
    tabItem(tabName = "selectiveReportingIV",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Selective reporting of the independent variable"),
                         HTML("The dataset contains one control group variable and multiple treatment group variables. The p-value is hacked by recomputing the statistical test with different treatment groups and reporting a significant test result.")
            ))),
    tabItem(tabName = "statAnalysis",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Exploiting statistical analysis options"),
                         HTML("If one statistical test does not yield a significant result (e.g., t-test), a slightly different statistical test is used (e.g., Welch-test).")
            ))),
    tabItem(tabName = "subgroupAnalysis",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Subgroup analyses / Tinkering with inclusion criteria"),
                         HTML("If a statistical test is not significant, the test is repeated in different subgroups of the sample (e.g., for male and female participants).")
            ))),
    tabItem(tabName = "variableTransformation",
            fluidRow(box(width = 10, status = "primary", height = 800,
                         h3("Exploiting variable transformations"),
                         HTML("If the results of an initial statistical test are not significant, the involved variables are transformed to obtain statistical significance.")
            )))
  )
)

# ==============================================================================
# Create UI
# ==============================================================================
dashboardPage(header, sidebar, body)
