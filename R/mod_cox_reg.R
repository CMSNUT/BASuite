#' cox_reg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cox_reg_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel(strong("Cox Regression Analysis",style="color:blue")),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput(ns("time"), "Time Variable", choices = NULL),
        selectInput(ns("status"), "Status Variable", choices = NULL),
        selectizeInput(ns("covariates"), "Covariates", choices = NULL, multiple = TRUE),

        h4("Analysis Settings"),
        selectInput(ns("analysis_type"), "Analysis Type",
                    choices = c("Univariate", "Multivariate", "Stepwise", "LASSO")),
        conditionalPanel(
          condition = "input.analysis_type == 'Univariate'",
          checkboxInput(ns("forest_uni"), "Show Forest Plot", TRUE)
        ),
        actionButton(ns("run"), "Run Analysis")
      ),
      mainPanel(
        width = 10,
        tabsetPanel(
          id = ns("cox_tabs"),
          tabPanel("Results",
                   conditionalPanel(
                     condition = "input.analysis_type == 'Univariate'",
                     plotOutput(ns("forest_plot_uni")),
                     downloadButton(ns("download_forest_uni"), "Download Plot")
                   ),
                   tableOutput(ns("result_table"))),

          tabPanel("Model Comparison",
                   tableOutput(ns("model_metrics")),
                   plotOutput(ns("model_comparison_plot"))),

          tabPanel("Nomogram",
                   plotOutput(ns("nomogram_plot")),
                   downloadButton(ns("download_nomogram"), "Download Plot")),

          tabPanel("Validation Curves",
                   plotOutput(ns("roc_plot")),
                   plotOutput(ns("calibration_plot")),
                   plotOutput(ns("decision_curve")))
        )
      )
    )
  )
}

#' cox_reg Server Functions
#'
#' @noRd
mod_cox_reg_server <- function(id,df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    data <- reactiveVal()

    observe({
      req(df())
      data(df())
      vars <- names(data())

      updateSelectInput(session, "time", choices = vars)
      updateSelectInput(session, "status", choices = vars)
      updateSelectizeInput(session, "covariates", choices = vars)
    })

    # cox_models ------
    cox_models <- eventReactive(input$run,{
      req(data(), input$time, input$status, input$covariates)

      # Prepare survival formula
      surv_formula <- as.formula(paste("Surv(", input$time, ",", input$status, ") ~ ."))

      # Perform analysis based on selected type
      switch(input$analysis_type,
             "Univariate" = {
               map(input$covariates, ~coxph(as.formula(paste0("Surv(", input$time, ",", input$status, ") ~", .x)), data = data()))
             },
             "Multivariate" = {
               coxph(surv_formula, data = data())
             },
             "Stepwise" = {
               full_model <- coxph(surv_formula, data = data())
               step(full_model, direction = "both")
             },
             "LASSO" = {
               x <- model.matrix(~., data()[, input$covariates])[,-1]
               y <- Surv(data()[[input$time]], data()[[input$status]])
               cvfit <- cv.glmnet(x, y, family = "cox")
               glmnet(x, y, family = "cox", lambda = cvfit$lambda.min)
             })
    })

    # forest_plot_uni ---------
    output$forest_plot_uni <- renderPlot({
      req(input$analysis_type == "Univariate")
      forest_model(cox_models())
    })

    # result_table  --------
    output$result_table <- renderTable({
      models <- cox_models()
      if (input$analysis_type == "Univariate") {
        map_df(models, ~broom::tidy(.x, exponentiate = TRUE, conf.int = TRUE))
      } else {
        broom::tidy(models, exponentiate = TRUE, conf.int = TRUE)
      }
    })

    # nomogram_plot --------
    output$nomogram_plot <- renderPlot({
      req(input$analysis_type %in% c("Multivariate", "Stepwise"))
      dd <<- datadist(data())
      options(datadist = "dd")
      cph_model <- cph(surv_formula, data = data(), surv = TRUE)
      nom <- nomogram(cph_model, fun = function(x) 1 - x, funlabel = "Survival Probability")
      plot(nom)
    })

    # roc_plot ----------
    output$roc_plot <- renderPlot({
      # ROC curve implementation using timeROC
      # Example for 1-year survival:
      roc_data <- timeROC(T = data()[[input$time]],
                          delta = data()[[input$status]],
                          marker = predict(cox_models()),
                          cause = 1,
                          times = 365)
      plot(roc_data)
    })

    # forest_plot ---------
    output$download_forest_uni <- downloadHandler(
      filename = function() "forest_plot.png",
      content = function(file) {
        ggsave(file, plot = forest_plot_uni(), device = "png")
      }
    )

  })
}

## To be copied in the UI
# mod_cox_reg_ui("cox_reg_1")

## To be copied in the server
# mod_cox_reg_server("cox_reg_1")
