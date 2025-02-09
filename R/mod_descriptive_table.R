#' descriptive_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_descriptive_table_ui <- function(id) {
  ns <- NS(id)
  tagList(sidebarLayout(
    sidebarPanel(
      width = 6,
      DTOutput(ns("tabone_data")),

      tags$br(),
      div(style = "border: 1px solid blue;height:0px; margin-top:3px; margin-bottom:0px"),
      tags$br(),

      column(
        width = 6,
        uiOutput(ns("select_vars_analysis_ui"))
      ),
      column(
        width = 6,
        checkboxInput(ns("is_custom_table"), "Customize table content"),
        conditionalPanel(
          ns = ns,
          condition = "input.is_custom_table",
          textAreaInput(
            ns("custom_table"),
            rows = 11,
            width = "100%",
            div(
              style = "disp:inline-block",
              strong("The statement for generating descriptive table:", style =
                       "color:red"),
              a("`compareGroups`", href = "https://isubirana.github.io/compareGroups/index.html", target =
                  "_blank")
            ),
            value = paste0("descrTable(.~.,
                           data,
                           show.descr = TRUE,
                           sd.type = 2,
                           q.type = c(2,2),
                           method = 4,
                           extra.labels=c('','','',''))")
          )
        )
      ),

      uiOutput(ns("export_items_ui")),

      div(
        style = "disp:inline-block",
        actionButton(ns("disp_table_one_btn"), "Displaying descriptive table"),
        downloadButton(ns("tabone_down_btn"), "Export the baseline analysis table")
      )
    ),
    mainPanel(width = 6, htmlOutput(ns("tabone_ui")))
  ))
}

#' descriptive_table Server Functions
#'
#' @noRd
mod_descriptive_table_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df1 <- reactiveVal()

    allVars <- reactiveVal()
    fVars <- reactiveVal()
    nfVars <- reactiveVal()

    allVars_opptions <- reactiveVal()
    fVars_opptions <- reactiveVal()
    nfVars_opptions <- reactiveVal()
    exportFile <- reactiveVal()

    # reactive for variables ----
    observe({
      req(df())
      dat <- df()

      allVars(colnames(dat))

      fVars(names(dat)[sapply(dat, is.factor)])

      nfVars(setdiff(allVars(), fVars()))

    })

    # export_items UI ----

    output$export_items_ui <- renderUI({
      if (input$strataing == TRUE) {
        tagList(radioButtons(
          ns("tabone_down_type_setting"),
          strong("Choosing the export type for baseline table:", style =
                   "color:red;"),
          c(
            "Word(.docx)" = ".docx",
            # "CSV(.csv)"=".csv",
            "PDF(.pdf)" = ".pdf",
            "HTML(.html)" = ".html",
            # "Markdown(.md)"=".md",
            "Latex(.tex)" = ".tex"
          ),
          inline = T
        ))
      } else {
        tagList(radioButtons(
          ns("tabone_down_type_setting"),
          strong("Choosing the export type for baseline table:", style =
                   "color:red;"),
          c(
            "Word(.docx)" = ".docx",
            "CSV(.csv)" = ".csv",
            "PDF(.pdf)" = ".pdf",
            "HTML(.html)" = ".html",
            # "Markdown(.md)"=".md",
            "Latex(.tex)" = ".tex"
          ),
          inline = T
        ))
      }

    })

    # data table ----
    output$tabone_data <- renderDT(
      # req(df()),
      df(),
      rownames = FALSE,
      escape = TRUE,
      extensions = 'Buttons',
      options = list(
        pageLength = 5,
        # lengthMenu = c(5, 10, 25, 50,100),
        lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
        dom = 'Bltipr',
        buttons = list(
          'copy',
          'print',
          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )
        ),
        scrollX = TRUE
      )
    )

    # custom_table -----
    observe({
      req(input$cols_to_tabone)
      vars <- input$cols_to_tabone
      generate_code<-""
      if ((input$grouping == TRUE) && nchar(input$group_var_to_tabone)>0) {
        group_var <- input$group_var_to_tabone

        if ((input$strataing == TRUE) && nchar(input$strata_var_to_tabone>0)){
          strata_var <- input$strata_var_to_tabone
          allvars <- setdiff(vars, c(group_var, strata_var))
          generate_code <-
            paste0(
              "strata(
            descrTable(",
              group_var,
              " ~ ",
              allvars,
              ",data,
              show.all = FALSE,
              show.descr = TRUE,
              sd.type = 2,
              q.type = c(2,2),
              method = 4,
              extra.labels=c('','','','')),",
              "'",
              strata_var,
              "')"
            )

        } else {
          allvars <- setdiff(vars, group_var)

          generate_code <-
            paste0(
              "
            descrTable(",
              group_var,
              " ~ ",
              allvars,
              ",data,
              show.all = FALSE,
              show.descr = TRUE,
              sd.type = 2,
              q.type = c(2,2),
              method = 4,
              extra.labels=c('','','',''))"
            )
        }

      }
      updateTextAreaInput(session, "custom_table", value = generate_code)

    })

    # Choosing Variables UI ----
    output$select_vars_analysis_ui <- renderUI({
      tagList(
        pickerInput(
          ns("cols_to_tabone"),
          strong("Choosing variable(s)", style = "color:red"),

          # selected = allVars(),
          choices = allVars(),
          options = pickerOptions(
            container = "body",
            actionsBox = TRUE,
            title = "Nothing Selected"
          ),
          multiple = TRUE,
          width = "100%"
        ),

        checkboxInput(
          ns("grouping"),
          strong("Grouping by:", style = "color:red")
        ),

        conditionalPanel(
          ns = ns,
          condition = "input.grouping",
          pickerInput(
            ns("group_var_to_tabone"),
            strong("Choosing grouping variable:", style = "color:blue;font-size=20px"),
            selected = 0,
            choices = fVars(),
            options = pickerOptions(
              container = "body",
              actionsBox = TRUE,
              title = "Nothing Selected"
            ),
            multiple = FALSE,
            width = "100%"
          ),

          checkboxInput(
            ns("strataing"),
            strong("Strataing by:", style = "color:red")
          ),

          conditionalPanel(
            ns = ns,
            condition = "input.strataing",
            pickerInput(
              ns("strata_var_to_tabone"),
              strong("Choosing strataing variable:", style = "color:blue;font-size=20px"),
              options = pickerOptions(
                container = "body",
                actionsBox = TRUE,
                title = "Nothing Selected"
              ),
              choices = fVars(),
              multiple = FALSE,
              width = "100%"
            )
          ),
          checkboxInput(ns("show_all"), strong("Show `all` column"), value = TRUE)
        )

      )
    })


    # disp_table_one_btn -----------
    observeEvent(input$disp_table_one_btn, {
      req(df())
      data <- df()

      req(input$cols_to_tabone)

      assign("data", df(), envir = .GlobalEnv) # 这个非常关键，否则找不到数据

      if (input$is_custom_table == TRUE) {
        ## compareGroups ----
        if (input$grouping) {
          formula <- as.formula(paste0(
            input$group_var_to_tabone,
            "~",
            paste0(
              setdiff(input$cols_to_tabone, input$group_var_to_tabone),
              collapse = "+"
            )
          ))

        } else {
          formula <- as.formula(paste0(
            input$group_var_to_tabone,
            "~",
            paste0(input$cols_to_tabone, collapse = "+")
          ))
        }

        if (input$grouping &&
            length(input$group_var_to_tabone) > 0 &&
            input$strataing &&
            length(input$strata_var_to_tabone) > 0 &&
            input$strata_var_to_tabone != input$group_var_to_tabone) {
          formula2 <- paste0(input$group_var_to_tabone,
                             "~",
                             paste0(setdiff(
                               input$cols_to_tabone,
                               c(
                                 input$group_var_to_tabone,
                                 input$strata_var_to_tabone
                               )
                             ), collapse = "+"))

          if (input$show_all) {
            generate_code <- paste0(
              "strataTable(descrTable(",
              formula2,
              ",data,
          show.all= TRUE,
          all.last= TRUE,
          show.descr = TRUE,
            sd.type = 2,
            q.type = c(2,2),
            method = 4,
            na.action = na.exclude,
            extra.labels=c('','','','')),",
              "'",
              input$strata_var_to_tabone,
              "')"
            )
          } else {
            generate_code <- paste0(
              "strataTable(descrTable(",
              formula2,
              ",data,
            show.all= FALSE,
            show.descr = TRUE,
            sd.type = 2,
            q.type = c(2,2),
            method = 4,
            na.action = na.exclude,
            extra.labels=c('','','','')),",
              "'",
              input$strata_var_to_tabone,
              "')"
            )
          }

          expr <- parse(text = generate_code)
          restab <- eval(expr)

        } else {
          showAll <- input$show_all

          if (input$show_all) {
            restab <- descrTable(
              formula,
              data,
              show.all = TRUE,
              all.last = TRUE,
              show.descr = TRUE,
              # show.ci = TRUE,
              sd.type = 2,
              q.type = c(2, 2),
              method = 4,
              extra.labels = c("", "", "", "")
            )
          } else {
            restab <- descrTable(
              formula,
              data,
              show.all = FALSE,
              show.descr = TRUE,
              # show.ci = TRUE,
              sd.type = 2,
              q.type = c(2, 2),
              method = 4,
              extra.labels = c("", "", "", "")
            )
          }
        }
      } else {
        if (length(input$custom_table) > 0 ) {
          custom_code <- input$custom_table
          expr <- parse(text = custom_code)
        restab <- eval(expr)
        }
      }

      if (class(restab)[1] == "descrTable" ) {
        html <- export2html(
          restab,
          header.background = "black",
          header.color = "white"
        )
        rows_num <- str_count(html, "</tr>") - 1

        ## 三线表样式 ----
        html <- html %>%
          kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
          row_spec(0,
                   bold = TRUE,
                   extra_css = "border-bottom: 1px solid black;
           border-top: 1px solid black;") %>%
          row_spec(rows_num, extra_css = "border-bottom: 1px solid black;") %>%
          column_spec(1, bold = TRUE)
      } else if (class(restab)[1] == "cbind.createTable") {
        html <- export2md(restab, format = "html", strip = T)
        rows_num <- str_count(html, "</tr>") - 2

        ## 三线表样式 ----
        html <- html %>%
          kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%

          row_spec(1, bold = TRUE, extra_css = "border-bottom: 1px solid black;") %>%
          row_spec(rows_num, extra_css = "border-bottom: 1px solid black;") %>%
          column_spec(1, bold = TRUE)
      }


      exportFile(restab)

      # tablone ----
      output$tabone_ui <- renderUI({
        HTML(html)
      })
    })

    # tabone_down_btn ----
    output$tabone_down_btn <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("tableone",
               sep = "-",
               Sys.Date(),
               input$tabone_down_type_setting)
      },
      content = function(file) {
        # formula <- as.formula(
        #   paste0(input$group_var_to_tabone, "~", paste0(setdiff(input$cols_to_tabone,input$group_var_to_tabone),collapse = "+"))
        # )
        # # res <- compareGroups(formula, df())
        # # restab <- createTable(res)
        # restab <- descrTable(formula,data = data)
        restab <- exportFile()

        if (input$tabone_down_type_setting == ".docx") {
          export2word(restab, file)
        } else if (input$tabone_down_type_setting == ".csv") {
          export2csv(restab, file)
        } else if (input$tabone_down_type_setting == ".pdf") {
          export2pdf(restab, file)
        } else if (input$tabone_down_type_setting == ".md") {
          export2md(restab, file, format = "html")
        } else if (input$tabone_down_type_setting == ".html") {
          export2html(restab, file)
        } else {
          export2latex(restab, file)
        }
      }
    )
  })
}

## To be copied in the UI
# mod_descriptive_table_ui("descriptive_table_1")

## To be copied in the server
# mod_descriptive_table_server("descriptive_table_1")
