#' data_preparation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_preparation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 6,
        div(
          # style = "inline-block",
          radioButtons(ns("data_source"),HTML("<format style='font-size:16pt;color:blue'><strong>Choosing data source</strong></format>"),
                       choices = c("Local Data"="local","Sample Data"="example"),
                       inline = TRUE
          ),
          # uiOutput(ns("data_source_ui")),
          conditionalPanel(
            ns = ns,
            condition = "input.data_source == 'local'",
            fileInput(
              ns("file"),
              strong("Uploading local data:",style="color:blue;font-size:16px"),
              accept = c(".csv",".xls",".xlsx"),
              width = "100%",
              placeholder = "uploading file (*.csv, *.xlsx,*.xls)"
            )
          ),
          conditionalPanel(
            ns = ns,
            condition = "input.data_source == 'example'",
            pickerInput(
              ns("sample_data"),
              strong("Loading sample data",style="color:blue;font-size:12pt"),
              list(`Baseline Data` = list("Cross-Sectional Study"="regicor",
                                          "Case-Control Study"= "SNPs"),
                   `Kaplan-Meier Survival Curve` = list("Survival in patients with advanced lung cancer"="lung"),
                   `Cox's Regression` = list("Primary biliary cholangitis"="pbc"),
                   `Logistic Regression` = list("Adjuvant chemotherapy for colon cancer"="colon")
              ),
              options = pickerOptions(container = "body",
                                      title = "Choosing sample data")
            )
          )
        ),

        div(style="border: 1px solid blue;height:0px; margin-top:3px; margin-bottom:0px"),

        tabsetPanel(
          id=ns("tabs"),

          ## Converting Data ----
          tabPanel(
            "Converting Data",
            uiOutput(ns("data_convert_ui")),
            actionButton(ns("data_convert_btn"),"Confirm")
          ),

          ## Renaming Variable ----
          tabPanel(
            "Renaming Variable",
            uiOutput(ns("col_rename_ui")),
            actionButton(ns("col_rename_btn"),"Confirm")
          ),

          ## Resetting Type of Variables ----
          tabPanel(
            "Resetting Type of Variables",
            uiOutput(ns("col_retype_ui")),
            actionButton(ns("col_retype_btn"),"Confirm")
          ),

          ## Relabelling Factor Variable ----
          tabPanel(
            "Relabelling Factor Variable",
            uiOutput(ns("factor_relabel_ui")),
            actionButton(ns("factor_relabel_btn"),"Confirm")
          ),

          ## Generating New Variable ----
          tabPanel(
            "Generating New Variable",
            uiOutput(ns("col_generate_ui")),
            actionButton(ns("col_generate_btn"),"Confirm")
          ),

          ## Setting Data Subsets ----
          tabPanel(
            "Setting Data Subsets",
            uiOutput(ns("subset_setting_ui")),
            checkboxInput(ns("split_setting"),"Spliting the dataset to the training subset and testing subset",value = FALSE),
            uiOutput(ns("split_setting_ui")),
            actionButton(ns("subset_setting_btn"),"Confirm")
          )
        )
      ),
      mainPanel(
        width = 6,
        DTOutput(ns("data_table")),
        div(style="border: 1px solid blue;height:0px; margin-top:3px; margin-bottom:0px"),
        verbatimTextOutput(ns("data_summary"))
      )
    )
  )
}

#' data_preparation Server Functions
#'
#' @noRd
mod_data_preparation_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    df <- reactiveVal()

    allVars <- reactiveVal()
    fVars <- reactiveVal()
    nfVars <- reactiveVal()

    allVars_opptions <- reactiveVal()
    fVars_opptions <- reactiveVal()
    nfVars_opptions <- reactiveVal()


    # ######### loading data ####################


    ## uploading local data ####
    df_1 <- reactive({
      req(input$file)

      file <- input$file
      ext <- tools::file_ext(file$datapath)
      validate(need(ext %in% c("csv", "xlsx", "xls"),
                    "Needing *.csv, *.xlxs, or *.xls file"))

      if (ext == "csv") {
        dat <- read.csv(
          file$datapath
        )
      } else if (ext %in% c("xlsx", "xls")) {
        dat <- read_excel(
          file$datapath
        )
      } else {
        stop("Unsupported the file format!")
      }
    })

    ## Reactive value of Local data  ####
    observe({
      req(df_1())
      df(as.data.frame(df_1()))
    })

    ## loading sample data ####
    observeEvent(input$sample_data, {
      req(input$sample_data)
      data_name <- input$sample_data
      data(list = data_name)
      dat <- get(data_name)
      df(dat)
    })



    # ######### Display data information ########


    ## Data Table ####
    output$data_table <- renderDT(
      df(),
      rownames = FALSE,
      escape = TRUE,
      extensions = 'Buttons',
      options=list(
        pageLength = 5,
        # lengthMenu = c(5, 10, 25, 50,100),
        lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
        dom = 'Bltipr',
        buttons = list(
          'copy',
          'print',
#
#           list(
#             extend = 'print',
#             text = 'Print'
#           ),

          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )
        ),
        scrollX = TRUE
      )
    )

    ## Data Summary #####
    output$data_summary <- renderPrint({
      req(df())
      str(df())
      # summary(df())
    })




    # ######### Data Convert UI #################


    output$data_convert_ui <- renderUI({
      tagList(
        radioButtons(
          ns("data_convert"),
          strong("Choosing convert type:", style = "color:blue"),
          choices = c(
            "Wide data convert to Long data" = "w2l",
            "Long data convert to Wide data" = "l2w",
            "Custom statement" = "custom"
          ),
          inline = T
        ),

        conditionalPanel(
          ns = ns,
          condition = "input.data_convert == 'w2l'",
            pickerInput(
              ns("cols"),
              "Grouping by ...",
              choices = c(allVars_opptions()),
              multiple = T,
              options = pickerOptions(container = "body", actionsBox = TRUE)
            ),
            textInput(
              ns("names_to"),
              "Naming the new grouping column",
              value = "name",
              placeholder = "the name of grouping column"
            ),
            textInput(
              ns("value_to"),
              "Naming the new statistical column",
              value = "value",
              placeholder = "the name of statistical column"
            ),
            checkboxInput(ns("values_drop_na"), "Droping the missing values", value = FALSE)
          ),

          conditionalPanel(
            ns = ns,
            condition = "input.data_convert == 'l2w'",
            selectInput(
              ns("named_from"),
              "Get the name from the grouping column",
              selected = 0,
              choices = c(`Factor` = list(fVars()), `Non-Facator` = list(nfVars()))
            ),
            pickerInput(
              ns("values_from"),
              "Get the values from the column(s)",
              multiple = T,
              options = pickerOptions(container = "body", actionsBox = TRUE),
              choices = c(`Factor` = list(fVars()), `Non-Facator` = list(nfVars()))
            ),
            textInput(
              ns("values_fill"),
              "Filled missing values",
              value = NA,
              placeholder = "eg. NA or 0 ..., default NA"
            )
          ),

          conditionalPanel(
            ns = ns,
            condition = "input.data_convert == 'custom'",

            textAreaInput(
              ns("data_convert_statement"),
              div('Customize(',
                  a("Usage: Pivot Details",
                    href = "https://tidyr.tidyverse.org/articles/pivot.html",
                    target = "_blank"),")"),
              rows = 10,
              placeholder = 'long data convert to wide data:
                        pivot_wider(
                          data,
                          ...,
                          id_cols = NULL,
                          id_expand = FALSE,
                          names_from = name,
                          names_prefix = "",
                          names_sep = "_",
                          names_glue = NULL,
                          names_sort = FALSE,
                          names_vary = "fastest",
                          names_expand = FALSE,
                          names_repair = "check_unique",
                          values_from = value,
                          values_fill = NULL,
                          values_fn = NULL,
                          unused_fn = NULL
                        );
                        wide data convert to long data:
                        pivot_longer(
                          data,
                          cols,
                          ...,
                          cols_vary = "fastest",
                          names_to = "name",
                          names_prefix = NULL,
                          names_sep = NULL,
                          names_pattern = NULL,
                          names_ptypes = NULL,
                          names_transform = NULL,
                          names_repair = "check_unique",
                          values_to = "value",
                          values_drop_na = FALSE,
                          values_ptypes = NULL,
                          values_transform = NULL
                        )'
            )
          )
        )
    })



    # ######### Reanaming Variable UI ###########

    output$col_rename_ui <- renderUI({

      tagList(
        selectInput(ns("rename_var"), "Choosing variable for renaming", choices = allVars_opptions()),
        textInput(ns("new_var_name"), "New name for the variable", value = ""),
        helpText(p("Not acceptable special characters in variable names.", style = "color:red"))
      )
    })


    # ######### Retype variables  UI #################

    output$col_retype_ui <- renderUI({
      tagList(
        pickerInput(
          ns("retype_vars"),
          "Choosing the variables",
          choices = allVars_opptions(),
          multiple = TRUE,
          options = pickerOptions(container = "body", actionsBox = TRUE),
          width = "100%"
        ),

        pickerInput(
          ns("target_class"),
          "Choosing the new class for variables",
          choices = c("factor variable"="factor",
                      "charater variable"="charater",
                      "numeric variable"="numeric",
                      "integer variable"="integer"),
          multiple = FALSE,
          options = pickerOptions(container = "body", actionsBox = TRUE),
          width = "100%"
        )
      )
    })


    # ##### Relabelling factor variable UI ######

    output$factor_relabel_ui <- renderUI({
        tagList(
          pickerInput(
            ns("relabel_var"),
            strong("Choosing the factor variable",style="color:blue;font-size=20px"),
            choices = fVars_opptions(),
            multiple = FALSE,
            options = pickerOptions(container = "body", title = "Nothing Selected"),
            width = "100%"
          ),

          radioButtons(
            ns("modify_method"),
            strong("Choosing the relabelling method",style="color:blue;font-size=20px"),
            choices = c("Relabelling on the original column"="notNew","Generate new variable (column)" = "addNew"),
            selected = "notNew",
            inline = TRUE
          )
        )
      })


    # ######### Generating New Variable UI ######

    output$col_generate_ui <- renderUI({

    })


    # ######### Setting Data Subsets UI #########

    output$subset_setting_ui <- renderUI({
      tagList(
        pickerInput(
          ns("cols_for_subset"),
          "Choosing columns(s) for subset",
          selected = allVars_opptions(),
          choices = allVars_opptions(),
          multiple = TRUE,
          options = pickerOptions(container = "body", actionsBox = TRUE),
          width = "100%"
        ),
        pickerInput(
          ns("rows_for_subset"),
          "Choosing row(s) for subset",
          selected = c(1:nrow(df())),
          choices = c(1:nrow(df())),
          multiple = TRUE,
          options = pickerOptions(container = "body", actionsBox = TRUE),
          width = "100%"
        )
      )
    })

    output$split_setting_ui <- renderUI({
      if (input$split_setting && !("subset" %in% allVars())){
        tagList(
          numericInput(ns("seed"),"Seeting random seed",value = 1234,min=0,step=1),
          numericInput(ns("ratio"),"The ratio for training set:",
                       max=1,
                       min=0.1,
                       step = 0.01,
                       value = 0.7
          ),
          pickerInput(
            ns("yvar"),
            strong("Choosing the dependent variable:",style="color:blue;font-size=20px"),
            choices = input$cols_for_subset,
            multiple = FALSE,
            options = pickerOptions(container = "body", title = "Nothing Selected"),
            width = "100%"
          )
        )
      }
    })


    # ######### data_convert_btn #################

    observeEvent(input$data_convert_btn, {
      req(df())
      dat <- df()

      if (input$data_convert == "w2l") {
        # 使用pivot_longer将宽数据转为长数据

        long_data <- dat %>%
          pivot_longer(
            cols = input$cols,
            names_to = input$names_to,
            value_to = input$value_to,
            values_drop_na = input$values_drop_na
          )
        df(long_data)
      } else if (input$data_convert == "l2w") {
        # 使用pivot_wider将长数据转为宽数据
        wide_data <- dat %>%
          pivot_wider(
            names_from = input$names_from,
            values_from = input$values_from,
            values_fill = input$values_fill
          )
        df(wide_data)
      } else {
        data <- dat
        parsed_code <- parse(text = input$data_convert_statement)
        data <- eval(parsed_code)
        df(data)
      }
    })



    # ######### col_rename_btn #################

    observeEvent(input$col_rename_btn,{
      req(df())
      dm <- df()

      if (nchar(input$rename_var) > 0 &&
          nchar(trimws(input$new_var_name)) > 0 &&
          input$rename_var != trimws(input$new_var_name)) {
        cnames <- allVars()
        cnames[which(cnames == input$rename_var)] = trimws(input$new_var_name)
        allVars(cnames)
        colnames(dm) <- allVars()
        df(dm)
      }

    })

    # ######### col_retype_btn #################

    observeEvent(input$col_retype_btn,{
      req(df())

      if (length(input$retype_vars) > 1) {
        dm <- df()
        if (input$target_class == "character") {
          try(
            {dm[, input$retype_vars] <- lapply(dm[, input$retype_vars], as.character)},
            silent = TRUE
          )
        } else if (input$target_class == "factor") {
          try(
            {dm[, input$retype_vars] <- lapply(dm[, input$retype_vars], as.factor)},
            silent = TRUE
          )
        } else if (input$target_class == "numeric") {
          try(
            {dm[, input$retype_vars] <- lapply(dm[, input$retype_vars], as.numeric)},
            silent = TRUE
          )
        } else if (input$target_class == "integer") {
          try({
            dm[, input$retype_vars] <- lapply(dm[, input$retype_vars], as.integer)
          },silent = TRUE)
        }
        df(dm)
      }
    })


    # ######### factor_relabel #################



    # Used to store dynamically generated UI elements's ID
    ui_ids <- reactiveValues(ids = c())

    ## UI ----
    observeEvent(input$relabel_var,{
      dm2 <- df()
      if (length(ui_ids$ids) > 0) {
        for (i in 1:length(ui_ids$ids)){
          removeUI(
            # selector = paste0("#", ui_ids$ids[i])
            selector = paste0("div:has(> #", ui_ids$ids[i])
          )
        }
      }

      if (nchar(input$relabel_var) > 0) {
        lbs <- unique(dm2[, input$relabel_var])
        for (i in 1:length(lbs)) {
          new_id <- paste0(ns("labstxt_"), i)
          ui_ids$ids <- c(ui_ids$ids, new_id)
        }

        for (i in 1:length(lbs)) {
          insertUI(
            selector = paste0("#", ns("modify_method")),
            where = "beforeBegin",
            ui = textInput(paste0(ns("labstxt_"), i),
                           paste0("修改变量标签 ", i," 的值"),
                           value = lbs[i])
          )
        }
      }
    })

    ## factor_relabel_btn ----
    observeEvent(input$factor_relabel_btn, {
      req(df())

      data <- df()
      input_lst <- reactiveValuesToList(input)
      lbs <- unique(data[, input$relabel_var]) %>% as.character
      newLbs <- c()
      for (i in 1:length(lbs)) {
        newLbs <- c(newLbs,input_lst[[sub(paste0(id,"-"),"",ui_ids$ids[i])]])
      }

      if (input$modify_method == "notNew") {
        data[,input$relabel_var]=factor(data[,input$relabel_var], levels = lbs,labels = newLbs)

      } else if (input$modify_method == "addNew") {
        newVar <- paste("new",input$relabel_var,sep = "_")

        data[,newVar] <- data[,input$relabel_var]
        data[,newVar]=factor(data[,newVar], levels = lbs,labels = newLbs)
      }

      df(data)

    })


    # ######### subset_setting_btn #################

    observeEvent(input$subset_setting_btn,{
      req(df())
      dm <- df()
      subset_dm <- dm[input$rows_for_subset,input$cols_for_subset]

      if (input$split_setting){
        set.seed(input$seed)

        # 删除因变量y中的缺失值
        subset_dm <- subset_dm[!is.na(subset_dm[,input$yvar]), ]

        index <- createDataPartition(subset_dm[,input$yvar], p = input$ratio, list = FALSE)

        # 创建训练集和测试集
        train_data <- subset_dm[index, ]
        test_data <- subset_dm[-index, ]

        train_data$subset <- "train"
        test_data$subset <- "test"

        subset_dm <- rbind(train_data,test_data)
        subset_dm$subset <- as.factor(subset_dm$subset)
      }

      df(subset_dm)
    })


    # reactive for variables ----
    observe({
      req(df())
      dat <- df()

      allVars(colnames(dat))

      fVars(names(dat)[sapply(dat,is.factor)])

      nfVars(setdiff(allVars(),fVars()))

      if (length(allVars()) > 0 ) {
        f <- sapply(dat,class)
        g1<- c(s=names(f))
        names(g1) <- paste0(names(f)," {",f,"}")
        allVars_opptions(c(g1))
      } else {
        allVars_opptions(NULL)
      }

      if (length(fVars()) > 0 ) {
        f <- sapply(dat[fVars()],class)
        g1<- c(s=names(f))
        names(g1) <- paste0(names(f)," {",f,"}")
        fVars_opptions(c(g1))
      } else{
        fVars_opptions(NULL)
      }

      if (length(nfVars()) > 0 ) {
        f <- sapply(dat[nfVars()],class)
        g1<- c(s=names(f))
        names(g1) <- paste0(names(f)," {",f,"}")
        nfVars_opptions(c(g1))
      } else {
        nfVars_opptions(NULL)
      }

      if ("subset" %in% allVars()) {
        updateCheckboxInput(inputId = "split_setting",value = FALSE)
      }
    })

    # Return ####

    return(df)
  })
}

## To be copied in the UI
# mod_data_preparation_ui("data_preparation_1")

## To be copied in the server
# mod_data_preparation_server("data_preparation_1")
