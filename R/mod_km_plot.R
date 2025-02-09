#' km_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_km_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(sidebarLayout(
    # sidebarPanel ----
    sidebarPanel(
      width = 6,
      fluidRow(


        ## Essential Settings -----
        # column(
        box(
          width = 4,
          solidHeader = T,
          collapsible = T,
          status = "primary",
          title = strong("Essential Settings"),
          # h4(strong("Essential Settings"), style = "color:blue;"),
          ### time ----
          selectInput(ns("time"), "Time Variable", choices = NULL),

          ###  status ----
          selectInput(ns("status"), "Status Variable", choices = NULL),

          ### task  ----
          radioButtons(
            ns("run_task"),
            "Target Task",
            choices = c(
              "Single Survival Analysis" = "single",
              "Batch Survival Analysis" = "batch"
            )
          ),


          ### strata  ----
          pickerInput(
            ns("strata"),
            "Strataing Variable",
            choices = NULL,
            multiple = T,
            options = pickerOptions(container = "body", actionsBox = TRUE)
          ),


          conditionalPanel(
            ns = ns,
            condition = "input.run_task == 'single'",

            radioButtons(
              ns("run_group"),
              "Grouping",
              selected = "No",
              choices = c("Yes", "No"),
              inline = T
            ),

            ### group ----
            conditionalPanel(
              ns = ns,
              condition = "input.run_group == 'Yes'",
              pickerInput(
                ns("group"),
                "Grouping by:",
                choices = NULL,
                multiple = T,
                options = pickerOptions(container = "body", actionsBox = TRUE)
              )
            ),

            conditionalPanel(
              ns = ns,
              condition = "input.run_group == 'No'",

              radioButtons(
                ns("run_facet"),
                "Faceting",
                selected = "No",
                choices = c("Yes", "No"),
                inline = T
              ),

              ### facet ----
              conditionalPanel(
                ns = ns,
                condition = "input.run_facet == 'Yes'",
                pickerInput(
                  ns("facet"),
                  "Facing by:",
                  choices = NULL,
                  multiple = T,
                  options = pickerOptions(container = "body", actionsBox = TRUE)
                )
              )
            )
          )
        ),
        # end box 1

        ## Curve Settings ----
        box(
          width = 4,
          solidHeader = T,
          collapsible = T,
          status = "primary",
          title = strong("Curve Settings"),

          ### Plot Type ----
          selectInput(
            ns("fun"),
            "Plot Type",
            choices = c(
              "Survival Curve" = "pct",
              "Cumulative Events" = "event",
              "Cumulative Risk" = "cumhaz"
            )
          ),

          ### Line Color ----
          selectInput(
            ns("palette"),
            "Line Color",
            selected = "npg",
            choices = c(
              "hue",
              "npg",
              "aaas",
              "lancet",
              "jco",
              "ucscgb",
              "uchicago",
              "simpsons",
              "rickandmorty"
            )
          ),

          ### Line Size ----
          numericInput(
            "linesize",
            "Line Size",
            value = 1,
            min = 0.5,
            max = 3
          ),

          ### Median Survival Time ----
          selectInput(
            ns("surv_median_line"),
            "Showing Median Survival Time",
            selected = "hv",
            choices = c(
              "None" = "none",
              "hv" = "hv",
              "Horizontal" = "h",
              "Vertical" = "v"
            )
          ),

          checkboxInput(ns("censor"),strong("Showing Censor Maker"),TRUE),

          conditionalPanel(
            ns=ns,
            condition = "input.censor",

            pickerInput(
              ns("censor_shape"),
              "Censor Maker Shape",
              selected = "3",
              choices = c("Triangle"="1", "Fork"="2", "Cross"="3", "Circle"="4")
            ),
            numericInput(
              ns("censor_size"),
              "Censor Maker Size",
              value = 3,
              max = 10,
              min = 0,
              step = 0.01
            ),
          checkboxInput(ns("add_all"), strong("Showing All"), FALSE)
        )
        ),
        # end box 2

        ## Legend Settings ----
        box(
          width = 4,
          solidHeader = T,
          collapsible = T,
          status = "primary",
          title = strong("Legend"),

          textInput(ns("legend_title"),"Title",value = "Strata", placeholder = "Strata"),
          numericInput(ns("legend_fontsize"),"Title Text Size",
                       value = 6,max=10,min=0,step = 0.1),
          selectInput(ns("legend_fontface"),"Title Text Style",
                      selected = "bold", choices = c("Plain" = "plain",
                                                     "Bold" = "bold",
                                                     "Italic" = "italic",
                                                     "Bold Italic" = "bold.italic"),
          ),

          colourpicker::colourInput(ns("legend_fontcolor"), "Title Text Color", value = "#030303"),

          uiOutput(ns("labs")),

          selectInput(
            ns("legend_pos"),
            "Legend Position Type",
            selected = "inside",
            choices = c(
              "Outside" = "outside",
              "Inside" = "inside",
              "None" = "none"
            )
          ),
          conditionalPanel(
            ns=ns,
            condition = "input.legend_pos == 'outside'",
            pickerInput(
              ns("legend"),
              "Legend Position",
              selected = "top",
              choices = c(
                "Top" = "top",
                "Bottom" = "bottom",
                "Left" = "left",
                "Right" = "right"
              )
            )
          ),
          conditionalPanel(
            ns=ns,
            condition = "input.legend_pos == 'inside'",

            numericInput(
              ns("legend_x"),
              "Legend X Coord",
              value = 0.9,
              max = 1,
              min = 0
            ),

            numericInput(
              ns("legend_y"),
              "Legend Y Coord",
              value = 0.8,
              max = 1,
              min = 0
            )
          )
          ) # end box 3
      ),
      # end fr1

      fluidRow(
        ## Confidence Settings ----
        box(
          width = 4,
          solidHeader = T,
          collapsible = T,
          collapsed = T,
          status = "primary",
          title = strong("Confidence"),

          checkboxInput(ns("conf_int"),strong("Showing Confidence Interval"),TRUE),

          conditionalPanel(
            ns=ns,
            condition = "input.conf_int",

            radioButtons(
              ns("conf_int_style"),
              strong("Confidence Interval Style"),
              selected = "ribbon",
              choices = c("ribbon", "step"),
              inline = T
            ),
            conditionalPanel(
              ns=ns,
              condition = "input.conf_int_style == 'ribbon'",
              numericInput(
                ns("conf_int_alpha"),
                "Transparency",
                value = 0.3,
                min = 0,
                max = 1,
                step = 0.1
              )
            )
          )
        ), # # end box 1

        ## p-Value Settings ----
        box(
          width = 4,
          solidHeader = T,
          collapsible = T,
          collapsed = T,
          status = "primary",
          title = strong("p-Value"),

          checkboxInput(ns("pval"), strong("Showing p-Value"), TRUE),
          checkboxInput(ns("pval_method"), strong("Showing p-Value Calculation Method"), FALSE),

          conditionalPanel(
            ns=ns,
            condition = "input.pval",
            numericInput(ns("pval_size"),"Text Size",value = 5,max=10,min=0,step = 0.1),
            numericInput(ns("pval_coord_y"),"Text Y Coord", value = 10, max = 100, min = 0,step = 1),
            numericInput(ns("pval_coord_x"),"p_Value Text X Coord", value = 0)
          ),

          conditionalPanel(
            ns=ns,
            condition = "input.pval_method",
            numericInput(ns("pval_method_coord_x"),"p_Value Calculation Method Text X Coord", value = 0)
          )

        ), # end box 2

        ## Tables ----
        box(
          width = 4,
          solidHeader = T,
          collapsible = T,
          collapsed = T,
          status = "primary",
          title = strong("Tables"),

          checkboxInput(ns("risk_table"),"Showing risk table",TRUE),
          checkboxInput(ns("ncensor_plot"),"Showing ncensor plot",TRUE),
          checkboxInput(ns("cumevents"),"Showing cumevents table",FALSE),
          checkboxInput(ns("cumcensor"),"Showing cumcensor table",FALSE),

          conditionalPanel(
            ns=ns,
            condition = "input.risk_table | input.ncensor_plot | input.cumevents  |  input.cumcensor",
            numericInput(
            ns("tables_height"),
            "Tables Height",
            value = 0.25,
            max = 1,
            min = 0,
            step = 0.01
          ),
          checkboxInput(ns("tables_y_text"), strong("Showing Tables Y-Axis Label"), FALSE),

          numericInput(ns("tables_fontsize"),"Tables Text Size",
                       value = 5,max=10,min=0,step = 0.1)
          ),

          conditionalPanel(
            ns=ns,
            condition = "input.risk_table",

            # numericInput(ns("risk_table_fontsize"),"Tables Text Size",
            #              value = 5,max=10,min=0,step = 0.1),

            radioButtons(ns("risk_table_pos"),
                       "Risk Table Position",
                       choices = c("Outside the main plot" ="out",
                                   'Inside the main plot' ="in")
            )
          )
        )
        # end box 3
      ),
      # end fr2

      fluidRow(
        ## Axes ------
        box(
          width = 3,
          solidHeader = T,
          collapsible = T,
          collapsed = T,
          status = "primary",
          title = strong("Axes"),

          checkboxInput(ns("axes_offset"),strong("Axes Offset"),TRUE),

          numericInput(ns("axes_fontsize"),"Axes label size",
                       value = 6,max=10,min=0,step = 0.1),

          selectInput(ns("axes_fontface"),"Axes label style",
                      selected = "bold", choices = c("Bold" = "bold","Plain" = "plain",

                                                     "Italic" = "italic",
                                                     "Bold Italic" = "bold.italic")),

          colourpicker::colourInput(ns("axes_fontcolor"), "Axes label color", value = "#030303"),



          numericInput(ns("tickslab_fontsize"),"Tickslab Size",
                       value = 6,max=10,min=0,step = 0.1),
          selectInput(ns("tickslab_fontface"),"Tickslab Style",
                      selected = "plain", choices = c("Plain" = "plain",
                                                     "Bold" = "bold",
                                                     "Italic" = "italic",
                                                     "Bold Italic" = "bold.italic")),

          colourpicker::colourInput(ns("tickslab_fontcolor"), "Tickslab Color", value = "#030303")
        ), # end box 1

        ## X-Axis Setting ----
        box(
          width = 3,
          solidHeader = T,
          collapsible = T,
          collapsed = T,
          status = "primary",
          title = strong("X-Axis"),

          numericInput(ns("xlim_lower"),"X-Axis lower limit ",value = 0 ),
          numericInput(ns("xlim_upper"),"X-Axis upper limit ",value = NULL ),
          # numericInput(ns("break_x_by"),"X_Axes Scale Spacing",value = NULL ),

          textInput(ns("xlab"), "X-Axis Label", "Time"),
          selectInput(ns("xunit"), "X-Axis Unit",c("Day","Month","Year")),
          selectInput(ns("xconvert_direction"), "Time convert direction",
                      choices = c("None"= "none",
                                  "Days to Months" = "d2m",
                                 "Days to Years" = "d2y",
                                 "Months to Years" = "m2y",
                                 "Months to Days" = "m2d",
                                 "Years to Days" = "y2d",
                                 "Years to Months" = "y2m"
                                )
                      )
        ), # end box 2

        ## Y-Axis Setting ----
        box(
          width = 3,
          solidHeader = T,
          collapsible = T,
          collapsed = T,
          status = "primary",
          title = strong("Y-Axis"),

          numericInput(ns("ylim_lower"),"Y-Axis lower limit",value = 0),
          numericInput(ns("ylim_upper"),"Y-Axis upper limit",value = 100),
          textInput(ns("ylab"), "Y-Axis Label", "Survival Probability")

          # numericInput(ns("break_y_by"),"Y_Axes Scale Spacing",value = 0.2 ),

        )
        # , # # end box 3

#         ## More Settings ----
#         box(
#           width = 3,
#           solidHeader = T,
#           collapsible = T,
#           collapsed = T,
#           status = "primary",
#           title = strong("More"),
# #
# #           textInput("main", "Main Title", "Kaplan-Meier Survival Curve"),
# #
# #           selectInput(ns("ggtheme"), "Tables theme", choices =  c( "survminer" = "theme_survminer()",
# #                                                                    "bw" = "theme_bw()",
# #                                                                         "cleantable"="theme_cleantable()")),
# #
# #
# #           selectInput(ns("tables_theme"), "Tables theme", choices =  c( "survminer" = "theme_survminer()",
# #                                                                         "cleantable"="theme_cleantable()"))
#
#         ) # end box 4
      ) # end fr3

    ),

    ### end sidebarPanel ----


    # mainPanel ----
    mainPanel(width = 6,
      # tabsetPanel(
      # id = ns("km_tabs"),
      # tabPanel(
      #   "Data Preview",
      #   fluidRow(
      #     DTOutput(ns("km_plot_data"))
      #   )
      # ),
      # tabPanel(
      #   "KM Plotter",
        fluidRow(
          plotOutput(ns("km_plot_ui"),height = "800px"),
          box(
            width = 6,
            solidHeader = T,
            status = "primary",
            title = strong("Graphics Settings"),
            selectInput(ns("unit"),"Dimension Unit",selected = "cm",
                        choices = c("cm","mm","inch"="in","pixel"="px")
                        ),
            numericInput(ns("width"),"Width",value = 7),
            numericInput(ns("height"),"Height",value = 10)
          ),
          box(
            width = 6,
            solidHeader = T,
            status = "primary",
            title = strong("Saving settings"),
            numericInput(ns("dpi"),"Image Resolution",value = 300,max = 2400,min=72),
            selectInput(ns("device"),"Image Format",
                        choices = c("png", "bmp", "jpeg", "tiff", "pdf", "svg","eps", "ps", "tex", "wmf")),
            downloadButton(
              ns("downBtn"),
              "Save Image"
            )
          )
        )
      ### end fluidRow ----
    #   )
    # )
    #### end tabsetPanel ----
    )
    ### end mainPanel ----

    )
    ## end sidelayout ----
  )
  # end tagList ----
}

#' km_plot Server Functions
#'
#' @noRd
mod_km_plot_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update Data and Variables ----
    observe({
      req(df())
      data <- df() %>% as.data.frame
      vars <- colnames(data)

      updateSelectInput(session, "time", choices = vars[sapply(data, is.numeric)])
      updateSelectInput(session, "status", choices = vars)
      updatePickerInput(session, "strata", choices = vars)
      updatePickerInput(session, "group", choices = vars)
      updatePickerInput(session, "facet", choices = vars)

    })

    km_plot_obj <- reactiveVal()

    # KM Plotter ----
    output$km_plot_ui <- renderPlot({
      req(df(), input$time, input$status)
      data <- df()

      attach(data)

      data[, input$time] <- as.numeric(data[, input$time])
      data[, input$status] <- as.numeric(data[, input$status])

      if (input$xconvert_direction == "none") {
        x_convert = 1
      } else if (input$xconvert_direction == "d2y") {
        x_convert = 1/365
        updateSelectInput(session,"xunit",selected = "Year")
      } else if (input$xconvert_direction == "m2y") {
        x_convert = 1/12
        updateSelectInput(session,"xunit",selected = "Year")
      } else if (input$xconvert_direction == "y2d") {
        x_convert = 365
        updateSelectInput(session,"xunit",selected = "Day")
      } else if (input$xconvert_direction == "y2m") {
        x_convert = 12
        updateSelectInput(session,"xunit",selected = "Month")
      } else if (input$xconvert_direction == "d2m") {
        x_convert = 1/30
        updateSelectInput(session,"xunit",selected = "Month")
      } else {
        x_convert = 30
        updateSelectInput(session,"xunit",selected = "Day")
      }

      data[, input$time] <- data[, input$time] * x_convert

      updateNumericInput(session, "xlim_upper",value = max(data[,input$time]))
      # updateNumericInput(session, "break_x_by",value = max(data[,input$time])/5)

      ## Single analysis ----
      if (input$run_task == "single") {
        ### faceting ----
        if (input$run_facet == "Yes") {

        }
        ### grouping ----
        else if (input$run_group == "Yes") {

        }
        ### Plain ----
        else if (input$run_facet == "No" && input$run_group == "No") {
          #### formula ----
          req(input$strata)

          formula <- as.formula(paste0(
            "Surv(",
            input$time,
            ",",
            input$status,
            ")~",
            paste0(input$strata, collapse = "+")
          ))

          #### fit ----
          fit <- surv_fit(formula, data = data)

          #### legend position -----
          if (input$legend_pos == "none") {
            legend = "none"
          } else if (input$legend_pos == "outside") {
            legend = input$legend
          } else if (input$legend_pos == "inside") {
            legend = c(input$legend_x,input$legend_y)
          }

          #### km plot ----
          p <- ggsurvplot(
            fit = fit,
            data = data,

            ##### curve ----
            fun = input$fun,
            palette = input$palette,
            linesize = input$linesize,
            surv.median.line = input$surv_median_line,
            add.all = input$add_all,

            ##### confidence interval ----
            conf.int = input$conf_int,
            conf.int.style = input$conf_int_style,
            conf.int.alpha = input$conf_int_alpha,

            ##### censor marker ----
            censor = input$censor,
            censor.shape = as.numeric(input$censor_shape),
            censor.size = input$censor_size,

            ##### p-value ----
            pval = input$pval,
            pval.method = input$pval_method,
            pval.size = as.numeric(input$pval_size),
            pval.coord = c(
              as.numeric(input$pval_coord_x),
              as.numeric(input$pval_coord_y)
            ),
            pval.method.coord= c(
              as.numeric(input$pval_method_coord_x),
              as.numeric(input$pval_coord_y)
            ),


            ##### Legend ----
            legend = legend,
            legend.title = input$legend_title,
            font.legend = c(input$legend_fontsize,
                            input$legend_fontface,
                            input$legend_fontcolor),

            ##### Axes ----
            axes.offset = input$axes_offset,
            font.tickslab = c(input$tickslab_fontsize,
                              input$tickslab_fontface,
                              input$tickslab_fontcolor),

            ##### X-Axis ----
            # break.x.by  = input$break_x_by,
            xlim = c(input$xlim_lower,input$xlim_upper),
            font.x = c(input$axes_fontsize,input$axes_fontface,input$axes_fontcolor),
            xlab = paste0(input$xlab,"(",input$xunit,")"),
            # xlim = c(0,500),


            ##### Y-Axis ----
            # break.y.by  = input$break_y_by,
            ylim = c(input$ylim_lower,input$ylim_upper),
            font.y = c(input$axes_fontsize,input$axes_fontface,input$axes_fontcolor),
            ylab = input$ylab,

            ##### Tables ----
            tables.height = input$tables_height,
            tables.y.text = input$tables_y_text,
            fontsize = input$tables_fontsize,

            ##### Risk Table ----
            risk.table = input$risk_table,
            # risk.table.fontsize = input$risk_table_fontsize,
            risk.table.pos = input$risk_table_pos,

            ##### ncensor -----
            ncensor.plot = input$ncensor_plot,

            ##### cumcensor ----
            cumcensor = input$cumcensor,

            ##### cumevents ----
            cumevents = input$cumevents
            # ,

            ##### Theme ------
            # ggtheme=input$ggtheme,
            # tables.theme = input$tables_theme

          )

          splots <- list()
          splots[[1]] <- p

          km_plot_obj(arrange_ggsurvplots(splots, ncol = 1, nrow = 1))
          p

        }

      }
      ## Batch analysis -----
      else {

      }

    })

    output$downBtn <- downloadHandler(
      filename = function() {
        paste("KM_Plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".",input$device, sep="")
      },
      content = function(file) {
        plot_obj <- km_plot_obj()

        ggsave(
          file,
          plot = plot_obj,
          device = input$device,
          scale = 1,
          width = input$width,
          height = input$height,
          units = input$unit,
          dpi = input$dpi,
          limitsize = TRUE
        )
      }
    )

  })
}

## To be copied in the UI
# mod_km_plot_ui("km_plot_1")

## To be copied in the server
# mod_km_plot_server("km_plot_1")
