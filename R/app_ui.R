#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import compareGroups
#' @import DT
#' @import googlesheets4
#' @import haven
#' @import shiny
#' @import shinyBS
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyhelper
#' @import shinyjs
#' @import shinyWidgets
#' @import survival
#' @import survminer
#' @import autoReg
#' @import moonBook
#' @import rrtable

#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      useShinyjs(),
      HTML(
        "<style type='text/css'> #ResponseVariableORPanel, #LoadDataOptionsExcel, #LoadDataOptionsTxt, #extralabelsPanel {color:white;background-color:rgba(60,141,188,1)}</style>"
      ),
      HTML(
        "<style type='text/css'> #ratioAccordion, #formatAccordion, #decimalsAccordion {color:rgba(60,141,188,1)}</style>"
      ),

      # get window sizes.
      tags$head(tags$script('var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),

      HTML("<link href='https://fonts.googleapis.com/css?family=Knewave' rel='stylesheet'>"),

      titlePanel(HTML("<p style='margin-top:-20px'></p>"), windowTitle="compareGroups | Explore and Summarise Epidemiological Data in R"),
      dashboardPage(
        preloader=NULL,
        skin="blue",

        ## header ----
        header = dashboardHeader(
          # title = logo_cg,
          title=tagList(
            span(
              class = "logo-lg",
              HTML(
                "<img src='www/logo.png' width=35 style='margin-right:10px;margin-bottom:10px'/><format style='text-align: left; font-family: Knewave; font-size: 30px; font-style: normal; font-variant: normal;'>BASuite<br>xxx</format>"
              )
            ),
            img(src = "www/logo.png", width = 35)
          ),
          # titleWidth = 300,
          fixed = FALSE,
          #enable_rightsidebar = TRUE,
          controlbarIcon = shiny::icon("gears"),
          # leftUi=tagList(
          #   # hidden(
          #     div(
          #       id="dropdownData",
          #         tags$table(
          #          tags$tr(
          #            tags$td(style="padding-right:5px",
          #                    dropdownButton(inputId="valuextoptionsaction", circle=FALSE, status="primary", label=HTML("<format style='font-size:13pt'>View options</format>"),
          #                                   checkboxInput("showlabels","Show variable labels",TRUE),
          #                                   sliderInput("valueextsize", "Resize (%):", min=10, max=300, value=100, step=10),
          #                                   sliderInput("valueextwidth", "Width (%)", value=100, min=50, max=200)
          #                    )
          #            ),
          #            tags$td(style="padding-right:5px",
          #                    bsButton("udpateSelection",HTML("<format style='font-size:13pt'>Update Selection</format>"),style="primary")
          #            )
          #          )
          #         )
          #     )
          #   # ),
          # ),
          userOutput("github")
        ),


        ## sidebar ----
        sidebar = dashboardSidebar(
          sidebarMenu(
            id="leftmenu",
            ### Home ----
            menuItem(
              HTML("<format style='font-size:13pt'>Home</format>"),
              tabName="Home",
              icon=icon("home")
            ),

            ### Data ----
            div(
              id="DataHeader",
              style="padding-top:5px;",
              uiOutput("DataHeaderText"),
              div(style="border: 1px solid white;height:0px; margin-top:-3px; margin-bottom:0px")
            )
            #,
          )
        ),


        ## body ----
        body = dashboardBody(
          setShadow(class = "dropdown-menu"),
          # cg_theme,
          tags$head(
            tags$style(
              HTML('.content-wrapper, .right-side {background-color: white;}')
            )
          ),

          ### Home Panel ####
          div(id="homePanel",
              HTML("<p style='text-align: center;><format color:#357CA5; font-family: Knewave; font-size: 40pt; font-style: normal; font-variant: normal;'>BASuite</format></p>"),
              HTML("<h3 style='text-align: center;><format style='text-align: center'><i><strong><code>B</code>ioinformatics <code>A</code>nalysis Tools <code>Suite</code> with R Language</strong></format></i></h3>"),
              includeMarkdown("/www/home.md")
          )
        )

      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BASuite"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
