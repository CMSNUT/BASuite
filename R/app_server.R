#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  output$github <- renderUser({
    div(style="margin-top:5px;border:1px solid #3C8DBC;",
        HTML(
          '<a title="github" href="https://github.com/CMSNUT/BASuite" target="_blank" class="btn btn-social-icon">
        <i style="background-color:#3C8DBC; color:white;" class="fab fa-github"></i>
        </a>'
        )
    )
  })
}
