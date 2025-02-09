#' logi_reg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_logi_reg_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' logi_reg Server Functions
#'
#' @noRd
mod_logi_reg_server <- function(id,df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_logi_reg_ui("logi_reg_1")

## To be copied in the server
# mod_logi_reg_server("logi_reg_1")
