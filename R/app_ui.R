#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    bs4Dash::bs4DashPage(
      sidebar_collapsed = TRUE,
      sidebar_mini = FALSE,
      body = bs4Dash::bs4DashBody(
        hamiltonThemes::use_bs4Dash_distill_theme(),
        fluidRow(
          bs4Dash::column(
            width = 4,
            br(),
            sliderInput("R0", "Average number of infections from each infected person (R0) for under 65s", 0, 10, 1.5, step=0.1),
            
            sliderInput("R0_1", "Average number of infections from each infected person (R0) for over 65s", 0, 10, 0.8, step=0.1),
            
            sliderInput(inputId = "R0_O_Y",
                        label = "Average number of infections passed between under and over 65s per infected person (Cross R0)",
                        0, 10, 0.3, step=0.1),
            
            sliderInput("dead_0", "Case fatality rate (%) for under 65s", 0, 20, 0.5, step = 0.1),
            
            sliderInput("dead_1", "Case fatality rate (%) for over 65s", 0, 20, 10, step = 0.1),
            shinyjs::useShinyjs(),
            actionButton(inputId = "button", label = "show extra options"),
            
            numericInput(inputId = "exp",
                         label = "Number of asymptomatic spreaders under 65 at start date",
                         value = 3000),
            
            numericInput(inputId = "inf",
                         label = "Number of symptomatic spreaders under 65 at start date",
                         value = 3000),
            
            numericInput(inputId = "exp2",
                         label = "Number of asymptomatic spreaders over 65 at start date",
                         value = 400),
            
            numericInput(inputId = "inf2",
                         label = "Number of symptomatic spreaders over 65 at start date",
                         value = 400),
            
            numericInput(inputId = "rec",
                         label = "Number of recovered (i.e. immune) people under 65 at start date",
                         value = 200000),
            
            numericInput(inputId = "rec2",
                         label = "Number of recovered (i.e. immune) people over 65 at start date",
                         value = 100000),
            
            sliderInput("dead_shift", "Gap (days) between cases and deaths", 0, 50, 21, step = 1),
            
            numericInput(inputId = "pop_under_65",
                         label = "Population of Ireland under 65",
                         value = 4000000),
            
            numericInput(inputId = "pop_over_65",
                         label = "Population of Ireland over 65",
                         value = 900000),
            
            numericInput(inputId = "num_sim",
                         label = "Number of simulations to run (higher = slower but more accurate)",
                         value = 200)
          ),

          bs4Dash::bs4TabCard(
            width = 8,
            title = "COVID-19 Over 65s Cocooning effect",
            id = "tabcard",
            closable = FALSE,
            collapsible = FALSE,
            bs4Dash::bs4TabPanel(
              tabName = "Spread",
              plotly::plotlyOutput("plot", height = 500) %>% hamiltonThemes::distill_load_spinner(),
              checkboxInput("log_scale", "Log scale?", value = FALSE),
              checkboxInput("show_data", "Show data?", value = FALSE)
            ),
            bs4Dash::bs4TabPanel(
              tabName = "Assumptions",
              div(style = "color: black;", get_assumptions_text())
            )
          )
        )
      ),
      footer = hamiltonThemes:::bs4dash_distill_footer()
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  add_resource_path(
    "distill", system.file("distill", package = "hamiltonThemes")
  )
 
  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'hamiltonSeirOver65'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = system.file("distill", package = "hamiltonThemes"),
      app_title = 'hamiltonSeirOver65'
    )
  )
}

