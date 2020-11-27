#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  start_date = reactive({
    as.Date(Sys.time()) - input$dead_shift
  })

  dataInput <- reactive({
    # Load in latest data
    data_use = hamiltonSeirOver65::latest_covid %>% 
      dplyr::filter(Date >= start_date())
  })

  observeEvent(input$button, {
    shinyjs::toggle("exp")
    shinyjs::toggle("exp2")
    shinyjs::toggle("inf")
    shinyjs::toggle("inf2")
    shinyjs::toggle("rec")
    shinyjs::toggle("rec2")
    shinyjs::toggle("dead_shift")
    shinyjs::toggle("pop_under_65")
    shinyjs::toggle("pop_over_65")
    shinyjs::toggle("num_sim")
  }, ignoreNULL = FALSE)


  #realisation <- reactive({
  output$plot <- plotly::renderPlotly({
    ##### General setup
    # Inputs are YS, YE, YI, YR, OS, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O

    # Number of simulations
    num_sim = input$num_sim
    store = vector('list', num_sim)
    for (i in 1:num_sim) {
      store[[i]] = twoages(YS = input$pop_under_65, # Under 65s susceptible
                           YE = input$exp,
                           YI = input$inf,
                           YR = input$rec,
                           OS = input$pop_over_65,
                           OE = input$exp2,
                           OI = input$inf2,
                           OR = input$rec2,
                           YR0Y = input$R0,
                           YR0O = input$R0_O_Y,
                           OR0Y = input$R0_O_Y,
                           OR0O = input$R0_1) %>%
        as.data.frame %>%
        dplyr::rename("Time" = 1, "YS" = 2,"YE" = 3,
               "YI" = 4, "YR" = 5, "OS" = 6,
               "OE" = 7, "OI" = 8, "OR" = 9)
    }

    # Quick plot
    # plot(store[[1]]$Time, store[[1]]$YI, type = 'l')
    # lines(store[[1]]$Time, store[[1]]$OI, col = 'red')
    # Should be plotting difference in removed category
    # plot(store[[1]]$Time, diff2(store[[1]]$YR), type = 'l')
    # lines(store[[1]]$Time, diff2(store[[1]]$OR), col = 'red')

    # Extract out the infections and quantiles for each group
    # Add 0s to each vector to make them the same length
    nrows = lapply(store, 'nrow') %>% unlist
    max_row = max(nrows)
    time_max = store[[which.max(nrows)]]$Time

    YR_padded <- purrr::map_dfc(store,  ~.x %>% dplyr::select(YR) %>% dplyr::mutate(YR = diff3(YR)) %>%  dplyr::bind_rows(data.frame(YR = rep(0, max_row - nrow(.x)))))


    # Now calculate medians and 90% CI
    YR_median = (apply(YR_padded, 1, 'quantile', 0.5))
    YR_high = (apply(YR_padded, 1, 'quantile', 0.95))
    YR_low = (apply(YR_padded, 1, 'quantile', 0.05))

    
    # Final data frame for YR
    dead_shift = input$dead_shift # Gap between cases and deaths
    dates = as.Date(Sys.time())+time_max - dead_shift # Start from 3 weeks ago
    YR_final = tibble::tibble(Date = dates,
                      `Under 65sXXXInfected - Value` = YR_median,
                      `Under 65sXXXInfected - low est` = YR_low,
                      `Under 65sXXXInfected - high est` = YR_high,
                      `Under 65sXXXDead - Value` = c(rep(0, dead_shift), utils::head(YR_median, -dead_shift)*input$dead_0/100),
                      `Under 65sXXXDead - low est` = c(rep(0, dead_shift), utils::head(YR_low, -dead_shift)*input$dead_0/100),
                      `Under 65sXXXDead - high est` = c(rep(0, dead_shift), utils::head(YR_high, -dead_shift)*input$dead_0/100))

    # Now do the same thing for old infected
    OR_padded <- purrr::map_dfc(
      store,
      ~.x %>% 
        dplyr::select(OR) %>% 
        dplyr::mutate(OR = diff3(OR)) %>% 
        dplyr::bind_rows(data.frame(OR = rep(0, max_row - nrow(.x))))
    )

    # Now calculate medians and 90% CI
    OR_median = (apply(OR_padded, 1, 'quantile', 0.5))
    OR_high = (apply(OR_padded, 1, 'quantile', 0.95))
    OR_low = (apply(OR_padded, 1, 'quantile', 0.05))

    # Final data frame for OR
    OR_final = tibble::tibble(Date = dates,
                      `Over 65sXXXInfected - Value` = OR_median,
                      `Over 65sXXXInfected - low est` = OR_low,
                      `Over 65sXXXInfected - high est` = OR_high,
                      `Over 65sXXXDead - Value` = c(rep(0, dead_shift), utils::head(OR_median, -dead_shift)*input$dead_1/100),
                      `Over 65sXXXDead - low est` = c(rep(0, dead_shift), utils::head(OR_low, -dead_shift)*input$dead_1/100),
                      `Over 65sXXXDead - high est` = c(rep(0, dead_shift), utils::head(OR_high, -dead_shift)*input$dead_1/100),
                      `TotalXXXDead - Value` = c(rep(0, dead_shift), utils::head(OR_median, -dead_shift)*input$dead_1/100 +
                                                   utils::head(YR_median, -dead_shift)*input$dead_0/100))

    # Tidy up into one data frame
    final = dplyr::left_join(YR_final, OR_final, by = "Date") %>% 
      tidyr::pivot_longer(names_to = 'Type', values_to = 'Count', -Date)
    final_twocols = as.matrix(stringr::str_split(final$Type, 'XXX', simplify = TRUE))    
    final$`Age group` = final_twocols[,1]
    final$Type = final_twocols[,2]
    final2 = final %>% 
      tidyr::pivot_wider(names_from = "Type", values_from = "Count") %>% 
      dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, 0)) %>% 
      tidyr::pivot_longer(names_to = "Type", values_to = "Count", -c(Date, `Age group`))
    final2_twocols = as.matrix(stringr::str_split(final2$Type, ' - ', simplify = TRUE))    
    final2$Type = final2_twocols[,1]
    final2$Est = final2_twocols[,2]
    final2 = final2 %>% 
      tidyr::pivot_wider(names_from = "Est", values_from = "Count")
    final2$Type = factor(final2$Type, levels = c('Infected', 'Dead'), ordered = TRUE)
    
    # This caused a load of pain but replaced three of the above lines
    #   tidyr::separate(Type, c("Age group", "Type"), sep = "XXX") %>%

    plt1 = ggplot2::ggplot(final2, ggplot2::aes(x = Date, colour = `Age group`)) +
      ggplot2::geom_line(ggplot2::aes(y = `Value`)) +
      #geom_ribbon(aes(ymin = `low est`, ymax = `high est`, fill = `Age group`), alpha = 0.1) +
      ggplot2::labs(x = "Date", title = "Infected/dead per day", y = NULL) +
      ggplot2::scale_x_date( date_labels = "%d-%b") +
      ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = hamiltonThemes:::distill_status_to_colour("primary")),
        strip.text = ggplot2::element_text(colour = "white")
      ) + 
      ggplot2::facet_wrap(~ Type , nrow = 2, scales = 'free_y')
    # theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0))
    if(input$log_scale) plt1 = plt1 + ggplot2::scale_y_log10(expand = c(0, 0), labels = scales::comma)

    if(input$show_data) {
      df_use = tibble::tibble(
        Date = dataInput()$Date,
        `Age group` = dataInput()$`Age group`,
        Value = dataInput()$Value
      ) %>%
        dplyr::mutate(Type = dplyr::case_when(
          `Age group` == "Total" ~ "Dead",
          TRUE ~ "Infected"
        ))
      plt1 = plt1 + ggplot2::geom_point(data = df_use, ggplot2::aes(y = Value))
    }
    plotly::ggplotly(plt1)
    
  })
}



diff3 = function(x) return(c(0, diff(x)))
