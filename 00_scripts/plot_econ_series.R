plot_econ_series <-
function(data_clean, var = "PRFIC1") {

    g <- data_clean %>%

        # Calculate percent change
        filter(symbol %in% c("GDPC1",
                             var)) %>%

        mutate(symbol = fct_recode(symbol,
                                   "GDP" = "GDPC1",
                                   "Industry" = var)) %>%

        mutate(symbol = factor(symbol, levels = c("GDP", "Industry"))) %>%

        # Plot
        ggplot(aes(date, change)) +
        geom_line(aes(color = symbol)) +
        geom_point(aes(text = text), size = 0.1, alpha = 0) +
        theme_tq() +
        theme(legend.position = "bottom",
              strip.text = element_text(size = 8),
              panel.grid.major.y = element_blank()) +

        geom_hline(yintercept = 0, color = "#18BC9C", size = 0.5, alpha = 0.7) +

        # Shade for recession
        geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
                   aes(xintercept = as.numeric(date)), color = "#4876FF", alpha = 0.3) +

        labs(subtitle = "US Recessions in Blue Shade",
             y = "Year-Over-Year Percent Change",
             x = NULL,
             color = NULL)

    # Make the plot interactive
    plotly::ggplotly(g, tooltip = "text")

}
