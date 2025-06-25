#' Generate regional analysis facet graph.
#'
#' Generates facet graph for regional case breakdown. Uses current year, and previous 2 years. Must contain clean region, adj_mw_..., proxy_onset_date columns. Exports generated to .png file based on to_path argument.
#'
#' @param linelist_latest dataframe of disease table (typically uses latest table)
#' @param to_path string, path of generated .png file
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#' @import here
#' @import stats
#'
#' @export
analyze_cases_regions <- function(linelist_latest, to_path) {
  # download translation gsheet, make function for this too
  regions <- tbandothers::download_translations('region')

  # clean data for facet generation
  linelist_latest <- linelist_latest %>%
    dplyr::mutate(
      region = dplyr::recode(region, !!!regions),
      year = lubridate::year(proxy_onset_date)
    ) %>%
    dplyr::filter(!is.na(region))

  # get current year/week
  this_year <- max(linelist_latest$year)
  year_before_1 <- this_year - 1
  year_before_2 <- year_before_1 - 1

  last_years <- c(this_year, year_before_1, year_before_2)

  this_year_label <- paste0(as.character(this_year), ' Cases')
  year_before_1_label <- as.character(year_before_1)
  year_before_2_label <- as.character(year_before_2)

  # filter data based on target years and latest morbidity week
  this_week <- linelist_latest %>% dplyr::filter(year == this_year) %>% dplyr::select(paste0('adj_mw_', this_year)) %>% max()

  disease_this_week <- linelist_latest %>%
    dplyr::filter(
      year %in% last_years &
        !!dplyr::sym(paste0('adj_mw_', this_year)) <= this_week
    )

  # sort cases by regions
  disease_regions <- disease_this_week %>%
    dplyr::group_by(year, !!dplyr::sym(paste0('adj_mw_', this_year)), region) %>%
    dplyr::summarise(
      cases = dplyr::n(), .groups='drop'
    ) %>%
    dplyr::mutate(
      year = as.factor(year)
    )

  # dynamically set facet colors to years
  facet_labels <- c(this_year_label, year_before_1_label, year_before_2_label)
  facet_colors <- c("#073763", "#762023", "#cdc50a")

  facet_label_colors <- stats::setNames(facet_colors, facet_labels)

  # generate facet
  disease_facet <- ggplot2::ggplot() +
    # Add bars for current years so they appear first
    ggplot2::geom_col(data = dplyr::filter(disease_regions, year == this_year),
             ggplot2::aes(x = !!dplyr::sym(paste0('adj_mw_', this_year)), y = cases, fill = paste0(this_year, " Cases")),
             width = 0.80) +
    # Add lines for past two years first so they appear on top
    ggplot2::geom_line(data = dplyr::filter(disease_regions, year == year_before_1),
              ggplot2::aes(x = !!dplyr::sym(paste0('adj_mw_', this_year)), y = cases, group = year, color = factor(year_before_1)),
              linewidth = 1) +
    ggplot2::geom_line(data = dplyr::filter(disease_regions, year == year_before_2),
              ggplot2::aes(x = !!dplyr::sym(paste0('adj_mw_', this_year)), y = cases, group = year, color = factor(year_before_2)),
              linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Morbidity Week",
      y = "No. of Cases",
      fill = "Year",
      color = "Year"
    ) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_y_continuous(labels = scales::comma,
                       breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::scale_fill_manual(values = facet_label_colors) +
    ggplot2::scale_color_manual(values = facet_label_colors) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(color = "black", size = 10, face = "bold"),
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(color = "black", linewidth = 1),
      strip.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1, "lines")
    ) +
    ggplot2::facet_wrap(~ region, scales = "fixed", labeller = ggplot2::labeller(region = ggplot2::label_value),
               strip.position = "bottom")

  ggplot2::ggsave(
    filename = here::here(to_path),
    plot = disease_facet,
    width = 10,
    height = 8,
    dpi = 300,
    units = "in"
  )
}
