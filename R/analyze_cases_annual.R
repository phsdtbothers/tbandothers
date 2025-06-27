#' Generates year by year epicurve
#'
#' Creates epicurve with bars for current year cases, and line for previous year cases. Automatically adjusts displayed years based on current morbidity week, keeps cases in the middle, and shows previous years if needed.
#'
#' @param disease_latest latest table- requires proxy_onset_date, adj_mw_...
#' @param to_path file path for .png
#'
#' @export
analyze_cases_annual <- function(disease_latest, to_path = 'analyze_cases_annual - Output.png') {
  mw <- tbandothers::download_morbidity_weeks()

  # add year number
  disease_latest <- disease_latest %>%
    dplyr::mutate(
      case_year = lubridate::year(proxy_onset_date)
    )

  # get year data
  year_current <- disease_latest$case_year %>% max()
  year_last <- year_current - 1

  # get morbidity week data
  adj_mw_col <- dplyr::sym(paste0('adj_mw_', year_current))

  mw_current <- disease_latest %>% dplyr::filter(case_year == year_current) %>% dplyr::select(!!adj_mw_col) %>% max()
  mw_max <- mw %>% dplyr::filter(year == year_current) %>% dplyr::select(week) %>% max()
  mw_max_last <- mw %>% dplyr::filter(year == year_last) %>% dplyr::select(week) %>% max()

  # analyze trends
  disease_trend_curr <- disease_latest %>%
    dplyr::group_by(!!adj_mw_col) %>%
    dplyr::summarise(
      cases_curr = sum(case_year == year_current),
      cases_last = sum(case_year == year_last)
    ) %>%
    dplyr::mutate(year = year_current)

  disease_trend_last <- disease_latest %>%
    dplyr::group_by(!!adj_mw_col) %>%
    dplyr::summarise(
      cases_curr = sum(case_year == year_last),
    ) %>%
    dplyr::mutate(
      cases_last = NA,
      year = year_last
    )

  disease_trend <- dplyr::bind_rows(disease_trend_curr, disease_trend_last) %>%
    dplyr::arrange(dplyr::desc(year), dplyr::desc(!!adj_mw_col)) %>%
    # ensures proper visualization on year transition
    # - around half of later morbidity weeks shown in graph
    # - if before MW 9, does not go under 9 shown MWs and also shows some previous MWs from prev year
    dplyr::filter(
      (
        year == year_current
        & !!adj_mw_col <= min(mw_max, mw_current * 1.75) %>% max(mw_current + 9)
      )
      | (
        year == year_last
        & !!adj_mw_col > mw_max_last + (mw_current - 8)
      )
    )

  cases_max <- max(disease_trend$cases_curr, disease_trend$cases_last)

  # ggplot visualization
  facet_labels <- c(paste0('MW 1 - MW ', mw_current), 2024)
  facet_colors <- c('#A5A5A5', '#A5A5A5')

  facet_label_colors <- stats::setNames(facet_colors, facet_labels)

  y_axis_height <- ceiling(cases_max / 100) * 100
  y_axis_break <- y_axis_height / 10

  disease_bar <- ggplot2::ggplot(data = disease_trend) +
    # current cases, morbidity weeks, year/s (bar)
    ggplot2::geom_col(
      ggplot2::aes(
        x = !!adj_mw_col,
        y = cases_curr,
        color = paste0('MW 1 - MW ', mw_current)
      ),
      fill = '#203864'
    ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(year),
      scales='free_x',
      space='free_x',
      switch='x'
    ) +
    # last year cases (line)
    ggplot2::geom_line(
      ggplot2::aes(
        x = !!adj_mw_col,
        y = cases_last,
        color = factor(year_last)
      ),
      linewidth = 1.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = !!adj_mw_col,
        y = cases_last,
        color = factor(year_last)
      ),
      size = 2
    ) +
    # visuals
    ggplot2::theme_classic() +
    ggplot2::theme(
      # background colors (transparent)
      panel.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      # axis colors
      axis.line = ggplot2::element_line(color = '#A5A5A5'),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = '#595959', size = 16),
      axis.title = ggplot2::element_text(color = '#595959', size = 16),
      # year/facet cards
      strip.placement = 'outside',
      strip.background.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(color = '#595959', size = 15),
      # legend
      legend.title = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.9, 'cm'),
      legend.text = ggplot2::element_text(color = '#595959', size = 15),
      legend.position = c(0.85,0.95),
      legend.direction = 'horizontal',
      legend.background = ggplot2::element_rect(fill = 'transparent', color = NA)
    ) +
    ggplot2::scale_color_manual(
      name = NA,
      values = facet_label_colors
    ) +
    ggplot2::scale_x_continuous('Morbidity Week', labels = as.character(1:mw_max), breaks = 1:mw_max, expand = c(0,0)) +
    ggplot2::scale_y_continuous('No. of Cases', expand = c(0,0), breaks = seq(0, y_axis_height, by=y_axis_break), limits = c(0, y_axis_height))

  # save file
  ggplot2::ggsave(
    filename = here::here(to_path),
    plot = disease_bar,
    width = 21,
    height = 9,
    dpi = 300,
    units = "in"
  )
}
