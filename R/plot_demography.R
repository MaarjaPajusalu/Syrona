# ── Syrona Plot: Population Pyramid ───────────────────────────────────────────
#
# Back-to-back bar chart, one per dataset.
# Interactive (ggiraph): hover tooltip shows sex, birth year, count, % of cohort.

#' Build an interactive population pyramid
#'
#' @param demo_df Demographics data frame with birth_year, sex, patient_count.
#' @param dataset_name Name of the dataset (used as title).
#' @return A girafe object.
#' @export
build_pyramid <- function(demo_df, dataset_name) {
  if (nrow(demo_df) == 0) return(NULL)

  total <- sum(demo_df$patient_count)

  wide <- demo_df |>
    tidyr::pivot_wider(
      names_from = .data$sex,
      values_from = .data$patient_count,
      values_fill = 0L
    ) |>
    dplyr::rename(female = "F", male = "M") |>
    dplyr::mutate(
      f_pct = -.data$female / total * 100,
      m_pct =  .data$male   / total * 100
    )

  long <- dplyr::bind_rows(
    wide |> dplyr::mutate(
      sex_label = "Female",
      count = .data$female,
      pct = .data$female / total * 100,
      y_val = .data$f_pct
    ),
    wide |> dplyr::mutate(
      sex_label = "Male",
      count = .data$male,
      pct = .data$male / total * 100,
      y_val = .data$m_pct
    )
  ) |>
    dplyr::mutate(
      tooltip = paste0(
        .data$sex_label, " \u00b7 Born ", .data$birth_year, "\n",
        format(.data$count, big.mark = ","), " persons (",
        sprintf("%.2f", .data$pct), "%)"
      )
    )

  x_limit <- max(abs(c(wide$f_pct, wide$m_pct))) * 1.05
  min_year <- min(wide$birth_year)

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$birth_year, y = .data$y_val)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(fill = .data$sex_label, tooltip = .data$tooltip,
                   data_id = paste(.data$birth_year, .data$sex_label)),
      width = 0.9
    ) +
    ggplot2::scale_fill_manual(
      values = c("Female" = COLOR_SEX_F, "Male" = COLOR_SEX_M),
      guide = "none"
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "white", linewidth = 0.7) +
    ggplot2::annotate("text", x = min_year, y = -x_limit * 0.5,
                      label = "\u25C4  Female", color = COLOR_SEX_F,
                      fontface = "bold", size = 3.2, hjust = 0.5, vjust = -0.8) +
    ggplot2::annotate("text", x = min_year, y = x_limit * 0.5,
                      label = "Male  \u25BA", color = COLOR_SEX_M,
                      fontface = "bold", size = 3.2, hjust = 0.5, vjust = -0.8) +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_y_continuous(
      limits = c(-x_limit, x_limit),
      labels = function(x) paste0(abs(round(x, 2)), "%")
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = dataset_name,
      subtitle = paste0(format(total, big.mark = ","), " persons"),
      x = "Birth year",
      y = "% of all patients"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 13, face = "bold", color = COLOR_INK),
      plot.subtitle = ggplot2::element_text(size = 10, color = COLOR_INK_MUTED),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 7, color = COLOR_INK_SUBTLE),
      axis.text.x = ggplot2::element_text(size = 8, color = COLOR_INK_SUBTLE),
      axis.title.x = ggplot2::element_text(size = 9, color = COLOR_INK_MUTED,
                                            margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(size = 9, color = COLOR_INK_MUTED,
                                            margin = ggplot2::margin(r = 6)),
      plot.margin = ggplot2::margin(16, 16, 8, 8)
    )

  ggiraph::girafe(
    ggobj = p,
    width_svg = 6, height_svg = 7,
    options = list(
      ggiraph::opts_hover(css = "fill-opacity:1; stroke:black; stroke-width:0.5;"),
      ggiraph::opts_tooltip(css = "background:#333; color:#fff; padding:6px 10px; border-radius:4px; font-size:12px; line-height:1.4;",
                            delay_mouseout = 300)
    )
  )
}
