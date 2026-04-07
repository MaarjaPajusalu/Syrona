# ── Syrona Plot: Heatmap ──────────────────────────────────────────────────────
#
# Overview heatmap: meta_agegroups data faceted by sex x age group.
# Diverging blue-gray-orange scale, ggiraph interactive tiles.

#' Build an interactive heatmap of prevalence ratios
#'
#' @param data Data frame with meta_agegroups data (concept_id, concept_name,
#'   concept_code, sex, age_group, log2_pr, ci_low, ci_high, p_value).
#' @param name1 Reference dataset name.
#' @param name2 Comparison dataset name.
#' @param scale_mode "fold" or "log2" for axis labels.
#' @param concept_weights Data frame with concept_id, concept_name, pop_weight.
#' @param concept_order Vector of concept_ids in desired display order.
#' @return A girafe object.
#' @export
build_heatmap <- function(data, name1, name2, scale_mode = "log2",
                          concept_weights = NULL, concept_order = NULL) {
  if (nrow(data) == 0) return(NULL)

  age_group_levels <- c("0-9", "10-19", "20-29", "30-39", "40-49",
                        "50-59", "60-69", "70-79", "80+")

  data$concept_label <- truncate_name(data$concept_name)

  # Prepend weight-colored square to concept labels if weights provided
  if (!is.null(concept_weights) && "pop_weight" %in% names(concept_weights)) {
    weight_lookup <- concept_weights |>
      dplyr::mutate(
        label = truncate_name(.data$concept_name),
        color = weight_color(.data$pop_weight),
        rich_label = paste0("<span style='color:", .data$color, ";'>\u25A0</span> ", .data$label)
      )
    id_to_rich <- stats::setNames(weight_lookup$rich_label, as.character(weight_lookup$concept_id))
    data$concept_label <- ifelse(
      as.character(data$concept_id) %in% names(id_to_rich),
      id_to_rich[as.character(data$concept_id)],
      data$concept_label
    )
    pw_lookup <- stats::setNames(concept_weights$pop_weight, as.character(concept_weights$concept_id))
    data$pop_weight <- pw_lookup[as.character(data$concept_id)]
  }

  # Order concept labels
  if (!is.null(concept_order)) {
    id_to_label <- data |> dplyr::distinct(.data$concept_id, .data$concept_label)
    ordered_labels <- id_to_label |>
      dplyr::mutate(pos = match(.data$concept_id, concept_order)) |>
      dplyr::arrange(.data$pos) |>
      dplyr::pull(.data$concept_label) |>
      unique()
    data$concept_label <- factor(data$concept_label, levels = ordered_labels)
  } else {
    data$concept_label <- factor(data$concept_label,
                                 levels = sort(unique(data$concept_label)))
  }

  data$age_label <- paste0(substr(data$age_group, 1, 2), "..")
  data$age_label <- factor(data$age_label,
                           levels = paste0(substr(age_group_levels, 1, 2), ".."))

  data$sex_label <- factor(
    ifelse(data$sex == "F", "F", "M"),
    levels = c("F", "M")
  )

  data$age_label <- droplevels(data$age_label)
  data$sex_label <- droplevels(data$sex_label)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$age_group, y = .data$concept_label, fill = .data$log2_pr)) +
    ggiraph::geom_tile_interactive(
      color = NA, width = 1.25,
      ggplot2::aes(
        tooltip = paste0(
          .data$concept_name,
          "\nCode: ", .data$concept_code,
          if (!is.null(concept_weights)) paste0("\nPopulation: ", signif(.data$pop_weight, 2), "% of persons") else "",
          "\nSex: ", .data$sex_label, ", Age: ", .data$age_group,
          "\np: ", format_pval(.data$p_value),
          "\nlog\u2082 PR: ", round(.data$log2_pr, 2),
          " [", round(.data$ci_low, 2), ", ", round(.data$ci_high, 2), "]",
          "\n", format_fold(.data$log2_pr)
        ),
        data_id = .data$concept_id
      )
    ) +
    ggplot2::scale_fill_gradient2(
      low = COLOR_DS_REF, mid = "#EDEDED", high = COLOR_DS_COMP,
      midpoint = 0, limits = c(-2, 2), oob = scales::squish,
      name = if (scale_mode == "fold") "Fold Difference" else "log\u2082 PR",
      breaks = c(-2, -1, 0, 1, 2),
      labels = if (scale_mode == "fold") c("0.25", "0.5", "1", "2", "4")
               else c("-2", "-1", "0", "+1", "+2")
    ) +
    ggplot2::labs(
      title = paste0(
        "Prevalence higher in <span style='color:", COLOR_DS_REF, ";'>",
        name1, "</span> or <span style='color:", COLOR_DS_COMP, ";'>",
        name2, "</span>"
      ),
      subtitle = if (scale_mode == "fold") {
        paste0(
          "Fold Difference: <span style='color:", COLOR_DS_COMP, ";'>", name2,
          "</span> / <span style='color:", COLOR_DS_REF, ";'>", name1,
          "</span> (1 = equal prevalence)"
        )
      } else {
        paste0(
          "log\u2082(PR) = log\u2082(<span style='color:", COLOR_DS_COMP, ";'>", name2,
          "</span> / <span style='color:", COLOR_DS_REF, ";'>", name1,
          "</span>) \u00b7 Meta-analysis across years"
        )
      },
      x = "Age Group", y = NULL
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "plain", hjust = 0,
                                               color = COLOR_INK_MUTED),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "plain", size = 7, colour = COLOR_INK_MUTED),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 8),
      strip.text.y = ggtext::element_markdown(size = 9, angle = 0, hjust = 0),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = "white", linewidth = 0),
      panel.spacing.y = ggplot2::unit(0, "cm"),
      panel.spacing.x = ggplot2::unit(0, "cm"),
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.key.height = ggplot2::unit(0.3, "cm"),
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8)
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$concept_label),
      cols = ggplot2::vars(.data$sex_label, .data$age_label),
      scales = "free", space = "free_y",
      labeller = ggplot2::labeller(sex_label = ggplot2::label_value,
                                   age_label = ggplot2::label_value,
                                   concept_label = ggplot2::label_value)
    )

  n_concepts <- length(unique(data$concept_label))
  svg_height <- n_concepts * 0.17 + 1.8

  ggiraph::girafe(
    ggobj = plot, height_svg = svg_height, width_svg = 12,
    options = list(
      ggiraph::opts_tooltip(
        opacity = 0.95,
        css = "background-color:#FFFFFF; color:#1A1A1A; padding:8px 12px; border-radius:4px; border:1px solid #D0D0D0; box-shadow:0 2px 8px rgba(0,0,0,0.12); font-size:12px; font-family:-apple-system,sans-serif; max-width:320px; white-space:pre-line;"
      ),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "single", only_shiny = FALSE,
                              css = "stroke:#444444;stroke-width:0.5"),
      ggiraph::opts_hover(css = "stroke:#999999;stroke-width:0.4;cursor:pointer;")
    )
  )
}
