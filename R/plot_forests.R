# ── Syrona Plot: Forest Plots ─────────────────────────────────────────────────
#
# Forest plot builders for the Syrona dashboard:
# - build_forest_detail: per-year forest for a single concept
# - build_point_diff: dumbbell chart of absolute prevalence
# - build_forest_summary: F/M/Both compact forest for a single concept
# - build_forest_overview: F/M/Both meta PR for all concepts (paired with heatmap)

#' Build a per-year forest plot for a single concept
#'
#' Shows yearly points + CIs, meta-analysis diamond, and colored heatmap tile row.
#' Faceted by sex x age group.
#'
#' @param yearly_data Yearly comparison data for one concept.
#' @param meta_data Meta-analysis data (across years) for one concept.
#' @param concept_name Concept name string.
#' @param concept_code Concept code string.
#' @param name1 Reference dataset name.
#' @param name2 Comparison dataset name.
#' @param scale_mode "fold" or "log2".
#' @return A girafe object.
#' @export
build_forest_detail <- function(yearly_data, meta_data, concept_name, concept_code,
                                name1, name2, scale_mode = "log2") {
  if (nrow(yearly_data) == 0) return(NULL)

  years <- sort(unique(yearly_data$year))
  year_levels <- c(as.character(years), "Meta", "Heatmap")
  tile_y <- length(year_levels)
  sex_lvl <- c("F", "M")
  make_sex <- function(s) factor(s, levels = sex_lvl)

  yearly_plot <- yearly_data |>
    dplyr::mutate(year_f = factor(as.character(.data$year), levels = year_levels),
                  sig_label = ifelse(.data$sig, "sig", "nosig"),
                  sex_label = make_sex(.data$sex))

  has_meta <- nrow(meta_data) > 0
  if (has_meta) {
    meta_plot <- meta_data |>
      dplyr::mutate(year_f = factor("Meta", levels = year_levels),
                    sig_label = ifelse(.data$sig, "sig", "nosig"),
                    sex_label = make_sex(.data$sex))
    meta_sig   <- meta_plot |> dplyr::filter(.data$sig)
    meta_nosig <- meta_plot |> dplyr::filter(!.data$sig)
    tile_plot  <- meta_data |>
      dplyr::mutate(year_f = factor("Heatmap", levels = year_levels),
                    sex_label = make_sex(.data$sex),
                    fold_label = format_fold(.data$log2_pr))
  } else {
    meta_sig <- meta_nosig <- tile_plot <- data.frame()
  }

  x_breaks <- c(-2, -1, 0, 1, 2)
  x_labels <- if (scale_mode == "fold") c("\u00bc", "\u00bd", "1", "2", "4")
              else c("-2", "-1", "0", "+1", "+2")

  p <- ggplot2::ggplot(yearly_plot, ggplot2::aes(x = .data$log2_pr, y = .data$year_f)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = tile_y - 0.5,
                      fill = COLOR_DS_REF, alpha = 0.07) +
    ggplot2::annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = tile_y - 0.5,
                      fill = COLOR_DS_COMP, alpha = 0.07) +
    ggplot2::geom_vline(xintercept = 0, color = COLOR_INK_SUBTLE, linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = c(-log2(FOLD_THRESHOLD), log2(FOLD_THRESHOLD)),
                        linetype = "dotted", color = "orangered", linewidth = 0.3, alpha = 0.6)

  # Heatmap tile row
  if (nrow(tile_plot) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data = tile_plot, ggplot2::aes(fill = .data$log2_pr),
        xmin = -Inf, xmax = Inf,
        ymin = tile_y - 0.45, ymax = tile_y + 0.45,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_gradient2(
        low = COLOR_DS_REF, mid = "#EDEDED", high = COLOR_DS_COMP,
        midpoint = 0, limits = c(-2, 2), oob = scales::squish, guide = "none"
      ) +
      ggplot2::geom_text(
        data = tile_plot,
        ggplot2::aes(x = 0, y = .data$year_f, label = .data$fold_label),
        inherit.aes = FALSE,
        size = 2.5, color = "black", fontface = "bold"
      )
  }

  # Yearly error bars + points
  p <- p +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high,
                   color = .data$sig_label, alpha = .data$sig_label),
      width = 0, linewidth = 0.4
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(color = .data$sig_label, alpha = .data$sig_label,
                   tooltip = paste0(
                     .data$year, " \u00b7 ", .data$sex_label, " \u00b7 ", .data$age_group,
                     "\nlog\u2082 PR: ", round(.data$log2_pr, 2),
                     " [", round(.data$ci_low, 2), ", ", round(.data$ci_high, 2), "]",
                     "\n", format_fold(.data$log2_pr),
                     "\np: ", format_pval(.data$p_value),
                     "\nPatients: ", .data$patient_count_d1, " vs ", .data$patient_count_d2
                   )),
      shape = 15, size = 2
    )

  # Meta significant
  if (nrow(meta_sig) > 0) {
    p <- p +
      ggplot2::geom_errorbar(
        data = meta_sig,
        ggplot2::aes(x = .data$log2_pr, xmin = .data$ci_low, xmax = .data$ci_high, y = .data$year_f),
        width = 0, linewidth = 0.5, color = "darkred",
        inherit.aes = FALSE
      ) +
      ggiraph::geom_point_interactive(
        data = meta_sig,
        ggplot2::aes(x = .data$log2_pr, y = .data$year_f,
                     tooltip = paste0(
                       "Meta \u00b7 ", .data$sex_label, " \u00b7 ", .data$age_group,
                       "\nlog\u2082 PR: ", round(.data$log2_pr, 2),
                       " [", round(.data$ci_low, 2), ", ", round(.data$ci_high, 2), "]",
                       "\n", format_fold(.data$log2_pr),
                       "\np: ", format_pval(.data$p_value),
                       "\nModel: ", .data$meta_model_type, " (", .data$n_strata, " strata)"
                     )),
        shape = 23, size = 3, fill = "darkred", color = "darkred",
        inherit.aes = FALSE
      )
  }

  # Meta not significant
  if (nrow(meta_nosig) > 0) {
    p <- p +
      ggplot2::geom_errorbar(
        data = meta_nosig,
        ggplot2::aes(x = .data$log2_pr, xmin = .data$ci_low, xmax = .data$ci_high, y = .data$year_f),
        width = 0, linewidth = 0.5, color = COLOR_NOSIG,
        inherit.aes = FALSE
      ) +
      ggiraph::geom_point_interactive(
        data = meta_nosig,
        ggplot2::aes(x = .data$log2_pr, y = .data$year_f,
                     tooltip = paste0(
                       "Meta \u00b7 ", .data$sex_label, " \u00b7 ", .data$age_group,
                       "\nlog\u2082 PR: ", round(.data$log2_pr, 2),
                       " [", round(.data$ci_low, 2), ", ", round(.data$ci_high, 2), "]",
                       "\n", format_fold(.data$log2_pr),
                       "\np: ", format_pval(.data$p_value),
                       "\nModel: ", .data$meta_model_type, " (", .data$n_strata, " strata)"
                     )),
        shape = 23, size = 3, fill = COLOR_NOSIG, color = COLOR_NOSIG,
        inherit.aes = FALSE
      )
  }

  # Scales, facets, theme
  p <- p +
    ggplot2::scale_color_manual(values = c("sig" = COLOR_SIG, "nosig" = COLOR_NOSIG), guide = "none") +
    ggplot2::scale_alpha_manual(values = c("sig" = 1, "nosig" = NOSIG_ALPHA), guide = "none") +
    ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels,
                                limits = c(-3, 3), oob = scales::squish) +
    ggplot2::scale_y_discrete(drop = FALSE,
                              labels = function(x) ifelse(x == "Heatmap", "", x)) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$sex_label, .data$age_group)) +
    ggplot2::labs(
      title = paste0(concept_name, "  (", concept_code, ")"),
      subtitle = if (scale_mode == "fold") {
        paste0(
          "Fold Difference: <span style='color:", COLOR_DS_COMP, ";'>", name2,
          "</span> / <span style='color:", COLOR_DS_REF, ";'>", name1,
          "</span> \u00b7 Dotted lines at ", FOLD_THRESHOLD, "\u00d7"
        )
      } else {
        paste0(
          "log\u2082(<span style='color:", COLOR_DS_COMP, ";'>", name2,
          "</span> / <span style='color:", COLOR_DS_REF, ";'>", name1,
          "</span>) \u00b7 Dotted lines at \u00b1", round(log2(FOLD_THRESHOLD), 2)
        )
      },
      x = if (scale_mode == "fold") "Fold Difference" else "log\u2082 PR",
      y = NULL
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 10, color = COLOR_INK_MUTED, hjust = 0),
      axis.text.y = ggplot2::element_text(size = 9, color = COLOR_INK_DATA),
      axis.text.x = ggplot2::element_text(size = 8),
      axis.title.x = ggplot2::element_text(size = 9, color = COLOR_INK_MUTED),
      axis.line = ggplot2::element_line(color = "#D0D0D0", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(color = "#D0D0D0", linewidth = 0.3),
      strip.text = ggplot2::element_text(size = 8),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = "white", linewidth = 0),
      panel.spacing.x = ggplot2::unit(0.25, "cm"),
      panel.grid.major.y = ggplot2::element_line(color = "#f0f0f0", linewidth = 0.2)
    )

  ggiraph::girafe(
    ggobj = p, height_svg = 5, width_svg = 14,
    options = list(
      ggiraph::opts_tooltip(
        opacity = 0.95,
        css = "background-color:#FFFFFF; color:#1A1A1A; padding:8px 12px; border-radius:4px; border:1px solid #D0D0D0; box-shadow:0 2px 8px rgba(0,0,0,0.12); font-size:12px; font-family:-apple-system,sans-serif; max-width:320px; white-space:pre-line;"
      ),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
    )
  )
}


#' Build a dumbbell chart showing absolute prevalence values
#'
#' Complements the forest detail (relative -> absolute).
#' Same facet structure: sex x age_group.
#'
#' @param yearly_data Yearly comparison data for one concept.
#' @param concept_name Concept name string.
#' @param concept_code Concept code string.
#' @param name1 Reference dataset name.
#' @param name2 Comparison dataset name.
#' @return A girafe object.
#' @export
build_point_diff <- function(yearly_data, concept_name, concept_code,
                             name1, name2) {
  if (nrow(yearly_data) == 0) return(NULL)

  sex_lvl <- c("F", "M")
  plot_data <- yearly_data |>
    dplyr::mutate(
      year_f = factor(as.character(.data$year)),
      sex_label = factor(.data$sex, levels = sex_lvl)
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = .data$year_f)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = .data$prevalence_d1, y = .data$year_f, yend = .data$year_f),
      color = "#D0D0D0", linewidth = 0.3
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = .data$prevalence_d2, y = .data$year_f, yend = .data$year_f),
      color = "#D0D0D0", linewidth = 0.3
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$prevalence_d1, xend = .data$prevalence_d2,
                   y = .data$year_f, yend = .data$year_f),
      color = COLOR_INK, linewidth = 0.6
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(x = .data$prevalence_d1,
                   tooltip = paste0(
                     name1, " \u00b7 ", .data$year, " \u00b7 ", .data$sex_label, " \u00b7 ", .data$age_group,
                     "\nPrevalence: ", signif(.data$prevalence_d1, 3),
                     "\nPatients: ", format(.data$patient_count_d1, big.mark = ","),
                     "\nDenominator: ", format(.data$denominator_d1, big.mark = ",")
                   )),
      color = COLOR_DS_REF, size = 2.5, shape = 16
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(x = .data$prevalence_d2,
                   tooltip = paste0(
                     name2, " \u00b7 ", .data$year, " \u00b7 ", .data$sex_label, " \u00b7 ", .data$age_group,
                     "\nPrevalence: ", signif(.data$prevalence_d2, 3),
                     "\nPatients: ", format(.data$patient_count_d2, big.mark = ","),
                     "\nDenominator: ", format(.data$denominator_d2, big.mark = ",")
                   )),
      color = COLOR_DS_COMP, size = 2.5, shape = 16
    ) +
    ggplot2::geom_vline(xintercept = 0, color = "#D0D0D0", linewidth = 0.3) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(n = 3),
      labels = function(x) ifelse(x == 0, "0", signif(x, 2))
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$sex_label, .data$age_group)) +
    ggplot2::labs(
      title = paste0(concept_name, "  (", concept_code, ")"),
      subtitle = paste0(
        "Prevalence: <span style='color:", COLOR_DS_COMP, ";'>", name2,
        "</span> / <span style='color:", COLOR_DS_REF, ";'>", name1, "</span>"
      ),
      x = "Prevalence",
      y = NULL
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 10, color = COLOR_INK_MUTED, hjust = 0),
      axis.text.y = ggplot2::element_text(size = 9, color = COLOR_INK_DATA),
      axis.text.x = ggplot2::element_text(size = 7),
      axis.title.x = ggplot2::element_text(size = 9, color = COLOR_INK_MUTED),
      axis.line = ggplot2::element_line(color = "#D0D0D0", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(color = "#D0D0D0", linewidth = 0.3),
      strip.text = ggplot2::element_text(size = 8),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = "white", linewidth = 0),
      panel.spacing.x = ggplot2::unit(0.25, "cm"),
      panel.grid.major.y = ggplot2::element_line(color = "#f0f0f0", linewidth = 0.2)
    )

  ggiraph::girafe(
    ggobj = p, height_svg = 3.5, width_svg = 14,
    options = list(
      ggiraph::opts_tooltip(
        opacity = 0.95,
        css = "background-color:#FFFFFF; color:#1A1A1A; padding:8px 12px; border-radius:4px; border:1px solid #D0D0D0; box-shadow:0 2px 8px rgba(0,0,0,0.12); font-size:12px; font-family:-apple-system,sans-serif; max-width:320px; white-space:pre-line;"
      ),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
    )
  )
}


#' Build a compact F/M/Both forest plot for a single concept
#'
#' @param meta_sex_data Meta-analysis data by sex (F, M) for one concept.
#' @param meta_summary_data Meta-analysis summary (Both) for one concept.
#' @param concept_name Concept name string.
#' @param concept_code Concept code string.
#' @param name1 Reference dataset name.
#' @param name2 Comparison dataset name.
#' @param scale_mode "fold" or "log2".
#' @return A girafe object.
#' @export
build_forest_summary <- function(meta_sex_data, meta_summary_data,
                                  concept_name, concept_code,
                                  name1, name2, scale_mode = "log2") {

  plot_data <- dplyr::bind_rows(meta_sex_data, meta_summary_data) |>
    dplyr::mutate(
      sex_label = dplyr::case_when(
        .data$sex == "F"    ~ "Female",
        .data$sex == "M"    ~ "Male",
        .data$sex == "Both" ~ "Both",
        TRUE                ~ .data$sex
      ),
      sex_label = factor(.data$sex_label, levels = c("Both", "Male", "Female")),
      sig_label = ifelse(.data$sig, "sig", "nosig")
    )

  if (nrow(plot_data) == 0) return(NULL)

  if (scale_mode == "fold") {
    x_breaks <- log2(c(0.25, 0.5, 1, 2, 4))
    x_labels <- as.character(2^x_breaks)
  } else {
    x_breaks <- c(-2, -1, 0, 1, 2)
    x_labels <- as.character(x_breaks)
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$log2_pr, y = .data$sex_label)) +
    ggplot2::geom_vline(xintercept = 0, color = COLOR_INK_DATA, linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = log2(FOLD_THRESHOLD), color = COLOR_INK_MUTED,
                        linetype = 3, linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = -log2(FOLD_THRESHOLD), color = COLOR_INK_MUTED,
                        linetype = 3, linewidth = 0.4) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high, color = .data$sig_label),
      width = 0, linewidth = 0.5
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(color = .data$sig_label,
                   tooltip = paste0(
                     .data$sex_label, "\n",
                     format_fold(.data$log2_pr),
                     "\np: ", format_pval(.data$p_value),
                     "\nModel: ", .data$meta_model_type, " (", .data$n_strata, " strata)"
                   )),
      shape = 18, size = 3.5
    ) +
    ggplot2::scale_color_manual(values = c("sig" = COLOR_SIG, "nosig" = COLOR_NOSIG), guide = "none") +
    ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    ggplot2::labs(
      title = paste0(truncate_name(concept_name, 60), " [", concept_code, "]"),
      x = if (scale_mode == "fold") "Fold Difference" else expression(log[2]~PR),
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 9, color = COLOR_INK_MUTED),
      axis.text = ggplot2::element_text(size = 10, color = COLOR_INK_DATA),
      plot.title = ggplot2::element_text(size = 11, color = COLOR_INK, face = "bold"),
      plot.margin = ggplot2::margin(4, 12, 4, 8)
    )

  ggiraph::girafe(
    ggobj = p,
    width_svg = 8, height_svg = 1.5,
    options = list(
      ggiraph::opts_tooltip(
        opacity = 0.95,
        css = "background-color:#FFFFFF; color:#1A1A1A; padding:8px 12px; border-radius:4px; border:1px solid #D0D0D0; box-shadow:0 2px 8px rgba(0,0,0,0.12); font-size:12px; font-family:-apple-system,sans-serif; max-width:320px; white-space:pre-line;"
      ),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
    )
  )
}


#' Build an overview forest plot paired with the heatmap
#'
#' Compact forest showing Female, Male, Both meta PR for every concept.
#' Concept y-axis ordering matches the heatmap.
#'
#' @param meta_sex_df Meta-analysis by sex data (F/M rows).
#' @param meta_summary_df Meta-analysis summary data (Both rows).
#' @param concept_ids Vector of concept_ids to include.
#' @param concept_info Data frame with concept_id, concept_name, concept_code, pop_weight.
#' @param name1 Reference dataset name.
#' @param name2 Comparison dataset name.
#' @param scale_mode "fold" or "log2".
#' @param concept_order Vector of concept_ids in desired display order.
#' @return A girafe object.
#' @export
build_forest_overview <- function(meta_sex_df, meta_summary_df, concept_ids,
                                  concept_info, name1, name2, scale_mode = "log2",
                                  concept_order = NULL) {

  sex_data <- meta_sex_df |>
    dplyr::filter(.data$concept_id %in% concept_ids) |>
    dplyr::mutate(sex_label = ifelse(.data$sex == "F", "Female", "Male"))

  both_data <- meta_summary_df |>
    dplyr::filter(.data$concept_id %in% concept_ids) |>
    dplyr::mutate(sex_label = "Both")

  has_weight <- "pop_weight" %in% names(concept_info)

  plot_data <- dplyr::bind_rows(sex_data, both_data) |>
    dplyr::left_join(concept_info |> dplyr::select(dplyr::all_of("concept_id"),
                                                    dplyr::all_of("concept_name"),
                                                    dplyr::all_of("concept_code"),
                                                    dplyr::any_of("pop_weight")),
                     by = "concept_id") |>
    dplyr::mutate(
      concept_label = truncate_name(.data$concept_name),
      sex_label = factor(.data$sex_label, levels = c("Female", "Male", "Both")),
      sig = ifelse(.data$sig, "sig", "nosig")
    )

  # Apply weight-colored square to concept labels
  if (has_weight && "pop_weight" %in% names(plot_data)) {
    weight_by_id <- plot_data |>
      dplyr::distinct(.data$concept_id, .data$concept_label, .data$pop_weight) |>
      dplyr::mutate(
        color = weight_color(.data$pop_weight),
        rich_label = paste0("<span style='color:", .data$color, ";'>\u25A0</span> ", .data$concept_label)
      )
    id_to_rich <- stats::setNames(weight_by_id$rich_label, as.character(weight_by_id$concept_id))
    plot_data$concept_label <- id_to_rich[as.character(plot_data$concept_id)]
  }

  # Order concept labels
  if (!is.null(concept_order)) {
    id_to_label <- plot_data |> dplyr::distinct(.data$concept_id, .data$concept_label)
    ordered_labels <- id_to_label |>
      dplyr::mutate(pos = match(.data$concept_id, concept_order)) |>
      dplyr::arrange(.data$pos) |>
      dplyr::pull(.data$concept_label) |>
      unique()
    plot_data$concept_label <- factor(plot_data$concept_label, levels = ordered_labels)
  } else {
    plot_data$concept_label <- factor(plot_data$concept_label,
                                       levels = sort(unique(plot_data$concept_label)))
  }

  if (nrow(plot_data) == 0) return(NULL)

  x_breaks <- c(-2, -1, 0, 1, 2)
  if (scale_mode == "fold") {
    x_labels <- c("0.25", "0.5", "1", "2", "4")
  } else {
    x_labels <- c("-2", "-1", "0", "1", "2")
  }

  sex_colors <- c("Female" = COLOR_SEX_F, "Male" = COLOR_SEX_M, "Both" = COLOR_SEX_BOTH)

  n_concepts <- length(unique(plot_data$concept_label))

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$log2_pr, y = .data$concept_label)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
                      fill = COLOR_DS_REF, alpha = 0.05) +
    ggplot2::annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = COLOR_DS_COMP, alpha = 0.05) +
    ggplot2::geom_vline(xintercept = 0, color = "grey") +
    ggplot2::geom_vline(xintercept = log2(FOLD_THRESHOLD), color = COLOR_INK_MUTED,
                        linetype = 3, linewidth = 0.5, alpha = 0.9) +
    ggplot2::geom_vline(xintercept = -log2(FOLD_THRESHOLD), color = COLOR_INK_MUTED,
                        linetype = 3, linewidth = 0.5, alpha = 0.9) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = pmax(.data$ci_low, -2.5), xmax = pmin(.data$ci_high, 2.5),
                   color = .data$sex_label, alpha = .data$sig),
      width = 0.2, position = ggplot2::position_dodge(width = 0.6)
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(color = .data$sex_label, fill = .data$sex_label, alpha = .data$sig,
                   tooltip = paste0(
                     .data$concept_name, " [", .data$concept_code, "]",
                     if (has_weight) paste0("\nPopulation: ", signif(.data$pop_weight, 2), "% of persons") else "",
                     "\n", .data$sex_label,
                     "\n", format_fold(.data$log2_pr),
                     "\np: ", format_pval(.data$p_value),
                     "\nlog\u2082 PR: ", round(.data$log2_pr, 2),
                     " [", round(.data$ci_low, 2), ", ", round(.data$ci_high, 2), "]"
                   ),
                   data_id = .data$concept_id),
      shape = 21, size = 2, position = ggplot2::position_dodge(width = 0.6)
    ) +
    ggplot2::scale_color_manual(values = sex_colors, guide = "none") +
    ggplot2::scale_fill_manual(values = sex_colors, guide = "none") +
    ggplot2::scale_alpha_manual(values = c("nosig" = 0.5, "sig" = 1), guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = x_breaks, labels = x_labels,
      limits = c(-2.5, 2.5)
    ) +
    ggplot2::labs(
      title = paste0(
        "Prevalence higher in <span style='color:", COLOR_DS_REF, ";'>",
        name1, "</span> or <span style='color:", COLOR_DS_COMP, ";'>",
        name2, "</span>"
      ),
      subtitle = paste0(
        "<span style='color:", COLOR_SEX_F, ";'>Female</span>  ",
        "<span style='color:", COLOR_SEX_M, ";'>Male</span>  ",
        "<span style='color:", COLOR_SEX_BOTH, ";'>Both</span>",
        " \u00b7 Meta-analysis across years, age groups"
      ),
      x = if (scale_mode == "fold") "Fold Difference" else "log\u2082 PR",
      y = NULL
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$concept_label), scales = "free_y", space = "free_y",
      labeller = ggplot2::labeller(concept_label = ggplot2::label_value)
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
      strip.text.y = ggtext::element_markdown(size = 9, angle = 0, hjust = 0),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = "white", linewidth = 0),
      panel.spacing.y = ggplot2::unit(0, "cm"),
      panel.spacing.x = ggplot2::unit(0, "cm")
    )

  svg_height <- n_concepts * 0.17 + 1.8

  ggiraph::girafe(
    ggobj = p, height_svg = svg_height, width_svg = 12,
    options = list(
      ggiraph::opts_tooltip(
        opacity = 0.9,
        css = "background-color:#FFFFFF; color:#1A1A1A; padding:8px 12px; border-radius:4px; border:1px solid #D0D0D0; box-shadow:0 2px 8px rgba(0,0,0,0.12); font-size:12px; font-family:-apple-system,sans-serif; max-width:320px; white-space:pre-line;"
      ),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "single", only_shiny = FALSE,
                              css = "stroke:#444444;stroke-width:0.5"),
      ggiraph::opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
    )
  )
}
