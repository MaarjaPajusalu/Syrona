# ── PR Distribution Histogram ──────────────────────────────────────────────────
#
# Histogram of pooled prevalence ratios across all compared concepts.
# Shows distribution bias between two datasets.

#' Build a PR distribution histogram from meta_summary data
#'
#' @param data Filtered meta_summary rows (one per concept).
#'   Expected columns: log2_pr, fold_diff, sig, concept_id.
#' @param d1_name Name of reference dataset (shown on left side).
#' @param d2_name Name of comparison dataset (shown on right side).
#' @param scale_mode "fold" or "log2" - controls x-axis labels only.
#' @param fold_thresh Fold difference threshold for zone coloring.
#' @param domain_label Label for domain (e.g. "conditions", "drugs").
#' @return A ggplot object.
#' @export
build_pr_distribution <- function(data, d1_name, d2_name, scale_mode = "fold",
                                  fold_thresh = FOLD_THRESHOLD,
                                  domain_label = "conditions") {

  log2_thresh <- log2(fold_thresh)
  n_div <- max(3L, round(log2_thresh / 0.075))
  binw <- log2_thresh / n_div

  plot_data <- data |>
    dplyr::filter(!is.na(.data$fold_diff), is.finite(.data$fold_diff)) |>
    dplyr::mutate(
      log2_fd = log2(pmax(pmin(.data$fold_diff, 8), 0.125)),
      zone = dplyr::case_when(
        .data$fold_diff > fold_thresh   ~ "d2",
        .data$fold_diff < 1 / fold_thresh ~ "d1",
        TRUE ~ "similar"
      ),
      bin = floor(.data$log2_fd / binw) * binw + binw / 2
    )

  n_total <- nrow(plot_data)
  median_fold <- stats::median(plot_data$fold_diff, na.rm = TRUE)
  n_over    <- sum(plot_data$fold_diff > fold_thresh, na.rm = TRUE)
  n_under   <- sum(plot_data$fold_diff < 1 / fold_thresh, na.rm = TRUE)
  n_similar <- n_total - n_over - n_under
  pct_over    <- round(n_over / n_total * 100, 1)
  pct_under   <- round(n_under / n_total * 100, 1)
  pct_similar <- round(n_similar / n_total * 100, 1)

  binned <- plot_data |>
    dplyr::group_by(.data$bin, .data$zone) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(
      xmin = 2^(.data$bin - binw / 2),
      xmax = 2^(.data$bin + binw / 2)
    )

  if (scale_mode == "fold") {
    x_label <- "Fold Difference"
    x_breaks <- c(0.125, 0.25, 0.5, 1, 2, 4, 8)
    x_labels <- as.character(x_breaks)
  } else {
    x_label <- expression(log[2]~PR)
    x_breaks <- c(0.125, 0.25, 0.5, 1, 2, 4, 8)
    x_labels <- as.character(log2(x_breaks))
  }

  annotation <- paste0(
    n_total, " ", domain_label, "\n",
    "Median fold: ", sprintf("%.2f", median_fold), "\n\n",
    "Over in ", d2_name, ": ", n_over, " (", pct_over, "%)\n",
    "Similar: ", n_similar, " (", pct_similar, "%)\n",
    "Under in ", d2_name, ": ", n_under, " (", pct_under, "%)"
  )

  fill_vals <- c("d1" = COLOR_DS_REF, "d2" = COLOR_DS_COMP, "similar" = "#4D4D4D")

  binned <- binned |>
    dplyr::arrange(.data$bin, .data$zone) |>
    dplyr::group_by(.data$bin) |>
    dplyr::mutate(ymax = cumsum(.data$count), ymin = .data$ymax - .data$count) |>
    dplyr::ungroup()

  p <- ggplot2::ggplot(binned, ggplot2::aes(fill = .data$zone)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                     ymin = .data$ymin, ymax = .data$ymax),
                       color = "white", linewidth = 0.2, alpha = 0.8) +
    ggplot2::scale_x_continuous(
      x_label,
      trans = "log2",
      limits = c(0.125, 8),
      breaks = x_breaks,
      labels = x_labels
    ) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::geom_vline(xintercept = 1, color = COLOR_INK_DATA, linetype = 1,
                        linewidth = 0.7, alpha = 0.9) +
    ggplot2::geom_vline(xintercept = fold_thresh, color = COLOR_INK_MUTED,
                        linetype = 3, linewidth = 0.5, alpha = 0.9) +
    ggplot2::geom_vline(xintercept = 1 / fold_thresh, color = COLOR_INK_MUTED,
                        linetype = 3, linewidth = 0.5, alpha = 0.9) +
    ggplot2::annotate("text", x = 1 / 1.7, y = Inf, label = paste0("\u25C4 ", d1_name),
                      hjust = 1, vjust = 1.5, size = 3.5, color = COLOR_DS_REF,
                      fontface = "bold") +
    ggplot2::annotate("text", x = 1.7, y = Inf, label = paste0(d2_name, " \u25BA"),
                      hjust = 0, vjust = 1.5, size = 3.5, color = COLOR_DS_COMP,
                      fontface = "bold") +
    ggplot2::annotate("label", x = 8, y = Inf, label = annotation,
                      hjust = 1, vjust = 1.3, size = 3.2, color = COLOR_INK_DATA,
                      lineheight = 1.2, fill = "white", label.size = 0,
                      label.padding = ggplot2::unit(6, "pt")) +
    ggplot2::labs(y = paste0("Number of ", domain_label)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 11, color = COLOR_INK_MUTED),
      axis.text = ggplot2::element_text(size = 10, color = COLOR_INK_DATA),
      plot.margin = ggplot2::margin(12, 16, 8, 8)
    )

  p
}


#' Build a faceted PR distribution - one small histogram per chapter
#'
#' Interactive (ggiraph): clicking a chapter facet returns its chapter_id.
#'
#' @param data Filtered meta_summary joined with chapters (one row per
#'   concept x chapter). Expected columns: log2_pr, fold_diff, sig,
#'   concept_id, chapter_name, chapter_id.
#' @param d1_name Name of reference dataset.
#' @param d2_name Name of comparison dataset.
#' @param scale_mode "fold" or "log2".
#' @param fold_thresh Fold difference threshold.
#' @param domain_label Label for domain.
#' @return A girafe object. Selection data_id = chapter_id (character).
#' @export
build_pr_distribution_chapters <- function(data, d1_name, d2_name, scale_mode = "fold",
                                           fold_thresh = FOLD_THRESHOLD,
                                           domain_label = "conditions") {

  plot_data <- data |>
    dplyr::filter(!is.na(.data$fold_diff), is.finite(.data$fold_diff)) |>
    dplyr::mutate(
      log2_fd = log2(pmax(pmin(.data$fold_diff, 8), 0.125)),
      zone = dplyr::case_when(
        .data$fold_diff > fold_thresh   ~ "d2",
        .data$fold_diff < 1 / fold_thresh ~ "d1",
        TRUE ~ "similar"
      ),
      chapter_code = ifelse(
        .data$chapter_name == "(Unmapped)", "(Unmapped)",
        sub("\\. .*", "", .data$chapter_name)
      )
    )

  chapter_counts <- plot_data |>
    dplyr::group_by(.data$chapter_code, .data$chapter_id) |>
    dplyr::summarise(n = dplyr::n_distinct(.data$concept_id), .groups = "drop")

  multi_chapter <- plot_data |>
    dplyr::distinct(.data$concept_id, .data$chapter_id) |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  shared_per_chapter <- multi_chapter |>
    dplyr::group_by(.data$chapter_id) |>
    dplyr::summarise(n_shared = dplyr::n_distinct(.data$concept_id), .groups = "drop")

  chapter_counts <- chapter_counts |>
    dplyr::left_join(shared_per_chapter, by = "chapter_id") |>
    dplyr::mutate(n_shared = tidyr::replace_na(.data$n_shared, 0L))

  plot_data <- plot_data |>
    dplyr::left_join(chapter_counts, by = c("chapter_code", "chapter_id")) |>
    dplyr::mutate(chapter_label = ifelse(
      .data$n_shared > 0,
      paste0("**", .data$chapter_code, "** <span style='font-size:6pt; color:#999999;'>(", .data$n, ", ", .data$n_shared, " shared)</span>"),
      paste0("**", .data$chapter_code, "** <span style='font-size:6pt; color:#999999;'>(", .data$n, ")</span>")
    ))

  label_order <- chapter_counts |>
    dplyr::mutate(sort_key = ifelse(.data$chapter_code == "(Unmapped)", "ZZZ", .data$chapter_code)) |>
    dplyr::arrange(.data$sort_key)
  label_levels <- ifelse(
    label_order$n_shared > 0,
    paste0("**", label_order$chapter_code, "** <span style='font-size:6pt; color:#999999;'>(", label_order$n, ", ", label_order$n_shared, " shared)</span>"),
    paste0("**", label_order$chapter_code, "** <span style='font-size:6pt; color:#999999;'>(", label_order$n, ")</span>")
  )
  plot_data$chapter_label <- factor(plot_data$chapter_label, levels = label_levels)

  log2_thresh <- log2(fold_thresh)
  n_div <- max(3L, round(log2_thresh / 0.075))
  binw <- log2_thresh / n_div

  plot_data <- plot_data |>
    dplyr::mutate(bin = floor(.data$log2_fd / binw) * binw + binw / 2)

  binned <- plot_data |>
    dplyr::group_by(.data$chapter_label, .data$chapter_id, .data$bin, .data$zone) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(
      xmin = 2^(.data$bin - binw / 2),
      xmax = 2^(.data$bin + binw / 2),
      tooltip = paste0(.data$count, " ", domain_label),
      data_id = as.character(.data$chapter_id)
    ) |>
    dplyr::arrange(.data$chapter_label, .data$bin, .data$zone) |>
    dplyr::group_by(.data$chapter_label, .data$bin) |>
    dplyr::mutate(ymax = cumsum(.data$count), ymin = .data$ymax - .data$count) |>
    dplyr::ungroup()

  x_breaks <- c(0.125, 0.25, 0.5, 1, 2, 4, 8)
  if (scale_mode == "fold") {
    x_label <- "Fold Difference"
    x_labels <- as.character(x_breaks)
  } else {
    x_label <- expression(log[2]~PR)
    x_labels <- as.character(log2(x_breaks))
  }

  fill_vals <- c("d1" = COLOR_DS_REF, "d2" = COLOR_DS_COMP, "similar" = "#4D4D4D")

  p <- ggplot2::ggplot(binned, ggplot2::aes(fill = .data$zone)) +
    ggiraph::geom_rect_interactive(
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
                   tooltip = .data$tooltip, data_id = .data$data_id),
      color = "white", linewidth = 0.15, alpha = 0.8
    ) +
    ggplot2::scale_x_continuous(
      x_label,
      trans = "log2",
      limits = c(0.125, 8),
      breaks = x_breaks,
      labels = x_labels
    ) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::geom_vline(xintercept = 1, color = COLOR_INK_DATA, linetype = 1,
                        linewidth = 0.5, alpha = 0.8) +
    ggplot2::geom_vline(xintercept = fold_thresh, color = COLOR_INK_MUTED,
                        linetype = 3, linewidth = 0.4, alpha = 0.7) +
    ggplot2::geom_vline(xintercept = 1 / fold_thresh, color = COLOR_INK_MUTED,
                        linetype = 3, linewidth = 0.4, alpha = 0.7) +
    ggplot2::annotate("text", x = 0.13, y = Inf, label = d1_name,
                      hjust = 0, vjust = 1.5, size = 2.2, color = COLOR_DS_REF,
                      fontface = "bold") +
    ggplot2::annotate("text", x = 7.5, y = Inf, label = d2_name,
                      hjust = 1, vjust = 1.5, size = 2.2, color = COLOR_DS_COMP,
                      fontface = "bold") +
    ggplot2::facet_wrap(~chapter_label, ncol = 4, scales = "free_y") +
    ggplot2::labs(y = paste0("Number of ", domain_label)) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 9, color = COLOR_INK_MUTED),
      axis.text = ggplot2::element_text(size = 7, color = COLOR_INK_DATA),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggtext::element_markdown(size = 8, hjust = 0, color = COLOR_INK),
      panel.spacing = ggplot2::unit(0.6, "lines"),
      plot.margin = ggplot2::margin(8, 12, 8, 8)
    )

  n_facets <- length(levels(binned$chapter_label))
  ncol <- 4
  n_rows <- ceiling(n_facets / ncol)
  height_svg <- max(5, n_rows * 2.0 + 1.5)

  ggiraph::girafe(
    ggobj = p,
    width_svg = 12, height_svg = height_svg,
    options = list(
      ggiraph::opts_hover(css = "fill-opacity:1; stroke:black; stroke-width:0.3;"),
      ggiraph::opts_selection(type = "single", only_shiny = TRUE,
                              css = "stroke:black; stroke-width:0.5;"),
      ggiraph::opts_tooltip(css = "background:#333; color:#fff; padding:4px 8px; border-radius:4px; font-size:11px;",
                            delay_mouseout = 200)
    )
  )
}
