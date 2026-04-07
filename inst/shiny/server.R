# ── Syrona Dashboard - Server ─────────────────────────────────────────────────

function(input, output, session) {

  # ── Domain state ────────────────────────────────────────────────────────────
  active_domain <- reactiveVal("condition")

  domain_cfg <- reactive({
    DOMAIN_CONFIG[[active_domain()]]
  })

  # ── Domain selector buttons ─────────────────────────────────────────────────
  domain_btns <- c(
    "domain_condition" = "condition",
    "domain_procedure" = "procedure",
    "domain_drug"      = "drug"
  )

  observeEvent(input$domain_condition, { active_domain("condition") })
  observeEvent(input$domain_procedure, { active_domain("procedure") })
  observeEvent(input$domain_drug,      { active_domain("drug") })

  observe({
    dom <- active_domain()
    avail <- comp_data()$available_domains
    for (btn_id in names(domain_btns)) {
      d <- domain_btns[[btn_id]]
      session$sendCustomMessage("toggleDomainBtn", list(
        id = btn_id,
        active = (d == dom),
        disabled = !(d %in% avail)
      ))
    }
  })

  # ── Core reactive: comparison data ──────────────────────────────────────────
  comp_data <- reactive({
    req(input$comparison_select)
    build_comparison_data(input$comparison_select, active_domain())
  })

  output$comparison_label <- renderUI({
    cd <- comp_data()
    tags$span(
      span(class = "ds1", cd$d1_name),
      " vs ",
      span(class = "ds2", cd$d2_name)
    )
  })

  # ── Reset on comparison or domain change ────────────────────────────────────
  observeEvent(list(input$comparison_select, active_domain()), {
    cd <- comp_data()
    cfg <- domain_cfg()
    if (!cd$available) return()

    updateSelectInput(session, "f_class_type",
      choices = cfg$class_types,
      selected = cfg$class_types[1])

    class_type <- cfg$class_types[1]
    updateSelectizeInput(session, "f_chapter",
      choices = get_chapter_choices(cd$chapters_df, class_type),
      selected = character(0))

    updateCheckboxGroupInput(session, "f_sex", selected = c("F", "M"))
    updateSelectizeInput(session, "f_age_group", selected = age_group_levels)

    search_choices <- setNames(
      as.character(cd$concept_info$concept_id),
      paste0(cd$concept_info$concept_name, "  (", cd$concept_info$concept_code, ")")
    )
    updateSelectizeInput(session, "concept_search",
      choices = c(setNames("", ""), search_choices), selected = character(0))
    selected_concept_id(NULL)
    rank_mode(NULL)
  }, ignoreInit = FALSE)

  # ── Attribute group container ───────────────────────────────────────────────
  output$attr_group <- renderUI({
    cfg <- domain_cfg()
    if (length(cfg$attr_types) == 0) return(NULL)
    div(class = "filter-group filter-group-attr",
      tags$span(class = "filter-group-label", "Attributes"),
      uiOutput("attr_slot_1"),
      uiOutput("attr_slot_2"),
      uiOutput("attr_slot_3"),
      uiOutput("attr_slot_4")
    )
  })

  # ── Dynamic attribute filter slots ──────────────────────────────────────────
  lapply(seq_len(MAX_ATTR_SLOTS), function(i) {
    output[[paste0("attr_slot_", i)]] <- renderUI({
      cfg <- domain_cfg()
      cd  <- comp_data()
      if (!cd$available) return(NULL)

      attr_types <- cfg$attr_types
      if (i > length(attr_types)) return(NULL)

      rel <- attr_types[i]
      label_text <- cfg$attr_labels[[rel]]
      tip_text   <- cfg$attr_tips[[rel]]
      input_id   <- paste0("f_attr_", i)
      counter_id <- paste0("cnt_attr_", i)

      selectizeInput(input_id,
        label = filter_label(label_text, tip_text, counter_id),
        choices = get_attribute_choices(cd$attrs_df, rel),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Any"))
    })
  })

  lapply(seq_len(MAX_ATTR_SLOTS), function(i) {
    output[[paste0("cnt_attr_", i)]] <- renderUI({
      cfg <- domain_cfg()
      cd  <- comp_data()
      attr_types <- cfg$attr_types
      if (i > length(attr_types)) return(NULL)

      rel <- attr_types[i]
      scope_ids <- chapter_scope()
      total <- length(get_attribute_choices(cd$attrs_df, rel, scope_ids))
      sel <- length(input[[paste0("f_attr_", i)]])
      cls <- if (sel > 0) "filter-counter has-selection" else "filter-counter"
      tags$span(class = cls, paste0(sel, " of ", total))
    })
  })

  # ── Sub-chapter slot ────────────────────────────────────────────────────────
  output$subchapter_slot <- renderUI({
    cd <- comp_data()
    if (!cd$available) return(NULL)
    ct <- input$f_class_type
    if (is.null(ct) || !has_subchapters(cd$chapters_df, ct)) return(NULL)

    cfg <- domain_cfg()
    tip <- if (!is.null(cfg$subchapter_tip)) cfg$subchapter_tip else "Sub-chapters within the selected chapter."
    choices <- get_subchapter_choices(cd$chapters_df, ct, input$f_chapter)
    selectizeInput("f_subchapter",
      label = filter_label("Sub-chapter", tip, "cnt_subchapter"),
      choices = choices,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Any sub-chapter"))
  })

  output$pr_chapter_toggle <- renderUI({
    cfg <- domain_cfg()
    checkboxInput("pr_by_chapter", cfg$chapter_facet_label, FALSE)
  })

  # ── Classification type switch ──────────────────────────────────────────────
  observeEvent(input$f_class_type, {
    cd <- comp_data()
    if (!cd$available) return()
    updateSelectizeInput(session, "f_chapter",
      choices = get_chapter_choices(cd$chapters_df, input$f_class_type),
      selected = character(0))
  }, ignoreInit = TRUE)

  observeEvent(input$f_chapter, {
    cd <- comp_data()
    if (!cd$available) return()
    ct <- input$f_class_type
    if (is.null(ct) || !has_subchapters(cd$chapters_df, ct)) return()

    new_choices <- get_subchapter_choices(cd$chapters_df, ct, input$f_chapter)
    current_sel <- input$f_subchapter
    valid_sel <- intersect(current_sel, new_choices)
    updateSelectizeInput(session, "f_subchapter",
      choices = new_choices, selected = valid_sel)
  }, ignoreInit = TRUE)

  # ── Chapter scope reactive ─────────────────────────────────────────────────
  chapter_scope <- reactive({
    cd <- comp_data()
    if (!cd$available) return(NULL)
    get_chapter_scope(cd$chapters_df, input$f_class_type,
                      input$f_chapter, input$f_subchapter)
  })

  # ── Cascade: chapter changes -> narrow attribute choices ────────────────────
  observe({
    cd <- comp_data()
    if (!cd$available) return()
    cfg <- domain_cfg()
    scope_ids <- chapter_scope()

    for (i in seq_along(cfg$attr_types)) {
      rel <- cfg$attr_types[i]
      input_id <- paste0("f_attr_", i)
      new_choices <- get_attribute_choices(cd$attrs_df, rel, scope_ids)
      current_sel <- input[[input_id]]
      valid_sel <- intersect(current_sel, new_choices)
      updateSelectizeInput(session, input_id,
        choices = new_choices, selected = valid_sel)
    }
  }) |> bindEvent(list(input$f_chapter, input$f_subchapter), ignoreInit = TRUE)

  # ── Filter counters ─────────────────────────────────────────────────────────
  output$cnt_chapter <- renderUI({
    cd <- comp_data()
    if (!cd$available) return(NULL)
    total <- length(get_chapter_choices(cd$chapters_df, input$f_class_type))
    sel <- length(input$f_chapter)
    cls <- if (sel > 0) "filter-counter has-selection" else "filter-counter"
    tags$span(class = cls, paste0(sel, " of ", total))
  })

  output$cnt_subchapter <- renderUI({
    cd <- comp_data()
    if (!cd$available) return(NULL)
    ct <- input$f_class_type
    if (is.null(ct) || !has_subchapters(cd$chapters_df, ct)) return(NULL)
    total <- length(get_subchapter_choices(cd$chapters_df, ct, input$f_chapter))
    sel <- length(input$f_subchapter)
    cls <- if (sel > 0) "filter-counter has-selection" else "filter-counter"
    tags$span(class = cls, paste0(sel, " of ", total))
  })

  # ── Ranking toggle ─────────────────────────────────────────────────────────
  rank_mode <- reactiveVal(NULL)

  observeEvent(input$rank_over, {
    rank_mode(if (identical(rank_mode(), "over")) NULL else "over")
  })
  observeEvent(input$rank_under, {
    rank_mode(if (identical(rank_mode(), "under")) NULL else "under")
  })
  observeEvent(input$rank_similar, {
    rank_mode(if (identical(rank_mode(), "similar")) NULL else "similar")
  })

  observe({
    mode <- rank_mode()
    for (btn_id in c("rank_over", "rank_under", "rank_similar")) {
      is_active <- (!is.null(mode) && (
        (btn_id == "rank_over" && mode == "over") ||
        (btn_id == "rank_under" && mode == "under") ||
        (btn_id == "rank_similar" && mode == "similar")
      ))
      session$sendCustomMessage("toggleRankBtn", list(id = btn_id, active = is_active))
    }
  })

  ranked_concept_ids <- reactive({
    mode <- rank_mode()
    if (is.null(mode)) return(NULL)

    cd <- comp_data()
    summary <- cd$meta_summary_df |>
      filter(concept_id %in% filtered_concepts(),
             !is.na(fold_diff), is.finite(fold_diff))

    min_wt <- if (!is.null(input$min_weight)) as.numeric(input$min_weight) else 0
    if (min_wt > 0) {
      heavy_ids <- cd$concept_info |>
        filter(pop_weight >= min_wt) |>
        pull(concept_id)
      summary <- summary |> filter(concept_id %in% heavy_ids)
    }

    switch(mode,
      "over"    = summary |> arrange(desc(fold_diff)) |> head(40) |> pull(concept_id),
      "under"   = summary |> arrange(fold_diff) |> head(40) |> pull(concept_id),
      "similar" = summary |> arrange(abs(log2(fold_diff))) |> head(40) |> pull(concept_id)
    )
  })

  # ── Filtered concept_ids ────────────────────────────────────────────────────
  filtered_concepts <- reactive({
    cd <- comp_data()
    if (!cd$available) return(integer(0))
    ids <- cd$compared_concept_ids

    scope_ids <- chapter_scope()
    if (!is.null(scope_ids)) ids <- intersect(ids, scope_ids)

    cfg <- domain_cfg()
    for (i in seq_along(cfg$attr_types)) {
      rel <- cfg$attr_types[i]
      selected <- input[[paste0("f_attr_", i)]]
      if (length(selected) > 0) {
        matching <- cd$attrs_df |>
          filter(relationship == rel, target_concept_id %in% as.integer(selected)) |>
          pull(concept_id) |> unique()
        ids <- intersect(ids, matching)
      }
    }
    ids
  })

  # ── Filtered yearly data ────────────────────────────────────────────────────
  filtered_yearly <- reactive({
    cd <- comp_data()
    if (!cd$available) return(tibble())
    sex_sel <- if (length(input$f_sex) > 0) input$f_sex else c("F", "M")
    age_sel <- if (length(input$f_age_group) > 0) input$f_age_group else age_group_levels

    cd$yearly_df |>
      filter(concept_id %in% filtered_concepts()) |>
      filter(sex %in% sex_sel) |>
      filter(age_group %in% age_sel) |>
      left_join(cd$concept_info |> select(concept_id, concept_name, concept_code),
                by = "concept_id") |>
      select(concept_name, concept_code, year, sex, age_group,
             patient_count_d1, patient_count_d2,
             prevalence_d1, prevalence_d2,
             log2_pr, fold_diff, fold_ci_low, fold_ci_high,
             p_value, sig) |>
      arrange(concept_name, year, sex, age_group)
  })

  # ── Filtered meta_agegroups data ────────────────────────────────────────────
  filtered_meta_ag <- reactive({
    cd <- comp_data()
    if (!cd$available) return(tibble())
    sex_sel <- if (length(input$f_sex) > 0) input$f_sex else c("F", "M")
    age_sel <- if (length(input$f_age_group) > 0) input$f_age_group else age_group_levels

    df <- cd$meta_ag_df |>
      filter(concept_id %in% filtered_concepts()) |>
      filter(sex %in% sex_sel) |>
      filter(age_group %in% age_sel) |>
      left_join(cd$concept_info, by = "concept_id")

    if (isTRUE(input$sig_filter)) df <- df |> filter(sig)

    top_ids <- ranked_concept_ids()
    if (!is.null(top_ids)) df <- df |> filter(concept_id %in% top_ids)

    df
  })

  # ── Overview tab title ──────────────────────────────────────────────────────
  output$overview_title <- renderUI({
    cd <- comp_data()
    cfg <- domain_cfg()
    if (!cd$available) return(NULL)
    tags$h4(
      style = "font-weight: 600; margin: 8px 0 4px 0; font-size: 15px;",
      HTML(paste0(
        "Prevalence ratio distribution - higher in ",
        "<span style='color:", COLOR_DS_COMP, "; font-weight: 700;'>", cd$d2_name, "</span>",
        " or ",
        "<span style='color:", COLOR_DS_REF, "; font-weight: 700;'>", cd$d1_name, "</span>"
      ))
    )
  })

  output$dataset_counts <- renderUI({
    cd <- comp_data()
    dc <- cd$domain_counts
    parts <- c()
    if (dc$condition > 0)  parts <- c(parts, paste0(format(dc$condition, big.mark = ","), " conditions"))
    if (dc$procedure > 0)  parts <- c(parts, paste0(format(dc$procedure, big.mark = ","), " procedures"))
    if (dc$drug > 0)       parts <- c(parts, paste0(format(dc$drug, big.mark = ","), " drug ingredients"))
    tags$span(class = "status-badge", paste(parts, collapse = " \u00b7 "))
  })

  # ── PR Distribution histogram ──────────────────────────────────────────────

  filtered_summary <- reactive({
    cd <- comp_data()
    if (!cd$available) return(tibble())
    cd$meta_summary_df |>
      filter(concept_id %in% filtered_concepts())
  })

  output$pr_distribution <- renderPlot({
    cd <- comp_data()
    cfg <- domain_cfg()
    validate(need(cd$available,
      paste0(cfg$label, " data not available for this comparison.")))
    data <- filtered_summary()
    validate(need(nrow(data) > 0,
      paste0("No matching ", tolower(cfg$label), ". Adjust filters.")))
    thresh <- if (!is.null(input$fold_threshold) && input$fold_threshold >= 1.05)
      input$fold_threshold else FOLD_THRESHOLD
    build_pr_distribution(data, cd$d1_name, cd$d2_name,
                          scale_mode = input$scale_mode_pr,
                          fold_thresh = thresh,
                          domain_label = tolower(cfg$label))
  }, res = 120)

  chapter_facet_data <- reactive({
    cd <- comp_data()
    cfg <- domain_cfg()
    if (!cd$available) return(list(data_ch = tibble(), n_unique = 0L, n_pairs = 0L, n_multi = 0L))
    data <- filtered_summary()
    ch_type <- input$f_class_type
    chapters <- cd$chapters_df |>
      filter(chapter_type == ch_type, chapter_level == 1L) |>
      select(concept_id, chapter_name, chapter_id) |>
      distinct()
    data_ch <- data |>
      inner_join(chapters, by = "concept_id")
    n_unique <- n_distinct(data_ch$concept_id)
    n_multi_val <- data_ch |>
      distinct(concept_id, chapter_id) |>
      count(concept_id) |>
      filter(n > 1) |>
      nrow()
    list(data_ch = data_ch, n_unique = n_unique, n_pairs = nrow(distinct(data_ch, concept_id, chapter_id)), n_multi = n_multi_val)
  })

  output$chapter_overlap_note <- renderUI({
    fd <- chapter_facet_data()
    if (fd$n_multi == 0 || fd$n_unique == 0) return(NULL)
    pct <- round(fd$n_multi / fd$n_unique * 100, 0)
    ch_type <- input$f_class_type
    reason <- if (ch_type %in% c("icd10_chapter", "atc_1st")) {
      "Multiple vocabulary mappings"
    } else {
      "SNOMED CT poly-hierarchy"
    }
    div(style = "font-size: 11px; color: #888; margin-bottom: 6px; padding: 4px 8px; background: #f8f8f6; border-radius: 3px; border: 1px solid #e8e8e4;",
      icon("info-circle", style = "margin-right: 4px;"),
      sprintf("%s: %s of %s %s (%s%%) appear in more than one chapter. Totals across facets may exceed the unique count.",
        reason,
        format(fd$n_multi, big.mark = ","),
        format(fd$n_unique, big.mark = ","),
        tolower(domain_cfg()$label),
        pct)
    )
  })

  output$pr_distribution_chapters <- renderGirafe({
    fd <- chapter_facet_data()
    cfg <- domain_cfg()
    validate(need(nrow(fd$data_ch) > 0,
      paste0("No matching ", tolower(cfg$label), " with chapter mappings.")))
    cd <- comp_data()
    thresh <- if (!is.null(input$fold_threshold) && input$fold_threshold >= 1.05)
      input$fold_threshold else FOLD_THRESHOLD
    build_pr_distribution_chapters(fd$data_ch, cd$d1_name, cd$d2_name,
                                   scale_mode = input$scale_mode_pr,
                                   fold_thresh = thresh,
                                   domain_label = tolower(cfg$label))
  })

  observeEvent(input$pr_distribution_chapters_selected, {
    sel <- input$pr_distribution_chapters_selected
    if (!is.null(sel) && length(sel) > 0 && nchar(sel) > 0) {
      cd <- comp_data()
      ch_type <- input$f_class_type
      chapter_choices <- get_chapter_choices(cd$chapters_df, ch_type)
      updateSelectizeInput(session, "f_chapter",
        choices = chapter_choices, selected = sel)
      updateTabsetPanel(session, "main_tabs", selected = "Heatmap")
    }
  }, ignoreInit = TRUE)

  # ── Concept sort order ──────────────────────────────────────────────────────
  concept_sort_order <- reactive({
    cd <- comp_data()
    if (!cd$available) return(NULL)
    mode <- input$heatmap_sort %||% "fold_dir"
    summary <- cd$meta_summary_df |>
      filter(concept_id %in% filtered_concepts()) |>
      left_join(cd$concept_info |> select(concept_id, concept_name, pop_weight),
                by = "concept_id")

    ordered <- switch(mode,
      "fold_dir" = summary |> arrange(desc(log2_pr)),
      "fold_abs" = summary |> arrange(desc(abs(log2_pr))),
      "weight"   = summary |> arrange(desc(pop_weight)),
      "alpha"    = summary |> arrange(concept_name),
      summary |> arrange(desc(log2_pr))
    )
    ordered$concept_id
  })

  # ── Heatmap ─────────────────────────────────────────────────────────────────
  output$heatmap <- renderGirafe({
    cd <- comp_data()
    cfg <- domain_cfg()
    data <- filtered_meta_ag()
    n_concepts <- length(unique(data$concept_id))
    max_concepts <- input$heatmap_max %||% HEATMAP_MAX_CONCEPTS

    validate(
      need(cd$available,
        paste0(cfg$label, " data not available for this comparison.")),
      need(nrow(data) > 0,
        paste0("No matching ", tolower(cfg$label), ". Adjust filters.")),
      need(n_concepts <= max_concepts,
           paste0("\n\nToo many ", tolower(cfg$label), " (", n_concepts,
                  ") for heatmap display. ",
                  "Select a chapter or filter to narrow to \u2264 ",
                  max_concepts, ", or increase the Max concepts limit.\n"))
    )

    build_heatmap(data, cd$d1_name, cd$d2_name, scale_mode = input$scale_mode,
                  concept_weights = cd$concept_info,
                  concept_order = concept_sort_order())
  })

  # ── Forest overview ─────────────────────────────────────────────────────────
  output$forest_overview <- renderGirafe({
    cd <- comp_data()
    data <- filtered_meta_ag()
    concept_ids <- unique(data$concept_id)
    n_concepts <- length(concept_ids)
    sex_sel <- input$forest_sex_filter

    validate(
      need(n_concepts > 0, ""),
      need(n_concepts <= (input$heatmap_max %||% HEATMAP_MAX_CONCEPTS), ""),
      need(length(sex_sel) > 0, "Select at least one sex category to display the forest plot.")
    )

    meta_sex_filtered <- cd$meta_by_sex_df
    if (!("Female" %in% sex_sel)) {
      meta_sex_filtered <- meta_sex_filtered |> filter(sex != "F")
    }
    if (!("Male" %in% sex_sel)) {
      meta_sex_filtered <- meta_sex_filtered |> filter(sex != "M")
    }

    meta_summary_filtered <- if ("Both" %in% sex_sel) {
      cd$meta_summary_df
    } else {
      cd$meta_summary_df |> filter(FALSE)
    }

    build_forest_overview(meta_sex_filtered, meta_summary_filtered,
                          concept_ids, cd$concept_info,
                          cd$d1_name, cd$d2_name,
                          scale_mode = input$scale_mode,
                          concept_order = concept_sort_order())
  })

  # ── Concept selection ───────────────────────────────────────────────────────
  selected_concept_id <- reactiveVal(NULL)

  observeEvent(input$heatmap_selected, {
    sel <- input$heatmap_selected
    if (!is.null(sel) && length(sel) > 0) {
      cid <- as.integer(sel)
      selected_concept_id(cid)
      updateSelectizeInput(session, "concept_search", selected = as.character(cid))
      updateTabsetPanel(session, "main_tabs", selected = "Detail")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$forest_overview_selected, {
    sel <- input$forest_overview_selected
    if (!is.null(sel) && length(sel) > 0) {
      cid <- as.integer(sel)
      selected_concept_id(cid)
      updateSelectizeInput(session, "concept_search", selected = as.character(cid))
      updateTabsetPanel(session, "main_tabs", selected = "Detail")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$concept_search, {
    sel <- input$concept_search
    if (!is.null(sel) && nchar(sel) > 0) {
      selected_concept_id(as.integer(sel))
    } else {
      selected_concept_id(NULL)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$detail_panel <- renderUI({
    cid <- selected_concept_id()
    if (is.null(cid)) {
      return(div(class = "detail-hint",
                 "Select a concept above, or click one on the Heatmap tab."))
    }

    cd <- comp_data()
    cinfo <- cd$concept_info |> filter(concept_id == cid)
    if (nrow(cinfo) == 0) return(NULL)

    icd_row <- if (active_domain() == "condition" && nrow(icd10_lookup) > 0) {
      codes <- icd10_lookup |>
        filter(concept_id == cid) |>
        arrange(icd10_code)
      if (nrow(codes) > 0) {
        code_tags <- lapply(seq_len(nrow(codes)), function(i) {
          tags$code(title = codes$icd10_name[i], codes$icd10_code[i])
        })
        div(class = "concept-card-icd",
          tags$span(class = "icd-label", "ICD-10: "),
          code_tags
        )
      }
    }

    div(class = "detail-section",
      div(class = "concept-card",
        tags$span(class = "concept-card-name", cinfo$concept_name),
        tags$span(class = "concept-card-code",
          paste0("SNOMED ", cinfo$concept_code)),
        icd_row
      ),
      div(class = "detail-header",
        tags$span(class = "detail-title", "Per-year detail")
      ),
      withSpinner(
        girafeOutput("forest_detail", width = "100%", height = "auto"),
        color = COLOR_SPINNER, type = 7, size = 0.6
      ),
      div(class = "detail-header", style = "margin-top: 12px;",
        tags$span(class = "detail-title", "Absolute prevalence")
      ),
      withSpinner(
        girafeOutput("point_diff_detail", width = "100%", height = "auto"),
        color = COLOR_SPINNER, type = 7, size = 0.6
      )
    )
  })

  output$forest_detail <- renderGirafe({
    cid <- selected_concept_id()
    req(cid)
    cd <- comp_data()
    req(cd$available)

    cinfo <- cd$concept_info |> filter(concept_id == cid)
    req(nrow(cinfo) > 0)

    sex_sel <- if (length(input$f_sex) > 0) input$f_sex else c("F", "M")
    age_sel <- if (length(input$f_age_group) > 0) input$f_age_group else age_group_levels

    yearly_detail <- cd$yearly_df |>
      filter(concept_id == cid, sex %in% sex_sel, age_group %in% age_sel)

    meta_detail <- cd$meta_ag_df |>
      filter(concept_id == cid, sex %in% sex_sel, age_group %in% age_sel)

    if (isTRUE(input$sig_filter)) {
      yearly_detail <- yearly_detail |> filter(sig)
      meta_detail   <- meta_detail |> filter(sig)
    }

    validate(need(nrow(yearly_detail) > 0,
                  "No yearly data for this concept with current filters."))

    build_forest_detail(
      yearly_detail, meta_detail,
      cinfo$concept_name, cinfo$concept_code,
      cd$d1_name, cd$d2_name,
      scale_mode = input$scale_mode
    )
  })

  output$point_diff_detail <- renderGirafe({
    cid <- selected_concept_id()
    req(cid)
    cd <- comp_data()
    req(cd$available)

    cinfo <- cd$concept_info |> filter(concept_id == cid)
    req(nrow(cinfo) > 0)

    sex_sel <- if (length(input$f_sex) > 0) input$f_sex else c("F", "M")
    age_sel <- if (length(input$f_age_group) > 0) input$f_age_group else age_group_levels

    yearly_detail <- cd$yearly_df |>
      filter(concept_id == cid, sex %in% sex_sel, age_group %in% age_sel)

    validate(need(nrow(yearly_detail) > 0,
                  "No yearly data for this concept with current filters."))

    build_point_diff(yearly_detail, cinfo$concept_name, cinfo$concept_code,
                     cd$d1_name, cd$d2_name)
  })

  # ── Summary by Sex table ────────────────────────────────────────────────────
  output$summary_table <- DT::renderDataTable({
    cd <- comp_data()
    validate(need(cd$available, "Data not available for this domain."))
    concept_ids <- filtered_concepts()
    info <- cd$concept_info |> select(concept_id, concept_name, concept_code)

    both <- cd$meta_summary_df |>
      filter(concept_id %in% concept_ids) |>
      mutate(sex = "Both")
    by_sex <- cd$meta_by_sex_df |>
      filter(concept_id %in% concept_ids) |>
      mutate(sex = ifelse(sex == "F", "Female", "Male"))

    df <- bind_rows(both, by_sex) |>
      left_join(info, by = "concept_id") |>
      mutate(sex = factor(sex, levels = c("Both", "Female", "Male"))) |>
      select(concept_name, concept_code, sex,
             prevalence_d1, prevalence_d2,
             log2_pr, fold_diff, fold_ci_low, fold_ci_high,
             p_value) |>
      arrange(concept_name, sex)

    col_labels <- c(
      "Concept", "Code", "Sex",
      paste0("Prev. (", cd$d1_name, ")"),
      paste0("Prev. (", cd$d2_name, ")"),
      "log\u2082 PR", "Fold diff", "Fold CI low", "Fold CI high",
      "p-value"
    )

    hidden_cols <- which(names(df) %in% c("concept_code", "fold_ci_low", "fold_ci_high")) - 1

    DT::datatable(
      df,
      rownames = FALSE,
      colnames = col_labels,
      filter = "top",
      extensions = c("Buttons"),
      options = list(
        pageLength = 30,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list("colvis", "csv", "excel"),
        columnDefs = list(
          list(visible = FALSE, targets = hidden_cols)
        ),
        order = list(list(5, "desc"))
      )
    ) |>
      DT::formatRound(c("prevalence_d1", "prevalence_d2"), digits = 4) |>
      DT::formatRound(c("log2_pr", "fold_diff", "fold_ci_low", "fold_ci_high"), digits = 2) |>
      DT::formatSignif("p_value", digits = 3) |>
      DT::formatStyle("log2_pr",
        backgroundColor = DT::styleInterval(0, c(COLOR_DS_REF_ALPHA, COLOR_DS_COMP_ALPHA))
      )
  })

  # ── Results table (stratified yearly rows) ──────────────────────────────────
  output$results_table <- DT::renderDataTable({
    cd <- comp_data()
    validate(need(cd$available, "Data not available for this domain."))
    df <- filtered_yearly()

    col_names <- names(df)
    hidden_cols <- which(col_names %in% c("concept_code", "fold_ci_low", "fold_ci_high")) - 1

    col_labels <- c(
      "Concept", "Code", "Year", "Sex", "Age group",
      paste0("Patients (", cd$d1_name, ")"),
      paste0("Patients (", cd$d2_name, ")"),
      paste0("Prev. (", cd$d1_name, ")"),
      paste0("Prev. (", cd$d2_name, ")"),
      "log\u2082 PR", "Fold diff", "Fold CI low", "Fold CI high",
      "p-value", "Sig."
    )

    DT::datatable(
      df,
      rownames = FALSE,
      colnames = col_labels,
      filter = "top",
      extensions = c("Buttons"),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list("colvis", "csv", "excel"),
        columnDefs = list(
          list(visible = FALSE, targets = hidden_cols)
        )
      )
    ) |>
      DT::formatRound(c("prevalence_d1", "prevalence_d2"), digits = 4) |>
      DT::formatRound(c("log2_pr", "fold_diff", "fold_ci_low", "fold_ci_high"), digits = 2) |>
      DT::formatSignif("p_value", digits = 3) |>
      DT::formatStyle("log2_pr",
        backgroundColor = DT::styleInterval(0, c(COLOR_DS_REF_ALPHA, COLOR_DS_COMP_ALPHA))
      ) |>
      DT::formatStyle("sig",
        fontWeight = DT::styleEqual(c(TRUE, FALSE), c("bold", "normal")),
        color = DT::styleEqual(c(TRUE, FALSE), c(COLOR_INK, COLOR_INK_MUTED))
      )
  })

  # ── Counts per Concept table ────────────────────────────────────────────────
  output$counts_table <- DT::renderDataTable({
    cd <- comp_data()
    validate(need(cd$available, "Data not available for this domain."))
    df <- cd$counts_df |>
      filter(concept_id %in% filtered_concepts()) |>
      select(concept_code, concept_name, patients_d1, patients_d2, pct_d1, pct_d2)

    col_labels <- c(
      "Code", "Concept",
      paste0("Patients (", cd$d1_name, ")"),
      paste0("Patients (", cd$d2_name, ")"),
      paste0("% (", cd$d1_name, ")"),
      paste0("% (", cd$d2_name, ")")
    )

    DT::datatable(
      df,
      rownames = FALSE,
      colnames = col_labels,
      filter = "top",
      extensions = c("Buttons"),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list("csv", "excel"),
        order = list(list(2, "desc"))
      )
    ) |>
      DT::formatRound(c("pct_d1", "pct_d2"), digits = 2) |>
      DT::formatCurrency(c("patients_d1", "patients_d2"),
                          currency = "", interval = 3, mark = ",", digits = 0)
  })

  # ── ICD-10 Lookup table ───────────────────────────────────────────────────
  output$icd10_table <- DT::renderDataTable({
    cd <- comp_data()
    validate(need(cd$available, "Data not available for this domain."))
    validate(need(nrow(icd10_lookup) > 0,
      "ICD-10 lookup not available. Run: Rscript scripts/build_icd10_lookup.R"))

    df <- icd10_lookup |>
      filter(concept_id %in% cd$compared_concept_ids) |>
      left_join(
        cd$concept_info |> select(concept_id, concept_name, concept_code),
        by = "concept_id"
      ) |>
      select(concept_name, concept_code, icd10_code, icd10_name) |>
      arrange(concept_name, icd10_code)

    DT::datatable(
      df,
      rownames = FALSE,
      colnames = c("Concept", "SNOMED code", "ICD-10 code", "ICD-10 name"),
      filter = "top",
      extensions = c("Buttons"),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list("csv", "excel")
      )
    )
  })

  # ── Concept Lookup table ────────────────────────────────────────────────────
  output$concept_lookup_table <- DT::renderDataTable({
    cd <- comp_data()
    cfg <- DOMAIN_CONFIG[[cd$domain]]
    validate(need(cd$available, "Data not available for this domain."))

    base <- cd$concept_info |>
      filter(concept_id %in% cd$compared_concept_ids) |>
      select(concept_id, concept_name, concept_code)

    if (nrow(cd$chapters_df) > 0) {
      chap_wide <- cd$chapters_df |>
        filter(chapter_level == 1) |>
        group_by(concept_id, chapter_type) |>
        summarise(labels = paste(sort(unique(chapter_name)), collapse = "; "),
                  .groups = "drop") |>
        pivot_wider(names_from = chapter_type, values_from = labels, values_fill = "")
      base <- base |> left_join(chap_wide, by = "concept_id")

      l2 <- cd$chapters_df |> filter(chapter_level == 2)
      if (nrow(l2) > 0) {
        sub_wide <- l2 |>
          mutate(col_key = paste0(chapter_type, "_sub")) |>
          group_by(concept_id, col_key) |>
          summarise(labels = paste(sort(unique(chapter_name)), collapse = "; "),
                    .groups = "drop") |>
          pivot_wider(names_from = col_key, values_from = labels, values_fill = "")
        base <- base |> left_join(sub_wide, by = "concept_id")
      }
    }

    if (nrow(cd$attrs_df) > 0) {
      attr_wide <- cd$attrs_df |>
        group_by(concept_id, relationship) |>
        summarise(labels = paste(sort(unique(target_concept_name)), collapse = "; "),
                  .groups = "drop") |>
        pivot_wider(names_from = relationship, values_from = labels, values_fill = "")
      base <- base |> left_join(attr_wide, by = "concept_id")
    }

    base[is.na(base)] <- ""

    col_renames <- c(
      "concept_name" = "Concept",
      "concept_code" = "Code"
    )
    chap_lookup <- setNames(names(cfg$class_types), unname(cfg$class_types))
    sub_lookup <- setNames(
      paste(names(cfg$class_types), "sub-ch."),
      paste0(unname(cfg$class_types), "_sub")
    )
    attr_lookup <- setNames(unlist(cfg$attr_labels), names(cfg$attr_labels))
    all_renames <- c(col_renames, chap_lookup, sub_lookup, attr_lookup)

    current_names <- names(base)
    new_names <- current_names
    for (i in seq_along(current_names)) {
      if (current_names[i] %in% names(all_renames)) {
        new_names[i] <- all_renames[[current_names[i]]]
      }
    }

    ct_vals <- unname(cfg$class_types)
    chap_col_order <- unlist(lapply(ct_vals, function(ct) {
      cols <- c(ct, paste0(ct, "_sub"))
      cols[cols %in% current_names]
    }))
    attr_cols <- intersect(names(cfg$attr_labels), current_names)
    col_order <- c("concept_id", "concept_name", "concept_code",
                   chap_col_order, attr_cols)
    col_order <- c(col_order, setdiff(current_names, col_order))
    base <- base[, col_order]
    current_names <- names(base)
    new_names <- current_names
    for (i in seq_along(current_names)) {
      if (current_names[i] %in% names(all_renames)) {
        new_names[i] <- all_renames[[current_names[i]]]
      }
    }

    display_df <- base |> select(-concept_id)
    display_names <- new_names[current_names != "concept_id"]

    DT::datatable(
      display_df,
      rownames = FALSE,
      colnames = display_names,
      filter = "top",
      extensions = c("Buttons"),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list("csv", "excel")
      )
    )
  })

  # ── Demography: population pyramids ─────────────────────────────────────────
  output$pyramid_d1 <- renderGirafe({
    cd <- comp_data()
    build_pyramid(cd$d1$demographics, cd$d1_name)
  })

  output$pyramid_d2 <- renderGirafe({
    cd <- comp_data()
    build_pyramid(cd$d2$demographics, cd$d2_name)
  })

  # ── About modal ────────────────────────────────────────────────────────────
  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = "About Syrona",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$script(HTML("
        var content = document.getElementById('about_content');
        if (content) {
          var modal = document.querySelector('.modal-body');
          if (modal) modal.innerHTML = content.innerHTML;
        }
      "))
    ))
  })

  # Suspend rendering when not visible
  outputOptions(output, "pr_distribution", suspendWhenHidden = TRUE)
  outputOptions(output, "pr_distribution_chapters", suspendWhenHidden = TRUE)
  outputOptions(output, "heatmap", suspendWhenHidden = TRUE)
  outputOptions(output, "summary_table", suspendWhenHidden = TRUE)
  outputOptions(output, "results_table", suspendWhenHidden = TRUE)
  outputOptions(output, "counts_table", suspendWhenHidden = TRUE)
  outputOptions(output, "icd10_table", suspendWhenHidden = TRUE)
  outputOptions(output, "pyramid_d1", suspendWhenHidden = TRUE)
  outputOptions(output, "pyramid_d2", suspendWhenHidden = TRUE)
}
