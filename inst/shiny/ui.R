# ── Syrona Dashboard - UI ─────────────────────────────────────────────────────

fluidPage(
  tags$head(tags$style(HTML(sprintf("
    body { background-color: %s; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; color: %s; }
    .header-bar { background: %s; padding: 16px 15px; margin: -15px -15px 20px -15px; border-bottom: 1px solid %s; }
    .header-bar h2 { margin: 0 0 2px 0; font-size: 22px; font-weight: 600; color: %s; }
    .header-bar .subtitle { font-size: 14px; color: %s; display: flex; align-items: center; gap: 8px; flex-wrap: wrap; }
    .header-bar .subtitle .ds1 { color: %s; font-weight: 600; }
    .header-bar .subtitle .ds2 { color: %s; font-weight: 600; }
    .header-bar .subtitle .form-group { margin-bottom: 0; }
    .header-bar .subtitle .selectize-control { vertical-align: middle; }
    .header-bar .subtitle .selectize-input { font-size: 13px; padding: 4px 10px; min-height: 0; }
    .status-badge { display: inline-block; background: %s; border: 1px solid %s;
                    padding: 3px 10px; border-radius: 3px; font-size: 12px; color: %s;
                    margin-left: 12px; vertical-align: middle; }
    .filter-row { margin-top: 10px; margin-bottom: 8px; display: flex; gap: 16px; flex-wrap: wrap; align-items: flex-start; }
    .filter-row .form-group { margin-bottom: 6px; }
    .filter-row label { font-size: 11px; color: %s; font-weight: 500; text-transform: uppercase; letter-spacing: 0.05em; }
    .filter-group { display: flex; gap: 10px; align-items: flex-start; flex-wrap: wrap; padding: 8px 12px 4px 12px; background: rgba(0,0,0,0.018); border-radius: 4px; position: relative; }
    .filter-group-label { position: absolute; top: -7px; left: 10px; font-size: 9px; color: %s; font-weight: 500; text-transform: uppercase; letter-spacing: 0.06em; background: %s; padding: 0 4px; }
    .filter-group .form-group { margin-bottom: 4px; }
    .filter-group .selectize-control { min-width: 160px; }
    .filter-group-class .selectize-control:first-of-type { min-width: 140px; }
    .filter-group-attr { flex: 1; min-width: 0; }
    .selectize-input { font-size: 13px; }
    hr.divider { border-top: 1px solid %s; margin: 12px 0 16px 0; }
    .scale-toggle { display: inline-block; }
    .scale-toggle .form-group { margin-bottom: 0; }
    .scale-toggle .radio-inline { font-size: 12px; }
    #sig_filter { margin: 0; }
    #sig_filter + label { margin-bottom: 0; }

    /* Tab group styling */
    #main_tabs > .nav-tabs > li:nth-child(4) { margin-right: 24px; }
    #main_tabs > .nav-tabs > li:nth-child(7) { margin-right: 24px; }
    #main_tabs > .nav-tabs > li:not(.active):nth-child(n+5) > a { color: %s; }

    /* Classification type selector */
    .class-type-select .form-group { margin-bottom: 0; }
    .class-type-select .selectize-input { font-size: 12px; }

    /* Strata bar */
    .strata-toggle { font-size: 12px; color: %s; text-decoration: none; cursor: pointer; display: inline-block; margin-bottom: 6px; }
    .strata-toggle:hover { color: %s; text-decoration: none; }
    .strata-bar { display: flex; align-items: center; gap: 16px; flex-wrap: wrap; margin-bottom: 4px; }
    .strata-bar .form-group { margin-bottom: 0; }
    .strata-bar .checkbox-inline { font-size: 12px; }
    .strata-bar label { font-size: 11px; color: %s; font-weight: 500; text-transform: uppercase; letter-spacing: 0.05em; }

    /* Info icon */
    .filter-info {
      display: inline-block; cursor: help; color: #aaa;
      font-size: 10px; border: 1px solid #ccc; border-radius: 50%%;
      width: 13px; height: 13px; line-height: 13px;
      text-align: center; margin-left: 4px; vertical-align: middle;
      user-select: none; position: relative;
    }
    .filter-info::after {
      content: attr(data-tooltip);
      position: absolute; left: 0; top: 18px;
      background: #333; color: #fff;
      padding: 5px 8px; border-radius: 4px;
      font-size: 11px; line-height: 1.4;
      white-space: normal; width: 220px;
      z-index: 9999; display: none;
      text-transform: none; letter-spacing: normal; font-weight: normal;
      pointer-events: none;
    }
    .filter-info:hover::after { display: block; }

    /* Counter badge */
    .filter-counter {
      font-size: 10px; color: %s; font-weight: 400;
      letter-spacing: 0; text-transform: none;
    }
    .filter-counter.has-selection { color: %s; font-weight: 600; }
  ",
    COLOR_BG, COLOR_INK,
    COLOR_SURFACE, COLOR_BORDER,
    COLOR_INK,
    COLOR_INK_MUTED,
    COLOR_DS_REF, COLOR_DS_COMP,
    COLOR_BG, COLOR_BORDER, COLOR_INK_MUTED,
    COLOR_INK_SUBTLE,
    COLOR_INK_SUBTLE, COLOR_BG,
    COLOR_BORDER,
    COLOR_INK_SUBTLE,
    COLOR_INK_SUBTLE, COLOR_INK,
    COLOR_INK_SUBTLE,
    COLOR_INK_SUBTLE,
    COLOR_DS_REF
  )))),

  # JS handler for ranking button toggle
  tags$script(HTML("
    Shiny.addCustomMessageHandler('toggleRankBtn', function(msg) {
      var btn = document.getElementById(msg.id);
      if (btn) {
        if (msg.active) { btn.classList.add('active'); }
        else { btn.classList.remove('active'); }
      }
    });
  ")),

  # Detail panel + validation styles + ranking buttons
  tags$style(HTML("
    .detail-section { margin-top: 16px; }
    .detail-header { margin-bottom: 8px; }
    .detail-title { font-size: 11px; color: #767676; font-weight: 500;
                    text-transform: uppercase; letter-spacing: 0.05em; }
    .detail-hint { font-size: 12px; color: #767676; font-style: italic; margin-top: 8px; }
    .concept-card { background: #fff; border: 1px solid #E2E2DE; border-radius: 6px;
                    padding: 10px 14px; margin-bottom: 14px; }
    .concept-card .concept-card-name { font-size: 15px; font-weight: 600; color: #1A1A1A; }
    .concept-card .concept-card-code { font-size: 12px; color: #767676; margin-left: 6px; }
    .concept-card .concept-card-icd { font-size: 12px; color: #555; margin-top: 4px; }
    .concept-card .concept-card-icd .icd-label { color: #767676; font-weight: 500; }
    .concept-card .concept-card-icd code { background: #F0F0F0; padding: 1px 5px;
                    border-radius: 3px; font-size: 11px; margin-right: 3px; }
    .shiny-output-error-validation { padding: 12px 0; line-height: 1.6; white-space: pre-line; }

    /* Ranking toggle buttons */
    .rank-buttons { display: inline-flex; gap: 6px; align-items: center; }
    .rank-buttons .btn-rank {
      font-size: 11px; padding: 3px 10px; border-radius: 3px;
      border: 1px solid #ccc; background: #fff; color: #555;
      cursor: pointer; transition: all 0.15s;
    }
    .rank-buttons .btn-rank:hover { border-color: #999; color: #333; }
    .rank-buttons .btn-rank.active { background: #333; color: #fff; border-color: #333; }
    .rank-buttons .rank-label {
      font-size: 11px; color: #999; font-weight: 500;
      text-transform: uppercase; letter-spacing: 0.05em; margin-right: 2px;
    }
  ")),

  # Domain selector styles
  tags$style(HTML("
    .domain-selector { display: flex; flex-direction: column; gap: 4px; align-self: stretch; }
    .domain-selector .btn-domain {
      font-size: 11px; padding: 3px 10px; border-radius: 3px;
      border: 1px solid #ccc; background: #fff; color: #555;
      cursor: pointer; transition: all 0.15s;
    }
    .domain-selector .btn-domain:hover { border-color: #999; color: #333; }
    .domain-selector .btn-domain.active { background: #333; color: #fff; border-color: #333; }
    .domain-selector .btn-domain:disabled { opacity: 0.35; cursor: not-allowed; }
    .btn-about {
      font-size: 11px; padding: 3px 10px; border-radius: 3px;
      border: 1px solid #ccc; background: #fff; color: #767676;
      cursor: pointer; transition: all 0.15s;
    }
    .btn-about:hover { border-color: #999; color: #333; }
  ")),

  # JS handler for domain button toggle
  tags$script(HTML("
    Shiny.addCustomMessageHandler('toggleDomainBtn', function(msg) {
      var btn = document.getElementById(msg.id);
      if (btn) {
        if (msg.active) { btn.classList.add('active'); }
        else { btn.classList.remove('active'); }
        if (msg.disabled !== undefined) { btn.disabled = msg.disabled; }
      }
    });
  ")),

  # Header
  div(class = "header-bar",
    div(style = "display: flex; align-items: flex-start; justify-content: space-between;",
      h2("Syrona"),
      actionButton("about_btn", tagList(icon("info-circle"), "About"), class = "btn-about")
    ),
    div(class = "subtitle",
      selectInput("comparison_select", NULL,
        choices = comparison_choices,
        selected = comparison_choices[1],
        width = "360px"),
      uiOutput("comparison_label", inline = TRUE),
      uiOutput("dataset_counts", inline = TRUE)
    )
  ),

  # Filter Row: Domain selector + Classification group + Attribute group
  div(class = "filter-row",
    div(class = "domain-selector",
      actionButton("domain_condition", "Conditions", class = "btn-domain active"),
      actionButton("domain_procedure", "Procedures", class = "btn-domain"),
      actionButton("domain_drug", "Drugs", class = "btn-domain")
    ),
    div(class = "filter-group filter-group-class",
      tags$span(class = "filter-group-label", "Classification"),
      div(class = "class-type-select",
        selectInput("f_class_type",
          label = tags$span("Group by ", info_icon("Classification system used to group concepts into chapters. Different systems reveal different clinical perspectives on the same data.")),
          choices = DEFAULT_CLASS_TYPES,
          selected = names(DEFAULT_CLASS_TYPES)[1],
          width = "100%")
      ),
      div(
        selectizeInput("f_chapter",
          label = filter_label("Chapter", DOMAIN_CONFIG[[DEFAULT_DOMAIN]]$chapter_tip, "cnt_chapter"),
          choices = NULL,
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Any"))
      ),
      uiOutput("subchapter_slot")
    ),
    uiOutput("attr_group")
  ),

  # Strata bar
  conditionalPanel(
    condition = "input.main_tabs == 'Heatmap' || input.main_tabs == 'Detail' || input.main_tabs == 'Full Breakdown'",
    tags$span(style = "display: inline-flex; align-items: center; gap: 4px;",
      tags$a(class = "strata-toggle", href = "#", onclick = "
        var bar = document.getElementById('strata_bar');
        if (bar.style.display === 'none') { bar.style.display = 'flex'; this.textContent = '\u25BC Sex & Age group'; }
        else { bar.style.display = 'none'; this.textContent = '\u25B6 Sex & Age group'; }
        return false;
      ", "\u25B6 Sex & Age group"),
      info_icon("Filter by sex and age group. Deselecting a subgroup removes it from the meta-analysis, not just from display.")
    ),
    div(id = "strata_bar", class = "strata-bar", style = "display: none;",
      checkboxGroupInput("f_sex", NULL,
        choices = c("Female" = "F", "Male" = "M"),
        selected = c("F", "M"), inline = TRUE),
      div(style = "flex: 1; min-width: 200px;",
        selectizeInput("f_age_group", NULL,
          choices = age_group_levels,
          selected = age_group_levels,
          multiple = TRUE,
          width = "100%",
          options = list(plugins = list("remove_button"), placeholder = "All age groups"))
      )
    )
  ),

  tags$hr(class = "divider"),

  # Tabs
  tabsetPanel(id = "main_tabs", type = "tabs",
    tabPanel("Overview", icon = icon("chart-bar"),
      uiOutput("overview_title"),
      div(class = "scale-toggle", style = "display: flex; align-items: center; gap: 16px;",
        radioButtons("scale_mode_pr", NULL,
          choices = c("Fold Difference" = "fold", "log\u2082 PR" = "log2"),
          selected = "fold", inline = TRUE),
        info_icon("Fold Difference: ratio of prevalences (1 = equal, 2 = twice as common). log\u2082 PR: log-scaled ratio (0 = equal, +1 = 2\u00d7 higher, -1 = 2\u00d7 lower). Same data, different scale."),
        div(style = "display: flex; align-items: center; gap: 6px;",
          tags$label("Threshold:", style = "font-size: 12px; color: #767676; margin: 0;"),
          numericInput("fold_threshold", NULL, value = 1.3,
                       min = 1.05, max = 4, step = 0.1, width = "70px"),
          info_icon("Clinical relevance boundary. Concepts beyond this fold difference are highlighted with dotted reference lines on plots. Default 1.3\u00d7.")
        ),
        uiOutput("pr_chapter_toggle", inline = TRUE)
      ),
      conditionalPanel("!input.pr_by_chapter",
        plotOutput("pr_distribution", height = "480px")
      ),
      conditionalPanel("input.pr_by_chapter",
        div(style = "font-size: 11px; color: #999; margin-bottom: 4px; font-style: italic;",
          "Click a chapter to open it in the Heatmap tab."),
        uiOutput("chapter_overlap_note"),
        girafeOutput("pr_distribution_chapters", width = "100%", height = "auto")
      )
    ),
    tabPanel("Heatmap", icon = icon("th"),
      div(style = "display: flex; align-items: center; gap: 16px; margin-bottom: 8px;",
        div(class = "scale-toggle", style = "margin-bottom: 0;",
          radioButtons("scale_mode", NULL,
            choices = c("Fold Difference" = "fold", "log\u2082 PR" = "log2"),
            selected = "fold", inline = TRUE),
          info_icon("Fold Difference: ratio of prevalences (1 = equal, 2 = twice as common). log\u2082 PR: log-scaled ratio (0 = equal, +1 = 2\u00d7 higher, -1 = 2\u00d7 lower). Same data, different scale.")
        ),
        div(style = "display: flex; align-items: center; gap: 6px; margin-bottom: 0;",
          span(style = "font-size: 11px; color: #999; white-space: nowrap;", "Sort:"),
          selectInput("heatmap_sort", NULL,
            choices = c("Fold direction" = "fold_dir",
                        "Fold (extreme first)" = "fold_abs",
                        "Population weight" = "weight",
                        "Alphabetical" = "alpha"),
            selected = "fold_dir", width = "150px"),
          info_icon("Fold direction: overrepresented first, then underrepresented. Extreme first: largest fold difference regardless of direction. Population weight: most common concepts first. Alphabetical: by concept name.")
        ),
        div(style = "display: flex; align-items: center; gap: 6px; margin-bottom: 0;",
          span(style = "font-size: 11px; color: #999; white-space: nowrap;", "Max concepts:"),
          numericInput("heatmap_max", NULL, value = 150, min = 10, max = 500, step = 10,
                       width = "70px"),
          info_icon("Maximum number of concepts shown on the heatmap. Increase for completeness, decrease for readability and performance.")
        ),
        div(style = "flex: 1;"),
        div(style = "display: flex; align-items: center; gap: 6px; margin-bottom: 0;",
          span(style = "font-size: 11px; color: #999; white-space: nowrap;", "Min weight:"),
          selectInput("min_weight", NULL,
            choices = c("Off" = "0", "0.01%" = "0.01", "0.1%" = "0.1",
                        "0.5%" = "0.5", "1%" = "1", "5%" = "5"),
            selected = "0", width = "80px"),
          span(class = "filter-info", "i",
               `data-tooltip` = "Minimum population weight: max % of persons with this concept across both datasets. Filters rare concepts from ranking.")
        ),
        div(class = "rank-buttons", style = "margin-bottom: 0;",
          span(class = "rank-label", "Show top 40:"),
          actionButton("rank_over", "Overrepresented", class = "btn-rank"),
          actionButton("rank_under", "Underrepresented", class = "btn-rank"),
          actionButton("rank_similar", "Most similar", class = "btn-rank"),
          info_icon("Quick-select the 40 most extreme concepts. Overrepresented: highest fold in comparison dataset. Underrepresented: highest fold in reference dataset. Most similar: closest to equal prevalence.")
        )
      ),
      div(style = "text-align: right; font-size: 10px; color: #999; margin-bottom: 4px;",
        HTML(paste0(
          "<span style='color:#D8D8D8;'>\u25A0</span> &lt;0.1% &nbsp; ",
          "<span style='color:#8B8B8B;'>\u25A0</span> ~1% &nbsp; ",
          "<span style='color:#2D2D2D;'>\u25A0</span> &gt;10% &mdash; ",
          "population weight (max % of persons across datasets)"
        ))
      ),
      withSpinner(
        girafeOutput("heatmap", width = "100%", height = "auto"),
        color = COLOR_SPINNER, type = 7, size = 0.6
      ),
      div(style = "margin-top: 16px;",
        div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 6px;",
          span(style = "font-size: 11px; color: #999;", "Forest plot sex filter:"),
          checkboxGroupInput("forest_sex_filter", NULL,
            choices = c("Female", "Male", "Both"),
            selected = c("Female", "Male", "Both"),
            inline = TRUE)
        ),
        girafeOutput("forest_overview", width = "100%", height = "auto")
      )
    ),
    tabPanel("Detail", icon = icon("search-plus"),
      div(style = "margin-bottom: 12px; max-width: 400px;",
        selectizeInput("concept_search", NULL,
          choices = NULL, multiple = FALSE,
          options = list(placeholder = "Search by name or code...",
                         allowEmptyOption = TRUE))
      ),
      uiOutput("detail_panel")
    ),
    tabPanel("Population", icon = icon("users"),
      fluidRow(
        column(6, girafeOutput("pyramid_d1", width = "100%", height = "auto")),
        column(6, girafeOutput("pyramid_d2", width = "100%", height = "auto"))
      )
    ),
    tabPanel("Summary by Sex", icon = icon("table"),
      DT::dataTableOutput("summary_table")
    ),
    tabPanel("Full Breakdown", icon = icon("table"),
      DT::dataTableOutput("results_table")
    ),
    tabPanel("Patient Counts", icon = icon("table"),
      DT::dataTableOutput("counts_table")
    ),
    tabPanel("ICD-10 Codes", icon = icon("book"),
      DT::dataTableOutput("icd10_table")
    ),
    tabPanel("Concept Index", icon = icon("book"),
      div(style = "font-size: 12px; color: #666; margin-bottom: 8px; font-style: italic;",
        "One row per concept with all chapter assignments and attribute labels. Use column filters to search."
      ),
      DT::dataTableOutput("concept_lookup_table")
    )
  ),

  # About content (rendered in modal via server)
  div(id = "about_content", style = "display: none;",
      div(class = "about-tab",
        tags$style(HTML("
          .about-tab { max-width: 860px; line-height: 1.7; font-size: 14px; color: #333; }
          .about-tab h3 { margin-top: 28px; margin-bottom: 8px; font-size: 17px; font-weight: 600; color: #1A1A1A; }
          .about-tab h4 { margin-top: 20px; margin-bottom: 6px; font-size: 14px; font-weight: 600; color: #444; }
          .about-tab p { margin-bottom: 10px; }
          .about-tab ul { margin-bottom: 12px; padding-left: 20px; }
          .about-tab li { margin-bottom: 4px; }
          .about-tab .term { font-weight: 600; }
          .about-tab .example { color: #555; font-style: italic; }
          .about-tab table.glossary { border-collapse: collapse; width: 100%; margin: 12px 0; }
          .about-tab table.glossary th { text-align: left; font-size: 12px; color: #666;
            text-transform: uppercase; letter-spacing: 0.03em; padding: 6px 12px;
            border-bottom: 2px solid #ddd; }
          .about-tab table.glossary td { padding: 6px 12px; border-bottom: 1px solid #eee; vertical-align: top; }
          .about-tab .color-swatch { display: inline-block; width: 12px; height: 12px;
            border-radius: 2px; vertical-align: middle; margin-right: 4px; }
        ")),

        h3("What is Syrona?"),
        p("Syrona compares the clinical profiles of two health datasets across three domains:",
          tags$strong("conditions"), ",", tags$strong("procedures"), ", and", tags$strong("drugs"), ".",
          "It is developed by Maarja Pajusalu (maarja.pajusalu@ut.ee),",
          "a Junior Research Fellow of Health Informatics at the Institute of Computer Science, University of Tartu.",
          "For every clinical concept recorded in both datasets, it calculates how much more (or less)",
          "common that concept is in one dataset compared to the other.",
          "This helps identify systematic differences in prevalence between populations,",
          "hospitals, or cohorts."),

        h3("Where does the data come from?"),
        p("Each dataset is extracted from a health database that follows the",
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/", target = "_blank",
                 "OMOP Common Data Model"),
          "(CDM), a widely used standard for observational health data.",
          "Clinical concepts are coded using standard vocabularies:",
          tags$strong("SNOMED CT"), "for conditions and procedures, and",
          tags$strong("RxNorm"), "(mapped to", tags$strong("ATC"), ") for drugs."),
        p("For each concept, Syrona counts how many patients have it in each year,",
          "broken down by sex (female/male) and age group (10-year brackets from 0\u20139 to 80+).",
          "These counts form the basis of all calculations."),

        h3("Three clinical domains"),

        h4("Conditions"),
        p("Diagnoses and clinical findings recorded in the", tags$code("condition_occurrence"), "table.",
          "Each SNOMED CT condition concept is counted per patient per stratum.",
          "Conditions are classified using three systems:"),
        tags$ul(
          tags$li(tags$span(class = "term", "ICD-10 Chapter"),
                  " \u2014 derived from SNOMED\u2192ICD-10 mappings via the OMOP",
                  tags$code("concept_relationship"), "table.",
                  " The 3-character ICD-10 code prefix is matched to one of the 22 WHO chapters.",
                  " Concepts without an ICD-10 mapping are grouped as \u201c(Unmapped)\u201d."),
          tags$li(tags$span(class = "term", "Body system"),
                  " \u2014 SNOMED CT hierarchy under", tags$em("Disorder of body system"),
                  " (concept 4180628).",
                  " Chapters are the first level of descendants in the", tags$code("concept_ancestor"), "table."),
          tags$li(tags$span(class = "term", "Disease category"),
                  " \u2014 SNOMED CT hierarchy under", tags$em("Disease"),
                  " (concept 4274025), excluding body-system chapters.",
                  " Captures categories like infectious diseases, neoplasms, and metabolic disorders.")
        ),
        p("Attribute filters narrow conditions further using SNOMED CT relationships:"),
        tags$ul(
          tags$li(tags$span(class = "term", "Finding site"),
                  " \u2014 anatomical location where the condition manifests",
                  " (SNOMED \u2018Has finding site\u2019 relationship)."),
          tags$li(tags$span(class = "term", "Morphology"),
                  " \u2014 structural change such as inflammation or neoplasm",
                  " (SNOMED \u2018Has associated morphology\u2019 relationship)."),
          tags$li(tags$span(class = "term", "Clinical course"),
                  " \u2014 temporal pattern such as acute or chronic",
                  " (SNOMED \u2018Has clinical course\u2019 relationship).")
        ),

        h4("Procedures"),
        p("Clinical procedures recorded in the", tags$code("procedure_occurrence"), "table.",
          "Each SNOMED CT procedure concept is counted per patient per stratum.",
          "Procedures are classified using two SNOMED hierarchy-based systems:"),
        tags$ul(
          tags$li(tags$span(class = "term", "By method"),
                  " \u2014 SNOMED CT hierarchy under", tags$em("Procedure by method"),
                  " (concept 4029205).",
                  " Groups procedures by the type of action: excision, repair, imaging, etc."),
          tags$li(tags$span(class = "term", "By site"),
                  " \u2014 SNOMED CT hierarchy under", tags$em("Procedure by site"),
                  " (concept 4180627).",
                  " Groups procedures by the body region where they are performed.")
        ),
        p("Attribute filters for procedures:"),
        tags$ul(
          tags$li(tags$span(class = "term", "Body site"),
                  " \u2014 anatomical location of the procedure",
                  " (SNOMED \u2018Has direct procedure site\u2019 relationship)."),
          tags$li(tags$span(class = "term", "Method"),
                  " \u2014 type of action performed",
                  " (SNOMED \u2018Has method\u2019 relationship).")
        ),

        h4("Drugs"),
        p("Drug exposures recorded in the", tags$code("drug_exposure"), "table.",
          "Drug concepts are rolled up to the", tags$strong("RxNorm Ingredient"), "level",
          "using the", tags$code("concept_ancestor"), "table,",
          "so that branded products, clinical drugs, and different dose forms",
          "are aggregated to a single ingredient concept.",
          "Each ingredient is then counted per patient per stratum."),
        p("Drugs are classified by:"),
        tags$ul(
          tags$li(tags$span(class = "term", "ATC 1st level"),
                  " \u2014 the WHO Anatomical Therapeutic Chemical classification.",
                  " The 14 top-level ATC chapters group drugs by the organ system they target",
                  " (e.g. \u201cA \u2014 Alimentary tract and metabolism\u201d,",
                  " \u201cC \u2014 Cardiovascular system\u201d).",
                  " ATC mappings are resolved via the", tags$code("concept_ancestor"), "table.",
                  " Ingredients without an ATC mapping are grouped as \u201c(Unmapped)\u201d.")
        ),
        p("Drug attribute filters are not currently available.",
          "RxNorm relationships (dose form, brand name, supplier) are structural",
          "rather than clinically informative for prevalence comparison."),

        h3("What is calculated?"),

        h4("Step 1: Prevalence"),
        p("For each concept in each stratum (year \u00d7 sex \u00d7 age group),",
          "the prevalence is the fraction of observed patients who have that concept."),
        p(class = "example", "Example: if 200 out of 10,000 observed patients in a stratum",
          "have hypertension, the prevalence is 200 / 10,000 = 0.02 (2%)."),

        h4("Step 2: Prevalence Ratio (PR)"),
        p("The prevalence ratio compares the prevalence in one dataset to the other:",
          tags$br(),
          tags$code("PR = prevalence in dataset 2 / prevalence in dataset 1")),
        p("A PR of 1.0 means equal prevalence. A PR of 2.0 means the concept is",
          "twice as common in dataset 2. A PR of 0.5 means it is half as common."),

        h4("Step 3: Meta-analysis"),
        p("A single concept has many strata (e.g. 8 years \u00d7 2 sexes \u00d7 9 age groups = up to 144).",
          "Syrona pools these using",
          tags$strong("random-effects meta-analysis"),
          "(Paule\u2013Mandel estimator for between-stratum heterogeneity)",
          "at three levels:"),
        tags$ul(
          tags$li(tags$span(class = "term", "Across years"), " \u2192 one estimate per sex \u00d7 age group"),
          tags$li(tags$span(class = "term", "Across age groups"), " \u2192 one estimate per sex (Female, Male)"),
          tags$li(tags$span(class = "term", "Across sexes"), " \u2192 one overall estimate per concept (Both)")
        ),

        h3("Glossary of measures"),
        tags$table(class = "glossary",
          tags$thead(tags$tr(
            tags$th("Measure"), tags$th("Meaning"), tags$th("How to read it")
          )),
          tags$tbody(
            tags$tr(
              tags$td("Prevalence"),
              tags$td("Fraction of observed patients with the concept in a given stratum."),
              tags$td("0.02 = 2% of patients.")
            ),
            tags$tr(
              tags$td(HTML("log<sub>2</sub> PR")),
              tags$td("Prevalence ratio on a logarithmic (base 2) scale. This is the primary analytical scale."),
              tags$td("0 = equal. +1 = twice as common in dataset 2. \u22121 = half as common.")
            ),
            tags$tr(
              tags$td("Fold difference"),
              tags$td(HTML("The prevalence ratio on a natural scale: fold = 2<sup>log<sub>2</sub> PR</sup>.")),
              tags$td("1.0 = equal. 2.0 = twice as common. 0.5 = half as common.")
            ),
            tags$tr(
              tags$td("Confidence interval (CI)"),
              tags$td("The range within which the true PR likely falls (95% confidence)."),
              tags$td("Narrow CI = precise estimate. Wide CI = uncertain.")
            ),
            tags$tr(
              tags$td("p-value"),
              tags$td("Probability of observing this difference by chance, assuming no real difference exists."),
              tags$td("p < 0.05 is conventionally considered statistically significant.")
            ),
            tags$tr(
              tags$td("Fold threshold"),
              tags$td(paste0("A clinical relevance boundary (default: ", FOLD_THRESHOLD,
                             "). Concepts within 1/", FOLD_THRESHOLD, " to ", FOLD_THRESHOLD,
                             " are considered similar.")),
              tags$td("Shown as dotted lines on plots. Concepts outside this range are flagged as over- or underrepresented.")
            ),
            tags$tr(
              tags$td("k-anonymity"),
              tags$td("Privacy protection: any stratum with fewer than 5 patients is suppressed."),
              tags$td("Some rare concepts may be excluded entirely.")
            )
          )
        ),

        h3("Color encoding"),
        tags$ul(
          tags$li(tags$span(class = "color-swatch", style = "background:#005FC8;"), " ",
                  tags$strong("Blue"), " \u2014 the reference dataset (listed first in the comparison)."),
          tags$li(tags$span(class = "color-swatch", style = "background:#FF6600;"), " ",
                  tags$strong("Orange"), " \u2014 the comparison dataset (listed second)."),
          tags$li("On heatmaps and tables, blue shading means higher prevalence in the reference,",
                  "orange means higher in the comparison.")
        ),

        h3("What each tab shows"),
        p(tags$em("Visual tabs:")),
        tags$ul(
          tags$li(tags$span(class = "term", "Overview"),
                  " \u2014 Histogram of fold differences across all concepts in the active domain.",
                  " Shows the overall balance between datasets.",
                  " The per-chapter view breaks this down by the domain\u2019s primary classification."),
          tags$li(tags$span(class = "term", "Heatmap"),
                  " \u2014 Interactive heatmap of prevalence ratios, faceted by sex and age group,",
                  " paired with a forest plot showing Female/Male/Both meta PR.",
                  " Click a concept to open its detail."),
          tags$li(tags$span(class = "term", "Detail"),
                  " \u2014 Per-year forest plot and absolute prevalence dumbbell chart for a single concept.",
                  " Opens automatically when clicking a concept on the Heatmap."),
          tags$li(tags$span(class = "term", "Population"),
                  " \u2014 Population pyramids showing the sex and birth year distribution of each dataset.",
                  " This tab is domain-independent.")
        ),
        p(tags$em("Data tables:")),
        tags$ul(
          tags$li(tags$span(class = "term", "Summary by Sex"),
                  " \u2014 Table with one row per concept per sex (Both, Female, Male).",
                  " Meta-analyzed across years and age groups."),
          tags$li(tags$span(class = "term", "Full Breakdown"),
                  " \u2014 Full stratified table: one row per concept \u00d7 year \u00d7 sex \u00d7 age group.",
                  " Use column filters to drill down."),
          tags$li(tags$span(class = "term", "Patient Counts"),
                  " \u2014 Raw patient counts and percentages per dataset, before any ratio calculation.")
        ),
        p(tags$em("Reference:")),
        tags$ul(
          tags$li(tags$span(class = "term", "ICD-10 Codes"),
                  " \u2014 SNOMED to ICD-10 code mappings for all compared concepts."),
          tags$li(tags$span(class = "term", "Concept Index"),
                  " \u2014 One row per concept with all chapter assignments and attribute labels.")
        ),

        h3("Filtering"),
        p("Use the", tags$strong("domain selector"), "(Conditions / Procedures / Drugs)",
          "in the header bar to switch between clinical domains.",
          "Each domain has its own classification systems and attribute filters."),
        p("The", tags$strong("Group by"), "selector chooses a classification system.",
          "Available systems depend on the active domain:"),
        tags$ul(
          tags$li(tags$span(class = "term", "Conditions:"),
                  " ICD-10 Chapter, Body system, Disease category"),
          tags$li(tags$span(class = "term", "Procedures:"),
                  " By method, By site"),
          tags$li(tags$span(class = "term", "Drugs:"),
                  " ATC 1st level")
        ),
        p("Only one classification system is active at a time.",
          "Attribute filters (where available) can be combined freely to narrow concepts.",
          "Domains without relevant attributes (e.g. Drugs) show no attribute filters."),
        p(tags$strong("Sex & Age group filters"), "appear on the Heatmap, Detail, and Full Breakdown tabs.",
          "They control which demographic strata are included in the meta-analysis, not just which rows are displayed."),

        h3("SNOMED CT poly-hierarchy and chapter overlap"),
        p("SNOMED CT is a poly-hierarchical terminology: a concept may have multiple parent concepts",
          "and therefore appear under more than one chapter within the same classification axis.",
          "For example, a condition may belong to both", tags$em("Disorder of cardiovascular system"),
          "and", tags$em("Disorder of respiratory system"), "under the Body system classification."),
        p("This means that chapter groupings based on SNOMED CT (Body system, Disease category,",
          "By method, By site) are", tags$strong("not mutually exclusive"), ".",
          "When viewing the per-chapter PR distribution facets, some concepts appear in more",
          "than one facet. The dashboard reports the overlap count per facet and in the status badge."),
        p(tags$strong("ICD-10 Chapter"), "and", tags$strong("ATC 1st level"),
          "are mono-hierarchical classifications where each", tags$em("code"),
          "belongs to exactly one chapter. However, a single SNOMED concept may map to",
          "multiple ICD-10 or ATC codes via the OMOP vocabulary, causing it to appear in",
          "more than one chapter. This is typically less overlap than with SNOMED-based",
          "classifications, but it is not zero."),
        p("Note that chapter filters and attribute filters (e.g. Finding site, Device) both derive",
          "from the SNOMED CT ontology and are therefore", tags$strong("correlated, not independent"), ".",
          "For instance, filtering to", tags$em("Disorder of cardiovascular system"),
          "and then filtering by Finding site \u2192 Heart structure will largely overlap,",
          "because both reflect the same underlying clinical semantics.",
          "The cascading filter updates communicate this dependency \u2014 narrowing a chapter",
          "reduces the available attribute choices accordingly."),
        p("Prevalence ratios are always computed at the individual concept level,",
          "where poly-hierarchy has no effect: the same concept appears on both sides of the ratio",
          "regardless of its hierarchical placement."),

        tags$hr(),
        p(style = "font-size: 12px; color: #999;",
          "Syrona \u00b7 Prevalence ratio comparison tool \u00b7",
          "Built with R, Shiny, dplyr, ggiraph, and the meta package.")
      )
  )
)
