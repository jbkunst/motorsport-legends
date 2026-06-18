clean_txt <- function(x) {
  # Limpia texto extraído desde HTML.
  
  x |>
    stringr::str_replace_all("\\u00a0", " ") |>
    stringr::str_squish()
}

parse_num <- function(x) {
  x <- x |> stringr::str_squish()
  
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    !stringr::str_detect(x, "\\d") ~ NA_real_,
    stringr::str_detect(x, "\\d{1,3}(,\\d{3})+(\\.\\d+)?") ~ readr::parse_number(x, locale = readr::locale(grouping_mark = ",")),
    stringr::str_detect(x, "\\d{1,3}(\\.\\d{3})+(,\\d+)?") ~ readr::parse_number(x, locale = readr::locale(grouping_mark = ".", decimal_mark = ",")),
    TRUE ~ readr::parse_number(x)
  )
}

get_col <- function(data, col) {
  if (col %in% names(data)) data[[col]] else NA_character_
}

image_url_to_data_uri <- function(url) {
  if (is.na(url) || url == "") return(NA_character_)

  url <- url |>
    stringr::str_replace_all("&amp;", "&") |>
    utils::URLencode(reserved = FALSE)

  if (!stringr::str_detect(url, "^https?://")) return(NA_character_)

  resp <- httr2::request(url) |>
    httr2::req_user_agent("Joshua Kunst jbkunst@gmail.com") |>
    httr2::req_perform()

  raw <- httr2::resp_body_raw(resp)
  content_type <- httr2::resp_header(resp, "content-type")

  if (is.null(content_type)) {
    content_type <- "image/jpeg"
  }

  paste0("data:", content_type, ";base64,", base64enc::base64encode(raw))
}

get_page_html <- function(session, url, wait = 5, n_scroll = 10, scroll_wait = 800, timeout = 45) {
  session$Page$navigate(url, timeout_ = timeout)
  session$Page$loadEventFired(timeout_ = timeout)
  Sys.sleep(wait)
  
  session$Runtime$evaluate(
    glue::glue("
      async function scrollPage() {{
        for (let i = 0; i < {n_scroll}; i++) {{
          window.scrollBy(0, window.innerHeight * 0.8);
          await new Promise(r => setTimeout(r, {scroll_wait}));
        }}
      }}
      scrollPage();
    "),
    awaitPromise = TRUE,
    timeout_ = timeout
  )
  
  session$Runtime$evaluate("document.documentElement.outerHTML", timeout_ = timeout)$result$value
}

make_dt <- function(data, element_id = NULL, photo_height = 96, search = "global") {
  # Crea una tabla DT responsive, con HTML seguro, fotos con lightbox,
  # tooltips opcionales, búsqueda simple y conteo visible de registros.
  
  search <- rlang::arg_match(search, c("global", "columns", "none"))
  
  clean_dt_name <- function(x) {
    stringr::str_replace_all(x, "_", " ") |>
      stringr::str_to_sentence()
  }
  
  detect_html_cols <- function(data) {
    html_pattern <- "<\\s*(img|a|span|div|br|strong|em|svg|circle|line|path)\\b|data:image/"
    
    names(data)[
      purrr::map_lgl(
        data,
        ~ is.character(.x) && any(stringr::str_detect(.x, html_pattern), na.rm = TRUE)
      )
    ]
  }
  
  detect_img_cols <- function(data) {
    names(data)[
      purrr::map_lgl(
        data,
        ~ is.character(.x) && any(stringr::str_detect(.x, "<\\s*img\\b"), na.rm = TRUE)
      )
    ]
  }
  
  detect_tooltips <- function(data) {
    data |>
      dplyr::select(where(is.character)) |>
      unlist(use.names = FALSE) |>
      stringr::str_detect("dt-tooltip") |>
      any(na.rm = TRUE)
  }
  
  build_responsive_defs <- function(data, img_cols) {
    priority_1_targets <- unique(c(0, match(img_cols, names(data)) - 1))
    priority_1_targets <- priority_1_targets[!is.na(priority_1_targets)]
    
    priority_2_targets <- setdiff(
      seq_len(min(4, ncol(data))) - 1,
      priority_1_targets
    )
    
    purrr::compact(list(
      if (length(priority_1_targets) > 0) {
        list(responsivePriority = 1, targets = priority_1_targets)
      },
      if (length(priority_2_targets) > 0) {
        list(responsivePriority = 2, targets = priority_2_targets)
      },
      list(responsivePriority = 100, targets = "_all")
    ))
  }
  
  build_lightbox_js <- function(has_img, photo_height) {
    if (!has_img) return(character())
    
    c(
      glue::glue(
        "  container.find('td img').each(function() {{
             var img = $(this);
             img.addClass('lightbox-img');
             img.css({{
               'height': '{photo_height}px',
               'width': 'auto',
               'border-radius': '8px',
               'cursor': 'zoom-in'
             }});
             if (!img.attr('data-full')) img.attr('data-full', img.attr('src'));
           }});"
      ),
      "  if (!document.getElementById('lb-overlay')) {",
      "    var overlay = document.createElement('div');",
      "    overlay.id = 'lb-overlay';",
      "    overlay.style.cssText = 'display:none;position:fixed;top:0;left:0;width:100%;height:100%;background:rgba(0,0,0,0.86);z-index:9999;justify-content:center;align-items:center;cursor:zoom-out;backdrop-filter:blur(3px);';",
      "    var lbImg = document.createElement('img');",
      "    lbImg.id = 'lb-img';",
      "    lbImg.style.cssText = 'max-width:92vw;max-height:92vh;border-radius:10px;box-shadow:0 18px 60px rgba(0,0,0,0.65);object-fit:contain;';",
      "    overlay.appendChild(lbImg);",
      "    document.body.appendChild(overlay);",
      "    overlay.addEventListener('click', function() { overlay.style.display = 'none'; });",
      "    document.addEventListener('keydown', function(e) { if(e.key === 'Escape') overlay.style.display = 'none'; });",
      "  }",
      "  var overlay = document.getElementById('lb-overlay');",
      "  var lbImg = document.getElementById('lb-img');",
      "  $(document).off('click.dtLightbox', 'td img.lightbox-img').on('click.dtLightbox', 'td img.lightbox-img', function() {",
      "    var thumbSrc = this.src;",
      "    var fullSrc = this.dataset.full || thumbSrc;",
      "    lbImg.onerror = function() { lbImg.onerror = null; lbImg.src = thumbSrc; };",
      "    lbImg.src = fullSrc;",
      "    overlay.style.display = 'flex';",
      "  });"
    )
  }
  
  build_tooltip_css <- function() {
    htmltools::tags$style(htmltools::HTML("
      .dt-tooltip {
        position: relative;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        margin-left: 6px;
        vertical-align: middle;
        cursor: help;
      }

      .dt-tooltip-icon {
        width: 18px;
        height: 18px;
        fill: none;
        stroke: #6c757d;
        stroke-width: 2;
        stroke-linecap: round;
        stroke-linejoin: round;
        transition: all .15s ease;
      }

      .dt-tooltip:hover .dt-tooltip-icon {
        stroke: #111827;
        transform: scale(1.08);
      }

      .dt-tooltip:hover::after {
        content: attr(data-tip);
        position: absolute;
        z-index: 9999;
        left: 50%;
        top: calc(100% + 10px);
        transform: translateX(-50%);
        width: 420px;
        max-width: 70vw;
        padding: 14px 16px;
        border: 1px solid rgba(0, 0, 0, 0.08);
        border-radius: 14px;
        background: rgba(255, 255, 255, 0.98);
        color: #212529;
        font-size: 13px;
        font-weight: 400;
        line-height: 1.45;
        white-space: normal;
        box-shadow: 0 14px 40px rgba(0, 0, 0, 0.18);
        backdrop-filter: blur(8px);
      }

      .dt-tooltip:hover::before {
        content: '';
        position: absolute;
        z-index: 10000;
        left: 50%;
        top: calc(100% + 4px);
        transform: translateX(-50%) rotate(45deg);
        width: 12px;
        height: 12px;
        background: rgba(255, 255, 255, 0.98);
        border-left: 1px solid rgba(0, 0, 0, 0.08);
        border-top: 1px solid rgba(0, 0, 0, 0.08);
      }
    "))
  }
  
  data_dt <- data |>
    dplyr::rename_with(clean_dt_name)
  
  html_cols <- detect_html_cols(data_dt)
  img_cols <- detect_img_cols(data_dt)
  has_img <- length(img_cols) > 0
  has_tooltip <- detect_tooltips(data_dt)
  
  escape_cols <- if (length(html_cols) == 0) {
    TRUE
  } else {
    setdiff(names(data_dt), html_cols)
  }
  
  responsive_defs <- build_responsive_defs(data_dt, img_cols)
  lightbox_js <- build_lightbox_js(has_img, photo_height)
  
  dt <- DT::datatable(
    data_dt,
    elementId = element_id,
    extensions = c("FixedHeader", "Responsive"),
    filter = if (search == "columns") "top" else "none",
    class = "hover",
    escape = escape_cols,
    rownames = FALSE,
    options = list(
      fixedHeader = TRUE,
      responsive = TRUE,
      autoWidth = FALSE,
      scrollX = FALSE,
      paging = FALSE,
      search = list(regex = TRUE, smart = FALSE),
      dom = paste0(if (search == "global") "f" else "", "rti"),
      scrollY = dplyr::case_when(
        search == "columns" ~ "calc(100vh - 185px)",
        search == "global" ~ "calc(100vh - 165px)",
        TRUE ~ "calc(100vh - 135px)"
      ),
      scrollCollapse = TRUE,
      columnDefs = responsive_defs,
      language = list(
        info = "_TOTAL_ entries",
        infoEmpty = "0 entries",
        infoFiltered = "filtered from _MAX_ total"
      ),
      initComplete = DT::JS(
        c(
          "function(settings, json) {",
          "  var link = document.createElement('link');",
          "  link.rel = 'stylesheet';",
          "  link.href = 'https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap';",
          "  document.head.appendChild(link);",
          "  var api = this.api();",
          "  var container = $(api.table().container());",
          "  container.css({'font-family': 'Inter, system-ui, sans-serif', 'font-size': '13px'});",
          "  container.find('thead th').css({'font-weight': '600', 'font-size': '11px', 'text-transform': 'uppercase', 'letter-spacing': '0.06em', 'color': '#555'});",
          "  container.find('.dataTables_info').css({'padding-top': '8px', 'font-size': '12px', 'color': '#555'});",
          "  container.find('td').css({'vertical-align': 'top', 'max-width': '260px', 'white-space': 'normal', 'line-height': '1.35'});",
          "  container.find('thead input, thead select').css({'width': '100%', 'min-width': '0', 'box-sizing': 'border-box', 'font-size': '11px'});",
          "  var syncResponsiveFilters = function(columns) {",
          "    var filterRows = container.find('thead tr').filter(function() { return $(this).find('input, select').length > 0; });",
          "    if (!columns) {",
          "      columns = [];",
          "      api.columns().every(function(i) { columns[i] = $(api.column(i).header()).is(':visible'); });",
          "    }",
          "    filterRows.each(function() {",
          "      $(this).children('th, td').each(function(i) { $(this).toggle(columns[i] !== false); });",
          "    });",
          "  };",
          "  api.on('responsive-resize', function(e, datatable, columns) { syncResponsiveFilters(columns); });",
          "  api.on('draw', function() { syncResponsiveFilters(); });",
          "  setTimeout(function() { syncResponsiveFilters(); }, 100);",
          "  container.find('td').each(function() {",
          "    if ($(this).text().length > 120) {",
          "      $(this).css({'font-size': '11px', 'color': '#555'});",
          "    }",
          "  });",
          lightbox_js,
          "}"
        )
      )
    )
  )
  
  if (has_tooltip) {
    dt <- dt |>
      htmlwidgets::prependContent(build_tooltip_css())
  }
  
  dt
}
