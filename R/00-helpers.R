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

make_dt <- function(data, element_id = NULL, photo_height = 72) {
  
  data_dt <- data |>
    dplyr::rename_with(
      ~ stringr::str_replace_all(.x, "_", " ") |>
        stringr::str_to_sentence()
    )
  
  html_cols <- data_dt |>
    dplyr::summarise(
      dplyr::across(
        where(is.character),
        ~ any(
          stringr::str_detect(
            .x,
            "<\\s*(img|a|span|div|br|strong|em)\\b|data:image/"
          ),
          na.rm = TRUE
        )
      )
    ) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::filter(value) |>
    dplyr::pull(name)
  
  escape_cols <- if (length(html_cols) == 0) {
    TRUE
  } else {
    setdiff(names(data_dt), html_cols)
  }
  
  has_img <- if (length(html_cols) == 0) {
    FALSE
  } else {
    any(
      purrr::map_lgl(
        data_dt[html_cols],
        ~ any(stringr::str_detect(.x, "<\\s*img\\b"), na.rm = TRUE)
      )
    )
  }
  
  lightbox_js <- if (has_img) {
    c(
      glue::glue(
        "  container.find('td img').each(function() {{
             var img = $(this);
             img.addClass('lightbox-img');
             img.css({{
               'height': '{photo_height}px',
               'width': 'auto',
               'border-radius': '6px',
               'cursor': 'zoom-in'
             }});
             if (!img.attr('data-full')) img.attr('data-full', img.attr('src'));
           }});"
      ),
      "  if (!document.getElementById('lb-overlay')) {",
      "    var overlay = document.createElement('div');",
      "    overlay.id = 'lb-overlay';",
      "    overlay.style.cssText = 'display:none;position:fixed;top:0;left:0;width:100%;height:100%;background:rgba(0,0,0,0.85);z-index:9999;justify-content:center;align-items:center;cursor:zoom-out;';",
      "    var lbImg = document.createElement('img');",
      "    lbImg.id = 'lb-img';",
      "    lbImg.style.cssText = 'max-width:90vw;max-height:90vh;border-radius:6px;box-shadow:0 8px 32px rgba(0,0,0,0.6);object-fit:contain;';",
      "    overlay.appendChild(lbImg);",
      "    document.body.appendChild(overlay);",
      "    overlay.addEventListener('click', function() { overlay.style.display = 'none'; });",
      "    document.addEventListener('keydown', function(e) { if(e.key === 'Escape') overlay.style.display = 'none'; });",
      "  }",
      "  var overlay = document.getElementById('lb-overlay');",
      "  var lbImg = document.getElementById('lb-img');",
      "  $(document).on('click', 'td img.lightbox-img', function() {",
      "    var thumbSrc = this.src;",
      "    var fullSrc = this.dataset.full || thumbSrc;",
      "    lbImg.onerror = function() { lbImg.onerror = null; lbImg.src = thumbSrc; };",
      "    lbImg.src = fullSrc;",
      "    overlay.style.display = 'flex';",
      "  });"
    )
  } else {
    character()
  }
  
  DT::datatable(
    data_dt,
    elementId = element_id,
    extensions = "FixedHeader",
    class = "hover",
    escape = escape_cols,
    rownames = FALSE,
    options = list(
      fixedHeader = TRUE,
      paging = FALSE,
       dom = "frt",
      scrollY = "calc(100vh - 200px)",
      scrollCollapse = TRUE,
      initComplete = DT::JS(
        c(
          "function(settings, json) {",
          "  var link = document.createElement('link');",
          "  link.rel = 'stylesheet';",
          "  link.href = 'https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap';",
          "  document.head.appendChild(link);",
          
          "  var container = $(this.api().table().container());",
          "  container.css({'font-family': 'Inter, system-ui, sans-serif', 'font-size': '13px'});",
          "  container.find('thead th').css({'font-weight': '600', 'font-size': '11px', 'text-transform': 'uppercase', 'letter-spacing': '0.06em', 'color': '#555'});",
          "  container.find('td').css({'vertical-align': 'top', 'max-width': '260px', 'white-space': 'normal', 'line-height': '1.35'});",
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
}
