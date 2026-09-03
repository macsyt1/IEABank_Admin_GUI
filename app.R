# ============================================================
# IEABank Admin GUI
# ============================================================


library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(pool)
library(DT)
library(readxl)
library(writexl)
library(httr2)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

APP_VERSION <- "1.1.0"
IEA_RED  <- "#E2231A"
IEA_GRAY <- "#54565A"

# ------------------------------------------------------------
# Database connection
# ------------------------------------------------------------
pool <- dbPool(
  RPostgres::Postgres(),
  host = Sys.getenv("SUPABASE_HOST"),
  port = as.integer(Sys.getenv("SUPABASE_PORT")),
  dbname = Sys.getenv("SUPABASE_DB"),
  user = Sys.getenv("SUPABASE_ED_USER"),
  password = Sys.getenv("SUPABASE_ED_PWD"),
  sslmode = "require"
)
onStop(function() poolClose(pool))

SUPABASE_URL <- Sys.getenv("SUPABASE_URL")
SUPABASE_ANON_KEY <- Sys.getenv("SUPABASE_ANON_KEY")

supabase_sign_in <- function(email, password) {
  req <- request(paste0(SUPABASE_URL, "/auth/v1/token?grant_type=password")) |>
    req_method("POST") |>
    req_headers(
      apikey = SUPABASE_ANON_KEY,
      Authorization = paste("Bearer", SUPABASE_ANON_KEY)
    ) |>
    req_body_json(list(email = email, password = password))

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) >= 400) return(NULL)
  resp_body_json(resp)
}

supabase_update_password <- function(access_token, new_password) {
  req <- request(paste0(SUPABASE_URL, "/auth/v1/user")) |>
    req_method("PUT") |>
    req_headers(
      apikey = SUPABASE_ANON_KEY,
      Authorization = paste("Bearer", access_token),
      `Content-Type` = "application/json"
    ) |>
    req_body_raw(
      jsonlite::toJSON(list(password = new_password), auto_unbox = TRUE)
    )

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  !is.null(resp) && resp_status(resp) < 400
}

supabase_send_password_reset <- function(email) {
  reset_url <- "https://macsyt1.github.io/IEABank_Admin_GUI/reset-password.html"

  req <- request(paste0(SUPABASE_URL, "/auth/v1/recover")) |>
    req_url_query(redirect_to = reset_url) |>
    req_method("POST") |>
    req_headers(
      apikey = SUPABASE_ANON_KEY,
      Authorization = paste("Bearer", SUPABASE_ANON_KEY)
    ) |>
    req_body_json(list(email = email))

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  !is.null(resp) && resp_status(resp) < 400
}

get_user_profile <- function(user_id) {
  dbGetQuery(
    pool,
    "
    select user_id, first_name, last_name, role, active
    from public.user_profiles
    where user_id = $1
    ",
    params = list(user_id)
  )
}

# ============================================================
# Editable schema
# Only Core + Scales are exposed in the Admin GUI.
# ============================================================
fk <- function(table, value, label = value) {
  list(table = table, value = value, label = label)
}

fld <- function(name, type = "text", required = FALSE, fk = NULL, pk = FALSE) {
  list(name = name, type = type, required = required, fk = fk, pk = pk)
}

META <- list(
  admin = list(
    group = "Core", pk = "admin_id", pk_generated = FALSE, audit = TRUE,
    fields = list(
      fld("admin_id", "text", TRUE, pk = TRUE),
      fld("study", "text", TRUE),
      fld("phase"),
      fld("year", "smallint", TRUE),
      fld("instrument"),
      fld("target"),
      fld("cycle")
    )
  ),
  category = list(
    group = "Core", pk = "category_id", pk_generated = FALSE, audit = TRUE,
    fields = list(
      fld("category_id", "text", TRUE, pk = TRUE),
      fld("category_name", "text", TRUE)
    )
  ),
  item_id = list(
    group = "Core", pk = "item_uid", pk_generated = FALSE, audit = TRUE,
    fields = list(
      fld("item_uid", "text", TRUE, pk = TRUE),
      fld("item_name", "text", TRUE),
      fld(
        "category_id", "text", TRUE,
        fk = fk("category", "category_id", "category_id || ' / ' || category_name")
      )
    )
  ),
  item_admin = list(
    group = "Core", pk = "item_admin_pk", pk_generated = TRUE, audit = TRUE,
    fields = list(
      fld("item_admin_id", "text", TRUE),
      fld("item_var", "text", TRUE),
      fld("admin_id", "text", TRUE, fk = fk("admin", "admin_id")),
      fld(
        "item_uid", "text", TRUE,
        fk = fk("item_id", "item_uid", "item_uid || ' / ' || item_name")
      ),
      fld("varname"),
      fld("dataset_label"),
      fld("wording_question"),
      fld("wording_item"),
      fld("wording_instruction"),
      fld("wording_context"),
      fld("wording_heading"),
      fld("type", "text", TRUE),
      fld("miss_id", "text", fk = fk("miss_scheme", "miss_id")),
      fld("response_id", "text", fk = fk("value_scheme", "response_id")),
      fld("puf", "bool", TRUE)
    )
  ),
  item_example = list(
    group = "Core", pk = c("item_admin_id", "admin_id", "path"),
    pk_generated = FALSE, audit = FALSE,
    fields = list(
      fld("item_admin_id", "text", TRUE, pk = TRUE),
      fld("admin_id", "text", TRUE, pk = TRUE, fk = fk("admin", "admin_id")),
      fld("path", "text", TRUE, pk = TRUE),
      fld("label")
    )
  ),
  miss_scheme = list(
    group = "Core", pk = "miss_id", pk_generated = FALSE, audit = TRUE,
    fields = list(fld("miss_id", "text", TRUE, pk = TRUE))
  ),
  miss_scheme_value = list(
    group = "Core", pk = c("miss_id", "value"), pk_generated = FALSE, audit = TRUE,
    fields = list(
      fld("miss_id", "text", TRUE, pk = TRUE, fk = fk("miss_scheme", "miss_id")),
      fld("value", "smallint", TRUE, pk = TRUE),
      fld("category", "text", TRUE)
    )
  ),
  value_scheme = list(
    group = "Core", pk = "response_id", pk_generated = FALSE, audit = TRUE,
    fields = list(fld("response_id", "text", TRUE, pk = TRUE))
  ),
  value_scheme_value = list(
    group = "Core", pk = c("response_id", "value"), pk_generated = FALSE, audit = TRUE,
    fields = list(
      fld("response_id", "text", TRUE, pk = TRUE, fk = fk("value_scheme", "response_id")),
      fld("value", "smallint", TRUE, pk = TRUE),
      fld("label", "text", TRUE)
    )
  ),
  scale = list(
    group = "Scales", pk = "scale_uid", pk_generated = FALSE, audit = FALSE,
    fields = list(
      fld("scale_uid", "text", TRUE, pk = TRUE),
      fld("scale_name", "text", TRUE)
    )
  ),
  scale_version = list(
    group = "Scales", pk = "scale_id", pk_generated = FALSE, audit = TRUE,
    fields = list(
      fld("scale_id", "text", TRUE, pk = TRUE),
      fld("scale_description", "text", TRUE),
      fld(
        "scale_uid", "text", FALSE,
        fk = fk("scale", "scale_uid", "scale_uid || ' / ' || scale_name")
      ),
      fld("scale_var"),
      fld("scale_varname")
    )
  ),
  scale_items = list(
    group = "Scales", pk = "scale_item_pk", pk_generated = TRUE, audit = TRUE,
    fields = list(
      fld(
        "scale_id", "text", TRUE,
        fk = fk("scale_version", "scale_id", "scale_id || ' / ' || scale_description")
      ),
      fld(
        "item_admin_pk", "int", TRUE,
        fk = fk("item_admin", "item_admin_pk", "item_admin_id || ' / ' || admin_id")
      )
    )
  )
)

TABLE_CHOICES <- list(
  Core = c(
    "admin", "category", "item_id", "item_admin", "item_example",
    "miss_scheme", "miss_scheme_value", "value_scheme", "value_scheme_value"
  ),
  Scales = c("scale", "scale_version", "scale_items")
)

cast_ph <- function(i, type) {
  switch(
    type,
    smallint = sprintf("$%d::smallint", i),
    int = sprintf("$%d::bigint", i),
    bool = sprintf("$%d::boolean", i),
    sprintf("$%d", i)
  )
}

fk_choices <- function(f) {
  sql <- sprintf(
    "select %s as v, %s as l from %s order by l",
    f$value, f$label, f$table
  )
  d <- dbGetQuery(pool, sql)
  setNames(as.character(d$v), as.character(d$l))
}

make_input <- function(f, value = NULL, mode = "add") {
  id <- paste0("fld_", f$name)

  if (mode == "edit" && isTRUE(f$pk)) {
    return(tags$p(tags$strong(f$name), ": ", as.character(value %||% "")))
  }

  if (!is.null(f$fk)) {
    return(
      selectizeInput(
        id, f$name,
        choices = c("" = "", fk_choices(f$fk)),
        selected = as.character(value %||% "")
      )
    )
  }

  if (f$type == "bool") {
    return(checkboxInput(id, f$name, value = isTRUE(as.logical(value))))
  }

  if (f$type %in% c("smallint", "int")) {
    return(
      numericInput(
        id, f$name,
        value = if (is.null(value) || is.na(value)) NA else as.numeric(value)
      )
    )
  }

  if (grepl("^wording_", f$name)) {
    return(textAreaInput(id, f$name, value = as.character(value %||% ""), rows = 3))
  }

  textInput(id, f$name, value = as.character(value %||% ""))
}

read_input <- function(input, f) {
  id <- paste0("fld_", f$name)

  if (f$type == "bool") {
    return(as.character(isTRUE(input[[id]])))
  }

  v <- input[[id]]
  if (is.null(v)) return(NA_character_)
  if (is.numeric(v) && is.na(v)) return(NA_character_)
  if (is.character(v) && !nzchar(trimws(v))) return(NA_character_)
  as.character(v)
}

visible_columns <- function(table_name, m) {
  fields <- vapply(m$fields, `[[`, "", "name")
  cols <- unique(c(m$pk, fields))
  if (isTRUE(m$audit)) cols <- c(cols, "modified_by", "modified_at")
  cols
}

# ============================================================
# Excel mass upload
# ============================================================
sh <- function(cols, required, int = character(), bool = character()) {
  list(cols = cols, required = required, int = int, bool = bool)
}

SHEET_SPEC <- list(
  admin = sh(
    c("admin_id", "study", "phase", "year", "instrument", "target", "cycle"),
    c("admin_id", "study", "year"),
    int = "year"
  ),
  category = sh(
    c("category_id", "category_name"),
    c("category_id", "category_name")
  ),
  item_id = sh(
    c("item_uid", "item_name", "category_id"),
    c("item_uid", "item_name", "category_id")
  ),
  miss_scheme = sh(c("miss_id"), c("miss_id")),
  miss_scheme_value = sh(
    c("miss_id", "value", "category"),
    c("miss_id", "value", "category"),
    int = "value"
  ),
  value_scheme = sh(c("response_id"), c("response_id")),
  value_scheme_value = sh(
    c("response_id", "value", "label"),
    c("response_id", "value", "label"),
    int = "value"
  ),
  scale = sh(
    c("scale_uid", "scale_name"),
    c("scale_uid", "scale_name")
  ),
  scale_version = sh(
    c("scale_id", "scale_description", "scale_uid", "scale_var", "scale_varname"),
    c("scale_id", "scale_description")
  ),
  item_admin = sh(
    c(
      "item_admin_id", "item_var", "admin_id", "item_uid", "varname", "dataset_label",
      "wording_question", "wording_item", "wording_instruction", "wording_context",
      "wording_heading", "type", "miss_id", "response_id", "puf"
    ),
    c("item_admin_id", "item_var", "admin_id", "item_uid", "type", "puf"),
    bool = "puf"
  ),
  item_example = sh(
    c("item_admin_id", "admin_id", "path", "label"),
    c("item_admin_id", "admin_id", "path")
  ),
  scale_items = sh(
    c("scale_id", "item_admin_id", "admin_id"),
    c("scale_id", "item_admin_id", "admin_id")
  )
)

validate_upload <- function(path) {
  errs <- character()
  data <- list()
  sheets <- readxl::excel_sheets(path)

  unknown <- setdiff(sheets, names(SHEET_SPEC))
  if (length(unknown)) {
    errs <- c(errs, paste("Unrecognized sheets:", paste(unknown, collapse = ", ")))
  }

  recognized <- intersect(sheets, names(SHEET_SPEC))
  if (!length(recognized)) {
    errs <- c(errs, "The workbook does not contain any recognized IEABank sheets.")
  }

  for (s in recognized) {
    df <- readxl::read_excel(path, sheet = s)
    spec <- SHEET_SPEC[[s]]

    miss_cols <- setdiff(spec$required, names(df))
    if (length(miss_cols)) {
      errs <- c(
        errs,
        sprintf(
          "%s: required columns are missing: %s",
          s, paste(miss_cols, collapse = ", ")
        )
      )
      next
    }

    for (rc in spec$required) {
      col <- df[[rc]]
      empty <- is.na(col) | (is.character(col) & !nzchar(trimws(col)))
      if (any(empty)) {
        errs <- c(errs, sprintf("%s: the required column '%s' contains empty cells", s, rc))
      }
    }

    for (ic in spec$int) {
      if (ic %in% names(df)) {
        v <- suppressWarnings(as.integer(as.character(df[[ic]])))
        if (any(is.na(v) & !is.na(df[[ic]]))) {
          errs <- c(errs, sprintf("%s: '%s' contains values that are not integers", s, ic))
        }
      }
    }

    for (bc in spec$bool) {
      if (bc %in% names(df)) {
        norm <- tolower(trimws(as.character(df[[bc]])))
        ok <- norm %in% c("true", "false", "t", "f", "1", "0", "yes", "no", "si", "sí", "na", "")
        if (!all(ok)) {
          errs <- c(errs, sprintf("%s: '%s' is not Boolean (use TRUE/FALSE)", s, bc))
        }
      }
    }

    data[[s]] <- df
  }

  list(errors = errs, data = data)
}

to_staging <- function(df, spec) {
  df <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)

  for (bc in spec$bool) {
    if (bc %in% names(df)) {
      n <- tolower(trimws(df[[bc]]))
      df[[bc]] <- ifelse(
        n %in% c("true", "t", "1", "yes", "si", "sí"), "true",
        ifelse(n %in% c("false", "f", "0", "no"), "false", NA)
      )
    }
  }

  df
}

do_load <- function(data, modified_by) {
  con <- poolCheckout(pool)
  on.exit(poolReturn(con))

  mb <- dbQuoteLiteral(con, modified_by)
  present <- names(data)
  stg <- paste0("stg_", present)

  for (t in stg) dbExecute(con, sprintf("drop table if exists %s", t))

  tryCatch({
    for (s in present) {
      dbWriteTable(
        con,
        paste0("stg_", s),
        to_staging(data[[s]], SHEET_SPEC[[s]]),
        temporary = TRUE,
        overwrite = TRUE
      )
    }

    dbBegin(con)
    run <- function(sql) dbExecute(con, sql)
    has <- function(s) s %in% present

    if (has("admin")) {
      run(sprintf(
        "insert into admin (admin_id,study,phase,year,instrument,target,cycle,modified_by)
         select admin_id,study,nullif(phase,''),year::smallint,nullif(instrument,''),nullif(target,''),nullif(cycle,''),%s
         from stg_admin", mb
      ))
    }

    if (has("category")) {
      run(sprintf(
        "insert into category (category_id,category_name,modified_by)
         select category_id,category_name,%s from stg_category", mb
      ))
    }

    if (has("item_id")) {
      run(sprintf(
        "insert into item_id (item_uid,item_name,category_id,modified_by)
         select item_uid,item_name,category_id,%s from stg_item_id", mb
      ))
    }

    if (has("miss_scheme")) {
      run(sprintf(
        "insert into miss_scheme (miss_id,modified_by)
         select miss_id,%s from stg_miss_scheme", mb
      ))
    }

    if (has("miss_scheme_value")) {
      run(sprintf(
        "insert into miss_scheme_value (miss_id,value,category,modified_by)
         select miss_id,value::smallint,category,%s from stg_miss_scheme_value", mb
      ))
    }

    if (has("value_scheme")) {
      run(sprintf(
        "insert into value_scheme (response_id,modified_by)
         select response_id,%s from stg_value_scheme", mb
      ))
    }

    if (has("value_scheme_value")) {
      run(sprintf(
        "insert into value_scheme_value (response_id,value,label,modified_by)
         select response_id,value::smallint,label,%s from stg_value_scheme_value", mb
      ))
    }

    if (has("scale")) {
      run(
        "insert into scale (scale_uid,scale_name)
         select scale_uid,scale_name from stg_scale"
      )
    }

    if (has("scale_version")) {
      run(sprintf(
        "insert into scale_version (scale_id,scale_description,scale_uid,scale_var,scale_varname,modified_by)
         select scale_id,scale_description,nullif(scale_uid,''),nullif(scale_var,''),nullif(scale_varname,''),%s
         from stg_scale_version", mb
      ))
    }

    if (has("item_admin")) {
      run(sprintf(
        "insert into item_admin (
           item_admin_id,item_var,admin_id,item_uid,varname,dataset_label,
           wording_question,wording_item,wording_instruction,wording_context,
           wording_heading,type,miss_id,response_id,puf,modified_by
         )
         select
           item_admin_id,item_var,admin_id,item_uid,nullif(varname,''),nullif(dataset_label,''),
           nullif(wording_question,''),nullif(wording_item,''),nullif(wording_instruction,''),
           nullif(wording_context,''),nullif(wording_heading,''),type,nullif(miss_id,''),
           nullif(response_id,''),puf::boolean,%s
         from stg_item_admin", mb
      ))
    }

    if (has("item_example")) {
      run(
        "insert into item_example (item_admin_id,admin_id,path,label)
         select item_admin_id,admin_id,path,nullif(label,'') from stg_item_example"
      )
    }

    if (has("scale_items")) {
      run(
        "do $$
         declare n int;
         begin
           select count(*) into n
           from stg_scale_items s
           left join item_admin ia
             on ia.item_admin_id = s.item_admin_id
            and ia.admin_id = s.admin_id
           where ia.item_admin_pk is null;
           if n > 0 then
             raise exception 'scale_items: % row(s) reference an item administration that does not exist', n;
           end if;
         end $$;"
      )

      run(sprintf(
        "insert into scale_items (scale_id,item_admin_pk,modified_by)
         select s.scale_id,ia.item_admin_pk,%s
         from stg_scale_items s
         join item_admin ia
           on ia.item_admin_id = s.item_admin_id
          and ia.admin_id = s.admin_id", mb
      ))
    }

    dbCommit(con)

    for (t in stg) dbExecute(con, sprintf("drop table if exists %s", t))
    list(ok = TRUE, rows = vapply(data, nrow, integer(1)))
  }, error = function(e) {
    try(dbRollback(con), silent = TRUE)
    for (t in stg) try(dbExecute(con, sprintf("drop table if exists %s", t)), silent = TRUE)
    list(ok = FALSE, msg = conditionMessage(e))
  })
}

template_workbook <- function() {
  lapply(SHEET_SPEC, function(s) {
    df <- as.data.frame(
      matrix(character(), nrow = 0, ncol = length(s$cols)),
      stringsAsFactors = FALSE
    )
    names(df) <- s$cols
    df
  })
}

# ============================================================
# Theme and styles
# ============================================================
iea_theme <- bs_theme(version = 5, primary = IEA_RED, secondary = IEA_GRAY)

iea_css <- sprintf("
:root { --iea-red:%s; --iea-gray:%s; }
.iea-banner{ position:relative; display:flex; align-items:center; justify-content:center;
  padding:14px 24px; background:#fff; border-bottom:4px solid var(--iea-red); margin-bottom:4px; }
.iea-logo{ position:absolute; left:24px; height:54px; }
.iea-title{ font-size:1.6rem; font-weight:700; color:var(--iea-gray); text-align:center; letter-spacing:.3px; }
.iea-version{ position:absolute; right:24px; bottom:10px; font-size:.78rem; color:#9a9a9a; cursor:pointer; user-select:none; }
.iea-nav{ display:flex; gap:14px; justify-content:center; margin:18px 0 22px; flex-wrap:wrap; }
.iea-navbtn{ border:2px solid var(--iea-gray); background:#fff; color:var(--iea-gray);
  font-weight:600; padding:12px 24px; border-radius:12px; transition:all .15s; }
.iea-navbtn:hover{ border-color:var(--iea-red); color:var(--iea-red); }
.iea-navbtn.active{ background:var(--iea-red); border-color:var(--iea-red); color:#fff; }
.iea-navbtn .fa, .iea-navbtn .fas{ margin-right:8px; }
.editor-bar{ max-width:520px; margin:0 auto 10px; text-align:center; }
.editor-bar .form-group{ margin-bottom:4px; }
", IEA_RED, IEA_GRAY)

iea_js <- "
(function(){
  function wire(){
    var btns = document.querySelectorAll('.iea-navbtn');
    btns.forEach(function(b){
      b.addEventListener('click', function(){
        btns.forEach(function(x){ x.classList.remove('active'); });
        this.classList.add('active');
      });
    });
    var v = document.getElementById('app-version'); var c = 0;
    if (v) v.addEventListener('click', function(){
      if (++c >= 10) { window.open('https://www.iea.nl','_blank'); c = 0; }
    });
  }
  if (document.readyState !== 'loading') wire();
  else document.addEventListener('DOMContentLoaded', wire);
})();
"

login_ui <- function() {
  div(
    style = "max-width:420px;margin:90px auto;padding:32px;border:1px solid #ddd;border-radius:12px;background:#fff;box-shadow:0 2px 10px rgba(0,0,0,.08);",
    div(
      style = "text-align:center;margin-bottom:24px;",
      img(src = "iea_logo.png", style = "height:52px;margin-bottom:14px;"),
      div(style = "font-size:1.4rem;font-weight:700;color:#54565A;", "IEABank Portal")
    ),
    textInput("login_email", "Email", placeholder = "name@example.org"),
    passwordInput("login_password", "Password"),
    actionButton("login_btn", "Sign in", class = "btn-primary w-100"),
    div(
      style = "text-align:center;margin-top:14px;",
      actionLink("forgot_password_btn", "Forgot password?")
    ),
    uiOutput("login_message")
  )
}

portal_ui <- function() {
  tagList(
    div(
      class = "iea-banner",
      img(src = "iea_logo.png", class = "iea-logo", alt = "IEA"),
      div(class = "iea-title", "Item Bank · Administration"),
      div(class = "iea-version", id = "app-version", paste0("version ", APP_VERSION))
    ),
    div(class = "editor-bar", textOutput("logged_user")),
    div(
      class = "iea-nav",
      actionButton(
        "nav_edit", tagList(icon("table"), "Edit / Delete / Add"),
        class = "iea-navbtn active"
      ),
      actionButton(
        "nav_load", tagList(icon("upload"), "Upload Excel"),
        class = "iea-navbtn"
      ),
      actionButton(
        "change_password_btn", tagList(icon("key"), "Change password"),
        class = "iea-navbtn"
      ),
      actionButton(
        "logout_btn", tagList(icon("sign-out-alt"), "Sign out"),
        class = "iea-navbtn"
      )
    ),
    navset_hidden(
      id = "mode",
      nav_panel(
        "edit",
        layout_sidebar(
          sidebar = sidebar(
            title = "Actions",
            width = 290,
            selectInput(
              "table", "Table",
              choices = TABLE_CHOICES,
              selected = "admin"
            ),
            div(
              style = "display:flex;flex-direction:column;gap:8px;",
              actionButton("add", "Add row", icon = icon("plus"), class = "btn-primary w-100"),
              actionButton("edit", "Edit Selected", icon = icon("pen"), class = "w-100"),
              actionButton("del", "Delete Selected", icon = icon("trash"), class = "btn-outline-danger w-100")
            )
          ),
          card(
            card_header(textOutput("table_title")),
            DTOutput("grid")
          )
        )
      ),
      nav_panel(
        "load",
        card(
          card_header("Massive upload via Excel"),
          p("The template contains only the Core and Scales tables exposed in this Admin GUI."),
          downloadButton(
            "dl_template",
            paste0("Download template (", length(SHEET_SPEC), " sheets)"),
            class = "btn-outline-secondary"
          ),
          hr(),
          fileInput(
            "xlsx", "Upload the Excel file (mass upload)",
            accept = ".xlsx", width = "100%"
          ),
          uiOutput("load_ui")
        )
      )
    )
  )
}

# ============================================================
# UI
# ============================================================
ui <- page_fluid(
  theme = iea_theme,
  tags$head(tags$style(HTML(iea_css))),
  uiOutput("app_ui"),
  tags$script(HTML(iea_js))
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  current_user <- reactiveVal(NULL)
  current_auth <- reactiveVal(NULL)
  login_error <- reactiveVal(NULL)

  output$app_ui <- renderUI({
    if (is.null(current_user())) login_ui() else portal_ui()
  })

  output$login_message <- renderUI({
    req(login_error())
    div(
      class = "text-danger",
      style = "margin-top:14px;text-align:center;",
      login_error()
    )
  })

  observeEvent(input$forgot_password_btn, {
    showModal(
      modalDialog(
        title = "Reset password",
        p("Enter your email address. If it is registered, you will receive a password recovery link."),
        textInput("reset_email", "Email"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("send_reset_btn", "Send recovery email", class = "btn-primary")
        ),
        easyClose = FALSE
      )
    )
  })

  observeEvent(input$send_reset_btn, {
    email <- trimws(input$reset_email %||% "")

    if (!nzchar(email)) {
      showNotification("Enter your email address.", type = "error")
      return()
    }

    try(supabase_send_password_reset(email), silent = TRUE)
    removeModal()
    showNotification(
      "If this email address is registered, a password recovery link has been requested.",
      type = "message", duration = 8
    )
  })

  observeEvent(input$login_btn, {
    login_error(NULL)

    email <- trimws(input$login_email %||% "")
    password <- input$login_password %||% ""

    if (!nzchar(email) || !nzchar(password)) {
      login_error("Enter your email and password.")
      return()
    }

    auth <- supabase_sign_in(email, password)
    if (is.null(auth) || is.null(auth$user$id)) {
      login_error("Invalid email or password.")
      return()
    }

    profile <- get_user_profile(auth$user$id)
    if (nrow(profile) != 1) {
      login_error("This account has no IEABank profile.")
      return()
    }

    if (!isTRUE(profile$active[1])) {
      login_error("This account is inactive. Contact an administrator.")
      return()
    }

    current_user(as.list(profile[1, ]))
    current_auth(auth)
  })

  observeEvent(input$change_password_btn, {
    showModal(
      modalDialog(
        title = "Change password",
        passwordInput("new_password", "New password"),
        passwordInput("new_password_confirm", "Confirm new password"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_password_btn", "Update password", class = "btn-primary")
        ),
        easyClose = FALSE
      )
    )
  })

  observeEvent(input$save_password_btn, {
    new_password <- input$new_password %||% ""
    confirm_password <- input$new_password_confirm %||% ""

    if (nchar(new_password) < 8) {
      showNotification("Your password must contain at least 8 characters.", type = "error")
      return()
    }

    if (!identical(new_password, confirm_password)) {
      showNotification("The passwords do not match.", type = "error")
      return()
    }

    auth <- current_auth()
    if (is.null(auth) || is.null(auth$access_token)) {
      showNotification("Your session has expired. Please sign in again.", type = "error")
      return()
    }

    ok <- supabase_update_password(auth$access_token, new_password)
    if (ok) {
      removeModal()
      showNotification("Password updated successfully.", type = "message")
    } else {
      showNotification(
        "It was not possible to update the password. Please sign in again and retry.",
        type = "error"
      )
    }
  })

  observeEvent(input$logout_btn, {
    current_user(NULL)
    current_auth(NULL)
    login_error(NULL)
  })

  current_user_name <- reactive({
    user <- current_user()
    req(user)
    trimws(paste(user$first_name, user$last_name))
  })

  output$logged_user <- renderText({
    user <- current_user()
    req(user)
    paste0(
      "Signed in as: ", user$first_name, " ", user$last_name,
      " (", user$role, ")"
    )
  })

  observeEvent(input$nav_edit, nav_select("mode", "edit"))
  observeEvent(input$nav_load, nav_select("mode", "load"))

  refresh <- reactiveVal(0)

  meta <- reactive({
    req(input$table)
    META[[input$table]]
  })

  table_data <- reactive({
    refresh()
    req(input$table)
    m <- meta()
    cols <- visible_columns(input$table, m)
    sql <- sprintf(
      "select %s from %s order by %s",
      paste(cols, collapse = ", "),
      input$table,
      paste(m$pk, collapse = ", ")
    )
    dbGetQuery(pool, sql)
  })

  output$grid <- renderDT({
    datatable(
      table_data(),
      selection = "single",
      rownames = FALSE,
      filter = "top",
      options = list(
        dom = "t",
        scrollX = TRUE,
        paging = FALSE,
        scrollY = "65vh",
        scrollCollapse = TRUE
      )
    )
  }, server = TRUE)

  output$table_title <- renderText({
    paste("Editing table:", input$table)
  })

  selected_row <- reactive({
    sel <- input$grid_rows_selected
    if (is.null(sel) || !length(sel)) return(NULL)
    as.list(table_data()[sel, , drop = FALSE])
  })

  require_author <- function() {
    user <- current_user()

    if (is.null(user) || !isTRUE(user$active)) {
      showModal(
        modalDialog(
          "Your session is no longer active. Please sign in again.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return(FALSE)
    }

    TRUE
  }

  show_form <- function(mode) {
    m <- meta()
    row <- if (mode == "edit") selected_row() else NULL

    inputs <- lapply(m$fields, function(f) {
      make_input(
        f,
        value = if (!is.null(row)) row[[f$name]] else NULL,
        mode = mode
      )
    })

    showModal(
      modalDialog(
        title = paste(if (mode == "edit") "Edit" else "Add", input$table),
        do.call(tagList, inputs),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save", "Save", class = "btn-primary")
        ),
        easyClose = FALSE,
        size = "l"
      )
    )

    session$userData$mode <- mode
  }

  observeEvent(input$add, {
    if (require_author()) show_form("add")
  })

  observeEvent(input$edit, {
    if (is.null(selected_row())) {
      showModal(
        modalDialog(
          "Select a row first.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    } else if (require_author()) {
      show_form("edit")
    }
  })

  observeEvent(input$save, {
    m <- meta()
    mode <- session$userData$mode

    writable <- Filter(
      function(f) {
        if (mode == "add") !(m$pk_generated && isTRUE(f$pk)) else !isTRUE(f$pk)
      },
      m$fields
    )

    missing <- Filter(
      function(f) f$required && is.na(read_input(input, f)),
      writable
    )

    if (length(missing)) {
      showModal(
        modalDialog(
          paste(
            "Required columns are missing:",
            paste(vapply(missing, `[[`, "", "name"), collapse = ", ")
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return()
    }

    cols <- vapply(writable, `[[`, "", "name")
    vals <- vapply(writable, function(f) read_input(input, f), "")

    result <- tryCatch({
      if (mode == "add") {
        casts <- vapply(
          seq_along(writable),
          function(i) cast_ph(i, writable[[i]]$type),
          ""
        )

        insert_cols <- cols
        placeholders <- casts
        params <- as.list(vals)

        if (isTRUE(m$audit)) {
          insert_cols <- c(insert_cols, "modified_by")
          placeholders <- c(placeholders, sprintf("$%d", length(params) + 1))
          params <- c(params, current_user_name())
        }

        sql <- sprintf(
          "insert into %s (%s) values (%s)",
          input$table,
          paste(insert_cols, collapse = ", "),
          paste(placeholders, collapse = ", ")
        )

        dbExecute(pool, sql, params = unname(params))
      } else {
        sets <- vapply(
          seq_along(writable),
          function(i) sprintf("%s = %s", cols[i], cast_ph(i, writable[[i]]$type)),
          ""
        )

        params <- as.list(vals)

        if (isTRUE(m$audit)) {
          sets <- c(sets, sprintf("modified_by = $%d", length(params) + 1))
          params <- c(params, current_user_name())
        }

        pk <- m$pk
        pk_start <- length(params)
        where <- paste(
          sprintf("%s = $%d", pk, pk_start + seq_along(pk)),
          collapse = " and "
        )
        pkvals <- vapply(pk, function(p) as.character(selected_row()[[p]]), "")
        params <- c(params, as.list(pkvals))

        sql <- sprintf(
          "update %s set %s where %s",
          input$table,
          paste(sets, collapse = ", "),
          where
        )

        dbExecute(pool, sql, params = unname(params))
      }

      "ok"
    }, error = function(e) conditionMessage(e))

    if (identical(result, "ok")) {
      removeModal()
      refresh(refresh() + 1)
    } else {
      showModal(
        modalDialog(
          title = "Unable to save",
          tags$pre(result),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }
  })

  observeEvent(input$del, {
    if (is.null(selected_row())) {
      showModal(
        modalDialog(
          "Select a row first.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return()
    }

    if (!require_author()) return()

    showModal(
      modalDialog(
        title = "Confirm removal",
        "Delete the selected row? This action cannot be undone.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("del_confirm", "Delete", class = "btn-danger")
        )
      )
    )
  })

  observeEvent(input$del_confirm, {
    m <- meta()
    pk <- m$pk
    where <- paste(
      sprintf("%s = $%d", pk, seq_along(pk)),
      collapse = " and "
    )
    pkvals <- vapply(pk, function(p) as.character(selected_row()[[p]]), "")

    result <- tryCatch({
      dbExecute(
        pool,
        sprintf("delete from %s where %s", input$table, where),
        params = unname(as.list(pkvals))
      )
      "ok"
    }, error = function(e) conditionMessage(e))

    removeModal()

    if (identical(result, "ok")) {
      refresh(refresh() + 1)
    } else {
      showModal(
        modalDialog(
          title = "It wasn't possible to delete it",
          "The table probably has associated records (foreign keys).",
          tags$pre(result),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }
  })

  output$dl_template <- downloadHandler(
    filename = function() "IEABank_mass_upload_template.xlsx",
    content = function(file) writexl::write_xlsx(template_workbook(), file)
  )

  upload <- reactiveVal(NULL)

  observeEvent(input$xlsx, {
    upload(validate_upload(input$xlsx$datapath))
  })

  output$load_ui <- renderUI({
    u <- upload()
    if (is.null(u)) return(NULL)

    if (length(u$errors)) {
      tagList(
        div(class = "text-danger fw-bold", "Nothing will be loaded: fix the Excel file."),
        tags$ul(lapply(u$errors, tags$li))
      )
    } else {
      summary_text <- paste(
        sprintf("%s: %d row(s)", names(u$data), vapply(u$data, nrow, integer(1))),
        collapse = " · "
      )
      tagList(
        div(class = "text-success", paste("Valid Excel file.", summary_text)),
        actionButton("do_load", "Upload to the database", class = "btn-primary")
      )
    }
  })

  observeEvent(input$do_load, {
    if (!require_author()) return()

    u <- upload()
    req(u)

    res <- do_load(u$data, current_user_name())

    if (isTRUE(res$ok)) {
      showModal(
        modalDialog(
          title = "Upload complete",
          paste(sprintf("%s: +%d", names(res$rows), res$rows), collapse = " · "),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      upload(NULL)
      refresh(refresh() + 1)
    } else {
      showModal(
        modalDialog(
          title = "Returned data (no rows were added)",
          tags$pre(res$msg),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }
  })
}

shinyApp(ui, server)
