library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(bslib)
library(plotly)
library(scales)
library(tools)

# ═══════════════════════════════════════════════
#  CONFIG — chemin vers les fichiers .txt
# ═══════════════════════════════════════════════
DATA_DIR <- "./Data"   # dossier à côté du app.R

### FUNCTIONS ###
extractData <- function(file){
  df = read.table(file,col.names = c("Trial","Target","MaskerL","MaskerR","Response","RT","ESCU","Angle"))
  df= df%>%
    mutate(participant = file_path_sans_ext(basename(file)),
           angle = as.numeric(gsub(";\\s*$", "", Angle)),
           trial = as.numeric(gsub(",\\s*$", "", Trial)),
           RT = as.numeric(RT),
           ColorResponse = str_split_fixed(Response, " ", 2)[, 1],
           DigitResponse = str_split_fixed(Response, " ", 2)[, 2],
           ColorTarget = str_split_fixed(gsub("\\.wav$", "", df$Target, ignore.case = TRUE), "_",5)[,4],
           DigitTarget = str_split_fixed(gsub("\\.wav$", "", df$Target, ignore.case = TRUE), "_",5)[,5],
           ColorMaskerL = str_split_fixed(gsub("\\.wav$", "", df$MaskerL, ignore.case = TRUE), "_",5)[,4],
           DigitMaskerL = str_split_fixed(gsub("\\.wav$", "", df$MaskerL, ignore.case = TRUE), "_",5)[,5],
           ColorMaskerR = str_split_fixed(gsub("\\.wav$", "", df$MaskerR, ignore.case = TRUE), "_",5)[,4],
           DigitMaskerR = str_split_fixed(gsub("\\.wav$", "", df$MaskerR, ignore.case = TRUE), "_",5)[,5],
           correct = ifelse(ColorResponse == ColorTarget & DigitResponse == DigitTarget, 1,0),
           correctColor = ifelse(ColorResponse == ColorTarget,1,0),
           correctDigit = ifelse(DigitResponse == DigitTarget,1,0))
  df = df %>%
    group_by(participant)%>%
    mutate(Prct_cumul = cumsum(correct) / seq_len(n())*100)%>% ungroup()
  return(df)
}
#data_folder = "C:\\Users\\chamery\\Desktop\\SpatCRM\\Data\\"

load_data_dir <- function(dir) {
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) == 0) return(NULL)
  dfs <- lapply(files, function(f) {
    tryCatch(extractData(f), error = function(e) NULL)
  })
  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) return(NULL)
  bind_rows(dfs)
}

# ═══════════════════════════════════════════════
#  DEMO DATA
# ═══════════════════════════════════════════════
generate_demo <- function() {
  set.seed(42)
  pids <- paste0("P0", 1:5)
  bind_rows(lapply(pids, function(pid) {
    angle <- 30
    bind_rows(lapply(1:32, function(i) {
      correct <- runif(1) > 0.38
      res <- data.frame(
        row = i, angle = angle, trial = i,
        correct = correct, correctColor = correct || runif(1) > 0.5,
        correctDigit = correct || runif(1) > 0.5,
        ESCU = sample(6:13, 1), RT = sample(2800:5500, 1),
        participant = pid, stringsAsFactors = FALSE
      )
      angle <<- max(0, if (correct) angle - 3 else angle + 6)
      res
    }))
  }))
}

# ═══════════════════════════════════════════════
#  HELPERS
# ═══════════════════════════════════════════════
is_pid <- function(pid) length(pid) == 1 && !is.null(pid) && pid != "ALL"

make_pal <- function(pids) {
  cols <- c("#5e9bff","#3ecf8e","#ff5f6d","#b06bff","#f5a623",
            "#00d2d3","#ff9f43","#54a0ff","#5f27cd","#01abc7")
  setNames(rep_len(cols, length(pids)), sort(pids))
}

add_hl <- function(df, pid) {
  df$hl <- if (is_pid(pid)) df$participant == pid else FALSE
  df
}

# ═══════════════════════════════════════════════
#  THEME
# ═══════════════════════════════════════════════
app_theme <- bs_theme(
  version = 5, bg = "#0f1117", fg = "#e8eaf0",
  primary = "#5e9bff", secondary = "#7c5cbf",
  success = "#3ecf8e", warning = "#f5a623", danger = "#ff5f6d",
#  base_font    = "Arial", #font_google("DM Sans"),
#  heading_font = "Arial", # font_google("Space Grotesk"),
  `enable-rounded` = TRUE
)

pdark <- function(fig) {
  fig %>% layout(
    paper_bgcolor = "#161b2c", plot_bgcolor  = "#161b2c",
    font   = list(color = "#e8eaf0"),# family = "Arial", #"DM Sans"),
    legend = list(bgcolor = "#1e2338", bordercolor = "#2a2f45"),
    xaxis  = list(tickfont = list(color = "#7a80a0"), gridcolor = "#2a2f45", zerolinecolor = "#2a2f45"),
    yaxis  = list(tickfont = list(color = "#7a80a0"), gridcolor = "#2a2f45", zerolinecolor = "#2a2f45")
  )
}

CSS <- '
body{background:#0f1117}
.hero{background:linear-gradient(135deg,#1a1f2e,#0f1117 60%,#1a0f2e);
  border-bottom:1px solid #2a2f45;padding:1.6rem 2rem 1.3rem}
.hero-title{font-size:1.85rem;font-weight:700;letter-spacing:-.03em;
  background:linear-gradient(90deg,#5e9bff,#b06bff);
  -webkit-background-clip:text;-webkit-text-fill-color:transparent;margin:0}
.hero-sub{color:#7a80a0;font-size:.86rem;margin-top:.25rem}
.card{background:#161b2c!important;border:1px solid #2a2f45!important;border-radius:12px!important}
.card-body{padding:1.3rem!important}
.card h5{color:#5e9bff;font-size:.73rem;text-transform:uppercase;
  letter-spacing:.12em;font-weight:600;margin-bottom:.9rem}
.stat-box{background:#1e2338;border:1px solid #2a2f45;border-radius:10px;
  padding:.85rem;text-align:center;margin-bottom:.5rem}
.stat-val{font-size:1.6rem;font-weight:700;color:#5e9bff;line-height:1}
.stat-lbl{font-size:.68rem;color:#7a80a0;text-transform:uppercase;letter-spacing:.1em;margin-top:.25rem}
.hl-box{background:linear-gradient(135deg,#1a1f35,#201535);
  border:1px solid #5e9bff44;border-radius:12px;padding:1.1rem;margin-bottom:1rem}
.nav-pills .nav-link{color:#7a80a0!important;font-size:.83rem;border-radius:8px;padding:.35rem .9rem}
.nav-pills .nav-link.active{background:#5e9bff22!important;color:#5e9bff!important;
  border:1px solid #5e9bff44!important}
.btn-update{background:#5e9bff22!important;color:#5e9bff!important;
  border:1px solid #5e9bff55!important;border-radius:8px!important;
  font-size:.82rem!important;font-weight:600!important;padding:.4rem 1.1rem!important}
.btn-update:hover{background:#5e9bff44!important}
.selectize-input{background:#1e2338!important;border-color:#2a2f45!important;
  color:#e8eaf0!important;border-radius:8px!important}
.selectize-dropdown{background:#1e2338!important;border-color:#2a2f45!important}
.selectize-dropdown-content .option{color:#e8eaf0!important}
.selectize-dropdown-content .option:hover{background:#2a2f45!important}
.shiny-input-container label{color:#7a80a0;font-size:.81rem}
hr{border-color:#2a2f45}
'

# ═══════════════════════════════════════════════
#  UI
# ═══════════════════════════════════════════════
ui <- page_fluid(
  theme = app_theme,
  tags$head(tags$style(HTML(CSS))),
  
  # ── HEADER ──
  div(class = "hero",
      fluidRow(
        column(8,
               tags$h1(class = "hero-title", "🎧 Intelligibilité & Effort d'Écoute"),
               div(class = "hero-sub", "3 locuteurs · Cible en face · Masqueurs spatialisés · Coordinate Response Measure")
        ),
        column(4, style = "text-align:right;padding-top:.5rem",
               br(), br(),
               actionButton("update_btn", "⟳  Mettre à jour", class = "btn-update"),
               br(),
               tags$small(style = "color:#7a80a0;font-size:.7rem",
                          paste0("Dossier : ", DATA_DIR))
        )
      )
  ),
  
  div(style = "padding:1.3rem",
      
      # ── KPIs ──
      uiOutput("kpis"),
      
      # ── Selector ──
      div(class = "hl-box",
          fluidRow(
            column(4,
                   selectInput("sel_pid", "🔍 Mettre un participant en avant",
                               choices = c("— Groupe entier —" = "ALL"), width = "100%")
            ),
            column(8, uiOutput("pid_summary"))
          )
      ),
      
      # ── TABS ──
      navset_pill(
        
        # 1 — Intelligibilité
        nav_panel("⚡ Intelligibilité",
                  fluidRow(
                    column(6, div(class = "card", div(class = "card-body",
                                                      tags$h5("Au fil des essais"),
                                                      plotlyOutput("p_intell_trial", height = "320px")
                    ))),
                    column(6, div(class = "card", div(class = "card-body",
                                                      tags$h5("En fonction de l'angle"),
                                                      plotlyOutput("p_intell_angle", height = "320px")
                    )))
                  ),
                  fluidRow(style = "padding-top:1rem",
                    column(12,div(class="card",div(class = "card-body",
                                                   tags$h5("Au fil des essais"),
                                                   plotlyOutput("p_angle_trials",height="320px")
                    )))
                  )
        ),
        # 2 — Effort ESCU
        nav_panel("🧠 Effort (ESCU)",
                  fluidRow(
                    column(6, div(class = "card", div(class = "card-body",
                                                      tags$h5("Au fil des essais"),
                                                      plotlyOutput("p_escu_trial", height = "320px")
                    ))),
                    column(6, div(class = "card", div(class = "card-body",
                                                      tags$h5("En fonction de l'angle"),
                                                      plotlyOutput("p_escu_angle", height = "320px")
                    )))
                  ),
                  fluidRow(style = "padding-top:1rem",
                              column(12, div(class = "card", div(class = "card-body",
                                                                tags$h5("Intelligibilité en fonction de l'effort d'écoute"),
                                                               plotlyOutput("p_intell_escu", height = "420px")
                           )))
                        )
        ),
        # 4 — Erreurs couleur / chiffre
        nav_panel("🎯 Analyse des erreurs",
                  fluidRow(
                    column(12, div(class = "card", div(class = "card-body",
                                                      tags$h5("Taux d'erreur global par participant (couleur vs chiffre)"),
                                                      plotlyOutput("p_err_type", height = "340px")
                    )))
                  )
        ),
        
        # 5 — RT vs ESCU
        nav_panel("⏱ Temps de réaction",
                  fluidRow(
                    column(6, div(class = "card", div(class = "card-body",
                                                      tags$h5("Temps de réaction vs ESCU"),
                                                      plotlyOutput("p_rt_escu", height = "340px")
                    ))),
                    column(6, div(class = "card", div(class = "card-body",
                                                      tags$h5("Temps de réaction au fil des essais"),
                                                      plotlyOutput("p_rt_trial", height = "340px")
                    )))
                  )
        )
      )
  )
)

# ═══════════════════════════════════════════════
#  SERVER
# ═══════════════════════════════════════════════
server <- function(input, output, session) {
  
  # ── Data reactive (triggered by Update button) ──
  data_r <- reactiveVal(NULL)
  
  load_fresh <- function() {
    df <- load_data_dir(DATA_DIR)
    
    if (is.null(df)) df <- generate_demo()
    data_r(df)
  }
  
  # Load on start
  load_fresh()
  
  # Reload on button
  observeEvent(input$update_btn, { load_fresh() })
  
  # ── Participant list ──
  observe({
    df   <- data_r(); req(df)
    pids <- sort(unique(df$participant))
    updateSelectInput(session, "sel_pid",
                      choices  = c("— Groupe entier —" = "ALL", setNames(pids, pids)),
                      selected = "ALL")
  })
  
  # ── Stats ──
  stats_r <- reactive({
    df <- data_r(); req(df)
    df %>% group_by(participant) %>%
      summarise(
        n_trials      = n(),
        pct_correct   = round(mean(correct, na.rm = TRUE) * 100, 1),
        mean_escu     = round(mean(ESCU, na.rm = TRUE), 1),
        mean_rt       = round(mean(RT, na.rm = TRUE)),
        trials_to_zero = sum(cumsum(angle == 0) == 0),  # essais avant premier 0°
        .groups = "drop"
      )
  })
  
  # ── KPIs ──
  output$kpis <- renderUI({
    df <- data_r(); req(df)
    st <- stats_r()
    mk <- function(val, lbl, col = "#5e9bff")
      div(class = "stat-box",
          div(class = "stat-val", style = paste0("color:", col), val),
          div(class = "stat-lbl", lbl))
    
    n_files <- length(list.files(DATA_DIR, pattern="\\.txt$"))
    src_lbl <- if (n_files > 0)
      paste0(n_files, " fichier", if(n_files>1)"s" else "", " chargé", if(n_files>1)"s")
    else "Données démo"
    
    div(style = "margin-bottom:1rem",
        fluidRow(
          column(2, mk(length(unique(df$participant)), "Participants",    "#5e9bff")),
          column(2, mk(paste0(round(mean(st$pct_correct),1),"%"), "Intell. moy.",  "#3ecf8e")),
          column(2, mk(round(mean(st$mean_escu),1),    "ESCU moyen",      "#b06bff")),
          column(2, mk(paste0(round(mean(st$mean_rt)/1000,1),"s"), "RT moyen", "#f5a623")),
          column(2, mk(round(mean(st$trials_to_zero)), "Essais → 0°",    "#ff5f6d")),
          #nombre d'erreurs moyen
          column(2, div(class = "stat-box",
                        div(class = "stat-val", style = "font-size:.95rem;color:#7a80a0", src_lbl),
                        div(class = "stat-lbl", "Source")))
  
        )
    )
  })
  
  # ── Participant summary ──
  output$pid_summary <- renderUI({
    pid <- input$sel_pid
    if (!is_pid(pid)) return(
      div(style = "color:#7a80a0;font-size:.83rem;padding-top:.55rem",
          "Sélectionnez un participant pour mettre sa courbe en avant sur tous les graphiques."))
    st <- stats_r()
    s  <- st[st$participant == pid, ]
    if (nrow(s) == 0) return(NULL)
    fluidRow(
      column(3, div(class="stat-box", div(class="stat-val",style="color:#3ecf8e",paste0(s$pct_correct,"%")), div(class="stat-lbl","Intelligibilité"))),
      column(3, div(class="stat-box", div(class="stat-val",style="color:#b06bff",s$mean_escu),               div(class="stat-lbl","ESCU moyen"))),
      column(3, div(class="stat-box", div(class="stat-val",style="color:#f5a623",paste0(round(s$mean_rt/1000,1),"s")), div(class="stat-lbl","RT moyen"))),
      column(3, div(class="stat-box", div(class="stat-val",style="color:#ff5f6d",s$trials_to_zero),          div(class="stat-lbl","Essais → 0°")))
    )
  })
  
  # ══════════════════════════════════
  #  SHARED ggplot2 THEME
  # ══════════════════════════════════

  gg_theme <- function() {
    theme_minimal(base_family = "sans") +
      theme(
        plot.background   = element_rect(fill = "#161b2c", color = NA),
        panel.background  = element_rect(fill = "#161b2c", color = NA),
        panel.grid.major  = element_line(color = "#2a2f45"),
        panel.grid.minor  = element_blank(),
        text              = element_text(color = "#e8eaf0"),
        axis.text         = element_text(color = "#7a80a0"),
        legend.background = element_rect(fill = "#1e2338", color = NA),
        legend.key        = element_rect(fill = "#1e2338"),
        legend.title      = element_blank()
      )
  }
  
  prep_hl <- function(df, pid) {
    df %>% 
      mutate(
        line_alpha  =ifelse(participant == pid, 1, 0.1) ,
        line_size   = ifelse(participant == pid, 0.9, 0.5) ,
        point_size   = ifelse(participant == pid, 2, 1) 
      )
  }
  
  # ══════════════════════════════════
  #  TAB 1 — Intelligibilité
  # ══════════════════════════════════
  
  # PLOT INTELLIGIBILITE ALONG TRIALS
  output$p_intell_trial <- renderPlotly({
    df  <- data_r(); req(df); pid <- input$sel_pid
    df = prep_hl(df,pid)
    pal <- make_pal(unique(df$participant))
    p = ggplot(df, aes(trial,Prct_cumul,
                       group = participant, color = participant,
                       text = paste0(participant,
                                     "\nEssai : ", trial,"\nIntell. : ", round(Prct_cumul, 1), "%"))) +
      geom_point(aes(size = point_size,alpha = line_alpha))+
      geom_line(aes(linewidth = line_size,alpha = line_alpha))+
      stat_summary(data = df, aes(x = trial, y = Prct_cumul, group = 1),
                   geom="line",fun = "mean",color = "#f5a623", size = 1, linetype = "dashed",inherit.aes = FALSE)+
      scale_color_manual(values = pal) +
      scale_size_identity()+
      scale_alpha_identity() +     
      scale_linewidth_identity() +
      labs(x = "Essai", y = "Intelligibilité cumulée (%)") +
      coord_cartesian(ylim = c(0, 105)) +
      gg_theme()+
      theme(legend.position="none")
    
    
    ggplotly(p, tooltip = "text") %>% pdark()
    
  })
  
  # PLOT INTELLIGIBILITY PER ANGLE 
  output$p_intell_angle <- renderPlotly({
    df  <- data_r(); req(df); pid <- input$sel_pid
    pal <- make_pal(unique(df$participant))
    
  # mean perf per angle per participant
      df2 <- df %>%
      group_by(participant, angle) %>%
      summarise(pct = mean(correct, na.rm = TRUE) * 100, .groups = "drop") %>%
      arrange(participant, angle) %>%
      prep_hl(pid)
    
    p <- ggplot(df2, aes(x = angle, y = pct,
                         group = participant, color = participant,
                         text = paste0(participant,
                                       "\nAngle : ", angle,"\nIntell. moy : ", round(pct, 1), "%"))) +
      geom_point(aes(size = point_size,alpha = line_alpha))+
      geom_line(aes(linewidth = line_size,alpha = line_alpha))+
      stat_summary(data = df2, aes(x = angle, y = pct, group = 1),
                   fun = "mean", geom = "line",
                   color = "#f5a623", linewidth = 1, linetype = "dashed",
                   inherit.aes = FALSE) +
      scale_color_manual(values = pal) +
      scale_alpha_identity() +
      scale_linewidth_identity() +
      scale_size_identity() +
      labs(x = "Angle de séparation (°)", y = "Intelligibilité (%)") +
      coord_cartesian(ylim = c(0, 105)) +
      gg_theme()+
      theme(legend.position="none")
    
    ggplotly(p, tooltip = "text") %>% pdark()
  })
  
  # Plot Angle au fil des essais 
  output$p_angle_trials <- renderPlotly({
    df  <- data_r(); req(df); pid <- input$sel_pid
    pal <- make_pal(unique(df$participant))
    df = prep_hl(df,pid)
    
    p = ggplot(df, aes(x = trial, y = angle,
                         group = participant, color = participant,
                         text = paste0(participant,
                                       "\nAngle : ", angle,"\nEssai : ", trial))) +
      geom_point(aes(size = point_size,alpha = line_alpha))+
      geom_line(aes(linewidth = line_size,alpha = line_alpha))+
      stat_summary(data = df, aes(x = trial, y = angle, group = 1),
                   fun = "mean", geom = "line",
                   color = "#f5a623", linewidth = 1, linetype = "dashed",
                   inherit.aes = FALSE) +
      scale_color_manual(values = pal) +
      scale_alpha_identity() +
      scale_linewidth_identity() +
      scale_size_identity() +
      labs(x = "Essais", y = "Angle de séparation (°)") +
      coord_cartesian(ylim = c(0, 90)) +
      gg_theme()
    
    ggplotly(p, tooltip = "text") %>% pdark()
  })
  
  # ══════════════════════════════════
  #  TAB 2 — ESCU
  # ══════════════════════════════════
  
  # PLOT ESCU PER TRIAL
  output$p_escu_trial <- renderPlotly({
    df  <- data_r(); req(df); pid <- input$sel_pid
    pal <- make_pal(unique(df$participant))
    
    df2 <- df %>% prep_hl(pid)
    
    p <- ggplot(df2, aes(x = trial, y = ESCU,
                         group = participant, color = participant,
                         text = paste0(participant,
                                       "\nEssai : ", trial,"\nEffort. : ", ESCU))) +
      geom_point(aes(size = point_size,alpha = line_alpha))+
      geom_line(aes(linewidth = line_size,alpha = line_alpha))+
      stat_summary(data = df2, aes(x = trial, y = ESCU, group = 1),
                   fun = "mean", geom = "line",
                   color = "#f5a623", linewidth = 1, linetype = "dashed",
                   inherit.aes = FALSE) +
      scale_color_manual(values = pal) +
      scale_alpha_identity() +
      scale_linewidth_identity() +
      scale_size_identity() +
      labs(x = "Essai", y = "Effort d'écoute subjectif") +
      coord_cartesian(ylim = c(0, 14)) +
      gg_theme()+
      theme(legend.position="none")
    
    
    ggplotly(p, tooltip = "text") %>% pdark() 
  })
  

  
  # PLOT ESCU PER ANGLE
  output$p_escu_angle <- renderPlotly({
    df  <- data_r(); req(df); pid <- input$sel_pid
    pal <- make_pal(unique(df$participant))
    
    # Mean ESCU per angle
    df2 <- df %>%
      group_by(participant, angle) %>%
      summarise(escu = mean(ESCU, na.rm = TRUE), .groups = "drop") %>%
      arrange(participant, angle) %>%
      prep_hl(pid)
    
    p <- ggplot(df2, aes(x = angle, y = escu,
                         group = participant, color = participant,
                         alpha = line_alpha, linewidth = line_size,
                         text = paste0(participant,
                                       "\nAngle : ", angle, "°",
                                       "\nESCU moy. : ", round(escu, 1)))) +
      geom_point(aes(size = point_size,alpha = line_alpha))+
      geom_line(aes(linewidth = line_size,alpha = line_alpha))+
      #geom_smooth(data = df2, aes(x = angle, y = escu, group = 1),
       #           method = "loess", formula = y ~ x, se = TRUE,
        #          color = "#f5a623", fill = "#f5a62330",
         #         linewidth = 1.2, inherit.aes = FALSE) +
      stat_summary(data = df2, aes(x = angle, y = escu, group = 1),
                   fun = "mean", geom = "line",
                   color = "#f5a623", linewidth = 1, linetype = "dashed",
                   inherit.aes = FALSE)+
      scale_color_manual(values = pal) +
      scale_alpha_identity() +
      scale_linewidth_identity() +
      scale_size_identity() +
      labs(x = "Angle de séparation (°)", y = "ESCU moyen") +
      coord_cartesian(ylim = c(0, 14)) +
      gg_theme()+
      theme(legend.position="none")
    
    
    ggplotly(p, tooltip = "text") %>% pdark()
  })
  
  # ══════════════════════════════════
  #  TAB 3 — Intelligibilité vs ESCU
  # ══════════════════════════════════
  
  output$p_intell_escu <- renderPlotly({
    df  <- data_r(); req(df); pid <- input$sel_pid
    pal <- make_pal(unique(df$participant))
    
    # One row per participant: mean intelligibility vs mean ESCU
    st <- df %>%
      group_by(participant) %>%
      summarise(pct  = mean(correct, na.rm = TRUE) * 100,
                escu = mean(ESCU, na.rm = TRUE),
                .groups = "drop")%>%
      mutate(
                  line_alpha  =ifelse(participant == pid, 1, 0.3) ,
                  line_size   = ifelse(participant == pid, 1.25, 0.6) ,
                  point_size   = ifelse(participant == pid, 2.5, 1) 
                )
    
    p <- ggplot(st, aes(x = pct, y = escu,
                        color = participant,
                        text = paste0(participant,
                                      "\nIntell. : ", round(pct, 1), "%",
                                      "\nESCU moy. : ", round(escu, 1)))) +
  geom_point(aes(size = point_size,alpha = line_alpha))+
      geom_line(aes(linewidth = line_size,alpha = line_alpha))+
          geom_text(aes(label = participant), vjust = -1.2, size = 3,
                color = "#e8eaf0", show.legend = FALSE) +
      #geom_smooth(data = st, aes(x = pct, y = escu, group = 1),
       #           method = "lm", formula = y ~ x, se = TRUE,
        #          color = "#b06bff", fill = "#b06bff30",
         #         linewidth = 1, inherit.aes = FALSE) +
      stat_summary(data = st, aes(x = pct, y = escu, group = 1),
                   fun = "mean", geom = "line",
                   color = "#f5a623", linewidth = 1, linetype = "dashed",
                   inherit.aes = FALSE)+
      scale_color_manual(values = pal) +
      scale_alpha_identity() +
      scale_size_identity() +
      labs(x = "Intelligibilité (%)", y = "ESCU moyen") +
      coord_cartesian(ylim = c(0, 14)) +
      gg_theme()
    
    ggplotly(p, tooltip = "text") %>% pdark()
  })
  
  # ══════════════════════════════════
  #  TAB 4 — Erreurs couleur / chiffre
  # ══════════════════════════════════
  
  classify_errors <- function(df,pid) {
    df %>%
      filter(!is.na(correctColor) & !is.na(correctDigit)) %>%
      mutate(
        err_type = case_when(
          correctColor &  correctDigit ~ "Correct",
          correctColor & !correctDigit ~ "Erreur chiffre",
          !correctColor &  correctDigit ~ "Erreur couleur",
          !correctColor & !correctDigit ~ "Double erreur"
        ))
  }
  
  pal_err <- c(
    "Correct"        = "#3ecf8e",
    "Erreur couleur" = "#f5a623",
    "Erreur chiffre" = "#ff5f6d",
    "Double erreur"  = "#b06bff"
  )
  
  # Stacked bars — ggplot2 handles stacking natively with position = "stack"
  output$p_err_type <- renderPlotly({
    df  <- data_r(); req(df); pid <- input$sel_pid
    df2 <- classify_errors(df)
    
    st <- df2 %>%
      group_by(participant, err_type) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(participant) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup() %>%
      mutate(err_type = factor(err_type, levels = names(pal_err)))
    
    st_bg <- if (is_pid(pid)) filter(st, participant != pid) else st
    st_hl <- if (is_pid(pid)) filter(st, participant == pid) else st[0, ]
    
    p <- ggplot(data = st_bg, aes(x = participant, y = pct, fill = err_type,
                                  text = paste0(participant, "\n", err_type, " : ", round(pct, 1), "%"))) +
      geom_col(               position = "stack", color = "#0f1117", linewidth = 0.3, alpha = 0.6) +
      geom_col(data = st_hl, aes(x = participant, y = pct, fill = err_type),
               position = "stack", color = "#0f1117", linewidth = 0.3, alpha = 0.95) +
      scale_fill_manual(values = pal_err) +
      labs(x = "Participant", y = "%", fill = "") +
      coord_cartesian(ylim = c(0, 101)) +
      gg_theme()
    
    ggplotly(p, tooltip = "text") %>%
      pdark()#%>%layout(legend = list(orientation = "h", y = -0.22))
  })
  
  
  
  # ══════════════════════════════════
  #  TAB 5 — Temps de réaction
  # ══════════════════════════════════
  
  output$p_rt_escu <- renderPlotly({
    df  <- data_r(); req(df); pid <- input$sel_pid
    pal <- make_pal(unique(df$participant))
    
    df2 <- df %>%
      mutate(RT_s = RT / 1000) %>%
      prep_hl(pid)
    
    p <- ggplot(df2, aes(x = ESCU, y = RT_s,
                         color = participant,
                         alpha = line_alpha, size = line_size,
                         text = paste0(participant,
                                       "\nEssai : ", trial,
                                       "\nESCU : ", ESCU,
                                       "\nRT : ", round(RT_s, 2), "s"))) +
      geom_point() +
      geom_smooth(data = df2, aes(x = ESCU, y = RT_s, group = 1),
                  method = "loess", formula = y ~ x, se = FALSE,
                  color = "#f5a623", linewidth = 1, linetype = "dashed",
                  inherit.aes = FALSE) +
      scale_color_manual(values = pal) +
      
      scale_alpha_identity() +
      scale_size_identity() +
      labs(x = "ESCU (effort subjectif)", y = "Temps de réaction (s)") +
      gg_theme()
    
    ggplotly(p, tooltip = "text") %>% pdark()
  })
  
  #PLOT RT PER INTELL
  output$p_rt_intell <- renderPlotly({
    df  <- data_r(); req(df); pid <- input$sel_pid
    pal <- make_pal(unique(df$participant))
    df2 <- df %>%
      mutate(RT_s = RT / 1000) %>%
      prep_hl(pid)
    
    p <- ggplot(df2, aes(x = correct, y = RT_s,
                         color = participant,
                         alpha = line_alpha, size = line_size,
                         text = paste0(participant,
                                       "\nEssai : ", trial,
                                       "\nESCU : ", ESCU,
                                       "\nRT : ", round(RT_s, 2), "s"))) +
      geom_point() +
      geom_smooth(data = df2, aes(x = ESCU, y = RT_s, group = 1),
                  method = "loess", formula = y ~ x, se = TRUE,
                  color = "#f5a623", fill = "#f5a62330",
                  linewidth = 1.2, inherit.aes = FALSE) +
      scale_color_manual(values = pal) +
      scale_alpha_identity() +
      scale_size_identity() +
      labs(x = "ESCU (effort subjectif)", y = "Temps de réaction (s)") +
      gg_theme()
    
    ggplotly(p, tooltip = "text") %>% pdark()
  })
  
  #PLOT RT TRIALS
  output$p_rt_trial <- renderPlotly({
    df  <- data_r(); req(df); pid <- input$sel_pid
    pal <- make_pal(unique(df$participant))
    
    df2 <- df %>%
      mutate(RT_s = RT / 1000) %>%
      prep_hl(pid)
    
    p <- ggplot(df2, aes(x = trial, y = RT_s,
                         group = participant, color = participant,
                         alpha = line_alpha, linewidth = line_size,
                         text = paste0(participant,
                                       "\nEssai : ", trial,
                                       "\nRT : ", round(RT_s, 2), "s"))) +
      geom_line() +
      stat_summary(data = df2, aes(x = trial, y = RT_s, group = 1),
                   fun = "mean", geom = "line",
                   color = "#f5a623", linewidth = 1.2, linetype = "dashed",
                   inherit.aes = FALSE) +
      scale_color_manual(values = pal) +
      scale_alpha_identity() +
      scale_linewidth_identity() +
      labs(x = "Essai", y = "Temps de réaction (s)") +
      gg_theme()
    
    ggplotly(p, tooltip = "text") %>% pdark()
  })
  
}

shinyApp(ui, server)