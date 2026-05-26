library(shiny)
library(ggplot2)
library(shinythemes)
library(MASS) # for mvrnorm

# --- INTERFAȚA UTILIZATOR (UI) ---
ui <- fluidPage(
#   theme = shinytheme("lumen"), disgusting white theme ew 
  theme = shinytheme("darkly"),
  titlePanel("Transformări de Variabile Aleatoare Continue"),
  
  sidebarLayout(
    sidebarPanel(
      # Parametru comun pentru ambele tab-uri
      numericInput("n", "Dimensiunea globală a Eșantionului (n \u2265 10):", 
                   value = 1000, min = 10, max = 1000000, step = 100),
      hr(),
      
      # UI specific pentru Componenta Unidimensională
      conditionalPanel(
        condition = "input.main_tabs == 'Componenta Unidimensionala'",
        h4("Opțiuni"),
        
        # 1. Alegerea Repartiției
        selectInput("dist", "Alege Repartiția lui X:",
                    choices = c("Normală (Gaussiană)" = "norm",
                                "Uniformă" = "unif",
                                "Exponențială" = "exp",
                                "Gamma" = "gamma")),

        withMathJax(uiOutput("dist_function")),
        
        # UI Dinamic pentru parametrii repartiției alese
        uiOutput("dist_params"),
        
        # 3. Alegerea Transformării (5 transformări)
        selectInput("trans", "Alege Transformarea Y = g(X):",
                    choices = c("g(x) = x^2" = "x2",
                                "g(x) = |x|" = "abs",
                                "g(x) = log(x)" = "log",
                                "g(x) = e^x" = "exp",
                                "g(x) = sin(x)" = "sin")),
        
        actionButton("sim", "Generează / Actualizează", class = "btn-primary", style = "width: 100%; margin-top: 15px;")
      ),
      
      # UI specific pentru Componenta Bidimensională
      conditionalPanel(
        condition = "input.main_tabs == 'Componenta bidimensionala'",
        h4("Parametrii Simulării 2D"),
        
        radioButtons("mode_2d", "Modul de generare (X, Y):",
                     choices = c("Independente" = "indep",
                                 "Normală Bidimensională" = "bvnorm")),
        
        # UI pentru Independente
        conditionalPanel(
          condition = "input.mode_2d == 'indep'",
          h5("Repartiția pentru X"),
          selectInput("dist_x_2d", "Repartiția lui X:",
                      choices = c("Normală" = "norm", "Uniformă" = "unif", "Exponențială" = "exp", "Gamma" = "gamma")),
          uiOutput("params_x_2d"),
          
          h5("Repartiția pentru Y"),
          selectInput("dist_y_2d", "Repartiția lui Y:",
                      choices = c("Normală" = "norm", "Uniformă" = "unif", "Exponențială" = "exp", "Gamma" = "gamma")),
          uiOutput("params_y_2d")
        ),
        
        # UI pentru Bivariate Normal
        conditionalPanel(
          condition = "input.mode_2d == 'bvnorm'",
          numericInput("mu_x_2d", "Media lui X (\u03bcX):", value = 0),
          numericInput("mu_y_2d", "Media lui Y (\u03bcY):", value = 0),
          numericInput("sigma_x_2d", "Deviația Std lui X (\u03c3X > 0):", value = 1, min = 0.0001),
          numericInput("sigma_y_2d", "Deviația Std lui Y (\u03c3Y > 0):", value = 1, min = 0.0001),
          sliderInput("rho_2d", "Coeficient de corelație (\u03c1):", min = -0.99, max = 0.99, value = 0, step = 0.01)
        ),
        
        hr(),
        selectInput("trans_2d", "Alege Transformarea Z = h(X, Y):",
                    choices = c("Z = X + Y" = "add",
                                "Z = X - Y" = "sub",
                                "Z = X * Y" = "mul",
                                "Z = sqrt(X^2 + Y^2)" = "euclid")),
        
        actionButton("sim_2d", "Generează / Actualizează 2D", class = "btn-success", style = "width: 100%; margin-top: 15px;")
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",
        tabPanel("Componenta Unidimensionala",
          fluidRow(
            # Coloana pentru X
            column(6,
                   h3("Distribuția Originală X"),
                   plotOutput("plot_x"),
                   br(),
                   h4("Indicatori Statistici pentru X"),
                   tableOutput("stats_x")
            ),
            # Coloana pentru Y
            column(6,
                   h3("Distribuția Transformată Y"),
                   plotOutput("plot_y"),
                   uiOutput("warnings"), # Pentru avertismente (ex. log din numere negative)
                   br(),
                   h4("Indicatori Statistici pentru Y"),
                   tableOutput("stats_y")
            )
          ),
          fluidRow(
            column(12,
                   h3("Interpretare automată:"),
                   tableOutput("automatic_stats")
            )
          )
        ),
        tabPanel("Componenta bidimensionala",
          fluidRow(
            column(6,
                   h3("Scatterplot Perechi (X, Y)"),
                   plotOutput("plot_xy_2d"),
                   tableOutput("stats_xy_2d"),
                   br(),
                   h3("Histograma X"),
                   plotOutput("plot_x_2d"),
                   tableOutput("stats_x_2d")
            ),
            column(6,
                   h3("Histograma Transformării Z = h(X, Y)"),
                   plotOutput("plot_z_2d"),
                   tableOutput("stats_z_2d"),
                   br(),
                   h3("Histograma Y"),
                   plotOutput("plot_y_2d"),
                   tableOutput("stats_y_2d")
            )
          )
        )
      )
    )
  ),
  
  # Pseudo footer for credits
  tags$div(
    style = "text-align: center; margin-top: 20px; padding-top: 20px; border-top: 1px solid #444; margin-bottom: 20px;",
    h5("Credite:", style = "margin-bottom: 15px;"),
    fluidRow(
      column(
        width = 3,
        img(
          src = "mircea2.1.gif",
          width = "100%",
          style = "max-width: 220px; border-radius: 8px; display: block; margin: 0 auto;"
        )
      ),
      column(
        width = 3,
        img(
          src = "Sirghe2.png",
          width = "100%",
          style = "max-width: 220px; border-radius: 8px; display: block; margin: 0 auto;"
        )
      ),
      column(
        width = 3,
        img(
          src = "Bujor2.gif",
          width = "100%",
          style = "max-width: 220px; border-radius: 8px; display: block; margin: 0 auto;"
        )
      ),
      column(
        width = 3,
        img(
          src = "robert2.1.gif",
          width = "100%",
          style = "max-width: 220px; border-radius: 8px; display: block; margin: 0 auto;"
        )
      )
    )
  ),

  tags$head(
    tags$style(HTML("
    #stats_xy_2d table tbody tr:nth-child(n+3) {
      background-color: transparent !important;
    }
  "))
  )
)

# --- LOGICA SERVERULUI ---
server <- function(input, output, session) {
  
  # UI Dinamic pentru parametrii componenti bidimensionale - actori independenti
  output$params_x_2d <- renderUI({
    switch(input$dist_x_2d,
           "norm" = tagList(
             numericInput("mu_x_indep", "Medie X (\u03bc):", value = 0),
             numericInput("sigma_x_indep", "Deviație Standard X (\u03c3 > 0):", value = 1, min = 0.0001)
           ),
           "unif" = sliderInput("range_unif_x", "Interval X [a, b]:", min = -100, max = 100, value = c(-2, 2)),
           "exp" = numericInput("rate_x_indep", "Rată X (\u03bb > 0):", value = 1, min = 0.0001),
           "gamma" = tagList(
             numericInput("shape_x_indep", "Formă X (\u03b1 > 0):", value = 2, min = 0.0001),
             numericInput("rate_gamma_x", "Rată X (\u03bb > 0):", value = 1, min = 0.0001)
           )
    )
  })
  
  output$params_y_2d <- renderUI({
    switch(input$dist_y_2d,
           "norm" = tagList(
             numericInput("mu_y_indep", "Medie Y (\u03bc):", value = 0),
             numericInput("sigma_y_indep", "Deviație Standard Y (\u03c3 > 0):", value = 1, min = 0.0001)
           ),
           "unif" = sliderInput("range_unif_y", "Interval Y [a, b]:", min = -100, max = 100, value = c(-2, 2)),
           "exp" = numericInput("rate_y_indep", "Rată Y (\u03bb > 0):", value = 1, min = 0.0001),
           "gamma" = tagList(
             numericInput("shape_y_indep", "Formă Y (\u03b1 > 0):", value = 2, min = 0.0001),
             numericInput("rate_gamma_y", "Rată Y (\u03bb > 0):", value = 1, min = 0.0001)
           )
    )
  })
  
  # Afișează parametrii potriviți pentru fiecare distribuție
  output$dist_params <- renderUI({
    switch(input$dist,
           "norm" = tagList(
             numericInput("mu", "Medie (\u03bc):", value = 0),
             numericInput("sigma", "Deviație Standard (\u03c3 > 0):", value = 1, min = 0.0001)
           ),
        #    "unif" = tagList(
        #      numericInput("min", "Minim (a):", value = -2),
        #      numericInput("max", "Maxim (b > a):", value = 2)
        #    ),
            "unif" = sliderInput(
                "range_unif",
                "Interval [a, b]:",
                min = -100,
                max = 100,
                value = c(-2, 2)
            ),
           "exp" = tagList(
             numericInput("rate", "Rată (\u03bb > 0):", value = 1, min = 0.0001)
           ),
           "gamma" = tagList(
             numericInput("shape", "Formă (\u03b1 > 0):", value = 2, min = 0.0001),
             numericInput("rate_gamma", "Rată (\u03bb > 0):", value = 1, min = 0.0001)
           )
    )
  })
  output$dist_function <- renderUI({
  formula <- switch(input$dist,
    # "norm"  = "$$f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} \\exp\\!\\left(-\\frac{1}{2}\\left(\\frac{x-\\mu}{\\sigma}\\right)^{\\!2}\\right)$$",
    "norm" = "$$f(x) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}}e^{-\\frac{{(x-\\mu)}^2}{2\\sigma^2}}$$",
    "unif"  = "$$f(x) = \\begin{cases} \\dfrac{1}{b-a} & a \\le x \\le b \\\\ 0 & \\text{altfel} \\end{cases}$$",
    "exp"   = "$$f(x) = \\begin{cases} \\lambda e^{-\\lambda x} & x \\ge 0 \\\\ 0 & \\text{altfel} \\end{cases}$$",
    "gamma" = "$$f(x) = \\begin{cases} \\dfrac{\\lambda^\\alpha}{\\Gamma(\\alpha)}\\, x^{\\alpha-1} e^{-\\lambda x} & x > 0 \\\\ 0 & \\text{altfel} \\end{cases}$$"
    )
    withMathJax(HTML(formula))
  })
  
  # Reactivitate: Generarea datelor declanșată de buton
  sim_data <- eventReactive(input$sim, {
    
    # VALIDAREA DATELOR DE INTRARE
    req(input$n >= 10) # n minim acceptat
    
    x <- numeric(0)
    theory_fun <- NULL
    theory_args <- list()
    
    if (input$dist == "norm") {
      mu_val <- if (is.null(input$mu)) 0 else input$mu
      sigma_val <- if (is.null(input$sigma)) 1 else input$sigma
      validate(need(sigma_val > 0, "Eroare: Deviația standard trebuie să fie strict pozitivă!"))
      x <- rnorm(input$n, mean = mu_val, sd = sigma_val)
      theory_fun <- dnorm
      theory_args <- list(mean = mu_val, sd = sigma_val)
      
    } else if (input$dist == "unif") {
      runif_val <- if (is.null(input$range_unif)) c(-2, 2) else input$range_unif
      validate(need(runif_val[2] > runif_val[1], "Eroare: Maximul trebuie să fie strict mai mare decât minimul!"))
      x <- runif(input$n, min = runif_val[1], max = runif_val[2])
      theory_fun <- dunif
      theory_args <- list(min = runif_val[1], max = runif_val[2])
      
    } else if (input$dist == "exp") {
      rate_val <- if (is.null(input$rate)) 1 else input$rate
      validate(need(rate_val > 0, "Eroare: Rata trebuie să fie strict pozitivă!"))
      x <- rexp(input$n, rate = rate_val)
      theory_fun <- dexp
      theory_args <- list(rate = rate_val)
      
    } else if (input$dist == "gamma") {
      shape_val <- if (is.null(input$shape)) 2 else input$shape
      rate_gamma_val <- if (is.null(input$rate_gamma)) 1 else input$rate_gamma
      validate(
        need(shape_val > 0, "Eroare: Parametrul de formă trebuie să fie > 0!"),
        need(rate_gamma_val > 0, "Eroare: Parametrul de rată trebuie să fie > 0!")
      )
      x <- rgamma(input$n, shape = shape_val, rate = rate_gamma_val)
      theory_fun <- dgamma
      theory_args <- list(shape = shape_val, rate = rate_gamma_val)
    }
    
    trans_val <- if (is.null(input$trans)) "x2" else input$trans
    
    # APLICAREA TRANSFORMĂRII Y = g(X)
    y <- switch(trans_val,
                "x2"  = x^2,
                "abs" = abs(x),
                "log" = suppressWarnings(log(x)), # Va produce NaN pentru x <= 0
                "exp" = exp(x),
                "sin" = sin(x))
    
    list(x = x, y = y, theory_fun = theory_fun, theory_args = theory_args)
  }, ignoreNULL = FALSE) # Se execută o dată la pornirea aplicației
  
  # --- GENERARE DATE PENTRU COMPONENTA BIDIMENSIONALA ---
  sim_data_2d <- eventReactive(input$sim_2d, {
    mode_2d_val <- if(is.null(input$mode_2d)) "indep" else input$mode_2d
    req(input$n >= 10, mode_2d_val)
    
    n <- input$n
    x <- numeric(n)
    y <- numeric(n)
    
    if (mode_2d_val == "indep") {
      dist_x <- if(is.null(input$dist_x_2d)) "norm" else input$dist_x_2d
      dist_y <- if(is.null(input$dist_y_2d)) "norm" else input$dist_y_2d
      
      # Geneare X
      if (dist_x == "norm") {
        mu_x <- if(is.null(input$mu_x_indep)) 0 else input$mu_x_indep
        sigma_x <- if(is.null(input$sigma_x_indep)) 1 else input$sigma_x_indep
        x <- rnorm(n, mean = mu_x, sd = sigma_x)
      } else if (dist_x == "unif") {
        range_x <- if(is.null(input$range_unif_x)) c(-2, 2) else input$range_unif_x
        x <- runif(n, min = range_x[1], max = range_x[2])
      } else if (dist_x == "exp") {
        rate_x <- if(is.null(input$rate_x_indep)) 1 else input$rate_x_indep
        x <- rexp(n, rate = rate_x)
      } else if (dist_x == "gamma") {
        shape_x <- if(is.null(input$shape_x_indep)) 2 else input$shape_x_indep
        rate_g_x <- if(is.null(input$rate_gamma_x)) 1 else input$rate_gamma_x
        x <- rgamma(n, shape = shape_x, rate = rate_g_x)
      }
      
      # Generare Y
      if (dist_y == "norm") {
        mu_y <- if(is.null(input$mu_y_indep)) 0 else input$mu_y_indep
        sigma_y <- if(is.null(input$sigma_y_indep)) 1 else input$sigma_y_indep
        y <- rnorm(n, mean = mu_y, sd = sigma_y)
      } else if (dist_y == "unif") {
        range_y <- if(is.null(input$range_unif_y)) c(-2, 2) else input$range_unif_y
        y <- runif(n, min = range_y[1], max = range_y[2])
      } else if (dist_y == "exp") {
        rate_y <- if(is.null(input$rate_y_indep)) 1 else input$rate_y_indep
        y <- rexp(n, rate = rate_y)
      } else if (dist_y == "gamma") {
        shape_y <- if(is.null(input$shape_y_indep)) 2 else input$shape_y_indep
        rate_g_y <- if(is.null(input$rate_gamma_y)) 1 else input$rate_gamma_y
        y <- rgamma(n, shape = shape_y, rate = rate_g_y)
      }
    } else {
      # Bivariate Normal
      mu_x <- if(is.null(input$mu_x_2d)) 0 else input$mu_x_2d
      mu_y <- if(is.null(input$mu_y_2d)) 0 else input$mu_y_2d
      sigma_x <- if(is.null(input$sigma_x_2d)) 1 else input$sigma_x_2d
      sigma_y <- if(is.null(input$sigma_y_2d)) 1 else input$sigma_y_2d
      rho_val <- if(is.null(input$rho_2d)) 0 else input$rho_2d
      
      mu <- c(mu_x, mu_y)
      cov_xy <- rho_val * sigma_x * sigma_y
      sigma_mat <- matrix(c(sigma_x^2, cov_xy, cov_xy, sigma_y^2), 2, 2)
      
      out <- mvrnorm(n, mu = mu, Sigma = sigma_mat)
      x <- out[, 1]
      y <- out[, 2]
    }
    
    trans_2d_val <- if(is.null(input$trans_2d)) "add" else input$trans_2d
    
    # Transformare Z = h(X, Y)
    z <- switch(trans_2d_val,
                "add" = x + y,
                "sub" = x - y,
                "mul" = x * y,
                "euclid" = sqrt(x^2 + y^2))
                
    data.frame(X = x, Y = y, Z = z)
  }, ignoreNULL = FALSE)
  
  # GRAFICE
  output$plot_xy_2d <- renderPlot({
    df <- sim_data_2d()
    ggplot(df, aes(x = X, y = Y)) +
      geom_point(alpha = 0.5, color = "purple") +
      labs(title = "Scatterplot pentru datele generate (X, Y)") +
      theme_minimal()
  })
  
  output$plot_x_2d <- renderPlot({
    df <- sim_data_2d()
    ggplot(df, aes(x = X)) +
      geom_histogram(fill = "skyblue", color = "black", alpha = 0.7, bins = 30) +
      labs(title = "Distribuția lui X") +
      theme_minimal()
  })
  
  output$plot_y_2d <- renderPlot({
    df <- sim_data_2d()
    ggplot(df, aes(x = Y)) +
      geom_histogram(fill = "lightgreen", color = "black", alpha = 0.7, bins = 30) +
      labs(title = "Distribuția lui Y") +
      theme_minimal()
  })
  
  output$plot_z_2d <- renderPlot({
    df <- sim_data_2d()
    ggplot(df, aes(x = Z)) +
      geom_histogram(fill = "tomato", color = "black", alpha = 0.7, bins = 30) +
      labs(title = "Distribuția lui Z = h(X, Y)") +
      theme_minimal()
  })
  
  # STATISTICI 2D
  output$stats_x_2d <- renderTable({
    df <- sim_data_2d()
    calc_stats(df$X)
  }, digits = 4,striped = TRUE,width = "100%")
  
  output$stats_y_2d <- renderTable({
    df <- sim_data_2d()
    calc_stats(df$Y)
  }, digits = 4,striped = TRUE,width = "100%")
  
  output$stats_z_2d <- renderTable({
    df <- sim_data_2d()
    calc_stats(df$Z)
  }, digits = 4,striped = TRUE,width = "100%")
  
  output$stats_xy_2d <- renderTable({
    df <- sim_data_2d()
    data.frame(
      Indicator = c("Covarianță Empirică (X,Y)", "Coeficient de Corelație (X,Y)", "\u2003", "\u2003", "\u2003", "\u2003", "\u2003", "\u2003"), #unicode spatiere ca sa stea aranjate :)
      Valoare = c(cov(df$X, df$Y), cor(df$X, df$Y), "\u2003", "\u2003", "\u2003", "\u2003", "\u2003", "\u2003")
    )
  }, digits = 4,striped = TRUE,width = "100%")
  
  # Avertisment UI pentru cazurile în care g(x) generează NaN (ex: log(-1))
  output$warnings <- renderUI({
    data <- sim_data()
    if (any(is.na(data$y) | is.infinite(data$y))) {
      HTML("<p style='color: red; font-weight: bold;'>Atenție: Transformarea (ex. logaritm din valori negative) a generat valori nedefinite (NaN) sau infinite. Acestea au fost excluse automat din graficul și statisticile lui Y.</p>")
    }
  })
  
  # Grafic pentru X: Histograma + Densitatea Teoretică + Densitatea Empirică
  output$plot_x <- renderPlot({
    data <- sim_data()
    df <- data.frame(X = data$x)
    
    ggplot(df, aes(x = X)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black", alpha = 0.6) +
      geom_density(color = "blue", linewidth = 1, linetype = "dashed") + # Densitate empirică
      stat_function(fun = data$theory_fun, args = data$theory_args, color = "red", linewidth = 1) + # Densitate teoretică
      labs(subtitle = "Roșu: Densitate Teoretică | Albastru: Densitate Empirică", x = "X", y = "Densitate") +
      theme_minimal(base_size = 14)
  })
  
  # Grafic pentru Y: Histograma + Densitatea Empirică
  output$plot_y <- renderPlot({
    data <- sim_data()
    y_valid <- data$y[is.finite(data$y)] # Eliminăm NaN/Inf pentru a putea trasa graficul
    df <- data.frame(Y = y_valid)
    
    ggplot(df, aes(x = Y)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgreen", color = "black", alpha = 0.6) +
      geom_density(color = "darkgreen", linewidth = 1) +
      labs(subtitle = "Verde Închis: Densitate Empirică a lui Y", x = "Y = g(X)", y = "Densitate") +
      theme_minimal(base_size = 14)
  })
  
  # Funcție ajutătoare pentru calculul indicatorilor
  calc_stats <- function(vec) {
    v <- vec[is.finite(vec)] # Ignorăm NaN/Inf
    data.frame(
      Indicator = c("Medie Empirică", "Dispersie Empirică", "Deviație Standard Empirică", 
                    "Minim", "Q1 (25%)", "Mediană (50%)", "Q3 (75%)", "Maxim"),
      Valoare = c(mean(v), var(v), sd(v), 
                  min(v), quantile(v, 0.25), median(v), quantile(v, 0.75), max(v))
    )
  }
 # Functie ajutatoare pentru calculul coeficientului de asimetrie a lui Pearson
 # https://www.statisticshowto.com/probability-and-statistics/statistics-definitions/pearsons-coefficient-of-skewness/
  
  calc_skewness <- function(vec) {
    v <- vec[is.finite(vec)]
    sk <- 3 * (mean(v) - median(v)) / sd(v)
    return(sk)
  }

  # IF-uri pentru interpretare automata simpla
  #astea genereaza string-uri pe care le injectam in UI
  #cerinta 7
  if_skewness <- function(sk) {
    if(sk > 0.2) {
      return("asimetrică pozitiv")
    } else if(sk < -0.2) {
      return("asimetrică negativ")
    } else {
      return("aproximativ simetrică")
    }
  }

  if_valori_comprimate <- function(max_X, max_Y) {
    if(max_Y < max_X) {
        return("comprimare")
    } else {
        return("extindere")
    }
  }

  if_strict_pos <- function(vec) {
    if(all(vec > 0)) {
      return("valori strict pozitive")
    } else {
      return("valori :)")
    }
  }

  if_accent_extreme <- function(x,y) {
    iqrx <- quantile(x, 3/4) - quantile(x, 1/4)
    iqry <- quantile(y, 3/4) - quantile(y, 1/4)
    if(sum(x > quantile(x, 3/4) + 1.5 * iqrx | x < quantile(x, 1/4) - 1.5 * iqrx) <
       sum(y > quantile(y, 3/4) + 1.5 * iqry | y < quantile(y, 1/4) - 1.5 * iqry)) {
      return("accentuare")
    } else {
      return("diminuare")
    }
  }

  calc_auto <- function(vecx,vecy){
    x <- vecx[is.finite(vecx)]
    y <- vecy[is.finite(vecy)]
    
    skx <- calc_skewness(x)
    sky <- calc_skewness(y)
    data.frame(
      Indicator = c("Distribuția X",
                    "Distribuția Y",
                    "Transformarea a modificat simetria",
                    "Transformarea a influentat valorile mari prin",
                    "Transformarea a produs",
                    "Transformarea a influentat valorile extreme prin"),
      Valoare = c(if_skewness(skx),
                  if_skewness(sky),
                  !if_skewness(sky)==if_skewness(skx),
                  if_valori_comprimate(max(x),max(y)),
                  if_strict_pos(y),
                  if_accent_extreme(x,y)
                  )
      )
  }
  
  
  # Afișare Tabele
  output$stats_x <- renderTable({ calc_stats(sim_data()$x) }, digits = 4, striped = TRUE, hover = TRUE, width = "100%")
  output$stats_y <- renderTable({ calc_stats(sim_data()$y) }, digits = 4, striped = TRUE, hover = TRUE, width = "100%")
  output$automatic_stats <- renderTable({calc_auto(sim_data()$x,sim_data()$y)},digits = 4,striped = TRUE, hover = TRUE,width = "100%")
}

# Rulează aplicația
shinyApp(ui = ui, server = server)