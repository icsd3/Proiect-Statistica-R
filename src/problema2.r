library(shiny)
library(ggplot2)
library(shinythemes)

# --- INTERFAȚA UTILIZATOR (UI) ---
ui <- fluidPage(
#   theme = shinytheme("lumen"), disgusting white theme ew 
  theme = shinytheme("darkly"),
  titlePanel("Transformări de Variabile Aleatoare Continue"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parametrii Simulării"),
      
      # 1. Alegerea Repartiției
      selectInput("dist", "Alege Repartiția lui X:",
                  choices = c("Normală (Gaussiană)" = "norm",
                              "Uniformă" = "unif",
                              "Exponențială" = "exp",
                              "Gamma" = "gamma")),

      textOutput("dist_function"),
      
      # UI Dinamic pentru parametrii repartiției alese
      uiOutput("dist_params"),
      
      # 2. Dimensiunea Eșantionului
      numericInput("n", "Dimensiunea Eșantionului (n):", 
                   value = 1000, min = 10, max = 1000000, step = 100),
      
      # 3. Alegerea Transformării (5 transformări)
      selectInput("trans", "Alege Transformarea Y = g(X):",
                  choices = c("g(x) = x^2" = "x2",
                              "g(x) = |x|" = "abs",
                              "g(x) = log(x)" = "log",
                              "g(x) = e^x" = "exp",
                              "g(x) = sin(x)" = "sin")),
      
      actionButton("sim", "Generează / Actualizează", class = "btn-primary", style = "width: 100%; margin-top: 15px;")
    ),
    
    mainPanel(
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
      column(6,
             h3("Interpretare automată:"),
             tableOutput("automatic_stats")
      )
    )
  )
)

# --- LOGICA SERVERULUI ---
server <- function(input, output, session) {
  
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
  output$dist_function <- renderText({
    switch(input$dist,
           "norm" = paste("PDF Normală: f(x) =", "1/(σ√(2π)) * exp(-0.5 * ((x - μ)/σ)^2)"),
           "unif" = paste("PDF Uniformă: f(x) =", "1/(b - a) pentru a ≤ x ≤ b, altfel 0"),
           "exp" = paste("PDF Exponențială: f(x) =", "λ * exp(-λx) pentru x ≥ 0, altfel 0"),
           "gamma" = paste("PDF Gamma: f(x) =", "(λ^α / Γ(α)) * x^(α-1) * exp(-λx) pentru x > 0, altfel 0")
    )
  })
  
  # Reactivitate: Generarea datelor declanșată de buton
  sim_data <- eventReactive(input$sim, {
    
    # VALIDAREA DATELOR DE INTRARE
    req(input$n >= 10) # n minim acceptat
    
    x <- numeric(0)
    theory_fun <- NULL
    theory_args <- list()
    
    if (input$dist == "norm") {
      validate(need(input$sigma > 0, "Eroare: Deviația standard trebuie să fie strict pozitivă!"))
      x <- rnorm(input$n, mean = input$mu, sd = input$sigma)
      theory_fun <- dnorm
      theory_args <- list(mean = input$mu, sd = input$sigma)
      
    } else if (input$dist == "unif") {
      validate(need(input$range_unif[2] > input$range_unif[1], "Eroare: Maximul trebuie să fie strict mai mare decât minimul!"))
    #   x <- runif(input$n, min = input$min, max = input$max)
        x <- runif(input$n, min = input$range_unif[1], max = input$range_unif[2])
      theory_fun <- dunif
    #   theory_args <- list(min = input$min, max = input$max)
        theory_args <- list(min = input$range_unif[1], max = input$range_unif[2])
      
    } else if (input$dist == "exp") {
      validate(need(input$rate > 0, "Eroare: Rata trebuie să fie strict pozitivă!"))
      x <- rexp(input$n, rate = input$rate)
      theory_fun <- dexp
      theory_args <- list(rate = input$rate)
      
    } else if (input$dist == "gamma") {
      validate(
        need(input$shape > 0, "Eroare: Parametrul de formă trebuie să fie > 0!"),
        need(input$rate_gamma > 0, "Eroare: Parametrul de rată trebuie să fie > 0!")
      )
      x <- rgamma(input$n, shape = input$shape, rate = input$rate_gamma)
      theory_fun <- dgamma
      theory_args <- list(shape = input$shape, rate = input$rate_gamma)
    }
    
    # APLICAREA TRANSFORMĂRII Y = g(X)
    y <- switch(input$trans,
                "x2"  = x^2,
                "abs" = abs(x),
                "log" = suppressWarnings(log(x)), # Va produce NaN pentru x <= 0
                "exp" = exp(x),
                "sin" = sin(x))
    
    list(x = x, y = y, theory_fun = theory_fun, theory_args = theory_args)
  }, ignoreNULL = FALSE) # Se execută o dată la pornirea aplicației
  
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