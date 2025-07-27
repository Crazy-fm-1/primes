library(shiny)
library(RcppAlgos)
library(primes)
library(tidyverse)
library(scales)
library(bslib)

ui <- page_navbar(
  nav_panel(
  "Prime number functions",
  fluidRow(
    column(6,
           selectInput(
             "func",
             "Choose a function",
             choices = c("--", "Prime-counting function π(n)", "Euler's totient function φ(n)"),
             selected = "--"
           )),
    column(6,
           numericInput(
             "n",
             "n",
             value = 10,
             min = 1
           )
    )
  ),
  conditionalPanel(
    condition = "!(Number.isInteger(input.n)) || input.n < 1 || input.n > 15000",
    p("Invalid input", style = "color: red;")
),
  conditionalPanel(
    condition = "input.func != '--' && input.n >= 1 && Number.isInteger(input.n) && input.n <= 15000",
    plotOutput("plot")
  ),
  conditionalPanel(
    withMathJax(),
    condition = "input.func == 'Prime-counting function π(n)' && input.n >= 1 && Number.isInteger(input.n) && input.n <= 15000",
    htmlOutput("pi_value"),
    p("The prime-counting function, also referred to as π(n), counts the number of prime numbers less than or equal to a real number n.
    The prime number theorem states: $$\\lim_{n \\to \\infty} \\frac{π(n)}{\\frac{n}{ln(n)}} = 1$$ This is equivalent to:
    $$π(n) \\sim \\frac{n}{ln(n)}$$ Thus, n/ln(n) approximates π(n) for large values of n. $$ $$")
  ),
  conditionalPanel(
    condition = "input.func == \"Euler's totient function φ(n)\" && input.n >= 1 && Number.isInteger(input.n) && input.n <= 15000",
    htmlOutput("phi_value"),
    p("Euler's φ function counts the number of positive integers less than or equal to a positive integer n that are coprime to n.
      Two integers are considered coprime, if their greatest common divisor is 1. φ(n) is referred to as the totient of n. n being a
      prime number is equivalent to φ(n) = n - 1. $$ $$")
  )
),
  nav_panel(
    "Prime factorization",
    numericInput(
      "n1",
      "Enter a positive integer",
      value = 10,
      min = 1
    ),
    conditionalPanel(
      condition = "!(Number.isInteger(input.n1)) || input.n1 < 1 || input.n1 > 1000004249",
      p("Invalid input", style = "color: red;")
    ),
    conditionalPanel(
      condition = "Number.isInteger(input.n1) && input.n1 >= 1 && input.n1 <= 1000004249",
      htmlOutput("prime")
    )
  ),
  nav_panel(
    "Generate nth prime number",
    numericInput(
      "n2",
      "n",
      value = 10,
      min = 1
    ),
    conditionalPanel(
      condition = "!(Number.isInteger(input.n2)) || input.n2 < 1 || input.n2 > 30000000",
      p("Invalid input", style = "color: red;")
    ),
    conditionalPanel(
      condition = "Number.isInteger(input.n2) && input.n2 >= 1 && input.n2 <= 30000000",
      htmlOutput("nth")
    )
  ),
   nav_panel(
     "Impressum",
     h3("Impressum"),
     p("Anbieter:", br(), "Fedor Miasnikov", br(), "Silcherstr. 3", br(), "80807 München"),
     p("Kontakt:", br(), "Telefon: 01721587811", br(), "E-Mail:miasnikowfedor@gmail.com")
   )
)

server <- function(input, output){
  
  output$pi_value <- renderUI({HTML(
    paste0("π(", input$n, ") = ", primeCount(input$n))
  )})
  
  output$phi_value <- renderUI({HTML(
    paste0("φ(", input$n, ") = ", phi(input$n))
  )})
  
  output$nth <- renderUI({
    
    HTML(
      paste0(
        "primes(", input$n2, ") = ", nth_prime(input$n2)
      )
    )
    
  })
  
  output$prime <- renderUI({
    
    if (input$n1 == 1){
      
      HTML(
        "1 is neither a prime nor a composite number."
       )
      
    } else if (is_prime(input$n1)){
      
      x <- 1
      
      while(nth_prime(x) < input$n1 & x < 10000){
        x <- x +1
      }
      
      if (input$n1 <= 104729){
      
      HTML(
        paste0(input$n1, " is a prime number.<br/><br/>primes(", x, ") = ", input$n1)
      )
        
      } else {
        
        HTML(
          paste0(input$n1, " is a prime number.")
        )
        
      }
      
    } else {
      
      fact <- prime_factors(input$n1) %>% unlist() %>% paste(collapse = "x")
      
      HTML(
        paste0(
          input$n1, " is a composite number.<br/><br/>", input$n1, " = ", fact
        )
      )
      
    }
    
  })
    
    output$plot <- renderPlot({
      
      if(input$func == "Prime-counting function π(n)"){
        
        if(input$n == 1){
          
          leg <- tibble(
            n = c(0, 1),
            val = c(0, 0),
            cat = c("a", "a")
          )
          
          ggplot(data = leg) +
            geom_step(
              aes(
                x = n,
                y = val,
                color = cat
              )
            ) +
            theme_minimal() +
            scale_color_manual(
              labels = "π(n)",
              values = "darkblue"
            ) +
            labs(
              x = "n",
              y = "",
              color = ""
            ) +
            scale_x_continuous(
              limits = c(-0,05, 1.5),
              expand = c(0, 0),
              breaks = c(0, 1)
            ) +
            scale_y_continuous(
              limits = c(-0.05, 1),
              expand = c(0, 0),
              breaks = c(0, 1)
            ) +
            theme(
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15, angle = 0, vjust = 0.6),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size = 15)
            )
          
        } else if (input$n == 2){
          
          leg <- tibble(
            n = c(0, 1, 2),
            val = c(0, 0, 1),
            cat = c("a", "a", "a")
          )
          
          ggplot(data = leg) +
            geom_step(
              aes(
                x = n,
                y = val,
                color = cat
              )
            ) +
            theme_minimal() +
            scale_color_manual(
              labels = "π(n)",
              values = "darkblue"
            ) +
            labs(
              x = "n",
              y = "",
              color = ""
            ) +
            scale_x_continuous(
              limits = c(-0.05, 2.5),
              expand = c(0, 0),
              breaks = c(0, 1, 2)
            ) +
            scale_y_continuous(
              limits = c(-0.05, 1.05),
              expand = c(0, 0),
              breaks = c(0, 1)
            ) +
            theme(
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15, angle = 0, vjust = 0.6),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size = 15)
            )
          
        } else {
          
          leg <- tibble(
            n = c(1, 1.5, 2, 2.01),
            val = c(0, 0, 2/(log(2)), 2.01/(log(2.01))),
            cat = c("a", "a", "b", "b")
          )
          
          num <- tibble(
            n = 0:input$n,
            pi = numeric(input$n + 1)
          )
          
          for (i in 2:(input$n + 1)){
            num[i, 2] <- primeCount(i-1)
          }
          
          loga <- tibble(
            n = seq(2, input$n, 0.01),
            pi = n/(log(n))
          )
          
          ggplot() +
            geom_line(
              data = leg,
              aes(
                x = n,
                y = val,
                color = cat
              )
            ) +
            geom_line(
              data = loga,
              aes(
                x = n,
                y = pi
              ),
              color = "red"
            ) +
            geom_step(
              data = num,
              aes(
                x = n,
                y = pi
              ),
              color = "darkblue"
            ) +
            theme_minimal() +
            scale_color_manual(
              labels = c("π(n)", expression(frac(n, ln(n)))),
              values = c("darkblue", "red")
            ) +
            scale_x_continuous(
              limits = c(-0.05, input$n + 0.5),
              expand = c(0, 0),
              breaks = breaks_pretty()
            ) +
            scale_y_continuous(
              limits = c(-0.05, max(num$pi, loga$pi) + 0.05),
              expand = c(0, 0),
              breaks = breaks_pretty()
            ) +
            labs(
              x = "n",
              y = "",
              color = ""
            ) +
            theme(
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15, angle = 0, vjust = 0.6),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size = 15)
            )
            
        }
        
      } else if (input$func == "Euler's totient function φ(n)"){
        
        if (input$n == 1){
          
          leg <- tibble(
            n = 1,
            val = 1,
            cat = "a"
          )
          
          ggplot() +
            geom_point(
              data = leg,
              aes(
                x = n,
                y = val,
                color = cat
              )
            ) + 
            theme_minimal() +
            scale_x_continuous(
              limits = c(0.95, 2),
              expand = c(0, 0),
              breaks = c(1, 1.5, 2)
            ) +
            scale_y_continuous(
              limits = c(0.95, 2.05),
              expand = c(0, 0),
              breaks = seq(1, 2, 0.2)
            ) +
            scale_color_manual(
              labels = "φ(n)",
              values = "darkblue"
            ) + 
            labs(
              x = "n",
              y = "",
              color = ""
            ) +
            theme(
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15, angle = 0, vjust = 0.6),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              legend.key.size = unit(1.5, "cm"),
              legend.text = element_text(size = 15)
            )
          
        } else {
        
        num <- tibble(
          n = 1:input$n,
          phi = phi(n)
        )
        
        leg <- tibble(
          n = 1,
          val = 1,
          cat = "a"
        )
        
        ggplot() +
          geom_point(
            data = num,
            aes(
              x = n,
              y = phi
            ),
            color = "darkblue",
            size = 2
          ) +
          geom_point(
            data = leg,
            aes(
              x = n,
              y = val,
              color = cat
            )
          ) + 
          theme_minimal() +
          scale_x_continuous(
            limits = c(0.95, input$n + 0.5),
            expand = c(0, 0),
            breaks = breaks_pretty()
          ) +
          scale_y_continuous(
            limits = c(0.95, input$n + 0.05),
            expand = c(0, 0),
            breaks = breaks_pretty()
          ) +
          scale_color_manual(
            labels = "φ(n)",
            values = "darkblue"
          ) + 
          labs(
            x = "n",
            y = "",
            color = ""
          ) +
          theme(
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15, angle = 0, vjust = 0.6),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            legend.key.size = unit(1.5, "cm"),
            legend.text = element_text(size = 15)
          )
        }
      }
      
    }, height = 300)
}

shinyApp(ui, server)
