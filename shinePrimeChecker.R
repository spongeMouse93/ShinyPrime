library(shiny)

isPrime = function(n){
  if (n <= 1)
      return(FALSE)
  if (n == 2 | n == 3)
      return(TRUE)
  if (n %% 2 == 0 | n %% 3 == 0)
      return(FALSE)
  if (sqrt(n) >= 5)
    for (i in seq(from = 5, to = sqrt(n), by = 6))
        if (n %% i == 0 | n %% (i + 2) == 0)
            return(FALSE)
  return(TRUE)
}

ui = fluidPage(
  titlePanel("PrimeChecker"),
  sidebarLayout(
    sidebarPanel(numericInput("num", "Enter an integer:", value = 2, min = 1, step = 1)),
    mainPanel(textOutput("primeResult"))
  )
)

server = function(input, output) {
  output$primeResult = renderText({
    n = input$num
    if (is.null(n) | is.na(n))
      return("Please input a number")
    if (n < 0)
      return("Please input a positive integer")
    if (n != floor(n))
      return("Please input a positive integer")
    if (isPrime(n)) 
      paste(n, " is a prime number")
    else 
      paste(n, " is NOT a prime number")
    
  })
}

shinyApp(ui = ui, server = server)
