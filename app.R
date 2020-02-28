library(shiny)
library(ggvoronoi)
library(gdata)
library(dplyr)
library(DT)
library(mvtnorm)

findName <- function(val, row) (names(row)[match(val, row)]);
getX <- function(name) Candidates$x[which(Candidates$name == name)];
getY <- function(name) Candidates$y[which(Candidates$name == name)];
getColor <- function(winner) {
  vald <- (Candidates$color[which(Candidates$name == winner)]);
  if (length(vald) == 0) {
    return('transparent');
  }
  return(vald);
}
getClosest <- function(row) findName(min(row[['d1']], row[['d2']], row[['d3']]), row);
getDistance <- function(x, y, key) round(sqrt((x - getX(key))^2 + (y - getY(key))^2), digits = 2);
getRanked <- function(row) {
  df <- data.frame(distance = sort(c(row[['d1']], row[['d2']], row[['d3']])), points = c(3, 2, 1));
  df$candidate = findName(df$distance, row);
  return(df[c("candidate", "points")]);
}

Candidates <- data.frame();
Winners <- tibble(x = 0, y = 0, winner = 'unknown', color = 'red');

ui <- fluidPage(
   titlePanel("Voting Models"),
   sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, numericInput("sims", "Simulations:", min = 100, max = 10000, step = 100, value = 100)),
          column(6, numericInput("sigma", "Sigma:", min = 0, max = 1000, step = 10, value = 50))
        ),
        selectInput("type", "Voting scheme:", c("Borda" = "borda", "Plurality" = "plurality")),
        fluidRow(
          column(6, numericInput("min", "Min:", min = -100, max = 0, step = 25, value = -25)),
          column(6, numericInput("max", "Max:", min = 25, max = 200, step = 25, value = 125))
        ),
        hr(),
        fluidRow(
          column(6, numericInput("d1x", "Party 1 x:", min = 0, max = 100, step = 1, value = 7)),
          column(6, numericInput("d1y", "Party 1 y:", min = 0, max = 100, step = 1, value = 17))
        ),
        fluidRow(
          column(6, numericInput("d2x", "Party 2 x:", min = 0, max = 100, step = 1, value = 49)),
          column(6, numericInput("d2y", "Party 2 y:", min = 0, max = 100, step = 1, value = 1))
        ),
        fluidRow(
          column(6, numericInput("d3x", "Party 3 x:", min = 0, max = 100, step = 1, value = 41)),
          column(6, numericInput("d3y", "Party 3 y:", min = 0, max = 100, step = 1, value = 2))
        ),
        hr(),
        actionButton("go", "Run Simulation")
      ),
      
      mainPanel(
         plotOutput("distPlot"),
         br(),
         dataTableOutput("table")
      )
   )
)

getPlot <- function() (
  ggplot(Winners) + geom_point(aes(x, y), color = Winners$color)
          + geom_point(aes(x, y, color=color), shape=21, size=5, stroke = 2, fill = Candidates$color, color='black', Candidates)
)

getTable <- function() datatable(Winners);

setData <- function(input) {
  Candidates <<- data.frame(
    x = c(input$d1x, input$d2x, input$d3x),
    y = c(input$d1y, input$d2y, input$d3y),
    name = c('d1', 'd2', 'd3'),
    color = c('red', 'green', 'blue'),
    stringsAsFactors=FALSE
  );
  
  winners <- tibble(x = 0, y = 0, winner = 'unknown', color = 'red', points = 0);
  
  withProgress(message = 'Making plot', value = 0, {
    for (xpoint in input$min:input$max) {
      for (ypoint in input$min:input$max) {
        voters <- rmvnorm(mean=c(xpoint, ypoint), sigma=matrix(c(input$sigma, 0, 0, input$sigma), 2), n=input$sims);
        x <- voters[,1];
        y <- voters[,2];
        votes <- data.frame(
          x,
          y,
          d1 = getDistance(x, y, 'd1'),
          d2 = getDistance(x, y, 'd2'),
          d3 = getDistance(x, y, 'd3')
        );
        winners <- getWinners(votes, winners, xpoint, ypoint, input$type);
      }
      incProgress(1/(input$max-input$min), detail = paste("Iteration ", xpoint))
    };
  })
  Winners <<- winners;
}

getWinners <- function(votes, winners, x, y, type) {
  tallys <- if (type == 'plurality')
    do.call(rbind, apply(votes, 1, getRanked)) %>% slice(seq_len(1)) %>% group_by(candidate) %>% summarise_each(funs(sum))
    else do.call(rbind, apply(votes, 1, getRanked)) %>% group_by(candidate) %>% summarise_each(funs(sum));
  winner <- as_tibble(tallys) %>% arrange(desc(points)) %>% slice(seq_len(1));
  return(add_row(
    winners,
    x,
    y,
    points = winner$points,
    color = getColor(winner$candidate),
    winner = winner$candidate
  ));   
}

server <- function(input, output) {
  v <- reactiveValues(runSim = FALSE);

  observeEvent(input$go, {
    v$runSim <- input$go
  });
  
  data <- reactive({ setData(input) });
  
  output$table <- renderDataTable({
    if (v$runSim == FALSE) return();
    isolate({ data(); getTable() });
  });

  output$distPlot <- renderPlot({
    if (v$runSim == FALSE) return();
    isolate({ data(); getPlot() });
  });
}

shinyApp(ui = ui, server = server)

