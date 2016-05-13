library(shiny)
library(ggplot2)
library(shinydashboard)
library(markdown)
library(reshape2)
library(dplyr)


server <- function(input, output) {
  
  set.seed(2558)
  
  data1 <- reactiveValues()
  data1$option <- 1
  data1$s <- 0
  data1$qsl <- 0
  data1$qsu <- 0
  
  data2 <- reactiveValues()
  data2$option <- 1
  data2$s <- 0
  data2$qsl <- 0
  data2$qsu <- 0
  
  p <- reactive({
    ab <- switch(input$prior,
                 NI = c(.001,.001),
                 five = c(3,3),
                 ten = c(10,10),
                 pfivefive = c(.5,5))
    ab
  })
  
  output$CD_choice <- renderUI({
    if (input$CD_choice == "Upload Data"){
      data1$option <- 2
      fluidRow(column(12, radioButtons("CD", "Censored Data:", c("No", "Yes" ), selected = "No")),
               fileInput('file1', 'Dataset',
                         accept=c('text/csv','text/comma-separated-values',
                                  'text/tab-separated-values',
                                  'text/plain',
                                  'csv',
                                  'tsv')),
               dataTableOutput('preview1')
      )
      
    } else if(input$CD_choice == "Input Summary"){
      data1$option <- 1
      fluidRow(numericInput("n", 
                            "Number of Observations:", 
                            value = 10,
                            min = 1, 
                            max = 10000),
               numericInput("m", 
                            "Mean time to failure:", 
                            value = 5,
                            min = 1, 
                            max = 100)
      )}
  })
  
  output$CD_choice2 <- renderUI({
    if (input$CD_choice2 == "Upload Data"){
      data2$option <- 2
      fluidRow(
               fileInput('file2', 'Dataset',
                         accept=c('text/csv','text/comma-separated-values',
                                  'text/tab-separated-values',
                                  'text/plain',
                                  'csv',
                                  'tsv')),
               dataTableOutput('preview2')
      )
      
    } else if(input$CD_choice2 == "Input Summary"){
      data2$option <- 1
      fluidRow(sliderInput("n", 
                          "Number of observations:", 
                          value = 10,
                          min = 1, 
                          max = 50),
              sliderInput("s", 
                          "Number of Successes:", 
                          value = 2,
                          min = 0, 
                          max = 50)
      )}
  })
  
  
  
  output$preview1 <- renderDataTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #     validate(
    #       need(file_ext(inFile$name) %in% c(
    #         'text/csv',
    #         'text/comma-separated-values',
    #         'text/tab-separated-values',
    #         'text/plain',
    #         'csv',
    #         'tsv'
    #       ), "Dataset 1: Wrong File Format try again!"))
    
    data1$csv <- read.csv(inFile$datapath,header=T)
    
    data1$csv
    
  }, options = list(pageLength = 5, searching = FALSE, 
                    lengthChange = FALSE))
  
  output$preview2 <- renderDataTable({
    
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    data2$csv <- read.csv(inFile$datapath,header=T)
    
    data2$csv
    
  }, options = list(pageLength = 5, searching = FALSE, 
                    lengthChange = FALSE))
  
  
  plots <- function(a,b,m,n){
    
    lamda <- seq(0.01, (1/m) + 1 + .5*(1/m) , 0.01)  #
    prior <- dgamma(lamda, a, b)
    
    logLikelihood <- n*log(lamda) - (lamda)*n*m
    
    posterior <- dgamma(lamda,((a+n)),((b+n*(m))))
    
    qs <- c(qgamma(.025,((a+n)),((b+n*(m)))), qgamma(.975,((a+n)),((b+n*(m)))))
    s <- (a+n)/(b+n*(m))
    
    ytopl <- dgamma(qs[1],((a+n)),((b+n*(m))))
    ytopu <- dgamma(qs[2],((a+n)),((b+n*(m))))
    
    data1$qsl <- round(qs[1], digits = 3)
    data1$qsu <- round(qs[2], digits = 3)
    data1$s <- round(s, digits = 3)
    
    
    xll <- function(w){
      exp(n - n*log(1/m) + n*log(w) - (w)*n*m)
    }
    stan <- integrate(xll, .01, (1/m) + 1 + .5*(1/m))
    
    likelihood <- (exp(n - n*log(1/m) + logLikelihood)) / stan[[1]]
    
    ylim<-c(0,max(c(likelihood,posterior)))
    xlim <- c(0,max(lamda))
    
    lower <- min(which(lamda>qs[1]))
    upper <- max(which(lamda<qs[2]))
    
    shade <- rbind(c(qs[1],0),
                   cbind(lamda[lower:upper], posterior[lower:upper]),
                   c(qs[2], 0))
    
    shade <- rbind(shade,
                   matrix(c(rep(0, (length(lamda)*3 - length(shade[,1]))), rep(0,(length(lamda)*3 - length(shade[,1])))), ncol = 2))
    

    shade  <- as.data.frame(shade)

    d <- as.data.frame(cbind(lamda, posterior, likelihood, prior))

    d2 <- melt(d, id="lamda")
    
    pp <- ggplot(d2, aes(lamda, value, colour=variable))  
    pp <- pp + xlim(xlim) + ylim(ylim) 
    pp <- pp + geom_line() 
    pp <- pp + scale_colour_manual(values=c("black", "blue", "red"),
                                   guide = guide_legend(title = NULL))
    if(input$CI){
      pp <- pp + geom_segment(aes(x=qs[1],y=0,xend=qs[1],yend=ytopl), col = "black") 
      pp <- pp + geom_segment(aes(x=qs[2],y=0,xend=qs[2],yend=ytopu), col = "black") 
      pp <- pp + geom_polygon(aes(shade[,1], shade[,2]), fill="black", alpha = 0.1)
    }
    
    pp
    
  }  
  
  # running the plot for censored - non-conjugate 
  cplots <- function(a, b, data){
    
    m <- mean(data[,1])
    
    lamda <- seq(0.001, (1/m) + 1 + .5*(1/m) , 0.001)  
    prior <- dgamma(lamda, a, b)
    
    w <- lapply(1:nrow(data), function(i){loglikelihood(data[i,1], data[i,2], lamda)}) 
    
    LL <- rowSums(matrix(unlist(w),nrow=length(lamda), ncol=nrow(data)))
    
    if(min(LL) < 0){
      LL <- LL + abs(min(LL)) + 1
    }
    stan <- sum(exp(LL))*.001
    
    likelihood <- exp(LL) / stan[[1]]
    
    posterior <- likelihood * prior
    
    stan <- sum(posterior)*.001
    
    posterior <- posterior/stan[[1]]
    
    s <- sample(lamda, 100000, replace = T, prob = posterior)
    s2 <- sum(lamda*posterior)*.001
    
    qs <- quantile(s, probs = c(.025, .975))
    
    data1$qsl <- round(qs[1], digits = 4)
    data1$qsu <- round(qs[2], digits = 4)
    data1$s <- round(s2, digits = 4)
    
    ylim<-c(0,max(likelihood, posterior))
    xlim <- c(0,max(lamda))
    plot(0,0,type="n",xlab="lamda",ylab="",ylim=ylim,xlim=xlim)
    lines(lamda,prior,col=2)
    lines(lamda,likelihood,col=3)
    lines(lamda,posterior,col=1,lwd=2)
    abline(v=qs, lwd = 2, lty = 2, col = "blue")
    legend("topright",c("prior","likelihood","posterior"),
           col=c(2,3,1),lwd=c(1,1,2),inset=0.01,cex=1.2)
  }
  
  loglikelihood <- function(obs, type, lamda){
    val <- 0
    
    if(type == ">"){
      val <- -lamda*obs
    }else if(type == "<"){
      val <- lamda*obs
    }else{
      val <- log(lamda) - lamda*obs
    }
    return(val)
  }
  
  output$plot1 <- renderPlot({
    
    a = p()[1]
    b = p()[2]
    
    if(data1$option == 1){
      m = input$m
      n = input$n
      
      if(is.na(m)){
        
      }else{
        plots(a,b,m,n)
      }
      
    }else if(data1$option == 2){
      if(input$CD == "No"){
        m = mean(data1$csv[,1])
        n = length(data1$csv[,1])
        
        if(is.na(m)){
          
        }else{
          plots(a,b,m,n)
        }
        
      }else{
        cplots(a,b,data1$csv)
      }
      
    }
    
  })
  
  output$m <- renderText({
    paste("Posterior Mean: ", data1$s)
  })
  output$qsl <- renderText({
    paste("95% Credible Interval lower: ",data1$qsl)
  })
  output$qsu <- renderText({
    paste("95% Credible Interval Upper: ",data1$qsu)
  })
  
  postpredict <- function(a,b,n,s,l){
    Life <- (((b+n)^(a+s))/gamma(a+s))*(gamma(a+n+1)/((b+s+l)^(a+n+1)))
    return(Life)
  }
  
  output$plot2 <- renderPlot({
    
    a = p()[1]
    b = p()[2]
    
    m = input$m
    n = input$n
    s = (m*n)
    
    l <- seq(0.01, (s) + .25*(s) , 0.01)
    
    lifedist <- postpredict(a,b,n,s,l)
    
    ylim<-c(0,max(lifedist))
    xlim <- c(0,max(l))
    
    
    ggplot() + xlim(xlim) + ylim(ylim) +
      geom_line(aes(l, lifedist), colour = "red2") 
  
    #     if(data1$option == 1){
    #       m = input$m
    #       n = input$n
    #       
    #       if(is.na(m)){
    #         
    #       }else{
    #         plots(a,b,m,n)
    #       }
    #       
    #     }else if(data1$option == 2){
    #       if(input$CD == "No"){
    #         m = mean(data1$csv[,1])
    #         n = length(data1$csv[,1])
    #         
    #         if(is.na(m)){
    #           
    #         }else{
    #           plots(a,b,m,n)
    #         }
    #         
    #       }else{
    #         cplots(a,b,data1$csv)
    #       }
    #      
    #    }
    
  })
  
  
  p2 <- reactive({
    ab2 <- switch(input$prior2,
                 one = c(1,1),
                 half = c(.5,.5),
                 ten = c(10,10),
                 twofive = c(2,5),
                 fivetwo = c(5,2))
    ab2
  })
  
  
  plots2 <- function(a,b,n,y){
    theta<-seq(0.001,0.999,0.001)
    prior<-dbeta(theta,a,b)
    likelihood<-dbinom(rep(y,length(theta)),n,theta)
    posterior<-dbeta(theta,a+y,n-y+b)
    
    y<<-y
    n<<-n
    xbin <- function(w){
      dbinom(y, n, w)
    }
    stan <- integrate(xbin,0,1)
    likelihood<-likelihood/stan[[1]]
    
    ylim<-c(0,max(prior))
    ylim<-c(0,max(c(prior,likelihood,posterior)))
    
    qs <- c(qbeta(.025,a+y,n-y+b), qbeta(.975,a+y,n-y+b))
    
    s <- (y+a)/(y+a+n-y+b)
    data2$qsl <- round(qs[1], digits = 3)
    data2$qsu <- round(qs[2], digits = 3)
    data2$s <- round(s, digits = 3)
    
    ytopl <- dbeta(qs[1],a+y,n-y+b)
    ytopu <- dbeta(qs[2],a+y,n-y+b)
    
    lower <- min(which(theta>qs[1]))
    upper <- max(which(theta<qs[2]))
    
    shade <- rbind(c(qs[1],0),
                   cbind(theta[lower:upper], posterior[lower:upper]),
                   c(qs[2], 0))
    
    shade <- rbind(shade,
                   matrix(c(rep(0, (length(theta)*3 - length(shade[,1]))), rep(0,(length(theta)*3 - length(shade[,1])))), ncol = 2))
    
    
    shade  <- as.data.frame(shade)
    
    d <- as.data.frame(cbind(theta, posterior, likelihood, prior))
    
    d2 <- melt(d, id="theta")
    
    pp <- ggplot(d2, aes(theta, value, colour=variable))  
    pp <- pp + ylim(ylim) 
    pp <- pp + geom_line() 
    pp <- pp + scale_colour_manual(values=c("black", "blue", "red"),
                          guide = guide_legend(title = NULL))
    if(input$CI2){
      pp <- pp + geom_segment(aes(x=qs[1],y=0,xend=qs[1],yend=ytopl), col = "black") 
      pp <- pp + geom_segment(aes(x=qs[2],y=0,xend=qs[2],yend=ytopu), col = "black") 
      pp <- pp + geom_polygon(aes(shade[,1], shade[,2]), fill="black", alpha = 0.1)
    }
    
    pp
  }  
  
  output$m2 <- renderText({
    paste("Posterior Mean: ", data2$s)
  })
  output$qsl2 <- renderText({
    paste("95% Credible Interval lower: ",data2$qsl)
  })
  output$qsu2 <- renderText({
    paste("95% Credible Interval Upper: ",data2$qsu)
  })
  
  output$plot3 <- renderPlot({
    
    a = p2()[1]
    b = p2()[2]

    
    if(data2$option == 1){
      n = input$n
      y = input$s
      
      if(is.na(y)){
        
      }else{
        plots2(a,b,n,y)
      }
      
    }else if(data2$option == 2){

        y = sum(data2$csv[,1])
        n = length(data2$csv[,1])
        
        if(is.na(y)){
        }else{
          plots2(a,b,n,y)
        }
    }
  })
}