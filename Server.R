library(shiny)
library(ggplot2)
library(shinydashboard)
library(markdown)


server <- function(input, output) {
  
  set.seed(2558)
  
  data1 <- reactiveValues()
  data1$option <- 1
  data1$s <- 0
  data1$qsl <- 0
  data1$qsu <- 0
  
  p <- reactive({
    ab <- switch(input$prior,
                 one = c(.001,.001),
                 half = c(.5,.5),
                 five = c(5,5),
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
  
  
  
  
  plots <- function(a,b,m,n){
    
    lamda <- seq(0.01, (1/m) + 1 + .5*(1/m) , 0.01)  #
    prior <- dgamma(lamda, a, b)
    
    logLikelihood <- n*log(lamda) - (lamda)*n*m
    
    posterior <- dgamma(lamda,((a+n)),((b+n*(m))))
    
    
    
    qs <- c(qgamma(.025,((a+n)),((b+n*(m)))), qgamma(.975,((a+n)),((b+n*(m)))))
    s <- (a+n)/(b+n*(m))
    
    ytopl <- dgamma(qs[1],((a+n)),((b+n*(m))))
    ytopu <- dgamma(qs[2],((a+n)),((b+n*(m))))
    
    data1$qsl <- round(qs[1], digits = 4)
    data1$qsu <- round(qs[2], digits = 4)
    data1$s <- round(s, digits = 4)
    
    
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

    ggplot(d2, aes(lamda, value, colour=variable)) + 
      xlim(xlim) + ylim(ylim) +
      geom_line() +
      scale_colour_manual(values=c("black", "blue", "red"),
                          guide = guide_legend(title = NULL)) +
      geom_segment(aes(x=qs[1],y=0,xend=qs[1],yend=ytopl), col = "black") +
      geom_segment(aes(x=qs[2],y=0,xend=qs[2],yend=ytopu), col = "black") +
      geom_polygon(aes(shade[,1], shade[,2]), fill="black", alpha = 0.1)

#       geom_segment(aes(x=qs[1],y=0,xend=qs[1],yend=ytopl), col = "green") + 
#       geom_segment(aes(x=qs[2],y=0,xend=qs[2],yend=ytopu), col = "green") +
#       geom_polygon(aes(shade[,1], shade[,2]), fill="green", alpha = 0.1)
    
#     ggplot(d) + xlim(xlim) + ylim(ylim) +
#       geom_line(aes(lamda, prior, colour = "Prior"), colour = "yellow3") + 
#       geom_line(aes(lamda, likelihood), colour = "blue") +
#       geom_line(aes(lamda, posterior), colour = "green4") +
#       geom_segment(aes(x=qs[1],y=0,xend=qs[1],yend=ytopl), col = "green") + 
#       geom_segment(aes(x=qs[2],y=0,xend=qs[2],yend=ytopu), col = "green") +
#       geom_polygon(aes(shade[,1], shade[,2]), fill="green", alpha = 0.1) + 
#       scale_color_manual(values=c("prior"="yellow3", "likelihood"="blue",
#                                   "posterior"="green4"))
    
  }  
  
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
    
    
    #     plot(0,0,type="n",xlab="Lifetime",ylab="",ylim=ylim,xlim=xlim)
    #     lines(l,lifedist,col=2)
    
    
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
    
    plot(theta,prior,type="l",col=2,lty=2,xlab="theta",ylab="",ylim=ylim)
    lines(theta,likelihood,col=3, lty=3)
    lines(theta,posterior,col=1,lwd=2, lty=1)
    legend("topright",c("prior","likelihood","posterior"),
           lty=c(2,3,1),col=c(2,3,1),lwd=c(1,1,2),inset=0.01,cex=1.2)
  }  
  
  postSum <- function(a,b,n,y){
    
    A    <- y+a
    B    <- n-y+b 
    Mean <- A/(A+B)
    Var  <- A*B/((A+B)*(A+B)*(A+B+1))
    Q05  <- qbeta(0.05,A,B)
    Q95  <- qbeta(0.95,A,B)
    #P50  <- pbeta(0.50,A,B)
    
    output <- cbind(Mean,Q05,Q95)
    output <- round(output,2)
    
    return(output)
  }  
  
  output$plot3 <- renderPlot({
    
    a = p2()[1]
    b = p2()[2]
    n = input$n
    y = input$s
    
    plots2(a,b,n,y)
  })
  
  output$summary <- renderPrint({
    
    a = p2()[1]
    b = p2()[2]
    n = input$n
    y = input$s
    
    postSum(a,b,n,y)
  })
  
  
}