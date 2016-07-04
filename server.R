


shinyServer(
  function(input, output, session) {

    
    
output$pmf <- renderPlot({
  # draw.plotmath.cell(expression (bgroup("(", atop(x,y), ")")))
  if (!(input$n <= 0 | input$n %% 1 !=0)) n=input$n else n=10
  
  if (input$prob >= 0 & input$prob <=1 & is.numeric(input$prob)) prob = input$prob else prob=0.5
  
  p=seq(0,1,by=0.001)
  par(mfrow=c(1,2))
  plot(0:n, dgeom(0:n,prob=prob), col="blue",pch=16,
       xlab="X (números de tentativas até o primeiro sucesso)", ylab="", main=paste("Função de probabilidade, p=", prob, "n=",n))
  segments(x0=0:n, y0=rep(0, (n+1)), x1=0:n, y1=dgeom(0:n, prob=prob),lwd=2,col="blue")
  abline(h=0)
  X <- rgeom(n=n,prob=prob)
  loglikgeo<-function(p){
    n*log(p) + (sum(X))*log(1-p)}
  plot(p,loglikgeo(p=p), type="l",xlim=c(0,1), ylab="",xlab="p", main=
         paste("Log verossimilhança, \n log(L(p|X=x", ", ", "n=", n, "))", sep="") )
  abline(v=n/(n+sum(X)), col="blue", lty=2)
  abline(v=prob, col="red", lty=2)
  
})





output$norm <- renderPlot({
  
  if (!(input$n <= 0 | input$n %% 1 !=0)) n=input$n else n=10
  
  if (is.numeric(input$mean)) mean = input$mean else mean=0
  
  if (input$var >= 0 & is.numeric(input$var)) var= input$var else var=1
  
  if (is.numeric(input$x)) x = input$x else x=50
  
  if (is.numeric(input$y)) y = input$y else y=25
  
  if (is.numeric(input$z)) z = input$z else z=80
  
  X<-rnorm(n,mean,var)
  
  MLEnorm <- function(mu,sigma2){
    n<- length(X)
    SumX<-0
    for(i in 1:length(X)){
      SumX<-SumX + (X[i]-mean)^2
    }
    T <- -(n/2)*log(2*pi) -(n/2)*log(sigma2) -(1/(2*sigma2))*(SumX)
    return(T)
  } 
  #estimadores de máxima verossimilhança
  mean(X)
  sd(X)
  #Gráfico da função de máxima verossimilhança
  g <- expand.grid(x = -20:20, y = 1:30, gr = 3:5)
  g$z <- MLEnorm(mu=g$x,sigma2=g$y)

  wireframe(z ~ x * y, data = g, groups = gr,
            scales = list(arrows = FALSE),
            drape = TRUE, colorkey = TRUE,
            screen = list( x = x,y=y,z=z))
  
})


output$beta <- renderPlot({
  
  
  if (!(input$n <= 0 | input$n %% 1 !=0)) n=input$n else n=10
  
  if (input$Alpha >= 0 & is.numeric(input$Alpha)) Alpha = input$Alpha else Alpha=2
  
  if (input$Beta >= 0  & is.numeric(input$Beta)) Beta = input$Beta else Beta=2
  
  if (is.numeric(input$x)) x = input$x else x=50
  
  if (is.numeric(input$y)) y = input$y else y=25
  
  if (is.numeric(input$z)) z = input$z else z=80
  
  
  library("lattice")
  
  X<-rbeta(10000,Alpha,beta)
  
  
  
  MLE<-function(theta){
    n<-length(X)
    T <- (theta[1]-1)*sum(log(X)) + (theta[2]-1)*sum(log(1-X)) - n*log(beta(theta[1],theta[2]))
    return(T)
  }
  
  chute <- c(mean(X)*beta + Alpha,mean(X)*Alpha + beta)
  Hat<-optim(par=chute,fn=MLE,method="BFGS",control=list(fnscale=-1))$par
  
  
  MLEwire<-function(Alpha,beta){
    n<-length(X)
    T <- (Alpha-1)*sum(log(X)) + (beta-1)*sum(log(1-X)) - n*log(beta(Alpha,beta))
    return(T)
  }
  
  g <- expand.grid(s = 1:20, t = 1:30, gr = 3)
  g$z <- MLEwire(Alpha=g$s,beta=g$t)
  wireframe(z ~ s * t, data = g, groups = gr,
            scales = list(arrows = FALSE),
            drape = TRUE, colorkey = TRUE,
            screen = list(z = z, y= y, x = x))

  
})

})



