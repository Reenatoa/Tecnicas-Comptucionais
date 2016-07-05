


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
  
  if (!(input$n1 <= 0 | input$n %% 1 !=0)) n1=input$n else n1=10
  
  if (is.numeric(input$mean)) mean = input$mean else mean=0
  
  if (input$var >= 0 & is.numeric(input$var)) var= input$var else var=1
  
  if (is.numeric(input$x)) x = input$x else x=50
  
  if (is.numeric(input$y)) y = input$y else y=25
  
  if (is.numeric(input$z)) z = input$z else z=80
  
  X<-rnorm(n1,mean,sqrt(var))
  
  MLEnorm <- function(mean,var){
    SumX<-0
    for(i in 1:n1){
      SumX<-SumX + (X[i]-mean)^2
    }
    T <- -(n1/2)*log(2*pi) -(n1/2)*log(var) - (1/(2*var))*(SumX)
    return(T)
  } 
  #estimadores de máxima verossimilhança
  head(X)
  mean(X)
  var(X)
  #Gráfico da função de máxima verossimilhança
  g <- expand.grid(mu = seq(mean-3*var,mean+3*var,length=50) , sigma2 = seq(0.1,2*var,length=50), gr = 1:3)
  g$z <- MLEnorm(mean=g$mu,var=g$sigma2)

  wireframe(z ~ mu * sigma2, data = g, groups = gr,
            scales = list(arrows = FALSE),
            drape = TRUE, colorkey = TRUE,
            screen = list( x = x,y= y,z= z))
  
})


output$beta <- renderPlot({
  
  
  if (!(input$n2 <= 0 | input$n2 %% 1 !=0)) n2=input$n2 else n2=10
  
  if (input$Alpha >= 0 & is.numeric(input$Alpha)) Alpha = input$Alpha else Alpha=2
  
  if (input$Beta >= 0  & is.numeric(input$Beta)) Beta = input$Beta else Beta=2
  
  if (is.numeric(input$x1)) x1 = input$x1 else x1=50
  
  if (is.numeric(input$y1)) y1 = input$y1 else y1=25
  
  if (is.numeric(input$z1)) z1 = input$z1 else z1=80
  
  
  library("lattice")
  
  X<-rbeta(n2,Alpha,Beta)
  
  
  
  MLE<-function(theta){
    T <- (theta[1]-1)*sum(log(X)) + (theta[2]-1)*sum(log(1-X)) - n2*log(beta(theta[1],theta[2]))
    return(T)
  }
  
  chute <- c(mean(X)*Beta + Alpha,mean(X)*Alpha + Beta)
  Hat<-optim(par=chute,fn=MLE,method="BFGS",control=list(fnscale=-1))$par
  
  
  MLEwire<-function(Alpha,Beta){
    T <- (Alpha-1)*sum(log(X)) + (Beta-1)*sum(log(1-X)) - n2*log(beta(Alpha,Beta))
    return(T)
  }
  
  g <- expand.grid(alpha = seq(Alpha-3*sd(X),Alpha+3*sd(X),length=50)  , beta = seq(1,2*Beta,length=50), gr = 1:3)
  g$z <- MLEwire(Alpha=g$alpha,Beta=g$beta)
  wireframe(z ~ alpha * beta, data = g, groups = gr,
            scales = list(arrows = FALSE),
            drape = TRUE, colorkey = TRUE,
            screen = list(z = z1, y= y1, x = x1))

  
})

})



