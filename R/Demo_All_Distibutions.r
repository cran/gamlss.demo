#library(rpanel)
#library(gamlss)
#ls(envir = asNamespace("gamlss.demo"))
# GAMLSS distributions Demos
#---------------------------------------------------------------------------------------
# two parameter continuous distributions
# - Inf to Inf
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# NO
demo.NO <- function()
{
 if (interactive()) 
   {
   mu <- sigma <- NULL
        density.draw <- function(panel) 
         {
         op <- par(mfrow = c(2, 1))
          y <- seq(-4, 4, length=200)
           plot(y, dNO(x=y, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="normal probability density function", type="l")
           plot(y, pNO(y, mu=panel$mu, sigma=panel$sigma), ylim=c(0.001,1), type="s", ylab="F(y)", main="normal cumulative probability function")
         par(op) 
          panel
          }
         NOpanel <- rp.control('Normal family', sigma = 1, mu=0,  ymax=0.8 )
        rp.slider(NOpanel,  var=mu, from=-2, to=2, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(NOpanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
}
#--------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# LO
demo.LO <- function()
{
 if (interactive()) 
   {
     mu <- sigma <- NULL
        density.draw <- function(panel) 
         {
         op<-par(mfrow = c(2, 1))
         y <- seq(-4, 4, length=200)
           plot(y, dLO(x=y, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="logistic probability density function", type="l")
           plot(y, pLO(y, mu=panel$mu, sigma=panel$sigma), ylim=c(0.001,1), type="s", ylab="F(y)", main="logistic cumulative probability function")
         par(op) 
          panel
          }
         NOpanel <- rp.control('Logistic family', sigma = 1, mu=0,  ymax=0.8 )
        rp.slider(NOpanel,  var=mu, from=-2, to=2, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(NOpanel,  var=sigma, from=0.01, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
}
#--------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
demo.NO.LO <- function()
{
 if (interactive()) 
   {
      mu <- sigma <- x <- NULL
        density.draw <- function(panel) 
         {
          op<-par(mfrow=c(2,1)) 
           curve(dNO(x,    mu=panel$mu, sigma=panel$sigma), -5, 5,  ylab="PDF", main="Normal")
           curve(dLO(x,    mu=panel$mu, sigma=panel$sigma), -5, 5,   ylab="PDF", main="Logistic")
           par(op) 
          panel
          }
         NOLOpanel <- rp.control('Normal and Log Normal family', sigma = 1, mu=0,  ymax=0.8 )
        rp.slider(NOLOpanel,  var=mu, from=-2, to=2, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(NOLOpanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
 }
#------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# GU
demo.GU <- function()
{
 if (interactive()) 
   {
      mu <- sigma <- NULL
        density.draw <- function(panel) 
         {
         op<-par(mfrow = c(2, 1))
         y <- seq(-4, 4, length=200)
           plot(y, dGU(x=y, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="Gumbel probability density function", type="l")
           plot(y, pGU(y, mu=panel$mu, sigma=panel$sigma), ylim=c(0.001,1), type="s", ylab="F(y)", main="Gumbel cumulative probability function")
         par(op) 
          panel
          }
         GUpanel <- rp.control('Gumber family', sigma = 1, mu=0,  ymax=0.8 )
        rp.slider(GUpanel,  var=mu, from=-2, to=2, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(GUpanel,  var=sigma, from=0.01, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
} 
#--------------------------------------------------------------------------------------- 
#------------------------------------------------------------------------------------
# RG
demo.RG <- function()
{
 if (interactive()) 
   {
      mu <- sigma <- NULL
        density.draw <- function(panel) 
         {
         op<-par(mfrow = c(2, 1))
         y <- seq(-4, 4, length=200)
           plot(y, dRG(x=y, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="Reverse Gumbel probability density function", type="l")
           plot(y, pRG(y, mu=panel$mu, sigma=panel$sigma), ylim=c(0.001,1), type="s", ylab="F(y)", main="Reverse Gumbel cumulative probability function")
         par(op) 
          panel
          }
         RGpanel <- rp.control('Reverse Gumbel family', sigma = 1, mu=0,  ymax=0.8 )
        rp.slider(RGpanel,  var=mu, from=-2, to=2, resolution=0.10,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(RGpanel,  var=sigma, from=0.01, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# 3 parameter distributions
# exGAUS
demo.exGAUS <- function()
{
 if (interactive()) 
   {
     mu <- sigma <- nu <- NULL
        density.draw <- function(panel) 
         {     op<-par(mfrow = c(2, 1))
            y <- seq(-10, 30, length=300)
             plot(y, dexGAUS(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), ylab="f(y)", main="exGAUS probability density function", type="l")
             plot(y, pexGAUS(q=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), ylab="F(y)", main="exGAUS cumulative probability function", type="l")
         par(op) 
          # curve(dTF(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), -5, 5,  ylab="PDF")
          panel
          }
         Epanel <- rp.control('exGAUS familly',  mu=0, sigma = 1, nu=1)
        rp.slider(Epanel,  var=mu, from=-10, to=20, resolution=0.1,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(Epanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(Epanel,  var=nu, from=0.1, to=10, resolution=0.01,  action = density.draw, title="nu",  showvalue = T) 
   }
 }
 
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#PE
demo.PE <- function()
{
 if (interactive()) 
   {
     mu <- sigma <- nu <- NULL
        density.draw <- function(panel) 
         {
            y <- seq(-5, 5, length=200)
             plot(y, dPE(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), ylab="f(y)", main="Power Exponential probability density function", type="l")
          # curve(dTF(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), -5, 5,  ylab="PDF")
          panel
          }
         PEpanel <- rp.control('PE familly', sigma = 1, mu=0, nu=2, ymax=0.6 )
        rp.slider(PEpanel,  var=mu, from=-2, to=2, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(PEpanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(PEpanel,  var=nu, from=1, to=10, resolution=0.01,  action = density.draw, title="nu",  showvalue = T) 
   }
 }
 
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# TF and NO
demo.PE.NO <- function()
{
 if (interactive()) 
   {
     mu <- sigma <- nu <- NULL
        density.draw <- function(panel) 
         {
               y <- seq(-5, 5, length=200)
            fnNO <- dPE(x=y, mu=panel$mu, sigma=panel$sigma)
            fnPE <- dPE(y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu)
            ymax <- max(c(fnNO, fnPE))
             plot(y, fnNO, ylab="f(y)", main="PE and normal probability density functions", type="l", ylim=c(0, ymax))
             lines(y , fnPE, col="red")
          panel
          }
         TFpanel <- rp.control('PE against NO', sigma = 1, mu=0, nu=2, ymax=0.6 )
        rp.slider(TFpanel,  var=mu, from=-2, to=2, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(TFpanel,  var=sigma, from=0.01, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(TFpanel,  var=nu, from=1, to=10, resolution=0.01,  action = density.draw, title="nu",  showvalue = T) 
   }
}
 
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# TF
demo.TF <- function()
{
 if (interactive()) 
   {
     mu <- sigma <- nu <- NULL
        density.draw <- function(panel) 
         {
            y <- seq(-10, 10, length=200)
             plot(y, dTF(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), ylab="f(y)", main="t probability density function", type="l")
          # curve(dTF(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), -5, 5,  ylab="PDF")
          panel
          }
         TFpanel <- rp.control('t-familly', sigma = 1, mu=0, nu=10, ymax=0.6 )
        rp.slider(TFpanel,  var=mu, from=-2, to=2, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(TFpanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(TFpanel,  var=nu, from=1, to=100, resolution=.1,  action = density.draw, title="nu",  showvalue = T) 
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# TF and NO
demo.TF.NO <- function()
{
 if (interactive()) 
   {
      mu <- sigma <- nu <- NULL
        density.draw <- function(panel) 
         {
            y <- seq(-10, 10, length=200)
             plot(y, dNO(x=y, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="t and normal probability density functions", type="l")
             lines(y , dTF(y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), col="red")
          panel
          }
         TFpanel <- rp.control('t against NO', sigma = 1, mu=0, nu=10, ymax=0.6 )
        rp.slider(TFpanel,  var=mu, from=-2, to=2, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(TFpanel,  var=sigma, from=0.01, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(TFpanel,  var=nu, from=1, to=100, resolution=0.1,  action = density.draw, title="nu",  showvalue = T) 
   }
}
# 4 parameter distributions
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# EGB2
demo.EGB2 <- function()
{
  if (interactive())  
   {
     mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
          y <- seq(-10, 10, length=200)
             plot(y, dEGB2(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), ylab="f(y)", main="EGB2 probability density functions", type="l")
          # curve(dSEP1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         EGB2panel <- rp.control('EGB2 family', sigma = 1, mu=0, nu=1, tau=1)
        rp.slider(EGB2panel, var=mu,    from=-10, to=10, resolution=0.1,  action = density.draw, title="mu",     showvalue = T)  
        rp.slider(EGB2panel, var=sigma, from=-2, to=2 ,resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(EGB2panel, var=nu,    from=0.1, to=5, resolution=0.01,  action = density.draw, title="nu",     showvalue = T)
        rp.slider(EGB2panel, var=tau,   from=0.1, to=5, resolution=0.01,  action = density.draw, title="tau",    showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# GT
demo.GT <- function()
{
  if (interactive())  
   {
     mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
          y <- seq(-10, 10, length=200)
             plot(y, dGT(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), ylab="f(y)", main="Generalised t  probability density functions", type="l")
          # curve(dSEP1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         GTpanel <- rp.control('EGB2 family', sigma = 1, mu=0, nu=1, tau=1,)
        rp.slider(GTpanel, var=mu,    from=-10, to=10, resolution=0.1,  action = density.draw, title="mu",     showvalue = T)  
        rp.slider(GTpanel, var=sigma, from=0.1, to=10 ,resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(GTpanel, var=nu,    from=0.1, to=10, resolution=0.01,  action = density.draw, title="nu",     showvalue = T)
        rp.slider(GTpanel, var=tau,   from=0.1, to=10, resolution=0.01,  action = density.draw, title="tau",    showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# JSU
#---------------------------------------------------------------------
demo.JSU <- function()
{  if (interactive())  
   {   mu <- sigma <- nu <- tau <- NULL 
    density.draw <- function(panel) 
         {
            y <- seq(-10, 10, length=200)
         plot(y, dJSU(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), ylab="f(y)", main="Jonhson's SU probability density functions", type="l")
         panel
         }
         JSUpanel <- rp.control('JSU family', sigma = 2, mu=0, nu=1, tau=1 ) 
        rp.slider(JSUpanel, var=mu, from=-4, to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(JSUpanel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(JSUpanel, var=nu, from=-15, to=15, resolution=0.1,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(JSUpanel, var=tau, from=0.1, to=10, resolution=0.01,  action = density.draw, title="tau",  showvalue = T)  
   }
}
# JSUo
#---------------------------------------------------------------------
demo.JSUo <- function()
{  if (interactive())  
   {   mu <- sigma <- nu <- tau <- NULL 
   density.draw <- function(panel) 
         {
            y <- seq(-10, 10, length=200)
         plot(y, dJSUo(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), ylab="f(y)", main="Jonhson's original SU probability density functions", type="l")
         panel
         }
         JSUpanel <- rp.control('JSU family', sigma = 2, mu=0, nu=1, tau=1 ) 
        rp.slider(JSUpanel, var=mu, from=-4, to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(JSUpanel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(JSUpanel, var=nu, from=-10, to=10, resolution=0.1,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(JSUpanel, var=tau, from=0.1, to=10, resolution=0.01,  action = density.draw, title="tau",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# NET
#tau must greater or equal than  nu but I do not know how to do that so I restrict 1<nu<2 ans 2<tau<10
demo.NET <- function()
{ if (interactive())  
   {   mu <- sigma <- nu <- tau <- NULL
     density.draw <- function(panel) 
         { 
          y <- seq(-10, 10, length=200)
         plot(y, dNET(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), ylab="f(y)", main="Normal-Exponential-t probability density functions", type="l")
          panel  
         }
         NETpanel <- rp.control('NET family', sigma = 1, mu=0, nu=1.5, tau=3)
        rp.slider(NETpanel, var=mu, from=-4, to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(NETpanel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(NETpanel, var=nu, from=1, to=2, resolution=0.01,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(NETpanel, var=tau, from=2.01, to=10, resolution=0.01,  action = density.draw, title="tau",  showvalue = T)  
    }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# SHASH
demo.SHASH <- function()
{ if (interactive())  
   {   mu <- sigma <- nu <- tau <- NULL
      density.draw <- function(panel) 
         {
           y <- seq(-10, 10, length=200)
         plot(y, dSHASH(x=y, mu=panel$mu, sigma=panel$sigma, nu=exp(panel$nu), tau=exp(panel$tau)), ylab="f(y)", main="SHASH probability density functions", type="l")
          panel   
         }
         SHASHpanel <- rp.control('SHASH family', sigma = 1, mu=0, nu=0, tau=0)
        rp.slider(SHASHpanel, var=mu, from=-4, to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(SHASHpanel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(SHASHpanel, var=nu,  from=-7, to=3,  resolution=0.01,  action = density.draw, title="log(nu)",  showvalue = T)
        rp.slider(SHASHpanel, var=tau, from=-7, to=3,  resolution=0.01,  action = density.draw, title="log(tau)",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# SEP1
demo.SEP1 <- function()
{
  if (interactive())  
   {
     mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
          y <- seq(-8, 8, length=200)
             plot(y, dSEP1(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$tau)), ylab="f(y)", main="SEP1  probability density functions", type="l")
          # curve(dSEP1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         SEP1panel <- rp.control('SEP1 family', sigma = 1, mu=0, nu=0, tau=1, ymax=0.8 )
        rp.slider(SEP1panel, var=mu, from=-5, to=5, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(SEP1panel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(SEP1panel, var=nu, from=-10, to=10, resolution=0.1,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(SEP1panel, var=tau, from=-1, to=7, resolution=0.01,  action = density.draw, title="log(tau)",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# SEP2 
demo.SEP2 <- function()
{
  if (interactive())  
   { 
       mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
          y <- seq(-8, 8, length=200)
             plot(y, dSEP2(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$tau)), ylab="f(y)", main="SEP2  probability density functions", type="l")
          # curve(dSEP1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         SEP2panel <- rp.control('SEP2 family', sigma = 1, mu=0, nu=0, tau=1)
        rp.slider(SEP2panel, var=mu,    from=-4,  to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(SEP2panel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(SEP2panel, var=nu,    from=-10, to=10, resolution=0.1,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(SEP2panel, var=tau,   from=-1,  to=7, resolution=0.01,  action = density.draw, title="log(tau)",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# SEP3
demo.SEP3 <- function()
{
  if (interactive())  
   {
      mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
          y <- seq(-7, 7, length=200)
             plot(y, dSEP3(x=y, mu=panel$mu, sigma=panel$sigma, nu=exp(panel$nu), tau=exp(panel$tau)), ylab="f(y)", main="SEP3  probability density functions", type="l")
          # curve(dSEP1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         SEP3panel <- rp.control('SEP3 family', sigma = 1, mu=0, nu=0, tau=1 )
        rp.slider(SEP3panel, var=mu, from=-4, to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(SEP3panel, var=sigma, from=0.01, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(SEP3panel, var=nu,  from=-3, to=7, resolution=0.01,  action = density.draw, title="log(nu)",  showvalue = T)
        rp.slider(SEP3panel, var=tau, from=-3, to=7, resolution=0.01,  action = density.draw, title="log(tau)",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# SEP4
demo.SEP4 <- function()
{
  if (interactive())  
   {     mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
          y <- seq(-7, 7, length=200)
             plot(y, dSEP4(x=y, mu=panel$mu, sigma=panel$sigma, nu=exp(panel$nu), tau=exp(panel$tau)), ylab="f(y)", main="SEP4  probability density functions", type="l")
          # curve(dSEP1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         SEP4panel <- rp.control('SEP4 family', sigma = 1, mu=0, nu=1, tau=1)
        rp.slider(SEP4panel, var=mu, from=-4, to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(SEP4panel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(SEP4panel, var= nu, from=-3, to=7, resolution=0.01,  action = density.draw, title="log(nu)",  showvalue = T)
        rp.slider(SEP4panel, var=tau, from=-3, to=7, resolution=0.01,  action = density.draw, title="log(tau)",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# ST1
demo.ST1 <- function()
{
  if (interactive())  
   {     mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
            y <- seq(-7, 7, length=200)
             plot(y, dST1(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), ylab="f(y)", main="ST1  probability density functions", type="l")
          # curve(dST1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF", main="Skew t type 1 distribution")
          panel
          }
         ST1panel <- rp.control('ST1 family', sigma = 1, mu=0, nu=0, tau=1)
        rp.slider(ST1panel, var=mu, from=-4, to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(ST1panel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(ST1panel, var=nu, from=-10, to=10, resolution=0.01,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(ST1panel, var=tau, from=0.1, to=7, resolution=0.01,  action = density.draw, title="tau",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# ST2
demo.ST2 <- function()
{
  if (interactive())  
   {      mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
            y <- seq(-7, 7, length=200)
             plot(y, dST2(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), ylab="f(y)", main="ST2  probability density functions", type="l")
          # curve(dST1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF", main="Skew t type 1 distribution")
          panel
          }
         ST2panel <- rp.control('ST2 family', sigma = 1, mu=0, nu=0, tau=1)
        rp.slider(ST2panel, var=mu, from=-4, to=4,    resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(ST2panel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(ST2panel, var=nu, from=-10, to=10, resolution=0.01,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(ST2panel, var=tau, from=0.1, to=7, resolution=0.01,  action = density.draw, title="tau",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# ST3
demo.ST3 <- function()
{
  if (interactive())  
   {    mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
            y <- seq(-7, 7, length=200)
             plot(y, dST3(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), ylab="f(y)", main="ST3  probability density functions", type="l")
          # curve(dST1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF", main="Skew t type 1 distribution")
          panel
          }
         ST3panel <- rp.control('ST2 family', sigma = 1, mu=0, nu=1, tau=1)
        rp.slider(ST3panel, var=mu, from=-4, to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(ST3panel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(ST3panel, var=nu,  from=0.1, to=7, resolution=0.01,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(ST3panel, var=tau, from=0.1, to=7, resolution=0.01,  action = density.draw, title="tau",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# ST4 
demo.ST4 <- function()
{
  if (interactive())  
   {     mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
            y <- seq(-7, 7, length=200)
             plot(y, dST4(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), ylab="f(y)", main="ST4  probability density functions", type="l")
          # curve(dST1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF", main="Skew t type 1 distribution")
          panel
          }
         ST4panel <- rp.control('ST4 family', sigma = 1, mu=0, nu=1, tau=1)
        rp.slider(ST4panel, var=mu, from=-4, to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(ST4panel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(ST4panel, var=nu,  from=0.1, to=10, resolution=0.01,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(ST4panel, var=tau, from=0.1, to=10, resolution=0.01,  action = density.draw, title="tau",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# ST5
demo.ST5 <- function()
{
  if (interactive())  
   {     mu <- sigma <- nu <- tau <- NULL
        density.draw <- function(panel) 
         {
            y <- seq(-7, 7, length=200)
             plot(y, dST5(x=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), ylab="f(y)", main="ST5  probability density functions", type="l")
          # curve(dST1(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=exp(panel$ltau)), -5, 5, ylim=c(0,panel$ymax), ylab="PDF", main="Skew t type 1 distribution")
          panel
          }
         ST5panel <- rp.control('ST2 family', sigma = 1, mu=0, nu=0, tau=1)
        rp.slider(ST5panel, var=mu, from=-4, to=4, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(ST5panel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(ST5panel, var=nu, from=-10, to=10, resolution=0.01,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(ST5panel, var=tau, from=0.1, to=7, resolution=0.01,  action = density.draw, title="tau",  showvalue = T)  
   }
}
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# 1 parameter
# EXP
# 1 parameter distributions
demo.EXP <- function()
{
 if (interactive()) 
   {
         mu <-  NULL
        density.draw <- function(panel) 
         {
         op <- par(mfrow = c(2, 1))
            y <- seq(0.01, 25, length=200)
           plot(y, dEXP(x=y, mu=panel$mu), ylab="f(y)", main="Exponential probability density function", type="l")
           plot(y, pEXP(q=y, mu=panel$mu), ylim=c(0.001,1), type="s", ylab="F(y)", main="Exponetial cumulative probability function")
         par(op) 
          panel
          }
         EXPpanel <- rp.control('Exponetial distribution', sigma = 1, mu = 1)
        rp.slider(EXPpanel,  var=mu, from=0.1, to=10, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
   }
}
#-------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# 2 parameter distributions
# GA
demo.GA <- function()
{
 if (interactive()) 
   {
       mu <- sigma <- NULL
        density.draw <- function(panel) 
         {
           x <- seq(0.01, 30, length=200)
           plot(x, dGA(x, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="GA  probability density functions", type="l") 
           #curve(dGA(x, mu=panel$mu, sigma=panel$sigma), 0.001, 10, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         GApanel <- rp.control('Gamma family', sigma = 1, mu = 1,  ymax=1 )
        rp.slider(GApanel,  var=mu, from=0.01, to=10, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(GApanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
}
#-------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------   
# LOGNO
demo.LOGNO <- function()
{
 if (interactive()) 
   {
       mu <- sigma <-  NULL
        density.draw <- function(panel) 
         { x <- seq(0.01, 30, length=200)
           plot(x, dLOGNO(x, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="LOGNO  probability density functions", type="l") 
          # curve(dLOGNO(x, mu=panel$mu, sigma=panel$sigma), 0.001, 10, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         LOGNOpanel <- rp.control('Log-normal family', sigma = 1, mu = 1,  ymax=1 )
        rp.slider(LOGNOpanel,  var=mu, from=0.1, to=5, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(LOGNOpanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
}
#-------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
demo.NO.LOGNO <- function()
{
 if (interactive()) 
   {
       mu <- sigma <-  x <- NULL
        density.draw <- function(panel) 
         {
          op<-par(mfrow=c(2,1)) 
           curve(dNO(x,    mu=panel$mu, sigma=panel$sigma), -4, 4,  ylab="PDF", main="Normal")
           curve(dLOGNO(x, mu=panel$mu, sigma=panel$sigma),  0, 4,   ylab="PDF", main="Log Normal")
           par(op) 
          panel
          }
         NOLOGNOpanel <- rp.control('Normal and Log Normal family', sigma = 1, mu=0)
        rp.slider(NOLOGNOpanel,  var=mu, from=-2, to=2, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(NOLOGNOpanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
}
#-----------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# IG
# 2 parameter distributions
demo.IG <- function()
{
 if (interactive()) 
   {
       mu <- sigma <- NULL
        density.draw <- function(panel) 
         {
           x <- seq(0.01, 30, length=200)
           plot(x, dIG(x, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="IG  probability density functions", type="l") 
           #curve(dGA(x, mu=panel$mu, sigma=panel$sigma), 0.001, 10, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         IGpanel <- rp.control('Inverse Gaussian family', sigma = .5, mu = 5)
        rp.slider(IGpanel,  var=  mu,  from=0.1, to=20, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(IGpanel,  var=sigma, from=0.1, to=5,  resolution=0.01,  action = density.draw, title="log(sigma)",  showvalue = T) 
   }
}

#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# WEI
demo.WEI <- function()
{
 if (interactive()) 
   {
       mu <- sigma <- NULL
        density.draw <- function(panel) 
         {
           x <- seq(0.01, 30, length=200)
           plot(x, dWEI(x, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="Weibull  probability density functions", type="l") 
           #curve(dGA(x, mu=panel$mu, sigma=panel$sigma), 0.001, 10, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         WEIpanel <- rp.control('Weibull distribution', sigma = 1, mu = 5)
        rp.slider(WEIpanel,  var=mu, from=0.1, to=20, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(WEIpanel,  var=sigma, from=0.1, to=4, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# WEI2
demo.WEI2 <- function()
{
 if (interactive()) 
   {
       mu <- sigma <- NULL
        density.draw <- function(panel) 
         {
           x <- seq(0.01, 30, length=200)
           plot(x, dWEI2(x, mu=exp(panel$mu), sigma=exp(panel$sigma)), ylab="f(y)", main="Weibull 2  probability density functions", type="l") 
           #curve(dGA(x, mu=panel$mu, sigma=panel$sigma), 0.001, 10, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         WEIpanel <- rp.control('Weibull 2 distribution', sigma = 0, mu = 0,  ymax=1 )
        rp.slider(WEIpanel,  var=mu,    from=-7, to=3, resolution=0.01,  action = density.draw, title="log(mu)",  showvalue = T)  
        rp.slider(WEIpanel,  var=sigma, from=-3, to=3, resolution=0.01,  action = density.draw, title="log(sigma)",  showvalue = T) 
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# WEI3
demo.WEI3 <- function()
{
 if (interactive()) 
   {
       mu <- sigma <- NULL
        density.draw <- function(panel) 
         {
           x <- seq(0.01, 30, length=200)
           plot(x, dWEI3(x, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="Weibull 3 probability density functions", type="l") 
           #curve(dGA(x, mu=panel$mu, sigma=panel$sigma), 0.001, 10, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         WEIpanel <- rp.control('Weibull 3 distribution', sigma = 2, mu = 5)
        rp.slider(WEIpanel,  var=mu,    from=0.1, to=20, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(WEIpanel,  var=sigma, from=0.1, to=4, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
}

#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# BCCG
demo.BCCG <- function()
{
 if (interactive()) 
   {
      mu <- sigma <- nu <- tau <- x <- NULL
        density.draw <- function(panel) 
         {
          x <- seq(0.01, 20, length=200)
           plot(x,dBCCG(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), ylab="f(y)", main="BCCG  probability density functions", type="l") 
          # curve(dBCCG(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), 0.001, 10, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         BCTpanel <- rp.control('BCCG family', sigma = 0.1, mu=10, nu=1)
        rp.slider(BCTpanel, var=mu, from=1, to=20, resolution=0.1,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(BCTpanel, var=sigma, from=0.01, to=0.5, resolution=0.001,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(BCTpanel, var=nu, from=-4, to=6, resolution=0.1,  action = density.draw, title="nu",  showvalue = T) 
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# GG
demo.GG <- function()
{
 if (interactive()) 
   {
      mu <- sigma <- nu <- tau <- x <- NULL
        density.draw <- function(panel) 
         {
           x <- seq(0.01, 30, length=200)
           plot(x,dGG(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), ylab="f(y)", main="Generalized Gamma  probability density functions", type="l") 
           #curve(dGG(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), 0.001, 10, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         GGpanel <- rp.control('Genaralized  Gamma family', sigma = 1, mu=5, nu=0)
        rp.slider(GGpanel, var=mu, from=.1, to=10, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(GGpanel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(GGpanel, var=nu, from=-5, to=5, resolution=0.1,  action = density.draw, title="nu",  showvalue = T) 
   }
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
demo.GIG <- function()
{
 if (interactive()) 
   {
      mu <- sigma <- nu <- tau <- x <- NULL
        density.draw <- function(panel) 
         {
           x <- seq(0.01, 30, length=200)
           plot(x,dGIG(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), ylab="f(y)", main="Generalized Inverse Gaussian  probability density functions", type="l") 
           #curve(dGG(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), 0.001, 10, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         GGpanel <- rp.control('Generalized Inverse Gaussian  family', sigma = 1, mu=5, nu=0)
        rp.slider(GGpanel, var=mu, from=.1, to=10, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(GGpanel, var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(GGpanel, var=nu, from=-5, to=5, resolution=0.1,  action = density.draw, title="nu",  showvalue = T) 
   }
 }
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# ZAGA
demo.ZAGA <- function()
  {
if (interactive()) 
    {
       mu <- sigma <- nu <- NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       y <- seq(0, 20, length=200)
       plotZAGA(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu, ylab="f(y)", main="Zero adjusted Gamma p.d.f.")
       plot(y, pZAGA(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), ylim=c(0.001,1), type="s", ylab="F(y)", main="Zero adjusted Gamma c.d.f.")
       points(0, pZAGA(0, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), col="blue")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('ZAGA',  mu= 5, sigma=.5, nu=0.1, ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.01, to=10, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.01, to=2, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T)  
         rp.slider(ppoispanel,  var=nu, from=0.01, to=0.99, resolution=0.01,  action = density.draw, title="nu",  showvalue = T)  

   }
  }
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# ZAIG
demo.ZAIG <- function()
  {
if (interactive()) 
    {
       mu <- sigma <- nu <- NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       y <- seq(0, 20, length=200)
       plotZAIG(y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, ylab="f(y)", main="Zero adjusted inverse Gaussian p.d.f." )
       plot(y, pZAIG(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), ylim=c(0.001,1), type="s", ylab="F(y)", main="Zero adjusted Inverse Gaussian c.d.f.")
       points(0, pZAIG(0, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), col="blue")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('DEL',  mu= 5, sigma=.5, nu=0.1, ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.01, to=10, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.01, to=2, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T)  
         rp.slider(ppoispanel,  var=nu, from=0.01, to=0.99, resolution=0.01,  action = density.draw, title="nu",  showvalue = T)  

   }
  }
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# BCT
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
demo.BCT <- function()
{
 if (interactive()) 
   {
      mu <- sigma <- nu <- tau <- x <- NULL
        density.draw <- function(panel) 
         {
         y <- seq(0, 10, length=200)
         plot(y, dBCT(y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, , tau=panel$tau), ylab="f(y)", main="Box-Cox t p.d.f." , type="l")
         #  curve(dBCT(x, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), 0.001, 10, ylim=c(0,panel$ymax), ylab="PDF")
          panel
          }
         BCTpanel <- rp.control('BCT family', sigma = 0.1, mu=5, nu=1, tau=10 )
        rp.slider(BCTpanel, var=mu, from=1, to=10, resolution=0.1,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(BCTpanel, var=sigma, from=0.01, to=0.5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(BCTpanel, var=nu, from=-4, to=6, resolution=0.1,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(BCTpanel, var=tau, from=1, to=30, resolution=0.1,  action = density.draw, title="tau",  showvalue = T)  
   }
}
#----------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# BCPE
demo.BCPE <- function()
{
  if (interactive()) 
 {
     mu <- sigma <- nu <- tau <- x <- NULL
        density.draw <- function(panel) 
         {
           y <- seq(0, 10, length=200)
         plot(y, dBCPE(y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, , tau=panel$tau), ylab="f(y)", main="Box-Cox power exponential p.d.f." , type="l")
          panel
          }
         BCPEpanel <- rp.control('BCPE family', sigma = 0.1, mu=5, nu=1, tau=2 )
        rp.slider(BCPEpanel, var=mu,     from=1, to=10, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(BCPEpanel, var=sigma, from=0.01, to=0.5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(BCPEpanel, var=nu,    from=-4, to=6, resolution=0.1,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(BCPEpanel, var=tau, from=.1, to=10, resolution=0.01,  action = density.draw, title="tau",  showvalue = T)  
 }
}
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#GB2
#----------------------------------------------------------------------------------------
demo.GB2 <- function()
{
 if (interactive()) 
   {    mu <- sigma <- nu <- tau <- x <- NULL
        density.draw <- function(panel) 
         {
            y <- seq(0, 20, length=200)
         plot(y, dGB2(y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, , tau=panel$tau), ylab="f(y)", main="Generalised Beta type 2 p.d.f." , type="l")
          panel
          }
         GB2panel <- rp.control('GB2 family', sigma = 1, mu=5, nu=1, tau=1)
        rp.slider(GB2panel, var=mu, from=1, to=10, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(GB2panel, var=sigma, from=-10, to=10, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
        rp.slider(GB2panel, var=nu, from=.1, to=6, resolution=0.01,  action = density.draw, title="nu",  showvalue = T)
        rp.slider(GB2panel, var=tau, from=.1, to=6, resolution=0.01,  action = density.draw, title="tau",  showvalue = T)  
   }
}


#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# continuous in 0 to 1
# BE
#--------------------------------------------------------------------------
demo.BE <- function()
{
 if (interactive()) 
   {   mu <- sigma <- NULL
        density.draw <- function(panel) 
         {
          y <- seq(0.01, .999, length=200)
           plot(y, dBE(y, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="Beta distribution p.d.f." , type="l")
           #  curve(dBE(x, mu=panel$mu, sigma=panel$sigma), 0.001, 0.999, ylim=c(0,panel$ymax), ylab="PDF",main="BE")
          panel
          }
         BEpanel <- rp.control('BE family', sigma = 0.5, mu = 0.5)
        rp.slider(BEpanel,  var=mu,    from=0.01, to=.99,  resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(BEpanel,  var=sigma, from=0.01, to=0.99, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
}
#-------------
#--------------------------------------------------------------------------
demo.BEo <- function()
{
 if (interactive()) 
   {
        mu <- sigma <-  NULL
        density.draw <- function(panel) 
         {
          y <- seq(0.001, .999, length=200)
           plot(y, dBEo(y, mu=panel$mu, sigma=panel$sigma), ylab="f(y)", main="Beta distribution p.d.f." , type="l")
           #  curve(dBE(x, mu=panel$mu, sigma=panel$sigma), 0.001, 0.999, ylim=c(0,panel$ymax), ylab="PDF",main="BE")
          panel
          }
         BEpanel <- rp.control('BE family', sigma = 5, mu = 5)
        rp.slider(BEpanel,  var=mu, from=0.01, to=10, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
        rp.slider(BEpanel,  var=sigma, from=0.01, to=10, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T) 
   }
}
#-----------------------------------------------------------------------------------
# GB1
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
demo.GB1 <- function()
{
 {if (interactive()) 
    {
       mu <- sigma <- nu <- tau <- NULL   
     density.draw <- function(panel) 
           { #   op<-par(mfrow = c(2, 1))
       y <- seq(0.001, .999,length=200)
       plot(y, dGB1(x= y,  mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), type="l", ylab="f(y)", main="GB1 probability density function")
      # plot(y, pGB1(q= y,  mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), type="l", ylim=c(0.001,1),  ylab="F(y)", main="GB1 cumulative probability function")
       #par(op) 
       panel }
          ppoispanel <- rp.control('GB1',  mu= 0.5, sigma=0.5, nu=1,tau=1, ymax=1 )
          rp.slider(ppoispanel,  var=mu,    from=0.01,  to = 0.99, resolution = 0.01,  action = density.draw, title="mu",  showvalue = T) 
          rp.slider(ppoispanel,  var=sigma, from=0.01,  to = 0.99, resolution = 0.01,  action = density.draw, title="sigma",  showvalue = T)  
          rp.slider(ppoispanel,  var=nu,    from = 0.1,to = 10,   resolution = 0.01,   action = density.draw, title="nu",  showvalue = T) 
          rp.slider(ppoispanel,  var=tau,   from = 0.1,to = 10,   resolution = 0.01,   action = density.draw, title="tau",  showvalue = T) 
   }
 }
}
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
demo.BEINF <- function()
{
 {if (interactive())         
    {   
           mu <- sigma <- nu <- tau <- NULL
          density.draw <- function(panel) 
           {   
           op<-par(mfrow = c(2, 1))
         y <- seq(0.001, .999,length=200)
       plotBEINF(mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau, ylab="f(y)", main="probability density function")
       plot(y, pBEINF(q=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), type="s", ylim=c(0.001,1),  ylab="F(y)", main="cumulative probability function")
       points(0, pBEINF(0, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), type="h")
       points(0, pBEINF(0, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), col="blue")
       points(1, pBEINF(1, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), type="h")
       points(1, pBEINF(1, mu=panel$mu, sigma=panel$sigma, nu=panel$nu, tau=panel$tau), col="blue")
       par(op) 
         panel 
         }
          ppoispanel <- rp.control('BEINF',  mu= 0.5, sigma=0.5, nu=0.5,tau=0.5)
          rp.slider(ppoispanel,  var=mu,    from=0.01,  to = 0.99, resolution = 0.01,  action = density.draw, title="mu",  showvalue = T) 
          rp.slider(ppoispanel,  var=sigma, from=0.01,  to = 0.99, resolution = 0.01,  action = density.draw, title="sigma",  showvalue = T)  
          rp.slider(ppoispanel,  var=nu,    from = 0.1, to = 10,   resolution = 0.01,   action = density.draw, title="nu",  showvalue = T) 
          rp.slider(ppoispanel,  var=tau,   from = 0.1, to = 10,   resolution = 0.01,   action = density.draw, title="tau",  showvalue = T) 
   }
 }
}

#-----------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
demo.BEINF0 <- function()
{
 {if (interactive())         
    {   
           mu <- sigma <- nu <- tau <- NULL
          density.draw <- function(panel) 
           {   
           op<-par(mfrow = c(2, 1))
         y <- seq(0.001, .999,length=200)
       plotBEINF0(mu=panel$mu, sigma=panel$sigma, nu=panel$nu, ylab="f(y)", main="probability density function")
         plot(y, pBEINF0(q=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), type="s", ylim=c(0.001,1),  ylab="F(y)", main="cumulative probability function")
       points(0, pBEINF0(0, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), type="h")
       points(0, pBEINF0(0, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), col="blue")
       par(op) 
         panel 
         }
          ppoispanel <- rp.control('BEINF',  mu= 0.5, sigma=0.5, nu=0.5)
          rp.slider(ppoispanel,  var=mu,    from=0.01,  to = 0.99, resolution = 0.01,  action = density.draw, title="mu",  showvalue = T) 
          rp.slider(ppoispanel,  var=sigma, from=0.01,  to = 0.99, resolution = 0.01,  action = density.draw, title="sigma",  showvalue = T)  
          rp.slider(ppoispanel,  var=nu,    from = 0.1,to = 10,   resolution = 0.1,   action = density.draw, title="nu",  showvalue = T) 
   }
 }
}
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
demo.BEINF1 <- function()
{
 {if (interactive())         
    {   
           mu <- sigma <- nu <- tau <- NULL
          density.draw <- function(panel) 
           {   
           op<-par(mfrow = c(2, 1))
         y <- seq(0.001, .999,length=200)
       plotBEINF1(mu=panel$mu, sigma=panel$sigma, nu=panel$nu, ylab="f(y)", main="probability density function")
         plot(y, pBEINF1(q=y, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), type="s", ylim=c(0.001,1),  ylab="F(y)", main="cumulative probability function")
        points(1, pBEINF1(1, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), type="h")
        points(1, pBEINF1(1, mu=panel$mu, sigma=panel$sigma, nu=panel$nu), col="blue")
       par(op) 
         panel 
         }
          ppoispanel <- rp.control('BEINF',  mu= 0.5, sigma=0.5, nu=0.5)
          rp.slider(ppoispanel,  var=mu,    from=0.01,  to = 0.99, resolution = 0.01,  action = density.draw, title="mu",  showvalue = T) 
          rp.slider(ppoispanel,  var=sigma, from=0.01,  to = 0.99, resolution = 0.01,  action = density.draw, title="sigma",  showvalue = T)  
          rp.slider(ppoispanel,  var=nu,    from = 0.1,to = 10,   resolution = 0.1,   action = density.draw, title="nu",  showvalue = T) 
   }
 }
}
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
# count data
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# one parameter
demo.PO <- function()
{
if (interactive()) 
    {
       mu <-  NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       x <- seq(0, 50, 1)
       plot(x, dPO(x, mu=panel$mu), type="h", ylab="f(x)", main="probability density function")
       points(x, dPO(x, mu=panel$mu), col="blue")
       plot(x, pPO(x, mu=panel$mu), type="s", ylab="F(x)", main="cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('Poisson',  mu= 5,  ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.1, to=30, resolution=0.01,  action = density.draw, title="mu",  showvalue = T)  
   }
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# LG
#----------------------------------------------------------
demo.LG <- function()
{
if (interactive()) 
    {
        mu <- NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       x <- seq(1, 50, 1)
       plot(x, dZALG(x, mu=panel$mu), type="h", ylab="f(x)", main="LG probability density function")
       points(x, dZALG(x, mu=panel$mu), col="blue")
       plot(x, pZALG(x, mu=panel$mu), type="s", ylab="F(x)", main="LG cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('ZALG',  mu= .5, sigma=.5,  ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.01, to=.99, resolution=0.001,  action = density.draw, title="mu",  showvalue = T) 
   }
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
demo.NBI <- function()
{
if (interactive()) 
    {
        mu <- sigma <- NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       x <- seq(0, 50, 1)
       plot(x, dNBI(x, mu=panel$mu, sigma=panel$sigma), type="h", ylab="f(x)", main="NBI probability density function")
       points(x, dNBI(x, mu=panel$mu, sigma=panel$sigma), col="blue")
       plot(x, pNBI(x, mu=panel$mu, sigma=panel$sigma), type="s", ylab="F(x)", main="NBI cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('NBI',  mu= 5, sigma=1,  ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.1, to=30, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T)  
   }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
demo.NBII <- function()
{
if (interactive()) 
    {
       mu <- sigma <- NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       x <- seq(0, 50, 1)
       plot(x, dNBII(x, mu=panel$mu, sigma=panel$sigma), type="h", ylab="f(x)", main="NBII probability density function")
       points(x, dNBII(x, mu=panel$mu, sigma=panel$sigma), col="blue")
       plot(x, pNBII(x, mu=panel$mu, sigma=panel$sigma), type="s", ylab="F(x)", main="NBII cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('NBII',  mu= 5, sigma=1,  ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.1, to=30, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.1, to=20, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T)  
   }
}
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#PIG
demo.PIG <- function()
{
if (interactive()) 
    {
       mu <- sigma <- nu <-  NULL
         density.draw <- function(panel) 
          {
       op <- par(mfrow = c(2, 1))
        y <- seq(0, 50, 1)
         plot(y, dPIG(y, mu=panel$mu, sigma=panel$sigma), type="h", ylab="f(y)", main="PIG probability density function")
       points(y, dPIG(y, mu=panel$mu, sigma=panel$sigma), col="blue")
         plot(y, pPIG(y, mu=panel$mu, sigma=panel$sigma), type="s", ylab="F(y)", main="PIG cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('PIG',  mu= 5, sigma=1, ymax=1 )
         rp.slider(ppoispanel,  var=mu,    from=0.1, to=30, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T)  
   }
}
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# DEL
demo.DEL <- function()
{
if (interactive()) 
    {
       mu <- sigma <- nu <-  NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       y <- seq(0, 50, 1)
       plot(y, dDEL(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), type="h", ylab="f(y)", main="probability density function")
       points(y, dDEL(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), col="blue")
       plot(y, pDEL(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), type="s", ylab="F(y)", main="cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('DEL',  mu= 5, sigma=1, nu=0.5, ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.1, to=30, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.1, to=10, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T)  
         rp.slider(ppoispanel,  var=nu, from=0.01, to=0.99, resolution=0.001,  action = density.draw, title="nu",  showvalue = T)  

   }
}
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# SICHEL
#-----------------------------------------------------------------------------
demo.SICHEL <- function()
{
if (interactive()) 
    {
        mu <- sigma <- nu <-  NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       y <- seq(0, 50, 1)
       plot(y, dSICHEL(y,   mu=panel$mu,   sigma=panel$sigma, nu=panel$nu), type="h", ylab="f(y)", main="probability density function")
       points(y, dSICHEL(y, mu=panel$mu,   sigma=panel$sigma, nu=panel$nu), col="blue")
       plot(y, pSICHEL(y,   mu=panel$mu,   sigma=panel$sigma, nu=panel$nu), type="s", ylab="F(y)", main="cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('SICHEL',  mu= 5, sigma=1, nu=0, ymax=1 )
         rp.slider(ppoispanel,  var=mu,    from=0.1, to=30, resolution=0.1,   action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.1, to=5,  resolution=0.01,  action = density.draw, title="sigma",  showvalue = T)  
         rp.slider(ppoispanel,  var=nu,    from=-4,  to=4,  resolution=0.01,  action = density.draw, title="nu",  showvalue = T)  

   }
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
demo.ZIP <- function()
{
if (interactive()) 
    {
       mu <- sigma <-  NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       x <- seq(0, 50, 1)
       plot(x, dZIP(x, mu=panel$mu, sigma=panel$sigma), type="h", ylab="f(x)", main="probability density function")
       points(x, dZIP(x, mu=panel$mu, sigma=panel$sigma), col="blue")
       plot(x, pZIP(x, mu=panel$mu, sigma=panel$sigma), type="s", ylab="F(x)", main="cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('ZIP',  mu= 5, sigma=.2,  ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.1, to=30, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.01, to=.99, resolution=0.001,  action = density.draw, title="sigma",  showvalue = T)  
   }
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
demo.ZIP2 <- function()
{
if (interactive()) 
    {
       mu <- sigma <-  NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       x <- seq(0, 50, 1)
       plot(x, dZIP2(x, mu=panel$mu, sigma=panel$sigma), type="h", ylab="f(x)", main="probability density function")
       points(x, dZIP2(x, mu=panel$mu, sigma=panel$sigma), col="blue")
       plot(x, pZIP2(x, mu=panel$mu, sigma=panel$sigma), type="s", ylab="F(x)", main="cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('ZIP2',  mu= 5, sigma=.2,  ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.1, to=30, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.01, to=.99, resolution=0.001,  action = density.draw, title="sigma",  showvalue = T)  
   }
}
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
demo.ZAP <- function()
{
if (interactive()) 
    {
        mu <- sigma <- NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       x <- seq(0, 40, 1)
       plot(x, dZAP(x, mu=panel$mu, sigma=panel$sigma), type="h", ylab="f(x)", main="ZAP probability density function")
       points(x, dZAP(x, mu=panel$mu, sigma=panel$sigma), col="blue")
       plot(x, pZAP(x, mu=panel$mu, sigma=panel$sigma), type="s", ylab="F(x)", main="ZAP cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('ZAP',  mu= 5, sigma=.1,  ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.1, to=20, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.01, to=.99, resolution=0.001,  action = density.draw, title="sigma",  showvalue = T)  
   }
}
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
demo.ZALG <- function()
{
if (interactive()) 
    {
        mu <- sigma <-  NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       x <- seq(0, 50, 1)
       plot(x, dZALG(x, mu=panel$mu, sigma=panel$sigma), type="h", ylab="f(x)", main="ZALG probability density function")
       points(x, dZALG(x, mu=panel$mu, sigma=panel$sigma), col="blue")
       plot(x, pZALG(x, mu=panel$mu, sigma=panel$sigma), type="s", ylab="F(x)", main="ZALG cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('ZALG',  mu= .5, sigma=.5,  ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.01, to=.99, resolution=0.001,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.01, to=.99, resolution=0.001,  action = density.draw, title="sigma",  showvalue = T)  
   }
}
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# ZANBI
demo.ZANBI <- function()
{
if (interactive()) 
    {
       mu <- sigma <- nu <-  NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       y <- seq(0, 50, 1)
       plot(y, dZANBI(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), type="h", ylab="f(y)", main="ZANBI probability density function")
       points(y, dZANBI(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), col="blue")
       plot(y, pZANBI(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), type="s", ylab="F(y)", main="ZANBI cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('ZANBI',  mu= 5, sigma=1, nu=0.1, ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.1, to=30, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.1, to=10, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T)  
         rp.slider(ppoispanel,  var=nu, from=0.01, to=0.99, resolution=0.001,  action = density.draw, title="nu",  showvalue = T)  

   }
}
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# ZINBI
demo.ZINBI <- function()
{
if (interactive()) 
    {
       mu <- sigma <- nu <-  NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       y <- seq(0, 50, 1)
       plot(y, dZINBI(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), type="h", ylab="f(y)", main="ZINBI probability density function")
       points(y, dZINBI(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), col="blue")
       plot(y, pZINBI(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), type="s", ylab="F(y)", main="ZINBI cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('ZINBI',  mu= 5, sigma=1, nu=0.1, ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.1, to=30, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.1, to=5, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T)  
         rp.slider(ppoispanel,  var=nu, from=0.01, to=0.99, resolution=0.001,  action = density.draw, title="nu",  showvalue = T)  

   }
}
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# ZANBII


#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# ZINBII



#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# ZIPIG
demo.ZIPIG <- function()
{
if (interactive()) 
    {
       mu <- sigma <- nu <-  NULL
         density.draw <- function(panel) 
          {
 
       op<-par(mfrow = c(2, 1))
       y <- seq(0, 50, 1)
       plot(y, dZIPIG(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), type="h", ylab="f(y)", main="ZIPIG probability density function")
       points(y, dZIPIG(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), col="blue")
       plot(y, pZIPIG(y, mu=panel$mu, sigma=panel$sigma,nu=panel$nu), type="s", ylab="F(y)", main="ZIPIG cumulative probability function")
      par(op) 
      panel
           }
         ppoispanel <- rp.control('ZIPIG',  mu=5, sigma=1, nu=0.1, ymax=1 )
         rp.slider(ppoispanel,  var=mu, from=0.1, to=30, resolution=0.01,  action = density.draw, title="mu",  showvalue = T) 
         rp.slider(ppoispanel,  var=sigma, from=0.1, to=10, resolution=0.01,  action = density.draw, title="sigma",  showvalue = T)  
         rp.slider(ppoispanel,  var=nu, from=0.01, to=0.99, resolution=0.001,  action = density.draw, title="nu",  showvalue = T)  

   }
}

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# binomial type
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
demo.BI <- function()
{if (interactive())     
      {   
       mu <- 0.5 ; N <- 10
      plotf <- function(panel) 
         {   
              with(panel, {
         mu   <- as.numeric(panel$mu)
         N    <- as.numeric(panel$N)
         xgrid <- seq(0, N, step=1, na.rm = TRUE)
         dgrid <- dBI(xgrid, mu=mu, bd=N)
         plot(xgrid, dgrid, type = "h", col = "blue", lwd = 3, ylab="BI dist.", xlab="x")
                          })
      panel
         }
            }
   bipanel <- rp.control("binomial distribution", N=10, mu=.5)
   rp.slider(bipanel, var = mu, from=0.01, to=1, resolution=0.01,  action =  plotf, title="mu",  showvalue = T)  
   rp.textentry(bipanel, N, plotf, labels = c("N"), initval = c(10))
   rp.do(bipanel, plotf)
        #rp.textentry(tables.panel, xobs, tables.redraw, "Observed value",
         #           pos = c(140, 75, 160, 25))
}
#---------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
demo.BB <- function()
{if (interactive())     
      {   
        mu <- 0.5 ; sigma <- 0.5 ; N <- 10
      plotf <- function(panel) 
         {   
              with(panel, {
         mu   <- as.numeric(panel$mu)
      sigma   <- as.numeric(panel$sigma)
         N    <- as.numeric(panel$N)
         xgrid <- seq(0, N, step=1, na.rm = TRUE)
         dgrid <- dBB(xgrid, mu=mu, sigma=sigma, bd=N)
         plot(xgrid, dgrid, type = "h", col = "blue", lwd = 3, ylab="BB dist.", xlab="x")
                          })
      panel
         }
            }
   bipanel <- rp.control("binomial distribution", N=10, mu=.5, sigma=1)
   rp.slider(bipanel, var = mu, from=0.001, to=.999, resolution=0.001,  action =  plotf, title="mu",  showvalue = T) 
   rp.slider(bipanel, var = sigma, from=0.01, to=3, resolution=0.001,  action =  plotf, title="sigma",  showvalue = T)   
   rp.textentry(bipanel, N, plotf, labels = c("N"), initval = c(10))
   rp.do(bipanel, plotf)
        #rp.textentry(tables.panel, xobs, tables.redraw, "Observed value",
         #           pos = c(140, 75, 160, 25))
}
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
demo.ZABI <- function()
{if (interactive())     
      {   
        mu <- 0.5; sigma <- 0.1 ; N <- 10
      plotf <- function(panel) 
         {   
              with(panel, {
         mu   <- as.numeric(panel$mu)
      sigma   <- as.numeric(panel$sigma)
         N    <- as.numeric(panel$N)
         xgrid <- seq(0, N, step=1, na.rm = TRUE)
         dgrid <- dZABI(xgrid, mu=mu, sigma=sigma, bd=N)
         plot(xgrid, dgrid, type = "h", col = "blue", lwd = 3, ylab="BB dist.", xlab="x")
                          })
      panel
         }
            }
   bipanel <- rp.control("ZABI distribution", N=10, mu=.5, sigma=.1)
   rp.slider(bipanel, var = mu, from=0.001, to=0.999, resolution=0.001,  action =  plotf, title="mu",  showvalue = T) 
   rp.slider(bipanel, var = sigma, from=0.001, to=0.999, resolution=0.001,  action =  plotf, title="sigma",  showvalue = T)   
   rp.textentry(bipanel, N, plotf, labels = c("N"), initval = c(10))
   rp.do(bipanel, plotf)
        #rp.textentry(tables.panel, xobs, tables.redraw, "Observed value",
         #           pos = c(140, 75, 160, 25))
}
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
demo.ZIBI <- function()
{if (interactive())     
      {   
        mu <- sigma <- N <- NULL
      plotf <- function(panel) 
         {   
              with(panel, {
         mu   <- as.numeric(panel$mu)
      sigma   <- as.numeric(panel$sigma)
         N    <- as.numeric(panel$N)
         xgrid <- seq(0, N, step=1, na.rm = TRUE)
         dgrid <- dZIBI(xgrid, mu=mu, sigma=sigma, bd=N)
         plot(xgrid, dgrid, type = "h", col = "blue", lwd = 3, ylab="BB dist.", xlab="x")
                          })
      panel
         }
            }
   bipanel <- rp.control("ZIBI distribution", N=10, mu=.5, sigma=.1)
   rp.slider(bipanel, var = mu, from=0.001, to=.999, resolution=0.001,  action =  plotf, title="mu",  showvalue = T) 
   rp.slider(bipanel, var = sigma, from=0.001, to=.999, resolution=0.001,  action =  plotf, title="sigma",  showvalue = T)   
   rp.textentry(bipanel, N, plotf, labels = c("N"), initval = c(10))
   rp.do(bipanel, plotf)
        #rp.textentry(tables.panel, xobs, tables.redraw, "Observed value",
         #           pos = c(140, 75, 160, 25))
}
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# ZABB
#------------------------------------------------------------------------------------------
demo.ZABB <- function()
{if (interactive())     
      {   
        mu <- 0.5 ; sigma <- 0.5 ; nu <-.1 ; N <- 10
      plotf <- function(panel) 
         {   
              with(panel, {
         mu   <- as.numeric(panel$mu)
      sigma   <- as.numeric(panel$sigma)
         nu   <- as.numeric(panel$nu)
         N    <- as.numeric(panel$N)
         xgrid <- seq(0, N, step=1, na.rm = TRUE)
         dgrid <- dZABB(xgrid, mu=mu, sigma=sigma, nu=nu, bd=N)
         plot(xgrid, dgrid, type = "h", col = "blue", lwd = 3, ylab="ZABB dist.", xlab="x")
                          })
      panel
         }
            }
   bipanel <- rp.control("ZABB distribution", N=10, mu=.5, sigma=1, nu=.1)
   rp.slider(bipanel, var = mu, from=0.001, to=.999, resolution=0.001,  action =  plotf, title="mu",  showvalue = T) 
   rp.slider(bipanel, var = sigma, from=0.01, to=3, resolution=0.001,  action =  plotf, title="sigma",  showvalue = T)
   rp.slider(bipanel, var = nu, from=0.001, to=.999, resolution=0.001,  action =  plotf, title="nu",  showvalue = T)    
   rp.textentry(bipanel, N, plotf, labels = c("N"), initval = c(10))
   rp.do(bipanel, plotf)
        #rp.textentry(tables.panel, xobs, tables.redraw, "Observed value",
         #           pos = c(140, 75, 160, 25))
}

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# ZIBB
demo.ZIBB <- function()
{if (interactive())     
      {   
        mu <- 0.5 ; sigma <- 0.5 ; nu <-.1 ; N <- 10
      plotf <- function(panel) 
         {   
              with(panel, {
         mu   <- as.numeric(panel$mu)
      sigma   <- as.numeric(panel$sigma)
         nu   <- as.numeric(panel$nu)
         N    <- as.numeric(panel$N)
         xgrid <- seq(0, N, step=1, na.rm = TRUE)
         dgrid <- dZIBB(xgrid, mu=mu, sigma=sigma, nu=nu, bd=N)
         plot(xgrid, dgrid, type = "h", col = "blue", lwd = 3, ylab="ZIBB dist.", xlab="x")
                          })
      panel
         }
            }
   bipanel <- rp.control("ZIBB distribution", N=10, mu=.5, sigma=1, nu=.1)
   rp.slider(bipanel, var = mu, from=0.001, to=.999, resolution=0.001,  action =  plotf, title="mu",  showvalue = T) 
   rp.slider(bipanel, var = sigma, from=0.01, to=3, resolution=0.001,  action =  plotf, title="sigma",  showvalue = T)
   rp.slider(bipanel, var = nu, from=0.001, to=.999, resolution=0.001,  action =  plotf, title="nu",  showvalue = T)    
   rp.textentry(bipanel, N, plotf, labels = c("N"), initval = c(10))
   rp.do(bipanel, plotf)
        #rp.textentry(tables.panel, xobs, tables.redraw, "Observed value",
         #           pos = c(140, 75, 160, 25))
}

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
