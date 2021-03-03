panel.cor <- function(x, y, digits=3, prefix="", cex.cor)
{

  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = cor(x,y)
  ccc = as.numeric(epi.ccc(x,y)$rho.c["est"])
  txt1 = format(r, digits = 3)
  txt2 = format(ccc, digits = 3)
  points(c(0,1), c(0,1), pch = -1)
  cex = min(1.5, .8/strwidth(txt1))
  text(.4, .8, txt1, cex =cex)
  text(.4, .4, txt2, cex=cex, col=2)

}

panel.lm <- function(x, y)
{
  usr <- par("usr"); on.exit(par(usr))
  points(x,y)
  abline(lm(y~x))
  abline(0,1, col = 2)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
