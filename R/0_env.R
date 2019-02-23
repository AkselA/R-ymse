ymseEnv <- new.env()

def.par <- list(xlog = FALSE, ylog = FALSE, adj = 0.5, ann =
TRUE, ask = FALSE, bg = "transparent", bty = "o", cex = 1,
cex.axis = 1, cex.lab = 1, cex.main = 1.2, cex.sub = 1, col =
"black", col.axis = "black", col.lab = "black", col.main =
"black", col.sub = "black", crt = 0, err = 0L, family = "", fg =
"black", fig = c(0, 1, 0, 1), fin = c(5.5, 5.5), font = 1L,
font.axis = 1L, font.lab = 1L, font.main = 2L, font.sub = 1L, lab
= c(5L, 5L, 7L), las = 0L, lend = "round", lheight = 1, ljoin =
"round", lmitre = 10, lty = "solid", lwd = 1, mai = c(1.02, 0.82,
0.82, 0.42), mar = c(5.1, 4.1, 4.1, 2.1), mex = 1, mfcol = c(1L,
1L), mfg = c(1L, 1L, 1L, 1L), mfrow = c(1L, 1L), mgp = c(3, 1,
0), mkh = 0.001, new = FALSE, oma = c(0, 0, 0, 0), omd = c(0, 1,
0, 1), omi = c(0, 0, 0, 0), pch = 1L, pin = c(4.26, 3.66), plt =
c(0.149090909090909, 0.923636363636364, 0.185454545454545,
0.850909090909091), ps = 12L, pty = "m", smo = 1, srt = 0, tck =
NA_real_, tcl = -0.5, usr = c(0, 1, 0, 1), xaxp = c(0, 1, 5),
xaxs = "r", xaxt = "s", xpd = FALSE, yaxp = c(0, 1, 5), yaxs =
"r", yaxt = "s", ylbias = 0.2)

old.par <- "not set"

assign("def.par", old.par, envir=ymseEnv)
assign("old.par", old.par, envir=ymseEnv)

# globalVariables(c("def.par", "old.par"))
