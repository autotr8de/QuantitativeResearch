blackplot = function (rows = 1, cols = 1, color = "white")
par(mar = c(2, 4, 1, 1), mfrow = c(rows, cols), bg = "black",
    fg = "white", col = color, col.axis = "white", col.lab = "white",
    col.main = "white", col.sub = "white", lwd = 2)