t <- table(df$depart_admin2_nom, df$destination_admin2_name)[-1, -1]

vec <- sort(t[1,], decreasing = T)[1:10]

p <- plot_ly(
  x = factor(names(vec), levels=names(vec)),
  y = as.numeric(vec),
  name = "Number of records over time",
  type = "bar"
)

p


#--------------------------------------------------------------------------------------------#

t <- prop.table(table(df$depart_admin2_nom, df$destination_admin2_name)[-1, -1], margin=1)
e <- attributes(t)$dimnames[[2]]
nt <- table(df$depart_admin2_nom, df$destination_admin2_name)[-1, -1]
  
  
pdf <- NULL

for (depart.point in 1:10){
  
  vec <- t[depart.point,]
  
  
  #a <- attributes(t)$dimnames[[1]]
  n <- nt[depart.point,]
  x <- names(vec)
  y <- as.numeric(vec)
  depart.point2 <- e[depart.point]
  df2 <- data.frame(x, y, n, depart.point2)
  pdf <- rbind(pdf,df2)
}

x.title <- list(
  title = ""
)
y.title <- list(
  title = "Proportion of migrants"
)


# now plot it with the animation control "frame" set to "p"

#row.vars <- attributes(t)$dimnames[[2]]

plt <- plot_ly(pdf, x = ~x, y = ~y, frame=~depart.point2, type = 'bar',
               hoverinfo = 'text', text = ~paste(n))  %>%
  layout(xaxis = x.title, yaxis = y.title)

htmlwidgets::saveWidget(as_widget(plt), "Migration Widget.html")


# https://stackoverflow.com/questions/34580095/using-r-and-plot-ly-how-do-i-script-saving-my-output-as-a-webpage
# https://stackoverflow.com/questions/43787027/creating-an-animated-line-graph-using-plotly-package


