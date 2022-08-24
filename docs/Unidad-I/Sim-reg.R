x <- rnorm(100, mean = 20, sd = 5)

alpha <- rnorm(100, mean = 0, sd = 0.5)
beta <- rnorm(100, mean = 0.1, sd = 0.01)

y <- alpha +  beta * x

write.csv(data.frame(x = x, y = y),
          "Unidad-I/Base-ejemplo-reg.csv",
          row.names = F)
