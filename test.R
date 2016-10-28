# test
library('ggplot2')

curve(x, from=1, to=50, xlab="x", ylab="y")

print("1")

q()

print("2")


p <- ggplot() + geom_hline(aes(yintercept=50))
p
