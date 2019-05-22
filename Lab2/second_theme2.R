# Тема 2
# Функции нахождения оценок
moments = function(n) { 
  return (function (x) (2 / n) * sum(x));
}
likelihood = function(n) {
  return (function (x) ((n + 1) / n) * max(x))
}
ordinal = function(n) {
  return (function (x) (2 * quantile(x, c(0.5))))
}
nList <- list(10, 40, 160)
varCount <- 20
m <- 15 + 2*22
s <- 3 + 3*22
l <- 5 + 22
myNorm = function(n){
  return(rnorm(n, m, s))
}
myExp = function(n){
  return(rexp(n, l))
}
foo = myNorm
isNorm <- TRUE
print("Нормальный закон распределения")
for (i in 1:2){
  sdMatrix <- matrix(0, nrow = 3, ncol = 3)
  for (i in 1:3){
    n <- nList[[i]]
    print("-----------------------")
    str(n)
    # Генерация матрицы по установленному закону распределения
    xs = foo(n * varCount)
    M <- matrix(xs, varCount, n)
    # Вычисление значений оценок
    rez1 <- apply(M, 1, moments(n))
    rez2 <- apply(M, 1, likelihood(n))
    rez3 <- apply(M, 1, ordinal(n))
    print("Оценки:")
    str(rez1)
    str(rez2)
    str(rez3)
    # Построение графиков получившихся оценок
    plot ( rez1 , col = " red ")
    lines ( rez1 , col = " red")
    par ( new =T )
    plot ( rez2 , col = " blue ")
    lines ( rez2 , col = " blue ")
    par ( new =T )
    plot ( rez3 , col = " yellow ")
    lines ( rez3 , col = " yellow ")
    par ( new =F )
    print("Среднеквадратическое отклонение")
    p1 <- sd(rez1); str(p1)
    p2 <- sd(rez2); str(p2)
    p3 <- sd(rez3); str(p3)
    sdMatrix[i, 1] <- p1
    sdMatrix[i, 2] <- p2
    sdMatrix[i, 3] <- p3
    print("Максимальное значение")
    max1 <- max(rez1); str(max1)
    max2 <- max(rez2); str(max2)
    max3 <- max(rez3); str(max3)
    print("Минимальное значение")
    min1 <- min(rez1); str(min1)
    min2 <- min(rez2); str(min2)
    min3 <- min(rez3); str(min3)
    print("Величина размаха")
    r1 <- max1 - min1; str(r1)
    r2 <- max2 - min2; str(r2)
    r3 <- max3 - min3; str(r3)
  }
  # Построение графика отклонений
  vals10 <- c(sdMatrix[1, 1], sdMatrix[1,2], sdMatrix[1,3])
  vals40 <- c(sdMatrix[2, 1], sdMatrix[2,2], sdMatrix[2,3])
  vals160 <- c(sdMatrix[3, 1], sdMatrix[3,2], sdMatrix[3,3])
  plot ( vals10 , col = " red ")
  lines ( vals10 , col = " red")
  par ( new =T )
  plot ( vals40 , col = " blue ")
  lines ( vals40 , col = " blue ")
  par ( new =T )
  plot ( vals160 , col = " yellow ")
  lines ( vals160 , col = " yellow ")
  par ( new =F )
  # Смена на показательный закон распределения
  foo = myExp
  if (isNorm){
    print("Показательный закон распределения")
    isNorm <- FALSE
  }
}