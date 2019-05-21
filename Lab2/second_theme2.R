# Тема 2
nList <- list(10, 40, 160);
varCount <- 20;
foo = rnorm
mList <- list();
# Генерация 3х матриц - 10x20, 40x20, 160x20 по нормальному закону распределения
for (i in 1:3){
  xs = foo(nList[[i]] * varCount);
  mList[[i]] <- matrix(xs, nList[[i]], varCount);
}
# Функции нахождения оценок
moments = function(n) { 
  return (function (x) (2 / n) * sum(x));
}
likelihood = function(n) {
  return (function (x) ((n + 1) / n) * max(x));
}
ordinal = function(n) {
  return (function (x) (2 * quantile(x, c(0.5))));
}

# Вичисление значения оценок
rezList1 <- list();
rezList2 <- list();
rezList3 <- list();
for (i in 1:3){
  n <- nList[[i]];
  rezList1[[i]] <- apply(mList[[i]], 1, moments(n = n));
  rezList2[[i]] <- apply(mList[[i]], 1, likelihood(n = n));
  rezList3[[i]] <- apply(mList[[i]], 1, ordinal(n = n));
}
