# Instalacja pakietu AMORE (odkomentuj linię poniżej, jeśli nie jest zainstalowany)
# install.packages("AMORE")

library(AMORE)

# Załaduj dane
data(mtcars)

# Określenie funkcji target
target <- function(x) {
  n <- length(x)
  values <- levels(x)
  l <- length(values)
  T <- matrix(0, nrow = n, ncol = l)
  for (i in 1:l)
    T[, i] <- (x == values[i])
  colnames(T) <- values
  return(T)
}

# Zastosowanie funkcji target dla kategorii "cyl"
wZadane <- target(as.factor(mtcars$cyl))

# Ustawienie ziarna dla powtarzalności wyników
set.seed(434)

# Podział danych na dane trenujące i testowe (2/3 - 1/3)
ile <- nrow(mtcars)
idxTren <- sample(1:ile, 2 * ile / 3)
idxTest <- setdiff(1:ile, idxTren)

# Tworzymy strukturę sieci
siec <- newff(n.neurons = c(1, 5, length(unique(mtcars$cyl))),
              learning.rate.global = 0.01,
              momentum.global = 0.3,
              hidden.layer = "sigmoid",
              output.layer = "purelin",
              method = "ADAPTgdwm",
              error.criterium = "LMS")

# Trenujemy sieć
wynik <- train(siec,
               mtcars[idxTren, "mpg"],
               wZadane[idxTren, ],
               error.criterium = "LMS",
               report = TRUE,
               show.step = 10,
               n.shows = 800)

# Wyświetlamy wartości błędów
plot(wynik$Merror, type = "l", xlab = "Iteracja (x10)",
     ylab = "Błąd", col = "darkred")

# Stosujemy wytrenowaną sieć do danych testowych
y <- sim(wynik$net, mtcars[idxTest, "mpg"])

# Wyniki klasyfikacji
wynik <- testKlasyfikacji(wZadane[idxTest, ], y)

# Określamy dokładność klasyfikacji
cat("Dokładność klasyfikacji:", sum(diag(wynik)) / sum(wynik) * 100, "%\n")
