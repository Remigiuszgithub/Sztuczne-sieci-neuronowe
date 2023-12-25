# Zakładamy, że chcemy przewidzieć, czy czas oczekiwania jest dłuższy niż średnia

# Instalacja pakietu AMORE (odkomentuj linię poniżej, jeśli nie jest zainstalowany)
# install.packages("AMORE")

library(AMORE)

# Załaduj dane
data(faithful)

# Określenie funkcji target
target <- function(x) {
  ifelse(x$waiting > mean(faithful$waiting), 1, 0)
}

# Zastosowanie funkcji target dla kategorii "waiting"
wZadane <- target(faithful)

# Ustawienie ziarna dla powtarzalności wyników
set.seed(888)

# Podział danych na dane trenujące i testowe (2/3 - 1/3)
ile <- nrow(faithful)
idxTren <- sample(1:ile, 2 * ile / 3)
idxTest <- setdiff(1:ile, idxTren)

# Tworzymy strukturę sieci
siec <- newff(n.neurons = c(2, 5, 1),
              learning.rate.global = 0.05,
              momentum.global = 0.5,
              hidden.layer = "sigmoid",
              output.layer = "purelin",
              method = "ADAPTgdwm",
              error.criterium = "LMS")

# Trenujemy sieć
wynik <- train(siec,
               faithful[idxTren, -2],  # Zakładając, że druga kolumna to waga
               wZadane[idxTren],
               error.criterium = "LMS",
               report = TRUE,
               show.step = 10,
               n.shows = 800)

# Wyświetlamy wartości błędów
plot(wynik$Merror, type = "l", xlab = "Iteracja (x10)",
     ylab = "Błąd", col = "darkred")

# Stosujemy wytrenowaną sieć do danych testowych
y <- sim(wynik$net, faithful[idxTest, -2])
y

# Definiujemy funkcję oceny klasyfikacji (zamieniamy liczby na etykietę)
testKlasyfikacji <- function(zad, wy) {
  zadane <- max.col(zad)
  rozpoznane <- max.col(wy)
  print(table(zadane, rozpoznane))
}

# Wyniki klasyfikacji
wynik <- testKlasyfikacji(wZadane[idxTest], y)

# Określamy dokładność klasyfikacji
dokladnosc <- sum(ifelse(y > 0.5, 1, 0) == wZadane[idxTest]) / length(wZadane[idxTest]) * 100

cat("Dokładność klasyfikacji:", dokladnosc, "%\n")
