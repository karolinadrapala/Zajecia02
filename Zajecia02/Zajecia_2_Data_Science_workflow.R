# 1. Import danych ----
kraje_1 = read.table("kraje_makro_1.csv", header=TRUE, sep=",", dec=".")
kraje_2 = read.table("kraje_makro_2.csv", header=TRUE, sep=",", dec=".")

# 2. Przygotowanie danych ----
# Pierwsze/ostatnie wiersze
head(kraje_1)	# pierwsze 6 wierszy (obserwacji)
head(kraje_2)      

head(kraje_1, 10)	# pierwsze 10 wierszy (obserwacji)
head(kraje_2, 10)

tail(kraje_1, 5)	# ostatnie 5 wierszy (obserwacji)
tail(kraje_2, 5)


# Podstawowe statystyki wszystkich kolumn (zmiennych)
summary(kraje_1)	# min, max, ?rednia, mediana, kwantyle
summary(kraje_2)

# Statystyki pojedynczej kolumny (zmiennej)
mean(kraje_1$Przyrost_populacji)		# ?rednia
median(kraje_1$Przyrost_populacji)	# mediana
min(kraje_1$Przyrost_populacji)		# minimum
max(kraje_1$Przyrost_populacji)		# maksimum


# Porz?dkowanie nazw kolumn (zmiennych) ----

# Usuwanie zb?dnej kolumny
kraje_1$X = NULL
kraje_2$X = NULL

# Zmiana nazw kolumn z angielskich na polskie
colnames(kraje_2) = c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")


# Porz?dkowanie typ?w danych ----

# W ramce danych kraje_2 sprawd? typ zmiennej Region 
is.numeric(kraje_2$Region) 	# czy zmienna jest liczbowa? Odp. Nie.
is.character(kraje_2$Region) 	# czy zmienna jest tekstowa? Odp. Tak.

# Region to zmienna kategorialna, wi?c nadajemy jej typ factor:
kraje_2$Region = as.factor(kraje_2$Region)

# Sprawdzenie kategorii:
summary(kraje_2)
levels(kraje_2$Region)

# 3. Łączenie (scalanie) ramek danych w jedną ----

# Teraz wida?, ?e jest 7 kategorii region?w, na kt?rych operuje zmienna Region.


# Porz?dkowanie brak?w danych ----

# Szybka kontrola brak?w danych we wszystkich kolumnach:
colSums(is.na(kraje_1))	# nie ma brak?w danych
colSums(is.na(kraje_2))	# s? 4 braki danych w kolumnie (zmiennej) Internet_proc.

# Liczba brak?w w konkretnej kolumnie:
sum(is.na(kraje_2$Internet_proc.)) 	# 4 braki


# Zobaczmy te 4 wiersze, w kt?rych brakuje warto?ci:
kraje_2[is.na(kraje_2$Internet_proc.), ]


# Braki danych s? cz??ci? rzeczywisto?ci ekonomisty, dlatego trzeba umie? je obs?u?y? i # podj?? decyzj? analityczn?:
# OPCJA 1 - Pozostawi? (teraz tak post?pimy)
# OPCJA 2 - Usun?? obserwacje z brakami (czy usuni?cie tych obserwacji zmieni analiz??)
# OPCJA 3 - Uzupe?ni? braki (np. imputacja median?)


# Czyszczenie danych ----
# W ramce danych kraje_2, w kolumnie Region s? kategorie, w kt?rych nazwie jest znak &:
levels(kraje_2$Region)
# [1] "East Asia & Pacific"       "Europe & Central Asia"    
# [3] "Latin America & Caribbean" "Middle East & North Africa"
# [5] "North America"             "South Asia"               
# [7] "Sub-Saharan Africa"

# Znak & bywa problematyczny przy dalszym przetwarzaniu, dlatego zast?p go s?ownym sp?jnikiem "and".
# Funkcja gsub() dzia?a jak "Znajd? i zamie?" (Ctrl+H) w Excelu. 
# Zamienia wszystkie wyst?pienia tekstu na inny tekst
# Przyk?adowo: gsub("stary_tekst", "nowy_tekst", ramka$kolumna)

# W naszym przypadku wykonamy nast?puj?cy kod:
kraje_2$Region <- gsub("&", "and", kraje_2$Region)

# Sprawdzenie (po zamianie ponownie ustawiamy typ factor):
kraje_2$Region = as.factor(kraje_2$Region)
levels(kraje_2$Region)

# Przyk?adowo: merge(ramka1, ramka2, by.x="kolumna1", by.y="kolumna2")

#   ??czenie (scalanie) ramek danych kraje_1 i kraje_2
kraje = merge(kraje_1, kraje_2, by.x="Kod", by.y="Kod_kraju")


# Usuwanie zb?dnej kolumny po po??czeniu
kraje$Nazwa = NULL


# Zobacz ramk? danych po scaleniu
summary(kraje)
str(kraje)

# 4. Podstawowa analiza danych ----
# Najcz??ciej u?ywane funkcje pakietu dplyr: ----
# mutate() - tworzenie nowych zmiennych na bazie istniej?cych
# filter() ? wybieranie wierszy spe?niaj?cych okre?lone warunki
# select() ? wybieranie kolumn
# arrange() - sortowanie
# group_by() - grupowanie
# summarise() ? obliczanie warto?ci zagregowanych (np. ?rednich, sum)


# mutate() ? tworzenie nowych zmiennych na bazie istniej?cych ----

library(dplyr)


# Tworzenie nowej zmiennej Populacja_w_mln w dplyr:
kraje = kraje %>%
  mutate(Populacja_mln = Populacja / 1e6)



# 1e6 to zapis miliona w R (1 razy 10 do pot?gi 6)
# 1e9  = 1 000 000 000 (miliard)
# 1e12 = 1 000 000 000 000 (bilion)


# Tworzenie nowej zmiennej PKB_per_capita w dplyr:
kraje = kraje %>%
  mutate(PKB_per_capita = PKB / Populacja)



# filter() ? wybieranie wierszy ----
# select() ? wybieranie kolumn ----

# Wy?wietl kraje, w kt?rych % poziom urbanizacji jest wi?kszy ni? 50
kraje %>%
  filter(Urbanizacja_proc. > 50)


# Wy?wietl tylko dane pokazuj?ce zmienne Panstwo, Region, PKB, Populacja_mln
kraje %>%
  select(Panstwo, Region, PKB, Populacja_mln)


# arrange() ? sortowanie ----

# Posortuj kraje wed?ug przyrostu populacji rosn?co
kraje %>%
  arrange(Przyrost_populacji)


# Posortuj kraje wed?ug przyrostu populacji malej?co
kraje %>%
  arrange(desc(Przyrost_populacji))



# Wybierz kraje z PKB wi?kszym ni? 1 bilion, posortuj je rosn?co wzgl?dem PKB 
# i wy?wietl nazw? pa?stwa, PKB i PKB per capita. Ile jest takich kraj?w?
kraje %>%
  filter(PKB > 1e12) %>%
  arrange(PKB) %>%
  select(Panstwo, PKB, PKB_per_capita)





# Wybierz kraje z regionu Afryki Subsaharyjskiej, 
# wybierz zmienne Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja,
# a nast?pnie posortuj malej?co po PKB per capita
kraje %>%
  filter(Region == "Sub-Saharan Africa") %>%
  select(Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja_proc.) %>%
  arrange(desc(PKB_per_capita))





# Wy?wietl tylko te kraje, kt?re s? bogatsze ni? ?rednia regionu
bogate = kraje %>%
  group_by(Region) %>%
  filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE))





# ave() liczy ?redni? wewn?trz grup i zwraca wektor tej samej d?ugo?ci co dane.



# Znajd? najwi?ksz? warto?? PKB per capita w ca?ym zbiorze kraj?w
kraje %>%
  summarise(max_PKB_per_capita = max(PKB_per_capita, na.rm = TRUE))






# Znajd? najwi?ksz? i najmniejsz? warto?? Populacji w mln w ca?ym zbiorze kraj?w
kraje %>%
  summarise(
    min_populacja = min(Populacja_mln, na.rm = TRUE),
    max_populacja = max(Populacja_mln, na.rm = TRUE))




# Oblicz ?redni? populacj? w ca?ym zbiorze kraj?w (jedna liczba dla ca?ej ramki)
kraje %>%
  summarise(srednia_populacja = mean(Populacja_mln, na.rm = TRUE))



# Ile kraj?w jest w ca?ym zbiorze danych?
kraje %>%
  summarise(liczba_krajow = n())


# R?wnowa?ny kod w base R:

nrow(kraje)



# Policz, ile kraj?w jest w ka?dym regionie
kraje %>%
  group_by(Region) %>%
  summarise(liczba_krajow = n())






# Dla ka?dego regionu ?wiata: oblicz liczb? kraj?w (n), ?redni % dost?p do internetu i ?redni % poziom urbanizacji, a nast?pnie posortuj regiony malej?co wg ?redniego % dost?pu do internetu
kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))




# 5. Wizualizacja ----

# Wizualizacja danych tak?e pozwala zidentyfikowa? wzorce i zale?no?ci w zbiorze danych.

# install.packages("ggplot2")
library(ggplot2)



# 1. Prosty wykres punktowy: urbanizacja a PKB per capita ----
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita)) +
  geom_point() +
  labs(
    title = "Urbanizacja a PKB per capita",
    x = "Urbanizacja (%)",
    y = "PKB per capita")



# 2. Zaawansowany wykres punktowy: urbanizacja a PKB per capita ----
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Urbanizacja a PKB per capita",
    subtitle = "Czy bardziej zurbanizowane kraje s? bogatsze?",
    x = "Urbanizacja (% ludno?ci miejskiej)",
    y = "PKB per capita (USD, skala log)",
    color = "Region ?wiata"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom")



# 3. Zaawansowany wykres punktowy: rozmiar gospodarki a populacja ----

ggplot(kraje, aes(x = Populacja_mln, y = PKB, size = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Skala gospodarki i demografia",
    x = "Populacja (mln, log10)",
    y = "PKB (USD, log10)",
    size = "PKB per capita"
  ) +
  theme_minimal()



# 4. Prosty wykres s?upkowy: liczba kraj?w w regionach ----
ggplot(kraje, aes(x = Region)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(
    title = "Liczba kraj?w w regionach ?wiata",
    x = "Region",
    y = "Liczba kraj?w"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5))


# 5. Zaawansowany wykres s?upkowy poziomy: TOP 15 najbogatszych kraj?w ----
kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita), y = PKB_per_capita, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TOP 15 najbogatszych kraj?w ?wiata (2016)",
    subtitle = "PKB per capita w USD",
    x = NULL,
    y = "PKB per capita (USD)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10))


# 6. Wykres pude?kowy (boxplot): dost?p do internetu wed?ug region?w ----
ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median), 
                  y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Dost?p do internetu wed?ug region?w ?wiata",
    subtitle = "(punkty to poszczeg?lne kraje)",
    x = NULL,
    y = "Dost?p do internetu (% populacji)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none")
# 7. Wykres pude?kowy (boxplot): przyrost populacji wed?ug region?w ----
# (mediana, rozrzut i obserwacje odstaj?ce)
ggplot(kraje, aes(x = Region, y = Przyrost_populacji)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Tempo przyrostu populacji w regionach ?wiata",
    subtitle = "(punkty to poszczeg?lne kraje, linia przerywana = 0%)",
    x = "Region",
    y = "Przyrost populacji (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14))

# 6. Eksport ----
# Zapisanie ramki danych do pliku CSV
write.csv(kraje, "kraje_analiza.csv") 



# Zapisanie ramki danych do pliku Excel wymaga pakietu writexl:
install.packages("writexl")
library(writexl)

write_xlsx(kraje, "kraje_wynik.xlsx")





# Zapisz wszystkie wykresy ? prawe dolne okno, zak?adka Plots:
# Export -> Save as image

# Niestety ka?dy wykres trzeba zapisa? r?cznie
# nie ma funkcji do masowego eksportu wykres?w.





