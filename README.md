# SPOP Spreadsheet

## Pakiety
* Data.List.Split: 
```
cabal install split
```
* Data.Matrix: 
```
cabal install matrix
```

## Polecenia
* :q - wyjście
* :n _wiersze_ _kolumny_ - tworzy nowy arkusz o podanej liczbie kolumn i wierszy
* :addrow - dodaj wiersz arkusza
* :delrow - usuń wiersz arkusza
* :addcol - dodaj kolumnę akrusza
* :delcol - usuń kolumnę arkusza
* :w - zapis do aktualnego pliku
* :w _plik_ - zapis do wskazanego pliku
* :o _plik_ - odczyt ze wskazanego pliku
* !_adres_ - wpisanie zawartości wskazanej komórki
* _adres_:_wartość_ - aktualizuje zawartość wskazanej komórki

### Adresy komórek

Adresy komórek są postaci _indeks wiersza_,_index kolumny_. Kolumny i wiersze numerowane są od jedynki. 
Przykład: __3,2__ to adres komówki w wierszu 3 i kolumnie 2. 

### Formuły

W komórce umieścić można formułę w odpowiedni sposób aktualizując jej zawartość.
Dostępne formuły to:
* =Sum(_zakres_) - suma wartości komórek z podanego zakresu
* =Mult(_zakres_) - iloczyn wartości komórek z podanego zakresu
* =Avg(_zakres_) - średnia wartości komórek z podanego zakresu

Zakres to ciąg adresów komórek oddzielonych znakiem ;.
W przpadku podania tylko 2 komórek w zakresie, które dodatkowo znajdują się w jednym wierszu lub kolumnie, do obliczeń włączone są także wszystkie komórki znajdujące się pomiędzy nimi.
Przykładowa formuła sumy komórek od 1 do 3 z pierwszego wiersza, umieszczająca wynik w komórce czwartej to:
```
1,4=Sum(1,1;1,2;1,3)
```
