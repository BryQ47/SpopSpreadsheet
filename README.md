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
* :addrow - 
* :delrow - 
* :addcol - 
* :delcol - 
* :w - zapis do aktualnego pliku
* :w _plik_ - zapis do wskazanego pliku w katalogu _data_
* :o _plik_ - odczyt ze wskazanego pliku z katalogu _data_
* !_adres_ - wpisanie zawartości wskazanej komórki
* _adres_:_wartość_ - aktualizuje zawartość wskazanej komórki

### Adresy komórek

Adresy komórek są postaci _indeks wiersza_,_index kolumny_. Kolumny i wiersze numerowane są od jedynki. Przykład: __3,2__ to adres komówki w wierszu 3 i kolumnie 2. 

### Formuły

W komórce umieścić można formułę w odpowiedni sposób aktualizując jej zawartość.
Dostępne formuły to:
* =Sum(_zakres_) - suma wartości komórek z podanego zakresu
* =Sum(_zakres_) - iloczyn wartości komórek z podanego zakresu
* =Sum(_zakres_) - średnia wartości komórek z podanego zakresu

Zakres to ciąg adresów komórek oddzielonych znakiem ;. 
Przykładowa formuła sumy komórek od 1 do 3 z pierwszego wiersza, umieszczająca wynik w komórce czwartej to:
```
1,4=Sum(1,1;1,2;1,3)
```
