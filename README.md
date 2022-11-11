### KOMPILACJA
W przypadku budowania kompilatora poza maszyną students należy przed wywołaniem `make` ustawić pod zmienną środowiskową BNFC ścieżkę do bnfc. Po zbudowaniu kompilatora poleceniem `make` w korzeniu znajdują się pliki wykonywalne `insc_jvm` oraz `insc_llvm`.

### URUCHAMIANIE
- `insc_jvm foo/bar/baz.ins` dla poprawnego programu wejściowego `baz.ins` tworzy pliki `baz.j` (kod Jasmin) oraz `baz.class` w katalogu `foo/bar`
- `insc_llvm foo/bar/baz.ins` dla poprawnego programu wejściowego `baz.ins` tworzy pliki `baz.ll` (tekstowy kod LLVM) oraz `baz.bc` (bitkod LLVM wykonywalny przy użyciu lli) w katalogu `foo/bar`

### STRUKTURA KATALOGU
- **`examples`**  - katalog zawierający programy przykładowe i ich oczekiwane wyjście
- **`src/JVMCompiler.hs`** - moduł generujący kod Jasmin dla danego drzewa składni
- **`src/JVMMain.hs`** - moduł obsługujący argumenty wiersza poleceń i generujący pliki `.j` oraz `.class` korzystając z modułu `JVMCompiler` oraz znajdującego się w katalogu `lib` pliku `jasmin.jar`
- **`src/LLVMCompiler.hs`** - moduł generujący tekstowy kod LLVM dla danego drzewa składni
- **`src/JVMMain.hs`** - moduł obsługujący argumenty wiersza poleceń i generujący pliki `.ll` oraz `.bt` korzystając z modułu `LLVMCompiler` i  polecenia `llvm-as`
- **`test_jvm.sh`** - skrypt uruchamiający interpreter do JVM na przykładowych programach i porównujący wyjście z odpowiednim plikiem `.output`
- **`test_llvm.sh`** - skrypt uruchamiający interpreter do LLVM na przykładowych programach i porównujący wyjście z odpowiednim plikiem `.output`
- **`lib/jasmin.jar`**
- **`Makefile`**

### TESTOWANIE
```
./test_jvm.sh insc_jvm examples
./test_llvm.sh insc_llvm examples
```
