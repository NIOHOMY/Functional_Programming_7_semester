# Functional_Programming_7_semester

Установка интерпретатора ghci (GHCup)

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

Установка инструмента для сборки Haskell
- Cabal Нужен для структурирования ваших проектов Haskell, их сборки, запуска, определения зависимостей... .
- Stack Альтернатива Cabal.

```powershell
ghcup tui
```

Просмотр версии компилятора

```powershell
ghc --version
```

Запуск интерпретатора (По-умолчанию)
```powershell
C:\ghcup\bin\ghci.exe
```
