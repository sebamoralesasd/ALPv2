# Propuesta TP final

## 1- Introducción
hledger[^1] es una aplicación de consola diseñada para registrar movimientos de dinero u otras commodities utilizando archivos de texto plano y aplicando el sistema de contabilidad de doble entrada[^2]. La aplicación lee el archivo de texto y muestra en consola distintos tipos de reportes útiles, tales como el estado de resultados (income statement) o el balance[^3]. Permite además llevar registro de varias cuentas de distintos tipos (activo, pasivo, ingreso, capital o patrimonio).

[^1]: https://hledger.org/
[^2]: https://plaintextaccounting.org/
[^3]: https://hledger.org/1.40/hledger.html#standard-report-commands

Este es un ejemplo de un archivo de texto con movimientos:

```
2024-08-30 Ingreso dinero
    assets:ars      ARS100000
    income:salario

2024-09-02 Fútbol 5
    expenses:futbol  ARS4300
    assets:ars

2024-09-03 Supermercado
    expenses:supermercado   ARS8529.00
    liabilities:tarjetacredito:santander

2024-09-04 YTPremium 
    expenses:ytpremium  ARS5881.41 
    assets:ars
```

## 2- Propuesta
Implementar un intérprete de un lenguaje inspirado en hledger, que registre los flujos de entrada y salida de una sola cuenta y cuente con un subconjunto de operaciones de este.
