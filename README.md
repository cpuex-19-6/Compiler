# Compiler

## Setup

```
git clone https://github.com/cpuex-19-6/Compiler.git
cd Compiler
make
```

## Usage

三角関数を用いない場合、例えば

```
./min-caml test/fib
```


で `test/` ディレクトリ内の `fib.ml` のアセンブラ `fib.s` が出力されます。

レイトレ以外で、三角関数を使うコード（例えば、`float-check.ml`）をコンパイルする場合は、
```
make float-check.s
```
で`test/` ディレクトリ内の `float-check.ml` のアセンブラ `float-check-2.s` が出力されます。
（コードは`test/` ディレクトリ内に置いてください。）


レイトレをコンパイルしたい場合は、

```
make min-rt
```
で `raytrace.s` にアセンブラが出力されます。