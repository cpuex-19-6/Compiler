# Compiler

## Setup

```
git clone https://github.com/cpuex-19-6/Compiler.git
cd Compiler
make
```

## Usage

例えば

```
./min-caml test/fib
```

で `test/` ディレクトリ内の `fib.ml` のアセンブラ `fib.s` とバイナリ `fib.bin` がそれぞれ出力されます。

レイトレをコンパイルしたい場合は、

```
make min-rt
```
で `raytrace.s` にアセンブラが出力されます。