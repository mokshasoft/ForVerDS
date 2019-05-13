# ForVerDS
Demonstrates how to test monadic C-code using [QuickCheck](https://en.wikipedia.org/wiki/QuickCheck) and property-based testing.
The same method can be used to test libraries in C++ and other languages.

## Environment
In order to run these tests at least the following is needed:
- A C compiler
- [Stack](https://haskellstack.org/)

## Building
Build the library:

```shell
mkdir build
cd build
cmake -G Ninja ..
ninja data-structures
```

## Testing
Test the library with QuickCheck:

```shell
cd test/quickcheck
stack test
```

Test the library in GHCi:
```shell
cd test/quickchek
stack ghci mempool-quickcheck:mempool-quickcheck-test
```

and then in the GHCi prompt
```shell
> runQuickCheckTests
+++ OK, passed 10000 tests.
```

## Valgrind
Combining QuickCheck with [Valgrind](http://www.valgrind.org/) is a powerful combination to find memory errors in C code.
```shell
valgrind --leak-check=full --trace-children=yes stack test
```
This test run shows a lot of memory errors, but none are related to the tested library.
