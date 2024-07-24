# Synopsis
A (toy) unsigned integer class with unlimited variable magnitude.

# Example
## Checkout and build
N.B. Requires C++20 compiler, plus libfmt-dev and catch2 installed.

```bash
git clone https://github.com/ch6574/BigNum.git && cd BigNum
cmake -S . -B build
cmake --build build
cmake --build build -t test
```

## Simple usage
```c++
#include "big_uint.hpp"
using BigNum::BigUInt;

int main() {
  // Construct via a hex string
  auto bn = BigUInt("0x1234'1234'1234'1234'1234'1234'1234'1234");

  // Divide by a constant
  auto res = BigUInt::div(bn, 3u);
  std::cout << bn << " / 3 is ... " << "quot: " << res.quot
            << " rem: " << res.rem << std::endl;
}
```

This will show `0x12341234123412341234123412341234 / 3 is ... quot: 0x6115b66b0bc06115b66b0bc06115b66 rem: 0x2`

You can validate this against `python3 -c "q,r=(divmod(0x1234_1234_1234_1234_1234_1234_1234_1234, 3)); print(hex(q), hex(r))"`

## Calculating a large factorial
```c++
#include "big_uint.hpp"
using BigNum::BigUInt;

BigUInt factorial(unsigned n) {
  auto res = BigUInt(n);
  while (--n > 1) {
    res *= BigUInt(n);
  }
  return res;
}

int main() {
  auto res = factorial(100);
  std::cout << res << std::endl;
  std::cout << res.to_decimal() << std::endl;
}
```

This will show `0x1b30964ec395dc24069528d54bbda40d16e966ef9a70eb21b5b2943a321cdf10391745570cca9420c6ecb3b72ed2ee8b02ea2735c61a000000000000000000000000`
and `93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000`

You can validate this against `python3 -c "import math; print(hex(math.factorial(100)))"` for example.

## RSA style encryption
```c++
#include "big_uint.hpp"
using BigNum::BigUInt;

int main() {
  const BigUInt m{"0xDEADBEEF"};    // message

  const BigUInt n{5551201688147u};  // public key (mod)
  const BigUInt e{65537u};          // public key (exp)
  const BigUInt d{109182490673u};   // private key

  auto c = BigUInt::pow(m, e, n);   // m ** e % n
  std::cout << "Encrypting " << m << " --> " << c << std::endl;

  auto mm = BigUInt::pow(c, d, n);  // c ** d % n
  std::cout << "Decrypting " << c << " --> " << mm << std::endl;
}
```

This will show `0xdeadbeef` encrypts to `0x3636679bea3` and vice versa.


# License
GPL v3.
