/*******************************************************************************
 * Copyright (c) 2024, Christopher Hill <ch6574@gmail.com>
 * GNU General Public License v3.0+ (see
 *https://www.gnu.org/licenses/gpl-3.0.txt) SPDX-License-Identifier:
 *GPL-3.0-or-later
 ******************************************************************************/

#include "big_uint.hpp"

using BigNum::BigUInt;

//
// Simple usage
//
void simple_demo() {
  // Construct via a hex string
  auto bn = BigUInt("0x1234'1234'1234'1234'1234'1234'1234'1234");

  // Divide by a constant
  auto res = BigUInt::div(bn, 3u);
  std::cout << bn << " / 3 is ... " << "quot: " << res.quot
            << " rem: " << res.rem << std::endl;
}

//
// Calculating a large factorial
//
BigUInt factorial(unsigned n) {
  auto res = BigUInt(n);
  while (--n > 1) {
    res *= BigUInt(n);
  }
  return res;
}
void factorial_demo() {
  auto res = factorial(100);
  std::cout << res << std::endl;
  std::cout << res.to_decimal() << std::endl;
}

//
// RSA demo
//
void rsa_demo() {
  const BigUInt m{"0xDEADBEEF"};  // message

  const BigUInt n{5551201688147u};  // public key (mod)
  const BigUInt e{65537u};          // public key (exp)
  const BigUInt d{109182490673u};   // private key

  auto c = BigUInt::pow(m, e, n);  // m ** e % n
  std::cout << "Encrypting " << m << " --> " << c << std::endl;

  auto mm = BigUInt::pow(c, d, n);  // c ** d % n
  std::cout << "Decrypting " << c << " --> " << mm << std::endl;

  std::cout << ((m == mm) ? "Success" : "Failure") << std::endl;
}

//
// Main
//
int main() {
  try {
    simple_demo();
    std::cout << "----------------------------------------" << std::endl;
    factorial_demo();
    std::cout << "----------------------------------------" << std::endl;
    rsa_demo();
    std::cout << "----------------------------------------" << std::endl;
  } catch (const std::invalid_argument &ex) {
    std::cout << ex.what() << std::endl;
  }
}
