/*******************************************************************************
 * Copyright (c) 2024, Christopher Hill <ch6574@gmail.com>
 * GNU General Public License v3.0+ (see
 * https://www.gnu.org/licenses/gpl-3.0.txt)
 * SPDX-License-Identifier: GPL-3.0-or-later
 ******************************************************************************/

#ifndef _BIGNUM_INCLUDE_BIG_INT_HPP_
#define _BIGNUM_INCLUDE_BIG_INT_HPP_

#include <functional>
#include <iostream>
#include <string>
#include <vector>

namespace BigNum {

struct DivMod;

/**
 *  @brief A (toy) integer class with unlimited variable magnitude.
 *
 * N.B. Will throw std::invalid_argument on anomalies (e.g. divide by zero).
 */
class BigInt {
 public:
  /**
   * @brief Creates a BigUIint equal to zero.
   */
  BigInt();

  /**
   * @brief Creates a BigIint equal to supplied data.
   * @param data An unsigned integer
   */
  BigInt(std::uint32_t data);

  /**
   * @brief Creates a BigIint equal to supplied data.
   * @param data A signed integer
   */
  BigInt(std::int32_t data);

  /**
   * @brief Creates a BigIint equal to supplied data.
   * @param data An unsigned integer
   */
  BigInt(std::uint64_t data);

  /**
   * @brief Creates a BigIint equal to supplied data.
   * @param data A signed integer
   */
  BigInt(std::int64_t data);

  /**
   * @brief Creates a BigIint equal to supplied string.
   * @param num A string, either decimal "1234"  or hex "0x1234"
   */
  explicit BigInt(std::string_view num);

  /**
   * @brief Creates a BigIint equal to supplied data, rounds towards zero.
   * @param num a single precision floating point
   */
  BigInt(float num);

  /**
   * @brief Creates a BigIint equal to supplied data, rounds towards zero.
   * @param num a double precision floating point
   */
  BigInt(double num);

  // Comparison operators. The others are provided by default (!=, <, <=, >=, >)
  std::strong_ordering operator<=>(const BigInt &other) const;
  bool operator==(const BigInt &other) const;

  // Assignment operators
  BigInt &operator+=(const BigInt &other);
  BigInt &operator-=(const BigInt &other);
  BigInt &operator*=(const BigInt &other);
  BigInt &operator/=(const BigInt &other);
  BigInt &operator%=(const BigInt &other);
  BigInt &operator&=(const BigInt &other);
  BigInt &operator|=(const BigInt &other);
  BigInt &operator^=(const BigInt &other);
  BigInt &operator<<=(std::size_t bits);  // Magnitude shift only
  BigInt &operator>>=(std::size_t bits);  // Magnitude shift only

  // Increment operators
  BigInt &operator++();
  BigInt operator++(int);
  BigInt &operator--();
  BigInt operator--(int);

  // Arithmetic operators
  friend BigInt operator+(const BigInt &lhs, const BigInt &rhs);
  BigInt operator+() const;
  friend BigInt operator-(const BigInt &lhs, const BigInt &rhs);
  BigInt operator-() const;
  friend BigInt operator*(const BigInt &lhs, const BigInt &rhs);
  friend BigInt operator/(const BigInt &lhs, const BigInt &rhs);
  friend BigInt operator%(const BigInt &lhs, const BigInt &rhs);
  BigInt operator~() const;
  friend BigInt operator&(const BigInt &lhs, const BigInt &rhs);
  friend BigInt operator|(const BigInt &lhs, const BigInt &rhs);
  friend BigInt operator^(const BigInt &lhs, const BigInt &rhs);
  friend BigInt operator<<(const BigInt &lhs, std::size_t bits);
  friend BigInt operator>>(const BigInt &lhs, std::size_t bits);

  static BigInt abs(const BigInt &num);

  /**
   * @brief Similar to Python's "divmod", division that returns quotient and
   * remainder together. Different in that here we divide towards zero.
   */
  static DivMod div(const BigInt &dividend, const BigInt &divisor);

  /**
   * @brief Raises base to the power of exp.
   */
  static BigInt pow(const BigInt &base, const BigInt &exp);

  /**
   * @brief Raises base to the power of exp, modulo mod.
   */
  static BigInt pow(const BigInt &base, const BigInt &exp, const BigInt &mod);

  /**
   * @brief The integer square root, where 1 <= result <= actual root.
   *
   * (Positive numbers only.)
   */
  static BigInt isqrt(const BigInt &num);

  /**
   * @brief Returns the greatest common divisor of a and b, else zero.
   */
  static BigInt gcd(const BigInt &a, const BigInt &b);

  /**
   * @brief The floor log2 of num.
   *
   * One can approximate logN(M) via log2(M)/log2(N).

   * (Positive numbers only.)
   */
  static BigInt log2(const BigInt &num);

  /**
   * @brief The size of this integer in bits.
   */
  std::size_t bitsize() const;

  /**
   * @brief The count of 1 bits in this integer.
   */
  std::size_t popcount() const;

  //
  // String representations
  //

  // Stream out as hex
  friend std::ostream &operator<<(std::ostream &os, const BigInt &bn);
  // Binary representation
  std::string to_binary() const;
  // Decimal representation
  std::string to_decimal() const;

  //
  // Fixed width representations (will truncate at INT_MAX/MIN equivalents)
  //
  std::uint32_t to_uint32_t() const;
  std::int32_t to_int32_t() const;
  std::uint64_t to_uint64_t() const;
  std::int64_t to_int64_t() const;

  // Floating point representations (will truncate at FLT_MAX equivalents)
  float to_float() const;
  double to_double() const;

  //
  // Useful constants
  //
  static const BigInt NEG_TEN;
  static const BigInt NEG_TWO;
  static const BigInt NEG_ONE;
  static const BigInt ZERO;
  static const BigInt ONE;
  static const BigInt TWO;
  static const BigInt TEN;

 private:
  /*
   * Implementation is "sign and magnitude".
   *
   * Internally this class simulates an arbitrary wide binary number, using a
   * vector of uint32_t for storage called "magnitude". Bits 0-31 (the first
   * "word") are stored in magnitude[0], bits 31-63 (the second "word") are
   * stored in magnitude[1] and so on...
   *
   * We use 32bit "words" as that makes later mathematical calculations fit
   * within 64 bits, and thus easy to parse out the result plus any "carry" bits
   * without worrying about overflow.
   */

  using WORD = std::uint32_t;
  using CALC = std::uint64_t;
  static const std::size_t WORD_BITS{32};
  static const WORD WORD_MASK{0xFFFFFFFF};

  // Magnitude. Extendable bit binary representation, in 32bit chunks
  std::vector<WORD> magnitude;

  // Sign. True if negative
  bool negative;

  // Safe setter/getter of underlying "words" in data
  WORD get_word(std::size_t offset) const;
  void set_word(std::size_t offset, WORD data);

  // Utilities
  BigInt &normalize();
  void map(const BigInt &other, std::function<WORD(WORD, WORD)> func);
  static BigInt map(const BigInt &lhs, const BigInt &rhs,
                    std::function<WORD(WORD, WORD)> func);

  // Math implementations
  BigInt &add(const BigInt &other);
  BigInt &sub(const BigInt &other);
  BigInt &mul(const BigInt &other);
};

struct DivMod {
  BigInt quot;
  BigInt rem;
};

}  // namespace BigNum

#endif  // _BIGNUM_INCLUDE_BIG_INT_HPP_
