/*******************************************************************************
 * Copyright (c) 2024, Christopher Hill <ch6574@gmail.com>
 * GNU General Public License v3.0+ (see
 * https://www.gnu.org/licenses/gpl-3.0.txt)
 * SPDX-License-Identifier: GPL-3.0-or-later
 ******************************************************************************/

#ifndef _BIGNUM_INCLUDE_BIG_UINT_HPP_
#define _BIGNUM_INCLUDE_BIG_UINT_HPP_

#include <functional>
#include <iostream>
#include <string>
#include <vector>

namespace BigNum {

struct DivMod;

/**
 *  @brief A (toy) unsigned integer class with unlimited variable magnitude.
 *
 * N.B. Will throw std::invalid_argument on anomalies (e.g. divide by zero).
 */
class BigUInt {
 private:
  /**
   * Internally this class simulates an arbitrary wide binary number, using a
   * vector of uint32_t for storage called "data". Bits 0-31 (the first "word")
   * are stored in data[0], bits 31-63 (the second "word") are stored in data[1]
   * and so on...
   *
   * We use 32bit "words" as that makes later mathematical calculations fit
   * within 64 bits, and thus easy to parse out the result plus any "carry" bits
   * without worrying about overflow.
   *
   * Note this is unsigned, so we only model magnitude and get to ignore the
   * sign and associated complexity.
   */

  using WORD = std::uint32_t;
  using CALC = std::uint64_t;
  static const std::size_t WORD_BITS{32};
  static const WORD WORD_MASK{0xFFFFFFFF};

  // Extendable bit binary representation, in 32bit chunks
  std::vector<WORD> data;

  // Safe setter/getter of underlying "words" in data
  WORD get_word(std::size_t offset) const;
  void set_word(std::size_t offset, WORD data);

  // Utilities
  BigUInt &trim();
  void map(const BigUInt &other, std::function<WORD(WORD, WORD)> func);
  static BigUInt map(const BigUInt &lhs, const BigUInt &rhs,
                     std::function<WORD(WORD, WORD)> func);

 public:
  /**
   * @brief Creates a BigUIint equal to zero.
   */
  BigUInt();

  /**
   * @brief Creates a BigUIint equal to supplied data.
   * @param data An unsigned integer
   */
  BigUInt(std::uint32_t data);

  /**
   * @brief Creates a BigUIint equal to supplied data.
   * @param data An unsigned integer
   */
  BigUInt(std::uint64_t data);

  /**
   * @brief Creates a BigUIint equal to supplied string.
   * @param num A string, either decimal "1234"  or hex "0x1234"
   */
  explicit BigUInt(std::string_view num);

  // Comparison operators. The others are provided by default (!=, <, <=, >=, >)
  std::strong_ordering operator<=>(const BigUInt &other) const;
  bool operator==(const BigUInt &other) const;

  // Assignment operators
  BigUInt &operator+=(const BigUInt &other);
  BigUInt &operator-=(const BigUInt &other);
  BigUInt &operator*=(const BigUInt &other);
  BigUInt &operator/=(const BigUInt &other);
  BigUInt &operator%=(const BigUInt &other);
  BigUInt &operator&=(const BigUInt &other);
  BigUInt &operator|=(const BigUInt &other);
  BigUInt &operator^=(const BigUInt &other);
  BigUInt &operator<<=(std::size_t bits);  // will not overflow
  BigUInt &operator>>=(std::size_t bits);  // will not overflow

  // Increment operators
  BigUInt &operator++();
  BigUInt operator++(int);
  BigUInt &operator--();
  BigUInt operator--(int);

  // Arithmetic operators
  friend BigUInt operator+(const BigUInt &lhs, const BigUInt &rhs);
  friend BigUInt operator-(const BigUInt &lhs, const BigUInt &rhs);
  friend BigUInt operator*(const BigUInt &lhs, const BigUInt &rhs);
  friend BigUInt operator/(const BigUInt &lhs, const BigUInt &rhs);
  friend BigUInt operator%(const BigUInt &lhs, const BigUInt &rhs);
  BigUInt &operator~();
  friend BigUInt operator&(const BigUInt &lhs, const BigUInt &rhs);
  friend BigUInt operator|(const BigUInt &lhs, const BigUInt &rhs);
  friend BigUInt operator^(const BigUInt &lhs, const BigUInt &rhs);
  friend BigUInt operator<<(const BigUInt &lhs, std::size_t bits);
  friend BigUInt operator>>(const BigUInt &lhs, std::size_t bits);

  /**
   * @brief Similar to Python's "divmod", division that returns quotient and
   * remainder together.
   */
  static DivMod div(const BigUInt &dividend, const BigUInt &divisor);

  /**
   * @brief Raises base to the power of exp.
   */
  static BigUInt pow(const BigUInt &base, const BigUInt &exp);

  /**
   * @brief Raises base to the power of exp, modulo mod.
   */
  static BigUInt pow(const BigUInt &base, const BigUInt &exp,
                     const BigUInt &mod);

  /**
   * @brief The integer square root, where 1 <= result <= actual root.
   */
  static BigUInt isqrt(const BigUInt &num);

  /**
   * @brief The floor log2 of num.
   *
   * One can approximate logN(M) via log2(M)/log2(N).
   */
  static BigUInt log2(const BigUInt &num);

  /**
   * @brief The size of this integer in bits.
   */
  std::size_t bitsize() const;

  //
  // String representations
  //

  // Stream out as hex
  friend std::ostream &operator<<(std::ostream &os, const BigUInt &bn);
  // Binary representation
  std::string to_binary() const;
  // Decimal representation
  std::string to_decimal() const;

  //
  // Fixed width representations (will truncate at UINT_MAX equivalent)
  //
  std::uint32_t to_unint32_t() const;
  std::uint64_t to_unint64_t() const;

  // Useful constants
  static const BigUInt ZERO;
  static const BigUInt ONE;
  static const BigUInt TWO;
};

struct DivMod {
  BigUInt quot;
  BigUInt rem;
};

}  // namespace BigNum

#endif  // _BIGNUM_INCLUDE_BIG_UINT_HPP_
