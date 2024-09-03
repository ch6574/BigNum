/*******************************************************************************
 * Copyright (c) 2024, Christopher Hill <ch6574@gmail.com>
 * GNU General Public License v3.0+ (see
 * https://www.gnu.org/licenses/gpl-3.0.txt)
 * SPDX-License-Identifier: GPL-3.0-or-later
 ******************************************************************************/

#include "big_int.hpp"

#include <assert.h>

#include <algorithm>
#include <bit>
#include <cmath>
#include <iostream>
#include <numeric>
#include <random>
#include <ranges>
#include <span>
#include <string>

namespace BigNum {

// Constants
const BigInt BigInt::NEG_TEN = BigInt(-10);
const BigInt BigInt::NEG_TWO = BigInt(-2);
const BigInt BigInt::NEG_ONE = BigInt(-1);
const BigInt BigInt::ZERO = BigInt();
const BigInt BigInt::ONE = BigInt(1u);
const BigInt BigInt::TWO = BigInt(2u);
const BigInt BigInt::TEN = BigInt(10u);

// Constructors
BigInt::BigInt() : magnitude(0), negative{false} {}

BigInt::BigInt(std::uint32_t data) : negative{false} {
  // Type must be equal to "WORD" for a direct copy to make sense
  static_assert(std::is_same<std::uint32_t, WORD>::value, "Type mismatch");
  magnitude = std::vector<WORD>{data};
  normalize();
}

BigInt::BigInt(std::int32_t data) {
  // C++20 mandates two's complement
  const auto mask = data >> 31;
  std::uint32_t m = (data + mask) ^ mask;
  magnitude = std::vector<WORD>{m};
  negative = mask & 1u;
  normalize();
}

BigInt::BigInt(std::uint64_t data) : negative{false} {
  std::uint32_t lower = data & WORD_MASK;
  std::uint32_t upper = data >> WORD_BITS;
  if (upper)
    magnitude = std::vector<WORD>{lower, upper};
  else
    magnitude = std::vector<WORD>{lower};
  normalize();
}

BigInt::BigInt(std::int64_t data) {
  // C++20 mandates two's complement
  const auto mask = data >> 63;
  std::uint64_t m = (data + mask) ^ mask;
  std::uint32_t lower = m & WORD_MASK;
  std::uint32_t upper = m >> WORD_BITS;
  if (upper)
    magnitude = std::vector<WORD>{lower, upper};
  else
    magnitude = std::vector<WORD>{lower};
  negative = mask & 1u;
  normalize();
}

/**
 * @brief Mainly string parsing logic. If the string starts with "0x" we parse
 * as hex, else we parse as decimal.
 *
 * Separators ' are allowed, so "0x1234'5678" is eqivalent to "0x12345678".
 */
BigInt::BigInt(std::string_view num) : negative{false} {
  if ((num.length() > 2 &&
       ((num.substr(0, 2) == "0x") || num.substr(0, 2) == "0X")) ||
      (num.length() > 3 &&
       ((num.substr(0, 3) == "-0x") || num.substr(0, 3) == "-0X"))) {
    //
    // hex strings "0x...". Remove prefix and any ' separators.
    //
    if (num[0] == '-') {
      negative = true;
      num.remove_prefix(3);
    } else {
      num.remove_prefix(2);
    }
    auto hex = std::string{num};
    hex.erase(std::remove(hex.begin(), hex.end(), '\''), hex.end());

    // Validate character set
    if (hex.find_first_not_of("0123456789abcdefABCDEF") != std::string::npos)
      throw std::invalid_argument("Invalid hex string: " + std::string{num});

    // Read string in chunks of 8 hex chars (32 bits), and sum into tmp
    auto tmp = BigInt::ZERO;
    std::ranges::for_each(
        std::string_view{hex} | std::views::chunk(8), [&](auto v) {
          WORD i{0};
          std::from_chars(v.data(), v.data() + v.size(), i, 16);
          tmp <<= v.size() * 4;  // each char is 4 bits
          tmp += i;
        });
    magnitude = std::move(tmp.magnitude);  // Take the tmp BigInt's data
  } else if (num.length() > 0) {
    //
    // Assume decimal. Remove any ' separators, then validate.
    //
    if (num[0] == '-') {
      negative = true;
      num.remove_prefix(1);
    }
    auto dec = std::string{num};
    dec.erase(std::remove(dec.begin(), dec.end(), '\''), dec.end());

    // Validate character set
    if (dec.find_first_not_of("0123456789") != std::string::npos)
      throw std::invalid_argument("Invalid decimal string: " +
                                  std::string{num});

    static unsigned pow10[] = {1,           10,           100,       1'000,
                               10'000,      100'000,      1'000'000, 10'000'000,
                               100'000'000, 1'000'000'000};
    // // Read string in chunks of 9 decimal chars, and sum into tmp
    auto tmp = BigInt::ZERO;
    std::ranges::for_each(
        std::string_view{dec} | std::views::chunk(9), [&](auto v) {
          WORD i{0};
          std::from_chars(v.data(), v.data() + v.size(), i, 10);
          tmp *= pow10[v.size()];
          tmp += i;
        });
    magnitude = std::move(tmp.magnitude);  // Take the tmp BigInt's data
  } else {
    throw std::invalid_argument("Invalid numeric string: " + std::string{num});
  }

  normalize();
}

BigInt::BigInt(std::float32_t num) {
  if (num < 1.0f && num > -1.0f) {
    *this = ZERO;
    return;
  }

  // IEEE 754
  auto tmp = reinterpret_cast<std::uint32_t &>(num);
  BigInt::negative = (tmp >> 31) & 0x1;      // bit 31 is sign
  auto exp = ((tmp >> 23) & 0xFF) - 127;     // bits 23-30 are exponent
  auto frac = (tmp & 0x7FFFFF) | (1 << 23);  // 0-22 are fraction, +1

  BigInt::magnitude.push_back(frac);
  if (bitsize() > exp) {
    *this >>= bitsize() - exp - 1;
  } else if (bitsize() <= exp) {
    *this <<= exp - bitsize() + 1;
  }

  normalize();
}

BigInt::BigInt(std::float64_t num) {
  if (num < 1.0 && num > -1.0) {
    *this = ZERO;
    return;
  }

  // IEEE 754
  auto tmp = reinterpret_cast<std::uint64_t &>(num);
  BigInt::negative = (tmp >> 63) & 0x1;               // bit 63 is sign
  auto exp = ((tmp >> 52) & 0x7FF) - 1023;            // bits 52-62 are exponent
  auto frac = (tmp & 0xFFFFFFFFFFFFF) | (1ul << 52);  // 0-51 are fraction, +1

  std::uint32_t lower = frac & WORD_MASK;
  std::uint32_t upper = frac >> WORD_BITS;
  if (upper)
    BigInt::magnitude = std::vector<WORD>{lower, upper};
  else
    BigInt::magnitude = std::vector<WORD>{lower};

  if (bitsize() > exp) {
    *this >>= bitsize() - exp - 1;
  } else if (bitsize() <= exp) {
    *this <<= exp - bitsize() + 1;
  }

  normalize();
}

/**
 * @brief A safe getter of a "word" at supplied offset.
 *
 * @param offset the offset
 * @return BigInt::WORD the word at that offset, else 0
 */
BigInt::WORD BigInt::get_word(std::size_t offset) const {
  if (magnitude.size() > offset) return magnitude[offset];
  return WORD{0};
}

/**
 * @brief A safe setter of a "word" at supplied offset. Underlying storage will
 * grow to accomodate offset if out of bounds.
 *
 * @param offset the offset
 * @param data the word to set
 */
void BigInt::set_word(std::size_t offset, WORD data) {
  if (magnitude.size() <= offset)
    magnitude.insert(magnitude.end(), offset - magnitude.size() + 1, 0);
  magnitude[offset] = data;
}

/**
 * @brief Discards any superfluous leading zeros
 */
inline BigInt &BigInt::normalize() {
  while (!magnitude.empty() && magnitude.back() == WORD{0})
    magnitude.pop_back();

  // Prevent negative zero
  if (magnitude.empty()) negative = false;
  return *this;
}

/**
 * @brief Mapping function. Applies the supplied func() to all "words" in
 * ourself and other.
 *
 * @param other the other number
 * @param func the supplied func to map over two WORDs
 */
void BigInt::map(const BigInt &other, std::function<WORD(WORD, WORD)> func) {
  auto max_size = std::max(magnitude.size(), other.magnitude.size());
  if (magnitude.size() < max_size) magnitude.resize(max_size);

  // If C++ gains "zip_longest" we could make this more concise.
  for (auto i = 0ul; i < max_size; ++i) {
    set_word(i, func(get_word(i), other.get_word(i)));
  }
  normalize();
}

/**
 * @brief Mapping function. Applies the supplied func() to all "words" in
 * supplied left and right sides.
 *
 * @param lhs the left hand side
 * @param rhs the right hand side
 * @param func the supplied func to map over two WORDs
 * @return BigInt. N.B. sign is copied from lhs to return value
 */
BigInt BigInt::map(const BigInt &lhs, const BigInt &rhs,
                   std::function<WORD(WORD, WORD)> func) {
  // Apply func() across all "words"
  auto max_size = std::max(lhs.magnitude.size(), rhs.magnitude.size());
  auto ret = ZERO;
  ret.magnitude.resize(max_size);

  // If C++ gains "zip_longest" we could make this more concise.
  for (auto i = 0ul; i < max_size; ++i) {
    ret.set_word(i, func(lhs.get_word(i), rhs.get_word(i)));
  }

  ret.negative = lhs.negative;
  return ret.normalize();
}

//
// Math implementations
//

/**
 * @brief Addition of magnitude. Algorithm is simple, add corresponding "words"
 * together at each increasing offset, and carry any overflow to the next
 * offset's sum.
 *
 * N.B. adding 3 lots of 32 bits (this + other + carry) will fit into 34 bits.
 *
 * @param other the quantity to add.
 * @return BigInt& this
 */
BigInt &BigInt::add(const BigInt &other) {
  // Short circuit simple cases
  if (other == ZERO) return *this;
  if (*this == ZERO) {
    *this = other;
    return *this;
  }

  std::size_t offset{0};
  WORD carry{0};
  for (auto &i : other.magnitude) {
    // All the math is 64bit
    CALC res =
        static_cast<CALC>(get_word(offset)) + static_cast<CALC>(i) + carry;
    set_word(offset, res & WORD_MASK);  // Lower 32 is result
    carry = res >> WORD_BITS;           // Upper 32 is the carry
    ++offset;
  }
  while (carry) {
    CALC res = static_cast<CALC>(get_word(offset)) + carry;
    set_word(offset, res & WORD_MASK);  // Lower 32 is result
    carry = res >> WORD_BITS;           // Upper 32 is the carry
    ++offset;
  }

  return *this;
}

/**
 * @brief Subtraction of magnitude. Algorithm is simple, subtract corresponding
 * "words" at each increasing offset, however we "borrow" 1 bit from the next
 * word to avoid handling underflow.
 *
 * N.B. we effectively subtract 33 bits (this) from 32 bits (other) for the
 * result, and check if we needed that borrowed bit. If we needed it, we
 * add it back at the next word's subtraction.
 *
 * @param other the quantity to subtract.
 * @return BigInt& this
 */
BigInt &BigInt::sub(const BigInt &other) {
  assert(*this >= other);  // Else can't guarantee borrow logic

  // Short circuit simple cases
  if (other == ZERO) return *this;

  std::size_t offset{0};
  WORD borrow{0};

  // i.e. "borrow 1 from next bucket"
  const CALC tentative = CALC{1} << WORD_BITS;

  for (auto &i : other.magnitude) {
    // All the math is 64bit
    CALC a = static_cast<CALC>(get_word(offset)) + tentative;
    CALC b = static_cast<CALC>(i) + borrow;
    CALC res = a - b;
    set_word(offset, res & WORD_MASK);  // Lower 32 is result
    borrow = res <= WORD_MASK;          // Did we need the tentative borrow?
    ++offset;
  }
  while (borrow) {
    CALC a = static_cast<CALC>(get_word(offset)) + tentative;
    CALC res = a - borrow;
    set_word(offset, res & WORD_MASK);  // Lower 32 is result
    borrow = res <= WORD_MASK;          // Did we need the tentative borrow?
    ++offset;
  }

  return this->normalize();
}

/**
 * @brief Multiplication. Algorithm is simple but O(n^2). We multiply each
 * "word" of our own by each "word" of other, and accumulate that result shifted
 * by the current power we are at. (Like high school math with pen and paper
 * would do so.)
 *
 * N.B. multiplying two 32 bits and adding a third 32 bits will fit into 64
 * bits.
 *
 * @param other the quantity to multiply by
 * @return BigInt& this
 */
BigInt &BigInt::mul(const BigInt &other) {
  // Short circuit simple cases
  if (other == ZERO) {
    magnitude.clear();
    negative = false;
    return *this;
  }

  // Handle sign
  if (negative == other.negative)
    negative = false;
  else
    negative = true;

  // Short circuits
  if (other == ONE || other == NEG_ONE) return *this;
  // Any powers of two can just be bit shifts
  if (other == TWO || other == NEG_TWO) return (*this) <<= 1u;
  // Can also do powers of 10 like below
  if (other == TEN || other == NEG_TEN) {
    auto tmp = *this << 1;  // * 2
    *this <<= 3;            // * 8
    *this += tmp;
    return *this;
  }

  // The main algorithm
  auto accum = ZERO;
  for (auto i = 0ul; i < magnitude.size(); ++i) {
    for (auto j = 0ul; j < other.magnitude.size(); ++j) {
      // All the math is 64bit
      CALC l = static_cast<CALC>(get_word(i));
      CALC r = static_cast<CALC>(other.get_word(j));

      // Shift product by current power we are at (i,j) and accumulate total
      auto res = BigInt(l * r) <<= ((i + j) * WORD_BITS);
      accum += res;
    }
  }

  magnitude = std::move(accum.magnitude);
  return this->normalize();
}

//
// Comparison operators
//

/**
 * @brief The "spaceship" operator, providing strong_ordering.
 */
std::strong_ordering BigInt::operator<=>(const BigInt &other) const {
  // Sign tests
  if (!negative && other.negative) return std::strong_ordering::greater;
  if (negative && !other.negative) return std::strong_ordering::less;

  // Fast magnitude tests
  if (magnitude == other.magnitude) return std::strong_ordering::equal;
  if (magnitude.size() < other.magnitude.size())
    return negative ? std::strong_ordering::greater
                    : std::strong_ordering::less;
  if (magnitude.size() > other.magnitude.size())
    return negative ? std::strong_ordering::less
                    : std::strong_ordering::greater;

  // "word" based compare, largest first
  auto i = std::max(magnitude.size(), other.magnitude.size()) - 1;
  do {
    if (get_word(i) < other.get_word(i))
      return negative ? std::strong_ordering::greater
                      : std::strong_ordering::less;
    if (get_word(i) > other.get_word(i))
      return negative ? std::strong_ordering::less
                      : std::strong_ordering::greater;
  } while (i-- > 0);

  assert(0);  // Shouldn't ever get here!
}

bool BigInt::operator==(const BigInt &other) const {
  return negative == other.negative && magnitude == other.magnitude;
}

//
// Assignment operators
//

BigInt &BigInt::operator+=(const BigInt &other) {
  if (negative == other.negative) {
    // Same signs, so add: abs(this) + abs(other)
    return add(other);
  } else if (other <= *this) {
    // Different signs, so sub: abs(this) - abs(other)
    return sub(other);
  } else {
    // Different signs, so sub: abs(other) - abs(this)
    auto tmp = other;
    tmp.sub(*this);
    magnitude = std::move(tmp.magnitude);
    return *this;
  }
}

BigInt &BigInt::operator-=(const BigInt &other) {
  if (negative != other.negative) {
    // Different signs, so add: abs(this) + abs(other)
    return add(other);
  } else if (other <= *this) {
    // Same signs, so sub: abs(this) - abs(other)
    return sub(other);
  } else {
    // Same signs, so sub: -(abs(other) - abs(this))
    auto tmp = other;
    tmp.add(*this);
    magnitude = std::move(tmp.magnitude);
    negative = true;
    return *this;
  }
}

BigInt &BigInt::operator*=(const BigInt &other) { return mul(other); }

BigInt &BigInt::operator/=(const BigInt &other) {
  auto tmp = div(BigInt(*this), other);
  magnitude = std::move(tmp.quot.magnitude);
  return *this;
}

BigInt &BigInt::operator%=(const BigInt &other) {
  auto tmp = div(BigInt(*this), other);
  magnitude = std::move(tmp.rem.magnitude);
  return *this;
}

BigInt &BigInt::operator&=(const BigInt &other) {
  map(other, [](auto l, auto r) { return l & r; });
  return *this;
}

BigInt &BigInt::operator|=(const BigInt &other) {
  map(other, [](auto l, auto r) { return l | r; });
  return *this;
}

BigInt &BigInt::operator^=(const BigInt &other) {
  map(other, [](auto l, auto r) { return l ^ r; });
  return *this;
}

/**
 * @brief Left shift operator. Will grow underlying storage accordingly (as
 * opposed to any overflow semantics).
 *
 * @param bits number of bits to shift
 * @return BigInt& this
 */
BigInt &BigInt::operator<<=(std::size_t bits) {
  auto words_shift = bits / WORD_BITS;
  auto bits_shift = bits % WORD_BITS;

  // "word" shifts add zeros to the vector's start
  if (words_shift) magnitude.insert(magnitude.begin(), words_shift, WORD{0u});

  // starting after the newly inserted "words", we left shift the remaining bits
  // in the subspan, carrying any overflow accordinly.
  if (bits_shift) {
    WORD carry{0};
    for (auto &i : std::span(magnitude).subspan(
             words_shift, magnitude.size() - words_shift)) {
      // Temporarily hold the high bits (the carry)
      auto tmp = i & (WORD_MASK << (WORD_BITS - bits_shift));
      i <<= bits_shift;
      if (carry) i |= carry;
      carry = tmp >> (WORD_BITS - bits_shift);
    }
    if (carry) magnitude.push_back(carry);
  }

  return *this;
}

/**
 * @brief Right shift operator. Will shrink underlying storage accordingly.
 *
 * @param bits number of bits to shift
 * @return BigInt& this
 */
BigInt &BigInt::operator>>=(std::size_t bits) {
  auto words_shift = bits / WORD_BITS;
  auto bits_shift = bits % WORD_BITS;

  // "word" shifts remove from the vector's start
  if (words_shift)
    magnitude.erase(
        magnitude.begin(),
        magnitude.begin() + std::min(words_shift, magnitude.size()));

  // Right shift the remaining bits, carrying any overflow accordinly.
  if (bits_shift) {
    WORD carry{0};
    for (auto &i : magnitude | std::views::reverse) {
      // Temporarily hold the low bits (the carry)
      auto tmp = i & (WORD_MASK >> (WORD_BITS - bits_shift));
      i >>= bits_shift;
      if (carry) i |= carry;
      carry = tmp << (WORD_BITS - bits_shift);
    }
  }

  return this->normalize();
}

//
// Increment operators
//

BigInt &BigInt::operator++() { return (*this) += ONE; }
BigInt BigInt::operator++(int) {
  BigInt ret(*this);
  ++(*this);
  return ret;
}

BigInt &BigInt::operator--() { return (*this) -= ONE; }
BigInt BigInt::operator--(int) {
  BigInt ret(*this);
  --(*this);
  return ret;
}

//
// Arithmetic operators
//

BigInt operator+(const BigInt &lhs, const BigInt &rhs) {
  auto ret = lhs;
  return ret += rhs;
}

BigInt BigInt::operator+() const { return *this; }

BigInt operator-(const BigInt &lhs, const BigInt &rhs) {
  auto ret = lhs;
  return ret -= rhs;
}

BigInt BigInt::operator-() const {
  auto ret = *this;
  if (*this != ZERO) ret.negative = !ret.negative;
  return ret;
}

BigInt operator*(const BigInt &lhs, const BigInt &rhs) {
  auto ret = lhs;
  return ret *= rhs;
}

BigInt operator/(const BigInt &lhs, const BigInt &rhs) {
  return BigInt::div(lhs, rhs).quot;
}

BigInt operator%(const BigInt &lhs, const BigInt &rhs) {
  return BigInt::div(lhs, rhs).rem;
}

/**
 * @brief Two's complement bitwise NOT.
 *
 * @return BigInt
 */
BigInt BigInt::operator~() const {
  if (*this < ZERO) return abs(*this) - ONE;
  return -(*this) - ONE;
}

BigInt operator&(const BigInt &lhs, const BigInt &rhs) {
  return BigInt::map(lhs, rhs, [](auto l, auto r) { return l & r; });
}

BigInt operator|(const BigInt &lhs, const BigInt &rhs) {
  return BigInt::map(lhs, rhs, [](auto l, auto r) { return l | r; });
}

BigInt operator^(const BigInt &lhs, const BigInt &rhs) {
  return BigInt::map(lhs, rhs, [](auto l, auto r) { return l ^ r; });
}

BigInt operator<<(const BigInt &lhs, std::size_t bits) {
  auto ret = lhs;
  return ret <<= bits;
}

BigInt operator>>(const BigInt &lhs, std::size_t bits) {
  auto ret = lhs;
  return ret >>= bits;
}

BigInt BigInt::abs(const BigInt &num) {
  auto ret = num;
  ret.negative = false;
  return ret;
}

/**
 * @brief Division, returning both quotient and remainder (a.k.a divmod).
 * Algorithm is "shift and subtract" and divides towards zero. (Like high school
 * math with pen and paper would do so, only in binary.)
 *
 * See https://en.wikipedia.org/wiki/Division_algorithm under "Long division"
 * for a more detailed explanation. The for loop runs one time for each of the
 * bits in the expected quotient.
 *
 * @param dividend the dividend
 * @param divisor the divisor
 * @return DivMod the result
 */
DivMod BigInt::div(const BigInt &dividend, const BigInt &divisor) {
  // Short circuits
  if (divisor == ZERO) throw std::invalid_argument("Divide by zero!");
  if (divisor == ONE) return DivMod(dividend, ZERO);
  if (divisor == NEG_ONE) return DivMod(-dividend, ZERO);
  // Any powers of two can just be bit shifts
  if (divisor == TWO || divisor == NEG_TWO) {
    auto quot = dividend >> 1u;
    auto rem = dividend & 1u;
    return DivMod(dividend.negative != divisor.negative ? -quot : quot,
                  divisor == NEG_TWO ? -rem : rem);
  }
  if (dividend == divisor)
    return DivMod(dividend.negative == divisor.negative ? ONE : NEG_ONE, ZERO);
  if (abs(dividend) < abs(divisor)) return DivMod(ZERO, dividend);
  // N.B. "Hackers Delight" chapter 10 "Integer Division By Constants" is
  // an entire section on alternative algorithms suitable for special cases

  auto div_d = abs(dividend);
  auto div_s = abs(divisor);

  // Setup loop for super simple "shift and subtract" division
  auto k = div_d.bitsize() - div_s.bitsize();  // Bits in quotient (minus 1)
  div_s <<= k;                                 // Normalize divisor
  auto quotient = ZERO;

  // Iterate k+1 times, calculating the quotient's bit on each iteration
  for (std::size_t i{0}; i <= k; ++i) {
    quotient <<= 1;
    if (div_d >= div_s) {
      div_d -= div_s;
      quotient += ONE;  // Set the i'th bit
    }
    div_s >>= 1;
  }

  // Handle sign
  if (dividend.negative != divisor.negative) quotient.negative = true;
  if (dividend.negative) div_d.negative = true;

  return DivMod(quotient.normalize(), div_d.normalize());
}

/**
 * @brief Power, (i.e. base ** exp). Algorithm is to multiply by base if
 * exponent is odd and subtract 1 from exponent, or to multiply by square of
 * base if exponent is even and divide that by two.
 *
 * @param base the base
 * @param exp the exponenet to raise the base by
 * @return BigInt the result
 */
BigInt BigInt::pow(const BigInt &base, const BigInt &exp) {
  if ((base == ONE || base == NEG_ONE) && exp == NEG_ONE) return base;
  if (exp < ZERO) return ZERO;  // These will always floor to zero
  if (exp == ZERO) return ONE;
  if (exp == ONE) return base;
  if (exp == TWO) return base * base;

  auto ret = ONE;
  auto x = base;
  auto n = exp;

  // Simple logic!
  // For all 'n', if odd multiply by base, if even multiply by base squared.
  while (n > ZERO) {
    if ((n & ONE) == ONE) {
      // n is odd
      ret *= x;
      n -= ONE;
    }
    // n is even, square and divide by 2
    x *= x;
    n >>= 1;
  }

  return ret;
}

/**
 * @brief Power, (i.e. base ** exp % mod). Algorithm is same as regular pow(),
 * only we now mod the values on each iteration.
 *
 * @param base the base
 * @param exp the exponenet to raise the base by
 * @return BigInt the result
 */
BigInt BigInt::pow(const BigInt &base, const BigInt &exp, const BigInt &mod) {
  if ((base == ONE || base == NEG_ONE) && exp == NEG_ONE) return base;
  if (exp < ZERO) return ZERO;  // These will always floor to zero
  if (exp == ZERO) return ONE;
  if (exp == ONE) return base % mod;
  if (exp == TWO) return (base * base) % mod;

  auto ret = ONE;
  auto x = base;
  auto n = exp;

  // Simple logic!
  // If odd multiply by base, if even multiply by base squared; then mod.
  while (n > ZERO) {
    if ((n & ONE) == ONE) {
      // n is odd
      ret *= x;
      ret %= mod;
      n -= ONE;
    }

    // n is even, square. mod, and divide by 2
    x *= x;
    x %= mod;
    n >>= 1;
  }

  return ret;
}

/**
 * @brief Integer square root (the floor of the actual root). Algorithm is
 * simple Newton's method.
 *
 * @param num the number to find the square root of
 * @return BigInt the result
 */
BigInt BigInt::isqrt(const BigInt &num) {
  if (num < ZERO)
    throw std::invalid_argument("Cannot caclulate negative roots!");
  if (num == ZERO) return ZERO;
  if (num == ONE) return ONE;
  if (num == TWO) return ONE;

  auto x = num;
  auto y = (num + ONE) >> 1;
  while (y < x) {
    x = y;
    y = (x + num / x) >> 1;
  }

  return x;
}

/**
 * @brief Returns the greatest common divisor of a and b, else zero. Algorithm
 * is simple Euclid's method.
 *
 * @param a
 * @param b
 * @return BigInt
 */
BigInt BigNum::BigInt::gcd(const BigInt &a, const BigInt &b) {
  if (a == ZERO && b == ZERO) return ZERO;
  if (a == ZERO) return b;
  if (b == ZERO) return a;
  if (a == ONE || a == NEG_ONE || b == ONE || b == NEG_ONE) return ONE;

  auto res = a;  // result
  auto rem = b;  // remainder
  while (rem != ZERO) {
    auto tmp = rem;
    rem = res % rem;
    res = tmp;
  }

  return abs(res);
}

BigInt BigInt::log2(const BigInt &num) {
  if (num < ZERO)
    throw std::invalid_argument("Cannot caclulate negative logs!");
  if (num == ZERO) throw std::invalid_argument("Log of 0!");
  return num.bitsize() - 1;
}

/**
 * @brief The size of this integer in bits.
 *
 * N.B. ZERO accupies zero bits.
 *
 * @return std::size_t a count of the bits.
 */
std::size_t BigInt::bitsize() const {
  std::size_t res{0};

  if (magnitude.size() > 0) {
    // Number of bits we occupy
    res += (magnitude.size() - 1) * WORD_BITS;
    res += WORD_BITS - std::countl_zero(magnitude.back());
  }

  return res;
}

/**
 * @brief The count of 1 bits in this integer.
 *
 * @return std::size_t a count of the 1 bits.
 */
std::size_t BigNum::BigInt::popcount() const {
  return std::ranges::fold_left(
      magnitude, 0, [](auto a, auto b) { return a + std::popcount(b); });
}

/**
 * @brief Stream out as hex, with leading "0x" characters.
 *
 * @param os ostream
 * @param bn the number
 * @return std::ostream& the human readable text
 */
std::ostream &operator<<(std::ostream &os, const BigInt &bn) {
  if (bn == BigInt::ZERO) {
    os << "0x0";
  } else {
    bool first = true;
    for (auto &i : bn.magnitude | std::views::reverse) {
      if (first) {
        if (bn.negative) os << "-";
        os << std::format("{:#x}", i);  // 0x prefix the most significant word
        first = false;
      } else {
        os << std::format("{:08x}", i);  // Zero pad the rest
      }
    }
  }

  return os;
}

/**
 * @brief Generate a binary representation, with leading "0b" characters.
 *
 * @return std::string the binary representation
 */
std::string BigInt::to_binary() const {
  if (*this == ZERO) return "0b";

  std::string result = "";
  if (negative) result.append("-");

  bool first = true;
  for (auto &i : magnitude | std::views::reverse) {
    if (first) {
      result.append(std::format("{:#b}", i));  // 0b prefix
      first = false;
    } else {
      result.append(std::format("{:032b}", i));  // Zero pad the rest
    }
  }

  return result;
}

/**
 * @brief Generate a decimal representation. Algorithm is simple, we
 * repeatedly divide by a large power of 10, and the remainders can be
 * rendered as decimal and concatenated together.
 *
 * @return std::string the decimal representation
 */
std::string BigInt::to_decimal() const {
  if (*this == ZERO) return "0";

  // A rather dumb converter where we divide by 1'000'000'000 in a loop.
  // This magic number must be less than the max value STOR can hold, as we
  // extract the remainder out via a single vector entry (rem.get_word(0)) and
  // pass that to std::to_string() to convert to human readable form.
  static const auto divisor = 1'000'000'000u;
  static_assert(WORD_MASK > divisor);

  std::string result = "";
  auto tmp = abs(*this);
  while (tmp != ZERO) {
    auto next = div(tmp, divisor);
    if (next.quot == ZERO) {
      // Left most digits, no padding
      result.insert(0, std::format("{:d}", next.rem.get_word(0)));
    } else {
      // Zero pad the rest
      result.insert(0, std::format("{:09d}", next.rem.get_word(0)));
    }
    tmp = next.quot;
  }

  if (negative) result.insert(0, "-");
  return result;
}

std::uint32_t BigNum::BigInt::to_uint32_t() const {
  if (*this > std::numeric_limits<std::uint32_t>::max())
    return std::numeric_limits<std::uint32_t>::max();
  if (*this < std::numeric_limits<std::uint32_t>::min())
    return std::numeric_limits<std::uint32_t>::min();

  return std::uint32_t(get_word(0));
}

std::int32_t BigNum::BigInt::to_int32_t() const {
  if (*this > std::numeric_limits<std::int32_t>::max())
    return std::numeric_limits<std::int32_t>::max();
  if (*this < std::numeric_limits<std::int32_t>::min())
    return std::numeric_limits<std::int32_t>::min();

  auto data = get_word(0);
  if (negative) {
    return (std::int32_t((1u << 31) - data) | (1u << 31));
  } else {
    return std::int32_t(data);
  }
}

std::uint64_t BigNum::BigInt::to_uint64_t() const {
  if (*this > std::numeric_limits<std::uint64_t>::max())
    return std::numeric_limits<std::uint64_t>::max();
  if (*this < std::numeric_limits<std::uint64_t>::min())
    return std::numeric_limits<std::uint64_t>::min();

  std::uint64_t ret{get_word(1)};
  ret <<= WORD_BITS;
  ret |= get_word(0);
  return ret;
}

std::int64_t BigNum::BigInt::to_int64_t() const {
  if (*this > std::numeric_limits<std::int64_t>::max())
    return std::numeric_limits<std::int64_t>::max();
  if (*this < std::numeric_limits<std::int64_t>::min())
    return std::numeric_limits<std::int64_t>::min();

  std::uint64_t data{get_word(1)};
  data <<= WORD_BITS;
  data |= get_word(0);
  if (negative) {
    return (std::int64_t((1ul << 63) - data) | (1ul << 63));
  } else {
    return std::int64_t(data);
  }
}

std::float32_t BigNum::BigInt::to_float() const {
  if (*this == NEG_ONE) return -1.0f;
  if (*this == ZERO) return 0.0f;
  if (*this == ONE) return 1.0f;

  if (abs(*this) > std::numeric_limits<std::float32_t>::max()) {
    if (*this > 0)
      return std::numeric_limits<std::float32_t>::max();
    else
      return -std::numeric_limits<std::float32_t>::max();
  }

  // To IEEE 754 form
  // Fraction is 23 low bits
  std::uint32_t tmp{0};
  if (bitsize() > 23) {
    tmp = (*this >> (bitsize() - 23)).to_int32_t() & 0x7FFFFF;
  } else {
    tmp = (*this << (23 - bitsize() + 1)).to_int32_t() & 0x7FFFFF;
  }

  // Exponent
  tmp |= (bitsize() + 127 - 1) << 23;

  // Sign bit
  if (negative) {
    tmp |= (1u << 31);
  }

  return reinterpret_cast<std::float32_t &>(tmp);
}

std::float64_t BigNum::BigInt::to_double() const {
  if (*this == NEG_ONE) return -1.0;
  if (*this == ZERO) return 0.0;
  if (*this == ONE) return 1.0;

  if (abs(*this) > std::numeric_limits<std::float64_t>::max()) {
    if (*this > 0)
      return std::numeric_limits<std::float64_t>::max();
    else
      return -std::numeric_limits<std::float64_t>::max();
  }

  // To IEEE 754 form
  // Fraction is 52 low bits
  std::uint64_t tmp{0};
  if (bitsize() > 52) {
    tmp = (*this >> (bitsize() - 52)).to_int64_t() & 0xFFFFFFFFFFFFF;
  } else {
    tmp = (*this << (52 - bitsize() + 1)).to_int64_t() & 0xFFFFFFFFFFFFF;
  }

  // Exponent
  tmp |= (bitsize() + 1023 - 1) << 52;

  // Sign bit
  if (negative) {
    tmp |= (1ul << 63);
  }

  return reinterpret_cast<std::float64_t &>(tmp);
}

//
// Random number
//

BigIntRand::BigIntRand() { rng = std::mt19937{std::random_device()()}; }

BigIntRand::BigIntRand(std::uint32_t seed) { rng = std::mt19937{seed}; }

BigInt BigIntRand::random(std::size_t bits) {
  // std::mt19937 is 32 bit, so ensure it matches WORD
  static_assert(std::is_same<std::uint32_t, BigInt::WORD>::value,
                "Type mismatch");

  if (bits == 0u) return BigInt::ZERO;

  auto ret = BigInt::ZERO;
  std::size_t i{0};
  for (i = 0; i < bits / BigInt::WORD_BITS; ++i)
    ret.set_word(i, word_dist(rng));

  auto remainder = bits % BigInt::WORD_BITS;
  if (remainder) {
    // Add another WORD of randomness, then shift it down to size
    ret.set_word(i, word_dist(rng));
    ret >>= (BigInt::WORD_BITS - remainder);
  }

  return ret.normalize();
}

}  // namespace BigNum
