/*******************************************************************************
 * Copyright (c) 2024, Christopher Hill <ch6574@gmail.com>
 * GNU General Public License v3.0+ (see
 * https://www.gnu.org/licenses/gpl-3.0.txt)
 * SPDX-License-Identifier: GPL-3.0-or-later
 ******************************************************************************/

#include "big_uint.hpp"

#include <assert.h>
#include <fmt/core.h>  // replace with std::format

#include <algorithm>
#include <bit>
#include <iostream>
#include <ranges>
#include <span>
#include <string>

namespace BigNum {

// Constants
const BigUInt BigUInt::ZERO = BigUInt();
const BigUInt BigUInt::ONE = BigUInt(1u);
const BigUInt BigUInt::TWO = BigUInt(2u);

// Constructors
BigUInt::BigUInt() : data(0) {}

BigUInt::BigUInt(std::uint32_t data) : data(0) {
  // Type must be equal to "WORD" for a direct copy to make sense
  static_assert(std::is_same<std::uint32_t, WORD>::value, "Type mismatch");
  BigUInt::data = std::vector<WORD>{data};
  trim();
}

BigUInt::BigUInt(std::uint64_t data) : data(0) {
  std::uint32_t lower = data & WORD_MASK;
  std::uint32_t upper = data >> WORD_BITS;
  if (upper)
    BigUInt::data = std::vector<WORD>{lower, upper};
  else
    BigUInt::data = std::vector<WORD>{lower};
  trim();
}

/**
 * @brief Mainly string parsing logic. If the string starts with "0x" we parse
 * as hex, else we parse as decimal.
 *
 * Separators ' are allowed, so "0x1234'5678" is eqivalent to "0x12345678".
 */
BigUInt::BigUInt(std::string_view num) {
  if (num.length() > 2 &&
      ((num.substr(0, 2) == "0x") || num.substr(0, 2) == "0x")) {
    // hex strings "0x...". Remove prefix and any ' separators, then validate
    num.remove_prefix(2);
    auto hex = std::string{num};
    hex.erase(std::remove(hex.begin(), hex.end(), '\''), hex.end());
    if (hex.find_first_not_of("0123456789abcdefABCDEF") != std::string::npos)
      throw std::invalid_argument("Invalid hex string: " + std::string{num});

    // Read string in chunks of 8 hex chars (i.e. 32bits) directly into data
    auto chunks = hex.length() / 8;
    auto remainder = hex.length() % 8;
    data = std::vector<WORD>(remainder ? chunks + 1 : chunks);

    std::size_t i{0};
    for (i = 0; i < chunks; ++i) {
      // Process chunks from right hand side of string as base 16
      auto start = hex.length() - (8 * i) - 8;
      auto text = hex.substr(start, 8);
      data[i] = std::stoul(text, nullptr, 16) & WORD_MASK;
    }
    if (remainder) {
      auto text = hex.substr(0, remainder);
      data[i] = std::stoul(text, nullptr, 16) & WORD_MASK;
    }

    // N.B. in c++23 one could use the below to do all the chunk processing...
    // data = std::string_view{hex}
    //   | std::views::reverse
    //   | std::views::chunk(8)
    //   | std::ranges::to<std::vector<std::string>>()
    //   | std::view::transform([](auto v){
    //         WORD i{0};
    //         std::from_chars(v.data(), v.data() + v.size(), i, 16);
    //         return i;
    //       })
    //   | std::ranges::to<std::vector<WORD>>();

  } else {
    // Assume decimal. Remove any ' separators, then validate.
    auto dec = std::string{num};
    dec.erase(std::remove(dec.begin(), dec.end(), '\''), dec.end());
    if (dec.find_first_not_of("0123456789") != std::string::npos)
      throw std::invalid_argument("Invalid decimal string: " +
                                  std::string{num});

    // Read string in chunks of 9 decimal chars, and sum into temporary BigUInt
    auto chunks = dec.length() / 9;
    auto remainder = dec.length() % 9;
    auto tmp = BigUInt::ZERO;
    static unsigned pow10[] = {1,           10,           100,       1'000,
                               10'000,      100'000,      1'000'000, 10'000'000,
                               100'000'000, 1'000'000'000};

    std::size_t i{0};
    for (i = 0; i < chunks; ++i) {
      // Process chunks from left side of string as base 10
      auto text = dec.substr(9 * i, 9);
      tmp *= pow10[9];                       // "shift" by 10**9
      tmp += std::stoul(text, nullptr, 10);  // add next chunk
    }
    if (remainder) {
      auto text = dec.substr(9 * i, remainder);
      tmp *= pow10[remainder];
      tmp += std::stoul(text, nullptr, 10);
    }
    data = std::move(tmp.data);  // Take the temporary BigUInt's data

    // N.B. in c++23 one could use the below to do all the chunk processing...
    // auto tmp = BigUInt::ZERO;
    // static unsigned pow10[] = {1, 10, 100, ... };
    // std::ranges::for_each(std::string_view{dec} | std::views::chunk(9),
    //   [&](auto v){
    //       WORD i{0};
    //       std::from_chars(v.data(), v.data() + v.size(), i, 10);
    //       tmp *= pow10[v.size()];
    //       tmp += i;
    //   });
  }

  trim();
}

/**
 * @brief A safe getter of a "word" at supplied offset.
 *
 * @param offset the offset
 * @return BigUInt::WORD the word at that offset, else 0
 */
BigUInt::WORD BigUInt::get_word(std::size_t offset) const {
  if (data.size() > offset) return data[offset];
  return WORD{0};
}

/**
 * @brief A safe setter of a "word" at supplied offset. Underlying storage will
 * grow to accomodate offset if out of bounds.
 *
 * @param offset the offset
 * @param data the word to set
 */
void BigUInt::set_word(std::size_t offset, WORD data) {
  if (this->data.size() <= offset)
    this->data.insert(this->data.end(), offset - this->data.size() + 1, 0);
  this->data[offset] = data;
}

/**
 * @brief Discards any superfluous leading zeros
 */
inline BigUInt &BigUInt::trim() {
  while (!data.empty() && data.back() == WORD{0}) data.pop_back();
  return *this;
}

/**
 * @brief Mapping function. Applies the supplied func() to all "words" in
 * ourself and other.
 *
 * @param other the other number
 * @param func the supplied func to map over two WORDs
 */
void BigUInt::map(const BigUInt &other, std::function<WORD(WORD, WORD)> func) {
  // C++23 has "zip" to make this more concise.
  for (auto i = 0; i < std::max(data.size(), other.data.size()); ++i) {
    set_word(i, func(get_word(i), other.get_word(i)));
  }
  trim();
}

/**
 * @brief Mapping function. Applies the supplied func() to all "words" in
 * supplied left and right sides.
 *
 * @param lhs the left hand side
 * @param rhs the right hand side
 * @param func the supplied func to map over two WORDs
 * @return BigUInt
 */
BigUInt BigUInt::map(const BigUInt &lhs, const BigUInt &rhs,
                     std::function<WORD(WORD, WORD)> func) {
  // Apply func() across all "words"
  auto ret = BigUInt();

  // C++23 has "zip" to make this more concise.
  for (auto i = 0; i < std::max(lhs.data.size(), rhs.data.size()); ++i) {
    ret.data.push_back(func(lhs.get_word(i), rhs.get_word(i)));
  }

  return ret.trim();
}

//
// Comparison operators
//

/**
 * @brief The "spaceship" operator, providing strong_ordering.
 */
std::strong_ordering BigUInt::operator<=>(const BigUInt &other) const {
  // Fast assertions
  if (data == other.data) return std::strong_ordering::equal;
  if (data.size() < other.data.size()) return std::strong_ordering::less;
  if (data.size() > other.data.size()) return std::strong_ordering::greater;

  // "word" based compare, largest first
  for (auto i = std::max(data.size(), other.data.size()) - 1; i >= 0; --i) {
    if (get_word(i) < other.get_word(i)) return std::strong_ordering::less;
    if (get_word(i) > other.get_word(i)) return std::strong_ordering::greater;
  }

  assert(0);  // Shouldn't ever get here!
}

bool BigUInt::operator==(const BigUInt &other) const {
  return data == other.data;
}

//
// Assignment operators
//

/**
 * @brief Addition. Algorithm is simple, add corresponding "words" together at
 * each increasing offset, and carry any overflow to the next offset's sum.
 *
 * N.B. adding 3 lots of 32 bits (this + other + carry) will fit into 34 bits.
 *
 * @param other the quantity to add.
 * @return BigUInt& this
 */
BigUInt &BigUInt::operator+=(const BigUInt &other) {
  std::size_t offset{0};
  WORD carry{0};
  for (auto &i : other.data) {
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
 * @brief Subtraction. Algorithm is simple, subtract corresponding "words" at
 * each increasing offset, however we "borrow" 1 bit from the next word to avoid
 * handling underflow.
 *
 * N.B. we effectively subtract 33 bits (this) from 32 bits (other) for the
 * result, and check if we needed that borrowed bit. If we needed it, we
 * add it back at the next word's subtraction.
 *
 * @param other the quantity to subtract.
 * @return BigUInt& this
 */
BigUInt &BigUInt::operator-=(const BigUInt &other) {
  if (other > *this) throw std::invalid_argument("Result would be negative!");

  std::size_t offset{0};
  WORD borrow{0};

  // i.e. "borrow 1 from next bucket"
  const CALC tentative = CALC{1} << WORD_BITS;

  for (auto &i : other.data) {
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

  return this->trim();
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
 * @return BigUInt& this
 */
BigUInt &BigUInt::operator*=(const BigUInt &other) {
  if (other == ZERO) {
    data.clear();
    return *this;
  }
  if (other == ONE) return *this;
  if (other == TWO) return (*this) <<= 1u;

  auto accum = BigUInt::ZERO;
  for (auto i = 0; i < data.size(); ++i) {
    for (auto j = 0; j < other.data.size(); ++j) {
      // All the math is 64bit
      CALC l = data[i];
      CALC r = other.data[j];

      // Shift product by current power we are at (i,j) and accumulate total
      auto res = BigUInt(l * r) <<= ((i + j) * WORD_BITS);
      accum += res;
    }
  }
  data = std::move(accum.data);

  return this->trim();
}

BigUInt &BigUInt::operator/=(const BigUInt &other) {
  auto ret = div(BigUInt(*this), other);
  data = std::move(ret.quot.data);
  return *this;
}

BigUInt &BigUInt::operator%=(const BigUInt &other) {
  auto ret = div(BigUInt(*this), other);
  data = std::move(ret.rem.data);
  return *this;
}

BigUInt &BigUInt::operator&=(const BigUInt &other) {
  map(other, [](auto l, auto r) { return l & r; });
  return *this;
}

BigUInt &BigUInt::operator|=(const BigUInt &other) {
  map(other, [](auto l, auto r) { return l | r; });
  return *this;
}

BigUInt &BigUInt::operator^=(const BigUInt &other) {
  map(other, [](auto l, auto r) { return l ^ r; });
  return *this;
}

/**
 * @brief Left shift operator. Will grow underlying storage accordingly (as
 * opposed to any overflow semantics).
 *
 * @param bits number of bits to shift
 * @return BigUInt& this
 */
BigUInt &BigUInt::operator<<=(std::size_t bits) {
  auto words_shift = bits / WORD_BITS;
  auto bits_shift = bits % WORD_BITS;

  // "word" shifts add zeros to the vector's start
  if (words_shift) data.insert(data.begin(), words_shift, WORD{0u});

  // starting after the newly inserted "words", we left shift the remaining bits
  // in the subspan, carrying any overflow accordinly.
  if (bits_shift) {
    WORD carry{0};
    for (auto &i :
         std::span(data).subspan(words_shift, data.size() - words_shift)) {
      // Temporarily hold the high bits (the carry)
      auto tmp = i & (WORD_MASK << (WORD_BITS - bits_shift));
      i <<= bits_shift;
      if (carry) i |= carry;
      carry = tmp >> WORD_BITS - bits_shift;
    }
    if (carry) data.push_back(carry);
  }

  return *this;
}

/**
 * @brief Right shift operator. Will shrink underlying storage accordingly.
 *
 * @param bits number of bits to shift
 * @return BigUInt& this
 */
BigUInt &BigUInt::operator>>=(std::size_t bits) {
  auto words_shift = bits / WORD_BITS;
  auto bits_shift = bits % WORD_BITS;

  // "word" shifts remove from the vector's start
  if (words_shift)
    data.erase(data.begin(), data.begin() + std::min(words_shift, data.size()));

  // Right shift the remaining bits, carrying any overflow accordinly.
  if (bits_shift) {
    WORD carry{0};
    for (auto &i : data | std::views::reverse) {
      // Temporarily hold the low bits (the carry)
      auto tmp = i & (WORD_MASK >> (WORD_BITS - bits_shift));
      i >>= bits_shift;
      if (carry) i |= carry;
      carry = tmp << WORD_BITS - bits_shift;
    }
  }

  return this->trim();
}

//
// Increment operators
//

BigUInt &BigUInt::operator++() { return (*this) += ONE; }
BigUInt BigUInt::operator++(int) {
  BigUInt ret(*this);
  ++(*this);
  return ret;
}

BigUInt &BigUInt::operator--() { return (*this) -= ONE; }
BigUInt BigUInt::operator--(int) {
  BigUInt ret(*this);
  --(*this);
  return ret;
}

//
// Arithmetic operators
//

BigUInt operator+(const BigUInt &lhs, const BigUInt &rhs) {
  auto ret = lhs;
  return ret += rhs;
}

BigUInt operator-(const BigUInt &lhs, const BigUInt &rhs) {
  auto ret = lhs;
  return ret -= rhs;
}

BigUInt operator*(const BigUInt &lhs, const BigUInt &rhs) {
  auto ret = lhs;
  return ret *= rhs;
}

BigUInt operator/(const BigUInt &lhs, const BigUInt &rhs) {
  return BigUInt::div(lhs, rhs).quot;
}

BigUInt operator%(const BigUInt &lhs, const BigUInt &rhs) {
  return BigUInt::div(lhs, rhs).rem;
}

/**
 * @brief Bitwise NOT. Algorithm simply toggles all bits in each "word", and
 * than removes any excess from the final "word" that were touched.
 *
 * N.B. As we use a variable bit size there is no concept of "leading zeros",
 * and so calling ~0x0 will return 0x1 (as opposed to infite 1s). This will lead
 * to the observation that ~0xff == 0x0, but ~0x0 != 0xff
 *
 * @return BigUInt& this
 */
BigUInt &BigUInt::operator~() {
  if (*this == ZERO) {
    *this = ONE;
    return *this;
  }

  // Last "word" requires care, need to re-zero any "extra" bits
  auto leading_zeros = std::countl_zero(data.back());
  std::transform(data.cbegin(), data.cend(), data.begin(),
                 [](auto &i) { return ~i; });
  data.back() &= (WORD_MASK >> leading_zeros);

  return this->trim();
}

BigUInt operator&(const BigUInt &lhs, const BigUInt &rhs) {
  return BigUInt::map(lhs, rhs, [](auto l, auto r) { return l & r; });
}

BigUInt operator|(const BigUInt &lhs, const BigUInt &rhs) {
  return BigUInt::map(lhs, rhs, [](auto l, auto r) { return l | r; });
}

BigUInt operator^(const BigUInt &lhs, const BigUInt &rhs) {
  return BigUInt::map(lhs, rhs, [](auto l, auto r) { return l ^ r; });
}

BigUInt operator<<(const BigUInt &lhs, std::size_t bits) {
  auto ret = lhs;
  return ret <<= bits;
}

BigUInt operator>>(const BigUInt &lhs, std::size_t bits) {
  auto ret = lhs;
  return ret >>= bits;
}

/**
 * @brief Division, returning both quotient and remainder (a.k.a divmod).
 * Algorithm is "shift and subtract". (Like high school math with pen and paper
 * would do so, only in binary.)
 *
 * See https://en.wikipedia.org/wiki/Division_algorithm under "Long division"
 * for a more detailed explanation. The for loop runs one time for each of the
 * bits in the expected quotient.
 *
 * @param dividend the dividend
 * @param divisor the divisor
 * @return DivMod the result
 */
DivMod BigUInt::div(const BigUInt &dividend, const BigUInt &divisor) {
  if (divisor == ZERO) throw std::invalid_argument("Divide by zero!");
  if (divisor == ONE) return DivMod(dividend, ZERO);
  if (divisor == TWO) return DivMod(dividend >> 1u, dividend & 1u);
  if (dividend == divisor) return DivMod(ONE, ZERO);
  if (dividend < divisor) return DivMod(ZERO, dividend);

  auto div_d = dividend;
  auto div_s = divisor;

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

  return DivMod(quotient.trim(), div_d.trim());
}

/**
 * @brief Power, (i.e. base ** exp). Algorithm is to multiply by base if
 * exponent is odd and subtract 1 from exponent, or to multiply by square of
 * base if exponent is even and divide that by two.
 *
 * @param base the base
 * @param exp the exponenet to raise the base by
 * @return BigUInt the result
 */
BigUInt BigUInt::pow(const BigUInt &base, const BigUInt &exp) {
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
 * @return BigUInt the result
 */
BigUInt BigUInt::pow(const BigUInt &base, const BigUInt &exp,
                     const BigUInt &mod) {
  if (exp == ZERO) return ONE;
  if (exp == ONE) return base;
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
 * @return BigUInt the result
 */
BigUInt BigUInt::isqrt(const BigUInt &num) {
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

BigUInt BigUInt::log2(const BigUInt &num) {
  if (num == ZERO) throw std::invalid_argument("Log of 0!");
  return num.bitsize() - 1;
}

/**
 * @brief Provides a count of all the bits we are using.
 *
 * N.B. ZERO accupies zero bits.
 *
 * @return std::size_t a count of the bits.
 */
std::size_t BigUInt::bitsize() const {
  std::size_t res{0};

  if (data.size() > 0) {
    // Number of bits we occupy
    res += (data.size() - 1) * WORD_BITS;
    res += WORD_BITS - std::countl_zero(data.back());
  }

  return res;
}

/**
 * @brief Stream out as hex, with leading "0x" characters.
 *
 * @param os ostream
 * @param bn the number
 * @return std::ostream& the human readable text
 */
std::ostream &operator<<(std::ostream &os, const BigUInt &bn) {
  if (bn == BigUInt::ZERO) {
    os << "0x0";
  } else {
    bool first = true;
    for (auto &i : bn.data | std::views::reverse) {
      if (first) {
        os << fmt::format("{:#x}", i);  // 0x prefix the most significant word
        first = false;
      } else {
        os << fmt::format("{:08x}", i);  // Zero pad the rest
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
std::string BigUInt::to_binary() const {
  if (*this == ZERO) return "0b";

  std::string result = "";
  bool first = true;
  for (auto &i : data | std::views::reverse) {
    if (first) {
      result.append(fmt::format("{:#b}", i));  // 0b prefix
      first = false;
    } else {
      result.append(fmt::format("{:032b}", i));  // Zero pad the rest
    }
  }

  return result;
}

/**
 * @brief Generate a decimal representation. Algorithm is simple, we repeatedly
 * divide by a large power of 10, and the remainders can be rendered as decimal
 * and concatenated together.
 *
 * @return std::string the decimal representation
 */
std::string BigUInt::to_decimal() const {
  if (*this == ZERO) return "0";

  // A rather dumb converter where we divide by 1'000'000'000 in a loop.
  // This magic number must be less than the max value STOR can hold, as we
  // extract the remainder out via a single vector entry (rem.get_word(0)) and
  // pass that to std::to_string() to convert to human readable form.
  static const auto divisor = 1'000'000'000u;
  static_assert(WORD_MASK > divisor);

  std::string result = "";
  auto tmp = *this;
  while (tmp > ZERO) {
    auto next = div(tmp, divisor);
    if (next.quot == ZERO)
      // Left most digits, no padding
      result.insert(0, fmt::format("{:d}", next.rem.get_word(0)));
    else
      // Zero pad the rest
      result.insert(0, fmt::format("{:09d}", next.rem.get_word(0)));
    tmp = next.quot;
  }

  return result;
}

std::uint32_t BigNum::BigUInt::to_unint32_t() const {
  if (*this > std::numeric_limits<std::uint32_t>::max())
    return std::numeric_limits<std::uint32_t>::max();
  return std::uint32_t(get_word(0));
}

std::uint64_t BigNum::BigUInt::to_unint64_t() const {
  if (*this > std::numeric_limits<std::uint64_t>::max())
    return std::numeric_limits<std::uint64_t>::max();
  std::uint64_t ret{get_word(1)};
  ret <<= WORD_BITS;
  ret |= get_word(0);
  return ret;
}

}  // namespace BigNum
