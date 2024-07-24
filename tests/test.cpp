/*******************************************************************************
 * Copyright (c) 2024, Christopher Hill <ch6574@gmail.com>
 * GNU General Public License v3.0+ (see
 * https://www.gnu.org/licenses/gpl-3.0.txt)
 * SPDX-License-Identifier: GPL-3.0-or-later
 ******************************************************************************/

#define CATCH_CONFIG_MAIN
#include <catch2/catch.hpp>

#include "big_uint.hpp"

namespace BigNum {

const auto bn_empty = BigUInt();
const auto bn_ff = BigUInt("0xFF");
const auto bn_2fs = BigUInt("0x'FFFFFFFF'FFFFFFFF");
const auto bn_big = BigUInt("0x12341234'12341234'12341234'12341234");

TEST_CASE("Basic") {
  SECTION("Equalities") {
    REQUIRE(bn_empty == BigUInt::ZERO);
    REQUIRE(BigUInt::ZERO == BigUInt::ZERO);
    REQUIRE(BigUInt::ZERO == BigUInt(0u));
    REQUIRE(BigUInt::ZERO == BigUInt("0"));
    REQUIRE(BigUInt::ZERO == BigUInt("0x0"));
    REQUIRE(BigUInt::ZERO != BigUInt::ONE);
    REQUIRE_FALSE(BigUInt::ZERO == BigUInt::ONE);
    REQUIRE(BigUInt::ONE == BigUInt(1u));
    REQUIRE(BigUInt::ONE == BigUInt("1"));
    REQUIRE(BigUInt::ONE == BigUInt("0x1"));

    // Test different constructor styles
    REQUIRE(bn_ff == BigUInt("0xFF"));
    REQUIRE(bn_ff == BigUInt("0x00000000'000000FF"));
    REQUIRE(bn_ff == BigUInt(0xFFu));
    REQUIRE(bn_ff == BigUInt(255u));
    REQUIRE(bn_ff == 0xFFu);
    REQUIRE(BigUInt(1234u) == 1234u);
    REQUIRE(1234u == BigUInt(1234u));

    // Test the same thing
    REQUIRE(bn_big == bn_big);

    // Test junk input
    REQUIRE_THROWS_WITH(BigUInt("123qwerty"),
                        Catch::Matchers::Contains("Invalid decimal string"));
    REQUIRE_THROWS_WITH(BigUInt("0xQwErTy"),
                        Catch::Matchers::Contains("Invalid hex string"));
  }

  SECTION("Compares") {
    REQUIRE(BigUInt::ZERO < BigUInt::ONE);
    REQUIRE(BigUInt::ZERO <= BigUInt::ONE);
    REQUIRE(BigUInt::ZERO <= BigUInt::ZERO);
    REQUIRE(BigUInt::ZERO <= bn_big);

    REQUIRE(BigUInt::ONE > BigUInt::ZERO);
    REQUIRE(BigUInt::ONE >= BigUInt::ZERO);
    REQUIRE(BigUInt::ONE >= BigUInt::ONE);
    REQUIRE(bn_big >= BigUInt::ONE);

    REQUIRE_FALSE(BigUInt::ZERO >= bn_big);
    REQUIRE_FALSE(bn_big <= BigUInt::ONE);
  }

  SECTION("Copies") {
    REQUIRE(bn_ff.bitsize() == 8);  // 0xff = 11111111

    // Test a copy
    auto tmp_ff1 = bn_ff;
    REQUIRE(tmp_ff1 == bn_ff);
    REQUIRE(tmp_ff1.bitsize() == 8);
    REQUIRE(bn_ff.bitsize() == 8);

    // Test a move
    auto tmp_ff2 = std::move(tmp_ff1);
    REQUIRE(tmp_ff2 == bn_ff);
    REQUIRE(tmp_ff2.bitsize() == 8);
    REQUIRE(tmp_ff1.bitsize() == 0);    // Now empty!
    REQUIRE(tmp_ff1 == BigUInt::ZERO);  // Which is the same as zero
    REQUIRE(bn_ff.bitsize() == 8);
  }
}

TEST_CASE("Math") {
  SECTION("Addition") {
    // Test adding two numbers
    REQUIRE(bn_ff + bn_ff == BigUInt(0x1FEu));
    REQUIRE(bn_2fs + BigUInt::ONE == (BigUInt(1u) <<= 64));

    // Test assigning an addition
    auto tmp = bn_ff;
    REQUIRE((tmp += bn_ff) == BigUInt(0x1FEu));
    REQUIRE((tmp += bn_big) ==
            BigUInt("0x12341234'12341234'12341234'12341432"));
    REQUIRE((tmp += bn_big) ==
            BigUInt("0x24682468'24682468'24682468'24682666"));

    // Test pre & post increments
    REQUIRE(++tmp == BigUInt("0x24682468'24682468'24682468'24682667"));
    REQUIRE(tmp++ == BigUInt("0x24682468'24682468'24682468'24682667"));
    REQUIRE(++tmp == BigUInt("0x24682468'24682468'24682468'24682669"));
  }

  SECTION("Subtraction") {
    // Test subtracting two numbers
    REQUIRE(bn_ff - bn_ff == BigUInt::ZERO);
    REQUIRE(bn_ff - BigUInt::ONE == BigUInt(0xFEu));

    // Test assigning an subtraction
    auto tmp = bn_big;
    REQUIRE((tmp -= BigUInt::ONE) ==
            BigUInt("0x12341234'12341234'12341234'12341233"));
    REQUIRE((tmp -= BigUInt("0x12341230'00000000'00000000'00000000")) ==
            BigUInt("0x4'12341234'12341234'12341233"));

    // Test pre & post decrements
    REQUIRE(--tmp == BigUInt("0x4'12341234'12341234'12341232"));
    REQUIRE(tmp-- == BigUInt("0x4'12341234'12341234'12341232"));
    REQUIRE(--tmp == BigUInt("0x4'12341234'12341234'12341230"));

    // Test a borrow carry cascading
    REQUIRE((BigUInt("0x11234123412341234") -= BigUInt("0xc000000000000000")) ==
            BigUInt("0x5234123412341234"));

    // Illogical cases
    REQUIRE_THROWS_WITH(BigUInt::ZERO - BigUInt::ONE,
                        Catch::Matchers::Contains("Result would be negative"));
  }

  SECTION("Multiplication") {
    // Test multiplying two numbers
    REQUIRE(bn_ff * 2u == BigUInt(0x1FEu));
    REQUIRE(bn_big * 2u == BigUInt("0x24682468'24682468'24682468'24682468"));

    // Test assigning a multiplication
    auto tmp = bn_big;
    REQUIRE((tmp *= 2u) == BigUInt("0x24682468'24682468'24682468'24682468"));
    REQUIRE((tmp *= 1u) == BigUInt("0x24682468'24682468'24682468'24682468"));
    REQUIRE((tmp *= 0u) == BigUInt::ZERO);
  }

  SECTION("Division") {
    // Test dividing two numbers
    REQUIRE(bn_ff / 1u == bn_ff);
    REQUIRE(bn_ff / 2u == BigUInt(0x7Fu));
    REQUIRE(bn_ff / bn_ff == BigUInt::ONE);
    REQUIRE(bn_big / 2u == BigUInt("0x91a091a091a091a091a091a091a091a"));
    REQUIRE(bn_big / 3u == BigUInt("0x6115b66b0bc06115b66b0bc06115b66"));
    REQUIRE(bn_big / bn_big == BigUInt::ONE);
    REQUIRE(3u / bn_big == BigUInt::ZERO);

    // Test "divmod"
    auto tmp1 = BigUInt::div(bn_big, 3u);
    REQUIRE(tmp1.quot == BigUInt("0x6115b66b0bc06115b66b0bc06115b66"));
    REQUIRE(tmp1.rem == BigUInt("0x2"));

    auto tmp2 = BigUInt::div(3u, bn_big);
    REQUIRE(tmp2.quot == BigUInt::ZERO);
    REQUIRE(tmp2.rem == 3u);

    auto tmp3 = BigUInt::div(bn_big, bn_big);
    REQUIRE(tmp3.quot == BigUInt::ONE);
    REQUIRE(tmp3.rem == BigUInt::ZERO);

    // Test assigning a division / mod
    REQUIRE((BigUInt(0x2222u) /= 2u) == BigUInt(0x1111u));
    REQUIRE((BigUInt(0x2222u) %= 2u) == BigUInt::ZERO);

    REQUIRE((BigUInt(0x2223u) /= 2u) == BigUInt(0x1111u));
    REQUIRE((BigUInt(0x2223u) %= 2u) == BigUInt::ONE);

    // Illogical cases
    REQUIRE_THROWS_WITH(bn_ff / BigUInt::ZERO,
                        Catch::Matchers::Contains("Divide by zero"));
  }

  SECTION("Power") {
    REQUIRE(BigUInt::pow(bn_ff, 2u) == 0xFE01u);     // 255 ** 2
    REQUIRE(BigUInt::pow(bn_ff, 2u, 0xAu) == 0x5u);  // 255 ** 2 % 10

    // Test 0xff ** 20
    REQUIRE(BigUInt::pow(bn_ff, 20u) ==
            BigUInt("0xecb99eb1063b1984b725d2e3c72b82e88cbdec01"));

    // 0xDEADBEEF ** 65537 % 5551201688147
    REQUIRE(BigUInt::pow(BigUInt("0xDEADBEEF"), 65537u, 5551201688147u) ==
            BigUInt("0x3636679bea3"));

    // 0x3636679bea3 ** 109182490673 % 5551201688147
    REQUIRE(BigUInt::pow(BigUInt("0x3636679bea3"), 109182490673u,
                         5551201688147u) == BigUInt("0xDEADBEEF"));
  }

  SECTION("Roots") {
    REQUIRE(BigUInt::isqrt(BigUInt::ZERO) == BigUInt::ZERO);
    REQUIRE(BigUInt::isqrt(5u) == BigUInt::TWO);
    REQUIRE(BigUInt::isqrt(bn_big) == BigUInt("0x4443c4434c41f33d"));
  }

  SECTION("Log") {
    REQUIRE_THROWS_WITH(BigUInt::log2(BigUInt::ZERO),
                        Catch::Matchers::Contains("Log of 0"));
    REQUIRE(BigUInt::log2(BigUInt::ONE) == 0u);
    REQUIRE(BigUInt::log2(bn_ff) == 7u);     // log2(255) == 7.994...
    REQUIRE(BigUInt::log2(bn_big) == 124u);  // really 124.186...

    // Log10 approximations
    REQUIRE(BigUInt::log2(bn_ff) / BigUInt::log2(10u) ==
            2u);  // log10(255) == 2.407...
  }

  SECTION("Bit Operations") {
    // Test sizings
    REQUIRE(BigUInt::ZERO.bitsize() == 0);
    REQUIRE(BigUInt::ONE.bitsize() == 1);
    REQUIRE(bn_ff.bitsize() == 8);

    // NOT
    REQUIRE(~BigUInt("0x0") == BigUInt::ONE);
    REQUIRE(~BigUInt("0x1") == BigUInt::ZERO);
    REQUIRE(~BigUInt("0xff") == BigUInt::ZERO);
    REQUIRE(~BigUInt("0xffffffff'ffffffff") == BigUInt::ZERO);
    REQUIRE(~BigUInt("0xffffff00'ffffffff") == BigUInt("0xff'00000000"));
    REQUIRE(~BigUInt("0x00ffff00'ffffffff") == BigUInt("0xff'00000000"));

    // ANDING / ORING / XORING
    auto tmp1 = bn_ff;
    REQUIRE((tmp1 &= BigUInt::ONE) == BigUInt::ONE);
    REQUIRE((bn_ff & BigUInt::ONE) == BigUInt::ONE);

    // 1010'1010 & 1111 == 1010
    REQUIRE((BigUInt(0xAAu) & BigUInt(0xFu)) == BigUInt(0xAu));
    REQUIRE((bn_big & BigUInt("0xFFFF0000'00000000")) ==
            BigUInt("0x12340000'00000000"));

    // 1010'1010 | 0101'0101 == 1111'1111
    REQUIRE((BigUInt(0xAAu) | BigUInt(0x55u)) == bn_ff);
    REQUIRE((bn_big | BigUInt("0xFFFF0000'00000000")) ==
            BigUInt("0x12341234'12341234'FFFF1234'12341234"));

    // 1111'1010 ^ 0101'0101 == 1010'1111
    REQUIRE((BigUInt(0xFAu) ^ BigUInt(0x55u)) == BigUInt(0xAFu));
    // 1111'1111 ^ 0101'0101 == 1010'1010
    auto tmp2 = bn_ff;
    REQUIRE((tmp2 ^= BigUInt(0x55u)) == BigUInt(0xAAu));

    // SHIFTING
    auto tmp3 = bn_ff;
    REQUIRE((tmp3 <<= 1) == BigUInt(0xFFu << 1));
    REQUIRE((tmp3 <<= 1) == BigUInt(0xFFu << 2));
    // Test crossing a "word" boundary
    //   0b01111111'10000000'00000000'00000000'00000000
    REQUIRE((tmp3 <<= 29) == BigUInt(0xFFul << 31));
    //   0b11111111'00000000'00000000'00000000'00000000
    REQUIRE((tmp3 <<= 1) == BigUInt(0xFFul << 32));
    // 0b1'11111110'00000000'00000000'00000000'00000000
    REQUIRE((tmp3 <<= 1) == BigUInt(0xFFul << 33));
    REQUIRE((tmp3 >>= 33) == bn_ff);  // Shift it all the way back again

    // Left shift by a thousand, and then back again
    auto tmp4 = BigUInt::ONE;
    REQUIRE((tmp4 <<= 1000).bitsize() == 1001);
    REQUIRE((tmp4 >>= 1000) == BigUInt::ONE);

    // Left shift way beyond current bits
    REQUIRE((bn_ff >> 1000) == BigUInt::ZERO);

    // Shift a const
    REQUIRE((bn_ff << 1) == 0x1FEu);
    REQUIRE((bn_ff >> 1) == 0x7Fu);
  }
}

TEST_CASE("Collections") {
  SECTION("Set") {
    // Test set membership by inserting 2 numbers
    std::set<BigUInt> tmp;
    tmp.insert(BigUInt::ZERO);
    tmp.insert(BigUInt::ONE);
    REQUIRE(tmp.size() == 2);

    // Insert 5 more, 4 of them duplicates
    tmp.insert(BigUInt::ZERO);
    tmp.insert(BigUInt::ONE);
    tmp.insert(bn_ff);
    tmp.insert(BigUInt::ZERO);
    tmp.insert(BigUInt::ONE);
    REQUIRE(tmp.size() == 3);  // i.e. no duplicates!

    REQUIRE(tmp.contains(bn_ff));
    REQUIRE_FALSE(tmp.contains(BigUInt(1234u)));
  }

  SECTION("Map") {
    std::map<std::string, BigUInt> tmp;
    tmp.insert(std::make_pair("zero", BigUInt::ZERO));
    tmp.insert(std::make_pair("one", BigUInt::ONE));
    tmp.insert(std::make_pair("one-again", BigUInt::ONE));
    tmp.insert(std::make_pair("ff", bn_ff));
    REQUIRE(tmp.size() == 4);
    REQUIRE(tmp.contains("ff"));
    REQUIRE(tmp.at("ff") == bn_ff);
    REQUIRE(tmp.at("one") == tmp.at("one-again"));
    REQUIRE_FALSE(tmp.contains("0x0"));  // never added this key

    tmp.clear();
    tmp["zero"] = BigUInt::ZERO;
    tmp["one"] = BigUInt::ONE;
    tmp["one-again"] = BigUInt::ONE;
    tmp["ff"] = bn_ff;
    REQUIRE(tmp.size() == 4);
    REQUIRE(tmp.contains("ff"));
    REQUIRE(tmp["ff"] == bn_ff);
    REQUIRE(tmp["one"] == tmp["one-again"]);
    REQUIRE_FALSE(tmp.contains("0x0"));  // never added this key
  }
}

BigUInt factorial(unsigned n) {
  auto res = BigUInt(n);
  while (--n > 1) {
    res = res * BigUInt(n);
  }
  return res;
}

TEST_CASE("Factorial") {
  // Testing 5!
  REQUIRE(factorial(5) == BigUInt("0x78"));

  // python3 -c "import math; print(hex(math.factorial(100)))"
  // 0x1b30964ec395dc24069528d54bbda40d16e966ef9a70eb21b5b2943a321cdf10391745570cca9420c6ecb3b72ed2ee8b02ea2735c61a000000000000000000000000
  REQUIRE(factorial(100) ==
          BigUInt("0x1b30964ec395dc24069528d54bbda40d16e966ef9a70eb21b5b2943a32"
                  "1cdf10391745570cca9420c6ecb3b72ed2ee8b02ea2735c61a0000000000"
                  "00000000000000"));
}

TEST_CASE("Output") {
  const auto bn_hex = BigUInt("0x12341234123412341234123412341234");
  const auto bn_dec = BigUInt("24196472569643293503308547655940182580");

  SECTION("Hex") {
    // Test streaming out various numbers as hex strings
    std::stringstream out;
    out << bn_ff;
    REQUIRE(out.str() == "0xff");

    out.str("");
    out << bn_hex;
    REQUIRE(out.str() == "0x12341234123412341234123412341234");

    out.str("");
    out << bn_dec;
    REQUIRE(out.str() == "0x12341234123412341234123412341234");
  }

  SECTION("Binary") {
    // Test some binary output
    REQUIRE(bn_ff.to_binary() == "0b11111111");
    REQUIRE(bn_hex.to_binary() ==
            "0b1001000110100000100100011010000010010001101000001001000110100000"
            "1001000110100000100100011010000010010001101000001001000110100");
  }

  SECTION("Decimal") {
    // Test generating various numbers as decimal strings
    REQUIRE(bn_ff.to_decimal() == "255");
    REQUIRE(bn_hex.to_decimal() == "24196472569643293503308547655940182580");
    REQUIRE(bn_hex == bn_dec);

    // factorial(100)
    auto tmp = BigUInt(
        "0x1b30964ec395dc24069528d54bbda40d16e966ef9a70eb21b5b2943a32"
        "1cdf10391745570cca9420c6ecb3b72ed2ee8b02ea2735c61a0000000000"
        "00000000000000");
    REQUIRE(tmp.to_decimal() ==
            "933262154439441526816992388562667004907159682643816214685929638952"
            "175999932299156089414639761565182862536979208272237582511852109168"
            "64000000000000000000000000");
  }

  SECTION("Fixed width") {
    REQUIRE(BigUInt::ZERO.to_unint32_t() == 0x0);
    REQUIRE(BigUInt::ONE.to_unint32_t() == 0x1);
    REQUIRE(BigUInt::TWO.to_unint32_t() == 0x2);

    REQUIRE(BigUInt::ZERO.to_unint64_t() == 0x0);
    REQUIRE(BigUInt::ONE.to_unint64_t() == 0x1);
    REQUIRE(BigUInt::TWO.to_unint64_t() == 0x2);

    REQUIRE(bn_ff.to_unint32_t() == 0xffu);
    REQUIRE(bn_ff.to_unint64_t() == 0xffu);

    REQUIRE(bn_2fs.to_unint32_t() == 0xFFFFFFFFu);  // Truncated
    REQUIRE(bn_2fs.to_unint64_t() == 0xFFFFFFFF'FFFFFFFFu);

    REQUIRE(bn_big.to_unint32_t() == 0xFFFFFFFFu);           // Truncated
    REQUIRE(bn_big.to_unint64_t() == 0xFFFFFFFF'FFFFFFFFu);  // Truncated too
  }
}

}  // namespace BigNum
