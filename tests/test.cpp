/*******************************************************************************
 * Copyright (c) 2024, Christopher Hill <ch6574@gmail.com>
 * GNU General Public License v3.0+ (see
 * https://www.gnu.org/licenses/gpl-3.0.txt)
 * SPDX-License-Identifier: GPL-3.0-or-later
 ******************************************************************************/

#define CATCH_CONFIG_MAIN
#include <catch2/catch_all.hpp>
#include <catch2/matchers/catch_matchers.hpp>

#include "big_int.hpp"

namespace BigNum {

// Constants for testing with
const auto bn_empty = BigInt();
const auto bn_ff = BigInt("0xFF");
const auto bn_2fs = BigInt("0x'FFFFFFFF'FFFFFFFF");
const auto bn_big = BigInt("0x12341234'12341234'12341234'12341234");

TEST_CASE("Basic") {
  SECTION("Equalities") {
    REQUIRE(bn_empty == BigInt::ZERO);
    REQUIRE(BigInt::ZERO == BigInt::ZERO);
    REQUIRE(BigInt::ZERO == BigInt(0u));
    REQUIRE(BigInt::ZERO == BigInt(0));
    REQUIRE(BigInt::ZERO == BigInt("0"));
    REQUIRE(BigInt::ZERO == BigInt("0x0"));
    REQUIRE(BigInt::ZERO == BigInt(0.0f));
    REQUIRE(BigInt::ZERO == BigInt(0.0));
    REQUIRE(BigInt::ZERO != BigInt::ONE);
    REQUIRE_FALSE(BigInt::ZERO == BigInt::ONE);
    REQUIRE(BigInt::ONE == BigInt(1u));
    REQUIRE(BigInt::ONE == BigInt(1));
    REQUIRE(BigInt::ONE == BigInt("1"));
    REQUIRE(BigInt::ONE == BigInt("0x1"));
    REQUIRE(BigInt::ONE == BigInt(1.0f));
    REQUIRE(BigInt::ONE == BigInt(1.0));

    REQUIRE_FALSE(BigInt::ONE == BigInt::NEG_ONE);
    REQUIRE(BigInt::NEG_ONE == BigInt::NEG_ONE);
    REQUIRE(BigInt::NEG_ONE == BigInt(-1));
    REQUIRE(BigInt::NEG_ONE == BigInt(-0x1));
    REQUIRE(BigInt::NEG_ONE == BigInt("-1"));
    REQUIRE(BigInt::NEG_ONE == BigInt("-0x1"));
    REQUIRE(BigInt::NEG_ONE == BigInt(-1.0f));
    REQUIRE(BigInt::NEG_ONE == BigInt(-1.0));

    // Test different constructor styles
    REQUIRE(bn_ff == BigInt("0xFF"));
    REQUIRE(bn_ff == BigInt("0x00000000'000000FF"));
    REQUIRE(bn_ff == BigInt(0xFFu));
    REQUIRE(bn_ff == BigInt(255u));
    REQUIRE(bn_ff == 0xFFu);
    REQUIRE(BigInt(1234u) == 1234u);
    REQUIRE(1234u == BigInt(1234u));

    // Test the same thing
    REQUIRE(bn_big == bn_big);

    // Test junk input
    REQUIRE_THROWS_WITH(BigInt("123qwerty"),
                        "Invalid decimal string: 123qwerty");
    REQUIRE_THROWS_WITH(BigInt("0xQwErTy"), "Invalid hex string: QwErTy");
  }

  SECTION("Compares") {
    REQUIRE(BigInt::ZERO < BigInt::ONE);
    REQUIRE(BigInt::ZERO <= BigInt::ONE);
    REQUIRE(BigInt::ZERO <= BigInt::ZERO);
    REQUIRE(BigInt::ZERO <= bn_big);

    REQUIRE(BigInt::ONE > BigInt::ZERO);
    REQUIRE(BigInt::ONE >= BigInt::ZERO);
    REQUIRE(BigInt::ONE >= BigInt::ONE);
    REQUIRE(bn_big >= BigInt::ONE);

    REQUIRE_FALSE(BigInt::ZERO >= bn_big);
    REQUIRE_FALSE(bn_big <= BigInt::ONE);

    REQUIRE(BigInt::NEG_ONE < BigInt::ONE);
    REQUIRE(BigInt::NEG_ONE <= BigInt::ONE);
    REQUIRE(BigInt::ONE > BigInt::NEG_TWO);
    REQUIRE(BigInt::ZERO > BigInt::NEG_TWO);

    REQUIRE(bn_big != -bn_big);
    REQUIRE(bn_big > -bn_big);
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
    REQUIRE(tmp_ff1.bitsize() == 0);   // Now empty!
    REQUIRE(tmp_ff1 == BigInt::ZERO);  // Which is the same as zero
    REQUIRE(bn_ff.bitsize() == 8);
  }

  SECTION("Floats & Doubles") {
    REQUIRE(BigInt(-1.9f) == BigInt::NEG_ONE);  // Note rounding direction
    REQUIRE(BigInt(-1.0f) == BigInt::NEG_ONE);
    REQUIRE(BigInt(0.0f) == BigInt::ZERO);
    REQUIRE(BigInt(1.0f) == BigInt::ONE);
    REQUIRE(BigInt(1.9f) == BigInt::ONE);
    REQUIRE(BigInt(3.142f) == BigInt(3u));
    REQUIRE(BigInt(16777216.0f) == BigInt("16777216"));
    REQUIRE(BigInt(16777217.0f) == BigInt("16777216"));  // Note rounding
    REQUIRE(BigInt(16777218.0f) == BigInt("16777218"));

    REQUIRE(BigInt(-1.0f).to_float() == -1.0f);
    REQUIRE(BigInt(0.0f).to_float() == 0.0f);
    REQUIRE(BigInt(1.0f).to_float() == 1.0f);
    REQUIRE(BigInt(3.142f).to_float() == 3.0f);
    // FLT_MAX +1
    REQUIRE(BigInt("340282346638528859811704183484516925441").to_float() ==
            std::numeric_limits<float>::max());

    REQUIRE(BigInt(-1.9) == BigInt::NEG_ONE);
    REQUIRE(BigInt(-1.0) == BigInt::NEG_ONE);
    REQUIRE(BigInt(0.0) == BigInt::ZERO);
    REQUIRE(BigInt(1.0) == BigInt::ONE);
    REQUIRE(BigInt(1.9) == BigInt::ONE);
    REQUIRE(BigInt(3.142) == BigInt(3u));
    REQUIRE(BigInt(9007199254740992.0) == BigInt("9007199254740992"));
    REQUIRE(BigInt(9007199254740993.0) == BigInt("9007199254740992"));
    REQUIRE(BigInt(9007199254740994.0) == BigInt("9007199254740994"));

    REQUIRE(BigInt(-1.0f).to_double() == -1.0);
    REQUIRE(BigInt(0.0f).to_double() == 0.0);
    REQUIRE(BigInt(1.0f).to_double() == 1.0);
    REQUIRE(BigInt(3.142f).to_double() == 3.0);
    // FLT_MAX +1
    REQUIRE_THAT(BigInt("340282346638528859811704183484516925441").to_double(),
                 Catch::Matchers::WithinRel(
                     340282346638528859811704183484516925441.0, 0.0000001));
  }
}

TEST_CASE("Math") {
  SECTION("Addition") {
    // Test adding two numbers
    REQUIRE(bn_ff + bn_ff == BigInt(0x1FEu));
    REQUIRE(bn_2fs + BigInt::ONE == (BigInt(1u) <<= 64));

    // Test assigning an addition
    auto tmp = bn_ff;
    REQUIRE((tmp += bn_ff) == BigInt(0x1FEu));
    REQUIRE((tmp += bn_big) == BigInt("0x12341234'12341234'12341234'12341432"));
    REQUIRE((tmp += bn_big) == BigInt("0x24682468'24682468'24682468'24682666"));

    // Test pre & post increments
    REQUIRE(++tmp == BigInt("0x24682468'24682468'24682468'24682667"));
    REQUIRE(tmp++ == BigInt("0x24682468'24682468'24682468'24682667"));
    REQUIRE(++tmp == BigInt("0x24682468'24682468'24682468'24682669"));

    // Negative cases
    REQUIRE(BigInt::ONE + BigInt::NEG_ONE == BigInt::ZERO);
  }

  SECTION("Subtraction") {
    // Test subtracting two numbers
    REQUIRE(bn_ff - bn_ff == BigInt::ZERO);
    REQUIRE(bn_ff - BigInt::ONE == BigInt(0xFEu));

    // Test assigning a subtraction
    auto tmp = bn_big;
    REQUIRE((tmp -= BigInt::ONE) ==
            BigInt("0x12341234'12341234'12341234'12341233"));
    REQUIRE((tmp -= BigInt("0x12341230'00000000'00000000'00000000")) ==
            BigInt("0x4'12341234'12341234'12341233"));

    // Test pre & post decrements
    REQUIRE(--tmp == BigInt("0x4'12341234'12341234'12341232"));
    REQUIRE(tmp-- == BigInt("0x4'12341234'12341234'12341232"));
    REQUIRE(--tmp == BigInt("0x4'12341234'12341234'12341230"));

    // Test a borrow carry cascading
    REQUIRE((BigInt("0x11234123412341234") -= BigInt("0xc000000000000000")) ==
            BigInt("0x5234123412341234"));

    // Negative cases
    REQUIRE(BigInt::ZERO - BigInt::ONE == BigInt::NEG_ONE);
  }

  SECTION("Negation") {
    // Flip sign
    auto tmp = BigInt::ONE;
    REQUIRE(-tmp == BigInt::NEG_ONE);
  }

  SECTION("Multiplication") {
    // Test multiplying two numbers
    REQUIRE(bn_ff * 2u == BigInt(0x1FEu));
    REQUIRE(bn_ff * 10u == BigInt(0x9F6u));
    REQUIRE(bn_big * 2u == BigInt("0x24682468'24682468'24682468'24682468"));
    REQUIRE(bn_big * 10u == BigInt("0xB608B608'B608B608'B608B608'B608B608"));

    // Test assigning a multiplication
    auto tmp = bn_big;
    REQUIRE((tmp *= 2u) == BigInt("0x24682468'24682468'24682468'24682468"));
    REQUIRE((tmp *= 1u) == BigInt("0x24682468'24682468'24682468'24682468"));
    REQUIRE((tmp *= 0u) == BigInt::ZERO);

    // Negative cases
    REQUIRE(BigInt::NEG_ONE * BigInt::ONE == BigInt::NEG_ONE);
    REQUIRE(BigInt::NEG_ONE * BigInt::NEG_ONE == BigInt::ONE);
    REQUIRE(bn_ff * BigInt::ONE == bn_ff);
    REQUIRE(bn_ff * BigInt::NEG_ONE == BigInt(-0xFF));
  }

  SECTION("Division") {
    // Test dividing two numbers
    REQUIRE(bn_ff / 1u == bn_ff);
    REQUIRE(bn_ff / 2u == BigInt(0x7Fu));
    REQUIRE(bn_ff / bn_ff == BigInt::ONE);
    REQUIRE(bn_big / 2u == BigInt("0x91A091A091A091A091A091A091A091A"));
    REQUIRE(bn_big / 3u == BigInt("0x6115B66B0BC06115B66B0BC06115B66"));
    REQUIRE(bn_big / bn_big == BigInt::ONE);
    REQUIRE(3u / bn_big == BigInt::ZERO);

    // Test "divmod"
    auto tmp1 = BigInt::div(bn_big, 3u);
    REQUIRE(tmp1.quot == BigInt("0x6115B66B0BC06115B66B0BC06115B66"));
    REQUIRE(tmp1.rem == BigInt::TWO);

    auto tmp2 = BigInt::div(3u, bn_big);
    REQUIRE(tmp2.quot == BigInt::ZERO);
    REQUIRE(tmp2.rem == 3u);

    auto tmp3 = BigInt::div(bn_big, bn_big);
    REQUIRE(tmp3.quot == BigInt::ONE);
    REQUIRE(tmp3.rem == BigInt::ZERO);

    // Test assigning a division / mod
    REQUIRE((BigInt(0x2222u) /= 2u) == BigInt(0x1111u));
    REQUIRE((BigInt(0x2222u) %= 2u) == BigInt::ZERO);

    REQUIRE((BigInt(0x2223u) /= 2u) == BigInt(0x1111u));
    REQUIRE((BigInt(0x2223u) %= 2u) == BigInt::ONE);

    // Negative cases
    REQUIRE(BigInt::NEG_ONE / BigInt::ONE == BigInt::NEG_ONE);
    REQUIRE(BigInt::NEG_ONE / BigInt::NEG_ONE == BigInt::ONE);

    REQUIRE(bn_ff / BigInt::NEG_ONE == -bn_ff);
    REQUIRE(bn_ff / BigInt::NEG_TWO == -BigInt(0x7Fu));
    REQUIRE(bn_ff / -bn_ff == BigInt::NEG_ONE);

    auto tmp4 = BigInt::div(BigInt(7), BigInt(5));
    REQUIRE(tmp4.quot == BigInt::ONE);
    REQUIRE(tmp4.rem == BigInt::TWO);

    auto tmp5 = BigInt::div(BigInt(-7), BigInt(5));
    REQUIRE(tmp5.quot == BigInt::NEG_ONE);
    REQUIRE(tmp5.rem == BigInt::NEG_TWO);

    auto tmp6 = BigInt::div(BigInt(7), BigInt(-5));
    REQUIRE(tmp6.quot == BigInt::NEG_ONE);
    REQUIRE(tmp6.rem == BigInt::TWO);

    auto tmp7 = BigInt::div(BigInt(-7), BigInt(-5));
    REQUIRE(tmp7.quot == BigInt::ONE);
    REQUIRE(tmp7.rem == BigInt::NEG_TWO);

    // Illogical cases
    REQUIRE_THROWS_WITH(bn_ff / BigInt::ZERO, "Divide by zero!");
  }

  SECTION("Power") {
    REQUIRE(BigInt::pow(bn_ff, 2u) == 0xFE01u);            // 255 ** 2
    REQUIRE(BigInt::pow(bn_ff, 2u, 0xAu) == 0x5u);         // 255 ** 2 % 10
    REQUIRE(BigInt::pow(bn_ff, 0xAu, 2u) == BigInt::ONE);  // 255 ** 10 % 2

    // Test 0xff ** 20
    REQUIRE(BigInt::pow(bn_ff, 20u) ==
            BigInt("0xECB99EB1063B1984B725D2E3C72B82E88CBDEC01"));

    // RSA style encrypt: message ** public key exp % public key mod
    // 0xDEADBEEF ** 65537 % 5551201688147
    REQUIRE(BigInt::pow(BigInt(0xDEADBEEF), 65537u, 5551201688147u) ==
            BigInt(0x3636679bea3));

    // RSA style decrypt: cypher ** private key % public key mod
    // 0x3636679bea3 ** 109182490673 % 5551201688147
    REQUIRE(BigInt::pow(BigInt(0x3636679bea3), 109182490673u, 5551201688147u) ==
            BigInt(0xDEADBEEF));

    // Negative tests
    REQUIRE(BigInt::pow(bn_ff, -3) == BigInt::ZERO);       //  255 ** -3
    REQUIRE(BigInt::pow(-bn_ff, -1) == BigInt::ZERO);      // -255 ** -1
    REQUIRE(BigInt::pow(-bn_ff, 1) == -bn_ff);             // -255 **  1
    REQUIRE(BigInt::pow(-bn_ff, 2) == BigInt(65025));      // -255 **  2
    REQUIRE(BigInt::pow(-bn_ff, 3) == BigInt(-16581375));  // -255 **  3
    REQUIRE(BigInt::pow(-bn_ff, 3, 6) == BigInt(-3));      // -255 **  3 % 6
  }

  SECTION("Roots") {
    REQUIRE_THROWS_WITH(BigInt::isqrt(BigInt::NEG_ONE),
                        "Cannot caclulate negative roots!");
    REQUIRE(BigInt::isqrt(BigInt::ZERO) == BigInt::ZERO);
    REQUIRE(BigInt::isqrt(5u) == BigInt::TWO);
    REQUIRE(BigInt::isqrt(bn_big) == BigInt("0x4443C4434C41F33D"));
  }

  SECTION("Greatest Common Divisor") {
    REQUIRE(BigInt::gcd(0, 0) == 0);
    REQUIRE(BigInt::gcd(24, 0) == 24);
    REQUIRE(BigInt::gcd(24, 1) == 1);
    REQUIRE(BigInt::gcd(24, 60) == 12);
    REQUIRE(BigInt::gcd(-24, 60) == 12);
    REQUIRE(BigInt::gcd(24, -60) == 12);
    REQUIRE(BigInt::gcd(-24, -60) == 12);
    REQUIRE(BigInt::gcd(bn_big, bn_ff) == 5);
  }

  SECTION("Log") {
    REQUIRE_THROWS_WITH(BigInt::log2(BigInt::NEG_ONE),
                        "Cannot caclulate negative logs!");
    REQUIRE_THROWS_WITH(BigInt::log2(BigInt::ZERO), "Log of 0!");
    REQUIRE(BigInt::log2(BigInt::ONE) == 0u);
    REQUIRE(BigInt::log2(bn_ff) == 7u);     // log2(255) == 7.994...
    REQUIRE(BigInt::log2(bn_big) == 124u);  // really 124.186...

    // Log10 approximations
    REQUIRE(BigInt::log2(bn_ff) / BigInt::log2(10u) ==
            2u);  // log10(255) == 2.407...
  }

  SECTION("Bit Operations") {
    // Test sizings
    REQUIRE(BigInt::ZERO.bitsize() == 0);
    REQUIRE(BigInt::ONE.bitsize() == 1);
    REQUIRE(BigInt::NEG_ONE.bitsize() == 1);
    REQUIRE(BigInt::TWO.bitsize() == 2);
    REQUIRE(bn_ff.bitsize() == 8);
    REQUIRE(bn_big.bitsize() == 125);

    REQUIRE(BigInt::ZERO.popcount() == 0);
    REQUIRE(BigInt::ONE.popcount() == 1);
    REQUIRE(BigInt::NEG_ONE.popcount() == 1);
    REQUIRE(BigInt::TWO.popcount() == 1);
    REQUIRE(bn_ff.popcount() == 8);
    REQUIRE(bn_big.popcount() == 40);

    // NOT
    REQUIRE(~BigInt(0x0) == BigInt::NEG_ONE);
    REQUIRE(~BigInt(0x1) == BigInt::NEG_TWO);
    REQUIRE(~BigInt(0xff) == BigInt(-0x100));
    REQUIRE(~BigInt("0xffffffff'ffffffff") == BigInt("-0x10000000000000000"));
    REQUIRE(~BigInt("0xffffff00'ffffffff") == BigInt("-0xffffff0100000000"));
    REQUIRE(~BigInt("0x00ffff00'ffffffff") == BigInt("-0xffff0100000000"));

    // ANDING / ORING / XORING
    auto tmp1 = bn_ff;
    REQUIRE((tmp1 &= BigInt::ONE) == BigInt::ONE);
    REQUIRE((bn_ff & BigInt::ONE) == BigInt::ONE);

    // 1010'1010 & 1111 == 1010
    REQUIRE((BigInt(0xAA) & BigInt(0xF)) == BigInt(0xAu));
    REQUIRE((BigInt(0xAAu) & BigInt(0xFu)) == BigInt(0xAu));
    REQUIRE((bn_big & BigInt("0xFFFF0000'00000000")) ==
            BigInt("0x12340000'00000000"));

    // 1010'1010 | 0101'0101 == 1111'1111
    REQUIRE((BigInt(0xAA) | BigInt(0x55)) == bn_ff);
    REQUIRE((BigInt(0xAAu) | BigInt(0x55u)) == bn_ff);
    REQUIRE((bn_big | BigInt("0xFFFF0000'00000000")) ==
            BigInt("0x12341234'12341234'FFFF1234'12341234"));

    // 1111'1010 ^ 0101'0101 == 1010'1111
    REQUIRE((BigInt(0xFA) ^ BigInt(0x55)) == BigInt(0xAFu));
    REQUIRE((BigInt(0xFAu) ^ BigInt(0x55u)) == BigInt(0xAFu));
    // 1111'1111 ^ 0101'0101 == 1010'1010
    auto tmp2 = bn_ff;
    REQUIRE((tmp2 ^= BigInt(0x55u)) == BigInt(0xAAu));

    // SHIFTING
    auto tmp3 = bn_ff;
    REQUIRE((tmp3 <<= 1) == BigInt(0xFFu << 1));
    REQUIRE((tmp3 <<= 1) == BigInt(0xFFu << 2));
    // Test crossing a "word" boundary
    //   0b01111111'10000000'00000000'00000000'00000000
    REQUIRE((tmp3 <<= 29) == BigInt(0xFFul << 31));
    //   0b11111111'00000000'00000000'00000000'00000000
    REQUIRE((tmp3 <<= 1) == BigInt(0xFFul << 32));
    // 0b1'11111110'00000000'00000000'00000000'00000000
    REQUIRE((tmp3 <<= 1) == BigInt(0xFFul << 33));
    REQUIRE((tmp3 >>= 33) == bn_ff);  // Shift it all the way back again

    // Left shift by a thousand, and then back again
    auto tmp4 = BigInt::ONE;
    REQUIRE((tmp4 <<= 1000).bitsize() == 1001);
    REQUIRE((tmp4 >>= 1000) == BigInt::ONE);

    // Left shift way beyond current bits, i.e capped at 0
    REQUIRE((bn_ff >> 1000) == BigInt::ZERO);

    // Shift a const
    REQUIRE((bn_ff << 1) == 0x1FEu);
    REQUIRE((bn_ff >> 1) == 0x7Fu);
  }
}

TEST_CASE("Collections") {
  SECTION("Set") {
    // Test set membership by inserting 2 numbers
    std::set<BigInt> tmp;
    tmp.insert(BigInt::ZERO);
    tmp.insert(BigInt::ONE);
    REQUIRE(tmp.size() == 2);

    // Insert 5 more, 4 of them duplicates
    tmp.insert(BigInt::ZERO);
    tmp.insert(BigInt::ONE);
    tmp.insert(bn_ff);
    tmp.insert(BigInt::ZERO);
    tmp.insert(BigInt::ONE);
    REQUIRE(tmp.size() == 3);  // i.e. no duplicates!

    // Negatives
    tmp.insert(BigInt::NEG_ONE);
    tmp.insert(BigInt::NEG_TWO);
    REQUIRE(tmp.size() == 5);

    REQUIRE(tmp.contains(bn_ff));
    REQUIRE_FALSE(tmp.contains(BigInt(1234u)));
  }

  SECTION("Map") {
    std::map<std::string, BigInt> tmp;
    tmp.insert(std::make_pair("zero", BigInt::ZERO));
    tmp.insert(std::make_pair("one", BigInt::ONE));
    tmp.insert(std::make_pair("one-again", BigInt::ONE));
    tmp.insert(std::make_pair("ff", bn_ff));
    REQUIRE(tmp.size() == 4);
    REQUIRE(tmp.contains("ff"));
    REQUIRE(tmp.at("ff") == bn_ff);
    REQUIRE(tmp.at("one") == tmp.at("one-again"));
    REQUIRE_FALSE(tmp.contains("0x0"));  // never added this key

    tmp.clear();
    tmp["zero"] = BigInt::ZERO;
    tmp["one"] = BigInt::ONE;
    tmp["one-again"] = BigInt::ONE;
    tmp["ff"] = bn_ff;
    REQUIRE(tmp.size() == 4);
    REQUIRE(tmp.contains("ff"));
    REQUIRE(tmp["ff"] == bn_ff);
    REQUIRE(tmp["one"] == tmp["one-again"]);
    REQUIRE_FALSE(tmp.contains("0x0"));  // never added this key
  }
}

BigInt factorial(unsigned n) {
  auto res = BigInt(n);
  while (--n > BigInt::ONE) {
    res *= BigInt(n);
  }
  return res;
}

TEST_CASE("Factorial") {
  // Testing 5!
  REQUIRE(factorial(5) == BigInt(0x78));

  // python3 -c "import math; print(hex(math.factorial(100)))"
  // 0x1b30964ec395dc24069528d54bbda40d16e966ef9a70eb21b5b2943a321cdf10391745570cca9420c6ecb3b72ed2ee8b02ea2735c61a000000000000000000000000
  REQUIRE(factorial(100) ==
          BigInt("0x1b30964ec395dc24069528d54bbda40d16e966ef9a70eb21b5b2943a32"
                 "1cdf10391745570cca9420c6ecb3b72ed2ee8b02ea2735c61a0000000000"
                 "00000000000000"));
}

TEST_CASE("Output") {
  const auto bn_hex = BigInt("0x12341234123412341234123412341234");
  const auto bn_dec = BigInt("24196472569643293503308547655940182580");

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

    REQUIRE(BigInt::NEG_ONE.to_decimal() == "-1");
    REQUIRE(bn_big.to_decimal() == "24196472569643293503308547655940182580");
    REQUIRE((-bn_big).to_decimal() ==
            "-24196472569643293503308547655940182580");

    // factorial(100)
    auto tmp = BigInt(
        "0x1b30964ec395dc24069528d54bbda40d16e966ef9a70eb21b5b2943a32"
        "1cdf10391745570cca9420c6ecb3b72ed2ee8b02ea2735c61a0000000000"
        "00000000000000");
    REQUIRE(tmp.to_decimal() ==
            "933262154439441526816992388562667004907159682643816214685929638952"
            "175999932299156089414639761565182862536979208272237582511852109168"
            "64000000000000000000000000");
  }

  SECTION("Fixed width") {
    // Unsigned
    REQUIRE(BigInt::ZERO.to_uint32_t() == 0u);
    REQUIRE(BigInt::ONE.to_uint32_t() == 1u);
    REQUIRE(BigInt::NEG_ONE.to_uint32_t() == 0u);  // i.e. UINT_MIN
    REQUIRE(BigInt::TWO.to_uint32_t() == 2u);
    REQUIRE(BigInt::NEG_TWO.to_uint32_t() == 0u);

    REQUIRE(BigInt::ZERO.to_uint64_t() == 0ul);
    REQUIRE(BigInt::ONE.to_uint64_t() == 1ul);
    REQUIRE(BigInt::NEG_ONE.to_uint64_t() == 0u);
    REQUIRE(BigInt::TWO.to_uint64_t() == 2ul);
    REQUIRE(BigInt::NEG_TWO.to_uint64_t() == 0u);

    REQUIRE(bn_ff.to_uint32_t() == 0xffu);
    REQUIRE(bn_ff.to_uint64_t() == 0xffu);

    REQUIRE(bn_2fs.to_uint32_t() == 0xFFFFFFFFu);  // Truncated
    REQUIRE(bn_2fs.to_uint64_t() == 0xFFFFFFFF'FFFFFFFFu);

    REQUIRE(bn_big.to_uint32_t() == 0xFFFFFFFFu);           // Truncated
    REQUIRE(bn_big.to_uint64_t() == 0xFFFFFFFF'FFFFFFFFu);  // Truncated too

    // Signed
    REQUIRE(BigInt::ZERO.to_int32_t() == 0);
    REQUIRE(BigInt::ONE.to_int32_t() == 1);
    REQUIRE(BigInt::NEG_ONE.to_int32_t() == -1);
    REQUIRE(BigInt::TWO.to_int32_t() == 2);
    REQUIRE(BigInt::NEG_TWO.to_int32_t() == -2);

    REQUIRE(BigInt::ZERO.to_int64_t() == 0l);
    REQUIRE(BigInt::ONE.to_int64_t() == 1l);
    REQUIRE(BigInt::NEG_ONE.to_int64_t() == -1l);
    REQUIRE(BigInt::TWO.to_int64_t() == 2l);
    REQUIRE(BigInt::NEG_TWO.to_int64_t() == -2l);

    REQUIRE(bn_ff.to_int32_t() == 0xff);
    REQUIRE(bn_ff.to_int64_t() == 0xff);
    REQUIRE(-bn_ff.to_int32_t() == -0xff);
    REQUIRE(-bn_ff.to_int64_t() == -0xff);

    REQUIRE(bn_2fs.to_int32_t() == 0x7FFFFFFF);             // Truncated
    REQUIRE(bn_2fs.to_int64_t() == 0x7FFFFFFF'FFFFFFFF);    // Truncated
    REQUIRE(-bn_2fs.to_int32_t() == -0x7FFFFFFF);           // Truncated
    REQUIRE(-bn_2fs.to_int64_t() == -0x7FFFFFFF'FFFFFFFF);  // Truncated

    REQUIRE(bn_big.to_int32_t() == 0x7FFFFFFF);             // Truncated
    REQUIRE(bn_big.to_int64_t() == 0x7FFFFFFF'FFFFFFFF);    // Truncated
    REQUIRE(-bn_big.to_int32_t() == -0x7FFFFFFF);           // Truncated
    REQUIRE(-bn_big.to_int64_t() == -0x7FFFFFFF'FFFFFFFF);  // Truncated
  }
}

TEST_CASE("Random") {
  SECTION("Fixed Seed") {
    // Using a seed of 1234 makes the random sequence predictable
    BigIntRand rng(1234u);
    REQUIRE(rng.random(136) == BigInt("0x700eecccd15ddc359f4285267f66e2d331"));
    REQUIRE(rng.random(136) == BigInt("0xdc54dab1c7ac7e8fc577d7f9c90d42989c"));
    REQUIRE(rng.random(136) == BigInt("0xcd477eab32d2201e46c65c9a269024e945"));
    REQUIRE(rng.random(136) == BigInt("0x1dbb3576e03d1f1a28a81f7cf5489eecd0"));
    REQUIRE(rng.random(136) == BigInt("0xaef76d457ca11ddc80413770034de8785b"));
  }
}
}  // namespace BigNum
