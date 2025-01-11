#include <algorithm>
#include <compare>
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

class BigInteger {
  private:
    bool is_negative_ = false;
    std::vector<long long> digits_;

    static const long long kBase_ = 1000000000;
    static const long long kCharsInDigits = 9;

    void delete_leading_zeroes();
    void swap(BigInteger&);
    bool is_zero() const;

    BigInteger karatsuba_multiplication(const BigInteger&, const BigInteger&);

  public:
    int sign() {
        if (is_zero()) {
            return 0;
        }
        return is_negative_ ? -1 : 1;
    }

    static BigInteger gcd(const BigInteger&, const BigInteger&);

    explicit BigInteger(const std::string&);
    BigInteger() = default;
    BigInteger(long long);

    ~BigInteger() = default;

    BigInteger operator-() const;

    BigInteger operator*(long long) const;

    friend std::strong_ordering operator<=>(const BigInteger&,
                                            const BigInteger&);

    friend std::istream& operator>>(std::istream&, BigInteger&);
    friend std::ostream& operator<<(std::ostream&, const BigInteger&);
    friend BigInteger operator""_bi(const char*, size_t);
    friend class Rational;

    void digitsSubstraction(const std::vector<long long>&,
                            const std::vector<long long>&);
    void digitsAddition(const std::vector<long long>&,
                        const std::vector<long long>&);

    BigInteger& operator+=(const BigInteger&);
    BigInteger& operator-=(const BigInteger&);
    BigInteger& operator*=(const BigInteger&);
    BigInteger& operator/=(const BigInteger&);
    BigInteger& operator%=(const BigInteger&);

    BigInteger& operator++();
    BigInteger& operator--();
    BigInteger operator++(int) &;
    BigInteger operator--(int) &;

    bool operator==(const BigInteger&) const = default;

    std::string toString() const;

    explicit operator bool();
};

bool BigInteger::is_zero() const {
    return digits_.back() == 0;
}

void BigInteger::delete_leading_zeroes() {
    while (digits_.size() > 1 && !digits_.back()) {
        digits_.pop_back();
    }
}

void BigInteger::swap(BigInteger& other) {
    digits_.swap(other.digits_);
    std::swap(is_negative_, other.is_negative_);
}

BigInteger::BigInteger(const std::string& inp) {
    size_t start = 0;
    if (inp[0] == '-') {
        is_negative_ = true;
        start = 1;
    } else {
        is_negative_ = false;
    }
    digits_.clear();

    for (long long i = static_cast<long long>(inp.size()) - 1;
         i >= static_cast<long long>(start); i -= BigInteger::kCharsInDigits) {
        digits_.push_back(0);
        for (size_t j = static_cast<size_t>(
                 std::max(i - BigInteger::kCharsInDigits + 1,
                          static_cast<long long>(start)));
             j <= static_cast<size_t>(i); ++j) {
            digits_.back() = digits_.back() * 10 +
                             static_cast<long long>(inp[j]) -
                             static_cast<long long>('0');
        }
    }

    delete_leading_zeroes();

    if (digits_.empty()) {
        digits_.assign(1, 0);
    }
    if (is_zero()) {
        is_negative_ = false;
    }
}

BigInteger::BigInteger(long long x) : is_negative_(x < 0) {
    x = std::abs(x);
    while (x != 0) {
        digits_.push_back(x % kBase_);
        x /= kBase_;
    }
    if (digits_.empty()) digits_.push_back(0);
}

BigInteger BigInteger::operator-() const {
    BigInteger ans = *this;
    if (ans.is_zero()) {
        ans.is_negative_ = false;
        return ans;
    }
    ans.is_negative_ = !is_negative_;
    return ans;
}

BigInteger operator+(const BigInteger& lhs, const BigInteger& rhs) {
    BigInteger ans(lhs);
    ans += rhs;
    return ans;
}

BigInteger operator-(const BigInteger& lhs, const BigInteger& rhs) {
    BigInteger ans(lhs);
    ans -= rhs;
    return ans;
}

BigInteger operator*(const BigInteger& lhs, const BigInteger& rhs) {
    BigInteger ans(lhs);
    ans *= rhs;
    return ans;
}

BigInteger operator/(const BigInteger& lhs, const BigInteger& rhs) {
    BigInteger ans(lhs);
    ans /= rhs;
    return ans;
}

BigInteger operator%(const BigInteger& lhs, const BigInteger& rhs) {
    BigInteger ans(lhs);
    ans %= rhs;
    return ans;
}

BigInteger BigInteger::karatsuba_multiplication(const BigInteger& lhs,
                                                const BigInteger& rhs) {
    if (lhs.is_zero() || rhs.is_zero()) {
        return 0;
    }
    if (rhs == 1 || rhs == -1) {
        return rhs.is_negative_ ? -lhs : lhs;
    }
    if (lhs == 1 || lhs == -1) {
        return lhs.is_negative_ ? -rhs : rhs;
    }

    const size_t kSquareBorder = 45;
    if (lhs.digits_.size() <= kSquareBorder) {
        BigInteger ans(0);
        for (size_t j = 0; j < rhs.digits_.size(); ++j) {
            BigInteger cur = lhs * rhs.digits_[j];
            cur.is_negative_ = false;
            if (j) {
                for (size_t i = 0; i < j; ++i)
                    cur.digits_.push_back(0);
                rotate(cur.digits_.begin(),
                       cur.digits_.end() - static_cast<int>(j),
                       cur.digits_.end());
            }
            ans += cur;
        }
        if (!ans.is_zero()) {
            ans.is_negative_ = lhs.is_negative_ ^ rhs.is_negative_;
        } else {
            ans.is_negative_ = false;
        }
        return ans;
    }

    BigInteger lhs_left;
    size_t split_sz = (lhs.digits_.size() + 1) / 2;
    lhs_left.digits_.assign(lhs.digits_.begin(),
                            lhs.digits_.begin() + static_cast<int>(split_sz));
    BigInteger lhs_right;
    lhs_right.digits_.assign(lhs.digits_.begin() + static_cast<int>(split_sz),
                             lhs.digits_.end());
    BigInteger rhs_left;
    rhs_left.digits_.assign(
        rhs.digits_.begin(),
        rhs.digits_.begin() +
            static_cast<int>(std::min(split_sz, rhs.digits_.size())));
    BigInteger rhs_right;

    if (split_sz >= rhs.digits_.size()) {
        rhs_right = 0;
    } else {
        rhs_right.digits_.assign(
            rhs.digits_.begin() + static_cast<int>(split_sz),
            rhs.digits_.end());
    }

    BigInteger mul_first = lhs_left * rhs_left;
    BigInteger mul_second = (lhs_left + lhs_right) * (rhs_left + rhs_right);
    BigInteger mul_third = lhs_right * rhs_right;
    mul_second -= mul_first + mul_third;
    mul_second.digits_.resize(mul_second.digits_.size() + split_sz);
    rotate(mul_second.digits_.begin(),
           mul_second.digits_.end() - static_cast<int>(split_sz),
           mul_second.digits_.end());
    mul_third.digits_.resize(mul_third.digits_.size() + 2 * split_sz);
    rotate(mul_third.digits_.begin(),
           mul_third.digits_.end() - static_cast<int>(2 * split_sz),
           mul_third.digits_.end());
    BigInteger res = mul_first + mul_second + mul_third;

    if (!res.is_zero())
        res.is_negative_ = lhs.is_negative_ ^ rhs.is_negative_;
    else
        res.is_negative_ = false;
    return res;
}

void BigInteger::digitsSubstraction(const std::vector<long long>& lhs_bits,
                                    const std::vector<long long>& rhs_bits) {
    long long remainder = 0;
    digits_.resize(lhs_bits.size());

    for (size_t i = 0; i < lhs_bits.size(); ++i) {
        long long next_digit =
            lhs_bits[i] - (i < rhs_bits.size() ? rhs_bits[i] : 0) - remainder;
        if (next_digit < 0) {
            next_digit += BigInteger::kBase_;
            remainder = 1;
        } else {
            remainder = 0;
        }
        digits_[i] = next_digit;
    }

    if (remainder) {
        digits_.back()--;
    }
}

BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
    if (is_negative_ == !rhs.is_negative_) {  // redirect to op+() if signs are
        is_negative_ = !is_negative_;
        *this += (rhs);
        delete_leading_zeroes();
        if (!is_zero()) {
            is_negative_ = !is_negative_;
        }
        return *this;
    }

    bool init_is_neg = is_negative_;
    is_negative_ = rhs.is_negative_;
    bool is_less_unsigned = (*this < rhs) ^ is_negative_;
    is_negative_ = is_less_unsigned ? !init_is_neg : init_is_neg;

    if (is_less_unsigned) {
        digitsSubstraction(rhs.digits_, digits_);
    } else {
        digitsSubstraction(digits_, rhs.digits_);
    }

    delete_leading_zeroes();
    return *this;
}

void BigInteger::digitsAddition(const std::vector<long long>& lhs_bits,
                                const std::vector<long long>& rhs_bits) {
    digits_.resize(lhs_bits.size() + 1);
    size_t min_sz = rhs_bits.size();

    long long remainder = 0;
    size_t i = 0;
    for (; i < lhs_bits.size(); ++i) {
        long long next_digit =
            lhs_bits[i] + (i < min_sz ? rhs_bits[i] : 0) + remainder;
        if (next_digit >= BigInteger::kBase_) {
            next_digit -= BigInteger::kBase_;
            remainder = 1;
        } else {
            remainder = 0;
        }
        digits_[i] = next_digit;
    }
    if (remainder) {
        digits_.back()++;
    }
}

BigInteger& BigInteger::operator+=(const BigInteger& rhs) {
    if (rhs.is_zero()) {
        return *this;
    }
    if (is_negative_ != rhs.is_negative_) {
        is_negative_ = !is_negative_;
        *this -= (rhs);
        delete_leading_zeroes();
        if (is_zero())
            is_negative_ = false;
        else
            is_negative_ = !is_negative_;
        return *this;
    }

    bool is_sz_greater = digits_.size() > rhs.digits_.size();

    if (is_sz_greater) {
        digitsAddition(digits_, rhs.digits_);
    }
    else {
        digitsAddition(rhs.digits_, digits_);
    }

    delete_leading_zeroes();
    if (is_zero()) {
        is_negative_ = false;
    }
    return *this;
}

BigInteger& BigInteger::operator*=(const BigInteger& rhs) {
    *this = karatsuba_multiplication(*this, rhs);
    delete_leading_zeroes();
    return *this;
}

BigInteger& BigInteger::operator%=(const BigInteger& rhs) {
    *this -= (*this / rhs) * rhs;
    return *this;
}

BigInteger& BigInteger::operator/=(const BigInteger& rhs_signed) {
    BigInteger rhs(rhs_signed);
    size_t div_sz = rhs.digits_.size();
    delete_leading_zeroes();
    reverse(digits_.begin(), digits_.end());
    BigInteger ans(0);
    ans.is_negative_ = is_negative_ ^ rhs.is_negative_;
    rhs.is_negative_ = false;
    is_negative_ = false;
    BigInteger cur_slice;
    cur_slice.digits_.assign(digits_.begin(),
                             digits_.begin() + static_cast<long>(div_sz));
    reverse(cur_slice.digits_.begin(), cur_slice.digits_.end());

    if (cur_slice < rhs) {
        if (div_sz == digits_.size()) {
            if (ans.is_zero()) ans.is_negative_ = false;
            swap(ans);
            return *this;
        }
        cur_slice.digits_.insert(cur_slice.digits_.begin(), digits_[div_sz]);
    }

    for (size_t i = cur_slice.digits_.size(); i <= digits_.size(); ++i) {
        long long left_border = 0, right_border = kBase_;
        while (right_border - left_border > 1) {
            long long middle = (left_border + right_border) / 2;
            if (rhs * middle <= cur_slice)
                left_border = middle;
            else
                right_border = middle;
        }
        ans.digits_.push_back(left_border);
        cur_slice -= rhs * left_border;
        if (i != digits_.size()) {
            cur_slice.digits_.insert(cur_slice.digits_.begin(), digits_[i]);
        }
        cur_slice.delete_leading_zeroes();
    }

    reverse(ans.digits_.begin(), ans.digits_.end());
    ans.delete_leading_zeroes();
    if (ans.is_zero()) ans.is_negative_ = false;
    swap(ans);
    return *this;
}

BigInteger BigInteger::operator*(long long x) const {
    if (is_zero()) return {0};
    long long remainder = 0;
    BigInteger ans;
    bool x_is_neg = (x < 0);
    x = abs(x);

    for (long long bit : digits_) {
        long long next_digit = bit * x + remainder;
        ans.digits_.push_back(next_digit % kBase_);
        remainder = next_digit / kBase_;
    }

    while (remainder) {
        ans.digits_.push_back(remainder % kBase_);
        remainder /= kBase_;
    }
    ans.delete_leading_zeroes();
    if (ans.is_zero()) {
        ans.is_negative_ = false;
    } else {
        ans.is_negative_ = is_negative_ ^ x_is_neg;
    }
    return ans;
}

BigInteger& BigInteger::operator++() {
    *this += 1;
    return *this;
}

BigInteger& BigInteger::operator--() {
    *this -= 1;
    return *this;
}

BigInteger BigInteger::operator++(int) & {
    BigInteger ans(*this);
    *this += 1;
    return ans;
}

BigInteger BigInteger::operator--(int) & {
    BigInteger ans(*this);
    *this -= 1;
    return ans;
}

std::strong_ordering operator<=>(const BigInteger& lhs, const BigInteger& rhs) {
    if (lhs.is_negative_ != rhs.is_negative_) {
        return (lhs.is_negative_ > rhs.is_negative_)
                   ? std::strong_ordering::less
                   : std::strong_ordering::greater;
    }
    bool sign = lhs.is_negative_;
    if (lhs.digits_.size() != rhs.digits_.size()) {
        bool unsigned_less = lhs.digits_.size() < rhs.digits_.size();
        return (unsigned_less ^ sign) ? std::strong_ordering::less
                                      : std::strong_ordering::greater;
    }

    bool equal = true;
    bool is_less_unsigned;
    size_t idx = (lhs.digits_.size());
    while (idx) {
        idx--;
        if (lhs.digits_[idx] != rhs.digits_[idx]) {
            is_less_unsigned = (lhs.digits_[idx] < rhs.digits_[idx]);
            equal = false;
            break;
        }
    }
    if (equal) {
        return std::strong_ordering::equal;
    }
    return is_less_unsigned ^ sign ? std::strong_ordering::less
                                   : std::strong_ordering::greater;
}

std::istream& operator>>(std::istream& input, BigInteger& rhs) {
    std::string s;
    input >> s;
    BigInteger to_swap(s);
    rhs.swap(to_swap);
    return input;
}

std::ostream& operator<<(std::ostream& output, const BigInteger& rhs) {
    output << rhs.toString();
    return output;
}

BigInteger operator""_bi(const char* str, size_t str_size) {
    BigInteger result;
    size_t start = 0;
    if (str[0] == '-') {
        result.is_negative_ = true;
        start = 1;
    } else {
        result.is_negative_ = false;
    }
    for (long long i = static_cast<long long>(str_size) - 1;
         i >= static_cast<long long>(start); i -= BigInteger::kCharsInDigits) {
        result.digits_.push_back(0);
        for (size_t j = static_cast<size_t>(
                 std::max(i - BigInteger::kCharsInDigits + 1,
                          static_cast<long long>(start)));
             j <= static_cast<size_t>(i); ++j) {
            result.digits_.back() = result.digits_.back() * 10 +
                                    static_cast<long long>(str[j]) -
                                    static_cast<long long>('0');
        }
    }

    result.delete_leading_zeroes();

    if (result.digits_.empty()) {
        result.digits_.assign(1, 0);
    }
    if (result.is_zero()) {
        result.is_negative_ = false;
    }
    return result;
}

BigInteger operator""_bi(unsigned long long c) {
    if (c > LLONG_MAX) {
        long long remainder = static_cast<long long>(c % 10ull);
        c /= 10ull;
        BigInteger ans(static_cast<long long>(c));
        ans *= 10;
        ans += remainder;
        return ans;
    }
    return static_cast<long long>(c);
}

std::string BigInteger::toString() const {
    std::string ans;
    for (long long x : digits_) {
        std::string cur_ans;
        for (long long j = 0; j < BigInteger::kCharsInDigits; ++j) {
            cur_ans += static_cast<char>(x % 10 + '0');
            x /= 10;
        }
        ans += cur_ans;
    }
    while (ans.size() > 1 && ans.back() == '0') {
        ans.pop_back();
    }
    if (is_zero()) {
        ans = "0";
    } else if (is_negative_) {
        ans.push_back('-');
    }
    reverse(ans.begin(), ans.end());
    return ans;
}

BigInteger::operator bool() {
    return !is_zero();
}

BigInteger BigInteger::gcd(const BigInteger& a, const BigInteger& b) {
    if (b.is_zero()) {
        return a;
    }
    return gcd(b, a % b);
};

class Rational {
    BigInteger numerator_;
    BigInteger denominator_;

    void to_prime_form() {
        bool is_neg = (numerator_.sign() * denominator_.sign() == -1);

        if (numerator_.sign() == -1) {
            numerator_ *= -1;
        }

        if (denominator_.sign() == -1) {
            denominator_ *= -1;
        }

        BigInteger gcd_ = BigInteger::gcd(numerator_, denominator_);
        numerator_ /= gcd_;
        denominator_ /= gcd_;
        if (is_neg) numerator_ *= -1;
    }

  public:
    Rational() = default;

    Rational(const BigInteger& numerator, const BigInteger& denominator)
        : numerator_(numerator), denominator_(denominator) {
        to_prime_form();
    }

    Rational(const BigInteger& x) : numerator_(x), denominator_(1) {
        to_prime_form();
    }

    Rational(long long x) : numerator_(x), denominator_(1) {}

    Rational& operator+=(const Rational&);
    Rational& operator-=(const Rational&);
    Rational& operator*=(const Rational&);
    Rational& operator/=(const Rational&);
    Rational operator-() const;
    friend std::strong_ordering operator<=>(const Rational&, const Rational&);
    friend std::istream& operator>>(std::istream&, Rational&);
    bool operator==(const Rational&) const = default;
    std::string toString() const;
    std::string asDecimal(size_t);
    explicit operator double();
};

std::istream& operator>>(std::istream& input, Rational& number) {
    input >> number.numerator_;
    return input;
}

Rational operator+(const Rational& lhs, const Rational& rhs) {
    Rational ans(lhs);
    ans += rhs;
    return ans;
}

Rational operator-(const Rational& lhs, const Rational& rhs) {
    Rational ans(lhs);
    ans -= rhs;
    return ans;
}

Rational operator*(const Rational& lhs, const Rational& rhs) {
    Rational ans(lhs);
    ans *= rhs;
    return ans;
}

Rational operator/(const Rational& lhs, const Rational& rhs) {
    Rational ans(lhs);
    ans /= rhs;
    return ans;
}

Rational& Rational::operator+=(const Rational& rhs) {
    numerator_ *= rhs.denominator_;
    numerator_ += rhs.numerator_ * denominator_;
    denominator_ *= rhs.denominator_;
    to_prime_form();
    return *this;
}

Rational& Rational::operator-=(const Rational& rhs) {
    numerator_ *= rhs.denominator_;
    numerator_ -= rhs.numerator_ * denominator_;
    denominator_ *= rhs.denominator_;
    to_prime_form();
    return *this;
}

Rational& Rational::operator*=(const Rational& rhs) {
    numerator_ *= rhs.numerator_;
    denominator_ *= rhs.denominator_;
    to_prime_form();
    return *this;
}

Rational& Rational::operator/=(const Rational& rhs) {
    numerator_ *= rhs.denominator_;
    denominator_ *= rhs.numerator_;
    to_prime_form();
    return *this;
}

Rational Rational::operator-() const {
    Rational result(*this);
    result.numerator_.is_negative_ ^= 1;
    return result;
}

std::strong_ordering operator<=>(const Rational& lhs, const Rational& rhs) {
    BigInteger left(lhs.numerator_ * rhs.denominator_);
    BigInteger right(lhs.denominator_ * rhs.numerator_);
    return left <=> right;
}

std::string Rational::toString() const {
    Rational ans_rational = *this;
    ans_rational.to_prime_form();
    std::string ans = ans_rational.numerator_.toString();
    if (ans_rational.denominator_ == BigInteger(1)) return ans;
    ans += "/" + ans_rational.denominator_.toString();
    return ans;
}

std::string Rational::asDecimal(size_t precision = 0) {
    if (precision == 0) {
        return (numerator_ / denominator_).toString();
    }
    bool is_neg = numerator_.sign() * denominator_.sign() == -1;
    if (numerator_.sign() == -1) numerator_ *= -1;
    BigInteger precise_number(numerator_);
    for (size_t i = 0; i < precision; ++i) {
        precise_number *= 10;
    }
    std::string int_part;

    if (is_neg && numerator_ != 0_bi) int_part.push_back('-');

    int_part += (numerator_ / denominator_).toString();
    std::string fractional_part = (precise_number / denominator_).toString();
    while (fractional_part.size() < precision) {
        fractional_part.insert(fractional_part.begin(), '0');
    }
    int_part.push_back('.');
    int_part +=
        fractional_part.substr(static_cast<size_t>(std::max(
                                   0, static_cast<int>(fractional_part.size()) -
                                          static_cast<int>(precision))),
                               precision);
    return int_part;
}

Rational::operator double() {
    const int kPrecision = 15;
    return std::stod(asDecimal(kPrecision));
}