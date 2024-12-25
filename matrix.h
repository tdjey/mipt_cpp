#include "BigInteger.h"
#include <algorithm>
#include <array>
#include <cassert>
#include <compare>
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>
#include <type_traits>
#include <vector>
#define CPP23

template <size_t N> class Residue {
    template <size_t cur_i, size_t m> struct IsPrimeHelper {
        static const bool value =
                              (m % cur_i) && IsPrimeHelper < (cur_i * cur_i > m)
                                  ? 0
                                  : cur_i + 1,
                          m > ::value;
    };

    template <size_t m> struct IsPrimeHelper<0, m> {
        static const bool value = true;
    };

    template <size_t m> struct IsPrimeHelper<m, m> {
        static const bool value = true;
    };

    template <size_t cur_i> struct IsPrimeHelper<cur_i, 1> {
        static const bool value = false;
    };

    long long x_ = 0;

    Residue bin_pow(int power) const {
        Residue result = 1;
        Residue multiplier = *this;

        while (power) {
            if (power & 1) {
                result *= multiplier;
            }
            power >>= 1;
            multiplier *= multiplier;
        }
        return result;
    }

    friend std::istream operator>>(std::istream& input, Residue& number);

  public:
    static const bool is_prime = IsPrimeHelper<2, N>::value;

    Residue() = default;

    Residue(int x) : x_(x) {  // мб тут лажа
        x_ %= static_cast<long long>(N);
        if (x_ < 0) {
            x_ += N;
        }
    }

    explicit operator int() const {
        return x_;
    }

    Residue& operator/=(const Residue& other) {
        static_assert(is_prime);
        operator*=(other.bin_pow(N - 2));
        return *this;
    }

    Residue& operator*=(const Residue& other) {
        x_ = (x_ * other.x_) % N;
        return *this;
    }

    Residue& operator+=(const Residue& other) {
        x_ = (x_ + other.x_) % N;
        return *this;
    }

    Residue& operator-=(const Residue& other) {
        x_ = (x_ + N - other.x_) % N;
        return *this;
    }

    long long get_value() const {
        return x_;
    }
};

template <size_t N>
Residue<N> operator/(const Residue<N>& lhs, const Residue<N>& rhs) {
    Residue result(lhs);
    result /= rhs;
    return result;
}

template <size_t N>
Residue<N> operator+(const Residue<N>& lhs, const Residue<N>& rhs) {
    Residue result(lhs);
    result += rhs;
    return result;
}

template <size_t N>
Residue<N> operator-(const Residue<N>& lhs, const Residue<N>& rhs) {
    Residue result(lhs);
    result -= rhs;
    return result;
}

template <size_t N>
Residue<N> operator*(const Residue<N>& lhs, const Residue<N>& rhs) {
    Residue<N> result(lhs);
    result *= rhs;
    return result;
}

template <size_t N>
std::istream& operator>>(std::istream& input, Residue<N>& number) {
    int value;
    input >> value;
    number = value;
    return input;
}

template <size_t N>
bool operator==(const Residue<N>& lhs, const Residue<N>& rhs) {
    return lhs.get_value() == rhs.get_value();
}

template <size_t N, size_t M, typename Field = Rational> class Matrix {
    std::vector<std::vector<Field>> data_;

    struct GaussInverseMatrixHelper {
        Matrix<N, N, Field> result;

        GaussInverseMatrixHelper() {
            for (size_t i = 0; i < N; i++) {
                result.data_[i][i] = 1;
            }
        }

        void rows_substraction(size_t target_row, size_t source_row,
                               const Field coef) {
            assert(target_row != source_row);
            for (size_t i = 0; i < M; ++i) {
                Field res = result.data_[source_row][i] * coef;
                result.data_[target_row][i] -= res;
            }
        }

        void row_multiplication(size_t target_row, const Field coef) {
            assert(coef != Field(0));
            for (size_t i = 0; i < M; ++i) {
                result.data_[target_row][i] *= coef;
            }
        }

        void swap_rows(size_t row1, size_t row2) {
            result.data_[row1].swap(result.data_[row2]);
        }
    };

    void rows_substraction(size_t, size_t, const Field);
    void row_multiplication(size_t, const Field);
    Field Gauss_method_forward(GaussInverseMatrixHelper* = nullptr);
    void Gauss_method_backward(GaussInverseMatrixHelper* = nullptr);
    static inline Field additive_id = Field(0);
    static inline Field multiplicative_id = Field(1);

  public:
    friend struct MatrixMultiply;

    Matrix() {
        data_.assign(N, std::vector<Field>(M, Field(0)));
    }

    Matrix(
        const std::initializer_list<const std::initializer_list<Field>> arr) {
        size_t outer_idx = 0;
        data_.assign(N, std::vector<Field>(0));
        for (const std::initializer_list<Field>& inner_arr : arr) {
            for (const Field& elem : inner_arr) {
                data_[outer_idx].push_back(elem);
            }
            ++outer_idx;
        }
    }

    Matrix& operator+=(const Matrix&);
    Matrix& operator-=(const Matrix&);
    Matrix& operator*=(const Field&);

    const Field& operator[](size_t, size_t) const;
    Field& operator[](size_t, size_t);

    Field det() const;
    Matrix<M, N, Field> transposed() const;
    size_t rank() const;
    Matrix inverted() const;
    void invert();
    Field trace() const;

    std::array<Field, M> getRow(unsigned row) {
        std::array<Field, M> result;
        for (size_t i = 0; i < M; ++i) {
            result[i] = data_[row][i];
        }
        return result;
    };

    std::array<Field, N> getColumn(unsigned column) {
        std::array<Field, N> result;
        for (size_t i = 0; i < N; ++i) {
            result[i] = data_[i][column];
        }
        return result;
    };
};

struct MatrixMultiply {
    template <size_t N, size_t M, size_t K, typename Field = Rational>
    static Matrix<N, K, Field> operator()(const Matrix<N, M, Field>& lhs,
                                          const Matrix<M, K, Field>& rhs) {
        Matrix<N, K, Field> result;
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < K; ++j) {
                for (size_t inner_idx = 0; inner_idx < M; ++inner_idx) {
                    result.data_[i][j] +=
                        lhs.data_[i][inner_idx] * rhs.data_[inner_idx][j];
                }
            }
        }
        return result;
    }
};

template <size_t N, size_t M, typename Field>
void Matrix<N, M, Field>::rows_substraction(size_t target_row,
                                            size_t source_row,
                                            const Field coef) {
    assert(target_row != source_row);
    for (size_t i = 0; i < M; ++i) {
        Field res = data_[source_row][i] * coef;
        data_[target_row][i] -= res;
    }
}

template <size_t N, size_t M, typename Field>
void Matrix<N, M, Field>::row_multiplication(size_t target_row,
                                             const Field coef) {
    assert(coef != Field(0));
    for (size_t i = 0; i < M; ++i) {
        data_[target_row][i] *= coef;
    }
}

template <size_t N, size_t M, typename Field>
Field Matrix<N, M, Field>::Gauss_method_forward(
    Matrix<N, M, Field>::GaussInverseMatrixHelper* gauss_helper) {
    size_t current_column = 0;
    size_t swap_counter = 0;
    Field determinant(1);
    size_t current_row = 0;
    while (current_row < N && current_column < M) {
        size_t non_zero_row = current_row;
        while (non_zero_row < N &&
               data_[non_zero_row][current_column] == Matrix::additive_id) {
            non_zero_row++;
        }
        if (non_zero_row == N) {
            current_column++;
            continue;
        }
        if (non_zero_row != current_row) {
            data_[current_row].swap(data_[non_zero_row]);
            swap_counter++;
            if (gauss_helper) {
                gauss_helper->swap_rows(current_row, non_zero_row);
            }
        }

        if (data_[current_row][current_column] != Matrix::multiplicative_id) {
            determinant *= data_[current_row][current_column];
            Field coef(Matrix::multiplicative_id);
            coef /= data_[current_row][current_column];
            row_multiplication(current_row, coef);
            if (gauss_helper) {
                gauss_helper->row_multiplication(current_row, coef);
            }
        }

        for (size_t i = current_row + 1; i < N; ++i) {
            if (data_[i][current_column] != Matrix::additive_id) {
                if (gauss_helper) {
                    gauss_helper->rows_substraction(i, current_row,
                                                    data_[i][current_column]);
                }
                rows_substraction(i, current_row, data_[i][current_column]);
            }
        }
        ++current_row;
        ++current_column;
    }
    if (swap_counter % 2 == 1) {
        determinant *= -1;
    }
    return determinant;
}

template <size_t N, size_t M, typename Field>
void Matrix<N, M, Field>::Gauss_method_backward(
    Matrix<N, M, Field>::GaussInverseMatrixHelper* gauss_helper) {
    size_t current_row = N;
    bool is_zeroes_row;
    do {
        --current_row;
        is_zeroes_row = true;
        for (size_t i = 0; i < M; i++) {
            if (data_[current_row][i] != Matrix::additive_id) {
                is_zeroes_row = false;
                break;
            }
        }
    } while (current_row > 0 && is_zeroes_row);

    size_t current_column = M - 1;

    while (current_row > 0) {
        // можно попробовать поassertить на количество не единичных столбцов
        while (current_column > 0 &&
               data_[current_row][current_column] == Matrix::additive_id) {
            --current_column;
        }
        if (current_column ==
            0) {  // можно поассертить на то, что выше нет единиц
            break;
        }
        for (size_t j = current_row; j != 0;) {
            --j;
            if (data_[j][current_column] != Matrix::additive_id) {
                if (gauss_helper) {
                    gauss_helper->rows_substraction(j, current_row,
                                                    data_[j][current_column]);
                }
                rows_substraction(j, current_row, data_[j][current_column]);
            }
        }
        --current_row;
    }
}

template <size_t N, typename Field = Rational>
using SquareMatrix = Matrix<N, N, Field>;

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field>& Matrix<N, M, Field>::operator+=(const Matrix& rhs) {
    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < M; ++j) {
            data_[i][j] += rhs.data_[i][j];
        }
    }
    return *this;
}

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field>& Matrix<N, M, Field>::operator-=(const Matrix& rhs) {
    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < M; ++j) {
            data_[i][j] -= rhs.data_[i][j];
        }
    }
    return *this;
}

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field>& Matrix<N, M, Field>::operator*=(const Field& rhs) {
    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < M; ++j) {
            data_[i][j] *= rhs;
        }
    }
    return *this;
}

template <size_t N, size_t M, typename Field>
const Field& Matrix<N, M, Field>::operator[](size_t i, size_t j) const {
    return data_[i][j];
}

template <size_t N, size_t M, typename Field>
Field& Matrix<N, M, Field>::operator[](size_t i, size_t j) {
    return data_[i][j];
}

template <size_t N, size_t M, typename Field>
Field Matrix<N, M, Field>::det() const {
    static_assert(N == M);
    Matrix<N, M, Field> gauss_copy(*this);
    Field ans = gauss_copy.Gauss_method_forward();

    for (size_t i = 0; i < std::min(N, M); ++i) {
        if (gauss_copy.data_[i][i] == Matrix::additive_id)
            return Matrix::additive_id;
    }
    return ans;
}

template <size_t N, size_t M, typename Field>
Matrix<M, N, Field> Matrix<N, M, Field>::transposed() const {
    Matrix<M, N, Field> result;
    for (size_t i = 0; i < M; i++) {
        for (size_t j = 0; j < N; j++) {
            result[i, j] = data_[j][i];
        }
    }
    return result;
}

template <size_t N, size_t M, typename Field>
size_t Matrix<N, M, Field>::rank() const {
    Matrix<N, M, Field> gauss_copy(*this);
    gauss_copy.Gauss_method_forward();
    size_t zeroes_rows_border = N;
    while (zeroes_rows_border) {
        bool is_zeroes = true;
        for (size_t j = 0; j < M; j++) {
            if (gauss_copy.data_[zeroes_rows_border - 1][j] !=
                Matrix::additive_id) {
                is_zeroes = false;
                break;
            }
        }
        if (!is_zeroes) {
            break;
        }
        --zeroes_rows_border;
    }
    return zeroes_rows_border;
}

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> Matrix<N, M, Field>::inverted() const {
    static_assert(N == M);
    Matrix gauss_copy(*this);
    GaussInverseMatrixHelper result;
    gauss_copy.Gauss_method_forward(&result);
    gauss_copy.Gauss_method_backward(&result);
    return result.result;
}

template <size_t N, size_t M, typename Field>
void Matrix<N, M, Field>::invert() {
    static_assert(N == M);
    GaussInverseMatrixHelper result;
    Gauss_method_forward(&result);
    Gauss_method_backward(&result);
    data_.swap(result.result.data_);
}

template <size_t N, size_t M, typename Field>
Field Matrix<N, M, Field>::trace() const {
    static_assert(N == M);
    Field result(0);
    for (size_t i = 0; i < N; ++i) {
        result += data_[i][i];
    }
    return result;
}

template <size_t N, typename Field = Rational>
SquareMatrix<N, Field>& operator*=(SquareMatrix<N, Field>& lhs,
                                   const SquareMatrix<N, Field>& rhs) {
    lhs = lhs * rhs;
    return lhs;
}

template <size_t N, size_t M, typename Field = Rational>
Matrix<N, M, Field> operator+(const Matrix<N, M, Field>& lhs,
                              const Matrix<N, M, Field>& rhs) {
    Matrix<N, M, Field> result(lhs);
    result += rhs;
    return result;
}

template <size_t N, size_t M, typename Field = Rational>
Matrix<N, M, Field> operator-(const Matrix<N, M, Field>& lhs,
                              const Matrix<N, M, Field>& rhs) {
    Matrix<N, M, Field> result(lhs);
    result -= rhs;
    return result;
}

template <size_t N, size_t M, typename Field = Rational, size_t K>
Matrix<N, K, Field> operator*(const Matrix<N, M, Field>& lhs,
                              const Matrix<M, K, Field>& rhs) {
    return MatrixMultiply::operator()(lhs, rhs);
}

template <size_t N, size_t M, typename Field = Rational>
Matrix<N, M, Field> operator*(const Field& coef,
                              const Matrix<N, M, Field>& matrix) {
    Matrix result(matrix);
    result *= coef;
    return result;
}

template <size_t N, size_t M, typename Field = Rational>
bool operator==(const Matrix<N, M, Field>& lhs,
                const Matrix<N, M, Field>& rhs) {
    for (size_t i = 0; i < N; ++i)
        for (size_t j = 0; j < M; ++j) {
            if (lhs[i, j] != rhs[i, j]) {
                return false;
            }
        }
    return true;
}