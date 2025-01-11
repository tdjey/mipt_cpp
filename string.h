#include <cstring>
#include <fstream>
#include <iostream>

class CowString {
    size_t capacity_ = 1;
    size_t size_ = 0;
    char* str_ = nullptr;
    size_t* instance_counter_ = nullptr;

  private:
    void self_copy(size_t);

    void self_copy() {
        self_copy(capacity_);
    }

    void swap(CowString& other);

    explicit CowString(size_t, size_t);

  public:
    CowString(const char*);
    CowString(size_t, char);
    CowString();
    CowString(const CowString&);
    CowString& operator=(const CowString&);
    ~CowString();
    char& operator[](int idx);
    const char& operator[](int idx) const;
    size_t length() const;
    size_t size() const;
    size_t capacity() const;
    void push_back(char);
    void pop_back();
    const char& front() const;
    char& front();
    const char& back() const;
    char& back();
    CowString& operator+=(char);
    CowString& operator+=(const CowString&);
    CowString operator+(const CowString&) const;
    CowString operator+(char) const;
    CowString operator+(const char*) const;
    size_t find(const CowString&) const;
    size_t rfind(const CowString&) const;
    CowString substr(size_t, size_t) const;
    bool empty() const;
    void clear();
    void shrink_to_fit();
    char* data();
    const char* data() const;
};

CowString operator+(char c, const CowString& str) {
    CowString result{1 + str.size(), '0'};
    result[0] = c;
    memcpy(result.data() + 1, str.data(), str.size());
    return result;
};

CowString operator+(const char* other_s, const CowString& str) {
    CowString result(other_s);
    result += str;
    return result;
};

std::istream& operator>>(std::istream& input, CowString& inp_str) {
    inp_str.clear();
    char cur_symbol = static_cast<char>(input.peek());
    for (; isspace(cur_symbol) && cur_symbol != EOF;
         cur_symbol = static_cast<char>(input.peek())) {
        input.get();
    }
    for (; !isspace(cur_symbol) && cur_symbol != EOF;
         cur_symbol = static_cast<char>(input.peek())) {
        input.get(cur_symbol);
        inp_str.push_back(cur_symbol);
    }
    return input;
}

std::ostream& operator<<(std::ostream& output, const CowString& out_str) {
    output.write(out_str.data(), static_cast<long>(out_str.size()));
    return output;
}

void CowString::self_copy(size_t new_capacity) {
    --(*instance_counter_);
    char* data_pointer = new char[sizeof(size_t) + new_capacity + 1];
    char* new_s = reinterpret_cast<char*>(data_pointer);
    new_s += sizeof(size_t);
    memcpy(new_s, str_, size_);
    str_ = new_s;
    str_[size_] = 0;
    if (*instance_counter_ == 0) {
        delete[] reinterpret_cast<char*>(instance_counter_);
    }
    instance_counter_ = reinterpret_cast<size_t*>(data_pointer);
    *instance_counter_ = 1;
    capacity_ = new_capacity;
}

void CowString::swap(CowString& other) {
    using std::swap;
    swap(size_, other.size_);
    swap(capacity_, other.capacity_);
    swap(str_, other.str_);
    swap(instance_counter_, other.instance_counter_);
}

CowString::CowString(size_t capacity, size_t size = 0)
    : capacity_(capacity), size_(size) {
    char* data_pointer = new char[sizeof(size_t) + capacity_ + 1];
    instance_counter_ = reinterpret_cast<size_t*>(data_pointer);
    *instance_counter_ = 1;
    str_ = reinterpret_cast<char*>(data_pointer);
    str_ += sizeof(size_t);
    str_[size_] = 0;
}

CowString::CowString(const char* other_s) : CowString(strlen(other_s) + 1) {
    --capacity_;
    size_ = capacity_;
    memcpy(this->str_, other_s, size_ + 1);
}

CowString::CowString(size_t n, char c) : CowString(n, n) {
    std::fill(str_, str_ + n, c);
    str_[size_] = 0;
}

CowString::CowString() : CowString(1) {}

CowString::CowString(const CowString& other)
    : capacity_(other.capacity_)
    , size_(other.size_)
    , str_(other.str_)
    , instance_counter_(other.instance_counter_) {
    ++(*instance_counter_);
}

CowString& CowString::operator=(const CowString& other) {
    if (this != &other) {
        CowString copied(other);
        this->swap(copied);
    }
    return *this;
}

CowString::~CowString() {
    if (*instance_counter_ == 1) {
        delete[] reinterpret_cast<char*>(instance_counter_);
    } else {
        --(*instance_counter_);
    }
}

bool operator==(const CowString& lhs, const CowString& rhs) {
    return (lhs.data() == rhs.data()) ||
           ((lhs.size() == rhs.size()) &&
            memcmp(lhs.data(), rhs.data(), lhs.size()) == 0);
}

bool operator<(const CowString& lhs, const CowString& rhs) {
    size_t border = std::min(lhs.size(), rhs.size());
    int val = memcmp(lhs.data(), rhs.data(), border);
    return (lhs.data() != rhs.data()) &&
           (val < 0 || (val == 0 && lhs.size() < rhs.size()));
}

bool operator>(const CowString& lhs, const CowString& rhs) {
    return rhs < lhs;
}

bool operator<=(const CowString& lhs, const CowString& rhs) {
    return !(lhs > rhs);
}

bool operator>=(const CowString& lhs, const CowString& rhs) {
    return !(lhs < rhs);
}

char& CowString::operator[](int idx) {
    if (*instance_counter_ != 1) {
        self_copy(capacity_);
    }
    return str_[idx];
}

const char& CowString::operator[](int idx) const {
    return str_[idx];
}

size_t CowString::length() const {
    return size_;
}

size_t CowString::size() const {
    return size_;
}

size_t CowString::capacity() const {
    return capacity_;
}

void CowString::push_back(char c) {
    bool need_realloc = false;
    size_t new_capacity = capacity_;
    if (size_ == capacity_) {
        new_capacity *= 2;
        need_realloc = true;
    } else if (*instance_counter_ != 1) {
        need_realloc = true;
    }

    if (need_realloc) {
        self_copy(new_capacity);
    }

    str_[size_++] = c;
    str_[size_] = '\0';
}

void CowString::pop_back() {
    if (*instance_counter_ != 1) self_copy();
    str_[--size_] = '\0';
}

const char& CowString::front() const {
    return str_[0];
}

char& CowString::front() {
    if (*instance_counter_ != 1) self_copy();
    return str_[0];
}

const char& CowString::back() const {
    return str_[size_ - 1];
}

char& CowString::back() {
    if (*instance_counter_ != 1) self_copy();
    return str_[size_ - 1];
}

CowString& CowString::operator+=(char c) {
    push_back(c);
    return *this;
}

CowString& CowString::operator+=(const CowString& other) {
    bool need_realloc = false;
    size_t new_capacity = capacity_;
    if (size_ + other.size_ > capacity_) {
        new_capacity = (size_ + other.size_) * 2;
        need_realloc = true;
    } else if (*instance_counter_ != 1) {
        need_realloc = true;
    }

    if (need_realloc) {
        self_copy(new_capacity);
    }
    memcpy(str_ + size_, other.str_, other.size_);
    size_ += other.size_;
    str_[size_] = '\0';
    return *this;
}

CowString CowString::operator+(const CowString& other) const {
    CowString result(*this);
    result += other;
    return result;
}

CowString CowString::operator+(char other_char) const {
    CowString result(*this);
    result.push_back(other_char);
    return result;
}

CowString CowString::operator+(const char* other) const {
    CowString result(*this);
    result += CowString(other);
    return result;
}

size_t CowString::find(const CowString& other) const {
    if (other.size_ == 0 || size_ < other.size_) {
        return size_;
    }
    for (size_t i = 0; i <= size_ - other.size_; ++i) {
        if (memcmp(str_ + i, other.str_, other.size_) == 0) {
            return i;
        }
    }
    return size_;
}

size_t CowString::rfind(const CowString& other) const {
    if (other.size_ == 0 || size_ < other.size_) {
        return size_;
    }

    for (int i = static_cast<int>(size_ - other.size_); i >= 0; --i) {
        if (memcmp(str_ + i, other.str_, other.size_) == 0) {
            return i;
        }
    }
    return size_;
}

CowString CowString::substr(size_t start, size_t count) const {
    if (count == 0 || start >= size_) {
        return {};
    }
    if (count > size_ - start) {
        count = size_ - start;
    }
    CowString result{count, '0'};
    memcpy(result.str_, str_ + start, count);
    return result;
}

bool CowString::empty() const {
    return size_ == 0;
}

void CowString::clear() {
    if (*instance_counter_ != 1) self_copy();
    size_ = 0;
    str_[size_] = '\0';
}

void CowString::shrink_to_fit() {
    bool need_realloc = false;
    size_t new_capacity = capacity_;
    if (capacity_ != size_) {
        new_capacity = size_;
        need_realloc = true;
    } else if (*instance_counter_ != 1) {
        need_realloc = true;
    }

    if (need_realloc) self_copy(new_capacity);
    str_[size_] = '\0';
}

char* CowString::data() {
    if (*instance_counter_ != 1) self_copy();
    return str_;
}

const char* CowString::data() const {
    return str_;
}