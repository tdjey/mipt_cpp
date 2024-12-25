#include <cstring>
#include <fstream>
#include <iostream>

class CowString {
    size_t capacity_ = 1;
    size_t size_ = 0;
    char* s_ = nullptr;
    int* instance_counter_ = nullptr;

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
    CowString& operator=(CowString);
    ~CowString();
    friend bool operator==(const CowString&, const CowString&);
    friend bool operator<(const CowString&, const CowString&);
    friend bool operator>(const CowString&, const CowString&);
    friend bool operator<=(const CowString&, const CowString&);
    friend bool operator>=(const CowString&, const CowString&);
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
    CowString substr(int, unsigned long) const;
    bool empty() const;
    void clear();
    void shrink_to_fit();
    char* data();
    const char* data() const;
    friend std::istream& operator>>(std::istream&, CowString&);
    friend std::ostream& operator<<(std::ostream&, const CowString&);

    friend CowString operator+(char c, const CowString& str) {
        CowString result{1 + str.size_, '0'};
        result[0] = c;
        memcpy(&result.s_[1], str.s_, str.size_);
        return result;
    };

    friend CowString operator+(const char* other_s, const CowString& str) {
        CowString result(other_s);
        result += str;
        return result;
    };
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
    output.write(out_str.s_, static_cast<long>(out_str.size_));
    return output;
}

void CowString::self_copy(size_t new_capacity) {
    --(*instance_counter_);
    char* data_pointer = new char[sizeof(int) + new_capacity + 1];
    char* new_s = reinterpret_cast<char*>(data_pointer);
    new_s += sizeof(int);
    memcpy(new_s, s_, size_);
    s_ = new_s;
    s_[size_] = 0;
    if (*instance_counter_ == 0) {
        delete[] reinterpret_cast<char*>(instance_counter_);
    }
    instance_counter_ = reinterpret_cast<int*>(data_pointer);
    *instance_counter_ = 1;
    capacity_ = new_capacity;
}

void CowString::swap(CowString& other) {
    using std::swap;
    swap(size_, other.size_);
    swap(capacity_, other.capacity_);
    swap(s_, other.s_);
    swap(instance_counter_, other.instance_counter_);
}

CowString::CowString(size_t capacity, size_t size = 0)
    : capacity_(capacity), size_(size) {
    char* data_pointer = new char[sizeof(int) + capacity_ + 1];
    instance_counter_ = reinterpret_cast<int*>(data_pointer);
    *instance_counter_ = 1;
    s_ = reinterpret_cast<char*>(data_pointer);
    s_ += sizeof(int);
    s_[size_] = 0;
}

CowString::CowString(const char* other_s) : CowString(strlen(other_s) + 1) {
    int symbols_cnt = 0;
    while (other_s[symbols_cnt] != '\0') {
        symbols_cnt++;
    }
    capacity_ = symbols_cnt;
    size_ = symbols_cnt;
    memcpy(this->s_, other_s, size_ + 1);
}

CowString::CowString(size_t n, char c) : CowString(n, n) {
    std::fill(s_, s_ + n, c);
    s_[size_] = 0;
}

CowString::CowString() : CowString(1) {}

CowString::CowString(const CowString& other)
    : capacity_(other.capacity_),
      size_(other.size_),
      instance_counter_(other.instance_counter_) {
    ++(*instance_counter_);
    s_ = other.s_;
}

CowString& CowString::operator=(CowString other) {
    if (this != &other) {
        this->swap(other);
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

bool operator==(const CowString& a, const CowString& b) {
    if (a.size_ != b.size_) return false;
    if (a.s_ == b.s_) return true;
    return memcmp(a.s_, b.s_, a.size_) == 0;
}

bool operator<(const CowString& a, const CowString& b) {
    if (a.s_ == b.s_) return false;
    size_t border = std::min(a.size_, b.size_);
    int val = memcmp(a.s_, b.s_, border);
    return val < 0 || (val == 0 && a.size_ < b.size_);
}

bool operator>(const CowString& a, const CowString& b) {
    return b < a;
}

bool operator<=(const CowString& a, const CowString& b) {
    return !(a > b);
}

bool operator>=(const CowString& a, const CowString& b) {
    return !(a < b);
}

char& CowString::operator[](int idx) {
    if (*instance_counter_ != 1) {
        self_copy(capacity_);
    }
    return s_[idx];
}

const char& CowString::operator[](int idx) const {
    return s_[idx];
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

    s_[size_++] = c;
    s_[size_] = '\0';
}

void CowString::pop_back() {
    if (*instance_counter_ != 1) self_copy();
    s_[--size_] = '\0';
}

const char& CowString::front() const {
    return s_[0];
}

char& CowString::front() {
    if (*instance_counter_ != 1) self_copy();
    return s_[0];
}

const char& CowString::back() const {
    return s_[size_ - 1];
}

char& CowString::back() {
    if (*instance_counter_ != 1) self_copy();
    return s_[size_ - 1];
}

CowString& CowString::operator+=(char c) {
    push_back(c);
    return *this;
}

CowString& CowString::operator+=(const CowString& other) {
    bool need_realloc = false;
    size_t new_capacity = capacity_;
    if (size_ + other.size_ > capacity_) {
        new_capacity = size_ + other.size_;
        need_realloc = true;
    } else if (*instance_counter_ != 1) {
        need_realloc = true;
    }

    if (need_realloc) {
        self_copy(new_capacity);
    }
    memcpy(s_ + size_, other.s_, other.size_);
    size_ += other.size_;
    s_[size_] = '\0';
    return *this;
}

CowString CowString::operator+(const CowString& other) const {
    CowString result{size_ + other.size_, '0'};
    memcpy(result.s_, s_, size_);
    memcpy(result.s_ + size_, other.s_, other.size_);
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
        if (memcmp(s_ + i, other.s_, other.size_) == 0) {
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
        if (memcmp(s_ + i, other.s_, other.size_) == 0) {
            return i;
        }
    }
    return size_;
}

CowString CowString::substr(int start, unsigned long count) const {
    if (count == 0 || (size_t)start >= size_) {
        return {};
    }
    if (count > size_ - (size_t)start) {
        count = size_ - start;
    }

    CowString result{(size_t)count, '0'};
    memcpy(result.s_, s_ + start, count);
    return result;
}

bool CowString::empty() const {
    return size_ == 0;
}

void CowString::clear() {
    if (*instance_counter_ != 1) self_copy();
    size_ = 0;
    s_[size_] = '\0';
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
    s_[size_] = '\0';
}

char* CowString::data() {
    if (*instance_counter_ != 1) self_copy();
    return s_;
}

const char* CowString::data() const {
    return s_;
}