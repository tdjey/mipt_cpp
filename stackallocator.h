#include <cassert>
#include <memory>

template <typename Type, size_t kStackSize>
class StackAllocator;

template <size_t kStackSize>
class StackStorage {
  template <typename, size_t>
  friend class StackAllocator;

  std::byte data_[kStackSize];
  std::byte* allocate_ptr_ = data_;
  size_t space_left_ = kStackSize;

  void* allocate(size_t pool_size, size_t alignment) {
    void* aligned_ptr = static_cast<void*>(allocate_ptr_);

    if (std::align(alignment, pool_size, aligned_ptr, space_left_)) {
      allocate_ptr_ = static_cast<std::byte*>(aligned_ptr);
      std::byte* pool_start = allocate_ptr_;
      allocate_ptr_ += pool_size;
      space_left_ -= pool_size;
      return static_cast<void*>(pool_start);
    }
    throw std::bad_alloc();
  }

  StackStorage(const StackStorage&) = delete;

public:
  StackStorage() = default;
};

template <typename Type, size_t kStackSize>
class StackAllocator {
  StackStorage<kStackSize>* storage_ptr_ = nullptr;

public:
  using value_type = Type;
  StackAllocator(StackStorage<kStackSize>& storage)
      : storage_ptr_(&storage) {
  }

  template <typename, size_t>
  friend class StackAllocator;

  template <typename OtherType>
  StackAllocator(const StackAllocator<OtherType, kStackSize>& given_alloc) noexcept
      : storage_ptr_(given_alloc.storage_ptr_) {
  }

  Type* allocate(size_t pool_size) {
    return static_cast<Type*>(storage_ptr_->allocate(pool_size * sizeof(Type), alignof(Type)));
  }

  template <typename OtherType>
  struct rebind {
    using other = StackAllocator<OtherType, kStackSize>;
  };

  void deallocate(Type*, size_t) noexcept {
  }

  bool operator==(const StackAllocator&) const noexcept = default;
};

template <typename Type, typename Allocator = std::allocator<Type>>
class List {
  struct BaseNode {
    BaseNode* next = this;
    BaseNode* prev = this;
    void swap(BaseNode& other) noexcept {
      std::swap(next, other.next);
      std::swap(prev, other.prev);
    }
  };

  struct Node : BaseNode {
    Type value{};
    Node() {
    }
    Node(const Type& value)
        : value(value) {
    }
  };
  using Traits = std::allocator_traits<Allocator>::template rebind_traits<Node>;
  using NodeAllocator = Traits::allocator_type;

  BaseNode base_node_;
  [[no_unique_address]] NodeAllocator alloc_{};
  size_t size_ = 0;

  void allocator_aware_copy(const List& other, NodeAllocator& new_alloc) {
    size_t prev_size = size_;
    size_ = 0;
    BaseNode old_base_node = base_node_;
    base_node_.next = &base_node_;
    base_node_.prev = &base_node_;
    BaseNode* other_cur = other.base_node_.next;

    try {
      while (other_cur != &other.base_node_) {
        push_back_alloc(static_cast<Node*>(other_cur)->value, new_alloc);
        other_cur = other_cur->next;
      }
      old_base_node.swap(base_node_);
      destroy_nodes();
      old_base_node.swap(base_node_);
      if (alloc_ != new_alloc) {
        alloc_ = new_alloc;
      }
    } catch (...) {
      BaseNode* cur_node = base_node_.next;
      while (cur_node != &base_node_) {
        BaseNode* next_node = cur_node->next;
        Traits::destroy(new_alloc, static_cast<Node*>(cur_node));
        Traits::deallocate(new_alloc, static_cast<Node*>(cur_node), 1);
        cur_node = next_node;
      }
      base_node_.next = old_base_node.next;
      base_node_.prev = old_base_node.prev;
      size_ = prev_size;
      throw;
    }
  }

  void push_back_alloc(const Type& value, NodeAllocator& alloc) {
    insert_node(value, base_node_.prev, &base_node_, alloc);
  }

  void erase_node(const BaseNode* node) noexcept {
    node->prev->next = node->next;
    node->next->prev = node->prev;
    Traits::destroy(alloc_, static_cast<Node*>(const_cast<BaseNode*>(node)));
    Traits::deallocate(alloc_, static_cast<Node*>(const_cast<BaseNode*>(node)), 1);
    --size_;
  }

  void insert_node(const Type& value, BaseNode* prev, BaseNode* next, NodeAllocator& alloc) {
    Node* new_node = Traits::allocate(alloc, 1);
    try {
      Traits::construct(alloc, new_node, value);
    } catch (...) {
      Traits::deallocate(alloc, new_node, 1);
      throw;
    }
    new_node->next = next;
    new_node->prev = prev;
    prev->next = static_cast<BaseNode*>(new_node);
    next->prev = static_cast<BaseNode*>(new_node);
    ++size_;
  }

  void insert_default_constructor(BaseNode* prev, BaseNode* next) {
    Node* new_node = Traits::allocate(alloc_, 1);
    try {
      Traits::construct(alloc_, new_node);
    } catch (...) {
      Traits::deallocate(alloc_, new_node, 1);
      throw;
    }
    new_node->next = next;
    new_node->prev = prev;
    prev->next = static_cast<BaseNode*>(new_node);
    next->prev = static_cast<BaseNode*>(new_node);
    ++size_;
  }

  void destroy_nodes() noexcept {
    BaseNode* cur_node = base_node_.next;
    while (cur_node != &base_node_) {
      BaseNode* next_node = cur_node->next;
      Traits::destroy(alloc_, static_cast<Node*>(cur_node));
      Traits::deallocate(alloc_, static_cast<Node*>(cur_node), 1);
      cur_node = next_node;
    }
    base_node_.next = &base_node_;
    base_node_.prev = &base_node_;
  }

  void allocator_unaware_construct(size_t n, const Type& filler) {
    try {
      for (; n > 0; --n) {
        push_back(filler);
      }
    } catch (...) {
      clear();
      throw;
    };
  }

  void allocator_unaware_default_construct(size_t n) {
    try {
      for (; n > 0; --n) {
        insert_default_constructor(base_node_.prev, &base_node_);
      }
    } catch (...) {
      clear();
      throw;
    };
  }

public:
  List() = default;

  List(size_t n, const Type& filler, const Allocator& given_alloc)
      : alloc_(given_alloc) {
    allocator_unaware_construct(n, filler);
  }

  List(size_t n, const Type& filler) {
    allocator_unaware_construct(n, filler);
  }

  List(const Allocator& given_alloc)
      : alloc_(given_alloc) {
  }

  List(size_t n) {
    allocator_unaware_default_construct(n);
    size_ = n;
  }

  List(size_t n, const Allocator& given_alloc)
      : alloc_(given_alloc) {
    allocator_unaware_default_construct(n);
  }

  List(const Type& filler, const Allocator& given_alloc)
      : List(1, filler, given_alloc) {
  }

  Allocator get_allocator() {
    return Allocator(alloc_);
  }

  List(const List& other)
      : alloc_(Traits::select_on_container_copy_construction(other.alloc_)) {
    allocator_aware_copy(other, alloc_);
  }

  ~List() noexcept {
    destroy_nodes();
  }

  List& operator=(const List& other) {
    if (&other == this) {
      return *this;
    }
    if constexpr (std::is_base_of_v<std::true_type,
                                    typename Traits::propagate_on_container_copy_assignment>) {
      NodeAllocator new_allocator(other.alloc_);
      allocator_aware_copy(other, new_allocator);
    } else {
      allocator_aware_copy(other, alloc_);
    }
    return *this;
  }

  size_t size() const noexcept {
    return size_;
  }

  bool empty() noexcept {
    return size_ == 0;
  }

  void clear() noexcept {
    size_ = 0;
    destroy_nodes();
  }

  void push_back(const Type& value) {
    push_back_alloc(value, alloc_);
  }

  void push_front(const Type& value) {
    insert_node(value, &base_node_, base_node_.next, alloc_);
  }

  void pop_back() noexcept {
    if (empty()) {
      return;
    }
    erase_node(static_cast<Node*>(base_node_.prev));
  }

  void pop_front() {
    if (empty()) {
      return;
    }
    erase_node(static_cast<Node*>(base_node_.next));
  }

  template <bool IsConst>
  class BaseIterator {
  private:
    using PointerType = std::conditional_t<IsConst, const Type*, Type*>;
    using ReferenceType = std::conditional_t<IsConst, const Type&, Type&>;
    using BaseNodeType = std::conditional_t<IsConst, const BaseNode, BaseNode>;
    using NodeType = std::conditional_t<IsConst, const Node, Node>;

    BaseNodeType* node_;
    friend class BaseIterator<!IsConst>;
    friend class List;

  public:
    using value_type = Type;
    using pointer = PointerType;
    using reference = ReferenceType;
    using iterator_category = std::bidirectional_iterator_tag;
    using difference_type = std::ptrdiff_t;

    BaseIterator(BaseNodeType& node)
        : node_(&node) {
    }

    BaseIterator(BaseNodeType* node)
        : node_(node) {
    }

    operator BaseIterator<true>() const {
      return BaseIterator<true>(node_);
    }

    BaseIterator& operator++() {
      node_ = node_->next;
      return *this;
    }

    BaseIterator operator++(int) {
      BaseIterator copy = *this;
      node_ = node_->next;
      return copy;
    }

    BaseIterator& operator--() {
      node_ = node_->prev;
      return *this;
    }

    BaseIterator operator--(int) {
      BaseIterator copy = *this;
      node_ = node_->prev;
      return copy;
    }

    ReferenceType operator*() {
      return static_cast<NodeType*>(node_)->value;
    }

    PointerType operator->() const {
      return *static_cast<NodeType*>(node_)->value;
    }

    template <bool IsOtherConst>
    bool operator==(const BaseIterator<IsOtherConst>& other) const {
      return node_ == other.node_;
    }
  };

  using iterator = BaseIterator<false>;
  using const_iterator = BaseIterator<true>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  iterator begin() {
    return {base_node_.next};
  }

  iterator end() {
    return {base_node_};
  }

  const_iterator begin() const {
    return {base_node_.next};
  }

  const_iterator end() const {
    return {base_node_};
  }

  const_iterator cbegin() const {
    return {base_node_.next};
  }

  const_iterator cend() const {
    return {base_node_};
  }

  reverse_iterator rbegin() {
    return std::make_reverse_iterator(end());
  }

  reverse_iterator rend() {
    return std::make_reverse_iterator(begin());
  }

  const_reverse_iterator rbegin() const {
    return std::make_reverse_iterator(end());
  }

  const_reverse_iterator rend() const {
    return std::make_reverse_iterator(begin());
  }

  const_reverse_iterator crbegin() const {
    return std::make_reverse_iterator(cend());
  }

  const_reverse_iterator crend() const {
    return std::make_reverse_iterator(cbegin());
  }

  template <bool IsConst>
  iterator insert(BaseIterator<IsConst> iter, const Type& value) {
    insert_node(value, const_cast<BaseNode*>(iter.node_->prev),
                const_cast<BaseNode*>(iter.node_), alloc_);
    return iter.node_->prev;
  }

  template <bool IsConst>
  void erase(BaseIterator<IsConst> iter) {
    erase_node(iter.node_);
  }
};