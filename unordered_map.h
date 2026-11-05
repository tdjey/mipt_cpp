#include <iostream>
#include <memory>
#include <utility>
#include <vector>

template <typename Type, typename Allocator = std::allocator<Type>>
class List {
    template <typename, typename, typename, typename, typename>
    friend class UnorderedMap;

    struct BaseNode {
        BaseNode* next = this;
        BaseNode* prev = this;

        BaseNode() {
            next = this;
            prev = this;
        }

        void swap(BaseNode& other) noexcept {
            std::swap(next, other.next);
            std::swap(prev, other.prev);
        }

        void swap_looped(BaseNode& other) noexcept {  // swap with looped node case support
            if (other.looped() && looped()) {
                return;
            }

            bool is_looped = looped();
            bool is_other_looped = other.looped();

            if (!is_looped) {
                next->prev = &other;
                prev->next = &other;
            } else {
                next = &other;
                prev = &other;
            }

            if (!is_other_looped) {
                other.next->prev = this;
                other.prev->next = this;
            } else {
                other.next = this;
                other.prev = this;
            }

            using std::swap;
            swap(next, other.next);
            swap(prev, other.prev);
        }

        bool looped() const {
            return next == this && prev == this;
        }

        BaseNode(BaseNode&& other)
            : next(other.next),
              prev(other.prev) {
            if (other.looped()) {
                next = prev = this;
                return;
            }
            other.next->prev = this;
            other.prev->next = this;
            other.next = &other;
            other.prev = &other;
        }

        BaseNode(const BaseNode& other) = default;
    };

    struct Node : BaseNode {
        Type value;

        Node() = default;

        Node(const Type& value)
            : value(value) {
        }

        Node(Type&& value) noexcept(std::is_nothrow_move_constructible_v<Type>)
            : value(std::move(value)) {
        }

        template <typename... Args>
        Node(Args&&... args)
            : BaseNode(),
              value(std::forward<Args>(args)...) {
        }
    };

    using Traits = std::allocator_traits<Allocator>::template rebind_traits<Node>;
    using NodeAllocator = Traits::allocator_type;

    BaseNode base_node_;
    [[no_unique_address]] NodeAllocator alloc_{};

    template <typename T>
    void push_back_impl(T&& value) {
        insert_node(std::forward<T>(value), base_node_.prev, &base_node_, alloc_);
    }

    void erase_node(const BaseNode* node) noexcept {
        node->prev->next = node->next;
        node->next->prev = node->prev;
        Traits::destroy(alloc_, static_cast<Node*>(const_cast<BaseNode*>(node)));
        Traits::deallocate(alloc_, static_cast<Node*>(const_cast<BaseNode*>(node)), 1);
    }

    template <typename T>
    void insert_node(T&& value, BaseNode* prev, BaseNode* next, NodeAllocator& alloc) {
        Node* new_node = Traits::allocate(alloc, 1);
        try {
            Traits::construct(alloc, new_node, std::forward<T>(value));
        } catch (...) {
            Traits::deallocate(alloc, new_node, 1);
            throw;
        }
        new_node->next = next;
        new_node->prev = prev;
        prev->next = static_cast<BaseNode*>(new_node);
        next->prev = static_cast<BaseNode*>(new_node);
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

    template <typename OtherAlloc>
    List(const List& other, const OtherAlloc& given_alloc)
        : List() {
        alloc_ = given_alloc;
        base_node_.next = &base_node_;
        base_node_.prev = &base_node_;
        BaseNode* other_cur = other.base_node_.next;
        while (other_cur != &other.base_node_) {
            push_back_impl(static_cast<Node*>(other_cur)->value);
            other_cur = other_cur->next;
        }
    }

public:
    List() = default;

    List(const Allocator& given_alloc)
        : alloc_(given_alloc) {
    }

    List(const List& other)
        : List(other, Traits::select_on_container_copy_construction(other.alloc_)) {
    }

    List(List&& other)
        : alloc_(std::move(other.alloc_)) {
        base_node_.swap_looped(other.base_node_);
        other.destroy_nodes();
    }

    ~List() noexcept {
        destroy_nodes();
    }

    List& operator=(const List& other) {
        if (&other == this) {
            return *this;
        }

        NodeAllocator new_allocator;
        if constexpr (std::is_base_of_v<std::true_type,
                                        typename Traits::propagate_on_container_copy_assignment>) {
            new_allocator = other.alloc_;
        } else {
            new_allocator = alloc_;
        }

        try {
            List copy(other, new_allocator);
            swap(copy, true);
        } catch (...) {
            throw;
        }

        return *this;
    }

    List& operator=(List&& other) {
        if (&other == this) {
            return *this;
        }
        if constexpr (Traits::is_always_equal::value) {
            base_node_.swap_looped(other.base_node_);
            other.destroy_nodes();
            return *this;
        } else {
            alloc_ = std::is_base_of_v<std::true_type,
                                       typename Traits::propagate_on_container_move_assignment>
                         ? alloc_
                         : other.alloc_;

            if constexpr (!std::is_nothrow_move_constructible_v<Type> &&
                          std::is_copy_constructible_v<Type>) {
                *this = other;
                return *this;
            }
            destroy_nodes();
            base_node_.next = &base_node_;
            base_node_.prev = &base_node_;

            BaseNode* other_cur = other.base_node_.next;
            while (other_cur != &other.base_node_) {
                push_back(std::move(static_cast<Node*>(other_cur)->value));
                other_cur = other_cur->next;
            }
            other.destroy_nodes();
            other.base_node_.next = &other.base_node_;
            other.base_node_.prev = &other.base_node_;
            return *this;
        }
    }

    void swap(List& other, bool alloc_propagation) noexcept {
        base_node_.swap_looped(other.base_node_);
        if (alloc_propagation) {
            std::swap(alloc_, other.alloc_);
        }
    }

    void clear() noexcept {
        destroy_nodes();
    }

    void push_back(const Type& value) {
        push_back_impl(value);
    }

    void push_back(Type&& value) {
        push_back_impl(std::move(value));
    }

    template <bool IsConst>
    class BaseIterator;

    using iterator = BaseIterator<false>;
    using const_iterator = BaseIterator<true>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

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
        using value_type = std::conditional_t<IsConst, const Type, Type>;
        using pointer = PointerType;
        using reference = ReferenceType;
        using iterator_category = std::bidirectional_iterator_tag;
        using difference_type = std::ptrdiff_t;

        BaseIterator() = default;

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

        ReferenceType operator*() const {
            return static_cast<NodeType*>(node_)->value;
        }

        PointerType operator->() const {
            return &static_cast<NodeType*>(node_)->value;
        }

        template <bool IsOtherConst>
        bool operator==(const BaseIterator<IsOtherConst>& other) const {
            return node_ == other.node_;
        }
    };

    iterator begin() {
        return {base_node_.next};
    }

    iterator end() noexcept {
        return {base_node_};
    }

    const_iterator begin() const {
        return {base_node_.next};
    }

    const_iterator end() const noexcept {
        return {base_node_};
    }

    void* end_address() {
        return end().node_;
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
    void erase(BaseIterator<IsConst> iter) noexcept {
        erase_node(iter.node_);
    }

    template <typename... Args>
    iterator emplace_node_construct(Args&&... args) {
        Node* new_node = Traits::allocate(alloc_, 1);
        try {
            Traits::construct(alloc_, new_node, std::forward<Args>(args)...);
        } catch (...) {
            Traits::deallocate(alloc_, new_node, 1);
            throw;
        }
        return new_node;
    }

    void link_after(iterator target_iter, iterator position_iter) noexcept {
        BaseNode* target = target_iter.node_;
        BaseNode* position = position_iter.node_;
        target->prev->next = target->next;
        target->next->prev = target->prev;
        target->next = position->next;
        target->next->prev = target;
        position->next = target;
        target->prev = position;
    }

    Type& back() {
        return *rbegin();
    }
};

template <typename Key, typename Value, typename Hash = std::hash<Key>,
          typename Equal = std::equal_to<Key>,
          typename Allocator = std::allocator<std::pair<const Key, Value>>>
class UnorderedMap {
public:
    using NodeType = std::pair<const Key, Value>;

private:
    struct Node {
        NodeType data;
        size_t hash;

        template <typename T>
        Node(T&& data, size_t hash)
            : data(std::forward<T>(data)),
              hash(hash) {
        }

        template <typename NodeKey, typename NodeValue>
        Node(NodeKey&& key, NodeValue&& value, size_t hash)
            : data(std::forward<Key>(key), std::forward<NodeValue>(value)),
              hash(hash) {
        }
    };

    using Traits = std::allocator_traits<Allocator>::template rebind_traits<Node>;
    using NodeAllocator = Traits::allocator_type;
    using ListType = List<Node, NodeAllocator>;
    using ListIteratorAllocator = Traits::template rebind_alloc<typename ListType::iterator>;

    ListType nodes_;
    std::vector<typename ListType::iterator, ListIteratorAllocator> buckets_;
    [[no_unique_address]] NodeAllocator alloc_;
    [[no_unique_address]] Hash hash_;
    [[no_unique_address]] Equal equal_;
    size_t size_ = 0;
    size_t buckets_cnt_ = 0;
    float max_load_factor_ = 1.;

private:
    void rehash() {
        buckets_.assign(buckets_cnt_, nodes_.end());
        for (auto iter = nodes_.begin(); iter != nodes_.end();) {
            int cur_hash = iter->hash % buckets_cnt_;
            auto cur_iter = iter++;
            if (buckets_[cur_hash] == nodes_.end()) {
                buckets_[cur_hash] = cur_iter;
                continue;
            }
            nodes_.link_after(cur_iter, buckets_[cur_hash]);
        }
    }

    UnorderedMap(Allocator alloc, Hash hash, Equal equal)
        : nodes_(static_cast<NodeAllocator>(alloc)),
          buckets_(static_cast<ListIteratorAllocator>(alloc)),
          alloc_(alloc),
          hash_(hash),
          equal_(equal) {
    }

    void calc_buckets() {
        for (auto iter = nodes_.begin(); iter != nodes_.end(); ++iter) {
            if (size_t cur_hash = iter->hash % buckets_cnt_; buckets_[cur_hash] == nodes_.end()) {
                buckets_[cur_hash] = iter;
            }
        }
    }

public:
    UnorderedMap() = default;

    UnorderedMap(size_t buckets_cnt)
        : buckets_cnt_(buckets_cnt) {
        buckets_.assign(buckets_cnt_, nodes_.end());
    }

    UnorderedMap(const UnorderedMap& other)
        : UnorderedMap(Traits::select_on_container_copy_construction(other.alloc_), other.hash_,
                       other.equal_) {
        for (const Node& node : other.nodes_) {
            nodes_.push_back(node);
        }
        size_ = other.size_;
        buckets_cnt_ = other.buckets_cnt_;
        max_load_factor_ = other.max_load_factor_;
        buckets_.resize(buckets_cnt_, nodes_.end());
        calc_buckets();
    }

    UnorderedMap(UnorderedMap&& other)
        : nodes_(std::move(other.nodes_)),
          buckets_(std::move(other.buckets_)),
          alloc_(std::move(other.alloc_)),
          hash_(std::move(other.hash_)),
          equal_(std::move(other.equal_)),
          size_(other.size_),
          buckets_cnt_(other.buckets_cnt_),
          max_load_factor_(other.max_load_factor_) {
        for (auto& it : buckets_) {
            if (it == other.nodes_.end()) {
                it = nodes_.end();
            }
        }
        other.size_ = 0;
        other.buckets_cnt_ = 0;
    }

private:
    template <typename MapType>
    void forwarding_assignment(MapType&& other) {
        NodeAllocator node_alloc(alloc_);
        decltype(nodes_) nodes(node_alloc);
        nodes = std::forward_like<MapType>(
            other.nodes_);  // assuming that traits for different allocators are the same
        buckets_ = std::forward_like<MapType>(other.buckets_);

        // noexcept part
        size_ = other.size_;
        buckets_cnt_ = other.buckets_cnt_;
        max_load_factor_ = other.max_load_factor_;

        for (auto& node_iter : buckets_) {
            node_iter = nodes_.end();
        }
        nodes_.swap(nodes, true);
        calc_buckets();
    }

public:
    UnorderedMap& operator=(
        const UnorderedMap& other) {  // не могу copy-and-swap из-за allocator-aware
        if (this == &other) {
            return *this;
        }
        forwarding_assignment(other);
        return *this;
    }

    UnorderedMap& operator=(UnorderedMap&& other) {
        if (this == &other) {
            return *this;
        }
        forwarding_assignment(std::move(other));
        other.size_ = 0;
        other.buckets_cnt_ = 0;
        other.buckets_.clear();
        other.nodes_.clear();
        return *this;
    }

    ~UnorderedMap() = default;

    void swap(UnorderedMap& other) {
        constexpr bool kAllocSwap =
            std::is_base_of_v<std::true_type, typename Traits::propagate_on_container_swap>;
        nodes_.swap(other.nodes_, kAllocSwap);
        buckets_.swap(other.buckets_);
        using std::swap;
        swap(size_, other.size_);
        swap(buckets_cnt_, other.buckets_cnt_);
        swap(max_load_factor_, other.max_load_factor_);
        if constexpr (kAllocSwap) {
            std::swap(alloc_, other.alloc_);
        }
    }

    size_t size() {
        return size_;
    }

    bool empty() {
        return size_ == 0;
    }

    template <bool IsConst>
    class BaseIterator;

    using iterator = BaseIterator<false>;
    using const_iterator = BaseIterator<true>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator begin() {
        return {nodes_.begin()};
    }

    iterator end() {
        return {nodes_.end()};
    }

    const_iterator begin() const {
        return {nodes_.begin()};
    }

    const_iterator end() const noexcept {
        return {nodes_.end()};
    }

    const_iterator cbegin() const {
        return {nodes_.begin()};
    }

    const_iterator cend() const {
        return {nodes_.end()};
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

public:
    std::pair<iterator, bool> insert(const NodeType& data) {
        return emplace(data);
    }

    template <typename T>
    std::pair<iterator, bool> insert(T&& data) {
        return emplace(std::forward<T>(data));
    }

    std::pair<iterator, bool> insert(NodeType&& data) {
        return emplace(std::move(data));
    }

    void erase(iterator iter) {
        size_t hash = iter.list_iter_->hash % buckets_cnt_;
        NodeType& data = *iter;
        if (equal_(buckets_[hash]->data.first, data.first)) {
            auto nodes_iter = buckets_[hash];
            ++nodes_iter;
            if (nodes_iter == nodes_.end() || nodes_iter->hash != hash) {
                buckets_[hash] = nodes_.end();
            } else {
                buckets_[hash] = nodes_iter;
            }
        }
        nodes_.erase(iter.list_iter_);
        --size_;
    }

    void erase(iterator first, iterator last) {
        while (first != last) {
            erase(first++);
        }
    }

    template <typename InputIterator>
    void insert(InputIterator first, InputIterator last) {
        static_assert(
            std::is_convertible_v<typename std::iterator_traits<InputIterator>::iterator_category,
                                  std::input_iterator_tag>);
        InputIterator cur(first);
        ListIteratorAllocator iter_alloc(alloc_);
        std::vector<typename ListType::iterator, ListIteratorAllocator> new_nodes(iter_alloc);
        try {
            while (cur != last) {
                auto [new_node_iter, was_added] = emplace(*cur);
                if (was_added) {
                    new_nodes.push_back(new_node_iter.list_iter_);
                }
                ++cur;
            }
        } catch (...) {
            for (typename ListType::iterator iter : new_nodes) {
                erase(iter);
            }
            throw;
        }
    }

    template <typename T>
    std::pair<iterator, bool> insert_node(T&& node) {
        auto iter = find(node.data.first, node.hash);
        if (iter != end()) {
            return {iter, false};
        }
        nodes_.push_back(std::forward<T>(node));
        auto node_iter = std::prev(nodes_.end());
        ++size_;
        return {iterator(node_iter), true};
    }

    template <typename... Args>
    std::pair<iterator, bool> emplace(Args&&... args) {
        using PairAlloc = Traits::template rebind_alloc<std::pair<Key, Value>>;
        PairAlloc pair_alloc(alloc_);
        std::pair<Key, Value>* p = std::allocator_traits<PairAlloc>::allocate(pair_alloc, 1);

        try {
            std::allocator_traits<PairAlloc>::construct(pair_alloc, p, std::forward<Args>(args)...);
        } catch (...) {
            std::allocator_traits<PairAlloc>::deallocate(pair_alloc, p, 1);
            throw;
        }

        size_t hash = hash_(p->first);
        auto iter = find(p->first, hash);
        if (iter != end()) {
            std::allocator_traits<PairAlloc>::deallocate(pair_alloc, p, 1);
            return {iter, false};
        }

        auto new_node_iter = nodes_.emplace_node_construct(std::move(*p), hash);
        std::allocator_traits<PairAlloc>::destroy(pair_alloc, p);
        std::allocator_traits<PairAlloc>::deallocate(pair_alloc, p, 1);
        ++size_;

        if (load_factor() > max_load_factor_) {
            while (load_factor() > max_load_factor_) {
                buckets_cnt_ = buckets_cnt_ ? buckets_cnt_ * 2 : 2;
            }
            nodes_.link_after(new_node_iter, nodes_.begin());
            rehash();
            return {new_node_iter, true};
        }

        if (buckets_[hash % buckets_cnt_] != nodes_.end()) {
            nodes_.link_after(new_node_iter, buckets_[hash % buckets_cnt_]);
        } else {
            nodes_.link_after(new_node_iter, nodes_.begin());
            buckets_[hash % buckets_cnt_] = new_node_iter;
        }

        return {new_node_iter, true};
    }

private:
    const_iterator find(const Key& key, size_t hash) const {
        if (buckets_cnt_ == 0) {
            return end();
        }
        auto iter = buckets_[hash % buckets_cnt_];
        while (iter != nodes_.end() && iter->hash == hash) {
            if (equal_(key, iter->data.first)) {
                return iterator(iter);
            }
            ++iter;
        }
        return end();
    }

    iterator find(const Key& key, size_t hash) {
        if (buckets_cnt_ == 0) {
            return end();
        }
        auto iter = buckets_[hash % buckets_cnt_];
        while (iter != nodes_.end() && iter->hash % buckets_cnt_ == hash % buckets_cnt_) {
            if (equal_(key, iter->data.first)) {
                return iterator(iter);
            }
            ++iter;
        }
        return end();
    }

public:
    iterator find(const Key& key) {
        size_t hash = hash_(key);
        return find(key, hash);
    }

private:
    template <typename T>
    Value& subscript_operator_impl(T&& key) {
        size_t hash = hash_(key);
        auto iter = find(key, hash);
        if (iter == end()) {
            iter = emplace(std::forward<T>(key), Value()).first;
        }
        return iter->second;
    }

public:
    Value& operator[](const Key& key) {
        return subscript_operator_impl(key);
    }

    Value& operator[](Key&& key) {
        return subscript_operator_impl(std::move(key));
    }

    const Value& operator[](const Key& key) const {
        return subscript_operator_impl(key);
    }

    const Value& operator[](Key&& key) const {
        return subscript_operator_impl(std::move(key));
    }

    const Value& at(const Key& key) const {
        size_t hash = hash_(key);
        auto iter = find(key, hash);
        if (iter == end()) {
            throw "the provided key is not in the map";
        }
        return iter->second;
    }

    Value& at(const Key& key) {
        size_t hash = hash_(key);
        auto iter = find(key, hash);
        if (iter == end()) {
            throw "the provided key is not in the map";
        }
        return iter->second;
    }

    float load_factor() const noexcept {
        return static_cast<float>(size_) / static_cast<float>(buckets_cnt_);
    }

    float max_load_factor() const noexcept {
        return max_load_factor_;
    }

    void max_load_factor(float max_load_factor) {
        max_load_factor_ = max_load_factor;
    }

    void reserve(size_t count) {
        buckets_cnt_ = std::ceilf(count / max_load_factor_);
        rehash();
    }
};

template <typename Key, typename Value, typename Hash, typename Equal, typename Allocator>
template <bool IsConst>
class UnorderedMap<Key, Value, Hash, Equal, Allocator>::BaseIterator {
private:
    using RealIteratorType =
        std::conditional_t<IsConst, typename ListType::const_iterator, typename ListType::iterator>;
    RealIteratorType list_iter_;
    friend class BaseIterator<!IsConst>;
    friend class UnorderedMap;

public:
    using value_type = std::conditional_t<IsConst, const NodeType, NodeType>;
    using pointer = value_type*;
    using reference = value_type&;
    using iterator_category = std::bidirectional_iterator_tag;
    using difference_type = std::ptrdiff_t;

    BaseIterator() {
    }

    BaseIterator(RealIteratorType list_iter)
        : list_iter_(list_iter) {
    }

    BaseIterator& operator=(const RealIteratorType& other) {
        list_iter_ = other.list_iter_;
    }

    operator BaseIterator<true>() const {
        return BaseIterator<true>(list_iter_);
    }

    BaseIterator& operator++() {
        ++list_iter_;
        return *this;
    }

    BaseIterator operator++(int) {
        BaseIterator copy = *this;
        ++list_iter_;
        return copy;
    }

    BaseIterator& operator--() {
        --list_iter_;
        return *this;
    }

    BaseIterator operator--(int) {
        BaseIterator copy = *this;
        --list_iter_;
        return copy;
    }

    value_type& operator*() const {
        return list_iter_->data;
    }

    pointer operator->() const {
        return &(list_iter_->data);
    }

    template <bool IsOtherConst>
    bool operator==(const BaseIterator<IsOtherConst>& other) const {
        return list_iter_ == other.list_iter_;
    }
};
