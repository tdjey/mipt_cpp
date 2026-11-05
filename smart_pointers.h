#include <cstddef>
#include <memory>

namespace SmartPtrInternals {
struct AbstractRegularControlBlock {
    size_t shared_count;
    size_t weak_count;

    AbstractRegularControlBlock(size_t shared_count, size_t weak_count)
        : shared_count(shared_count),
          weak_count(weak_count) {
    }

    virtual void destruct_object() = 0;

    virtual void self_destruct() = 0;

    virtual ~AbstractRegularControlBlock() {
    }
};

template <typename T>
struct RegularControlBlock : AbstractRegularControlBlock {
    T* obj_ptr = nullptr;

    RegularControlBlock(size_t shared_cnt, size_t weak_cnt, T* obj_ptr)
        : AbstractRegularControlBlock(shared_cnt, weak_cnt),
          obj_ptr(obj_ptr) {
    }

    void destruct_object() override {
        delete obj_ptr;
    }

    void self_destruct() override {
        delete this;
    }

    ~RegularControlBlock() override {
    }
};

template <typename T, typename Deleter>
struct ControlBlockWithDeleter : RegularControlBlock<T> {
    [[no_unique_address]] Deleter del;

    ControlBlockWithDeleter(T* ptr, Deleter del)
        : RegularControlBlock<T>(0, 0, ptr),
          del(del) {
    }

    void destruct_object() override {
        del(RegularControlBlock<T>::obj_ptr);
    }
};

template <typename T, typename Deleter, typename Alloc>
struct ControlBlockCompleteSpecified : ControlBlockWithDeleter<T, Deleter> {
    using AllocType = std::allocator_traits<Alloc>::template rebind_alloc<T>;
    [[no_unique_address]] AllocType alloc;

    ControlBlockCompleteSpecified(T* ptr, Deleter del, Alloc alloc)
        : ControlBlockWithDeleter<T, Deleter>(ptr, del),
          alloc(alloc) {
    }

    void self_destruct() override {
        using BlockAlloc =
            std::allocator_traits<AllocType>::template rebind_alloc<ControlBlockCompleteSpecified>;
        BlockAlloc block_alloc(alloc);
        std::allocator_traits<BlockAlloc>::deallocate(block_alloc, this, 1);
    }
};

template <typename T, typename Alloc = std::allocator<T>>
struct ControlBlockMakeShared : AbstractRegularControlBlock {
    using AllocTraits = std::allocator_traits<Alloc>;
    using BlockAllocType = AllocTraits::template rebind_alloc<ControlBlockMakeShared>;

    T value;
    [[no_unique_address]] BlockAllocType alloc{};

    template <typename... Args>
    ControlBlockMakeShared(Args&&... args)
        : AbstractRegularControlBlock(0, 0),
          value(std::forward<Args>(args)...) {
        Alloc all(alloc);
    }

    void destruct_object() override {
        using ObjectAllocType = AllocTraits::template rebind_alloc<T>;
        ObjectAllocType all(alloc);
        std::allocator_traits<ObjectAllocType>::destroy(all, &value);
    }

    void self_destruct() override {
        std::allocator_traits<BlockAllocType>::deallocate(alloc, this, 1);
    }

    ~ControlBlockMakeShared() override {
    }
};
}  // namespace SmartPtrInternals

template <typename T>
class EnableSharedFromThis;

template <typename T>
class SharedPtr {
private:
    template <typename U>
    friend class SharedPtr;

    template <typename U>
    friend class WeakPtr;

    template <typename U, typename... Args>
    friend SharedPtr<U> makeShared(Args&&...);

    template <typename U, typename Alloc, typename... Args>
    friend SharedPtr<U> allocateShared(Alloc, Args&&...);

    using AbstractRegularControlBlock = SmartPtrInternals::AbstractRegularControlBlock;
    template <typename U>
    using RegularControlBlock = SmartPtrInternals::RegularControlBlock<U>;

    template <typename U, typename Deleter>
    using ControlBlockWithDeleter = SmartPtrInternals::ControlBlockWithDeleter<U, Deleter>;

    template <typename U, typename Deleter, typename Alloc>
    using ControlBlockCompleteSpecified =
        SmartPtrInternals::ControlBlockCompleteSpecified<U, Deleter, Alloc>;

    T* ptr_ = nullptr;
    AbstractRegularControlBlock* block_ptr_ = nullptr;

    explicit SharedPtr(T* ptr, AbstractRegularControlBlock* block_ptr)
        : ptr_(ptr),
          block_ptr_(block_ptr) {
        if (block_ptr_ != nullptr) {
            ++block_ptr_->shared_count;
        }
        if constexpr (std::is_base_of_v<EnableSharedFromThis<T>, T>) {
            ptr_->ptr_ = *this;
        }
    }

    void decrement_shared_count() {
        if (block_ptr_->shared_count != 1) {
            --block_ptr_->shared_count;
            return;
        }

        block_ptr_->destruct_object();
        ptr_ = nullptr;

        if (block_ptr_->weak_count != 0) {
            --block_ptr_->shared_count;
            return;
        }

        block_ptr_->self_destruct();
        block_ptr_ = nullptr;
    }

public:
    void swap(SharedPtr& other) {
        using std::swap;
        swap(ptr_, other.ptr_);
        swap(block_ptr_, other.block_ptr_);
    }

    SharedPtr() = default;

    SharedPtr(T* ptr)
        : SharedPtr(ptr, static_cast<AbstractRegularControlBlock*>(
                             new RegularControlBlock<T>(0, 0, ptr))) {
    }

    template <typename U>
    SharedPtr(U* ptr)
        : SharedPtr(static_cast<T*>(ptr), static_cast<AbstractRegularControlBlock*>(
                                              new RegularControlBlock<U>(1, 0, ptr))) {
    }

    template <typename U>
    SharedPtr(const SharedPtr<U>& other)
        : SharedPtr(static_cast<T*>(other.ptr_), other.block_ptr_) {
    }

    SharedPtr(const SharedPtr& other)
        : SharedPtr(other.ptr_, other.block_ptr_) {
    }

    template <typename U>
    SharedPtr(SharedPtr<U>&& other) {
        ptr_ = std::exchange(other.ptr_, nullptr);
        block_ptr_ = std::exchange(other.block_ptr_, nullptr);
    }

    SharedPtr& operator=(SharedPtr other) {
        if (this == &other) {
            return *this;
        }
        swap(other);
        return *this;
    }

    template <typename U, typename Deleter>
    SharedPtr(U* ptr, Deleter del)
        : SharedPtr(static_cast<T*>(ptr), static_cast<AbstractRegularControlBlock*>(
                                              new ControlBlockWithDeleter<U, Deleter>(ptr, del))) {
    }

    template <typename U, typename Deleter, typename Alloc>
    SharedPtr(U* ptr, Deleter del, Alloc given_alloc)
        : ptr_(static_cast<T*>(ptr)) {
        using BlockType = ControlBlockCompleteSpecified<U, Deleter, Alloc>;
        using AllocType = std::allocator_traits<Alloc>::template rebind_alloc<BlockType>;
        AllocType alloc(given_alloc);
        auto p = std::allocator_traits<AllocType>::allocate(alloc, 1);
        new (p) BlockType(ptr, del, alloc);  // why can't shared ptr use allocator's construct?
        block_ptr_ = p;
        ++block_ptr_->shared_count;
    }

    template <typename U>
    SharedPtr(SharedPtr<U>& other, T* ptr)
        : SharedPtr(ptr, other.block_ptr_) {
    }

    size_t use_count() const {
        return block_ptr_->shared_count;
    }

    T& operator*() const {
        return *ptr_;
    }

    T* operator->() const {
        return ptr_;
    }

    template <typename U>
    void reset(U* ptr) {
        SharedPtr copy(ptr);
        swap(copy);
    }

    void reset() {
        SharedPtr empty;
        swap(empty);
    }

    T* get() const {
        return ptr_;
    }

    ~SharedPtr() {
        if (block_ptr_ == nullptr) {
            return;
        }
        decrement_shared_count();
    }
};

template <typename T, typename... Args>
SharedPtr<T> makeShared(Args&&... args) {
    auto block_ptr = new SmartPtrInternals::ControlBlockMakeShared<T>(std::forward<Args>(args)...);
    SharedPtr result =
        SharedPtr<T>(static_cast<T*>(&(block_ptr->value)),
                     static_cast<SmartPtrInternals::AbstractRegularControlBlock*>(block_ptr));
    return result;
}

template <typename T, typename Alloc, typename... Args>
SharedPtr<T> allocateShared(Alloc given_alloc, Args&&... args) {
    using BlockType = SmartPtrInternals::ControlBlockMakeShared<T, Alloc>;
    using BlockAlloc = std::allocator_traits<Alloc>::template rebind_alloc<BlockType>;
    BlockAlloc alloc(given_alloc);
    auto block_ptr = std::allocator_traits<BlockAlloc>::allocate(alloc, 1);
    std::allocator_traits<BlockAlloc>::construct(alloc, block_ptr, std::forward<Args>(args)...);
    SharedPtr result =
        SharedPtr<T>(static_cast<T*>(&block_ptr->value),
                     static_cast<SmartPtrInternals::AbstractRegularControlBlock*>(block_ptr));
    return result;
}

template <typename T>
class WeakPtr {
private:
    template <typename U>
    friend class WeakPtr;

    using AbstractRegularControlBlock = SmartPtrInternals::AbstractRegularControlBlock;

    template <typename U>
    using RegularControlBlock = SmartPtrInternals::RegularControlBlock<U>;

    T* ptr_ = nullptr;
    AbstractRegularControlBlock* block_ptr_ = nullptr;

    WeakPtr(T* ptr, AbstractRegularControlBlock* block_ptr)
        : ptr_(ptr),
          block_ptr_(block_ptr) {
        if (block_ptr_ != nullptr) {
            ++block_ptr_->weak_count;
        }
    }

    void swap(WeakPtr& other) {
        using std::swap;
        swap(ptr_, other.ptr_);
        swap(block_ptr_, other.block_ptr_);
    }

public:
    WeakPtr() = default;

    WeakPtr(const WeakPtr& other)
        : WeakPtr(other.ptr_, other.block_ptr_) {
    }

    WeakPtr(WeakPtr&& other) {
        ptr_ = std::exchange(other.ptr_, nullptr);
        block_ptr_ = std::exchange(other.block_ptr_, nullptr);
    }

    WeakPtr& operator=(WeakPtr other) {
        if (this == &other) {
            return *this;
        }
        swap(other);
        return *this;
    }

    template <typename U>
    WeakPtr(const SharedPtr<U>& other)
        : WeakPtr(other.ptr_, other.block_ptr_) {
    }

    template <typename U>
    WeakPtr(const WeakPtr<U>& other)
        : WeakPtr(other.ptr_, other.block_ptr_) {
    }

    bool expired() const noexcept {
        return block_ptr_ == nullptr || block_ptr_->shared_count == 0;
    }

    size_t use_count() const noexcept {
        if (expired()) {
            return 0;
        }
        return block_ptr_->shared_count;
    }

    SharedPtr<T> lock() const noexcept {
        if (expired()) {
            return SharedPtr<T>();
        }
        return SharedPtr<T>(ptr_, block_ptr_);
    }

    ~WeakPtr() {
        if (block_ptr_ == nullptr) {
            return;
        }
        --block_ptr_->weak_count;
        if (block_ptr_->shared_count == 0 && block_ptr_->weak_count == 0) {
            block_ptr_->self_destruct();
        }
    }
};

template <typename T>
class EnableSharedFromThis {
    template <typename U>
    friend class SharedPtr;

    template <typename U, typename... Args>
    friend SharedPtr<U> makeShared(Args&&...);

    template <typename U, typename Alloc, typename... Args>
    friend SharedPtr<U> allocateShared(Alloc, Args&&...);

    WeakPtr<T> ptr_{};

public:
    SharedPtr<T> shared_from_this() const {
        if (ptr_.expired()) {
            throw std::bad_weak_ptr();
        }
        return ptr_.lock();
    }
};
