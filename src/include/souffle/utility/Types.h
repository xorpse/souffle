#include <memory>
#include <vector>

namespace souffle {
template <typename A>
using Own = std::unique_ptr<A>;

template <typename A, typename B = A, typename... Args>
Own<A> mk(Args&&... xs) {
    return std::make_unique<B>(std::forward<Args>(xs)...);
}

template <typename A>
using VecOwn = std::vector<Own<A>>;

}  // namespace souffle
