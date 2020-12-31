#include "ast/Node.h"
#include <typeinfo>
#include <utility>

namespace souffle::ast {
Node::Node(SrcLocation loc) : location(std::move(loc)) {}

/** Set source location for the Node */
void Node::setSrcLoc(SrcLocation l) {
    location = std::move(l);
}

bool Node::operator==(const Node& other) const {
    if (this == &other) {
        return true;
    }

    return typeid(*this) == typeid(*&other) && equal(other);
}

Own<Node> Node::clone() const {
    return Own<Node>(cloneImpl());
}

/** Apply the mapper to all child nodes */
void Node::apply(const NodeMapper& /* mapper */) {}

Node::ConstChildNodes Node::getChildNodes() const {
    return ConstChildNodes(getChildNodesImpl(), detail::RefCaster());
}

Node::ChildNodes Node::getChildNodes() {
    return ChildNodes(getChildNodesImpl(), detail::ConstCaster());
}

std::ostream& operator<<(std::ostream& out, const Node& node) {
    node.print(out);
    return out;
}

bool Node::equal(const Node&) const {
    // FIXME: Change to this == &other?
    return true;
}

Node::NodeVec Node::getChildNodesImpl() const {
    return {};
}

}  // namespace souffle::ast
