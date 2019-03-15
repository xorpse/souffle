#pragma once

#include "AnalysisType.h"
#include "TypeSystem.h"

namespace souffle {

class TypeLattice {
public:
    TypeLattice() = delete;

    TypeLattice(const TypeEnvironment* typeEnvironment) {
        aliases["number"] = getStoredType(TopPrimitiveAnalysisType(Kind::NUMBER));
        aliases["symbol"] = getStoredType(TopPrimitiveAnalysisType(Kind::SYMBOL));
        for (const Type& type : typeEnvironment->getAllTypes()) {
            addType(type);
        }
    }

    /** Checks whether the constructed type lattice is valid */
    const bool isValid() const {
        return valid;
    }

    // TODO: note that the referenced analysis type will be in the lattice
    /** Finds the highest common subtype (intersection) of two types */
    const AnalysisType* meet(const AnalysisType* first, const AnalysisType* second);

    /** Finds the lowest common supertype (union) of two types */
    const AnalysisType* join(const AnalysisType* first, const AnalysisType* second);

    /** Checks if lhs is a subtype of rhs */
    bool isSubtype(const AnalysisType* lhs, const AnalysisType* rhs) const;
    bool isSubtype(const AnalysisType& lhs, const AnalysisType& rhs) const;

    /** Gets the type environment associated with the type lattice */
    const TypeEnvironment* getTypeEnvironment() const {
        // TODO: do you need this???
        return typeEnvironment;
    }

    /**
     * Gets the equivalent type stored in the lattice.
     * If the type does not yet exist in the lattice, it is created.
     */
    // TODO: make sure this is used everywhere it should be used
    // TODO: make a note on when it should be used? probs only in meet/join... also assert that the first and
    // second are also in the lattice? change things up...
    // TODO: what if same name but diff type? e.g. with records
    template <typename T>
    T* getStoredType(const T& type);

    /**
     * Gets the equivalent analysis type stored in the lattice.
     * If the type does not yet exist in the lattice, it is created.
     */
    const InnerAnalysisType* getAnalysisType(const AstTypeIdentifier& type);
    const InnerAnalysisType* getAnalysisType(const Type& type);

private:
    // TODO: add a comparator here
    // TODO: const?
    // TODO: const for all these unique pointers throughout?
    std::set<std::unique_ptr<AnalysisType>> storedTypes{};
    const TypeEnvironment* typeEnvironment;
    bool valid{true};
    std::map<AstTypeIdentifier, const InnerAnalysisType*> aliases{};

    /** Adds an analysis type to the lattice corresponding to the given type */
    const InnerAnalysisType* addType(const Type& type);
};

}  // end of namespace souffle
