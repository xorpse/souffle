#pragma once

#include "AnalysisType.h"
#include "AstAnalysis.h"
#include "AstVisitor.h"
#include "TypeConstraint.h"
#include "TypeLattice.h"
#include "TypeSystem.h"
#include <ostream>

namespace souffle {

class AstArgument;

/**
 * A type solver that computes the type for each argument in a given clause.
 **/
class TypeSolver {
public:
    // TODO: change this to take in a clause, then get cosntraints and resolve them all
    // TODO: fix constraint resolution etc.
    // TODO: lattice here because...?
    // TODO: get rid of things afterwrads
    // TODO: program as ref?
    TypeSolver(TypeLattice* lattice, const AstClause* clause, const AstProgram* program,
            std::stringstream* logStream = nullptr)
            : lattice(lattice), clause(clause), program(program) {
        generateConstraints();
        resolveConstraints();
    }

    /** Get the type lattice associated with the type solution */
    TypeLattice* getLattice() const {
        return lattice;
    }

    /** Checks if a type has been computed for the given argument */
    bool hasType(const AstArgument* arg) const {
        return typeMapping.find(arg) != typeMapping.end();
    }

    /** Get the computed type stored in the lattice for the given argument */
    const AnalysisType* getType(const AstArgument* arg) const {
        auto pos = typeMapping.find(arg);
        assert(pos != typeMapping.end() && "argument does not have a type");
        return pos->second;
    }

    /** Set the type of a given argument */
    // TODO: make private?
    void setType(const AstArgument* arg, const AnalysisType* type) {
        typeMapping[arg] = type;
    }

    /** Gets the set of type constraints generated by the clause */
    std::set<TypeConstraint*> getConstraints() const {
        return toPtrSet(constraints);
    }

    void print(std::ostream& out) const {
        for (const auto& pair : typeMapping) {
            assert(pair.first != nullptr && "nullptr argument in type solution");
            assert(pair.second != nullptr && "nullptr analysis type in type solution");
            out << "type(" << *pair.first << ") = " << *pair.second << std::endl;
        }
    }

private:
    // TODO: reorder - maybe get rid of some if possible etc.
    TypeLattice* lattice;
    const AstClause* clause;
    const AstProgram* program;
    std::stringstream* logStream;
    std::set<std::unique_ptr<TypeConstraint>> constraints{};
    std::map<const AstArgument*, const AnalysisType*> typeMapping{};
    std::map<const std::string, const AstVariable*> representatives{};

    /** Adds a constraint that needs to be satisfied by the type solution. */
    void addConstraint(std::unique_ptr<TypeConstraint> constraint) {
        constraints.insert(std::move(constraint));
    }

    /** Generates the set of type constraints associated with the clause */
    void generateConstraints();

    /** Resolves all stored constraints until they are simultaneously satisfied */
    void resolveConstraints();

    /**
     * Gets the canonical representative of a given argument.
     * - For variables, this is a variable pointer that represents all variables
     * bound to the same name.
     * - For non-variables, this is the argument pointer itself.
     **/
    const AstArgument* getRepresentative(const AstArgument* arg);
};

/** Type analysis entrypoint */
class TypeAnalysis : public AstAnalysis {
public:
    TypeAnalysis() = default;

    static constexpr const char* name = "type-analysis";

    void run(const AstTranslationUnit& translationUnit) override;

    void print(std::ostream& out) const {
        if (lattice->isValid()) {
            out << logStream.str();
        } else {
            out << "Unable to run type analysis: valid type lattice could not be constructed";
        }
    }

    /** Get the computed type stored in the lattice for the given argument */
    const AnalysisType* getType(const AstArgument* arg) const {
        auto pos = typeSolutions.find(arg);
        assert(pos != typeSolutions.end() && "argument does not have a type");
        return pos->second;
    }

    /** Get the type lattice associated with the analysis */
    TypeLattice* getLattice() const {
        return lattice.get();
    }

    /** Gets the set of all clauses that have been assigned a type */
    const std::vector<const AstClause*>& getTypedClauses() const {
        return typedClauses;
    }

    /** Checks whether any clauses could not be typechecked */
    bool foundInvalidClauses() const {
        return hasInvalidClauses;
    }

    /** Checks whether a clause can be typechecked in a given program */
    static bool isInvalidClause(const AstProgram* program, const AstClause* clause);

private:
    // TODO: why is this here
    std::unique_ptr<TypeLattice> lattice;
    std::map<const AstArgument*, const AnalysisType*> typeSolutions;
    std::vector<const AstClause*> typedClauses{};
    bool hasInvalidClauses{false};
    std::stringstream logStream{};
};

}  // end of namespace souffle
