/************************************************************************
*
* CppSharp
* Licensed under the MIT license.
*
************************************************************************/

#include "AST.h"
#include <string>
#include <vector>
#include <llvm/ADT/SmallString.h>
#include <llvm/Support/Path.h>

// copy from widenPath ('llvm/lib/Support/Windows/Path.inc')
static std::string normalizePath(const std::string & File) {
    llvm::SmallString<2 * 128> Result;

    for (llvm::sys::path::const_iterator I = llvm::sys::path::begin(File),
        E = llvm::sys::path::end(File);
        I != E; ++I) {
        if (I->size() == 1 && *I == ".")
            continue;
        if (I->size() == 2 && *I == "..")
            llvm::sys::path::remove_filename(Result);
        else
            llvm::sys::path::append(Result, *I);
    }

#ifdef _WIN32
    // Clean up the file path.
    std::replace(Result.begin(), Result.end(), '/', '\\');
#endif

    return Result.c_str();
}

template<typename T>
static std::vector<T> split(const T & str, const T & delimiters) {
    std::vector<T> v;
    if (str.length() == 0) {
        v.push_back(str);
        return v;
    }
    typename T::size_type start = 0;
    auto pos = str.find_first_of(delimiters, start);
    while(pos != T::npos) {
        if(pos != start) // ignore empty tokens
            v.emplace_back(str, start, pos - start);
        start = pos + 1;
        pos = str.find_first_of(delimiters, start);
    }
    if(start < str.length()) // ignore trailing delimiter
       // add what's left of the string
        v.emplace_back(str, start, str.length() - start);
    return v;
}

namespace CppSharp { namespace CppParser { namespace AST {

static void deleteExpression(ExpressionObsolete* expression)
{
    if (expression)
    {
        // HACK: see https://github.com/mono/CppSharp/issues/598
        switch (expression->_class)
        {
        case StatementClassObsolete::BinaryOperator:
            delete static_cast<BinaryOperatorObsolete*>(expression);
            break;
        case StatementClassObsolete::CallExprClass:
            delete static_cast<CallExprObsolete*>(expression);
            break;
        case StatementClassObsolete::CXXConstructExprClass:
            delete static_cast<CXXConstructExprObsolete*>(expression);
            break;
        default:
            delete expression;
            break;
        }
    }
}

Type::Type(TypeKind kind) : kind(kind) {}
Type::Type(const Type& rhs) : kind(rhs.kind), isDependent(rhs.isDependent) {}

QualifiedType::QualifiedType() : type(0) {}

TagType::TagType() : Type(TypeKind::Tag) {}

ArrayType::ArrayType() : Type(TypeKind::Array), size(0), elementSize(0) {}

FunctionType::FunctionType()
    : Type(TypeKind::Function)
    , callingConvention(CallingConvention::Default)
    , exceptionSpecType(ExceptionSpecType::None)
{
}

FunctionType::~FunctionType() {}

DEF_VECTOR(FunctionType, Parameter*, Parameters)

PointerType::PointerType() : Type(TypeKind::Pointer) {}

MemberPointerType::MemberPointerType() : Type(TypeKind::MemberPointer) {}

TypedefType::TypedefType() : Type(TypeKind::Typedef), declaration(0) {}

AttributedType::AttributedType() : Type(TypeKind::Attributed) {}

DecayedType::DecayedType() : Type(TypeKind::Decayed) {}

// Template
TemplateParameter::TemplateParameter(DeclarationKind kind)
    : Declaration(kind)
    , depth(0)
    , index(0)
    , isParameterPack(false)
{
}

TemplateParameter::~TemplateParameter()
{
}

TemplateTemplateParameter::TemplateTemplateParameter()
    : Template(DeclarationKind::TemplateTemplateParm)
    , isParameterPack(false)
    , isPackExpansion(false)
    , isExpandedParameterPack(false)
{
}

TemplateTemplateParameter::~TemplateTemplateParameter()
{
}

// TemplateParameter
TypeTemplateParameter::TypeTemplateParameter()
    : TemplateParameter(DeclarationKind::TemplateTypeParm)
{
}

TypeTemplateParameter::TypeTemplateParameter(const TypeTemplateParameter& rhs)
    : TemplateParameter(rhs.kind)
    , defaultArgument(rhs.defaultArgument)
{
}

TypeTemplateParameter::~TypeTemplateParameter() {}

NonTypeTemplateParameter::NonTypeTemplateParameter()
    : TemplateParameter(DeclarationKind::NonTypeTemplateParm)
    , defaultArgument(0)
    , position(0)
    , isPackExpansion(false)
    , isExpandedParameterPack(false)
{
}

NonTypeTemplateParameter::NonTypeTemplateParameter(const NonTypeTemplateParameter& rhs)
    : TemplateParameter(rhs.kind)
    , defaultArgument(rhs.defaultArgument)
    , position(rhs.position)
    , isPackExpansion(rhs.isPackExpansion)
    , isExpandedParameterPack(rhs.isExpandedParameterPack)
{
}

NonTypeTemplateParameter::~NonTypeTemplateParameter()
{
    deleteExpression(defaultArgument);
}

TemplateArgument::TemplateArgument() : declaration(0), integral(0) {}

TemplateSpecializationType::TemplateSpecializationType()
    : Type(TypeKind::TemplateSpecialization), _template(0) {}

TemplateSpecializationType::TemplateSpecializationType(
    const TemplateSpecializationType& rhs) : Type(rhs),
    Arguments(rhs.Arguments), _template(rhs._template), desugared(rhs.desugared) {}

TemplateSpecializationType::~TemplateSpecializationType() {}

DEF_VECTOR(TemplateSpecializationType, TemplateArgument, Arguments)

DependentTemplateSpecializationType::DependentTemplateSpecializationType()
    : Type(TypeKind::DependentTemplateSpecialization) {}

DependentTemplateSpecializationType::DependentTemplateSpecializationType(
    const DependentTemplateSpecializationType& rhs) : Type(rhs),
    Arguments(rhs.Arguments), desugared(rhs.desugared) {}

DependentTemplateSpecializationType::~DependentTemplateSpecializationType() {}

DEF_VECTOR(DependentTemplateSpecializationType, TemplateArgument, Arguments)

TemplateParameterType::TemplateParameterType() : Type(TypeKind::TemplateParameter), parameter(0) {}

TemplateParameterType::~TemplateParameterType() {}

TemplateParameterSubstitutionType::TemplateParameterSubstitutionType()
    : Type(TypeKind::TemplateParameterSubstitution), replacedParameter(0) {}

InjectedClassNameType::InjectedClassNameType()
    : Type(TypeKind::InjectedClassName)
    , _class(0)
{
}

DependentNameType::DependentNameType() : Type(TypeKind::DependentName) {}

DependentNameType::~DependentNameType() {}

PackExpansionType::PackExpansionType() : Type(TypeKind::PackExpansion) {}

UnaryTransformType::UnaryTransformType() : Type(TypeKind::UnaryTransform) {}

UnresolvedUsingType::UnresolvedUsingType() : Type(TypeKind::UnresolvedUsing) {}

VectorType::VectorType() : Type(TypeKind::Vector), numElements(0) {}

BuiltinType::BuiltinType() : CppSharp::CppParser::AST::Type(TypeKind::Builtin) {}

VTableComponent::VTableComponent() : offset(0), declaration(0) {}

// VTableLayout
VTableLayout::VTableLayout() {}
VTableLayout::VTableLayout(const VTableLayout& rhs) : Components(rhs.Components) {}
VTableLayout::~VTableLayout() {}

DEF_VECTOR(VTableLayout, VTableComponent, Components)

VFTableInfo::VFTableInfo() : VBTableIndex(0), VFPtrOffset(0), VFPtrFullOffset(0) {}
VFTableInfo::VFTableInfo(const VFTableInfo& rhs) : VBTableIndex(rhs.VBTableIndex),
    VFPtrOffset(rhs.VFPtrOffset), VFPtrFullOffset(rhs.VFPtrFullOffset),
    layout(rhs.layout) {}

LayoutField::LayoutField() : offset(0), fieldPtr(0) {}

LayoutField::LayoutField(const LayoutField & other)
    : offset(other.offset)
    , name(other.name)
    , qualifiedType(other.qualifiedType)
    , fieldPtr(other.fieldPtr)
{
}

LayoutField::~LayoutField() {}

LayoutBase::LayoutBase() : offset(0), _class(0) {}

LayoutBase::LayoutBase(const LayoutBase& other) : offset(other.offset), _class(other._class) {}

LayoutBase::~LayoutBase() {}

ClassLayout::ClassLayout() : ABI(CppAbi::Itanium), argABI(RecordArgABI::Default),
    hasOwnVFPtr(false), VBPtrOffset(0), alignment(0), size(0), dataSize(0) {}

DEF_VECTOR(ClassLayout, VFTableInfo, VFTables)

DEF_VECTOR(ClassLayout, LayoutField, Fields)

DEF_VECTOR(ClassLayout, LayoutBase, Bases)

Declaration::Declaration(DeclarationKind kind)
    : kind(kind)
    , access(AccessSpecifier::Public)
    , _namespace(0)
    , location(0)
    , lineNumberStart(0)
    , lineNumberEnd(0)
    , comment(0)
    , isIncomplete(false)
    , isDependent(false)
    , isImplicit(false)
    , isInvalid(false)
    , isDeprecated(false)
    , completeDeclaration(0)
    , definitionOrder(0)
    , originalPtr(0)
    , alignAs(0)
    , maxFieldAlignment(0)    
{
}

Declaration::Declaration(const Declaration& rhs)
    : kind(rhs.kind)
    , access(rhs.access)
    , _namespace(rhs._namespace)
    , location(rhs.location.ID)
    , lineNumberStart(rhs.lineNumberStart)
    , lineNumberEnd(rhs.lineNumberEnd)
    , name(rhs.name)
    , comment(rhs.comment)
    , debugText(rhs.debugText)
    , isIncomplete(rhs.isIncomplete)
    , isDependent(rhs.isDependent)
    , isImplicit(rhs.isImplicit)
    , isInvalid(rhs.isInvalid)
    , isDeprecated(rhs.isDeprecated)
    , completeDeclaration(rhs.completeDeclaration)
    , definitionOrder(rhs.definitionOrder)
    , PreprocessedEntities(rhs.PreprocessedEntities)
    , originalPtr(rhs.originalPtr)
{
}

Declaration::~Declaration()
{
}

DEF_VECTOR(Declaration, PreprocessedEntity*, PreprocessedEntities)
DEF_VECTOR(Declaration, Declaration*, Redeclarations)

DeclarationContext::DeclarationContext(DeclarationKind kind)
    : Declaration(kind)
    , isAnonymous(false)
{}

DEF_VECTOR(DeclarationContext, Namespace*, Namespaces)
DEF_VECTOR(DeclarationContext, Enumeration*, Enums)
DEF_VECTOR(DeclarationContext, Function*, Functions)
DEF_VECTOR(DeclarationContext, Class*, Classes)
DEF_VECTOR(DeclarationContext, Template*, Templates)
DEF_VECTOR(DeclarationContext, TypedefDecl*, Typedefs)
DEF_VECTOR(DeclarationContext, TypeAlias*, TypeAliases)
DEF_VECTOR(DeclarationContext, Variable*, Variables)
DEF_VECTOR(DeclarationContext, Friend*, Friends)

Declaration* DeclarationContext::FindAnonymous(const std::string& key)
{
    auto it = anonymous.find(key);
    return (it != anonymous.end()) ? it->second : 0;
}

Namespace* DeclarationContext::FindNamespace(const std::string& Name)
{
    auto namespaces = split<std::string>(Name, "::");
    return FindNamespace(namespaces);
}

Namespace*
DeclarationContext::FindNamespace(const std::vector<std::string>& Namespaces)
{
    auto currentNamespace = this;
    for (auto I = Namespaces.begin(), E = Namespaces.end(); I != E; ++I)
    {
        auto& _namespace = *I;

        auto childNamespace = std::find_if(currentNamespace->Namespaces.begin(),
            currentNamespace->Namespaces.end(),
            [&](CppSharp::CppParser::AST::Namespace* ns) {
                return ns->name == _namespace;
        });

        if (childNamespace == currentNamespace->Namespaces.end())
            return nullptr;

        currentNamespace = *childNamespace;
    }

    return (CppSharp::CppParser::AST::Namespace*) currentNamespace;
}

Namespace* DeclarationContext::FindCreateNamespace(const std::string& Name)
{
    auto _namespace = FindNamespace(Name);

    if (!_namespace)
    {
        _namespace = new Namespace();
        _namespace->name = Name;
        _namespace->_namespace = this;

        Namespaces.push_back(_namespace);
    }

    return _namespace;
}

Class* DeclarationContext::FindClass(const void* OriginalPtr,
    const std::string& Name, bool IsComplete)
{
    if (Name.empty()) return nullptr;

    auto entries = split<std::string>(Name, "::");

    if (entries.size() == 1)
    {
        auto _class = std::find_if(Classes.begin(), Classes.end(),
            [OriginalPtr, Name, IsComplete](Class* klass) {
                return (OriginalPtr && klass->originalPtr == OriginalPtr) ||
                    (klass->name == Name && klass->isIncomplete == !IsComplete); });

        return _class != Classes.end() ? *_class : nullptr;
    }

    auto className = entries[entries.size() - 1];

    std::vector<std::string> namespaces;
    std::copy_n(entries.begin(), entries.size() - 1, std::back_inserter(namespaces));

    auto _namespace = FindNamespace(namespaces);
    if (!_namespace)
        return nullptr;

    return _namespace->FindClass(OriginalPtr, className, IsComplete);
}

Class* DeclarationContext::CreateClass(const std::string& Name, bool IsComplete)
{
    auto _class = new Class();
    _class->name = Name;
    _class->_namespace = this;
    _class->isIncomplete = !IsComplete;

    return _class;
}

Class* DeclarationContext::FindClass(const void* OriginalPtr,
    const std::string& Name, bool IsComplete, bool Create)
{
    auto _class = FindClass(OriginalPtr, Name, IsComplete);

    if (!_class)
    {
        if (Create)
        {
            _class = CreateClass(Name, IsComplete);
            Classes.push_back(_class);
        }
        
        return _class;
    }

    return _class;
}

Enumeration* DeclarationContext::FindEnum(const void* OriginalPtr)
{
    auto foundEnum = std::find_if(Enums.begin(), Enums.end(),
        [&](Enumeration* enumeration) { return enumeration->originalPtr == OriginalPtr; });

    if (foundEnum != Enums.end())
        return *foundEnum;

    return nullptr;
}

Enumeration* DeclarationContext::FindEnum(const std::string& Name, bool Create)
{
    auto entries = split<std::string>(Name, "::");

    if (entries.size() == 1)
    {
        auto foundEnum = std::find_if(Enums.begin(), Enums.end(),
            [&](Enumeration* _enum) { return _enum->name == Name; });

        if (foundEnum != Enums.end())
            return *foundEnum;

        if (!Create)
            return nullptr;

        auto _enum = new Enumeration();
        _enum->name = Name;
        _enum->_namespace = this;
        Enums.push_back(_enum);
        return _enum;
    }

    auto enumName = entries[entries.size() - 1];

    std::vector<std::string> namespaces;
    std::copy_n(entries.begin(), entries.size() - 1, std::back_inserter(namespaces));

    auto _namespace = FindNamespace(namespaces);
    if (!_namespace)
        return nullptr;

    return _namespace->FindEnum(enumName, Create);
}

Enumeration* DeclarationContext::FindEnumWithItem(const std::string& Name)
{
    auto foundEnumIt = std::find_if(Enums.begin(), Enums.end(), 
        [&](Enumeration* _enum) { return _enum->FindItemByName(Name) != nullptr; });
    if (foundEnumIt != Enums.end())
        return *foundEnumIt;
    for (auto it = Namespaces.begin(); it != Namespaces.end(); ++it)
    {
        auto foundEnum = (*it)->FindEnumWithItem(Name);
        if (foundEnum != nullptr)
            return foundEnum;
    }
    for (auto it = Classes.begin(); it != Classes.end(); ++it)
    {
        auto foundEnum = (*it)->FindEnumWithItem(Name);
        if (foundEnum != nullptr)
            return foundEnum;
    }
    return nullptr;
}

Function* DeclarationContext::FindFunction(const std::string& USR)
{
    auto foundFunction = std::find_if(Functions.begin(), Functions.end(),
        [&](Function* func) { return func->USR == USR; });

    if (foundFunction != Functions.end())
        return *foundFunction;

    auto foundTemplate = std::find_if(Templates.begin(), Templates.end(),
        [&](Template* t) { return t->TemplatedDecl && t->TemplatedDecl->USR == USR; });

    if (foundTemplate != Templates.end())
        return static_cast<Function*>((*foundTemplate)->TemplatedDecl);

    return nullptr;
}

TypedefDecl* DeclarationContext::FindTypedef(const std::string& Name, bool Create)
{
    auto foundTypedef = std::find_if(Typedefs.begin(), Typedefs.end(),
            [&](TypedefDecl* tdef) { return tdef->name == Name; });

    if (foundTypedef != Typedefs.end())
        return *foundTypedef;

    if (!Create)
        return nullptr;
     
    auto tdef = new TypedefDecl();
    tdef->name = Name;
    tdef->_namespace = this;

    return tdef;
}

TypeAlias* DeclarationContext::FindTypeAlias(const std::string& Name, bool Create)
{
    auto foundTypeAlias = std::find_if(TypeAliases.begin(), TypeAliases.end(),
        [&](TypeAlias* talias) { return talias->name == Name; });

    if (foundTypeAlias != TypeAliases.end())
        return *foundTypeAlias;

    if (!Create)
        return nullptr;

    auto talias = new TypeAlias();
    talias->name = Name;
    talias->_namespace = this;

    return talias;
}

Variable* DeclarationContext::FindVariable(const std::string& USR)
{
    auto found = std::find_if(Variables.begin(), Variables.end(),
        [&](Variable* var) { return var->USR == USR; });

    if (found != Variables.end())
        return *found;

    return nullptr;
}

Friend* DeclarationContext::FindFriend(const std::string& USR)
{
    auto found = std::find_if(Friends.begin(), Friends.end(),
        [&](Friend* var) { return var->USR == USR; });

    if (found != Friends.end())
        return *found;

    return nullptr;
}

TypedefNameDecl::TypedefNameDecl(DeclarationKind Kind) : Declaration(Kind) {}

TypedefNameDecl::~TypedefNameDecl() {}

TypedefDecl::TypedefDecl() : TypedefNameDecl(DeclarationKind::Typedef) {}

TypedefDecl::~TypedefDecl() {}

TypeAlias::TypeAlias() : TypedefNameDecl(DeclarationKind::TypeAlias), describedAliasTemplate(0) {}

TypeAlias::~TypeAlias() {}

Friend::Friend() : CppSharp::CppParser::AST::Declaration(DeclarationKind::Friend), declaration(0) {}

Friend::~Friend() {}

StatementObsolete::StatementObsolete(const std::string& str, StatementClassObsolete stmtClass, Declaration* decl) : string(str), _class(stmtClass), decl(decl) {}

ExpressionObsolete::ExpressionObsolete(const std::string& str, StatementClassObsolete stmtClass, Declaration* decl)
    : StatementObsolete(str, stmtClass, decl) {}

BinaryOperatorObsolete::BinaryOperatorObsolete(const std::string& str, ExpressionObsolete* lhs, ExpressionObsolete* rhs, const std::string& opcodeStr)
    : ExpressionObsolete(str, StatementClassObsolete::BinaryOperator), LHS(lhs), RHS(rhs), opcodeStr(opcodeStr) {}

BinaryOperatorObsolete::~BinaryOperatorObsolete()
{
    deleteExpression(LHS);
    deleteExpression(RHS);
}


CallExprObsolete::CallExprObsolete(const std::string& str, Declaration* decl)
    : ExpressionObsolete(str, StatementClassObsolete::CallExprClass, decl) {}

CallExprObsolete::~CallExprObsolete()
{
    for (auto& arg : Arguments)
        deleteExpression(arg);
}

DEF_VECTOR(CallExprObsolete, ExpressionObsolete*, Arguments)

CXXConstructExprObsolete::CXXConstructExprObsolete(const std::string& str, Declaration* decl)
    : ExpressionObsolete(str, StatementClassObsolete::CXXConstructExprClass, decl) {}

CXXConstructExprObsolete::~CXXConstructExprObsolete()
{
    for (auto& arg : Arguments)
        deleteExpression(arg);
}

DEF_VECTOR(CXXConstructExprObsolete, ExpressionObsolete*, Arguments)

Parameter::Parameter()
    : Declaration(DeclarationKind::Parameter)
    , isIndirect(false)
    , hasDefaultValue(false)
    , defaultArgument(0)
    , defaultValue(0)
{
}

Parameter::~Parameter()
{
    deleteExpression(defaultArgument);
}

Function::Function() 
    : DeclarationContext(DeclarationKind::Function)
    , isReturnIndirect(false)
    , isConstExpr(false)
    , isVariadic(false)
    , isInline(false)
    , isPure(false)
    , isDeleted(false)
    , isDefaulted(false)
    , friendKind(FriendKind::None)
    , operatorKind(CXXOperatorKind::None)
    , callingConvention(CallingConvention::Default)
    , specializationInfo(0)
    , instantiatedFrom(0)
    , bodyStmt(0)
{
}

Function::~Function() {}
DEF_VECTOR(Function, Parameter*, Parameters)

Method::Method() 
    : Function()
    , isVirtual(false)
    , isStatic(false)
    , isConst(false)
    , isExplicit(false)
    , isDefaultConstructor(false)
    , isCopyConstructor(false)
    , isMoveConstructor(false)
    , refQualifier(RefQualifierKind::None)
{ 
    kind = DeclarationKind::Method; 
}

Method::~Method() {}

DEF_VECTOR(Method, Method*, OverriddenMethods)

// Enumeration

Enumeration::Enumeration() : DeclarationContext(DeclarationKind::Enumeration),
    modifiers((EnumModifiers)0), type(0), builtinType(0) {}

Enumeration::~Enumeration() {}

DEF_VECTOR(Enumeration, Enumeration::Item*, Items)

Enumeration::Item::Item() : Declaration(DeclarationKind::EnumerationItem) {}

Enumeration::Item::Item(const Item& rhs) : Declaration(rhs),
    expression(rhs.expression), value(rhs.value) {}

Enumeration::Item::~Item() {}

Enumeration::Item* Enumeration::FindItemByName(const std::string& Name)
{
    auto foundEnumItem = std::find_if(Items.begin(), Items.end(),
        [&](Item* _item) { return _item->name == Name; });
    if (foundEnumItem != Items.end())
        return *foundEnumItem;
    return nullptr;
}

Variable::Variable() : Declaration(DeclarationKind::Variable),
    isConstExpr(false), initializer(0) {}

Variable::~Variable() {}

BaseClassSpecifier::BaseClassSpecifier() : type(0), offset(0) {}

Field::Field() : Declaration(DeclarationKind::Field), _class(0),
    isBitField(false), bitWidth(0) {}

Field::~Field() {}

AccessSpecifierDecl::AccessSpecifierDecl()
    : Declaration(DeclarationKind::AccessSpecifier) {}

AccessSpecifierDecl::~AccessSpecifierDecl() {}

Class::Class()
    : DeclarationContext(DeclarationKind::Class)
    , isPOD(false)
    , isAbstract(false)
    , isUnion(false)
    , isDynamic(false)
    , isPolymorphic(false)
    , hasNonTrivialDefaultConstructor(false)
    , hasNonTrivialCopyConstructor(false)
    , hasNonTrivialDestructor(false)
    , isExternCContext(false)
    , isInjected(false)
    , layout(0)
{
}

Class::~Class()
{
    if (layout)
        delete layout;
}

DEF_VECTOR(Class, BaseClassSpecifier*, Bases)
DEF_VECTOR(Class, Field*, Fields)
DEF_VECTOR(Class, Method*, Methods)
DEF_VECTOR(Class, AccessSpecifierDecl*, Specifiers)

Template::Template() : Declaration(DeclarationKind::Template),
    TemplatedDecl(0) {}

Template::Template(DeclarationKind kind) : Declaration(kind), TemplatedDecl(0) {}

DEF_VECTOR(Template, Declaration*, Parameters)

TypeAliasTemplate::TypeAliasTemplate() : Template(DeclarationKind::TypeAliasTemplate) {}

TypeAliasTemplate::~TypeAliasTemplate() {}

ClassTemplate::ClassTemplate() : Template(DeclarationKind::ClassTemplate) {}

ClassTemplate::~ClassTemplate() {}

DEF_VECTOR(ClassTemplate, ClassTemplateSpecialization*, Specializations)

ClassTemplateSpecialization::ClassTemplateSpecialization() 
    : Class()
    , templatedDecl(0)
{ 
    kind = DeclarationKind::ClassTemplateSpecialization; 
}

ClassTemplateSpecialization::~ClassTemplateSpecialization() {}

DEF_VECTOR(ClassTemplateSpecialization, TemplateArgument, Arguments)

ClassTemplatePartialSpecialization::ClassTemplatePartialSpecialization()
    : ClassTemplateSpecialization()
{ 
    kind = DeclarationKind::ClassTemplatePartialSpecialization; 
}

ClassTemplatePartialSpecialization::~ClassTemplatePartialSpecialization() {}

FunctionTemplate::FunctionTemplate() : Template(DeclarationKind::FunctionTemplate) {}

FunctionTemplate::~FunctionTemplate() {}

DEF_VECTOR(FunctionTemplate, FunctionTemplateSpecialization*, Specializations)

FunctionTemplateSpecialization* FunctionTemplate::FindSpecialization(const std::string& usr)
{
    auto foundSpec = std::find_if(Specializations.begin(), Specializations.end(),
        [&](FunctionTemplateSpecialization* cts) { return cts->specializedFunction->USR == usr; });

    if (foundSpec != Specializations.end())
        return static_cast<FunctionTemplateSpecialization*>(*foundSpec);

    return nullptr;
}

FunctionTemplateSpecialization::FunctionTemplateSpecialization()
    : _template(0)
    , specializedFunction(0)
{
}

FunctionTemplateSpecialization::~FunctionTemplateSpecialization()
{
}

DEF_VECTOR(FunctionTemplateSpecialization, TemplateArgument, Arguments)

VarTemplate::VarTemplate() : Template(DeclarationKind::VarTemplate) {}

VarTemplate::~VarTemplate() {}

DEF_VECTOR(VarTemplate, VarTemplateSpecialization*, Specializations)

VarTemplateSpecialization* VarTemplate::FindSpecialization(const std::string& usr)
{
    auto foundSpec = std::find_if(Specializations.begin(), Specializations.end(),
        [&](VarTemplateSpecialization* cts) { return cts->USR == usr; });

    if (foundSpec != Specializations.end())
        return static_cast<VarTemplateSpecialization*>(*foundSpec);

    return nullptr;
}

VarTemplatePartialSpecialization* VarTemplate::FindPartialSpecialization(const std::string& usr)
{
    auto foundSpec = FindSpecialization(usr);
    if (foundSpec != nullptr)
        return static_cast<VarTemplatePartialSpecialization*>(foundSpec);
    return nullptr;
}

VarTemplateSpecialization::VarTemplateSpecialization()
    : Variable()
    , templatedDecl(0)
{
    kind = DeclarationKind::VarTemplateSpecialization;
}

VarTemplateSpecialization::~VarTemplateSpecialization() {}

DEF_VECTOR(VarTemplateSpecialization, TemplateArgument, Arguments)

VarTemplatePartialSpecialization::VarTemplatePartialSpecialization()
    : VarTemplateSpecialization()
{
    kind = DeclarationKind::VarTemplatePartialSpecialization;
}

VarTemplatePartialSpecialization::~VarTemplatePartialSpecialization()
{
}

UnresolvedUsingTypename::UnresolvedUsingTypename() : Declaration(DeclarationKind::UnresolvedUsingTypename) {}

UnresolvedUsingTypename::~UnresolvedUsingTypename() {}

Namespace::Namespace() 
    : DeclarationContext(DeclarationKind::Namespace)
    , isInline(false) 
{
}

Namespace::~Namespace() {}

PreprocessedEntity::PreprocessedEntity()
    : macroLocation(AST::MacroLocation::Unknown),
      originalPtr(0), kind(DeclarationKind::PreprocessedEntity) {}

MacroDefinition::MacroDefinition()
    : lineNumberStart(0), lineNumberEnd(0) { kind = DeclarationKind::MacroDefinition; }

MacroDefinition::~MacroDefinition() {}

MacroExpansion::MacroExpansion() : definition(0) { kind = DeclarationKind::MacroExpansion; }

MacroExpansion::~MacroExpansion() {}

TranslationUnit::TranslationUnit() { kind = DeclarationKind::TranslationUnit; }

TranslationUnit::~TranslationUnit() {}
DEF_VECTOR(TranslationUnit, MacroDefinition*, Macros)

NativeLibrary::NativeLibrary()
    : archType(AST::ArchType::UnknownArch) {}

NativeLibrary::~NativeLibrary() {}

// NativeLibrary
DEF_VECTOR_STRING(NativeLibrary, Symbols)
DEF_VECTOR_STRING(NativeLibrary, Dependencies)

// ASTContext
DEF_VECTOR(ASTContext, TranslationUnit*, TranslationUnits)

ClassTemplateSpecialization* ClassTemplate::FindSpecialization(const std::string& usr)
{
    auto foundSpec = std::find_if(Specializations.begin(), Specializations.end(),
        [&](ClassTemplateSpecialization* cts) { return cts->USR == usr; });

    if (foundSpec != Specializations.end())
        return static_cast<ClassTemplateSpecialization*>(*foundSpec);

    return nullptr;
}

ClassTemplatePartialSpecialization* ClassTemplate::FindPartialSpecialization(const std::string& usr)
{
    auto foundSpec = FindSpecialization(usr);
    if (foundSpec != nullptr)
        return static_cast<ClassTemplatePartialSpecialization*>(foundSpec);
    return nullptr;
}

ASTContext::ASTContext() {}

ASTContext::~ASTContext() {}

TranslationUnit* ASTContext::FindOrCreateModule(std::string File)
{
    auto normalizedFile = normalizePath(File);

    auto existingUnit = std::find_if(TranslationUnits.begin(),
        TranslationUnits.end(), [&](TranslationUnit* unit) {
            return unit && unit->fileName == normalizedFile;
    });

    if (existingUnit != TranslationUnits.end())
        return *existingUnit;

    auto unit = new TranslationUnit();
    unit->fileName = normalizedFile;
    TranslationUnits.push_back(unit);

    return unit;
}

// Comments
Comment::Comment(CommentKind kind) : kind(kind) {}

RawComment::RawComment() : fullCommentBlock(0) {}

RawComment::~RawComment()
{
    if (fullCommentBlock)
        delete fullCommentBlock;
}

FullComment::FullComment() : Comment(CommentKind::FullComment) {}

FullComment::~FullComment()
{
    for (auto& block : Blocks)
    {
        // HACK: see https://github.com/mono/CppSharp/issues/599
        switch (block->kind)
        {
        case CommentKind::BlockCommandComment:
            delete static_cast<BlockCommandComment*>(block);
            break;
        case CommentKind::ParamCommandComment:
            delete static_cast<ParamCommandComment*>(block);
            break;
        case CommentKind::TParamCommandComment:
            delete static_cast<TParamCommandComment*>(block);
            break;
        case CommentKind::VerbatimBlockComment:
            delete static_cast<VerbatimBlockComment*>(block);
            break;
        case CommentKind::VerbatimLineComment:
            delete static_cast<VerbatimLineComment*>(block);
            break;
        case CommentKind::ParagraphComment:
            delete static_cast<ParagraphComment*>(block);
            break;
        default:
            delete block;
            break;
        }
    }
}

DEF_VECTOR(FullComment, BlockContentComment*, Blocks)

BlockContentComment::BlockContentComment() : Comment(CommentKind::BlockContentComment) {}

BlockContentComment::BlockContentComment(CommentKind Kind) : Comment(Kind) {}

BlockCommandComment::Argument::Argument() {}

BlockCommandComment::Argument::Argument(const Argument& rhs) : text(rhs.text) {}


BlockCommandComment::BlockCommandComment() : BlockContentComment(CommentKind::BlockCommandComment), commandId(0), paragraphComment(0) {}

BlockCommandComment::BlockCommandComment(CommentKind Kind) : BlockContentComment(Kind), commandId(0), paragraphComment(0) {}

BlockCommandComment::~BlockCommandComment()
{
    delete paragraphComment;
}

DEF_VECTOR(BlockCommandComment, BlockCommandComment::Argument, Arguments)

ParamCommandComment::ParamCommandComment() : BlockCommandComment(CommentKind::ParamCommandComment), direction(PassDirection::In), paramIndex(0) {}

TParamCommandComment::TParamCommandComment() : BlockCommandComment(CommentKind::TParamCommandComment) {}

DEF_VECTOR(TParamCommandComment, unsigned, Position)

VerbatimBlockComment::VerbatimBlockComment() : BlockCommandComment(CommentKind::VerbatimBlockComment) {}

VerbatimBlockComment::~VerbatimBlockComment()
{
    for (auto& line : Lines)
        delete line;
}

DEF_VECTOR(VerbatimBlockComment, VerbatimBlockLineComment*, Lines)

VerbatimLineComment::VerbatimLineComment() : BlockCommandComment(CommentKind::VerbatimLineComment) {}

ParagraphComment::ParagraphComment() : BlockContentComment(CommentKind::ParagraphComment), isWhitespace(false) {}

ParagraphComment::~ParagraphComment()
{
    for (auto& content : Content)
    {
        // HACK: see https://github.com/mono/CppSharp/issues/599
        switch (content->kind)
        {
        case CommentKind::InlineCommandComment:
            delete static_cast<InlineCommandComment*>(content);
            break;
        case CommentKind::HTMLTagComment:
            delete static_cast<HTMLTagComment*>(content);
            break;
        case CommentKind::HTMLStartTagComment:
            delete static_cast<HTMLStartTagComment*>(content);
            break;
        case CommentKind::HTMLEndTagComment:
            delete static_cast<HTMLEndTagComment*>(content);
            break;
        case CommentKind::TextComment:
            delete static_cast<TextComment*>(content);
            break;
        default:
            delete content;
            break;
        }
    }
}

DEF_VECTOR(ParagraphComment, InlineContentComment*, Content)

HTMLTagComment::HTMLTagComment() : InlineContentComment(CommentKind::HTMLTagComment) {}

HTMLTagComment::HTMLTagComment(CommentKind Kind) : InlineContentComment(Kind) {}

HTMLStartTagComment::Attribute::Attribute() {}

HTMLStartTagComment::Attribute::Attribute(const Attribute& rhs) : name(rhs.name), value(rhs.value) {}

HTMLStartTagComment::HTMLStartTagComment() : HTMLTagComment(CommentKind::HTMLStartTagComment) {}

DEF_VECTOR(HTMLStartTagComment, HTMLStartTagComment::Attribute, Attributes)

HTMLEndTagComment::HTMLEndTagComment() : HTMLTagComment(CommentKind::HTMLEndTagComment) {}

InlineContentComment::InlineContentComment() : Comment(CommentKind::InlineContentComment), hasTrailingNewline(false) {}

InlineContentComment::InlineContentComment(CommentKind Kind) : Comment(Kind), hasTrailingNewline(false) {}

TextComment::TextComment() : InlineContentComment(CommentKind::TextComment) {}

InlineCommandComment::Argument::Argument() {}

InlineCommandComment::Argument::Argument(const Argument& rhs) : text(rhs.text) {}

InlineCommandComment::InlineCommandComment()
    : InlineContentComment(CommentKind::InlineCommandComment), commandId(0), commentRenderKind(RenderNormal) {}

DEF_VECTOR(InlineCommandComment, InlineCommandComment::Argument, Arguments)

VerbatimBlockLineComment::VerbatimBlockLineComment() : Comment(CommentKind::VerbatimBlockLineComment) {}

} } }

/************************************************************************
*
* CppSharp
* Licensed under the simplified BSD license. All rights reserved.
*
************************************************************************/

#include "Parser.h"

#include <clang/AST/Comment.h>
#include <clang/AST/ASTContext.h>

using namespace CppSharp::CppParser;

//-----------------------------------//

static RawCommentKind
ConvertRawCommentKind(clang::RawComment::CommentKind Kind)
{
    using clang::RawComment;

    switch (Kind)
    {
    case RawComment::RCK_Invalid: return RawCommentKind::Invalid;
    case RawComment::RCK_OrdinaryBCPL: return RawCommentKind::OrdinaryBCPL;
    case RawComment::RCK_OrdinaryC: return RawCommentKind::OrdinaryC;
    case RawComment::RCK_BCPLSlash: return RawCommentKind::BCPLSlash;
    case RawComment::RCK_BCPLExcl: return RawCommentKind::BCPLExcl;
    case RawComment::RCK_JavaDoc: return RawCommentKind::JavaDoc;
    case RawComment::RCK_Qt: return RawCommentKind::Qt;
    case RawComment::RCK_Merged: return RawCommentKind::Merged;
    }

    llvm_unreachable("Unknown comment kind");
}

RawComment* Parser::WalkRawComment(const clang::RawComment* RC)
{
    using namespace clang;

    auto& SM = c->getSourceManager();
    auto Comment = new RawComment();
    Comment->kind = ConvertRawCommentKind(RC->getKind());
    Comment->text = RC->getRawText(SM).str();
    Comment->briefText = RC->getBriefText(c->getASTContext());

    return Comment;
}

static InlineCommandComment::RenderKind
ConvertRenderKind(clang::comments::InlineCommandComment::RenderKind Kind)
{
    using namespace clang::comments;
    switch (Kind)
    {
    case clang::comments::InlineCommandComment::RenderNormal:
        return CppSharp::CppParser::AST::InlineCommandComment::RenderKind::RenderNormal;
    case clang::comments::InlineCommandComment::RenderBold:
        return CppSharp::CppParser::AST::InlineCommandComment::RenderKind::RenderBold;
    case clang::comments::InlineCommandComment::RenderMonospaced:
        return CppSharp::CppParser::AST::InlineCommandComment::RenderKind::RenderMonospaced;
    case clang::comments::InlineCommandComment::RenderEmphasized:
        return CppSharp::CppParser::AST::InlineCommandComment::RenderKind::RenderEmphasized;
    case clang::comments::InlineCommandComment::RenderAnchor:
        return CppSharp::CppParser::AST::InlineCommandComment::RenderKind::RenderAnchor;
    }
    llvm_unreachable("Unknown render kind");
}

static ParamCommandComment::PassDirection
ConvertParamPassDirection(clang::comments::ParamCommandComment::PassDirection Dir)
{
    using namespace clang::comments;
    switch (Dir)
    {
    case clang::comments::ParamCommandComment::In:
        return CppSharp::CppParser::AST::ParamCommandComment::PassDirection::In;
    case clang::comments::ParamCommandComment::Out:
        return CppSharp::CppParser::AST::ParamCommandComment::PassDirection::Out;
    case clang::comments::ParamCommandComment::InOut:
        return CppSharp::CppParser::AST::ParamCommandComment::PassDirection::InOut;
    }
    llvm_unreachable("Unknown parameter pass direction");
}

static void HandleInlineContent(const clang::comments::InlineContentComment* CK,
    InlineContentComment* IC)
{
    IC->hasTrailingNewline = CK->hasTrailingNewline();
}

static void HandleBlockCommand(const clang::comments::BlockCommandComment* CK,
    BlockCommandComment* BC)
{
    BC->commandId = CK->getCommandID();
    for (unsigned I = 0, E = CK->getNumArgs(); I != E; ++I)
    {
        auto Arg = BlockCommandComment::Argument();
        Arg.text = CK->getArgText(I).str();
        BC->Arguments.push_back(Arg);
    }
}

static Comment* ConvertCommentBlock(clang::comments::Comment* C)
{
    using namespace clang;
    using clang::comments::Comment;

    // This needs to have an underscore else we get an ICE under VS2012.
    CppSharp::CppParser::AST::Comment* _Comment = 0;

    switch (C->getCommentKind())
    {
    case Comment::FullCommentKind:
    {
        auto CK = cast<clang::comments::FullComment>(C);
        auto FC = new FullComment();
        _Comment = FC;
        for (auto I = CK->child_begin(), E = CK->child_end(); I != E; ++I)
        {
            auto Content = ConvertCommentBlock(*I);
            FC->Blocks.push_back(static_cast<BlockContentComment*>(Content));
        }
        break;
    }
    case Comment::BlockCommandCommentKind:
    {
        auto CK = cast<const clang::comments::BlockCommandComment>(C);
        auto BC = new BlockCommandComment();
        _Comment = BC;
        HandleBlockCommand(CK, BC);
        BC->paragraphComment = static_cast<ParagraphComment*>(ConvertCommentBlock(CK->getParagraph()));
        break;
    }
    case Comment::ParamCommandCommentKind:
    {
        auto CK = cast<clang::comments::ParamCommandComment>(C);
        auto PC = new ParamCommandComment();
        _Comment = PC;
        HandleBlockCommand(CK, PC);
        PC->direction = ConvertParamPassDirection(CK->getDirection());
        if (CK->isParamIndexValid() && !CK->isVarArgParam())
            PC->paramIndex = CK->getParamIndex();
        PC->paragraphComment = static_cast<ParagraphComment*>(ConvertCommentBlock(CK->getParagraph()));
        break;
    }
    case Comment::TParamCommandCommentKind:
    {
        auto CK = cast<clang::comments::TParamCommandComment>(C);
        _Comment = new TParamCommandComment();
        auto TC = new TParamCommandComment();
        _Comment = TC;
        HandleBlockCommand(CK, TC);
        if (CK->isPositionValid())
            for (unsigned I = 0, E = CK->getDepth(); I != E; ++I)
                TC->Position.push_back(CK->getIndex(I));
        TC->paragraphComment = static_cast<ParagraphComment*>(ConvertCommentBlock(CK->getParagraph()));
        break;
    }
    case Comment::VerbatimBlockCommentKind:
    {
        auto CK = cast<clang::comments::VerbatimBlockComment>(C);
        auto VB = new VerbatimBlockComment();
        _Comment = VB;
        for (auto I = CK->child_begin(), E = CK->child_end(); I != E; ++I)
        {
            auto Line = ConvertCommentBlock(*I);
            VB->Lines.push_back(static_cast<VerbatimBlockLineComment*>(Line));
        }
        break;
    }
    case Comment::VerbatimLineCommentKind:
    {
        auto CK = cast<clang::comments::VerbatimLineComment>(C);
        auto VL = new VerbatimLineComment();
        _Comment = VL;
        VL->text = CK->getText().str();
        break;
    }
    case Comment::ParagraphCommentKind:
    {
        auto CK = cast<clang::comments::ParagraphComment>(C);
        auto PC = new ParagraphComment();
        _Comment = PC;
        for (auto I = CK->child_begin(), E = CK->child_end(); I != E; ++I)
        {
            auto Content = ConvertCommentBlock(*I);
            PC->Content.push_back(static_cast<InlineContentComment*>(Content));
        }
        PC->isWhitespace = CK->isWhitespace();
        break;
    }
    case Comment::HTMLStartTagCommentKind:
    {
        auto CK = cast<clang::comments::HTMLStartTagComment>(C);
        auto TC = new HTMLStartTagComment();
        _Comment = TC;
        HandleInlineContent(CK, TC);
        TC->tagName = CK->getTagName().str();
        for (unsigned I = 0, E = CK->getNumAttrs(); I != E; ++I)
        {
            auto A = CK->getAttr(I);
            auto Attr = HTMLStartTagComment::Attribute();
            Attr.name = A.Name.str();
            Attr.value = A.Value.str();
            TC->Attributes.push_back(Attr);
        }
        break;
    }
    case Comment::HTMLEndTagCommentKind:
    {
        auto CK = cast<clang::comments::HTMLEndTagComment>(C);
        auto TC = new HTMLEndTagComment();
        _Comment = TC;
        HandleInlineContent(CK, TC);
        TC->tagName = CK->getTagName().str();
        break;
    }
    case Comment::TextCommentKind:
    {
        auto CK = cast<clang::comments::TextComment>(C);
        auto TC = new TextComment();
        _Comment = TC;
        HandleInlineContent(CK, TC);
        TC->text = CK->getText().str();
        break;
    }
    case Comment::InlineCommandCommentKind:
    {
        auto CK = cast<clang::comments::InlineCommandComment>(C);
        auto IC = new InlineCommandComment();
        _Comment = IC;
        HandleInlineContent(CK, IC);
        IC->commandId = CK->getCommandID();
        IC->commentRenderKind = ConvertRenderKind(CK->getRenderKind());
        for (unsigned I = 0, E = CK->getNumArgs(); I != E; ++I)
        {
            auto Arg = InlineCommandComment::Argument();
            Arg.text = CK->getArgText(I).str();
            IC->Arguments.push_back(Arg);
        }
        break;
    }
    case Comment::VerbatimBlockLineCommentKind:
    {
        auto CK = cast<clang::comments::VerbatimBlockLineComment>(C);
        auto VL = new VerbatimBlockLineComment();
        _Comment = VL;
        VL->text = CK->getText().str();
        break;
    }
    case Comment::NoCommentKind: return nullptr;
    default:
        llvm_unreachable("Unknown comment kind");
    }

    assert(_Comment && "Invalid comment instance");
    return _Comment;
}

void Parser::HandleComments(const clang::Decl* D, Declaration* Decl)
{
    using namespace clang;

    const clang::RawComment* RC = 0;
    if (!(RC = c->getASTContext().getRawCommentForAnyRedecl(D)))
        return;

    auto RawComment = WalkRawComment(RC);
    Decl->comment = RawComment;

    if (clang::comments::FullComment* FC = RC->parse(c->getASTContext(), &c->getPreprocessor(), D))
    {
        auto CB = static_cast<FullComment*>(ConvertCommentBlock(FC));
        RawComment->fullCommentBlock = CB;
    }
}


/************************************************************************
*
* CppSharp
* Licensed under the MIT license.
*
************************************************************************/

#include "CppParser.h"
#include "Parser.h"
#include <clang/Basic/Version.inc>

namespace CppSharp {
    namespace CppParser {

        CppParserOptions::CppParserOptions()
            : ASTContext(0)
            , toolSetToUse(0)
            , noStandardIncludes(false)
            , noBuiltinIncludes(false)
            , microsoftMode(false)
            , verbose(false)
            , unityBuild(false)
            , skipPrivateDeclarations(true)
            , skipLayoutInfo(false)
            , skipFunctionBodies(true)
            , clangVersion(CLANG_VERSION_STRING)
        {
        }

        CppParserOptions::~CppParserOptions() {}

        std::string CppParserOptions::getClangVersion() { return clangVersion; }

        DEF_VECTOR_STRING(CppParserOptions, Arguments)
            DEF_VECTOR_STRING(CppParserOptions, SourceFiles)
            DEF_VECTOR_STRING(CppParserOptions, IncludeDirs)
            DEF_VECTOR_STRING(CppParserOptions, SystemIncludeDirs)
            DEF_VECTOR_STRING(CppParserOptions, Defines)
            DEF_VECTOR_STRING(CppParserOptions, Undefines)
            DEF_VECTOR_STRING(CppParserOptions, SupportedStdTypes)

            ParserResult::ParserResult()
            : targetInfo(0)
        {
        }

        ParserResult::ParserResult(const ParserResult& rhs)
            : kind(rhs.kind)
            , Diagnostics(rhs.Diagnostics)
            , Libraries(rhs.Libraries)
            , targetInfo(rhs.targetInfo)
        {}

        ParserResult::~ParserResult()
        {
            for (auto Library : Libraries)
            {
                delete Library;
            }
        }

        DEF_VECTOR(ParserResult, ParserDiagnostic, Diagnostics)
            DEF_VECTOR(ParserResult, NativeLibrary*, Libraries)

            LinkerOptions::LinkerOptions() {}
        LinkerOptions::~LinkerOptions() {}

        DEF_VECTOR_STRING(LinkerOptions, Arguments)
            DEF_VECTOR_STRING(LinkerOptions, LibraryDirs)
            DEF_VECTOR_STRING(LinkerOptions, Libraries)

            ParserDiagnostic::ParserDiagnostic() {}

        ParserDiagnostic::ParserDiagnostic(const ParserDiagnostic& rhs)
            : fileName(rhs.fileName)
            , message(rhs.message)
            , level(rhs.level)
            , lineNumber(rhs.lineNumber)
            , columnNumber(rhs.columnNumber)
        {}

    }
}





// ----------------------------------------------------------------------------
// <auto-generated>
// This is autogenerated code by CppSharp.
// Do not edit this file or all your changes will be lost after re-generation.
// </auto-generated>
// ----------------------------------------------------------------------------


#include "Sources.h"
#include "Expr.h"

namespace CppSharp {
    namespace CppParser {
        namespace AST {

            Expr::Classification::Classification()
            {
            }

            Expr::Expr()
                : Stmt(StmtClass::NoStmt)
                , type(QualifiedType())
                , valueDependent(0)
                , typeDependent(0)
                , instantiationDependent(0)
                , containsUnexpandedParameterPack(0)
                , exprLoc(SourceLocation())
                , isLValue(0)
                , isRValue(0)
                , isXValue(0)
                , isGLValue(0)
                , isOrdinaryOrBitFieldObject(0)
                , sourceBitField(nullptr)
                , referencedDeclOfCallee(nullptr)
                , hasPlaceholderType(0)
            {
            }

            Expr::Expr(StmtClass klass)
                : Stmt(klass)
                , type(QualifiedType())
                , valueDependent(0)
                , typeDependent(0)
                , instantiationDependent(0)
                , containsUnexpandedParameterPack(0)
                , exprLoc(SourceLocation())
                , isLValue(0)
                , isRValue(0)
                , isXValue(0)
                , isGLValue(0)
                , isOrdinaryOrBitFieldObject(0)
                , sourceBitField(nullptr)
                , referencedDeclOfCallee(nullptr)
                , hasPlaceholderType(0)
            {
            }

            FullExpr::FullExpr()
                : Expr(StmtClass::NoStmt)
                , subExpr(nullptr)
            {
            }

            FullExpr::FullExpr(StmtClass klass)
                : Expr(klass)
                , subExpr(nullptr)
            {
            }

            ConstantExpr::ConstantExpr()
                : FullExpr(StmtClass::ConstantExpr)
            {
            }

            OpaqueValueExpr::OpaqueValueExpr()
                : Expr(StmtClass::OpaqueValueExpr)
                , isUnique(0)
                , location(SourceLocation())
                , sourceExpr(nullptr)
            {
            }

            DeclRefExpr::DeclRefExpr()
                : Expr(StmtClass::DeclRefExpr)
                , location(SourceLocation())
                , hadMultipleCandidates(0)
                , hasQualifier(0)
                , foundDecl(nullptr)
                , hasTemplateKWAndArgsInfo(0)
                , templateKeywordLoc(SourceLocation())
                , lAngleLoc(SourceLocation())
                , rAngleLoc(SourceLocation())
                , hasTemplateKeyword(0)
                , hasExplicitTemplateArgs(0)
                , numTemplateArgs(0)
                , refersToEnclosingVariableOrCapture(0)
            {
            }

            IntegerLiteral::IntegerLiteral()
                : Expr(StmtClass::IntegerLiteral)
                , location(SourceLocation())
                , value(0)
            {
            }

            FixedPointLiteral::FixedPointLiteral()
                : Expr(StmtClass::FixedPointLiteral)
                , location(SourceLocation())
                , value(0)
            {
            }

            CharacterLiteral::CharacterLiteral()
                : Expr(StmtClass::CharacterLiteral)
                , location(SourceLocation())
                , kind((CharacterLiteral::CharacterKind::Ascii))
                , value(0)
            {
            }

            FloatingLiteral::FloatingLiteral()
                : Expr(StmtClass::FloatingLiteral)
                , exact(0)
                , location(SourceLocation())
                , valueAsApproximateDouble(0)
            {
            }

            ImaginaryLiteral::ImaginaryLiteral()
                : Expr(StmtClass::ImaginaryLiteral)
                , subExpr(nullptr)
            {
            }

            StringLiteral::StringLiteral()
                : Expr(StmtClass::StringLiteral)
                , byteLength(0)
                , length(0)
                , charByteWidth(0)
                , kind((StringLiteral::StringKind::Ascii))
                , isAscii(0)
                , isWide(0)
                , isUTF8(0)
                , isUTF16(0)
                , isUTF32(0)
                , isPascal(0)
                , containsNonAscii(0)
                , containsNonAsciiOrNull(0)
                , numConcatenated(0)
            {
            }

            PredefinedExpr::PredefinedExpr()
                : Expr(StmtClass::PredefinedExpr)
                , location(SourceLocation())
                , identKind((PredefinedExpr::IdentKind::Func))
            {
            }

            ParenExpr::ParenExpr()
                : Expr(StmtClass::ParenExpr)
                , subExpr(nullptr)
                , lParen(SourceLocation())
                , rParen(SourceLocation())
            {
            }

            UnaryOperator::UnaryOperator()
                : Expr(StmtClass::UnaryOperator)
                , opcode((UnaryOperatorKind::PostInc))
                , subExpr(nullptr)
                , operatorLoc(SourceLocation())
                , canOverflow(0)
                , isPrefix(0)
                , isPostfix(0)
                , isIncrementOp(0)
                , isDecrementOp(0)
                , isIncrementDecrementOp(0)
                , isArithmeticOp(0)
                , isFPContractableWithinStatement(0)
            {
            }

            OffsetOfExpr::OffsetOfExpr()
                : Expr(StmtClass::OffsetOfExpr)
                , operatorLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , numComponents(0)
                , numExpressions(0)
            {
            }

            UnaryExprOrTypeTraitExpr::UnaryExprOrTypeTraitExpr()
                : Expr(StmtClass::UnaryExprOrTypeTraitExpr)
                , kind((UnaryExprOrTypeTrait::SizeOf))
                , operatorLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , isArgumentType(0)
                , argumentType(QualifiedType())
                , argumentExpr(nullptr)
                , typeOfArgument(QualifiedType())
            {
            }

            ArraySubscriptExpr::ArraySubscriptExpr()
                : Expr(StmtClass::ArraySubscriptExpr)
                , lHS(nullptr)
                , rHS(nullptr)
                , rBracketLoc(SourceLocation())
                , base(nullptr)
                , idx(nullptr)
            {
            }

            CallExpr::CallExpr()
                : Expr(StmtClass::CallExpr)
                , callee(nullptr)
                , rParenLoc(SourceLocation())
                , calleeDecl(nullptr)
                , directCallee(nullptr)
                , numArgs(0)
                , numCommas(0)
                , builtinCallee(0)
                , isCallToStdMove(0)
            {
            }

            CallExpr::CallExpr(StmtClass klass)
                : Expr(klass)
                , callee(nullptr)
                , rParenLoc(SourceLocation())
                , calleeDecl(nullptr)
                , directCallee(nullptr)
                , numArgs(0)
                , numCommas(0)
                , builtinCallee(0)
                , isCallToStdMove(0)
            {
            }

            DEF_VECTOR(CallExpr, Expr*, arguments)

                MemberExpr::MemberExpr()
                : Expr(StmtClass::MemberExpr)
                , base(nullptr)
                , arrow(0)
                , memberLoc(SourceLocation())
                , hadMultipleCandidates(0)
                , hasQualifier(0)
                , templateKeywordLoc(SourceLocation())
                , lAngleLoc(SourceLocation())
                , rAngleLoc(SourceLocation())
                , hasTemplateKeyword(0)
                , hasExplicitTemplateArgs(0)
                , numTemplateArgs(0)
                , operatorLoc(SourceLocation())
                , isImplicitAccess(0)
            {
            }

            CompoundLiteralExpr::CompoundLiteralExpr()
                : Expr(StmtClass::CompoundLiteralExpr)
                , initializer(nullptr)
                , fileScope(0)
                , lParenLoc(SourceLocation())
            {
            }

            CastExpr::CastExpr()
                : Expr(StmtClass::NoStmt)
                , castKind((CastKind::Dependent))
                , subExpr(nullptr)
                , castKindName(nullptr)
                , subExprAsWritten(nullptr)
                , conversionFunction(nullptr)
                , path_empty(0)
                , path_size(0)
            {
            }

            CastExpr::CastExpr(StmtClass klass)
                : Expr(klass)
                , castKind((CastKind::Dependent))
                , subExpr(nullptr)
                , castKindName(nullptr)
                , subExprAsWritten(nullptr)
                , conversionFunction(nullptr)
                , path_empty(0)
                , path_size(0)
            {
            }

            ImplicitCastExpr::ImplicitCastExpr()
                : CastExpr(StmtClass::ImplicitCastExpr)
                , isPartOfExplicitCast(0)
            {
            }

            ExplicitCastExpr::ExplicitCastExpr()
                : CastExpr(StmtClass::NoStmt)
                , typeAsWritten(QualifiedType())
            {
            }

            ExplicitCastExpr::ExplicitCastExpr(StmtClass klass)
                : CastExpr(klass)
                , typeAsWritten(QualifiedType())
            {
            }

            CStyleCastExpr::CStyleCastExpr()
                : ExplicitCastExpr(StmtClass::CStyleCastExpr)
                , lParenLoc(SourceLocation())
                , rParenLoc(SourceLocation())
            {
            }

            BinaryOperator::BinaryOperator()
                : Expr(StmtClass::BinaryOperator)
                , operatorLoc(SourceLocation())
                , opcode((BinaryOperatorKind::PtrMemD))
                , lHS(nullptr)
                , rHS(nullptr)
                , isPtrMemOp(0)
                , isMultiplicativeOp(0)
                , isAdditiveOp(0)
                , isShiftOp(0)
                , isBitwiseOp(0)
                , isRelationalOp(0)
                , isEqualityOp(0)
                , isComparisonOp(0)
                , isLogicalOp(0)
                , isAssignmentOp(0)
                , isCompoundAssignmentOp(0)
                , isShiftAssignOp(0)
                , isFPContractableWithinStatement(0)
                , isFEnvAccessOn(0)
            {
            }

            BinaryOperator::BinaryOperator(StmtClass klass)
                : Expr(klass)
                , operatorLoc(SourceLocation())
                , opcode((BinaryOperatorKind::PtrMemD))
                , lHS(nullptr)
                , rHS(nullptr)
                , isPtrMemOp(0)
                , isMultiplicativeOp(0)
                , isAdditiveOp(0)
                , isShiftOp(0)
                , isBitwiseOp(0)
                , isRelationalOp(0)
                , isEqualityOp(0)
                , isComparisonOp(0)
                , isLogicalOp(0)
                , isAssignmentOp(0)
                , isCompoundAssignmentOp(0)
                , isShiftAssignOp(0)
                , isFPContractableWithinStatement(0)
                , isFEnvAccessOn(0)
            {
            }

            CompoundAssignOperator::CompoundAssignOperator()
                : BinaryOperator(StmtClass::CompoundAssignOperator)
                , computationLHSType(QualifiedType())
                , computationResultType(QualifiedType())
            {
            }

            AbstractConditionalOperator::AbstractConditionalOperator()
                : Expr(StmtClass::NoStmt)
                , cond(nullptr)
                , trueExpr(nullptr)
                , falseExpr(nullptr)
                , questionLoc(SourceLocation())
                , colonLoc(SourceLocation())
            {
            }

            AbstractConditionalOperator::AbstractConditionalOperator(StmtClass klass)
                : Expr(klass)
                , cond(nullptr)
                , trueExpr(nullptr)
                , falseExpr(nullptr)
                , questionLoc(SourceLocation())
                , colonLoc(SourceLocation())
            {
            }

            ConditionalOperator::ConditionalOperator()
                : AbstractConditionalOperator(StmtClass::ConditionalOperator)
                , lHS(nullptr)
                , rHS(nullptr)
            {
            }

            BinaryConditionalOperator::BinaryConditionalOperator()
                : AbstractConditionalOperator(StmtClass::BinaryConditionalOperator)
                , common(nullptr)
                , opaqueValue(nullptr)
            {
            }

            AddrLabelExpr::AddrLabelExpr()
                : Expr(StmtClass::AddrLabelExpr)
                , ampAmpLoc(SourceLocation())
                , labelLoc(SourceLocation())
            {
            }

            StmtExpr::StmtExpr()
                : Expr(StmtClass::StmtExpr)
                , subStmt(nullptr)
                , lParenLoc(SourceLocation())
                , rParenLoc(SourceLocation())
            {
            }

            ShuffleVectorExpr::ShuffleVectorExpr()
                : Expr(StmtClass::ShuffleVectorExpr)
                , builtinLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , numSubExprs(0)
            {
            }

            ConvertVectorExpr::ConvertVectorExpr()
                : Expr(StmtClass::ConvertVectorExpr)
                , srcExpr(nullptr)
                , builtinLoc(SourceLocation())
                , rParenLoc(SourceLocation())
            {
            }

            ChooseExpr::ChooseExpr()
                : Expr(StmtClass::ChooseExpr)
                , isConditionTrue(0)
                , cond(nullptr)
                , lHS(nullptr)
                , rHS(nullptr)
                , builtinLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , isConditionDependent(0)
                , chosenSubExpr(nullptr)
            {
            }

            GNUNullExpr::GNUNullExpr()
                : Expr(StmtClass::GNUNullExpr)
                , tokenLocation(SourceLocation())
            {
            }

            VAArgExpr::VAArgExpr()
                : Expr(StmtClass::VAArgExpr)
                , subExpr(nullptr)
                , isMicrosoftABI(0)
                , builtinLoc(SourceLocation())
                , rParenLoc(SourceLocation())
            {
            }

            InitListExpr::InitListExpr()
                : Expr(StmtClass::InitListExpr)
                , arrayFiller(nullptr)
                , lBraceLoc(SourceLocation())
                , rBraceLoc(SourceLocation())
                , syntacticForm(nullptr)
                , numInits(0)
                , hasArrayFiller(0)
                , isExplicit(0)
                , isStringLiteralInit(0)
                , isTransparent(0)
                , isSemanticForm(0)
                , semanticForm(nullptr)
                , isSyntacticForm(0)
            {
            }

            DesignatedInitExpr::Designator::Designator()
            {
            }

            DesignatedInitExpr::FieldDesignator::FieldDesignator()
            {
            }

            DesignatedInitExpr::ArrayOrRangeDesignator::ArrayOrRangeDesignator()
            {
            }

            DesignatedInitExpr::DesignatedInitExpr()
                : Expr(StmtClass::DesignatedInitExpr)
                , equalOrColonLoc(SourceLocation())
                , init(nullptr)
                , size(0)
                , usesGNUSyntax(0)
                , numSubExprs(0)
                , designatorsSourceRange(SourceRange())
            {
            }

            NoInitExpr::NoInitExpr()
                : Expr(StmtClass::NoInitExpr)
            {
            }

            DesignatedInitUpdateExpr::DesignatedInitUpdateExpr()
                : Expr(StmtClass::DesignatedInitUpdateExpr)
                , base(nullptr)
                , updater(nullptr)
            {
            }

            ArrayInitLoopExpr::ArrayInitLoopExpr()
                : Expr(StmtClass::ArrayInitLoopExpr)
                , commonExpr(nullptr)
                , subExpr(nullptr)
            {
            }

            ArrayInitIndexExpr::ArrayInitIndexExpr()
                : Expr(StmtClass::ArrayInitIndexExpr)
            {
            }

            ImplicitValueInitExpr::ImplicitValueInitExpr()
                : Expr(StmtClass::ImplicitValueInitExpr)
            {
            }

            ParenListExpr::ParenListExpr()
                : Expr(StmtClass::ParenListExpr)
                , numExprs(0)
                , lParenLoc(SourceLocation())
                , rParenLoc(SourceLocation())
            {
            }

            GenericSelectionExpr::GenericSelectionExpr()
                : Expr(StmtClass::GenericSelectionExpr)
                , numAssocs(0)
                , genericLoc(SourceLocation())
                , defaultLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , controllingExpr(nullptr)
                , isResultDependent(0)
                , resultIndex(0)
                , resultExpr(nullptr)
            {
            }

            ExtVectorElementExpr::ExtVectorElementExpr()
                : Expr(StmtClass::ExtVectorElementExpr)
                , base(nullptr)
                , accessorLoc(SourceLocation())
                , numElements(0)
                , containsDuplicateElements(0)
                , isArrow(0)
            {
            }

            BlockExpr::BlockExpr()
                : Expr(StmtClass::BlockExpr)
                , caretLocation(SourceLocation())
                , body(nullptr)
            {
            }

            AsTypeExpr::AsTypeExpr()
                : Expr(StmtClass::AsTypeExpr)
                , srcExpr(nullptr)
                , builtinLoc(SourceLocation())
                , rParenLoc(SourceLocation())
            {
            }

            PseudoObjectExpr::PseudoObjectExpr()
                : Expr(StmtClass::PseudoObjectExpr)
                , syntacticForm(nullptr)
                , resultExprIndex(0)
                , resultExpr(nullptr)
                , numSemanticExprs(0)
            {
            }

            AtomicExpr::AtomicExpr()
                : Expr(StmtClass::AtomicExpr)
                , ptr(nullptr)
                , order(nullptr)
                , scope(nullptr)
                , val1(nullptr)
                , orderFail(nullptr)
                , val2(nullptr)
                , weak(nullptr)
                , valueType(QualifiedType())
                , op((AtomicExpr::AtomicOp::C11AtomicInit))
                , numSubExprs(0)
                , isVolatile(0)
                , isCmpXChg(0)
                , isOpenCL(0)
                , builtinLoc(SourceLocation())
                , rParenLoc(SourceLocation())
            {
            }

            TypoExpr::TypoExpr()
                : Expr(StmtClass::TypoExpr)
            {
            }

            CXXOperatorCallExpr::CXXOperatorCallExpr()
                : CallExpr(StmtClass::CXXOperatorCallExpr)
                , _operator((OverloadedOperatorKind::None))
                , isAssignmentOp(0)
                , isInfixBinaryOp(0)
                , operatorLoc(SourceLocation())
            {
            }

            CXXMemberCallExpr::CXXMemberCallExpr()
                : CallExpr(StmtClass::CXXMemberCallExpr)
                , implicitObjectArgument(nullptr)
                , methodDecl(nullptr)
            {
            }

            CUDAKernelCallExpr::CUDAKernelCallExpr()
                : CallExpr(StmtClass::CUDAKernelCallExpr)
                , config(nullptr)
            {
            }

            CXXNamedCastExpr::CXXNamedCastExpr()
                : ExplicitCastExpr(StmtClass::NoStmt)
                , castName(nullptr)
                , operatorLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , angleBrackets(SourceRange())
            {
            }

            CXXNamedCastExpr::CXXNamedCastExpr(StmtClass klass)
                : ExplicitCastExpr(klass)
                , castName(nullptr)
                , operatorLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , angleBrackets(SourceRange())
            {
            }

            CXXStaticCastExpr::CXXStaticCastExpr()
                : CXXNamedCastExpr(StmtClass::CXXStaticCastExpr)
            {
            }

            CXXDynamicCastExpr::CXXDynamicCastExpr()
                : CXXNamedCastExpr(StmtClass::CXXDynamicCastExpr)
                , isAlwaysNull(0)
            {
            }

            CXXReinterpretCastExpr::CXXReinterpretCastExpr()
                : CXXNamedCastExpr(StmtClass::CXXReinterpretCastExpr)
            {
            }

            CXXConstCastExpr::CXXConstCastExpr()
                : CXXNamedCastExpr(StmtClass::CXXConstCastExpr)
            {
            }

            UserDefinedLiteral::UserDefinedLiteral()
                : CallExpr(StmtClass::UserDefinedLiteral)
                , literalOperatorKind((UserDefinedLiteral::LiteralOperatorKind::Raw))
                , cookedLiteral(nullptr)
                , uDSuffixLoc(SourceLocation())
            {
            }

            CXXBoolLiteralExpr::CXXBoolLiteralExpr()
                : Expr(StmtClass::CXXBoolLiteralExpr)
                , value(0)
                , location(SourceLocation())
            {
            }

            CXXNullPtrLiteralExpr::CXXNullPtrLiteralExpr()
                : Expr(StmtClass::CXXNullPtrLiteralExpr)
                , location(SourceLocation())
            {
            }

            CXXStdInitializerListExpr::CXXStdInitializerListExpr()
                : Expr(StmtClass::CXXStdInitializerListExpr)
                , subExpr(nullptr)
            {
            }

            CXXTypeidExpr::CXXTypeidExpr()
                : Expr(StmtClass::CXXTypeidExpr)
                , exprOperand(nullptr)
                , isPotentiallyEvaluated(0)
                , isTypeOperand(0)
            {
            }

            MSPropertyRefExpr::MSPropertyRefExpr()
                : Expr(StmtClass::MSPropertyRefExpr)
                , isImplicitAccess(0)
                , baseExpr(nullptr)
                , isArrow(0)
                , memberLoc(SourceLocation())
            {
            }

            MSPropertySubscriptExpr::MSPropertySubscriptExpr()
                : Expr(StmtClass::MSPropertySubscriptExpr)
                , rBracketLoc(SourceLocation())
                , base(nullptr)
                , idx(nullptr)
            {
            }

            CXXUuidofExpr::CXXUuidofExpr()
                : Expr(StmtClass::CXXUuidofExpr)
                , exprOperand(nullptr)
                , isTypeOperand(0)
            {
            }

            CXXThisExpr::CXXThisExpr()
                : Expr(StmtClass::CXXThisExpr)
                , location(SourceLocation())
                , implicit(0)
            {
            }

            CXXThrowExpr::CXXThrowExpr()
                : Expr(StmtClass::CXXThrowExpr)
                , subExpr(nullptr)
                , throwLoc(SourceLocation())
                , isThrownVariableInScope(0)
            {
            }

            CXXDefaultArgExpr::CXXDefaultArgExpr()
                : Expr(StmtClass::CXXDefaultArgExpr)
                , expr(nullptr)
                , usedLocation(SourceLocation())
            {
            }

            CXXDefaultInitExpr::CXXDefaultInitExpr()
                : Expr(StmtClass::CXXDefaultInitExpr)
                , field(nullptr)
                , expr(nullptr)
            {
            }

            CXXBindTemporaryExpr::CXXBindTemporaryExpr()
                : Expr(StmtClass::CXXBindTemporaryExpr)
                , subExpr(nullptr)
            {
            }

            CXXConstructExpr::CXXConstructExpr()
                : Expr(StmtClass::CXXConstructExpr)
                , location(SourceLocation())
                , elidable(0)
                , hadMultipleCandidates(0)
                , listInitialization(0)
                , stdInitListInitialization(0)
                , requiresZeroInitialization(0)
                , parenOrBraceRange(SourceRange())
                , numArgs(0)
            {
            }

            CXXConstructExpr::CXXConstructExpr(StmtClass klass)
                : Expr(klass)
                , location(SourceLocation())
                , elidable(0)
                , hadMultipleCandidates(0)
                , listInitialization(0)
                , stdInitListInitialization(0)
                , requiresZeroInitialization(0)
                , parenOrBraceRange(SourceRange())
                , numArgs(0)
            {
            }

            DEF_VECTOR(CXXConstructExpr, Expr*, arguments)

                CXXInheritedCtorInitExpr::CXXInheritedCtorInitExpr()
                : Expr(StmtClass::CXXInheritedCtorInitExpr)
                , constructsVBase(0)
                , inheritedFromVBase(0)
                , location(SourceLocation())
            {
            }

            CXXFunctionalCastExpr::CXXFunctionalCastExpr()
                : ExplicitCastExpr(StmtClass::CXXFunctionalCastExpr)
                , lParenLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , isListInitialization(0)
            {
            }

            CXXTemporaryObjectExpr::CXXTemporaryObjectExpr()
                : CXXConstructExpr(StmtClass::CXXTemporaryObjectExpr)
            {
            }

            LambdaExpr::LambdaExpr()
                : Expr(StmtClass::LambdaExpr)
                , captureDefaultLoc(SourceLocation())
                , capture_size(0)
                , introducerRange(SourceRange())
                , callOperator(nullptr)
                , isGenericLambda(0)
                , body(nullptr)
                , isMutable(0)
                , hasExplicitParameters(0)
                , hasExplicitResultType(0)
            {
            }

            DEF_VECTOR(LambdaExpr, Expr*, capture_inits)

                CXXScalarValueInitExpr::CXXScalarValueInitExpr()
                : Expr(StmtClass::CXXScalarValueInitExpr)
                , rParenLoc(SourceLocation())
            {
            }

            CXXNewExpr::CXXNewExpr()
                : Expr(StmtClass::CXXNewExpr)
                , operatorNew(nullptr)
                , operatorDelete(nullptr)
                , allocatedType(QualifiedType())
                , isArray(0)
                , arraySize(nullptr)
                , numPlacementArgs(0)
                , isParenTypeId(0)
                , typeIdParens(SourceRange())
                , isGlobalNew(0)
                , hasInitializer(0)
                , initializationStyle((CXXNewExpr::InitializationStyle::NoInit))
                , initializer(nullptr)
                , constructExpr(nullptr)
                , directInitRange(SourceRange())
            {
            }

            DEF_VECTOR(CXXNewExpr, Expr*, placement_arguments)

                CXXDeleteExpr::CXXDeleteExpr()
                : Expr(StmtClass::CXXDeleteExpr)
                , isGlobalDelete(0)
                , isArrayForm(0)
                , isArrayFormAsWritten(0)
                , operatorDelete(nullptr)
                , argument(nullptr)
                , destroyedType(QualifiedType())
            {
            }

            CXXPseudoDestructorExpr::CXXPseudoDestructorExpr()
                : Expr(StmtClass::CXXPseudoDestructorExpr)
                , base(nullptr)
                , hasQualifier(0)
                , isArrow(0)
                , operatorLoc(SourceLocation())
                , colonColonLoc(SourceLocation())
                , tildeLoc(SourceLocation())
                , destroyedType(QualifiedType())
                , destroyedTypeLoc(SourceLocation())
            {
            }

            TypeTraitExpr::TypeTraitExpr()
                : Expr(StmtClass::TypeTraitExpr)
                , value(0)
                , numArgs(0)
            {
            }

            ArrayTypeTraitExpr::ArrayTypeTraitExpr()
                : Expr(StmtClass::ArrayTypeTraitExpr)
                , queriedType(QualifiedType())
                , value(0)
                , dimensionExpression(nullptr)
            {
            }

            ExpressionTraitExpr::ExpressionTraitExpr()
                : Expr(StmtClass::ExpressionTraitExpr)
                , queriedExpression(nullptr)
                , value(0)
            {
            }

            OverloadExpr::FindResult::FindResult()
            {
            }

            OverloadExpr::OverloadExpr()
                : Expr(StmtClass::NoStmt)
                , numDecls(0)
                , nameLoc(SourceLocation())
                , templateKeywordLoc(SourceLocation())
                , lAngleLoc(SourceLocation())
                , rAngleLoc(SourceLocation())
                , hasTemplateKeyword(0)
                , hasExplicitTemplateArgs(0)
                , numTemplateArgs(0)
            {
            }

            OverloadExpr::OverloadExpr(StmtClass klass)
                : Expr(klass)
                , numDecls(0)
                , nameLoc(SourceLocation())
                , templateKeywordLoc(SourceLocation())
                , lAngleLoc(SourceLocation())
                , rAngleLoc(SourceLocation())
                , hasTemplateKeyword(0)
                , hasExplicitTemplateArgs(0)
                , numTemplateArgs(0)
            {
            }

            UnresolvedLookupExpr::UnresolvedLookupExpr()
                : OverloadExpr(StmtClass::UnresolvedLookupExpr)
                , requiresADL(0)
                , isOverloaded(0)
            {
            }

            DependentScopeDeclRefExpr::DependentScopeDeclRefExpr()
                : Expr(StmtClass::DependentScopeDeclRefExpr)
                , location(SourceLocation())
                , templateKeywordLoc(SourceLocation())
                , lAngleLoc(SourceLocation())
                , rAngleLoc(SourceLocation())
                , hasTemplateKeyword(0)
                , hasExplicitTemplateArgs(0)
                , numTemplateArgs(0)
            {
            }

            ExprWithCleanups::ExprWithCleanups()
                : FullExpr(StmtClass::ExprWithCleanups)
                , numObjects(0)
                , cleanupsHaveSideEffects(0)
            {
            }

            CXXUnresolvedConstructExpr::CXXUnresolvedConstructExpr()
                : Expr(StmtClass::CXXUnresolvedConstructExpr)
                , lParenLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , typeAsWritten(QualifiedType())
                , isListInitialization(0)
                , arg_size(0)
            {
            }

            DEF_VECTOR(CXXUnresolvedConstructExpr, Expr*, arguments)

                CXXDependentScopeMemberExpr::CXXDependentScopeMemberExpr()
                : Expr(StmtClass::CXXDependentScopeMemberExpr)
                , isImplicitAccess(0)
                , base(nullptr)
                , baseType(QualifiedType())
                , isArrow(0)
                , operatorLoc(SourceLocation())
                , firstQualifierFoundInScope(nullptr)
                , memberLoc(SourceLocation())
                , templateKeywordLoc(SourceLocation())
                , lAngleLoc(SourceLocation())
                , rAngleLoc(SourceLocation())
                , hasTemplateKeyword(0)
                , hasExplicitTemplateArgs(0)
                , numTemplateArgs(0)
            {
            }

            UnresolvedMemberExpr::UnresolvedMemberExpr()
                : OverloadExpr(StmtClass::UnresolvedMemberExpr)
                , isImplicitAccess(0)
                , base(nullptr)
                , baseType(QualifiedType())
                , hasUnresolvedUsing(0)
                , isArrow(0)
                , operatorLoc(SourceLocation())
                , memberLoc(SourceLocation())
            {
            }

            CXXNoexceptExpr::CXXNoexceptExpr()
                : Expr(StmtClass::CXXNoexceptExpr)
                , operand(nullptr)
                , value(0)
            {
            }

            PackExpansionExpr::PackExpansionExpr()
                : Expr(StmtClass::PackExpansionExpr)
                , pattern(nullptr)
                , ellipsisLoc(SourceLocation())
            {
            }

            SizeOfPackExpr::SizeOfPackExpr()
                : Expr(StmtClass::SizeOfPackExpr)
                , operatorLoc(SourceLocation())
                , packLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , pack(nullptr)
                , packLength(0)
                , isPartiallySubstituted(0)
            {
            }

            SubstNonTypeTemplateParmExpr::SubstNonTypeTemplateParmExpr()
                : Expr(StmtClass::SubstNonTypeTemplateParmExpr)
                , nameLoc(SourceLocation())
                , replacement(nullptr)
            {
            }

            SubstNonTypeTemplateParmPackExpr::SubstNonTypeTemplateParmPackExpr()
                : Expr(StmtClass::SubstNonTypeTemplateParmPackExpr)
                , parameterPackLocation(SourceLocation())
                , argumentPack(TemplateArgument())
            {
            }

            FunctionParmPackExpr::FunctionParmPackExpr()
                : Expr(StmtClass::FunctionParmPackExpr)
                , parameterPackLocation(SourceLocation())
                , numExpansions(0)
            {
            }

            MaterializeTemporaryExpr::ExtraState::ExtraState()
            {
            }

            MaterializeTemporaryExpr::MaterializeTemporaryExpr()
                : Expr(StmtClass::MaterializeTemporaryExpr)
                , temporary(nullptr)
                , TemporaryExpr(nullptr)
                , manglingNumber(0)
                , isBoundToLvalueReference(0)
            {
            }

            CXXFoldExpr::CXXFoldExpr()
                : Expr(StmtClass::CXXFoldExpr)
                , lHS(nullptr)
                , rHS(nullptr)
                , isRightFold(0)
                , isLeftFold(0)
                , pattern(nullptr)
                , init(nullptr)
                , ellipsisLoc(SourceLocation())
                , _operator((BinaryOperatorKind::PtrMemD))
            {
            }

            CoroutineSuspendExpr::CoroutineSuspendExpr()
                : Expr(StmtClass::NoStmt)
                , keywordLoc(SourceLocation())
                , commonExpr(nullptr)
                , opaqueValue(nullptr)
                , readyExpr(nullptr)
                , suspendExpr(nullptr)
                , resumeExpr(nullptr)
            {
            }

            CoroutineSuspendExpr::CoroutineSuspendExpr(StmtClass klass)
                : Expr(klass)
                , keywordLoc(SourceLocation())
                , commonExpr(nullptr)
                , opaqueValue(nullptr)
                , readyExpr(nullptr)
                , suspendExpr(nullptr)
                , resumeExpr(nullptr)
            {
            }

            CoawaitExpr::CoawaitExpr()
                : CoroutineSuspendExpr(StmtClass::CoawaitExpr)
                , isImplicit(0)
                , operand(nullptr)
            {
            }

            DependentCoawaitExpr::DependentCoawaitExpr()
                : Expr(StmtClass::DependentCoawaitExpr)
                , operand(nullptr)
                , operatorCoawaitLookup(nullptr)
                , keywordLoc(SourceLocation())
            {
            }

            CoyieldExpr::CoyieldExpr()
                : CoroutineSuspendExpr(StmtClass::CoyieldExpr)
                , operand(nullptr)
            {
            }

        }
    }
}









// ----------------------------------------------------------------------------
// <auto-generated>
// This is autogenerated code by CppSharp.
// Do not edit this file or all your changes will be lost after re-generation.
// </auto-generated>
// ----------------------------------------------------------------------------


#include "AST.h"
#include "Parser.h"
#include <clang/AST/Expr.h>
#include <clang/AST/ExprCXX.h>

namespace CppSharp {
    namespace CppParser {

        AST::Expr* Parser::WalkExpression(const clang::Expr* Expr)
        {
            if (Expr == nullptr)
                return nullptr;

            AST::Expr* _Expr = 0;

            switch (Expr->getStmtClass())
            {
            case clang::Stmt::ConstantExprClass:
            {
                auto S = const_cast<clang::ConstantExpr*>(llvm::cast<clang::ConstantExpr>(Expr));
                auto _S = new AST::ConstantExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::OpaqueValueExprClass:
            {
                auto S = const_cast<clang::OpaqueValueExpr*>(llvm::cast<clang::OpaqueValueExpr>(Expr));
                auto _S = new AST::OpaqueValueExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->isUnique = S->isUnique();
                _S->sourceExpr = static_cast<AST::Expr*>(WalkExpression(S->getSourceExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::DeclRefExprClass:
            {
                auto S = const_cast<clang::DeclRefExpr*>(llvm::cast<clang::DeclRefExpr>(Expr));
                auto _S = new AST::DeclRefExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->hadMultipleCandidates = S->hadMultipleCandidates();
                _S->hasQualifier = S->hasQualifier();
                _S->foundDecl = static_cast<AST::Declaration*>(WalkDeclaration(S->getFoundDecl()));
                _S->hasTemplateKWAndArgsInfo = S->hasTemplateKWAndArgsInfo();
                _S->hasTemplateKeyword = S->hasTemplateKeyword();
                _S->hasExplicitTemplateArgs = S->hasExplicitTemplateArgs();
                _S->numTemplateArgs = S->getNumTemplateArgs();
                _S->refersToEnclosingVariableOrCapture = S->refersToEnclosingVariableOrCapture();
                _Expr = _S;
                break;
            }
            case clang::Stmt::IntegerLiteralClass:
            {
                auto S = const_cast<clang::IntegerLiteral*>(llvm::cast<clang::IntegerLiteral>(Expr));
                auto _S = new AST::IntegerLiteral();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->value = S->getValue().getLimitedValue();
                _Expr = _S;
                break;
            }
            case clang::Stmt::FixedPointLiteralClass:
            {
                auto S = const_cast<clang::FixedPointLiteral*>(llvm::cast<clang::FixedPointLiteral>(Expr));
                auto _S = new AST::FixedPointLiteral();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->value = S->getValue().getLimitedValue();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CharacterLiteralClass:
            {
                auto S = const_cast<clang::CharacterLiteral*>(llvm::cast<clang::CharacterLiteral>(Expr));
                auto _S = new AST::CharacterLiteral();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->kind = (CharacterLiteral::CharacterKind)S->getKind();
                _S->value = S->getValue();
                _Expr = _S;
                break;
            }
            case clang::Stmt::FloatingLiteralClass:
            {
                auto S = const_cast<clang::FloatingLiteral*>(llvm::cast<clang::FloatingLiteral>(Expr));
                auto _S = new AST::FloatingLiteral();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->exact = S->isExact();
                _S->valueAsApproximateDouble = S->getValueAsApproximateDouble();
                _Expr = _S;
                break;
            }
            case clang::Stmt::ImaginaryLiteralClass:
            {
                auto S = const_cast<clang::ImaginaryLiteral*>(llvm::cast<clang::ImaginaryLiteral>(Expr));
                auto _S = new AST::ImaginaryLiteral();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::StringLiteralClass:
            {
                auto S = const_cast<clang::StringLiteral*>(llvm::cast<clang::StringLiteral>(Expr));
                auto _S = new AST::StringLiteral();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->string = S->getString().str();
                _S->bytes = S->getBytes().str();
                _S->byteLength = S->getByteLength();
                _S->length = S->getLength();
                _S->charByteWidth = S->getCharByteWidth();
                _S->kind = (StringLiteral::StringKind)S->getKind();
                _S->isAscii = S->isAscii();
                _S->isWide = S->isWide();
                _S->isUTF8 = S->isUTF8();
                _S->isUTF16 = S->isUTF16();
                _S->isUTF32 = S->isUTF32();
                _S->isPascal = S->isPascal();
                _S->containsNonAscii = S->containsNonAscii();
                _S->containsNonAsciiOrNull = S->containsNonAsciiOrNull();
                _S->numConcatenated = S->getNumConcatenated();
                _Expr = _S;
                break;
            }
            case clang::Stmt::PredefinedExprClass:
            {
                auto S = const_cast<clang::PredefinedExpr*>(llvm::cast<clang::PredefinedExpr>(Expr));
                auto _S = new AST::PredefinedExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->identKind = (PredefinedExpr::IdentKind)S->getIdentKind();
                _Expr = _S;
                break;
            }
            case clang::Stmt::ParenExprClass:
            {
                auto S = const_cast<clang::ParenExpr*>(llvm::cast<clang::ParenExpr>(Expr));
                auto _S = new AST::ParenExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::UnaryOperatorClass:
            {
                auto S = const_cast<clang::UnaryOperator*>(llvm::cast<clang::UnaryOperator>(Expr));
                auto _S = new AST::UnaryOperator();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->opcode = (UnaryOperatorKind)S->getOpcode();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->canOverflow = S->canOverflow();
                _S->isPrefix = S->isPrefix();
                _S->isPostfix = S->isPostfix();
                _S->isIncrementOp = S->isIncrementOp();
                _S->isDecrementOp = S->isDecrementOp();
                _S->isIncrementDecrementOp = S->isIncrementDecrementOp();
                _S->isArithmeticOp = S->isArithmeticOp();
                _S->isFPContractableWithinStatement = S->isFPContractableWithinStatement(c->getLangOpts());
                _Expr = _S;
                break;
            }
            case clang::Stmt::OffsetOfExprClass:
            {
                auto S = const_cast<clang::OffsetOfExpr*>(llvm::cast<clang::OffsetOfExpr>(Expr));
                auto _S = new AST::OffsetOfExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->numComponents = S->getNumComponents();
                _S->numExpressions = S->getNumExpressions();
                _Expr = _S;
                break;
            }
            case clang::Stmt::UnaryExprOrTypeTraitExprClass:
            {
                auto S = const_cast<clang::UnaryExprOrTypeTraitExpr*>(llvm::cast<clang::UnaryExprOrTypeTraitExpr>(Expr));
                auto _S = new AST::UnaryExprOrTypeTraitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->kind = (UnaryExprOrTypeTrait)S->getKind();
                _S->isArgumentType = S->isArgumentType();
                if (S->isArgumentType())
                    _S->argumentType = GetQualifiedType(S->getArgumentType());
                _S->argumentExpr = static_cast<AST::Expr*>(WalkExpression(S->getArgumentExpr()));
                _S->typeOfArgument = GetQualifiedType(S->getTypeOfArgument());
                _Expr = _S;
                break;
            }
            case clang::Stmt::ArraySubscriptExprClass:
            {
                auto S = const_cast<clang::ArraySubscriptExpr*>(llvm::cast<clang::ArraySubscriptExpr>(Expr));
                auto _S = new AST::ArraySubscriptExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->lHS = static_cast<AST::Expr*>(WalkExpression(S->getLHS()));
                _S->rHS = static_cast<AST::Expr*>(WalkExpression(S->getRHS()));
                _S->base = static_cast<AST::Expr*>(WalkExpression(S->getBase()));
                _S->idx = static_cast<AST::Expr*>(WalkExpression(S->getIdx()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::CallExprClass:
            {
                auto S = const_cast<clang::CallExpr*>(llvm::cast<clang::CallExpr>(Expr));
                auto _S = new AST::CallExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->callee = static_cast<AST::Expr*>(WalkExpression(S->getCallee()));
                _S->calleeDecl = static_cast<AST::Declaration*>(WalkDeclaration(S->getCalleeDecl()));
                _S->directCallee = static_cast<AST::Function*>(WalkDeclaration(S->getDirectCallee()));
                _S->numArgs = S->getNumArgs();
                _S->numCommas = S->getNumCommas();
                _S->builtinCallee = S->getBuiltinCallee();
                _S->isCallToStdMove = S->isCallToStdMove();
                for (auto _E : S->arguments())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addarguments(_ES);
                }
                _Expr = _S;
                break;
            }
            case clang::Stmt::MemberExprClass:
            {
                auto S = const_cast<clang::MemberExpr*>(llvm::cast<clang::MemberExpr>(Expr));
                auto _S = new AST::MemberExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->base = static_cast<AST::Expr*>(WalkExpression(S->getBase()));
                _S->arrow = S->isArrow();
                _S->hadMultipleCandidates = S->hadMultipleCandidates();
                _S->hasQualifier = S->hasQualifier();
                _S->hasTemplateKeyword = S->hasTemplateKeyword();
                _S->hasExplicitTemplateArgs = S->hasExplicitTemplateArgs();
                _S->numTemplateArgs = S->getNumTemplateArgs();
                _S->isImplicitAccess = S->isImplicitAccess();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CompoundLiteralExprClass:
            {
                auto S = const_cast<clang::CompoundLiteralExpr*>(llvm::cast<clang::CompoundLiteralExpr>(Expr));
                auto _S = new AST::CompoundLiteralExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->initializer = static_cast<AST::Expr*>(WalkExpression(S->getInitializer()));
                _S->fileScope = S->isFileScope();
                _Expr = _S;
                break;
            }
            case clang::Stmt::ImplicitCastExprClass:
            {
                auto S = const_cast<clang::ImplicitCastExpr*>(llvm::cast<clang::ImplicitCastExpr>(Expr));
                auto _S = new AST::ImplicitCastExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->castKind = (CastKind)S->getCastKind();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->castKindName = S->getCastKindName();
                _S->subExprAsWritten = static_cast<AST::Expr*>(WalkExpression(S->getSubExprAsWritten()));
                _S->conversionFunction = static_cast<AST::Declaration*>(WalkDeclaration(S->getConversionFunction()));
                _S->path_empty = S->path_empty();
                _S->path_size = S->path_size();
                _S->isPartOfExplicitCast = S->isPartOfExplicitCast();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CStyleCastExprClass:
            {
                auto S = const_cast<clang::CStyleCastExpr*>(llvm::cast<clang::CStyleCastExpr>(Expr));
                auto _S = new AST::CStyleCastExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->castKind = (CastKind)S->getCastKind();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->castKindName = S->getCastKindName();
                _S->subExprAsWritten = static_cast<AST::Expr*>(WalkExpression(S->getSubExprAsWritten()));
                _S->conversionFunction = static_cast<AST::Declaration*>(WalkDeclaration(S->getConversionFunction()));
                _S->path_empty = S->path_empty();
                _S->path_size = S->path_size();
                _S->typeAsWritten = GetQualifiedType(S->getTypeAsWritten());
                _Expr = _S;
                break;
            }
            case clang::Stmt::BinaryOperatorClass:
            {
                auto S = const_cast<clang::BinaryOperator*>(llvm::cast<clang::BinaryOperator>(Expr));
                auto _S = new AST::BinaryOperator();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->opcode = (BinaryOperatorKind)S->getOpcode();
                _S->lHS = static_cast<AST::Expr*>(WalkExpression(S->getLHS()));
                _S->rHS = static_cast<AST::Expr*>(WalkExpression(S->getRHS()));
                _S->opcodeStr = S->getOpcodeStr().str();
                _S->isPtrMemOp = S->isPtrMemOp();
                _S->isMultiplicativeOp = S->isMultiplicativeOp();
                _S->isAdditiveOp = S->isAdditiveOp();
                _S->isShiftOp = S->isShiftOp();
                _S->isBitwiseOp = S->isBitwiseOp();
                _S->isRelationalOp = S->isRelationalOp();
                _S->isEqualityOp = S->isEqualityOp();
                _S->isComparisonOp = S->isComparisonOp();
                _S->isLogicalOp = S->isLogicalOp();
                _S->isAssignmentOp = S->isAssignmentOp();
                _S->isCompoundAssignmentOp = S->isCompoundAssignmentOp();
                _S->isShiftAssignOp = S->isShiftAssignOp();
                _S->isFPContractableWithinStatement = S->isFPContractableWithinStatement(c->getLangOpts());
                _S->isFEnvAccessOn = S->isFEnvAccessOn(c->getLangOpts());
                _Expr = _S;
                break;
            }
            case clang::Stmt::CompoundAssignOperatorClass:
            {
                auto S = const_cast<clang::CompoundAssignOperator*>(llvm::cast<clang::CompoundAssignOperator>(Expr));
                auto _S = new AST::CompoundAssignOperator();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->opcode = (BinaryOperatorKind)S->getOpcode();
                _S->lHS = static_cast<AST::Expr*>(WalkExpression(S->getLHS()));
                _S->rHS = static_cast<AST::Expr*>(WalkExpression(S->getRHS()));
                _S->opcodeStr = S->getOpcodeStr().str();
                _S->isPtrMemOp = S->isPtrMemOp();
                _S->isMultiplicativeOp = S->isMultiplicativeOp();
                _S->isAdditiveOp = S->isAdditiveOp();
                _S->isShiftOp = S->isShiftOp();
                _S->isBitwiseOp = S->isBitwiseOp();
                _S->isRelationalOp = S->isRelationalOp();
                _S->isEqualityOp = S->isEqualityOp();
                _S->isComparisonOp = S->isComparisonOp();
                _S->isLogicalOp = S->isLogicalOp();
                _S->isAssignmentOp = S->isAssignmentOp();
                _S->isCompoundAssignmentOp = S->isCompoundAssignmentOp();
                _S->isShiftAssignOp = S->isShiftAssignOp();
                _S->isFPContractableWithinStatement = S->isFPContractableWithinStatement(c->getLangOpts());
                _S->isFEnvAccessOn = S->isFEnvAccessOn(c->getLangOpts());
                _S->computationLHSType = GetQualifiedType(S->getComputationLHSType());
                _S->computationResultType = GetQualifiedType(S->getComputationResultType());
                _Expr = _S;
                break;
            }
            case clang::Stmt::ConditionalOperatorClass:
            {
                auto S = const_cast<clang::ConditionalOperator*>(llvm::cast<clang::ConditionalOperator>(Expr));
                auto _S = new AST::ConditionalOperator();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->trueExpr = static_cast<AST::Expr*>(WalkExpression(S->getTrueExpr()));
                _S->falseExpr = static_cast<AST::Expr*>(WalkExpression(S->getFalseExpr()));
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->trueExpr = static_cast<AST::Expr*>(WalkExpression(S->getTrueExpr()));
                _S->falseExpr = static_cast<AST::Expr*>(WalkExpression(S->getFalseExpr()));
                _S->lHS = static_cast<AST::Expr*>(WalkExpression(S->getLHS()));
                _S->rHS = static_cast<AST::Expr*>(WalkExpression(S->getRHS()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::BinaryConditionalOperatorClass:
            {
                auto S = const_cast<clang::BinaryConditionalOperator*>(llvm::cast<clang::BinaryConditionalOperator>(Expr));
                auto _S = new AST::BinaryConditionalOperator();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->trueExpr = static_cast<AST::Expr*>(WalkExpression(S->getTrueExpr()));
                _S->falseExpr = static_cast<AST::Expr*>(WalkExpression(S->getFalseExpr()));
                _S->common = static_cast<AST::Expr*>(WalkExpression(S->getCommon()));
                _S->opaqueValue = static_cast<AST::OpaqueValueExpr*>(WalkExpression(S->getOpaqueValue()));
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->trueExpr = static_cast<AST::Expr*>(WalkExpression(S->getTrueExpr()));
                _S->falseExpr = static_cast<AST::Expr*>(WalkExpression(S->getFalseExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::AddrLabelExprClass:
            {
                auto S = const_cast<clang::AddrLabelExpr*>(llvm::cast<clang::AddrLabelExpr>(Expr));
                auto _S = new AST::AddrLabelExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _Expr = _S;
                break;
            }
            case clang::Stmt::StmtExprClass:
            {
                auto S = const_cast<clang::StmtExpr*>(llvm::cast<clang::StmtExpr>(Expr));
                auto _S = new AST::StmtExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->subStmt = static_cast<AST::CompoundStmt*>(WalkStatement(S->getSubStmt()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::ShuffleVectorExprClass:
            {
                auto S = const_cast<clang::ShuffleVectorExpr*>(llvm::cast<clang::ShuffleVectorExpr>(Expr));
                auto _S = new AST::ShuffleVectorExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->numSubExprs = S->getNumSubExprs();
                _Expr = _S;
                break;
            }
            case clang::Stmt::ConvertVectorExprClass:
            {
                auto S = const_cast<clang::ConvertVectorExpr*>(llvm::cast<clang::ConvertVectorExpr>(Expr));
                auto _S = new AST::ConvertVectorExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->srcExpr = static_cast<AST::Expr*>(WalkExpression(S->getSrcExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::ChooseExprClass:
            {
                auto S = const_cast<clang::ChooseExpr*>(llvm::cast<clang::ChooseExpr>(Expr));
                auto _S = new AST::ChooseExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->isConditionTrue = S->isConditionTrue();
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->lHS = static_cast<AST::Expr*>(WalkExpression(S->getLHS()));
                _S->rHS = static_cast<AST::Expr*>(WalkExpression(S->getRHS()));
                _S->isConditionDependent = S->isConditionDependent();
                _S->chosenSubExpr = static_cast<AST::Expr*>(WalkExpression(S->getChosenSubExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::GNUNullExprClass:
            {
                auto S = const_cast<clang::GNUNullExpr*>(llvm::cast<clang::GNUNullExpr>(Expr));
                auto _S = new AST::GNUNullExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _Expr = _S;
                break;
            }
            case clang::Stmt::VAArgExprClass:
            {
                auto S = const_cast<clang::VAArgExpr*>(llvm::cast<clang::VAArgExpr>(Expr));
                auto _S = new AST::VAArgExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->isMicrosoftABI = S->isMicrosoftABI();
                _Expr = _S;
                break;
            }
            case clang::Stmt::InitListExprClass:
            {
                auto S = const_cast<clang::InitListExpr*>(llvm::cast<clang::InitListExpr>(Expr));
                auto _S = new AST::InitListExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->arrayFiller = static_cast<AST::Expr*>(WalkExpression(S->getArrayFiller()));
                if (S->isSyntacticForm())
                    _S->syntacticForm = static_cast<AST::InitListExpr*>(WalkExpression(S->getSyntacticForm()));
                _S->numInits = S->getNumInits();
                _S->hasArrayFiller = S->hasArrayFiller();
                _S->isExplicit = S->isExplicit();
                _S->isStringLiteralInit = S->isStringLiteralInit();
                _S->isTransparent = S->isTransparent();
                _S->isSemanticForm = S->isSemanticForm();
                if (S->isSemanticForm())
                    _S->semanticForm = static_cast<AST::InitListExpr*>(WalkExpression(S->getSemanticForm()));
                _S->isSyntacticForm = S->isSyntacticForm();
                _Expr = _S;
                break;
            }
            case clang::Stmt::DesignatedInitExprClass:
            {
                auto S = const_cast<clang::DesignatedInitExpr*>(llvm::cast<clang::DesignatedInitExpr>(Expr));
                auto _S = new AST::DesignatedInitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->init = static_cast<AST::Expr*>(WalkExpression(S->getInit()));
                _S->size = S->size();
                _S->usesGNUSyntax = S->usesGNUSyntax();
                _S->numSubExprs = S->getNumSubExprs();
                _Expr = _S;
                break;
            }
            case clang::Stmt::NoInitExprClass:
            {
                auto S = const_cast<clang::NoInitExpr*>(llvm::cast<clang::NoInitExpr>(Expr));
                auto _S = new AST::NoInitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _Expr = _S;
                break;
            }
            case clang::Stmt::DesignatedInitUpdateExprClass:
            {
                auto S = const_cast<clang::DesignatedInitUpdateExpr*>(llvm::cast<clang::DesignatedInitUpdateExpr>(Expr));
                auto _S = new AST::DesignatedInitUpdateExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->base = static_cast<AST::Expr*>(WalkExpression(S->getBase()));
                _S->updater = static_cast<AST::InitListExpr*>(WalkExpression(S->getUpdater()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::ArrayInitLoopExprClass:
            {
                auto S = const_cast<clang::ArrayInitLoopExpr*>(llvm::cast<clang::ArrayInitLoopExpr>(Expr));
                auto _S = new AST::ArrayInitLoopExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->commonExpr = static_cast<AST::OpaqueValueExpr*>(WalkExpression(S->getCommonExpr()));
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::ArrayInitIndexExprClass:
            {
                auto S = const_cast<clang::ArrayInitIndexExpr*>(llvm::cast<clang::ArrayInitIndexExpr>(Expr));
                auto _S = new AST::ArrayInitIndexExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _Expr = _S;
                break;
            }
            case clang::Stmt::ImplicitValueInitExprClass:
            {
                auto S = const_cast<clang::ImplicitValueInitExpr*>(llvm::cast<clang::ImplicitValueInitExpr>(Expr));
                auto _S = new AST::ImplicitValueInitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _Expr = _S;
                break;
            }
            case clang::Stmt::ParenListExprClass:
            {
                auto S = const_cast<clang::ParenListExpr*>(llvm::cast<clang::ParenListExpr>(Expr));
                auto _S = new AST::ParenListExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->numExprs = S->getNumExprs();
                _Expr = _S;
                break;
            }
            case clang::Stmt::GenericSelectionExprClass:
            {
                auto S = const_cast<clang::GenericSelectionExpr*>(llvm::cast<clang::GenericSelectionExpr>(Expr));
                auto _S = new AST::GenericSelectionExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->numAssocs = S->getNumAssocs();
                _S->controllingExpr = static_cast<AST::Expr*>(WalkExpression(S->getControllingExpr()));
                _S->isResultDependent = S->isResultDependent();
                _S->resultIndex = S->getResultIndex();
                _S->resultExpr = static_cast<AST::Expr*>(WalkExpression(S->getResultExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::ExtVectorElementExprClass:
            {
                auto S = const_cast<clang::ExtVectorElementExpr*>(llvm::cast<clang::ExtVectorElementExpr>(Expr));
                auto _S = new AST::ExtVectorElementExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->base = static_cast<AST::Expr*>(WalkExpression(S->getBase()));
                _S->numElements = S->getNumElements();
                _S->containsDuplicateElements = S->containsDuplicateElements();
                _S->isArrow = S->isArrow();
                _Expr = _S;
                break;
            }
            case clang::Stmt::BlockExprClass:
            {
                auto S = const_cast<clang::BlockExpr*>(llvm::cast<clang::BlockExpr>(Expr));
                auto _S = new AST::BlockExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->body = static_cast<AST::Stmt*>(WalkStatement(S->getBody()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::AsTypeExprClass:
            {
                auto S = const_cast<clang::AsTypeExpr*>(llvm::cast<clang::AsTypeExpr>(Expr));
                auto _S = new AST::AsTypeExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->srcExpr = static_cast<AST::Expr*>(WalkExpression(S->getSrcExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::PseudoObjectExprClass:
            {
                auto S = const_cast<clang::PseudoObjectExpr*>(llvm::cast<clang::PseudoObjectExpr>(Expr));
                auto _S = new AST::PseudoObjectExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->syntacticForm = static_cast<AST::Expr*>(WalkExpression(S->getSyntacticForm()));
                _S->resultExprIndex = S->getResultExprIndex();
                _S->resultExpr = static_cast<AST::Expr*>(WalkExpression(S->getResultExpr()));
                _S->numSemanticExprs = S->getNumSemanticExprs();
                _Expr = _S;
                break;
            }
            case clang::Stmt::AtomicExprClass:
            {
                auto S = const_cast<clang::AtomicExpr*>(llvm::cast<clang::AtomicExpr>(Expr));
                auto _S = new AST::AtomicExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->ptr = static_cast<AST::Expr*>(WalkExpression(S->getPtr()));
                _S->order = static_cast<AST::Expr*>(WalkExpression(S->getOrder()));
                _S->scope = static_cast<AST::Expr*>(WalkExpression(S->getScope()));
                _S->val1 = static_cast<AST::Expr*>(WalkExpression(S->getVal1()));
                _S->orderFail = static_cast<AST::Expr*>(WalkExpression(S->getOrderFail()));
                _S->val2 = static_cast<AST::Expr*>(WalkExpression(S->getVal2()));
                _S->weak = static_cast<AST::Expr*>(WalkExpression(S->getWeak()));
                _S->valueType = GetQualifiedType(S->getValueType());
                _S->op = (AtomicExpr::AtomicOp)S->getOp();
                _S->numSubExprs = S->getNumSubExprs();
                _S->isVolatile = S->isVolatile();
                _S->isCmpXChg = S->isCmpXChg();
                _S->isOpenCL = S->isOpenCL();
                _Expr = _S;
                break;
            }
            case clang::Stmt::TypoExprClass:
            {
                auto S = const_cast<clang::TypoExpr*>(llvm::cast<clang::TypoExpr>(Expr));
                auto _S = new AST::TypoExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXOperatorCallExprClass:
            {
                auto S = const_cast<clang::CXXOperatorCallExpr*>(llvm::cast<clang::CXXOperatorCallExpr>(Expr));
                auto _S = new AST::CXXOperatorCallExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->callee = static_cast<AST::Expr*>(WalkExpression(S->getCallee()));
                _S->calleeDecl = static_cast<AST::Declaration*>(WalkDeclaration(S->getCalleeDecl()));
                _S->directCallee = static_cast<AST::Function*>(WalkDeclaration(S->getDirectCallee()));
                _S->numArgs = S->getNumArgs();
                _S->numCommas = S->getNumCommas();
                _S->builtinCallee = S->getBuiltinCallee();
                _S->isCallToStdMove = S->isCallToStdMove();
                for (auto _E : S->arguments())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addarguments(_ES);
                }
                _S->_operator = (OverloadedOperatorKind)S->getOperator();
                _S->isAssignmentOp = S->isAssignmentOp();
                _S->isInfixBinaryOp = S->isInfixBinaryOp();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXMemberCallExprClass:
            {
                auto S = const_cast<clang::CXXMemberCallExpr*>(llvm::cast<clang::CXXMemberCallExpr>(Expr));
                auto _S = new AST::CXXMemberCallExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->callee = static_cast<AST::Expr*>(WalkExpression(S->getCallee()));
                _S->calleeDecl = static_cast<AST::Declaration*>(WalkDeclaration(S->getCalleeDecl()));
                _S->directCallee = static_cast<AST::Function*>(WalkDeclaration(S->getDirectCallee()));
                _S->numArgs = S->getNumArgs();
                _S->numCommas = S->getNumCommas();
                _S->builtinCallee = S->getBuiltinCallee();
                _S->isCallToStdMove = S->isCallToStdMove();
                for (auto _E : S->arguments())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addarguments(_ES);
                }
                _S->implicitObjectArgument = static_cast<AST::Expr*>(WalkExpression(S->getImplicitObjectArgument()));
                _S->methodDecl = static_cast<AST::Method*>(WalkDeclaration(S->getMethodDecl()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::CUDAKernelCallExprClass:
            {
                auto S = const_cast<clang::CUDAKernelCallExpr*>(llvm::cast<clang::CUDAKernelCallExpr>(Expr));
                auto _S = new AST::CUDAKernelCallExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->callee = static_cast<AST::Expr*>(WalkExpression(S->getCallee()));
                _S->calleeDecl = static_cast<AST::Declaration*>(WalkDeclaration(S->getCalleeDecl()));
                _S->directCallee = static_cast<AST::Function*>(WalkDeclaration(S->getDirectCallee()));
                _S->numArgs = S->getNumArgs();
                _S->numCommas = S->getNumCommas();
                _S->builtinCallee = S->getBuiltinCallee();
                _S->isCallToStdMove = S->isCallToStdMove();
                for (auto _E : S->arguments())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addarguments(_ES);
                }
                _S->config = static_cast<AST::CallExpr*>(WalkExpression(S->getConfig()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXStaticCastExprClass:
            {
                auto S = const_cast<clang::CXXStaticCastExpr*>(llvm::cast<clang::CXXStaticCastExpr>(Expr));
                auto _S = new AST::CXXStaticCastExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->castKind = (CastKind)S->getCastKind();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->castKindName = S->getCastKindName();
                _S->subExprAsWritten = static_cast<AST::Expr*>(WalkExpression(S->getSubExprAsWritten()));
                _S->conversionFunction = static_cast<AST::Declaration*>(WalkDeclaration(S->getConversionFunction()));
                _S->path_empty = S->path_empty();
                _S->path_size = S->path_size();
                _S->typeAsWritten = GetQualifiedType(S->getTypeAsWritten());
                _S->castName = S->getCastName();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXDynamicCastExprClass:
            {
                auto S = const_cast<clang::CXXDynamicCastExpr*>(llvm::cast<clang::CXXDynamicCastExpr>(Expr));
                auto _S = new AST::CXXDynamicCastExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->castKind = (CastKind)S->getCastKind();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->castKindName = S->getCastKindName();
                _S->subExprAsWritten = static_cast<AST::Expr*>(WalkExpression(S->getSubExprAsWritten()));
                _S->conversionFunction = static_cast<AST::Declaration*>(WalkDeclaration(S->getConversionFunction()));
                _S->path_empty = S->path_empty();
                _S->path_size = S->path_size();
                _S->typeAsWritten = GetQualifiedType(S->getTypeAsWritten());
                _S->castName = S->getCastName();
                _S->isAlwaysNull = S->isAlwaysNull();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXReinterpretCastExprClass:
            {
                auto S = const_cast<clang::CXXReinterpretCastExpr*>(llvm::cast<clang::CXXReinterpretCastExpr>(Expr));
                auto _S = new AST::CXXReinterpretCastExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->castKind = (CastKind)S->getCastKind();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->castKindName = S->getCastKindName();
                _S->subExprAsWritten = static_cast<AST::Expr*>(WalkExpression(S->getSubExprAsWritten()));
                _S->conversionFunction = static_cast<AST::Declaration*>(WalkDeclaration(S->getConversionFunction()));
                _S->path_empty = S->path_empty();
                _S->path_size = S->path_size();
                _S->typeAsWritten = GetQualifiedType(S->getTypeAsWritten());
                _S->castName = S->getCastName();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXConstCastExprClass:
            {
                auto S = const_cast<clang::CXXConstCastExpr*>(llvm::cast<clang::CXXConstCastExpr>(Expr));
                auto _S = new AST::CXXConstCastExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->castKind = (CastKind)S->getCastKind();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->castKindName = S->getCastKindName();
                _S->subExprAsWritten = static_cast<AST::Expr*>(WalkExpression(S->getSubExprAsWritten()));
                _S->conversionFunction = static_cast<AST::Declaration*>(WalkDeclaration(S->getConversionFunction()));
                _S->path_empty = S->path_empty();
                _S->path_size = S->path_size();
                _S->typeAsWritten = GetQualifiedType(S->getTypeAsWritten());
                _S->castName = S->getCastName();
                _Expr = _S;
                break;
            }
            case clang::Stmt::UserDefinedLiteralClass:
            {
                auto S = const_cast<clang::UserDefinedLiteral*>(llvm::cast<clang::UserDefinedLiteral>(Expr));
                auto _S = new AST::UserDefinedLiteral();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->callee = static_cast<AST::Expr*>(WalkExpression(S->getCallee()));
                _S->calleeDecl = static_cast<AST::Declaration*>(WalkDeclaration(S->getCalleeDecl()));
                _S->directCallee = static_cast<AST::Function*>(WalkDeclaration(S->getDirectCallee()));
                _S->numArgs = S->getNumArgs();
                _S->numCommas = S->getNumCommas();
                _S->builtinCallee = S->getBuiltinCallee();
                _S->isCallToStdMove = S->isCallToStdMove();
                for (auto _E : S->arguments())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addarguments(_ES);
                }
                _S->literalOperatorKind = (UserDefinedLiteral::LiteralOperatorKind)S->getLiteralOperatorKind();
                _S->cookedLiteral = static_cast<AST::Expr*>(WalkExpression(S->getCookedLiteral()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXBoolLiteralExprClass:
            {
                auto S = const_cast<clang::CXXBoolLiteralExpr*>(llvm::cast<clang::CXXBoolLiteralExpr>(Expr));
                auto _S = new AST::CXXBoolLiteralExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->value = S->getValue();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXNullPtrLiteralExprClass:
            {
                auto S = const_cast<clang::CXXNullPtrLiteralExpr*>(llvm::cast<clang::CXXNullPtrLiteralExpr>(Expr));
                auto _S = new AST::CXXNullPtrLiteralExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXStdInitializerListExprClass:
            {
                auto S = const_cast<clang::CXXStdInitializerListExpr*>(llvm::cast<clang::CXXStdInitializerListExpr>(Expr));
                auto _S = new AST::CXXStdInitializerListExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXTypeidExprClass:
            {
                auto S = const_cast<clang::CXXTypeidExpr*>(llvm::cast<clang::CXXTypeidExpr>(Expr));
                auto _S = new AST::CXXTypeidExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->exprOperand = static_cast<AST::Expr*>(WalkExpression(S->getExprOperand()));
                _S->isPotentiallyEvaluated = S->isPotentiallyEvaluated();
                _S->isTypeOperand = S->isTypeOperand();
                _Expr = _S;
                break;
            }
            case clang::Stmt::MSPropertyRefExprClass:
            {
                auto S = const_cast<clang::MSPropertyRefExpr*>(llvm::cast<clang::MSPropertyRefExpr>(Expr));
                auto _S = new AST::MSPropertyRefExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->isImplicitAccess = S->isImplicitAccess();
                _S->baseExpr = static_cast<AST::Expr*>(WalkExpression(S->getBaseExpr()));
                _S->isArrow = S->isArrow();
                _Expr = _S;
                break;
            }
            case clang::Stmt::MSPropertySubscriptExprClass:
            {
                auto S = const_cast<clang::MSPropertySubscriptExpr*>(llvm::cast<clang::MSPropertySubscriptExpr>(Expr));
                auto _S = new AST::MSPropertySubscriptExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->base = static_cast<AST::Expr*>(WalkExpression(S->getBase()));
                _S->idx = static_cast<AST::Expr*>(WalkExpression(S->getIdx()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXUuidofExprClass:
            {
                auto S = const_cast<clang::CXXUuidofExpr*>(llvm::cast<clang::CXXUuidofExpr>(Expr));
                auto _S = new AST::CXXUuidofExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->exprOperand = static_cast<AST::Expr*>(WalkExpression(S->getExprOperand()));
                _S->uuidStr = S->getGuidDecl()->getNameAsString();
                _S->isTypeOperand = S->isTypeOperand();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXThisExprClass:
            {
                auto S = const_cast<clang::CXXThisExpr*>(llvm::cast<clang::CXXThisExpr>(Expr));
                auto _S = new AST::CXXThisExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->implicit = S->isImplicit();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXThrowExprClass:
            {
                auto S = const_cast<clang::CXXThrowExpr*>(llvm::cast<clang::CXXThrowExpr>(Expr));
                auto _S = new AST::CXXThrowExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->isThrownVariableInScope = S->isThrownVariableInScope();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXDefaultArgExprClass:
            {
                auto S = const_cast<clang::CXXDefaultArgExpr*>(llvm::cast<clang::CXXDefaultArgExpr>(Expr));
                auto _S = new AST::CXXDefaultArgExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->expr = static_cast<AST::Expr*>(WalkExpression(S->getExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXDefaultInitExprClass:
            {
                auto S = const_cast<clang::CXXDefaultInitExpr*>(llvm::cast<clang::CXXDefaultInitExpr>(Expr));
                auto _S = new AST::CXXDefaultInitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->field = static_cast<AST::Field*>(WalkDeclaration(S->getField()));
                _S->expr = static_cast<AST::Expr*>(WalkExpression(S->getExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXBindTemporaryExprClass:
            {
                auto S = const_cast<clang::CXXBindTemporaryExpr*>(llvm::cast<clang::CXXBindTemporaryExpr>(Expr));
                auto _S = new AST::CXXBindTemporaryExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXConstructExprClass:
            {
                auto S = const_cast<clang::CXXConstructExpr*>(llvm::cast<clang::CXXConstructExpr>(Expr));
                auto _S = new AST::CXXConstructExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->elidable = S->isElidable();
                _S->hadMultipleCandidates = S->hadMultipleCandidates();
                _S->listInitialization = S->isListInitialization();
                _S->stdInitListInitialization = S->isStdInitListInitialization();
                _S->requiresZeroInitialization = S->requiresZeroInitialization();
                _S->numArgs = S->getNumArgs();
                for (auto _E : S->arguments())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addarguments(_ES);
                }
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXInheritedCtorInitExprClass:
            {
                auto S = const_cast<clang::CXXInheritedCtorInitExpr*>(llvm::cast<clang::CXXInheritedCtorInitExpr>(Expr));
                auto _S = new AST::CXXInheritedCtorInitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->constructsVBase = S->constructsVBase();
                _S->inheritedFromVBase = S->inheritedFromVBase();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXFunctionalCastExprClass:
            {
                auto S = const_cast<clang::CXXFunctionalCastExpr*>(llvm::cast<clang::CXXFunctionalCastExpr>(Expr));
                auto _S = new AST::CXXFunctionalCastExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->castKind = (CastKind)S->getCastKind();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->castKindName = S->getCastKindName();
                _S->subExprAsWritten = static_cast<AST::Expr*>(WalkExpression(S->getSubExprAsWritten()));
                _S->conversionFunction = static_cast<AST::Declaration*>(WalkDeclaration(S->getConversionFunction()));
                _S->path_empty = S->path_empty();
                _S->path_size = S->path_size();
                _S->typeAsWritten = GetQualifiedType(S->getTypeAsWritten());
                _S->isListInitialization = S->isListInitialization();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXTemporaryObjectExprClass:
            {
                auto S = const_cast<clang::CXXTemporaryObjectExpr*>(llvm::cast<clang::CXXTemporaryObjectExpr>(Expr));
                auto _S = new AST::CXXTemporaryObjectExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->elidable = S->isElidable();
                _S->hadMultipleCandidates = S->hadMultipleCandidates();
                _S->listInitialization = S->isListInitialization();
                _S->stdInitListInitialization = S->isStdInitListInitialization();
                _S->requiresZeroInitialization = S->requiresZeroInitialization();
                _S->numArgs = S->getNumArgs();
                for (auto _E : S->arguments())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addarguments(_ES);
                }
                _Expr = _S;
                break;
            }
            case clang::Stmt::LambdaExprClass:
            {
                auto S = const_cast<clang::LambdaExpr*>(llvm::cast<clang::LambdaExpr>(Expr));
                auto _S = new AST::LambdaExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->capture_size = S->capture_size();
                _S->callOperator = static_cast<AST::Method*>(WalkDeclaration(S->getCallOperator()));
                _S->isGenericLambda = S->isGenericLambda();
                _S->body = static_cast<AST::CompoundStmt*>(WalkStatement(S->getBody()));
                _S->isMutable = S->isMutable();
                _S->hasExplicitParameters = S->hasExplicitParameters();
                _S->hasExplicitResultType = S->hasExplicitResultType();
                for (auto _E : S->capture_inits())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addcapture_inits(_ES);
                }
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXScalarValueInitExprClass:
            {
                auto S = const_cast<clang::CXXScalarValueInitExpr*>(llvm::cast<clang::CXXScalarValueInitExpr>(Expr));
                auto _S = new AST::CXXScalarValueInitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXNewExprClass:
            {
                auto S = const_cast<clang::CXXNewExpr*>(llvm::cast<clang::CXXNewExpr>(Expr));
                auto _S = new AST::CXXNewExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->operatorNew = static_cast<AST::Function*>(WalkDeclaration(S->getOperatorNew()));
                _S->operatorDelete = static_cast<AST::Function*>(WalkDeclaration(S->getOperatorDelete()));
                _S->allocatedType = GetQualifiedType(S->getAllocatedType());
                _S->isArray = S->isArray();
                _S->arraySize = static_cast<AST::Expr*>(WalkExpression(S->getArraySize().getValue()));
                _S->numPlacementArgs = S->getNumPlacementArgs();
                _S->isParenTypeId = S->isParenTypeId();
                _S->isGlobalNew = S->isGlobalNew();
                _S->hasInitializer = S->hasInitializer();
                _S->initializationStyle = (CXXNewExpr::InitializationStyle)S->getInitializationStyle();
                _S->initializer = static_cast<AST::Expr*>(WalkExpression(S->getInitializer()));
                _S->constructExpr = static_cast<AST::CXXConstructExpr*>(WalkExpression(S->getConstructExpr()));
                for (auto _E : S->placement_arguments())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addplacement_arguments(_ES);
                }
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXDeleteExprClass:
            {
                auto S = const_cast<clang::CXXDeleteExpr*>(llvm::cast<clang::CXXDeleteExpr>(Expr));
                auto _S = new AST::CXXDeleteExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->isGlobalDelete = S->isGlobalDelete();
                _S->isArrayForm = S->isArrayForm();
                _S->isArrayFormAsWritten = S->isArrayFormAsWritten();
                _S->operatorDelete = static_cast<AST::Function*>(WalkDeclaration(S->getOperatorDelete()));
                _S->argument = static_cast<AST::Expr*>(WalkExpression(S->getArgument()));
                _S->destroyedType = GetQualifiedType(S->getDestroyedType());
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXPseudoDestructorExprClass:
            {
                auto S = const_cast<clang::CXXPseudoDestructorExpr*>(llvm::cast<clang::CXXPseudoDestructorExpr>(Expr));
                auto _S = new AST::CXXPseudoDestructorExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->base = static_cast<AST::Expr*>(WalkExpression(S->getBase()));
                _S->hasQualifier = S->hasQualifier();
                _S->isArrow = S->isArrow();
                _S->destroyedType = GetQualifiedType(S->getDestroyedType());
                _Expr = _S;
                break;
            }
            case clang::Stmt::TypeTraitExprClass:
            {
                auto S = const_cast<clang::TypeTraitExpr*>(llvm::cast<clang::TypeTraitExpr>(Expr));
                auto _S = new AST::TypeTraitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->value = S->getValue();
                _S->numArgs = S->getNumArgs();
                _Expr = _S;
                break;
            }
            case clang::Stmt::ArrayTypeTraitExprClass:
            {
                auto S = const_cast<clang::ArrayTypeTraitExpr*>(llvm::cast<clang::ArrayTypeTraitExpr>(Expr));
                auto _S = new AST::ArrayTypeTraitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->queriedType = GetQualifiedType(S->getQueriedType());
                _S->value = S->getValue();
                _S->dimensionExpression = static_cast<AST::Expr*>(WalkExpression(S->getDimensionExpression()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::ExpressionTraitExprClass:
            {
                auto S = const_cast<clang::ExpressionTraitExpr*>(llvm::cast<clang::ExpressionTraitExpr>(Expr));
                auto _S = new AST::ExpressionTraitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->queriedExpression = static_cast<AST::Expr*>(WalkExpression(S->getQueriedExpression()));
                _S->value = S->getValue();
                _Expr = _S;
                break;
            }
            case clang::Stmt::UnresolvedLookupExprClass:
            {
                auto S = const_cast<clang::UnresolvedLookupExpr*>(llvm::cast<clang::UnresolvedLookupExpr>(Expr));
                auto _S = new AST::UnresolvedLookupExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->numDecls = S->getNumDecls();
                _S->hasTemplateKeyword = S->hasTemplateKeyword();
                _S->hasExplicitTemplateArgs = S->hasExplicitTemplateArgs();
                _S->numTemplateArgs = S->getNumTemplateArgs();
                _S->requiresADL = S->requiresADL();
                _S->isOverloaded = S->isOverloaded();
                _Expr = _S;
                break;
            }
            case clang::Stmt::DependentScopeDeclRefExprClass:
            {
                auto S = const_cast<clang::DependentScopeDeclRefExpr*>(llvm::cast<clang::DependentScopeDeclRefExpr>(Expr));
                auto _S = new AST::DependentScopeDeclRefExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->hasTemplateKeyword = S->hasTemplateKeyword();
                _S->hasExplicitTemplateArgs = S->hasExplicitTemplateArgs();
                _S->numTemplateArgs = S->getNumTemplateArgs();
                _Expr = _S;
                break;
            }
            case clang::Stmt::ExprWithCleanupsClass:
            {
                auto S = const_cast<clang::ExprWithCleanups*>(llvm::cast<clang::ExprWithCleanups>(Expr));
                auto _S = new AST::ExprWithCleanups();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->subExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->numObjects = S->getNumObjects();
                _S->cleanupsHaveSideEffects = S->cleanupsHaveSideEffects();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXUnresolvedConstructExprClass:
            {
                auto S = const_cast<clang::CXXUnresolvedConstructExpr*>(llvm::cast<clang::CXXUnresolvedConstructExpr>(Expr));
                auto _S = new AST::CXXUnresolvedConstructExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->typeAsWritten = GetQualifiedType(S->getTypeAsWritten());
                _S->isListInitialization = S->isListInitialization();
                _S->arg_size = S->arg_size();
                for (auto _E : S->arguments())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addarguments(_ES);
                }
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXDependentScopeMemberExprClass:
            {
                auto S = const_cast<clang::CXXDependentScopeMemberExpr*>(llvm::cast<clang::CXXDependentScopeMemberExpr>(Expr));
                auto _S = new AST::CXXDependentScopeMemberExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->isImplicitAccess = S->isImplicitAccess();
                _S->base = static_cast<AST::Expr*>(WalkExpression(S->getBase()));
                _S->baseType = GetQualifiedType(S->getBaseType());
                _S->isArrow = S->isArrow();
                _S->firstQualifierFoundInScope = static_cast<AST::Declaration*>(WalkDeclaration(S->getFirstQualifierFoundInScope()));
                _S->hasTemplateKeyword = S->hasTemplateKeyword();
                _S->hasExplicitTemplateArgs = S->hasExplicitTemplateArgs();
                _S->numTemplateArgs = S->getNumTemplateArgs();
                _Expr = _S;
                break;
            }
            case clang::Stmt::UnresolvedMemberExprClass:
            {
                auto S = const_cast<clang::UnresolvedMemberExpr*>(llvm::cast<clang::UnresolvedMemberExpr>(Expr));
                auto _S = new AST::UnresolvedMemberExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->numDecls = S->getNumDecls();
                _S->hasTemplateKeyword = S->hasTemplateKeyword();
                _S->hasExplicitTemplateArgs = S->hasExplicitTemplateArgs();
                _S->numTemplateArgs = S->getNumTemplateArgs();
                _S->isImplicitAccess = S->isImplicitAccess();
                _S->base = static_cast<AST::Expr*>(WalkExpression(S->getBase()));
                _S->baseType = GetQualifiedType(S->getBaseType());
                _S->hasUnresolvedUsing = S->hasUnresolvedUsing();
                _S->isArrow = S->isArrow();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXNoexceptExprClass:
            {
                auto S = const_cast<clang::CXXNoexceptExpr*>(llvm::cast<clang::CXXNoexceptExpr>(Expr));
                auto _S = new AST::CXXNoexceptExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->operand = static_cast<AST::Expr*>(WalkExpression(S->getOperand()));
                _S->value = S->getValue();
                _Expr = _S;
                break;
            }
            case clang::Stmt::PackExpansionExprClass:
            {
                auto S = const_cast<clang::PackExpansionExpr*>(llvm::cast<clang::PackExpansionExpr>(Expr));
                auto _S = new AST::PackExpansionExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->pattern = static_cast<AST::Expr*>(WalkExpression(S->getPattern()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::SizeOfPackExprClass:
            {
                auto S = const_cast<clang::SizeOfPackExpr*>(llvm::cast<clang::SizeOfPackExpr>(Expr));
                auto _S = new AST::SizeOfPackExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->pack = static_cast<AST::Declaration*>(WalkDeclaration(S->getPack()));
                _S->packLength = S->getPackLength();
                _S->isPartiallySubstituted = S->isPartiallySubstituted();
                _Expr = _S;
                break;
            }
            case clang::Stmt::SubstNonTypeTemplateParmExprClass:
            {
                auto S = const_cast<clang::SubstNonTypeTemplateParmExpr*>(llvm::cast<clang::SubstNonTypeTemplateParmExpr>(Expr));
                auto _S = new AST::SubstNonTypeTemplateParmExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->replacement = static_cast<AST::Expr*>(WalkExpression(S->getReplacement()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::SubstNonTypeTemplateParmPackExprClass:
            {
                auto S = const_cast<clang::SubstNonTypeTemplateParmPackExpr*>(llvm::cast<clang::SubstNonTypeTemplateParmPackExpr>(Expr));
                auto _S = new AST::SubstNonTypeTemplateParmPackExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->argumentPack = WalkTemplateArgument(S->getArgumentPack());
                _Expr = _S;
                break;
            }
            case clang::Stmt::FunctionParmPackExprClass:
            {
                auto S = const_cast<clang::FunctionParmPackExpr*>(llvm::cast<clang::FunctionParmPackExpr>(Expr));
                auto _S = new AST::FunctionParmPackExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->numExpansions = S->getNumExpansions();
                _Expr = _S;
                break;
            }
            case clang::Stmt::MaterializeTemporaryExprClass:
            {
                auto S = const_cast<clang::MaterializeTemporaryExpr*>(llvm::cast<clang::MaterializeTemporaryExpr>(Expr));
                auto _S = new AST::MaterializeTemporaryExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->temporary = static_cast<AST::Stmt*>(WalkStatement(S->getSubExpr()));
                _S->TemporaryExpr = static_cast<AST::Expr*>(WalkExpression(S->getSubExpr()));
                _S->manglingNumber = S->getManglingNumber();
                _S->isBoundToLvalueReference = S->isBoundToLvalueReference();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CXXFoldExprClass:
            {
                auto S = const_cast<clang::CXXFoldExpr*>(llvm::cast<clang::CXXFoldExpr>(Expr));
                auto _S = new AST::CXXFoldExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->lHS = static_cast<AST::Expr*>(WalkExpression(S->getLHS()));
                _S->rHS = static_cast<AST::Expr*>(WalkExpression(S->getRHS()));
                _S->isRightFold = S->isRightFold();
                _S->isLeftFold = S->isLeftFold();
                _S->pattern = static_cast<AST::Expr*>(WalkExpression(S->getPattern()));
                _S->init = static_cast<AST::Expr*>(WalkExpression(S->getInit()));
                _S->_operator = (BinaryOperatorKind)S->getOperator();
                _Expr = _S;
                break;
            }
            case clang::Stmt::CoawaitExprClass:
            {
                auto S = const_cast<clang::CoawaitExpr*>(llvm::cast<clang::CoawaitExpr>(Expr));
                auto _S = new AST::CoawaitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->commonExpr = static_cast<AST::Expr*>(WalkExpression(S->getCommonExpr()));
                _S->opaqueValue = static_cast<AST::OpaqueValueExpr*>(WalkExpression(S->getOpaqueValue()));
                _S->readyExpr = static_cast<AST::Expr*>(WalkExpression(S->getReadyExpr()));
                _S->suspendExpr = static_cast<AST::Expr*>(WalkExpression(S->getSuspendExpr()));
                _S->resumeExpr = static_cast<AST::Expr*>(WalkExpression(S->getResumeExpr()));
                _S->isImplicit = S->isImplicit();
                _S->operand = static_cast<AST::Expr*>(WalkExpression(S->getOperand()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::DependentCoawaitExprClass:
            {
                auto S = const_cast<clang::DependentCoawaitExpr*>(llvm::cast<clang::DependentCoawaitExpr>(Expr));
                auto _S = new AST::DependentCoawaitExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->operand = static_cast<AST::Expr*>(WalkExpression(S->getOperand()));
                _S->operatorCoawaitLookup = static_cast<AST::UnresolvedLookupExpr*>(WalkExpression(S->getOperatorCoawaitLookup()));
                _Expr = _S;
                break;
            }
            case clang::Stmt::CoyieldExprClass:
            {
                auto S = const_cast<clang::CoyieldExpr*>(llvm::cast<clang::CoyieldExpr>(Expr));
                auto _S = new AST::CoyieldExpr();
                _S->type = GetQualifiedType(S->getType());
                _S->valueDependent = S->isValueDependent();
                _S->typeDependent = S->isTypeDependent();
                _S->instantiationDependent = S->isInstantiationDependent();
                _S->containsUnexpandedParameterPack = S->containsUnexpandedParameterPack();
                _S->isLValue = S->isLValue();
                _S->isRValue = S->isRValue();
                _S->isXValue = S->isXValue();
                _S->isGLValue = S->isGLValue();
                _S->isOrdinaryOrBitFieldObject = S->isOrdinaryOrBitFieldObject();
                _S->sourceBitField = static_cast<AST::Field*>(WalkDeclaration(S->getSourceBitField()));
                _S->referencedDeclOfCallee = static_cast<AST::Declaration*>(WalkDeclaration(S->getReferencedDeclOfCallee()));
                _S->hasPlaceholderType = S->hasPlaceholderType();
                _S->commonExpr = static_cast<AST::Expr*>(WalkExpression(S->getCommonExpr()));
                _S->opaqueValue = static_cast<AST::OpaqueValueExpr*>(WalkExpression(S->getOpaqueValue()));
                _S->readyExpr = static_cast<AST::Expr*>(WalkExpression(S->getReadyExpr()));
                _S->suspendExpr = static_cast<AST::Expr*>(WalkExpression(S->getSuspendExpr()));
                _S->resumeExpr = static_cast<AST::Expr*>(WalkExpression(S->getResumeExpr()));
                _S->operand = static_cast<AST::Expr*>(WalkExpression(S->getOperand()));
                _Expr = _S;
                break;
            }
            default:
                printf("Unhandled statement kind: %s\n", Expr->getStmtClassName());
            }

            return _Expr;
        }

    }
}









/************************************************************************
*
* CppSharp
* Licensed under the simplified BSD license. All rights reserved.
*
************************************************************************/

#ifdef DEBUG
#undef DEBUG // workaround DEBUG define messing with LLVM COFF headers
#endif

#include "Parser.h"
#include "ELFDumper.h"
#include "APValuePrinter.h"

#include <llvm/Support/Host.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Object/Archive.h>
#include <llvm/Object/COFF.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/MachO.h>
#include <llvm/Option/ArgList.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DataLayout.h>
#include <clang/Basic/Builtins.h>
#include <clang/Basic/Version.h>
#include <clang/Config/config.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/Comment.h>
#include <clang/AST/DeclFriend.h>
#include <clang/AST/ExprCXX.h>
#include <clang/Lex/DirectoryLookup.h>
#include <clang/Lex/HeaderSearch.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/Lex/PreprocessorOptions.h>
#include <clang/Lex/PreprocessingRecord.h>
#include <clang/Parse/ParseAST.h>
#include <clang/Sema/Sema.h>
#include <clang/Sema/SemaConsumer.h>
#include <clang/Frontend/Utils.h>
#include <clang/Driver/Driver.h>
#include <clang/Driver/ToolChain.h>
#include <clang/Driver/Util.h>
#include <clang/Index/USRGeneration.h>

#include <CodeGen/TargetInfo.h>
#include <CodeGen/CGCall.h>
#include <CodeGen/CGCXXABI.h>
#include <Driver/ToolChains/Linux.h>
#include <Driver/ToolChains/MSVC.h>

#if defined(__APPLE__) || defined(__linux__)
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <dlfcn.h>

#define HAVE_DLFCN
#endif

using namespace CppSharp::CppParser;

// We use this as a placeholder for pointer values that should be ignored.
void* IgnorePtr = reinterpret_cast<void*>(0x1);

//-----------------------------------//

Parser::Parser(CppParserOptions* Opts) : opts(Opts), index(0)
{
    for (const auto& SupportedStdType : Opts->SupportedStdTypes)
        supportedStdTypes.insert(SupportedStdType);
    supportedStdTypes.insert("allocator");
    supportedStdTypes.insert("basic_string");
}

LayoutField Parser::WalkVTablePointer(Class* Class,
    const clang::CharUnits& Offset, const std::string& prefix)
{
    LayoutField LayoutField;
    LayoutField.offset = Offset.getQuantity();
    LayoutField.name = prefix + "_" + Class->name;
    LayoutField.qualifiedType = GetQualifiedType(c->getASTContext().VoidPtrTy);
    return LayoutField;
}

static CppAbi GetClassLayoutAbi(clang::TargetCXXABI::Kind abi)
{
    switch (abi)
    {
    case clang::TargetCXXABI::Microsoft:
        return CppAbi::Microsoft;
    case clang::TargetCXXABI::GenericItanium:
        return CppAbi::Itanium;
    case clang::TargetCXXABI::GenericARM:
        return CppAbi::ARM;
    case clang::TargetCXXABI::iOS:
        return CppAbi::iOS;
    case clang::TargetCXXABI::iOS64:
        return CppAbi::iOS64;
    default:
        llvm_unreachable("Unsupported C++ ABI kind");
    }
}

void Parser::ReadClassLayout(Class* Class, const clang::RecordDecl* RD,
    clang::CharUnits Offset, bool IncludeVirtualBases)
{
    using namespace clang;

    const auto& Layout = c->getASTContext().getASTRecordLayout(RD);
    auto CXXRD = dyn_cast<CXXRecordDecl>(RD);

    auto Parent = static_cast<AST::Class*>(
        WalkDeclaration(RD));

    if (Class != Parent)
    {
        LayoutBase LayoutBase;
        LayoutBase.offset = Offset.getQuantity();
        LayoutBase._class = Parent;
        Class->layout->Bases.push_back(LayoutBase);
    }

    // Dump bases.
    if (CXXRD) {
        const CXXRecordDecl* PrimaryBase = Layout.getPrimaryBase();
        bool HasOwnVFPtr = Layout.hasOwnVFPtr();
        bool HasOwnVBPtr = Layout.hasOwnVBPtr();

        // Vtable pointer.
        if (CXXRD->isDynamicClass() && !PrimaryBase &&
            !c->getTarget().getCXXABI().isMicrosoft()) {
            auto VPtr = WalkVTablePointer(Parent, Offset, "vptr");
            Class->layout->Fields.push_back(VPtr);
        }
        else if (HasOwnVFPtr) {
            auto VTPtr = WalkVTablePointer(Parent, Offset, "vfptr");
            Class->layout->Fields.push_back(VTPtr);
        }

        // Collect nvbases.
        SmallVector<const CXXRecordDecl*, 4> Bases;
        for (const CXXBaseSpecifier& Base : CXXRD->bases()) {
            assert(!Base.getType()->isDependentType() &&
                "Cannot layout class with dependent bases.");
            if (!Base.isVirtual())
                Bases.push_back(Base.getType()->getAsCXXRecordDecl());
        }

        // Sort nvbases by offset.
        std::stable_sort(Bases.begin(), Bases.end(),
            [&](const CXXRecordDecl* L, const CXXRecordDecl* R) {
            return Layout.getBaseClassOffset(L) < Layout.getBaseClassOffset(R);
        });

        // Dump (non-virtual) bases
        for (const CXXRecordDecl* Base : Bases) {
            CharUnits BaseOffset = Offset + Layout.getBaseClassOffset(Base);
            ReadClassLayout(Class, Base, BaseOffset,
                /*IncludeVirtualBases=*/false);
        }

        // vbptr (for Microsoft C++ ABI)
        if (HasOwnVBPtr) {
            auto VBPtr = WalkVTablePointer(Parent,
                Offset + Layout.getVBPtrOffset(), "vbptr");
            Class->layout->Fields.push_back(VBPtr);
        }
    }

    // Dump fields.
    uint64_t FieldNo = 0;
    for (const clang::FieldDecl* Field : RD->fields()) {
        uint64_t LocalFieldOffsetInBits = Layout.getFieldOffset(FieldNo++);
        CharUnits FieldOffset =
            Offset + c->getASTContext().toCharUnitsFromBits(LocalFieldOffsetInBits);

        auto F = WalkFieldCXX(Field, Parent);
        LayoutField LayoutField;
        LayoutField.offset = FieldOffset.getQuantity();
        LayoutField.name = F->name;
        LayoutField.qualifiedType = GetQualifiedType(Field->getType());
        LayoutField.fieldPtr = (void*)Field;
        Class->layout->Fields.push_back(LayoutField);
    }

    // Dump virtual bases.
    if (CXXRD && IncludeVirtualBases) {
        const ASTRecordLayout::VBaseOffsetsMapTy& VtorDisps =
            Layout.getVBaseOffsetsMap();

        for (const CXXBaseSpecifier& Base : CXXRD->vbases()) {
            assert(Base.isVirtual() && "Found non-virtual class!");
            const CXXRecordDecl* VBase = Base.getType()->getAsCXXRecordDecl();

            CharUnits VBaseOffset = Offset + Layout.getVBaseClassOffset(VBase);

            if (VtorDisps.find(VBase)->second.hasVtorDisp()) {
                auto VtorDisp = WalkVTablePointer(Parent,
                    VBaseOffset - CharUnits::fromQuantity(4), "vtordisp");
                Class->layout->Fields.push_back(VtorDisp);
            }

            ReadClassLayout(Class, VBase, VBaseOffset,
                /*IncludeVirtualBases=*/false);
        }
    }
}

//-----------------------------------//

static clang::TargetCXXABI::Kind
ConvertToClangTargetCXXABI(CppSharp::CppParser::AST::CppAbi abi)
{
    using namespace clang;

    switch (abi)
    {
    case CppSharp::CppParser::AST::CppAbi::Itanium:
        return TargetCXXABI::GenericItanium;
    case CppSharp::CppParser::AST::CppAbi::Microsoft:
        return TargetCXXABI::Microsoft;
    case CppSharp::CppParser::AST::CppAbi::ARM:
        return TargetCXXABI::GenericARM;
    case CppSharp::CppParser::AST::CppAbi::iOS:
        return TargetCXXABI::iOS;
    case CppSharp::CppParser::AST::CppAbi::iOS64:
        return TargetCXXABI::iOS64;
    }

    llvm_unreachable("Unsupported C++ ABI.");
}

void Parser::Setup()
{
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();

    using namespace clang;

    std::vector<const char*> args;
    args.push_back("-cc1");

    for (unsigned I = 0, E = opts->Arguments.size(); I != E; ++I)
    {
        const auto& Arg = opts->Arguments[I];
        args.push_back(Arg.c_str());

        if (opts->verbose)
            printf("Compiler argument: %s\n", Arg.c_str());
    }

    c.reset(new CompilerInstance());
    c->createDiagnostics();

    CompilerInvocation* Inv = new CompilerInvocation();
    llvm::ArrayRef<const char*> arguments(args.data(), args.data() + args.size());
    CompilerInvocation::CreateFromArgs(*Inv, arguments, c->getDiagnostics());
    c->setInvocation(std::shared_ptr<CompilerInvocation>(Inv));
    c->getLangOpts() = *Inv->LangOpts;

    auto& TO = Inv->TargetOpts;

    if (opts->targetTriple.empty())
        opts->targetTriple = llvm::sys::getDefaultTargetTriple();
    TO->Triple = llvm::Triple::normalize(opts->targetTriple);

    if (opts->verbose)
        printf("Target triple: %s\n", TO->Triple.c_str());

    TargetInfo* TI = TargetInfo::CreateTargetInfo(c->getDiagnostics(), TO);
    if (!TI)
    {
        // We might have no target info due to an invalid user-provided triple.
        // Try again with the default triple.
        opts->targetTriple = llvm::sys::getDefaultTargetTriple();
        TO->Triple = llvm::Triple::normalize(opts->targetTriple);
        TI = TargetInfo::CreateTargetInfo(c->getDiagnostics(), TO);
    }

    assert(TI && "Expected valid target info");

    c->setTarget(TI);

    c->createFileManager();
    c->createSourceManager(c->getFileManager());

    auto& HSOpts = c->getHeaderSearchOpts();
    auto& PPOpts = c->getPreprocessorOpts();
    auto& LangOpts = c->getLangOpts();

    if (opts->noStandardIncludes)
    {
        HSOpts.UseStandardSystemIncludes = false;
        HSOpts.UseStandardCXXIncludes = false;
    }

    if (opts->noBuiltinIncludes)
        HSOpts.UseBuiltinIncludes = false;

    if (opts->verbose)
        HSOpts.Verbose = true;

    for (unsigned I = 0, E = opts->IncludeDirs.size(); I != E; ++I)
    {
        const auto& s = opts->IncludeDirs[I];
        HSOpts.AddPath(s, frontend::Angled, false, false);
    }

    for (unsigned I = 0, E = opts->SystemIncludeDirs.size(); I != E; ++I)
    {
        const auto& s = opts->SystemIncludeDirs[I];
        HSOpts.AddPath(s, frontend::System, false, false);
    }

    for (unsigned I = 0, E = opts->Defines.size(); I != E; ++I)
    {
        const auto& define = opts->Defines[I];
        PPOpts.addMacroDef(define);
    }

    for (unsigned I = 0, E = opts->Undefines.size(); I != E; ++I)
    {
        const auto& undefine = opts->Undefines[I];
        PPOpts.addMacroUndef(undefine);
    }

#ifdef _MSC_VER
    if (opts->microsoftMode)
    {
        LangOpts.MSCompatibilityVersion = opts->toolSetToUse;
        if (!LangOpts.MSCompatibilityVersion) LangOpts.MSCompatibilityVersion = 1700;
    }
#endif

    llvm::opt::InputArgList Args(0, 0);
    clang::driver::Driver D("", TO->Triple, c->getDiagnostics());
    clang::driver::ToolChain* TC = nullptr;
    llvm::Triple Target(TO->Triple);

    if (Target.getOS() == llvm::Triple::Linux)
        TC = new clang::driver::toolchains::Linux(D, Target, Args);
    else if (Target.getEnvironment() == llvm::Triple::EnvironmentType::MSVC)
        TC = new clang::driver::toolchains::MSVCToolChain(D, Target, Args);

    if (TC && !opts->noStandardIncludes) {
        llvm::opt::ArgStringList Includes;
        TC->AddClangSystemIncludeArgs(Args, Includes);
        TC->AddClangCXXStdlibIncludeArgs(Args, Includes);
        for (auto& Arg : Includes) {
            if (strlen(Arg) > 0 && Arg[0] != '-')
                HSOpts.AddPath(Arg, frontend::System, /*IsFramework=*/false,
                    /*IgnoreSysRoot=*/false);
        }
    }

    if (TC)
        delete TC;

    // Enable preprocessing record.
    PPOpts.DetailedRecord = true;

    c->createPreprocessor(TU_Complete);

    Preprocessor& PP = c->getPreprocessor();
    PP.getBuiltinInfo().initializeBuiltins(PP.getIdentifierTable(),
        PP.getLangOpts());

    c->createASTContext();
}

//-----------------------------------//

std::string Parser::GetDeclMangledName(const clang::Decl* D)
{
    using namespace clang;

    if (!D || !isa<NamedDecl>(D))
        return "";

    bool CanMangle = isa<FunctionDecl>(D) || isa<VarDecl>(D)
        || isa<CXXConstructorDecl>(D) || isa<CXXDestructorDecl>(D);

    if (!CanMangle) return "";

    auto ND = cast<NamedDecl>(D);
    std::unique_ptr<MangleContext> MC;

    auto& AST = c->getASTContext();
    auto targetABI = c->getTarget().getCXXABI().getKind();
    switch (targetABI)
    {
    default:
        MC.reset(ItaniumMangleContext::create(AST, AST.getDiagnostics()));
        break;
    case TargetCXXABI::Microsoft:
        MC.reset(MicrosoftMangleContext::create(AST, AST.getDiagnostics()));
        break;
    }

    if (!MC)
        llvm_unreachable("Unknown mangling ABI");

    std::string Mangled;
    llvm::raw_string_ostream Out(Mangled);

    bool IsDependent = false;
    if (const ValueDecl* VD = dyn_cast<ValueDecl>(ND))
        IsDependent |= VD->getType()->isDependentType();

    if (!IsDependent)
        IsDependent |= ND->getDeclContext()->isDependentContext();

    if (!MC->shouldMangleDeclName(ND) || IsDependent)
        return ND->getDeclName().getAsString();

    GlobalDecl GD;
    if (const CXXConstructorDecl* CD = dyn_cast<CXXConstructorDecl>(ND))
        GD = GlobalDecl(CD, Ctor_Base);
    else if (const CXXDestructorDecl* DD = dyn_cast<CXXDestructorDecl>(ND))
        GD = GlobalDecl(DD, Dtor_Base);
    else
        GD = GlobalDecl(ND);
    MC->mangleName(GD, Out);

    Out.flush();

    // Strip away LLVM name marker.
    if (!Mangled.empty() && Mangled[0] == '\01')
        Mangled = Mangled.substr(1);

    return Mangled;
}

//-----------------------------------//

static std::string GetDeclName(const clang::NamedDecl* D)
{
    if (const clang::IdentifierInfo* II = D->getIdentifier())
        return II->getName().str();
    return D->getNameAsString();
}

static std::string GetTagDeclName(const clang::TagDecl* D)
{
    using namespace clang;

    if (auto Typedef = D->getTypedefNameForAnonDecl())
    {
        assert(Typedef->getIdentifier() && "Typedef without identifier?");
        return GetDeclName(Typedef);
    }

    return GetDeclName(D);
}

static std::string GetDeclUSR(const clang::Decl* D)
{
    using namespace clang;
    SmallString<128> usr;
    if (!index::generateUSRForDecl(D, usr))
        return usr.str().str();
    return "<invalid>";
}

static clang::Decl* GetPreviousDeclInContext(const clang::Decl* D)
{
    assert(!D->getLexicalDeclContext()->decls_empty());

    clang::Decl* prevDecl = nullptr;
    for (auto it = D->getDeclContext()->decls_begin();
        it != D->getDeclContext()->decls_end(); it++)
    {
        if ((*it) == D)
            return prevDecl;
        prevDecl = (*it);
    }

    return nullptr;
}

bool IsExplicit(const clang::Decl* D)
{
    using namespace clang;

    auto CTS = llvm::dyn_cast<ClassTemplateSpecializationDecl>(D);
    return !CTS ||
        CTS->getSpecializationKind() == TSK_ExplicitSpecialization ||
        CTS->getSpecializationKind() == TSK_ExplicitInstantiationDeclaration ||
        CTS->getSpecializationKind() == TSK_ExplicitInstantiationDefinition;
}

static clang::SourceLocation GetDeclStartLocation(clang::CompilerInstance* C,
    const clang::Decl* D)
{
    auto& SM = C->getSourceManager();
    auto startLoc = SM.getExpansionLoc(D->getBeginLoc());
    auto startOffset = SM.getFileOffset(startLoc);

    if (clang::dyn_cast_or_null<clang::TranslationUnitDecl>(D) || !startLoc.isValid())
        return startLoc;

    auto lineNo = SM.getExpansionLineNumber(startLoc);
    auto lineBeginLoc = SM.translateLineCol(SM.getFileID(startLoc), lineNo, 1);
    auto lineBeginOffset = SM.getFileOffset(lineBeginLoc);
    assert(lineBeginOffset <= startOffset);

    if (D->getLexicalDeclContext()->decls_empty())
        return lineBeginLoc;

    auto prevDecl = GetPreviousDeclInContext(D);
    if (!prevDecl || !IsExplicit(prevDecl))
        return lineBeginLoc;

    auto prevDeclEndLoc = SM.getExpansionLoc(prevDecl->getEndLoc());
    auto prevDeclEndOffset = SM.getFileOffset(prevDeclEndLoc);

    if (SM.getFileID(prevDeclEndLoc) != SM.getFileID(startLoc))
        return lineBeginLoc;

    // TODO: Figure out why this asserts
    //assert(prevDeclEndOffset <= startOffset);

    if (prevDeclEndOffset < lineBeginOffset)
        return lineBeginLoc;

    // Declarations don't share same macro expansion
    if (SM.getExpansionLoc(prevDecl->getBeginLoc()) != startLoc)
        return prevDeclEndLoc;

    return GetDeclStartLocation(C, prevDecl);
}

std::string Parser::GetTypeName(const clang::Type* Type)
{
    using namespace clang;

    if (Type->isAnyPointerType() || Type->isReferenceType())
        Type = Type->getPointeeType().getTypePtr();

    if (Type->isEnumeralType() || Type->isRecordType())
    {
        const clang::TagType* Tag = Type->getAs<clang::TagType>();
        return GetTagDeclName(Tag->getDecl());
    }

    PrintingPolicy pp(c->getLangOpts());
    pp.SuppressTagKeyword = true;

    std::string TypeName;
    QualType::getAsStringInternal(Type, Qualifiers(), TypeName, pp);

    return TypeName;
}

static TypeQualifiers GetTypeQualifiers(const clang::QualType& Type)
{
    TypeQualifiers quals;
    quals.isConst = Type.isLocalConstQualified();
    quals.isRestrict = Type.isLocalRestrictQualified();
    quals.isVolatile = Type.isVolatileQualified();
    return quals;
}

QualifiedType Parser::GetQualifiedType(clang::QualType qual, const clang::TypeLoc* TL)
{
    if (qual.isNull())
        return QualifiedType();

    QualifiedType qualType;
    qualType.type = WalkType(qual, TL);
    qualType.qualifiers = GetTypeQualifiers(qual);
    return qualType;
}

//-----------------------------------//

static AccessSpecifier ConvertToAccess(clang::AccessSpecifier AS)
{
    switch (AS)
    {
    case clang::AS_private:
        return AccessSpecifier::Private;
    case clang::AS_protected:
        return AccessSpecifier::Protected;
    case clang::AS_public:
        return AccessSpecifier::Public;
    case clang::AS_none:
        return AccessSpecifier::Public;
    }

    llvm_unreachable("Unknown AccessSpecifier");
}

VTableComponent
Parser::WalkVTableComponent(const clang::VTableComponent& Component)
{
    using namespace clang;
    VTableComponent VTC;

    switch (Component.getKind())
    {
    case clang::VTableComponent::CK_VCallOffset:
    {
        VTC.kind = VTableComponentKind::VBaseOffset;
        VTC.offset = Component.getVCallOffset().getQuantity();
        break;
    }
    case clang::VTableComponent::CK_VBaseOffset:
    {
        VTC.kind = VTableComponentKind::VBaseOffset;
        VTC.offset = Component.getVBaseOffset().getQuantity();
        break;
    }
    case clang::VTableComponent::CK_OffsetToTop:
    {
        VTC.kind = VTableComponentKind::OffsetToTop;
        VTC.offset = Component.getOffsetToTop().getQuantity();
        break;
    }
    case clang::VTableComponent::CK_RTTI:
    {
        VTC.kind = VTableComponentKind::RTTI;
        auto RD = Component.getRTTIDecl();
        VTC.declaration = WalkRecordCXX(RD);
        break;
    }
    case clang::VTableComponent::CK_FunctionPointer:
    {
        VTC.kind = VTableComponentKind::FunctionPointer;
        auto MD = Component.getFunctionDecl();
        VTC.declaration = WalkMethodCXX(MD);
        break;
    }
    case clang::VTableComponent::CK_CompleteDtorPointer:
    {
        VTC.kind = VTableComponentKind::CompleteDtorPointer;
        auto MD = Component.getDestructorDecl();
        VTC.declaration = WalkMethodCXX(MD);
        break;
    }
    case clang::VTableComponent::CK_DeletingDtorPointer:
    {
        VTC.kind = VTableComponentKind::DeletingDtorPointer;
        auto MD = Component.getDestructorDecl();
        VTC.declaration = WalkMethodCXX(MD);
        break;
    }
    case clang::VTableComponent::CK_UnusedFunctionPointer:
    {
        VTC.kind = VTableComponentKind::UnusedFunctionPointer;
        auto MD = Component.getUnusedFunctionDecl();
        VTC.declaration = WalkMethodCXX(MD);
        break;
    }
    default:
        llvm_unreachable("Unknown vtable component kind");
    }

    return VTC;
}

VTableLayout Parser::WalkVTableLayout(const clang::VTableLayout& VTLayout)
{
    auto Layout = VTableLayout();

    for (const auto& VTC : VTLayout.vtable_components())
    {
        auto VTComponent = WalkVTableComponent(VTC);
        Layout.Components.push_back(VTComponent);
    }

    return Layout;
}


void Parser::WalkVTable(const clang::CXXRecordDecl* RD, Class* C)
{
    using namespace clang;

    assert(RD->isDynamicClass() && "Only dynamic classes have virtual tables");

    if (!C->layout)
        C->layout = new ClassLayout();

    auto targetABI = c->getTarget().getCXXABI().getKind();
    C->layout->ABI = GetClassLayoutAbi(targetABI);

    auto& AST = c->getASTContext();
    switch (targetABI)
    {
    case TargetCXXABI::Microsoft:
    {
        MicrosoftVTableContext VTContext(AST);

        const auto& VFPtrs = VTContext.getVFPtrOffsets(RD);
        for (const auto& VFPtrInfo : VFPtrs)
        {
            VFTableInfo Info;
            Info.VFPtrOffset = VFPtrInfo->NonVirtualOffset.getQuantity();
            Info.VFPtrFullOffset = VFPtrInfo->FullOffsetInMDC.getQuantity();

            auto& VTLayout = VTContext.getVFTableLayout(RD, VFPtrInfo->FullOffsetInMDC);
            Info.layout = WalkVTableLayout(VTLayout);

            C->layout->VFTables.push_back(Info);
        }
        break;
    }
    case TargetCXXABI::GenericItanium:
    {
        ItaniumVTableContext VTContext(AST);

        auto& VTLayout = VTContext.getVTableLayout(RD);
        C->layout->layout = WalkVTableLayout(VTLayout);
        break;
    }
    default:
        llvm_unreachable("Unsupported C++ ABI kind");
    }
}

void Parser::EnsureCompleteRecord(const clang::RecordDecl* Record,
    DeclarationContext* NS, Class* RC)
{
    using namespace clang;

    if (!RC->isIncomplete || RC->completeDeclaration)
        return;

    Decl* Definition;
    if (auto CXXRecord = dyn_cast<CXXRecordDecl>(Record))
        Definition = CXXRecord->getDefinition();
    else
        Definition = Record->getDefinition();

    if (!Definition)
        return;

    RC->completeDeclaration = WalkDeclaration(Definition);
}

Class* Parser::GetRecord(const clang::RecordDecl* Record, bool& Process)
{
    using namespace clang;
    Process = false;

    auto NS = GetNamespace(Record);
    assert(NS && "Expected a valid namespace");

    bool isCompleteDefinition = Record->isCompleteDefinition();

    Class* RC = nullptr;

    auto Name = GetTagDeclName(Record);
    auto HasEmptyName = Record->getDeclName().isEmpty();

    if (HasEmptyName)
    {
        auto USR = GetDeclUSR(Record);
        if (auto AR = NS->FindAnonymous(USR))
            RC = static_cast<Class*>(AR);
    }
    else
    {
        RC = NS->FindClass(opts->unityBuild ? Record : 0, Name,
            isCompleteDefinition, /*Create=*/false);
    }

    if (RC)
        return RC;

    RC = NS->FindClass(opts->unityBuild ? Record : 0, Name,
        isCompleteDefinition, /*Create=*/true);
    RC->isInjected = Record->isInjectedClassName();
    HandleDeclaration(Record, RC);
    EnsureCompleteRecord(Record, NS, RC);

    for (auto Redecl : Record->redecls())
    {
        if (Redecl->isImplicit() || Redecl == Record)
            continue;

        RC->Redeclarations.push_back(WalkDeclaration(Redecl));
    }

    if (HasEmptyName)
    {
        auto USR = GetDeclUSR(Record);
        NS->anonymous[USR] = RC;
    }

    if (!isCompleteDefinition)
        return RC;

    Process = true;
    return RC;
}

Class* Parser::WalkRecord(const clang::RecordDecl* Record)
{
    bool Process;
    auto RC = GetRecord(Record, Process);

    if (!RC || !Process)
        return RC;

    WalkRecord(Record, RC);

    return RC;
}

Class* Parser::WalkRecordCXX(const clang::CXXRecordDecl* Record)
{
    bool Process;
    auto RC = GetRecord(Record, Process);

    if (!RC || !Process)
        return RC;

    WalkRecordCXX(Record, RC);

    return RC;
}

static int I = 0;

static bool IsRecordValid(const clang::RecordDecl* RC,
    std::unordered_set<const clang::RecordDecl*>& Visited)
{
    using namespace clang;

    if (Visited.find(RC) != Visited.end())
        return true;

    Visited.insert(RC);
    if (RC->isInvalidDecl())
        return false;
    for (auto Field : RC->fields())
    {
        auto Type = Field->getType()->getUnqualifiedDesugaredType();
        const auto* RD = const_cast<CXXRecordDecl*>(Type->getAsCXXRecordDecl());
        if (!RD)
            RD = Type->getPointeeCXXRecordDecl();
        if (RD && !IsRecordValid(RD, Visited))
            return false;
    }
    return true;
}

static bool IsRecordValid(const clang::RecordDecl* RC)
{
    std::unordered_set<const clang::RecordDecl*> Visited;
    return IsRecordValid(RC, Visited);
}

static clang::CXXRecordDecl* GetCXXRecordDeclFromBaseType(const clang::QualType& Ty) {
    using namespace clang;

    if (auto RT = Ty->getAs<clang::RecordType>())
        return dyn_cast<clang::CXXRecordDecl>(RT->getDecl());
    else if (auto TST = Ty->getAs<clang::TemplateSpecializationType>())
        return dyn_cast<clang::CXXRecordDecl>(
            TST->getTemplateName().getAsTemplateDecl()->getTemplatedDecl());
    else if (auto Injected = Ty->getAs<clang::InjectedClassNameType>())
        return Injected->getDecl();

    assert("Could not get base CXX record from type");
    return nullptr;
}

bool Parser::HasLayout(const clang::RecordDecl* Record)
{
    if (opts->skipLayoutInfo)
        return false;

    if (Record->isDependentType() || !Record->getDefinition() ||
        !IsRecordValid(Record))
        return false;

    if (auto CXXRecord = llvm::dyn_cast<clang::CXXRecordDecl>(Record))
        for (auto Base : CXXRecord->bases())
        {
            auto CXXBase = GetCXXRecordDeclFromBaseType(Base.getType());
            if (!CXXBase || !HasLayout(CXXBase))
                return false;
        }

    return true;
}

bool Parser::IsSupported(const clang::NamedDecl* ND)
{
    return !c->getSourceManager().isInSystemHeader(ND->getBeginLoc()) ||
        (llvm::isa<clang::RecordDecl>(ND) &&
            supportedStdTypes.find(ND->getName().str()) != supportedStdTypes.end());
}

bool Parser::IsSupported(const clang::CXXMethodDecl* MD)
{
    using namespace clang;

    return !c->getSourceManager().isInSystemHeader(MD->getBeginLoc()) ||
        (isa<CXXConstructorDecl>(MD) && MD->getNumParams() == 0) ||
        isa<CXXDestructorDecl>(MD) ||
        (MD->getDeclName().isIdentifier() &&
            ((MD->getName() == "data" && MD->getNumParams() == 0 && MD->isConst()) ||
                (MD->getName() == "assign" && MD->getNumParams() == 1 &&
                    MD->parameters()[0]->getType()->isPointerType())) &&
            supportedStdTypes.find(MD->getParent()->getName().str()) !=
            supportedStdTypes.end());
}

static RecordArgABI GetRecordArgABI(
    clang::CodeGen::CGCXXABI::RecordArgABI argAbi)
{
    using namespace clang::CodeGen;
    switch (argAbi)
    {
    case CGCXXABI::RecordArgABI::RAA_DirectInMemory:
        return RecordArgABI::DirectInMemory;
    case CGCXXABI::RecordArgABI::RAA_Indirect:
        return RecordArgABI::Indirect;
    default:
        return RecordArgABI::Default;
    }
}

void Parser::WalkRecord(const clang::RecordDecl* Record, Class* RC)
{
    using namespace clang;

    if (Record->isImplicit())
        return;

    if (IsExplicit(Record))
    {
        auto headStartLoc = GetDeclStartLocation(c.get(), Record);
        auto headEndLoc = Record->getLocation(); // identifier location
        auto bodyEndLoc = Record->getEndLoc();

        auto headRange = clang::SourceRange(headStartLoc, headEndLoc);
        auto bodyRange = clang::SourceRange(headEndLoc, bodyEndLoc);

        HandlePreprocessedEntities(RC, headRange, MacroLocation::ClassHead);
        HandlePreprocessedEntities(RC, bodyRange, MacroLocation::ClassBody);
    }

    auto& Sema = c->getSema();

    RC->isUnion = Record->isUnion();
    RC->isDependent = Record->isDependentType();
    RC->isExternCContext = Record->isExternCContext();

    bool hasLayout = HasLayout(Record);

    if (hasLayout)
    {
        if (!RC->layout)
            RC->layout = new ClassLayout();

        auto targetABI = c->getTarget().getCXXABI().getKind();
        RC->layout->ABI = GetClassLayoutAbi(targetABI);

        if (auto CXXRD = llvm::dyn_cast_or_null<clang::CXXRecordDecl>(Record))
        {
            auto& CXXABI = codeGenTypes->getCXXABI();
            RC->layout->argABI = GetRecordArgABI(CXXABI.getRecordArgABI(CXXRD));
        }

        const auto& Layout = c->getASTContext().getASTRecordLayout(Record);
        RC->layout->alignment = (int)Layout.getAlignment().getQuantity();
        RC->layout->size = (int)Layout.getSize().getQuantity();
        RC->layout->dataSize = (int)Layout.getDataSize().getQuantity();

        ReadClassLayout(RC, Record, CharUnits(), true);
    }

    for (auto FD : Record->fields())
        WalkFieldCXX(FD, RC);

    if (c->getSourceManager().isInSystemHeader(Record->getBeginLoc()))
    {
        if (supportedStdTypes.find(Record->getName().str()) != supportedStdTypes.end())
        {
            for (auto D : Record->decls())
            {
                switch (D->getKind())
                {
                case Decl::CXXConstructor:
                case Decl::CXXDestructor:
                case Decl::CXXConversion:
                case Decl::CXXMethod:
                {
                    auto MD = cast<CXXMethodDecl>(D);
                    if (IsSupported(MD))
                        WalkDeclaration(MD);
                    break;
                }
                default:
                    break;
                }
            }
        }
        return;
    }

    if (opts->skipPrivateDeclarations &&
        Record->getAccess() == clang::AccessSpecifier::AS_private)
        return;

    for (auto D : Record->decls())
    {
        switch (D->getKind())
        {
        case Decl::AccessSpec:
        {
            AccessSpecDecl* AS = cast<AccessSpecDecl>(D);

            auto AccessDecl = new AccessSpecifierDecl();
            HandleDeclaration(AS, AccessDecl);

            AccessDecl->access = ConvertToAccess(AS->getAccess());
            AccessDecl->_namespace = RC;

            RC->Specifiers.push_back(AccessDecl);
            break;
        }
        case Decl::Field: // fields already handled
        case Decl::IndirectField: // FIXME: Handle indirect fields
            break;
        case Decl::CXXRecord:
            // Handle implicit records inside the class.
            if (D->isImplicit())
                continue;
            WalkDeclaration(D);
            break;
        case Decl::Friend:
        {
            FriendDecl* FD = cast<FriendDecl>(D);
            auto decl = FD->getFriendDecl();

            // Skip every friend declaration that isn't a function declaration
            if (decl && !isa<FunctionDecl>(decl))
                continue;
            WalkDeclaration(D);
            break;
        }
        case Decl::FriendTemplate:
        {
            // In this case always skip the declaration since, unlike Decl::Friend handled above,
            // it never is a declaration of a friend function or method
            break;
        }
        default:
        {
            WalkDeclaration(D);
            break;
        }
        }
    }
}

void Parser::WalkRecordCXX(const clang::CXXRecordDecl* Record, Class* RC)
{
    using namespace clang;

    if (Record->isImplicit())
        return;

    auto& Sema = c->getSema();
    Sema.ForceDeclarationOfImplicitMembers(const_cast<clang::CXXRecordDecl*>(Record));

    WalkRecord(Record, RC);

    if (!Record->hasDefinition())
        return;

    RC->isPOD = Record->isPOD();
    RC->isAbstract = Record->isAbstract();
    RC->isDynamic = Record->isDynamicClass();
    RC->isPolymorphic = Record->isPolymorphic();
    RC->hasNonTrivialDefaultConstructor = Record->hasNonTrivialDefaultConstructor();
    RC->hasNonTrivialCopyConstructor = Record->hasNonTrivialCopyConstructor();
    RC->hasNonTrivialDestructor = Record->hasNonTrivialDestructor();

    bool hasLayout = HasLayout(Record) &&
        Record->getDeclName() != c->getSema().VAListTagName;

    // Get the record layout information.
    const ASTRecordLayout* Layout = 0;
    if (hasLayout)
    {
        Layout = &c->getASTContext().getASTRecordLayout(Record);

        assert(RC->layout && "Expected a valid AST layout");
        RC->layout->hasOwnVFPtr = Layout->hasOwnVFPtr();
        RC->layout->VBPtrOffset = Layout->getVBPtrOffset().getQuantity();
    }

    // Iterate through the record bases.
    for (auto BS : Record->bases())
    {
        BaseClassSpecifier* Base = new BaseClassSpecifier();
        Base->access = ConvertToAccess(BS.getAccessSpecifier());
        Base->isVirtual = BS.isVirtual();

        auto BSTL = BS.getTypeSourceInfo()->getTypeLoc();
        Base->type = WalkType(BS.getType(), &BSTL);

        auto BaseDecl = GetCXXRecordDeclFromBaseType(BS.getType());
        if (BaseDecl && Layout)
        {
            auto Offset = BS.isVirtual() ? Layout->getVBaseClassOffset(BaseDecl)
                : Layout->getBaseClassOffset(BaseDecl);
            Base->offset = Offset.getQuantity();
        }

        RC->Bases.push_back(Base);
    }

    // Process the vtables
    if (hasLayout && Record->isDynamicClass())
        WalkVTable(Record, RC);
}

//-----------------------------------//

static TemplateSpecializationKind
WalkTemplateSpecializationKind(clang::TemplateSpecializationKind Kind)
{
    switch (Kind)
    {
    case clang::TSK_Undeclared:
        return TemplateSpecializationKind::Undeclared;
    case clang::TSK_ImplicitInstantiation:
        return TemplateSpecializationKind::ImplicitInstantiation;
    case clang::TSK_ExplicitSpecialization:
        return TemplateSpecializationKind::ExplicitSpecialization;
    case clang::TSK_ExplicitInstantiationDeclaration:
        return TemplateSpecializationKind::ExplicitInstantiationDeclaration;
    case clang::TSK_ExplicitInstantiationDefinition:
        return TemplateSpecializationKind::ExplicitInstantiationDefinition;
    }

    llvm_unreachable("Unknown template specialization kind");
}

//-----------------------------------//

struct Diagnostic
{
    clang::SourceLocation Location;
    llvm::SmallString<100> Message;
    clang::DiagnosticsEngine::Level Level;
};

struct DiagnosticConsumer : public clang::DiagnosticConsumer
{
    virtual void HandleDiagnostic(clang::DiagnosticsEngine::Level Level,
        const clang::Diagnostic& Info) override {
        // Update the base type NumWarnings and NumErrors variables.
        if (Level == clang::DiagnosticsEngine::Warning)
            NumWarnings++;

        if (Level == clang::DiagnosticsEngine::Error ||
            Level == clang::DiagnosticsEngine::Fatal)
        {
            NumErrors++;
            if (Decl)
            {
                Decl->setInvalidDecl();
                Decl = 0;
            }
        }

        auto Diag = Diagnostic();
        Diag.Location = Info.getLocation();
        Diag.Level = Level;
        Info.FormatDiagnostic(Diag.Message);
        Diagnostics.push_back(Diag);
    }

    std::vector<Diagnostic> Diagnostics;
    clang::Decl* Decl;
};

ClassTemplateSpecialization*
Parser::WalkClassTemplateSpecialization(const clang::ClassTemplateSpecializationDecl* CTS)
{
    using namespace clang;

    auto CT = WalkClassTemplate(CTS->getSpecializedTemplate());
    auto USR = GetDeclUSR(CTS);
    auto TS = CT->FindSpecialization(USR);
    if (TS != nullptr)
        return TS;

    TS = new ClassTemplateSpecialization();
    HandleDeclaration(CTS, TS);

    auto NS = GetNamespace(CTS);
    assert(NS && "Expected a valid namespace");
    TS->_namespace = NS;
    TS->name = CTS->getName().str();
    TS->templatedDecl = CT;
    TS->specializationKind = WalkTemplateSpecializationKind(CTS->getSpecializationKind());
    CT->Specializations.push_back(TS);

    auto& TAL = CTS->getTemplateArgs();
    auto TSI = CTS->getTypeAsWritten();
    if (TSI)
    {
        auto TL = TSI->getTypeLoc();
        auto TSL = TL.getAs<TemplateSpecializationTypeLoc>();
        TS->Arguments = WalkTemplateArgumentList(&TAL, &TSL);
    }
    else
    {
        TS->Arguments = WalkTemplateArgumentList(&TAL, (clang::TemplateSpecializationTypeLoc*)0);
    }

    if (CTS->isCompleteDefinition())
    {
        WalkRecordCXX(CTS, TS);
    }
    else
    {
        TS->isIncomplete = true;
        if (CTS->getDefinition())
        {
            auto Complete = WalkDeclarationDef(CTS->getDefinition());
            if (!Complete->isIncomplete)
                TS->completeDeclaration = Complete;
        }
    }

    return TS;
}

//-----------------------------------//

ClassTemplatePartialSpecialization*
Parser::WalkClassTemplatePartialSpecialization(const clang::ClassTemplatePartialSpecializationDecl* CTS)
{
    using namespace clang;

    auto CT = WalkClassTemplate(CTS->getSpecializedTemplate());
    auto USR = GetDeclUSR(CTS);
    auto TS = CT->FindPartialSpecialization(USR);
    if (TS != nullptr)
        return TS;

    TS = new ClassTemplatePartialSpecialization();
    HandleDeclaration(CTS, TS);

    auto NS = GetNamespace(CTS);
    assert(NS && "Expected a valid namespace");
    TS->_namespace = NS;
    TS->name = CTS->getName().str();
    TS->templatedDecl = CT;
    TS->specializationKind = WalkTemplateSpecializationKind(CTS->getSpecializationKind());
    CT->Specializations.push_back(TS);

    auto& TAL = CTS->getTemplateArgs();
    if (auto TSI = CTS->getTypeAsWritten())
    {
        auto TL = TSI->getTypeLoc();
        auto TSL = TL.getAs<TemplateSpecializationTypeLoc>();
        TS->Arguments = WalkTemplateArgumentList(&TAL, &TSL);
    }

    if (CTS->isCompleteDefinition())
    {
        WalkRecordCXX(CTS, TS);
    }
    else
    {
        TS->isIncomplete = true;
        if (CTS->getDefinition())
        {
            auto Complete = WalkDeclarationDef(CTS->getDefinition());
            if (!Complete->isIncomplete)
                TS->completeDeclaration = Complete;
        }
    }

    return TS;
}

//-----------------------------------//

std::vector<Declaration*> Parser::WalkTemplateParameterList(const clang::TemplateParameterList* TPL)
{
    auto params = std::vector<CppSharp::CppParser::Declaration*>();

    for (auto it = TPL->begin(); it != TPL->end(); ++it)
    {
        auto ND = *it;
        auto TP = WalkDeclaration(ND);
        params.push_back(TP);
    }

    return params;
}

//-----------------------------------//

ClassTemplate* Parser::WalkClassTemplate(const clang::ClassTemplateDecl* TD)
{
    auto NS = GetNamespace(TD);
    assert(NS && "Expected a valid namespace");

    auto USR = GetDeclUSR(TD);
    auto CT = NS->FindTemplate<ClassTemplate>(USR);
    if (CT != nullptr)
        return CT;

    CT = new ClassTemplate();
    HandleDeclaration(TD, CT);

    CT->name = GetDeclName(TD);
    CT->_namespace = NS;
    NS->Templates.push_back(CT);

    bool Process;
    auto RC = GetRecord(TD->getTemplatedDecl(), Process);
    CT->TemplatedDecl = RC;

    if (Process)
        WalkRecordCXX(TD->getTemplatedDecl(), RC);

    CT->Parameters = WalkTemplateParameterList(TD->getTemplateParameters());

    return CT;
}

//-----------------------------------//

TemplateTemplateParameter* Parser::WalkTemplateTemplateParameter(const clang::TemplateTemplateParmDecl* TTP)
{
    auto TP = walkedTemplateTemplateParameters[TTP];
    if (TP)
        return TP;

    TP = new TemplateTemplateParameter();
    HandleDeclaration(TTP, TP);
    TP->Parameters = WalkTemplateParameterList(TTP->getTemplateParameters());
    TP->isParameterPack = TTP->isParameterPack();
    TP->isPackExpansion = TTP->isPackExpansion();
    TP->isExpandedParameterPack = TTP->isExpandedParameterPack();
    if (TTP->getTemplatedDecl())
    {
        auto TD = WalkDeclaration(TTP->getTemplatedDecl());
        TP->TemplatedDecl = TD;
    }
    walkedTemplateTemplateParameters[TTP] = TP;
    return TP;
}

//-----------------------------------//

TypeTemplateParameter* Parser::WalkTypeTemplateParameter(const clang::TemplateTypeParmDecl* TTPD)
{
    auto TP = walkedTypeTemplateParameters[TTPD];
    if (TP)
        return TP;

    TP = new CppSharp::CppParser::TypeTemplateParameter();
    TP->name = GetDeclName(TTPD);
    HandleDeclaration(TTPD, TP);
    if (TTPD->hasDefaultArgument())
        TP->defaultArgument = GetQualifiedType(TTPD->getDefaultArgument());
    TP->depth = TTPD->getDepth();
    TP->index = TTPD->getIndex();
    TP->isParameterPack = TTPD->isParameterPack();
    walkedTypeTemplateParameters[TTPD] = TP;
    return TP;
}

//-----------------------------------//

NonTypeTemplateParameter* Parser::WalkNonTypeTemplateParameter(const clang::NonTypeTemplateParmDecl* NTTPD)
{
    auto NTP = walkedNonTypeTemplateParameters[NTTPD];
    if (NTP)
        return NTP;

    NTP = new CppSharp::CppParser::NonTypeTemplateParameter();
    NTP->name = GetDeclName(NTTPD);
    HandleDeclaration(NTTPD, NTP);
    if (NTTPD->hasDefaultArgument())
        NTP->defaultArgument = WalkExpressionObsolete(NTTPD->getDefaultArgument());
    NTP->depth = NTTPD->getDepth();
    NTP->index = NTTPD->getIndex();
    NTP->isParameterPack = NTTPD->isParameterPack();
    walkedNonTypeTemplateParameters[NTTPD] = NTP;
    return NTP;
}

//-----------------------------------//

UnresolvedUsingTypename* Parser::WalkUnresolvedUsingTypename(const clang::UnresolvedUsingTypenameDecl* UUTD)
{
    auto UUT = new CppSharp::CppParser::UnresolvedUsingTypename();
    HandleDeclaration(UUTD, UUT);

    return UUT;
}

//-----------------------------------//

template<typename TypeLoc>
std::vector<CppSharp::CppParser::TemplateArgument>
Parser::WalkTemplateArgumentList(const clang::TemplateArgumentList* TAL,
    TypeLoc* TSTL)
{
    using namespace clang;

    auto LocValid = TSTL && !TSTL->isNull() && TSTL->getTypePtr();

    auto params = std::vector<CppSharp::CppParser::TemplateArgument>();
    auto typeLocNumArgs = LocValid ? TSTL->getNumArgs() : 0;

    for (size_t i = 0, e = TAL->size(); i < e; i++)
    {
        auto TA = TAL->get(i);
        TemplateArgumentLoc TArgLoc;
        TemplateArgumentLoc* ArgLoc = 0;
        if (i < typeLocNumArgs && e == typeLocNumArgs)
        {
            TArgLoc = TSTL->getArgLoc(i);
            ArgLoc = &TArgLoc;
        }
        auto Arg = WalkTemplateArgument(TA, ArgLoc);
        params.push_back(Arg);
    }

    return params;
}

//-----------------------------------//

std::vector<CppSharp::CppParser::TemplateArgument>
Parser::WalkTemplateArgumentList(const clang::TemplateArgumentList* TAL,
    const clang::ASTTemplateArgumentListInfo* TALI)
{
    using namespace clang;

    auto params = std::vector<CppSharp::CppParser::TemplateArgument>();

    for (size_t i = 0, e = TAL->size(); i < e; i++)
    {
        auto TA = TAL->get(i);
        if (TALI)
        {
            auto ArgLoc = TALI->operator[](i);
            auto TP = WalkTemplateArgument(TA, &ArgLoc);
            params.push_back(TP);
        }
        else
        {
            auto TP = WalkTemplateArgument(TA, 0);
            params.push_back(TP);
        }
    }

    return params;
}

//-----------------------------------//

CppSharp::CppParser::TemplateArgument
Parser::WalkTemplateArgument(clang::TemplateArgument TA, clang::TemplateArgumentLoc* ArgLoc)
{
    auto Arg = CppSharp::CppParser::TemplateArgument();

    switch (TA.getKind())
    {
    case clang::TemplateArgument::Type:
    {
        Arg.kind = CppSharp::CppParser::TemplateArgument::ArgumentKind::Type;
        clang::TypeLoc ArgTL;
        if (ArgLoc && ArgLoc->getTypeSourceInfo())
        {
            ArgTL = ArgLoc->getTypeSourceInfo()->getTypeLoc();
        }
        auto Type = TA.getAsType();
        CompleteIfSpecializationType(Type);
        Arg.type = GetQualifiedType(Type, &ArgTL);
        break;
    }
    case clang::TemplateArgument::Declaration:
        Arg.kind = CppSharp::CppParser::TemplateArgument::ArgumentKind::Declaration;
        Arg.declaration = WalkDeclaration(TA.getAsDecl());
        break;
    case clang::TemplateArgument::NullPtr:
        Arg.kind = CppSharp::CppParser::TemplateArgument::ArgumentKind::NullPtr;
        break;
    case clang::TemplateArgument::Integral:
        Arg.kind = CppSharp::CppParser::TemplateArgument::ArgumentKind::Integral;
        //Arg.Type = WalkType(TA.getIntegralType(), 0);
        Arg.integral = TA.getAsIntegral().getLimitedValue();
        break;
    case clang::TemplateArgument::Template:
        Arg.kind = CppSharp::CppParser::TemplateArgument::ArgumentKind::Template;
        break;
    case clang::TemplateArgument::TemplateExpansion:
        Arg.kind = CppSharp::CppParser::TemplateArgument::ArgumentKind::TemplateExpansion;
        break;
    case clang::TemplateArgument::Expression:
        Arg.kind = CppSharp::CppParser::TemplateArgument::ArgumentKind::Expression;
        break;
    case clang::TemplateArgument::Pack:
        Arg.kind = CppSharp::CppParser::TemplateArgument::ArgumentKind::Pack;
        break;
    case clang::TemplateArgument::Null:
    default:
        llvm_unreachable("Unknown TemplateArgument");
    }

    return Arg;
}

//-----------------------------------//

TypeAliasTemplate* Parser::WalkTypeAliasTemplate(
    const clang::TypeAliasTemplateDecl* TD)
{
    using namespace clang;

    auto NS = GetNamespace(TD);
    assert(NS && "Expected a valid namespace");

    auto USR = GetDeclUSR(TD);
    auto TA = NS->FindTemplate<TypeAliasTemplate>(USR);
    if (TA != nullptr)
        return TA;

    TA = new TypeAliasTemplate();
    HandleDeclaration(TD, TA);

    TA->name = GetDeclName(TD);
    NS->Templates.push_back(TA);

    TA->TemplatedDecl = WalkDeclaration(TD->getTemplatedDecl());
    TA->Parameters = WalkTemplateParameterList(TD->getTemplateParameters());

    return TA;
}

//-----------------------------------//

FunctionTemplate* Parser::WalkFunctionTemplate(const clang::FunctionTemplateDecl* TD)
{
    if (opts->skipPrivateDeclarations &&
        TD->getAccess() == clang::AccessSpecifier::AS_private)
        return nullptr;

    using namespace clang;

    auto NS = GetNamespace(TD);
    assert(NS && "Expected a valid namespace");

    auto USR = GetDeclUSR(TD);
    auto FT = NS->FindTemplate<FunctionTemplate>(USR);
    if (FT != nullptr)
        return FT;

    CppSharp::CppParser::AST::Function* Function = nullptr;
    auto TemplatedDecl = TD->getTemplatedDecl();

    if (auto MD = dyn_cast<CXXMethodDecl>(TemplatedDecl))
        Function = WalkMethodCXX(MD);
    else
        Function = WalkFunction(TemplatedDecl);

    FT = new FunctionTemplate();
    HandleDeclaration(TD, FT);

    FT->name = GetDeclName(TD);
    FT->_namespace = NS;
    FT->TemplatedDecl = Function;
    FT->Parameters = WalkTemplateParameterList(TD->getTemplateParameters());

    NS->Templates.push_back(FT);

    return FT;
}

//-----------------------------------//

CppSharp::CppParser::FunctionTemplateSpecialization*
Parser::WalkFunctionTemplateSpec(clang::FunctionTemplateSpecializationInfo* FTSI, CppSharp::CppParser::Function* Function)
{
    using namespace clang;

    auto FTS = new CppSharp::CppParser::FunctionTemplateSpecialization();
    FTS->specializationKind = WalkTemplateSpecializationKind(FTSI->getTemplateSpecializationKind());
    FTS->specializedFunction = Function;
    FTS->_template = WalkFunctionTemplate(FTSI->getTemplate());
    FTS->_template->Specializations.push_back(FTS);
    if (auto TSA = FTSI->TemplateArguments)
    {
        if (auto TSAW = FTSI->TemplateArgumentsAsWritten)
        {
            if (TSA->size() == TSAW->NumTemplateArgs)
            {
                FTS->Arguments = WalkTemplateArgumentList(TSA, TSAW);
                return FTS;
            }
        }
        FTS->Arguments = WalkTemplateArgumentList(TSA,
            (const clang::ASTTemplateArgumentListInfo*)0);
    }

    return FTS;
}

//-----------------------------------//

VarTemplate* Parser::WalkVarTemplate(const clang::VarTemplateDecl* TD)
{
    auto NS = GetNamespace(TD);
    assert(NS && "Expected a valid namespace");

    auto USR = GetDeclUSR(TD);
    auto VT = NS->FindTemplate<VarTemplate>(USR);
    if (VT != nullptr)
        return VT;

    VT = new VarTemplate();
    HandleDeclaration(TD, VT);

    VT->name = GetDeclName(TD);
    VT->_namespace = NS;
    NS->Templates.push_back(VT);

    auto RC = WalkVariable(TD->getTemplatedDecl());
    VT->TemplatedDecl = RC;
    VT->Parameters = WalkTemplateParameterList(TD->getTemplateParameters());

    return VT;
}

VarTemplateSpecialization*
Parser::WalkVarTemplateSpecialization(const clang::VarTemplateSpecializationDecl* VTS)
{
    using namespace clang;

    auto VT = WalkVarTemplate(VTS->getSpecializedTemplate());
    auto USR = GetDeclUSR(VTS);
    auto TS = VT->FindSpecialization(USR);
    if (TS != nullptr)
        return TS;

    TS = new VarTemplateSpecialization();
    HandleDeclaration(VTS, TS);

    auto NS = GetNamespace(VTS);
    assert(NS && "Expected a valid namespace");
    TS->_namespace = NS;
    TS->name = VTS->getName().str();
    TS->templatedDecl = VT;
    TS->specializationKind = WalkTemplateSpecializationKind(VTS->getSpecializationKind());
    VT->Specializations.push_back(TS);

    auto& TAL = VTS->getTemplateArgs();
    auto TSI = VTS->getTypeAsWritten();
    if (TSI)
    {
        auto TL = TSI->getTypeLoc();
        auto TSL = TL.getAs<TemplateSpecializationTypeLoc>();
        TS->Arguments = WalkTemplateArgumentList(&TAL, &TSL);
    }
    else
    {
        TS->Arguments = WalkTemplateArgumentList(&TAL, (clang::TemplateSpecializationTypeLoc*)0);
    }

    WalkVariable(VTS, TS);

    return TS;
}

VarTemplatePartialSpecialization*
Parser::WalkVarTemplatePartialSpecialization(const clang::VarTemplatePartialSpecializationDecl* VTS)
{
    using namespace clang;

    auto VT = WalkVarTemplate(VTS->getSpecializedTemplate());
    auto USR = GetDeclUSR(VTS);
    auto TS = VT->FindPartialSpecialization(USR);
    if (TS != nullptr)
        return TS;

    TS = new VarTemplatePartialSpecialization();
    HandleDeclaration(VTS, TS);

    auto NS = GetNamespace(VTS);
    assert(NS && "Expected a valid namespace");
    TS->_namespace = NS;
    TS->name = VTS->getName().str();
    TS->templatedDecl = VT;
    TS->specializationKind = WalkTemplateSpecializationKind(VTS->getSpecializationKind());
    VT->Specializations.push_back(TS);

    auto& TAL = VTS->getTemplateArgs();
    if (auto TSI = VTS->getTypeAsWritten())
    {
        auto TL = TSI->getTypeLoc();
        auto TSL = TL.getAs<TemplateSpecializationTypeLoc>();
        TS->Arguments = WalkTemplateArgumentList(&TAL, &TSL);
    }

    WalkVariable(VTS, TS);

    return TS;
}

//-----------------------------------//

static CXXMethodKind GetMethodKindFromDecl(clang::DeclarationName Name)
{
    using namespace clang;

    switch (Name.getNameKind())
    {
    case DeclarationName::Identifier:
    case DeclarationName::CXXDeductionGuideName:
    case DeclarationName::ObjCZeroArgSelector:
    case DeclarationName::ObjCOneArgSelector:
    case DeclarationName::ObjCMultiArgSelector:
        return CXXMethodKind::Normal;
    case DeclarationName::CXXConstructorName:
        return CXXMethodKind::Constructor;
    case DeclarationName::CXXDestructorName:
        return CXXMethodKind::Destructor;
    case DeclarationName::CXXConversionFunctionName:
        return CXXMethodKind::Conversion;
    case DeclarationName::CXXOperatorName:
    case DeclarationName::CXXLiteralOperatorName:
        return CXXMethodKind::Operator;
    case DeclarationName::CXXUsingDirective:
        return CXXMethodKind::UsingDirective;
    }
    return CXXMethodKind::Normal;
}

static CXXOperatorKind GetOperatorKindFromDecl(clang::DeclarationName Name)
{
    using namespace clang;

    if (Name.getNameKind() != DeclarationName::CXXOperatorName)
        return CXXOperatorKind::None;

    switch (Name.getCXXOverloadedOperator())
    {
    case OO_None:
        return CXXOperatorKind::None;
    case NUM_OVERLOADED_OPERATORS:
        break;

#define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
    case OO_##Name: return CXXOperatorKind::Name;
#include "clang/Basic/OperatorKinds.def"
    }

    llvm_unreachable("Unknown OverloadedOperator");
}

Method* Parser::WalkMethodCXX(const clang::CXXMethodDecl* MD)
{
    const clang::CXXConstructorDecl* Ctor;
    if (opts->skipPrivateDeclarations &&
        MD->getAccess() == clang::AccessSpecifier::AS_private &&
        !MD->isVirtual() &&
        !MD->isCopyAssignmentOperator() &&
        !MD->isMoveAssignmentOperator() &&
        (!(Ctor = llvm::dyn_cast<clang::CXXConstructorDecl>(MD)) ||
            (!Ctor->isDefaultConstructor() && !Ctor->isCopyOrMoveConstructor())))
        return nullptr;

    using namespace clang;

    // We could be in a redeclaration, so process the primary context.
    if (MD->getPrimaryContext() != MD)
        return WalkMethodCXX(cast<CXXMethodDecl>(MD->getPrimaryContext()));

    auto RD = MD->getParent();
    auto Decl = WalkDeclaration(RD);

    auto Class = static_cast<CppSharp::CppParser::AST::Class*>(Decl);

    // Check for an already existing method that came from the same declaration.
    auto USR = GetDeclUSR(MD);
    for (auto& M : Class->Methods)
    {
        if (M->USR == USR)
        {
            return M;
        }
    }
    for (unsigned I = 0, E = Class->Templates.size(); I != E; ++I)
    {
        Template* Template = Class->Templates[I];
        if (Template->TemplatedDecl->USR == USR)
            return static_cast<Method*>(Template->TemplatedDecl);
    }

    auto Method = new CppSharp::CppParser::Method();
    HandleDeclaration(MD, Method);

    Method->access = ConvertToAccess(MD->getAccess());
    Method->methodKind = GetMethodKindFromDecl(MD->getDeclName());
    Method->isStatic = MD->isStatic();
    Method->isVirtual = MD->isVirtual();
    Method->isConst = MD->isConst();
    for (auto OverriddenMethod : MD->overridden_methods())
    {
        auto OM = WalkMethodCXX(OverriddenMethod);
        Method->OverriddenMethods.push_back(OM);
    }
    switch (MD->getRefQualifier())
    {
    case clang::RefQualifierKind::RQ_None:
        Method->refQualifier = RefQualifierKind::None;
        break;
    case clang::RefQualifierKind::RQ_LValue:
        Method->refQualifier = RefQualifierKind::LValue;
        break;
    case clang::RefQualifierKind::RQ_RValue:
        Method->refQualifier = RefQualifierKind::RValue;
        break;
    }

    Class->Methods.push_back(Method);

    WalkFunction(MD, Method);

    if (const CXXConstructorDecl* CD = dyn_cast<CXXConstructorDecl>(MD))
    {
        Method->isDefaultConstructor = CD->isDefaultConstructor();
        Method->isCopyConstructor = CD->isCopyConstructor();
        Method->isMoveConstructor = CD->isMoveConstructor();
        Method->isExplicit = CD->isExplicit();
    }
    else if (const CXXDestructorDecl* DD = dyn_cast<CXXDestructorDecl>(MD))
    {
    }
    else if (const CXXConversionDecl* CD = dyn_cast<CXXConversionDecl>(MD))
    {
        auto TL = MD->getTypeSourceInfo()->getTypeLoc().castAs<FunctionTypeLoc>();
        auto RTL = TL.getReturnLoc();
        Method->conversionType = GetQualifiedType(CD->getConversionType(), &RTL);
    }

    return Method;
}

//-----------------------------------//

Field* Parser::WalkFieldCXX(const clang::FieldDecl* FD, Class* Class)
{
    using namespace clang;

    const auto& USR = GetDeclUSR(FD);

    auto FoundField = std::find_if(Class->Fields.begin(), Class->Fields.end(),
        [&](Field* Field) { return Field->USR == USR; });

    if (FoundField != Class->Fields.end())
        return *FoundField;

    auto F = new Field();
    HandleDeclaration(FD, F);

    F->_namespace = Class;
    F->name = FD->getName().str();
    auto TL = FD->getTypeSourceInfo()->getTypeLoc();
    F->qualifiedType = GetQualifiedType(FD->getType(), &TL);
    F->access = ConvertToAccess(FD->getAccess());
    F->_class = Class;
    F->isBitField = FD->isBitField();
    if (F->isBitField && !F->isDependent && !FD->getBitWidth()->isInstantiationDependent())
        F->bitWidth = FD->getBitWidthValue(c->getASTContext());

    if (auto alignedAttr = FD->getAttr<clang::AlignedAttr>())
        F->alignAs = GetAlignAs(alignedAttr);

    Class->Fields.push_back(F);

    return F;
}

//-----------------------------------//

TranslationUnit* Parser::GetTranslationUnit(clang::SourceLocation Loc,
    SourceLocationKind* Kind)
{
    using namespace clang;

    clang::SourceManager& SM = c->getSourceManager();

    if (Loc.isMacroID())
        Loc = SM.getExpansionLoc(Loc);

    StringRef File;

    auto LocKind = GetLocationKind(Loc);
    switch (LocKind)
    {
    case SourceLocationKind::Invalid:
        File = "<invalid>";
        break;
    case SourceLocationKind::Builtin:
        File = "<built-in>";
        break;
    case SourceLocationKind::CommandLine:
        File = "<command-line>";
        break;
    default:
        File = SM.getFilename(Loc);
        assert(!File.empty() && "Expected to find a valid file");
        break;
    }

    if (Kind)
        *Kind = LocKind;

    auto Unit = opts->ASTContext->FindOrCreateModule(File.str());

    Unit->originalPtr = (void*)Unit;
    assert(Unit->originalPtr != nullptr);

    if (LocKind != SourceLocationKind::Invalid)
        Unit->isSystemHeader = SM.isInSystemHeader(Loc);

    return Unit;
}

//-----------------------------------//

TranslationUnit* Parser::GetTranslationUnit(const clang::Decl* D)
{
    clang::SourceLocation Loc = D->getLocation();

    SourceLocationKind Kind;
    TranslationUnit* Unit = GetTranslationUnit(Loc, &Kind);

    return Unit;
}

DeclarationContext* Parser::GetNamespace(const clang::Decl* D,
    const clang::DeclContext* Ctx)
{
    using namespace clang;

    auto Context = Ctx;

    // If the declaration is at global scope, just early exit.
    if (Context->isTranslationUnit())
        return GetTranslationUnit(D);

    TranslationUnit* Unit = GetTranslationUnit(cast<Decl>(Context));

    // Else we need to do a more expensive check to get all the namespaces,
    // and then perform a reverse iteration to get the namespaces in order.
    typedef SmallVector<const DeclContext*, 8> ContextsTy;
    ContextsTy Contexts;

    for (; Context != nullptr; Context = Context->getParent())
        Contexts.push_back(Context);

    assert(Contexts.back()->isTranslationUnit());
    Contexts.pop_back();

    DeclarationContext* DC = Unit;

    for (auto I = Contexts.rbegin(), E = Contexts.rend(); I != E; ++I)
    {
        const auto* Ctx = *I;

        switch (Ctx->getDeclKind())
        {
        case Decl::Namespace:
        {
            auto ND = cast<NamespaceDecl>(Ctx);
            if (ND->isAnonymousNamespace())
                continue;
            auto Name = ND->getName();
            DC = DC->FindCreateNamespace(Name.str());
            ((Namespace*)DC)->isAnonymous = ND->isAnonymousNamespace();
            ((Namespace*)DC)->isInline = ND->isInline();
            HandleDeclaration(ND, DC);
            continue;
        }
        case Decl::LinkageSpec:
        {
            const LinkageSpecDecl* LD = cast<LinkageSpecDecl>(Ctx);
            continue;
        }
        case Decl::CXXRecord:
        {
            auto RD = cast<CXXRecordDecl>(Ctx);
            DC = WalkRecordCXX(RD);
            continue;
        }
        default:
        {
            auto D = cast<Decl>(Ctx);
            auto Decl = WalkDeclaration(D);
            DC = static_cast<DeclarationContext*>(Decl);
        }
        }
    }

    return DC;
}

DeclarationContext* Parser::GetNamespace(const clang::Decl* D)
{
    return GetNamespace(D, D->getDeclContext());
}

static PrimitiveType WalkBuiltinType(const clang::BuiltinType* Builtin)
{
    assert(Builtin && "Expected a builtin type");

    switch (Builtin->getKind())
    {
    case clang::BuiltinType::Void: return PrimitiveType::Void;
    case clang::BuiltinType::Bool: return PrimitiveType::Bool;

    case clang::BuiltinType::SChar: return PrimitiveType::SChar;
    case clang::BuiltinType::Char_S: return PrimitiveType::Char;

    case clang::BuiltinType::UChar:
    case clang::BuiltinType::Char_U: return PrimitiveType::UChar;

    case clang::BuiltinType::WChar_S:
    case clang::BuiltinType::WChar_U: return PrimitiveType::WideChar;

    case clang::BuiltinType::Char16: return PrimitiveType::Char16;
    case clang::BuiltinType::Char32: return PrimitiveType::Char32;

    case clang::BuiltinType::Short: return PrimitiveType::Short;
    case clang::BuiltinType::UShort: return PrimitiveType::UShort;

    case clang::BuiltinType::Int: return PrimitiveType::Int;
    case clang::BuiltinType::UInt: return PrimitiveType::UInt;

    case clang::BuiltinType::Long: return PrimitiveType::Long;
    case clang::BuiltinType::ULong: return PrimitiveType::ULong;

    case clang::BuiltinType::LongLong: return PrimitiveType::LongLong;
    case clang::BuiltinType::ULongLong: return PrimitiveType::ULongLong;

    case clang::BuiltinType::Int128: return PrimitiveType::Int128;
    case clang::BuiltinType::UInt128: return PrimitiveType::UInt128;

    case clang::BuiltinType::Half: return PrimitiveType::Half;
    case clang::BuiltinType::Float: return PrimitiveType::Float;
    case clang::BuiltinType::Double: return PrimitiveType::Double;
    case clang::BuiltinType::LongDouble: return PrimitiveType::LongDouble;
    case clang::BuiltinType::Float128: return PrimitiveType::Float128;

    case clang::BuiltinType::NullPtr: return PrimitiveType::Null;

    default: break;
    }

    return PrimitiveType::Null;
}

//-----------------------------------//

clang::TypeLoc ResolveTypeLoc(clang::TypeLoc TL, clang::TypeLoc::TypeLocClass Class)
{
    using namespace clang;

    auto TypeLocClass = TL.getTypeLocClass();

    if (TypeLocClass == Class)
    {
        return TL;
    }
    if (TypeLocClass == TypeLoc::Qualified)
    {
        auto UTL = TL.getUnqualifiedLoc();
        TL = UTL;
    }
    else if (TypeLocClass == TypeLoc::Elaborated)
    {
        auto ETL = TL.getAs<ElaboratedTypeLoc>();
        auto ITL = ETL.getNextTypeLoc();
        TL = ITL;
    }
    else if (TypeLocClass == TypeLoc::Paren)
    {
        auto PTL = TL.getAs<ParenTypeLoc>();
        TL = PTL.getNextTypeLoc();
    }

    assert(TL.getTypeLocClass() == Class);
    return TL;
}

static FriendKind ConvertFriendKind(clang::Decl::FriendObjectKind FK)
{
    using namespace clang;

    switch (FK)
    {
    case Decl::FriendObjectKind::FOK_Declared:
        return FriendKind::Declared;
    case Decl::FriendObjectKind::FOK_Undeclared:
        return FriendKind::Undeclared;
    default:
        return FriendKind::None;
    }
}

static CallingConvention ConvertCallConv(clang::CallingConv CC)
{
    using namespace clang;

    switch (CC)
    {
    case CC_C:
        return CallingConvention::C;
    case CC_X86StdCall:
        return CallingConvention::StdCall;
    case CC_X86FastCall:
        return CallingConvention::FastCall;
    case CC_X86ThisCall:
        return CallingConvention::ThisCall;
    default:
        return CallingConvention::Unknown;
    }
}

static ExceptionSpecType ConvertExceptionType(clang::ExceptionSpecificationType EST)
{
    using namespace clang;

    switch (EST)
    {
    case ExceptionSpecificationType::EST_BasicNoexcept:
        return ExceptionSpecType::BasicNoexcept;
    case ExceptionSpecificationType::EST_DependentNoexcept:
        return ExceptionSpecType::DependentNoexcept;
    case ExceptionSpecificationType::EST_NoexceptFalse:
        return ExceptionSpecType::NoexceptFalse;
    case ExceptionSpecificationType::EST_NoexceptTrue:
        return ExceptionSpecType::NoexceptTrue;
    case ExceptionSpecificationType::EST_Dynamic:
        return ExceptionSpecType::Dynamic;
    case ExceptionSpecificationType::EST_DynamicNone:
        return ExceptionSpecType::DynamicNone;
    case ExceptionSpecificationType::EST_MSAny:
        return ExceptionSpecType::MSAny;
    case ExceptionSpecificationType::EST_Unevaluated:
        return ExceptionSpecType::Unevaluated;
    case ExceptionSpecificationType::EST_Uninstantiated:
        return ExceptionSpecType::Uninstantiated;
    case ExceptionSpecificationType::EST_Unparsed:
        return ExceptionSpecType::Unparsed;
    default:
        return ExceptionSpecType::None;
    }
}

static ParserIntType ConvertIntType(clang::TargetInfo::IntType IT)
{
    switch (IT)
    {
    case clang::TargetInfo::IntType::NoInt:
        return ParserIntType::NoInt;
    case clang::TargetInfo::IntType::SignedChar:
        return ParserIntType::SignedChar;
    case clang::TargetInfo::IntType::UnsignedChar:
        return ParserIntType::UnsignedChar;
    case clang::TargetInfo::IntType::SignedShort:
        return ParserIntType::SignedShort;
    case clang::TargetInfo::IntType::UnsignedShort:
        return ParserIntType::UnsignedShort;
    case clang::TargetInfo::IntType::SignedInt:
        return ParserIntType::SignedInt;
    case clang::TargetInfo::IntType::UnsignedInt:
        return ParserIntType::UnsignedInt;
    case clang::TargetInfo::IntType::SignedLong:
        return ParserIntType::SignedLong;
    case clang::TargetInfo::IntType::UnsignedLong:
        return ParserIntType::UnsignedLong;
    case clang::TargetInfo::IntType::SignedLongLong:
        return ParserIntType::SignedLongLong;
    case clang::TargetInfo::IntType::UnsignedLongLong:
        return ParserIntType::UnsignedLongLong;
    }

    llvm_unreachable("Unknown parser integer type");
}

static const clang::Type* GetFinalType(const clang::Type* Ty)
{
    auto FinalType = Ty;
    while (true)
    {
        FinalType = FinalType->getUnqualifiedDesugaredType();
        if (FinalType->getPointeeType().isNull())
            return FinalType;
        FinalType = FinalType->getPointeeType().getTypePtr();
    }
}

Type* Parser::WalkType(clang::QualType QualType, const clang::TypeLoc* TL,
    bool DesugarType)
{
    using namespace clang;

    if (QualType.isNull())
        return nullptr;

    auto LocValid = TL && !TL->isNull();

    const clang::Type* Type = QualType.getTypePtr();

    auto& AST = c->getASTContext();
    if (DesugarType)
    {
        clang::QualType Desugared = QualType.getDesugaredType(AST);
        assert(!Desugared.isNull() && "Expected a valid desugared type");
        Type = Desugared.getTypePtr();
    }

    CppSharp::CppParser::AST::Type* Ty = nullptr;

    assert(Type && "Expected a valid type");
    switch (Type->getTypeClass())
    {
    case clang::Type::Atomic:
    {
        auto Atomic = Type->getAs<clang::AtomicType>();
        assert(Atomic && "Expected an atomic type");

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        Ty = WalkType(Atomic->getValueType(), &Next);
        break;
    }
    case clang::Type::Attributed:
    {
        auto Attributed = Type->getAs<clang::AttributedType>();
        assert(Attributed && "Expected an attributed type");

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto AT = new AttributedType();

        auto Modified = Attributed->getModifiedType();
        AT->modified = GetQualifiedType(Modified, &Next);

        auto Equivalent = Attributed->getEquivalentType();
        AT->equivalent = GetQualifiedType(Equivalent, &Next);

        Ty = AT;
        break;
    }
    case clang::Type::Builtin:
    {
        auto Builtin = Type->getAs<clang::BuiltinType>();
        assert(Builtin && "Expected a builtin type");

        auto BT = new BuiltinType();
        BT->type = WalkBuiltinType(Builtin);

        Ty = BT;
        break;
    }
    case clang::Type::Enum:
    {
        auto ET = Type->getAs<clang::EnumType>();
        EnumDecl* ED = ET->getDecl();

        auto TT = new TagType();
        TT->declaration = TT->declaration = WalkDeclaration(ED);

        Ty = TT;
        break;
    }
    case clang::Type::Pointer:
    {
        auto Pointer = Type->getAs<clang::PointerType>();

        auto P = new PointerType();
        P->modifier = PointerType::TypeModifier::Pointer;

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto Pointee = Pointer->getPointeeType();
        P->qualifiedPointee = GetQualifiedType(Pointee, &Next);

        Ty = P;
        break;
    }
    case clang::Type::Typedef:
    {
        auto TT = Type->getAs<clang::TypedefType>();
        auto TD = TT->getDecl();

        auto TTL = TD->getTypeSourceInfo()->getTypeLoc();
        auto TDD = static_cast<TypedefNameDecl*>(WalkDeclaration(TD));

        auto Type = new TypedefType();
        Type->declaration = TDD;

        Ty = Type;
        break;
    }
    case clang::Type::Decayed:
    {
        auto DT = Type->getAs<clang::DecayedType>();

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto Type = new DecayedType();
        Type->decayed = GetQualifiedType(DT->getDecayedType(), &Next);
        Type->original = GetQualifiedType(DT->getOriginalType(), &Next);
        Type->pointee = GetQualifiedType(DT->getPointeeType(), &Next);

        Ty = Type;
        break;
    }
    case clang::Type::Elaborated:
    {
        auto ET = Type->getAs<clang::ElaboratedType>();

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        Ty = WalkType(ET->getNamedType(), &Next);
        break;
    }
    case clang::Type::Record:
    {
        auto RT = Type->getAs<clang::RecordType>();
        RecordDecl* RD = RT->getDecl();

        auto TT = new TagType();
        TT->declaration = WalkDeclaration(RD);

        Ty = TT;
        break;
    }
    case clang::Type::Paren:
    {
        auto PT = Type->getAs<clang::ParenType>();

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        Ty = WalkType(PT->getInnerType(), &Next);
        break;
    }
    case clang::Type::ConstantArray:
    {
        auto AT = AST.getAsConstantArrayType(QualType);

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto A = new ArrayType();
        auto ElemTy = AT->getElementType();
        A->qualifiedType = GetQualifiedType(ElemTy, &Next);
        A->sizeType = ArrayType::ArraySize::Constant;
        A->size = AST.getConstantArrayElementCount(AT);

        if (!ElemTy->isDependentType() && !opts->skipLayoutInfo)
            A->elementSize = (long)AST.getTypeSize(ElemTy);

        Ty = A;
        break;
    }
    case clang::Type::IncompleteArray:
    {
        auto AT = AST.getAsIncompleteArrayType(QualType);

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto A = new ArrayType();
        A->qualifiedType = GetQualifiedType(AT->getElementType(), &Next);
        A->sizeType = ArrayType::ArraySize::Incomplete;

        Ty = A;
        break;
    }
    case clang::Type::DependentSizedArray:
    {
        auto AT = AST.getAsDependentSizedArrayType(QualType);

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto A = new ArrayType();
        A->qualifiedType = GetQualifiedType(AT->getElementType(), &Next);
        A->sizeType = ArrayType::ArraySize::Dependent;
        //A->Size = AT->getSizeExpr();

        Ty = A;
        break;
    }
    case clang::Type::UnresolvedUsing:
    {
        auto UT = Type->getAs<clang::UnresolvedUsingType>();

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto U = new UnresolvedUsingType();
        U->declaration = static_cast<UnresolvedUsingTypename*>(
            WalkDeclaration(UT->getDecl()));

        Ty = U;
        break;
    }
    case clang::Type::FunctionNoProto:
    {
        auto FP = Type->getAs<clang::FunctionNoProtoType>();

        FunctionNoProtoTypeLoc FTL;
        TypeLoc RL;
        TypeLoc Next;
        if (LocValid)
        {
            while (!TL->isNull() && TL->getTypeLocClass() != TypeLoc::FunctionNoProto)
            {
                Next = TL->getNextTypeLoc();
                TL = &Next;
            }

            if (!TL->isNull() && TL->getTypeLocClass() == TypeLoc::FunctionNoProto)
            {
                FTL = TL->getAs<FunctionNoProtoTypeLoc>();
                RL = FTL.getReturnLoc();
            }
        }

        auto F = new FunctionType();
        F->returnType = GetQualifiedType(FP->getReturnType(), &RL);
        F->callingConvention = ConvertCallConv(FP->getCallConv());

        Ty = F;
        break;
    }
    case clang::Type::FunctionProto:
    {
        auto FP = Type->getAs<clang::FunctionProtoType>();

        FunctionProtoTypeLoc FTL;
        TypeLoc RL;
        TypeLoc Next;
        clang::SourceLocation ParamStartLoc;
        if (LocValid)
        {
            while (!TL->isNull() && TL->getTypeLocClass() != TypeLoc::FunctionProto)
            {
                Next = TL->getNextTypeLoc();
                TL = &Next;
            }

            if (!TL->isNull() && TL->getTypeLocClass() == TypeLoc::FunctionProto)
            {
                FTL = TL->getAs<FunctionProtoTypeLoc>();
                RL = FTL.getReturnLoc();
                ParamStartLoc = FTL.getLParenLoc();
            }
        }

        auto F = new FunctionType();
        F->returnType = GetQualifiedType(FP->getReturnType(), &RL);
        F->callingConvention = ConvertCallConv(FP->getCallConv());
        F->exceptionSpecType = ConvertExceptionType(FP->getExceptionSpecType());

        for (unsigned i = 0; i < FP->getNumParams(); ++i)
        {
            if (FTL && FTL.getParam(i))
            {
                auto PVD = FTL.getParam(i);
                auto FA = WalkParameter(PVD, ParamStartLoc);
                F->Parameters.push_back(FA);
            }
            else
            {
                auto FA = new Parameter();
                auto Arg = FP->getParamType(i);
                FA->name = "";
                FA->qualifiedType = GetQualifiedType(Arg);

                // In this case we have no valid value to use as a pointer so
                // use a special value known to the managed side to make sure
                // it gets ignored.
                FA->originalPtr = IgnorePtr;
                F->Parameters.push_back(FA);
            }
        }

        Ty = F;
        break;
    }
    case clang::Type::TypeOf:
    {
        auto TO = Type->getAs<clang::TypeOfType>();

        Ty = WalkType(TO->getUnderlyingType());
        break;
    }
    case clang::Type::TypeOfExpr:
    {
        auto TO = Type->getAs<clang::TypeOfExprType>();

        Ty = WalkType(TO->getUnderlyingExpr()->getType());
        break;
    }
    case clang::Type::MemberPointer:
    {
        auto MP = Type->getAs<clang::MemberPointerType>();

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto MPT = new MemberPointerType();
        MPT->pointee = GetQualifiedType(MP->getPointeeType(), &Next);

        Ty = MPT;
        break;
    }
    case clang::Type::TemplateSpecialization:
    {
        auto TS = Type->getAs<clang::TemplateSpecializationType>();
        auto TST = new TemplateSpecializationType();

        TemplateName Name = TS->getTemplateName();
        TST->_template = static_cast<Template*>(WalkDeclaration(
            Name.getAsTemplateDecl()));
        if (TS->isSugared())
            TST->desugared = GetQualifiedType(TS->getCanonicalTypeInternal(), TL);

        TypeLoc UTL, ETL, ITL;

        if (LocValid)
        {
            auto TypeLocClass = TL->getTypeLocClass();
            if (TypeLocClass == TypeLoc::Qualified)
            {
                UTL = TL->getUnqualifiedLoc();
                TL = &UTL;
            }
            else if (TypeLocClass == TypeLoc::Elaborated)
            {
                ETL = TL->getAs<ElaboratedTypeLoc>();
                ITL = ETL.getNextTypeLoc();
                TL = &ITL;
            }

            assert(TL->getTypeLocClass() == TypeLoc::TemplateSpecialization);
        }

        TemplateSpecializationTypeLoc TSpecTL;
        TemplateSpecializationTypeLoc* TSTL = 0;
        if (LocValid)
        {
            TSpecTL = TL->getAs<TemplateSpecializationTypeLoc>();
            TSTL = &TSpecTL;
        }

        ArrayRef<clang::TemplateArgument> TSArgs(TS->getArgs(), TS->getNumArgs());
        TemplateArgumentList TArgs(TemplateArgumentList::OnStack, TSArgs);
        TST->Arguments = WalkTemplateArgumentList(&TArgs, TSTL);

        Ty = TST;
        break;
    }
    case clang::Type::DependentTemplateSpecialization:
    {
        auto TS = Type->getAs<clang::DependentTemplateSpecializationType>();
        auto TST = new DependentTemplateSpecializationType();

        if (TS->isSugared())
            TST->desugared = GetQualifiedType(TS->getCanonicalTypeInternal(), TL);

        TypeLoc UTL, ETL, ITL;

        if (LocValid)
        {
            auto TypeLocClass = TL->getTypeLocClass();
            if (TypeLocClass == TypeLoc::Qualified)
            {
                UTL = TL->getUnqualifiedLoc();
                TL = &UTL;
            }
            else if (TypeLocClass == TypeLoc::Elaborated)
            {
                ETL = TL->getAs<ElaboratedTypeLoc>();
                ITL = ETL.getNextTypeLoc();
                TL = &ITL;
            }

            assert(TL->getTypeLocClass() == TypeLoc::DependentTemplateSpecialization);
        }

        DependentTemplateSpecializationTypeLoc TSpecTL;
        DependentTemplateSpecializationTypeLoc* TSTL = 0;
        if (LocValid)
        {
            TSpecTL = TL->getAs<DependentTemplateSpecializationTypeLoc>();
            TSTL = &TSpecTL;
        }

        ArrayRef<clang::TemplateArgument> TSArgs(TS->getArgs(), TS->getNumArgs());
        TemplateArgumentList TArgs(TemplateArgumentList::OnStack, TSArgs);
        TST->Arguments = WalkTemplateArgumentList(&TArgs, TSTL);

        Ty = TST;
        break;
    }
    case clang::Type::TemplateTypeParm:
    {
        auto TP = Type->getAs<TemplateTypeParmType>();

        auto TPT = new CppSharp::CppParser::TemplateParameterType();

        if (auto Ident = TP->getIdentifier())
            TPT->parameter->name = Ident->getName().str();

        TypeLoc UTL, ETL, ITL, Next;

        if (LocValid)
        {
            auto TypeLocClass = TL->getTypeLocClass();
            if (TypeLocClass == TypeLoc::Qualified)
            {
                UTL = TL->getUnqualifiedLoc();
                TL = &UTL;
            }
            else if (TypeLocClass == TypeLoc::Elaborated)
            {
                ETL = TL->getAs<ElaboratedTypeLoc>();
                ITL = ETL.getNextTypeLoc();
                TL = &ITL;
            }

            while (TL->getTypeLocClass() != TypeLoc::TemplateTypeParm)
            {
                Next = TL->getNextTypeLoc();
                TL = &Next;
            }

            assert(TL->getTypeLocClass() == TypeLoc::TemplateTypeParm);
            auto TTTL = TL->getAs<TemplateTypeParmTypeLoc>();

            TPT->parameter = WalkTypeTemplateParameter(TTTL.getDecl());
        }
        else if (TP->getDecl())
            TPT->parameter = WalkTypeTemplateParameter(TP->getDecl());
        TPT->depth = TP->getDepth();
        TPT->index = TP->getIndex();
        TPT->isParameterPack = TP->isParameterPack();

        Ty = TPT;
        break;
    }
    case clang::Type::SubstTemplateTypeParm:
    {
        auto TP = Type->getAs<SubstTemplateTypeParmType>();
        auto TPT = new TemplateParameterSubstitutionType();

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto RepTy = TP->getReplacementType();
        TPT->replacement = GetQualifiedType(RepTy, &Next);
        TPT->replacedParameter = (TemplateParameterType*)
            WalkType(clang::QualType(TP->getReplacedParameter(), 0), 0);
        TPT->replacedParameter->parameter = WalkTypeTemplateParameter(
            TP->getReplacedParameter()->getDecl());

        Ty = TPT;
        break;
    }
    case clang::Type::InjectedClassName:
    {
        auto ICN = Type->getAs<clang::InjectedClassNameType>();
        auto ICNT = new InjectedClassNameType();
        ICNT->_class = static_cast<Class*>(WalkDeclaration(
            ICN->getDecl()));
        ICNT->injectedSpecializationType = GetQualifiedType(
            ICN->getInjectedSpecializationType());

        Ty = ICNT;
        break;
    }
    case clang::Type::DependentName:
    {
        auto DN = Type->getAs<clang::DependentNameType>();
        auto DNT = new DependentNameType();
        switch (DN->getQualifier()->getKind())
        {
        case clang::NestedNameSpecifier::SpecifierKind::TypeSpec:
        case clang::NestedNameSpecifier::SpecifierKind::TypeSpecWithTemplate:
        {
            const auto& Qualifier = clang::QualType(DN->getQualifier()->getAsType(), 0);
            if (LocValid)
            {
                const auto& DNTL = TL->getAs<DependentNameTypeLoc>();
                if (!DNTL.isNull())
                {
                    const auto& QL = DNTL.getQualifierLoc();
                    const auto& NNSL = QL.getTypeLoc();
                    DNT->qualifier = GetQualifiedType(Qualifier, &NNSL);
                }
                else
                {
                    DNT->qualifier = GetQualifiedType(Qualifier, 0);
                }
            }
            else
            {
                DNT->qualifier = GetQualifiedType(Qualifier, 0);
            }
            break;
        }
        default: break;
        }
        DNT->identifier = DN->getIdentifier()->getName().str();

        Ty = DNT;
        break;
    }
    case clang::Type::LValueReference:
    {
        auto LR = Type->getAs<clang::LValueReferenceType>();

        auto P = new PointerType();
        P->modifier = PointerType::TypeModifier::LVReference;

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto Pointee = LR->getPointeeType();
        P->qualifiedPointee = GetQualifiedType(Pointee, &Next);

        Ty = P;
        break;
    }
    case clang::Type::RValueReference:
    {
        auto LR = Type->getAs<clang::RValueReferenceType>();

        auto P = new PointerType();
        P->modifier = PointerType::TypeModifier::RVReference;

        TypeLoc Next;
        if (LocValid) Next = TL->getNextTypeLoc();

        auto Pointee = LR->getPointeeType();
        P->qualifiedPointee = GetQualifiedType(Pointee, &Next);

        Ty = P;
        break;
    }
    case clang::Type::UnaryTransform:
    {
        auto UT = Type->getAs<clang::UnaryTransformType>();

        auto UTT = new UnaryTransformType();
        auto Loc = TL->getAs<UnaryTransformTypeLoc>().getUnderlyingTInfo()->getTypeLoc();
        UTT->desugared = GetQualifiedType(UT->isSugared() ? UT->getCanonicalTypeInternal() : UT->getBaseType(), &Loc);
        UTT->baseType = GetQualifiedType(UT->getBaseType(), &Loc);

        Ty = UTT;
        break;
    }
    case clang::Type::Vector:
    {
        auto V = Type->getAs<clang::VectorType>();

        auto VT = new VectorType();
        VT->elementType = GetQualifiedType(V->getElementType());
        VT->numElements = V->getNumElements();

        Ty = VT;
        break;
    }
    case clang::Type::PackExpansion:
    {
        // TODO: stubbed
        Ty = new PackExpansionType();
        break;
    }
    case clang::Type::Auto:
    {
        auto AT = Type->getAs<clang::AutoType>();
        if (AT->isSugared())
            Ty = WalkType(AT->getCanonicalTypeInternal());
        else
            return nullptr;
        break;
    }
    case clang::Type::Decltype:
    {
        auto DT = Type->getAs<clang::DecltypeType>();
        Ty = WalkType(DT->getUnderlyingType(), TL);
        break;
    }
    case clang::Type::MacroQualified:
    {
        auto MT = Type->getAs<clang::MacroQualifiedType>();
        Ty = WalkType(MT->getUnderlyingType(), TL);
        break;
    }
    default:
    {
        Debug("Unhandled type class '%s'\n", Type->getTypeClassName());
        return nullptr;
    }
    }

    Ty->isDependent = Type->isDependentType();
    return Ty;
}

//-----------------------------------//

Enumeration* Parser::WalkEnum(const clang::EnumDecl* ED)
{
    using namespace clang;

    auto NS = GetNamespace(ED);
    assert(NS && "Expected a valid namespace");

    auto E = NS->FindEnum(ED->getCanonicalDecl());
    if (E && !E->isIncomplete)
        return E;

    if (!E)
    {
        auto Name = GetTagDeclName(ED);
        if (!Name.empty())
            E = NS->FindEnum(Name, /*Create=*/false);
        else
        {
            // Enum with no identifier - try to find existing enum through enum items
            for (auto it = ED->enumerator_begin(); it != ED->enumerator_end(); ++it)
            {
                EnumConstantDecl* ECD = (*it);
                auto EnumItemName = ECD->getNameAsString();
                E = NS->FindEnumWithItem(EnumItemName);
                break;
            }
        }
    }

    if (E && !E->isIncomplete)
        return E;

    if (!E)
    {
        auto Name = GetTagDeclName(ED);
        if (!Name.empty())
            E = NS->FindEnum(Name, /*Create=*/true);
        else
        {
            E = new Enumeration();
            E->name = Name;
            E->_namespace = NS;
            NS->Enums.push_back(E);
        }
        HandleDeclaration(ED, E);
    }

    if (ED->isScoped())
        E->modifiers = (Enumeration::EnumModifiers)
        ((int)E->modifiers | (int)Enumeration::EnumModifiers::Scoped);

    // Get the underlying integer backing the enum.
    clang::QualType IntType = ED->getIntegerType();
    E->type = WalkType(IntType, 0);
    E->builtinType = static_cast<BuiltinType*>(WalkType(IntType, 0,
        /*DesugarType=*/true));

    if (!ED->isThisDeclarationADefinition())
    {
        E->isIncomplete = true;
        return E;
    }

    E->isIncomplete = false;
    for (auto it = ED->enumerator_begin(); it != ED->enumerator_end(); ++it)
    {
        E->Items.push_back(WalkEnumItem(*it));
    }

    return E;
}

Enumeration::Item* Parser::WalkEnumItem(clang::EnumConstantDecl* ECD)
{
    auto EnumItem = new Enumeration::Item();
    HandleDeclaration(ECD, EnumItem);

    EnumItem->name = ECD->getNameAsString();
    auto Value = ECD->getInitVal();
    EnumItem->value = Value.isSigned() ? Value.getSExtValue()
        : Value.getZExtValue();
    EnumItem->_namespace = GetNamespace(ECD);

    std::string Text;
    if (GetDeclText(ECD->getSourceRange(), Text))
        EnumItem->expression = Text;

    return EnumItem;
}

//-----------------------------------//

static const clang::CodeGen::CGFunctionInfo& GetCodeGenFunctionInfo(
    clang::CodeGen::CodeGenTypes* CodeGenTypes, const clang::FunctionDecl* FD)
{
    auto FTy = FD->getType()->getCanonicalTypeUnqualified();
    return CodeGenTypes->arrangeFreeFunctionType(
        FTy.castAs<clang::FunctionProtoType>());
}

bool Parser::CanCheckCodeGenInfo(clang::Sema& S, const clang::Type* Ty)
{
    auto FinalType = GetFinalType(Ty);

    if (FinalType->isDependentType() ||
        FinalType->isInstantiationDependentType() || FinalType->isUndeducedType())
        return false;

    if (auto RT = FinalType->getAs<clang::RecordType>())
        if (!HasLayout(RT->getDecl()))
            return false;

    // Lock in the MS inheritance model if we have a member pointer to a class,
    // else we get an assertion error inside Clang's codegen machinery.
    if (c->getTarget().getCXXABI().isMicrosoft())
    {
        if (auto MPT = Ty->getAs<clang::MemberPointerType>())
            if (!MPT->isDependentType())
                S.RequireCompleteType(clang::SourceLocation(), clang::QualType(Ty, 0), 1);
    }

    return true;
}

static clang::TypeLoc DesugarTypeLoc(const clang::TypeLoc& Loc)
{
    using namespace clang;

    switch (Loc.getTypeLocClass())
    {
    case TypeLoc::TypeLocClass::Attributed:
    {
        auto ATL = Loc.getAs<AttributedTypeLoc>();
        return ATL.getModifiedLoc();
    }
    case TypeLoc::TypeLocClass::Paren:
    {
        auto PTL = Loc.getAs<ParenTypeLoc>();
        return PTL.getInnerLoc();
    }
    default:
        break;
    }

    return Loc;
}

void Parser::CompleteIfSpecializationType(const clang::QualType& QualType)
{
    using namespace clang;

    auto Type = QualType->getUnqualifiedDesugaredType();
    auto RD = Type->getAsCXXRecordDecl();
    if (!RD)
        RD = const_cast<CXXRecordDecl*>(Type->getPointeeCXXRecordDecl());
    ClassTemplateSpecializationDecl* CTS;
    if (!RD ||
        !(CTS = llvm::dyn_cast<ClassTemplateSpecializationDecl>(RD)) ||
        CTS->isCompleteDefinition())
        return;

    auto existingClient = c->getSema().getDiagnostics().getClient();
    std::unique_ptr<::DiagnosticConsumer> SemaDiagnostics(new ::DiagnosticConsumer());
    SemaDiagnostics->Decl = CTS;
    c->getSema().getDiagnostics().setClient(SemaDiagnostics.get(), false);

    c->getSema().InstantiateClassTemplateSpecialization(CTS->getBeginLoc(),
        CTS, TSK_ImplicitInstantiation, false);

    c->getSema().getDiagnostics().setClient(existingClient, false);

    auto CT = WalkClassTemplate(CTS->getSpecializedTemplate());
    auto USR = GetDeclUSR(CTS);
    auto TS = CT->FindSpecialization(USR);
    if (TS != nullptr && TS->isIncomplete)
    {
        TS->isIncomplete = false;
        TS->specializationKind = WalkTemplateSpecializationKind(CTS->getSpecializationKind());
        WalkRecordCXX(CTS, TS);
    }
}

Parameter* Parser::WalkParameter(const clang::ParmVarDecl* PVD,
    const clang::SourceLocation& ParamStartLoc)
{
    using namespace clang;

    auto P = walkedParameters[PVD];
    if (P)
        return P;

    P = new Parameter();
    P->name = PVD->getNameAsString();

    TypeLoc PTL;
    if (auto TSI = PVD->getTypeSourceInfo())
        PTL = TSI->getTypeLoc();

    auto paramRange = PVD->getSourceRange();
    paramRange.setBegin(ParamStartLoc);

    HandlePreprocessedEntities(P, paramRange, MacroLocation::FunctionParameters);

    const auto& Type = PVD->getOriginalType();
    auto Function = PVD->getParentFunctionOrMethod();
    if (Function && cast<NamedDecl>(Function)->isExternallyVisible())
        CompleteIfSpecializationType(Type);
    P->qualifiedType = GetQualifiedType(Type, &PTL);
    P->hasDefaultValue = PVD->hasDefaultArg();
    P->index = PVD->getFunctionScopeIndex();
    if (PVD->hasDefaultArg() && !PVD->hasUnparsedDefaultArg())
    {
        if (PVD->hasUninstantiatedDefaultArg())
            P->defaultArgument = WalkExpressionObsolete(PVD->getUninstantiatedDefaultArg());
        else
            P->defaultArgument = WalkExpressionObsolete(PVD->getDefaultArg());
    }
    HandleDeclaration(PVD, P);
    walkedParameters[PVD] = P;
    auto Context = cast<Decl>(PVD->getDeclContext());
    P->_namespace = static_cast<DeclarationContext*>(WalkDeclaration(Context));

    return P;
}

void Parser::SetBody(const clang::FunctionDecl* FD, Function* F)
{
    F->body = GetFunctionBody(FD);
    F->isInline = FD->isInlined();
    if (!F->body.empty() && F->isInline)
        return;
    for (const auto& R : FD->redecls())
    {
        if (F->body.empty())
            F->body = GetFunctionBody(R);
        F->isInline |= R->isInlined();
        if (!F->body.empty() && F->isInline)
            break;
    }
}

static bool IsInvalid(clang::Stmt* Body, std::unordered_set<clang::Stmt*>& Bodies)
{
    using namespace clang;

    if (Bodies.find(Body) != Bodies.end())
        return false;
    Bodies.insert(Body);

    Decl* D = 0;
    switch (Body->getStmtClass())
    {
    case clang::Stmt::StmtClass::DeclRefExprClass:
        D = cast<clang::DeclRefExpr>(Body)->getDecl();
        break;
    case clang::Stmt::StmtClass::MemberExprClass:
        D = cast<clang::MemberExpr>(Body)->getMemberDecl();
        break;
    default:
        break;
    }
    if (D)
    {
        if (D->isInvalidDecl())
            return true;
        if (auto F = dyn_cast<FunctionDecl>(D))
            if (IsInvalid(F->getBody(), Bodies))
                return true;
    }
    for (auto C : Body->children())
        if (IsInvalid(C, Bodies))
            return true;
    return false;
}

std::stack<clang::Scope> Parser::GetScopesFor(clang::FunctionDecl* FD)
{
    using namespace clang;

    std::stack<DeclContext*> Contexts;
    DeclContext* DC = FD;
    while (DC)
    {
        Contexts.push(DC);
        DC = DC->getParent();
    }
    std::stack<Scope> Scopes;
    while (!Contexts.empty())
    {
        Scope S(Scopes.empty() ? 0 : &Scopes.top(),
            Scope::ScopeFlags::DeclScope, c->getDiagnostics());
        S.setEntity(Contexts.top());
        Scopes.push(S);
        Contexts.pop();
    }
    return Scopes;
}

void Parser::MarkValidity(Function* F)
{
    using namespace clang;

    auto FD = static_cast<FunctionDecl*>(F->originalPtr);

    if (!FD->getTemplateInstantiationPattern() || !FD->isExternallyVisible())
        return;

    auto existingClient = c->getSema().getDiagnostics().getClient();
    std::unique_ptr<::DiagnosticConsumer> SemaDiagnostics(new ::DiagnosticConsumer());
    SemaDiagnostics->Decl = FD;
    c->getSema().getDiagnostics().setClient(SemaDiagnostics.get(), false);

    auto TUScope = c->getSema().TUScope;
    std::stack<Scope> Scopes = GetScopesFor(FD);
    c->getSema().TUScope = &Scopes.top();
    c->getSema().InstantiateFunctionDefinition(FD->getBeginLoc(), FD,
        /*Recursive*/true);
    c->getSema().TUScope = TUScope;
    F->isInvalid = FD->isInvalidDecl();
    if (!F->isInvalid)
    {
        std::unordered_set<clang::Stmt*> Bodies{ 0 };
        F->isInvalid = IsInvalid(FD->getBody(), Bodies);
    }

    c->getSema().getDiagnostics().setClient(existingClient, false);
}

void Parser::WalkFunction(const clang::FunctionDecl* FD, Function* F)
{
    using namespace clang;

    assert(FD->getBuiltinID() == 0);
    auto FT = FD->getType()->getAs<clang::FunctionType>();

    auto NS = GetNamespace(FD);
    assert(NS && "Expected a valid namespace");

    F->name = FD->getNameAsString();
    F->_namespace = NS;
    F->isConstExpr = FD->isConstexpr();
    F->isVariadic = FD->isVariadic();
    F->isDependent = FD->isDependentContext();
    F->isPure = FD->isPure();
    F->isDeleted = FD->isDeleted();
    F->isDefaulted = FD->isDefaulted();
    SetBody(FD, F);
    if (auto InstantiatedFrom = FD->getTemplateInstantiationPattern())
        F->instantiatedFrom = static_cast<Function*>(WalkDeclaration(InstantiatedFrom));

    auto FK = FD->getFriendObjectKind();
    F->friendKind = ConvertFriendKind(FK);
    auto CC = FT->getCallConv();
    F->callingConvention = ConvertCallConv(CC);

    F->operatorKind = GetOperatorKindFromDecl(FD->getDeclName());

    TypeLoc RTL;
    FunctionTypeLoc FTL;
    if (auto TSI = FD->getTypeSourceInfo())
    {
        auto Loc = DesugarTypeLoc(TSI->getTypeLoc());
        FTL = Loc.getAs<FunctionTypeLoc>();
        if (FTL)
        {
            RTL = FTL.getReturnLoc();

            auto& SM = c->getSourceManager();
            auto headStartLoc = GetDeclStartLocation(c.get(), FD);
            auto headEndLoc = SM.getExpansionLoc(FTL.getLParenLoc());
            auto headRange = clang::SourceRange(headStartLoc, headEndLoc);

            HandlePreprocessedEntities(F, headRange, MacroLocation::FunctionHead);
            HandlePreprocessedEntities(F, FTL.getParensRange(), MacroLocation::FunctionParameters);
        }
    }

    auto ReturnType = FD->getReturnType();
    if (FD->isExternallyVisible())
        CompleteIfSpecializationType(ReturnType);
    F->returnType = GetQualifiedType(ReturnType, &RTL);

    const auto& Mangled = GetDeclMangledName(FD);
    F->mangled = Mangled;

    const auto& Body = GetFunctionBody(FD);
    F->body = Body;

    clang::SourceLocation ParamStartLoc = FD->getBeginLoc();
    clang::SourceLocation ResultLoc;

    auto FTSI = FD->getTypeSourceInfo();
    if (FTSI)
    {
        auto FTL = FTSI->getTypeLoc();
        while (FTL && !FTL.getAs<FunctionTypeLoc>())
            FTL = FTL.getNextTypeLoc();

        if (FTL)
        {
            auto FTInfo = FTL.castAs<FunctionTypeLoc>();
            assert(!FTInfo.isNull());

            ParamStartLoc = FTInfo.getLParenLoc();
            ResultLoc = FTInfo.getReturnLoc().getBeginLoc();
        }
    }

    clang::SourceLocation BeginLoc = FD->getBeginLoc();
    if (ResultLoc.isValid())
        BeginLoc = ResultLoc;

    clang::SourceRange Range(BeginLoc, FD->getEndLoc());

    std::string Sig;
    if (GetDeclText(Range, Sig))
        F->signature = Sig;

    for (auto VD : FD->parameters())
    {
        auto P = WalkParameter(VD, ParamStartLoc);
        F->Parameters.push_back(P);

        ParamStartLoc = VD->getEndLoc();
    }

    if (!opts->skipFunctionBodies)
    {
        if (FD->hasBody())
        {
            if (auto Body = FD->getBody())
                F->bodyStmt = WalkStatement(Body);
        }
    }

    auto& CXXABI = codeGenTypes->getCXXABI();
    bool HasThisReturn = false;
    if (auto CD = dyn_cast<CXXConstructorDecl>(FD))
        HasThisReturn = CXXABI.HasThisReturn(GlobalDecl(CD, Ctor_Complete));
    else if (auto DD = dyn_cast<CXXDestructorDecl>(FD))
        HasThisReturn = CXXABI.HasThisReturn(GlobalDecl(DD, Dtor_Complete));
    else
        HasThisReturn = CXXABI.HasThisReturn(FD);

    F->hasThisReturn = HasThisReturn;

    if (auto FTSI = FD->getTemplateSpecializationInfo())
        F->specializationInfo = WalkFunctionTemplateSpec(FTSI, F);

    const CXXMethodDecl* MD;
    if (FD->isDependentContext() ||
        ((MD = dyn_cast<CXXMethodDecl>(FD)) && !MD->isStatic() &&
            !HasLayout(cast<CXXRecordDecl>(MD->getDeclContext()))) ||
        !CanCheckCodeGenInfo(c->getSema(), FD->getReturnType().getTypePtr()) ||
        std::any_of(FD->parameters().begin(), FD->parameters().end(),
            [this](auto* P) { return !CanCheckCodeGenInfo(c->getSema(), P->getType().getTypePtr()); }))
    {
        F->qualifiedType = GetQualifiedType(FD->getType(), &FTL);
        return;
    }

    auto& CGInfo = GetCodeGenFunctionInfo(codeGenTypes.get(), FD);
    F->isReturnIndirect = CGInfo.getReturnInfo().isIndirect() ||
        CGInfo.getReturnInfo().isInAlloca();

    unsigned Index = 0;
    for (const auto& Arg : CGInfo.arguments())
    {
        F->Parameters[Index++]->isIndirect =
            Arg.info.isIndirect() && !Arg.info.getIndirectByVal();
    }

    MarkValidity(F);
    F->qualifiedType = GetQualifiedType(FD->getType(), &FTL);
}

Function* Parser::WalkFunction(const clang::FunctionDecl* FD)
{
    using namespace clang;

    assert(FD->getBuiltinID() == 0);

    auto NS = GetNamespace(FD);
    assert(NS && "Expected a valid namespace");

    auto USR = GetDeclUSR(FD);
    auto F = NS->FindFunction(USR);
    if (F != nullptr)
        return F;

    F = new Function();
    HandleDeclaration(FD, F);
    NS->Functions.push_back(F);
    WalkFunction(FD, F);

    return F;
}

//-----------------------------------//

SourceLocationKind Parser::GetLocationKind(const clang::SourceLocation& Loc)
{
    using namespace clang;

    clang::SourceManager& SM = c->getSourceManager();
    clang::PresumedLoc PLoc = SM.getPresumedLoc(Loc);

    if (PLoc.isInvalid())
        return SourceLocationKind::Invalid;

    const char* FileName = PLoc.getFilename();

    if (strcmp(FileName, "<built-in>") == 0)
        return SourceLocationKind::Builtin;

    if (strcmp(FileName, "<command line>") == 0)
        return SourceLocationKind::CommandLine;

    if (SM.getFileCharacteristic(Loc) == clang::SrcMgr::C_User)
        return SourceLocationKind::User;

    return SourceLocationKind::System;
}

bool Parser::IsValidDeclaration(const clang::SourceLocation& Loc)
{
    auto Kind = GetLocationKind(Loc);

    return Kind == SourceLocationKind::User;
}

//-----------------------------------//

void Parser::WalkAST(clang::TranslationUnitDecl* TU)
{
    for (auto D : TU->decls())
    {
        if (D->getBeginLoc().isValid() &&
            !c->getSourceManager().isInSystemHeader(D->getBeginLoc()))
            WalkDeclarationDef(D);
    }
}

//-----------------------------------//

void Parser::WalkVariable(const clang::VarDecl* VD, Variable* Var)
{
    HandleDeclaration(VD, Var);

    Var->isConstExpr = VD->isConstexpr();
    Var->name = VD->getName().str();
    Var->access = ConvertToAccess(VD->getAccess());
    Var->initializer = VD->getAnyInitializer() ? WalkVariableInitializerExpression(VD->getAnyInitializer()) : nullptr;

    auto TL = VD->getTypeSourceInfo()->getTypeLoc();
    Var->qualifiedType = GetQualifiedType(VD->getType(), &TL);

    auto Mangled = GetDeclMangledName(VD);
    Var->mangled = Mangled;
}

Variable* Parser::WalkVariable(const clang::VarDecl* VD)
{
    using namespace clang;

    auto NS = GetNamespace(VD);
    assert(NS && "Expected a valid namespace");

    auto USR = GetDeclUSR(VD);
    if (auto Var = NS->FindVariable(USR))
        return Var;

    auto Var = new Variable();
    Var->_namespace = NS;

    WalkVariable(VD, Var);

    NS->Variables.push_back(Var);

    return Var;
}

//-----------------------------------//

Friend* Parser::WalkFriend(const clang::FriendDecl* FD)
{
    using namespace clang;

    auto NS = GetNamespace(FD);
    assert(NS && "Expected a valid namespace");

    auto FriendDecl = FD->getFriendDecl();

    // Work around clangIndex's lack of USR handling for friends and pass the
    // pointed to friend declaration instead.
    auto USR = GetDeclUSR(FriendDecl ? ((Decl*)FriendDecl) : FD);
    if (auto F = NS->FindFriend(USR))
        return F;

    auto F = new Friend();
    HandleDeclaration(FD, F);
    F->_namespace = NS;

    if (FriendDecl)
    {
        F->declaration = GetDeclarationFromFriend(FriendDecl);
    }

    NS->Friends.push_back(F);

    return F;
}

//-----------------------------------//

bool Parser::GetDeclText(clang::SourceRange SR, std::string& Text)
{
    using namespace clang;
    clang::SourceManager& SM = c->getSourceManager();
    const LangOptions& LangOpts = c->getLangOpts();

    auto Range = CharSourceRange::getTokenRange(SR);

    bool Invalid;
    Text = Lexer::getSourceText(Range, SM, LangOpts, &Invalid).str();

    return !Invalid && !Text.empty();
}

PreprocessedEntity* Parser::WalkPreprocessedEntity(
    Declaration* Decl, clang::PreprocessedEntity* PPEntity)
{
    using namespace clang;

    for (unsigned I = 0, E = Decl->PreprocessedEntities.size();
        I != E; ++I)
    {
        auto Entity = Decl->PreprocessedEntities[I];
        if (Entity->originalPtr == PPEntity)
            return Entity;
    }

    auto& P = c->getPreprocessor();

    PreprocessedEntity* Entity = 0;

    switch (PPEntity->getKind())
    {
    case clang::PreprocessedEntity::MacroExpansionKind:
    {
        auto ME = cast<clang::MacroExpansion>(PPEntity);
        auto Expansion = new MacroExpansion();
        auto MD = ME->getDefinition();
        if (MD && MD->getKind() != clang::PreprocessedEntity::InvalidKind)
            Expansion->definition = (MacroDefinition*)
            WalkPreprocessedEntity(Decl, ME->getDefinition());
        Entity = Expansion;

        std::string Text;
        GetDeclText(PPEntity->getSourceRange(), Text);

        static_cast<MacroExpansion*>(Entity)->text = Text;
        break;
    }
    case clang::PreprocessedEntity::MacroDefinitionKind:
    {
        auto MD = cast<clang::MacroDefinitionRecord>(PPEntity);

        if (!IsValidDeclaration(MD->getLocation()))
            break;

        const IdentifierInfo* II = MD->getName();
        assert(II && "Expected valid identifier info");

        MacroInfo* MI = P.getMacroInfo((IdentifierInfo*)II);

        if (!MI || MI->isBuiltinMacro())
            break;

        clang::SourceManager& SM = c->getSourceManager();
        const LangOptions& LangOpts = c->getLangOpts();

        auto Loc = MI->getDefinitionLoc();

        if (!IsValidDeclaration(Loc))
            break;

        clang::SourceLocation BeginExpr =
            Lexer::getLocForEndOfToken(Loc, 0, SM, LangOpts);

        auto Range = clang::CharSourceRange::getTokenRange(
            BeginExpr, MI->getDefinitionEndLoc());

        bool Invalid;
        StringRef Expression = Lexer::getSourceText(Range, SM, LangOpts,
            &Invalid);

        if (Invalid || Expression.empty())
            break;

        auto Definition = new MacroDefinition();
        Definition->lineNumberStart = SM.getExpansionLineNumber(MD->getLocation());
        Definition->lineNumberEnd = SM.getExpansionLineNumber(MD->getLocation());
        Entity = Definition;

        Definition->name = II->getName().trim().str();
        Definition->expression = Expression.trim().str();
    }
    case clang::PreprocessedEntity::InclusionDirectiveKind:
        // nothing to be done for InclusionDirectiveKind
        break;
    default:
        llvm_unreachable("Unknown PreprocessedEntity");
    }

    if (!Entity)
        return nullptr;

    Entity->originalPtr = PPEntity;
    auto Namespace = GetTranslationUnit(PPEntity->getSourceRange().getBegin());

    if (Decl->kind == CppSharp::CppParser::AST::DeclarationKind::TranslationUnit)
    {
        Namespace->PreprocessedEntities.push_back(Entity);
    }
    else
    {
        Decl->PreprocessedEntities.push_back(Entity);
    }

    return Entity;
}

void Parser::HandlePreprocessedEntities(Declaration* Decl)
{
    using namespace clang;
    auto PPRecord = c->getPreprocessor().getPreprocessingRecord();

    for (auto it = PPRecord->begin(); it != PPRecord->end(); ++it)
    {
        clang::PreprocessedEntity* PPEntity = (*it);
        auto Entity = WalkPreprocessedEntity(Decl, PPEntity);
    }
}

AST::ExpressionObsolete* Parser::WalkExpressionObsolete(const clang::Expr* Expr)
{
    using namespace clang;

    switch (Expr->getStmtClass())
    {
    case clang::Stmt::BinaryOperatorClass:
    {
        auto BinaryOperator = cast<clang::BinaryOperator>(Expr);
        return new AST::BinaryOperatorObsolete(GetStringFromStatement(Expr),
            WalkExpressionObsolete(BinaryOperator->getLHS()), WalkExpressionObsolete(BinaryOperator->getRHS()),
            BinaryOperator->getOpcodeStr().str());
    }
    case clang::Stmt::CallExprClass:
    {
        auto CallExpr = cast<clang::CallExpr>(Expr);
        auto CallExpression = new AST::CallExprObsolete(GetStringFromStatement(Expr),
            CallExpr->getCalleeDecl() ? WalkDeclaration(CallExpr->getCalleeDecl()) : 0);
        for (auto arg : CallExpr->arguments())
        {
            CallExpression->Arguments.push_back(WalkExpressionObsolete(arg));
        }
        return CallExpression;
    }
    case clang::Stmt::DeclRefExprClass:
        return new AST::ExpressionObsolete(GetStringFromStatement(Expr), StatementClassObsolete::DeclRefExprClass,
            WalkDeclaration(cast<clang::DeclRefExpr>(Expr)->getDecl()));
    case clang::Stmt::CStyleCastExprClass:
    case clang::Stmt::CXXConstCastExprClass:
    case clang::Stmt::CXXDynamicCastExprClass:
    case clang::Stmt::CXXFunctionalCastExprClass:
    case clang::Stmt::CXXReinterpretCastExprClass:
    case clang::Stmt::CXXStaticCastExprClass:
    case clang::Stmt::ImplicitCastExprClass:
        return WalkExpressionObsolete(cast<clang::CastExpr>(Expr)->getSubExprAsWritten());
    case clang::Stmt::CXXOperatorCallExprClass:
    {
        auto OperatorCallExpr = cast<clang::CXXOperatorCallExpr>(Expr);
        return new AST::ExpressionObsolete(GetStringFromStatement(Expr), StatementClassObsolete::CXXOperatorCallExpr,
            OperatorCallExpr->getCalleeDecl() ? WalkDeclaration(OperatorCallExpr->getCalleeDecl()) : 0);
    }
    case clang::Stmt::CXXConstructExprClass:
    case clang::Stmt::CXXTemporaryObjectExprClass:
    {
        auto ConstructorExpr = cast<clang::CXXConstructExpr>(Expr);
        if (ConstructorExpr->getNumArgs() == 1)
        {
            auto Arg = ConstructorExpr->getArg(0);
            auto TemporaryExpr = dyn_cast<clang::MaterializeTemporaryExpr>(Arg);
            if (TemporaryExpr)
            {
                auto SubTemporaryExpr = TemporaryExpr->getSubExpr();
                auto Cast = dyn_cast<clang::CastExpr>(SubTemporaryExpr);
                if (!Cast ||
                    (Cast->getSubExprAsWritten()->getStmtClass() != clang::Stmt::IntegerLiteralClass &&
                        Cast->getSubExprAsWritten()->getStmtClass() != clang::Stmt::CXXNullPtrLiteralExprClass))
                    return WalkExpressionObsolete(SubTemporaryExpr);
                return new AST::CXXConstructExprObsolete(GetStringFromStatement(Expr),
                    WalkDeclaration(ConstructorExpr->getConstructor()));
            }
        }
        auto ConstructorExpression = new AST::CXXConstructExprObsolete(GetStringFromStatement(Expr),
            WalkDeclaration(ConstructorExpr->getConstructor()));
        for (auto arg : ConstructorExpr->arguments())
        {
            ConstructorExpression->Arguments.push_back(WalkExpressionObsolete(arg));
        }
        return ConstructorExpression;
    }
    case clang::Stmt::CXXBindTemporaryExprClass:
        return WalkExpressionObsolete(cast<clang::CXXBindTemporaryExpr>(Expr)->getSubExpr());
    case clang::Stmt::CXXDefaultArgExprClass:
        return WalkExpressionObsolete(cast<clang::CXXDefaultArgExpr>(Expr)->getExpr());
    case clang::Stmt::MaterializeTemporaryExprClass:
        return WalkExpressionObsolete(cast<clang::MaterializeTemporaryExpr>(Expr)->getSubExpr());
    default:
        break;
    }

    if (!Expr->isValueDependent())
    {
        clang::Expr::EvalResult integer;
        if (Expr->getStmtClass() == clang::Stmt::CharacterLiteralClass)
        {
            auto result = GetStringFromStatement(Expr);
            if (!result.empty() &&
                result.front() != '\'' &&
                Expr->EvaluateAsInt(integer, c->getASTContext()))
            {
                result = integer.Val.getInt().toString(10);
            }

            return new AST::ExpressionObsolete(result);
        }
        else if (Expr->getStmtClass() != clang::Stmt::CXXBoolLiteralExprClass &&
            Expr->getStmtClass() != clang::Stmt::UnaryExprOrTypeTraitExprClass &&
            Expr->EvaluateAsInt(integer, c->getASTContext())
            )
        {
            return new AST::ExpressionObsolete(integer.Val.getInt().toString(10));
        }
    }

    return new AST::ExpressionObsolete(GetStringFromStatement(Expr));
}

AST::ExpressionObsolete* Parser::WalkVariableInitializerExpression(const clang::Expr* Expr)
{
    using namespace clang;

    if (IsCastStmt(Expr->getStmtClass()))
        return WalkVariableInitializerExpression(cast<clang::CastExpr>(Expr)->getSubExprAsWritten());

    if (IsLiteralStmt(Expr->getStmtClass()))
        return WalkExpressionObsolete(Expr);

    clang::Expr::EvalResult result;
    if (Expr->EvaluateAsConstantExpr(result, clang::Expr::ConstExprUsage::EvaluateForCodeGen, c->getASTContext(), false))
    {
        std::string s;
        llvm::raw_string_ostream out(s);
        APValuePrinter printer{ c->getASTContext(), out };

        if (printer.Print(result.Val, Expr->getType()))
            return new AST::ExpressionObsolete(out.str());
    }

    return WalkExpressionObsolete(Expr);
}

bool Parser::IsCastStmt(clang::Stmt::StmtClass stmt)
{
    switch (stmt)
    {
    case clang::Stmt::CStyleCastExprClass:
    case clang::Stmt::CXXConstCastExprClass:
    case clang::Stmt::CXXDynamicCastExprClass:
    case clang::Stmt::CXXFunctionalCastExprClass:
    case clang::Stmt::CXXReinterpretCastExprClass:
    case clang::Stmt::CXXStaticCastExprClass:
    case clang::Stmt::ImplicitCastExprClass:
        return true;
    default:
        return false;
    }
}

bool Parser::IsLiteralStmt(clang::Stmt::StmtClass stmt)
{
    switch (stmt)
    {
    case clang::Stmt::CharacterLiteralClass:
    case clang::Stmt::FixedPointLiteralClass:
    case clang::Stmt::FloatingLiteralClass:
    case clang::Stmt::IntegerLiteralClass:
    case clang::Stmt::StringLiteralClass:
    case clang::Stmt::ImaginaryLiteralClass:
    case clang::Stmt::UserDefinedLiteralClass:
    case clang::Stmt::CXXNullPtrLiteralExprClass:
    case clang::Stmt::CXXBoolLiteralExprClass:
        return true;
    default:
        return false;
    }
}

std::string Parser::GetStringFromStatement(const clang::Stmt* Statement)
{
    using namespace clang;

    PrintingPolicy Policy(c->getLangOpts());
    std::string s;
    llvm::raw_string_ostream as(s);
    Statement->printPretty(as, 0, Policy);
    return as.str();
}

std::string Parser::GetFunctionBody(const clang::FunctionDecl* FD)
{
    if (!FD->getBody())
        return "";

    clang::PrintingPolicy Policy(c->getLangOpts());
    std::string s;
    llvm::raw_string_ostream as(s);
    FD->getBody()->printPretty(as, 0, Policy);
    return as.str();
}

void Parser::HandlePreprocessedEntities(Declaration* Decl,
    clang::SourceRange sourceRange,
    MacroLocation macroLocation)
{
    if (sourceRange.isInvalid()) return;

    auto& SourceMgr = c->getSourceManager();
    auto isBefore = SourceMgr.isBeforeInTranslationUnit(sourceRange.getEnd(),
        sourceRange.getBegin());

    if (isBefore) return;

    assert(!SourceMgr.isBeforeInTranslationUnit(sourceRange.getEnd(),
        sourceRange.getBegin()));

    using namespace clang;
    auto PPRecord = c->getPreprocessor().getPreprocessingRecord();

    auto Range = PPRecord->getPreprocessedEntitiesInRange(sourceRange);

    for (auto PPEntity : Range)
    {
        auto Entity = WalkPreprocessedEntity(Decl, PPEntity);
        if (!Entity) continue;

        if (Entity->macroLocation == MacroLocation::Unknown)
            Entity->macroLocation = macroLocation;
    }
}

void Parser::HandleOriginalText(const clang::Decl* D, Declaration* Decl)
{
    auto& SM = c->getSourceManager();
    auto& LangOpts = c->getLangOpts();

    auto Range = clang::CharSourceRange::getTokenRange(D->getSourceRange());

    bool Invalid;
    auto DeclText = clang::Lexer::getSourceText(Range, SM, LangOpts, &Invalid);

    if (!Invalid)
        Decl->debugText = DeclText.str();
}

void Parser::HandleDeclaration(const clang::Decl* D, Declaration* Decl)
{
    if (Decl->originalPtr != nullptr)
        return;

    Decl->originalPtr = (void*)D;
    Decl->USR = GetDeclUSR(D);
    Decl->isImplicit = D->isImplicit();
    Decl->location = SourceLocation(D->getLocation().getRawEncoding());
    auto IsDeclExplicit = IsExplicit(D);
    if (IsDeclExplicit)
    {
        Decl->lineNumberStart = c->getSourceManager().getExpansionLineNumber(D->getBeginLoc());
        Decl->lineNumberEnd = c->getSourceManager().getExpansionLineNumber(D->getEndLoc());
    }
    else
    {
        Decl->lineNumberStart = -1;
        Decl->lineNumberEnd = -1;
    }

    if (Decl->PreprocessedEntities.empty() && !D->isImplicit())
    {
        if (clang::dyn_cast<clang::TranslationUnitDecl>(D))
        {
            HandlePreprocessedEntities(Decl);
        }
        else if (clang::dyn_cast<clang::ParmVarDecl>(D))
        {
            // Ignore function parameters as we already walk their preprocessed entities.
        }
        else if (IsDeclExplicit)
        {
            auto startLoc = GetDeclStartLocation(c.get(), D);
            auto endLoc = D->getEndLoc();
            auto range = clang::SourceRange(startLoc, endLoc);

            HandlePreprocessedEntities(Decl, range);
        }
    }

    if (IsDeclExplicit)
        HandleOriginalText(D, Decl);
    HandleComments(D, Decl);

    if (const clang::ValueDecl* VD = clang::dyn_cast_or_null<clang::ValueDecl>(D))
        Decl->isDependent = VD->getType()->isDependentType();

    if (const clang::DeclContext* DC = clang::dyn_cast_or_null<clang::DeclContext>(D))
        Decl->isDependent |= DC->isDependentContext();

    Decl->access = ConvertToAccess(D->getAccess());
}

//-----------------------------------//

Declaration* Parser::WalkDeclarationDef(clang::Decl* D)
{
    auto Decl = WalkDeclaration(D);
    if (!Decl || Decl->definitionOrder > 0)
        return Decl;
    // We store a definition order index into the declarations.
    // This is needed because declarations are added to their contexts as
    // soon as they are referenced and we need to know the original order
    // of the declarations.
    clang::RecordDecl* RecordDecl;
    if ((RecordDecl = llvm::dyn_cast<clang::RecordDecl>(D)) &&
        RecordDecl->isCompleteDefinition())
        Decl->definitionOrder = index++;
    return Decl;
}

Declaration* Parser::WalkDeclaration(const clang::Decl* D)
{
    using namespace clang;

    if (D == nullptr)
        return nullptr;

    Declaration* Decl = nullptr;

    auto Kind = D->getKind();
    switch (D->getKind())
    {
    case Decl::Record:
    {
        auto RD = cast<RecordDecl>(D);
        Decl = WalkRecord(RD);
        break;
    }
    case Decl::CXXRecord:
    {
        auto RD = cast<CXXRecordDecl>(D);
        Decl = WalkRecordCXX(RD);
        break;
    }
    case Decl::ClassTemplate:
    {
        auto TD = cast<ClassTemplateDecl>(D);
        auto Template = WalkClassTemplate(TD);

        Decl = Template;
        break;
    }
    case Decl::ClassTemplateSpecialization:
    {
        auto TS = cast<ClassTemplateSpecializationDecl>(D);
        auto CT = WalkClassTemplateSpecialization(TS);

        Decl = CT;
        break;
    }
    case Decl::ClassTemplatePartialSpecialization:
    {
        auto TS = cast<ClassTemplatePartialSpecializationDecl>(D);
        auto CT = WalkClassTemplatePartialSpecialization(TS);

        Decl = CT;
        break;
    }
    case Decl::FunctionTemplate:
    {
        auto TD = cast<FunctionTemplateDecl>(D);
        auto FT = WalkFunctionTemplate(TD);

        Decl = FT;
        break;
    }
    case Decl::VarTemplate:
    {
        auto TD = cast<VarTemplateDecl>(D);
        auto Template = WalkVarTemplate(TD);

        Decl = Template;
        break;
    }
    case Decl::VarTemplateSpecialization:
    {
        auto TS = cast<VarTemplateSpecializationDecl>(D);
        auto CT = WalkVarTemplateSpecialization(TS);

        Decl = CT;
        break;
    }
    case Decl::VarTemplatePartialSpecialization:
    {
        auto TS = cast<VarTemplatePartialSpecializationDecl>(D);
        auto CT = WalkVarTemplatePartialSpecialization(TS);

        Decl = CT;
        break;
    }
    case Decl::TypeAliasTemplate:
    {
        auto TD = cast<TypeAliasTemplateDecl>(D);
        auto TA = WalkTypeAliasTemplate(TD);

        Decl = TA;
        break;
    }
    case Decl::Enum:
    {
        auto ED = cast<EnumDecl>(D);
        Decl = WalkEnum(ED);
        break;
    }
    case Decl::EnumConstant:
    {
        auto ED = cast<EnumConstantDecl>(D);
        auto E = static_cast<Enumeration*>(GetNamespace(ED));
        assert(E && "Expected a valid enumeration");
        Decl = E->FindItemByName(ED->getNameAsString());
        break;
    }
    case Decl::Function:
    {
        auto FD = cast<FunctionDecl>(D);

        // Check for and ignore built-in functions.
        if (FD->getBuiltinID() != 0)
            break;

        Decl = WalkFunction(FD);
        break;
    }
    case Decl::LinkageSpec:
    {
        auto LS = cast<LinkageSpecDecl>(D);

        for (auto it = LS->decls_begin(); it != LS->decls_end(); ++it)
        {
            clang::Decl* D = (*it);
            Decl = WalkDeclarationDef(D);
        }

        break;
    }
    case Decl::Typedef:
    {
        auto TD = cast<clang::TypedefDecl>(D);

        auto NS = GetNamespace(TD);
        auto Name = GetDeclName(TD);
        auto Typedef = NS->FindTypedef(Name, /*Create=*/false);
        if (Typedef) return Typedef;

        Typedef = NS->FindTypedef(Name, /*Create=*/true);
        HandleDeclaration(TD, Typedef);

        auto TTL = TD->getTypeSourceInfo()->getTypeLoc();
        // resolve the typedef before adding it to the list otherwise it might be found and returned prematurely
        // see "typedef _Aligned<16, char>::type type;" and the related classes in Common.h in the tests
        Typedef->qualifiedType = GetQualifiedType(TD->getUnderlyingType(), &TTL);
        AST::TypedefDecl* Existing;
        // if the typedef was added along the way, the just created one is useless, delete it
        if ((Existing = NS->FindTypedef(Name, /*Create=*/false)))
            delete Typedef;
        else
            NS->Typedefs.push_back(Existing = Typedef);

        Decl = Existing;
        break;
    }
    case Decl::TypeAlias:
    {
        auto TD = cast<clang::TypeAliasDecl>(D);

        auto NS = GetNamespace(TD);
        auto Name = GetDeclName(TD);
        auto TypeAlias = NS->FindTypeAlias(Name, /*Create=*/false);
        if (TypeAlias) return TypeAlias;

        TypeAlias = NS->FindTypeAlias(Name, /*Create=*/true);
        HandleDeclaration(TD, TypeAlias);

        auto TTL = TD->getTypeSourceInfo()->getTypeLoc();
        // see above the case for "Typedef"
        TypeAlias->qualifiedType = GetQualifiedType(TD->getUnderlyingType(), &TTL);
        AST::TypeAlias* Existing;
        if ((Existing = NS->FindTypeAlias(Name, /*Create=*/false)))
            delete TypeAlias;
        else
            NS->TypeAliases.push_back(Existing = TypeAlias);

        if (auto TAT = TD->getDescribedAliasTemplate())
            TypeAlias->describedAliasTemplate = WalkTypeAliasTemplate(TAT);

        Decl = Existing;
        break;
    }
    case Decl::TranslationUnit:
    {
        Decl = GetTranslationUnit(D);
        break;
    }
    case Decl::Namespace:
    {
        auto ND = cast<NamespaceDecl>(D);

        for (auto D : ND->decls())
        {
            if (!isa<NamedDecl>(D) || IsSupported(cast<NamedDecl>(D)))
                Decl = WalkDeclarationDef(D);
        }

        break;
    }
    case Decl::Var:
    {
        auto VD = cast<VarDecl>(D);
        Decl = WalkVariable(VD);
        break;
    }
    case Decl::CXXConstructor:
    case Decl::CXXDestructor:
    case Decl::CXXConversion:
    case Decl::CXXMethod:
    {
        auto MD = cast<CXXMethodDecl>(D);
        Decl = WalkMethodCXX(MD);
        if (Decl == nullptr)
            return Decl;

        auto NS = GetNamespace(MD);
        Decl->_namespace = NS;
        break;
    }
    case Decl::Friend:
    {
        auto FD = cast<FriendDecl>(D);
        Decl = WalkFriend(FD);
        break;
    }
    case Decl::TemplateTemplateParm:
    {
        auto TTP = cast<TemplateTemplateParmDecl>(D);
        Decl = WalkTemplateTemplateParameter(TTP);
        break;
    }
    case Decl::TemplateTypeParm:
    {
        auto TTPD = cast<TemplateTypeParmDecl>(D);
        Decl = WalkTypeTemplateParameter(TTPD);
        break;
    }
    case Decl::NonTypeTemplateParm:
    {
        auto NTTPD = cast<NonTypeTemplateParmDecl>(D);
        Decl = WalkNonTypeTemplateParameter(NTTPD);
        break;
    }
    case Decl::UnresolvedUsingTypename:
    {
        auto UUTD = cast<UnresolvedUsingTypenameDecl>(D);
        Decl = WalkUnresolvedUsingTypename(UUTD);
        break;
    }
    case Decl::BuiltinTemplate:
    case Decl::ClassScopeFunctionSpecialization:
    case Decl::PragmaComment:
    case Decl::PragmaDetectMismatch:
    case Decl::Empty:
    case Decl::AccessSpec:
    case Decl::Using:
    case Decl::UsingDirective:
    case Decl::UsingShadow:
    case Decl::ConstructorUsingShadow:
    case Decl::UnresolvedUsingValue:
    case Decl::IndirectField:
    case Decl::StaticAssert:
    case Decl::NamespaceAlias:
        break;
    default:
    {
        Debug("Unhandled declaration kind: %s\n", D->getDeclKindName());

        auto& SM = c->getSourceManager();
        auto Loc = D->getLocation();
        auto FileName = SM.getFilename(Loc);
        auto Offset = SM.getFileOffset(Loc);
        auto LineNo = SM.getLineNumber(SM.getFileID(Loc), Offset);
        Debug("  %s (line %u)\n", FileName.str().c_str(), LineNo);

        break;
    }
    };

    if (Decl && D->hasAttrs())
    {
        for (auto it = D->attr_begin(); it != D->attr_end(); ++it)
        {
            Attr* Attr = (*it);
            switch (Attr->getKind())
            {
            case clang::attr::Kind::MaxFieldAlignment:
            {
                auto MFA = cast<clang::MaxFieldAlignmentAttr>(Attr);
                Decl->maxFieldAlignment = MFA->getAlignment() / 8; // bits to bytes.
                break;
            }
            case clang::attr::Kind::Deprecated:
            {
                auto DA = cast<clang::DeprecatedAttr>(Attr);
                Decl->isDeprecated = true;
                break;
            }
            case clang::attr::Kind::Aligned:
                Decl->alignAs = GetAlignAs(cast<clang::AlignedAttr>(Attr));
                break;
            default:
                break;
            }
        }
    }

    return Decl;
}

int Parser::GetAlignAs(const clang::AlignedAttr* alignedAttr)
{
    return alignedAttr->isAlignas() &&
        !alignedAttr->isAlignmentErrorDependent() &&
        !alignedAttr->isAlignmentDependent()
        ? alignedAttr->getAlignment(c->getASTContext())
        : 0;
}

void Parser::HandleDiagnostics(ParserResult* res)
{
    auto DiagClient = (DiagnosticConsumer&)c->getDiagnosticClient();
    auto& Diags = DiagClient.Diagnostics;

    // Convert the diagnostics to the managed types
    for (unsigned I = 0, E = Diags.size(); I != E; ++I)
    {
        auto& Diag = DiagClient.Diagnostics[I];
        auto& Source = c->getSourceManager();
        auto FileName = Source.getFilename(Source.getFileLoc(Diag.Location));

        auto PDiag = ParserDiagnostic();
        PDiag.fileName = FileName.str();
        PDiag.message = Diag.Message.str().str();
        PDiag.lineNumber = 0;
        PDiag.columnNumber = 0;

        if (!Diag.Location.isInvalid())
        {
            clang::PresumedLoc PLoc = Source.getPresumedLoc(Diag.Location);
            if (PLoc.isValid())
            {
                PDiag.lineNumber = PLoc.getLine();
                PDiag.columnNumber = PLoc.getColumn();
            }
        }

        switch (Diag.Level)
        {
        case clang::DiagnosticsEngine::Ignored:
            PDiag.level = ParserDiagnosticLevel::Ignored;
            break;
        case clang::DiagnosticsEngine::Note:
            PDiag.level = ParserDiagnosticLevel::Note;
            break;
        case clang::DiagnosticsEngine::Warning:
            PDiag.level = ParserDiagnosticLevel::Warning;
            break;
        case clang::DiagnosticsEngine::Error:
            PDiag.level = ParserDiagnosticLevel::Error;
            break;
        case clang::DiagnosticsEngine::Fatal:
            PDiag.level = ParserDiagnosticLevel::Fatal;
            break;
        default:
            assert(0);
        }

        res->Diagnostics.push_back(PDiag);
    }
}

void Parser::SetupLLVMCodegen()
{
    // Initialize enough Clang codegen machinery so we can get at ABI details.
    LLVMModule.reset(new llvm::Module("", LLVMCtx));

    LLVMModule->setTargetTriple(c->getTarget().getTriple().getTriple());
    LLVMModule->setDataLayout(c->getTarget().getDataLayout());

    CGM.reset(new clang::CodeGen::CodeGenModule(c->getASTContext(),
        c->getHeaderSearchOpts(), c->getPreprocessorOpts(),
        c->getCodeGenOpts(), *LLVMModule, c->getDiagnostics()));

    codeGenTypes.reset(new clang::CodeGen::CodeGenTypes(*CGM.get()));
}

bool Parser::SetupSourceFiles(const std::vector<std::string>& SourceFiles,
    std::vector<const clang::FileEntry*>& FileEntries)
{
    // Check that the file is reachable.
    const clang::DirectoryLookup* Dir;
    llvm::SmallVector<
        std::pair<const clang::FileEntry*, const clang::DirectoryEntry*>,
        0> Includers;

    for (const auto& SourceFile : SourceFiles)
    {
        auto FileEntry = c->getPreprocessor().getHeaderSearchInfo().LookupFile(SourceFile,
            clang::SourceLocation(), /*isAngled*/true,
            nullptr, Dir, Includers, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr);

        if (!FileEntry)
            return false;

        FileEntries.push_back(&FileEntry.getPointer()->getFileEntry());
    }

    // Create a virtual file that includes the header. This gets rid of some
    // Clang warnings about parsing an header file as the main file.

    std::string source;
    for (const auto& SourceFile : SourceFiles)
    {
        source += "#include \"" + SourceFile + "\"" + "\n";
    }
    source += "\0";

    auto buffer = llvm::MemoryBuffer::getMemBufferCopy(source);
    auto& SM = c->getSourceManager();
    SM.setMainFileID(SM.createFileID(std::move(buffer)));

    return true;
}

class SemaConsumer : public clang::SemaConsumer {
    CppSharp::CppParser::Parser& Parser;
    std::vector<const clang::FileEntry*>& FileEntries;
public:
    SemaConsumer(CppSharp::CppParser::Parser& parser,
        std::vector<const clang::FileEntry*>& entries)
        : Parser(parser), FileEntries(entries) {}
    virtual void HandleTranslationUnit(clang::ASTContext& Ctx) override;
};

void SemaConsumer::HandleTranslationUnit(clang::ASTContext& Ctx)
{
    auto FileEntry = FileEntries[0];
    auto FileName = FileEntry->getName();
    auto Unit = Parser.opts->ASTContext->FindOrCreateModule(FileName.str());

    auto TU = Ctx.getTranslationUnitDecl();
    Parser.HandleDeclaration(TU, Unit);

    if (Unit->originalPtr == nullptr)
        Unit->originalPtr = (void*)FileEntry;

    Parser.WalkAST(TU);
}

ParserResult* Parser::Parse(const std::vector<std::string>& SourceFiles)
{
    assert(opts->ASTContext && "Expected a valid ASTContext");

    auto res = new ParserResult();

    if (SourceFiles.empty())
    {
        res->kind = ParserResultKind::FileNotFound;
        return res;
    }

    Setup();
    SetupLLVMCodegen();

    std::vector<const clang::FileEntry*> FileEntries;
    if (!SetupSourceFiles(SourceFiles, FileEntries))
    {
        res->kind = ParserResultKind::FileNotFound;
        return res;
    }

    std::unique_ptr<SemaConsumer> SC(new SemaConsumer(*this, FileEntries));
    c->setASTConsumer(std::move(SC));

    c->createSema(clang::TU_Complete, 0);

    std::unique_ptr<::DiagnosticConsumer> DiagClient(new ::DiagnosticConsumer());
    c->getDiagnostics().setClient(DiagClient.get(), false);

    DiagClient->BeginSourceFile(c->getLangOpts(), &c->getPreprocessor());

    ParseAST(c->getSema());

    DiagClient->EndSourceFile();

    HandleDiagnostics(res);

    if (DiagClient->getNumErrors() != 0)
    {
        res->kind = ParserResultKind::Error;
        return res;
    }

    res->targetInfo = GetTargetInfo();

    res->kind = ParserResultKind::Success;
    return res;
}

ParserResultKind Parser::ParseArchive(const std::string& File,
    llvm::object::Archive* Archive,
    std::vector<CppSharp::CppParser::NativeLibrary*>& NativeLibs)
{
    auto NativeLib = new NativeLibrary();
    NativeLib->fileName = File;

    for (const auto& Symbol : Archive->symbols())
    {
        llvm::StringRef SymRef = Symbol.getName();
        NativeLib->Symbols.push_back(SymRef.str());
    }
    NativeLibs.push_back(NativeLib);

    return ParserResultKind::Success;
}

static ArchType ConvertArchType(unsigned int archType)
{
    switch (archType)
    {
    case llvm::Triple::ArchType::x86:
        return ArchType::x86;
    case llvm::Triple::ArchType::x86_64:
        return ArchType::x86_64;
    }
    return ArchType::UnknownArch;
}

template<class ELFT>
static void ReadELFDependencies(const llvm::object::ELFFile<ELFT>* ELFFile, CppSharp::CppParser::NativeLibrary*& NativeLib)
{
    ELFDumper<ELFT> ELFDumper(ELFFile);
    for (const auto& Dependency : ELFDumper.getNeededLibraries())
        NativeLib->Dependencies.push_back(Dependency.str());
}

ParserResultKind Parser::ParseSharedLib(const std::string& File,
    llvm::object::ObjectFile* ObjectFile,
    std::vector<CppSharp::CppParser::NativeLibrary*>& NativeLibs)
{
    auto NativeLib = new NativeLibrary();
    NativeLib->fileName = File;
    NativeLib->archType = ConvertArchType(ObjectFile->getArch());
    NativeLibs.push_back(NativeLib);

    if (ObjectFile->isELF())
    {
        auto IDyn = llvm::cast<llvm::object::ELFObjectFileBase>(ObjectFile)->getDynamicSymbolIterators();
        for (auto it = IDyn.begin(); it != IDyn.end(); ++it)
        {
            std::string Sym;
            llvm::raw_string_ostream SymStream(Sym);

            if (it->printName(SymStream))
                continue;

            SymStream.flush();
            if (!Sym.empty())
                NativeLib->Symbols.push_back(Sym);
        }
        if (auto ELFObjectFile = llvm::dyn_cast<llvm::object::ELF32LEObjectFile>(ObjectFile))
        {
            ReadELFDependencies(ELFObjectFile->getELFFile(), NativeLib);
        }
        else if (auto ELFObjectFile = llvm::dyn_cast<llvm::object::ELF32BEObjectFile>(ObjectFile))
        {
            ReadELFDependencies(ELFObjectFile->getELFFile(), NativeLib);
        }
        else if (auto ELFObjectFile = llvm::dyn_cast<llvm::object::ELF64LEObjectFile>(ObjectFile))
        {
            ReadELFDependencies(ELFObjectFile->getELFFile(), NativeLib);
        }
        else if (auto ELFObjectFile = llvm::dyn_cast<llvm::object::ELF64BEObjectFile>(ObjectFile))
        {
            ReadELFDependencies(ELFObjectFile->getELFFile(), NativeLib);
        }
        return ParserResultKind::Success;
    }

    if (ObjectFile->isCOFF())
    {
        auto COFFObjectFile = static_cast<llvm::object::COFFObjectFile*>(ObjectFile);
        for (auto ExportedSymbol : COFFObjectFile->export_directories())
        {
            llvm::StringRef Symbol;
            if (!ExportedSymbol.getSymbolName(Symbol))
                NativeLib->Symbols.push_back(Symbol.str());
        }
        for (auto ImportedSymbol : COFFObjectFile->import_directories())
        {
            llvm::StringRef Name;
            if (!ImportedSymbol.getName(Name) && (Name.endswith(".dll") || Name.endswith(".DLL")))
                NativeLib->Dependencies.push_back(Name.str());
        }

        return ParserResultKind::Success;
    }

    if (ObjectFile->isMachO())
    {
        auto MachOObjectFile = static_cast<llvm::object::MachOObjectFile*>(ObjectFile);
        for (const auto& Load : MachOObjectFile->load_commands())
        {
            if (Load.C.cmd == llvm::MachO::LC_ID_DYLIB ||
                Load.C.cmd == llvm::MachO::LC_LOAD_DYLIB ||
                Load.C.cmd == llvm::MachO::LC_LOAD_WEAK_DYLIB ||
                Load.C.cmd == llvm::MachO::LC_REEXPORT_DYLIB ||
                Load.C.cmd == llvm::MachO::LC_LAZY_LOAD_DYLIB ||
                Load.C.cmd == llvm::MachO::LC_LOAD_UPWARD_DYLIB)
            {
                auto dl = MachOObjectFile->getDylibIDLoadCommand(Load);
                auto lib = llvm::sys::path::filename(Load.Ptr + dl.dylib.name);
                NativeLib->Dependencies.push_back(lib.str());
            }
        }
        // HACK: the correct way is with exported(Err) but it crashes with msvc 32
        // see https://bugs.llvm.org/show_bug.cgi?id=44433
        for (const auto& Symbol : MachOObjectFile->symbols())
        {
            if (Symbol.getName().takeError() || Symbol.getFlags().takeError())
                return ParserResultKind::Error;

            if ((Symbol.getFlags().get() & llvm::object::BasicSymbolRef::Flags::SF_Exported) &&
                !(Symbol.getFlags().get() & llvm::object::BasicSymbolRef::Flags::SF_Undefined))
                NativeLib->Symbols.push_back(Symbol.getName().get().str());
        }
        return ParserResultKind::Success;
    }

    return ParserResultKind::Error;
}

ParserResultKind Parser::ReadSymbols(llvm::StringRef File,
    llvm::object::basic_symbol_iterator Begin,
    llvm::object::basic_symbol_iterator End,
    CppSharp::CppParser::NativeLibrary*& NativeLib)
{
    auto LibName = File;
    NativeLib = new NativeLibrary();
    NativeLib->fileName = LibName.str();

    for (auto it = Begin; it != End; ++it)
    {
        std::string Sym;
        llvm::raw_string_ostream SymStream(Sym);

        if (it->printName(SymStream))
            continue;

        SymStream.flush();
        if (!Sym.empty())
            NativeLib->Symbols.push_back(Sym);
    }

    return ParserResultKind::Success;
}

ParserResult* Parser::ParseLibrary(const LinkerOptions* Opts)
{
    auto res = new ParserResult();

    for (const auto& Lib : Opts->Libraries)
    {
        if (Lib.empty())
        {
            res->kind = ParserResultKind::FileNotFound;
            return res;
        }

        std::string PrefixedLib = "lib" + Lib;
        std::string FileName;
        std::string FileEntry;

        using namespace llvm::sys;
        for (const auto& LibDir : Opts->LibraryDirs)
        {
            std::error_code ErrorCode;
            fs::directory_iterator Dir(LibDir, ErrorCode);
            for (const auto& File = Dir;
                Dir != fs::directory_iterator() && !ErrorCode;
                Dir = Dir.increment(ErrorCode))
            {
                FileName = path::filename(File->path()).str();
                if (FileName == Lib ||
                    FileName == PrefixedLib ||
                    path::stem(FileName) == Lib ||
                    path::stem(FileName) == PrefixedLib ||
                    path::stem(path::stem(FileName)) == Lib ||
                    path::stem(path::stem(FileName)) == PrefixedLib)
                {
                    FileEntry = File->path();
                    goto found;
                }
            }
        }

        if (FileEntry.empty())
        {
            res->kind = ParserResultKind::FileNotFound;
            return res;
        }

    found:
        auto BinaryOrErr = llvm::object::createBinary(FileEntry);
        if (!BinaryOrErr)
        {
            auto Error = BinaryOrErr.takeError();
            res->kind = ParserResultKind::Error;
            return res;
        }

        auto OwningBinary = std::move(BinaryOrErr.get());
        auto Bin = OwningBinary.getBinary();
        if (auto Archive = llvm::dyn_cast<llvm::object::Archive>(Bin)) {
            res->kind = ParseArchive(FileName, Archive, res->Libraries);
            if (res->kind == ParserResultKind::Error)
                return res;
        }

        if (auto ObjectFile = llvm::dyn_cast<llvm::object::ObjectFile>(Bin))
        {
            res->kind = ParseSharedLib(FileName, ObjectFile, res->Libraries);
            if (res->kind == ParserResultKind::Error)
                return res;
        }
    }

    res->kind = ParserResultKind::Success;
    return res;
}

ParserResult* ClangParser::ParseHeader(CppParserOptions* Opts)
{
    if (!Opts)
        return nullptr;

    auto& Headers = Opts->SourceFiles;
    if (Opts->unityBuild)
    {
        Parser parser(Opts);
        return parser.Parse(Headers);
    }

    ParserResult* res = 0;
    std::vector<Parser*> parsers;
    for (size_t i = 0; i < Headers.size(); i++)
    {
        auto parser = new Parser(Opts);
        parsers.push_back(parser);
        std::vector<std::string> Header(&Headers[i], &Headers[i + 1]);
        if (i < Headers.size() - 1)
            delete parser->Parse(Header);
        else
            res = parser->Parse(Header);
    }

    for (auto parser : parsers)
        delete parser;

    return res;
}

ParserResult* ClangParser::ParseLibrary(LinkerOptions* Opts)
{
    if (!Opts)
        return nullptr;

    return Parser::ParseLibrary(Opts);
}

ParserTargetInfo* Parser::GetTargetInfo()
{
    auto parserTargetInfo = new ParserTargetInfo();

    auto& TI = c->getTarget();
    parserTargetInfo->ABI = TI.getABI().str();

    parserTargetInfo->char16Type = ConvertIntType(TI.getChar16Type());
    parserTargetInfo->char32Type = ConvertIntType(TI.getChar32Type());
    parserTargetInfo->int64Type = ConvertIntType(TI.getInt64Type());
    parserTargetInfo->intMaxType = ConvertIntType(TI.getIntMaxType());
    parserTargetInfo->intPtrType = ConvertIntType(TI.getIntPtrType());
    parserTargetInfo->sizeType = ConvertIntType(TI.getSizeType());
    parserTargetInfo->uIntMaxType = ConvertIntType(TI.getUIntMaxType());
    parserTargetInfo->wCharType = ConvertIntType(TI.getWCharType());
    parserTargetInfo->wIntType = ConvertIntType(TI.getWIntType());

    parserTargetInfo->boolAlign = TI.getBoolAlign();
    parserTargetInfo->boolWidth = TI.getBoolWidth();
    parserTargetInfo->charAlign = TI.getCharAlign();
    parserTargetInfo->charWidth = TI.getCharWidth();
    parserTargetInfo->char16Align = TI.getChar16Align();
    parserTargetInfo->char16Width = TI.getChar16Width();
    parserTargetInfo->char32Align = TI.getChar32Align();
    parserTargetInfo->char32Width = TI.getChar32Width();
    parserTargetInfo->halfAlign = TI.getHalfAlign();
    parserTargetInfo->halfWidth = TI.getHalfWidth();
    parserTargetInfo->floatAlign = TI.getFloatAlign();
    parserTargetInfo->floatWidth = TI.getFloatWidth();
    parserTargetInfo->doubleAlign = TI.getDoubleAlign();
    parserTargetInfo->doubleWidth = TI.getDoubleWidth();
    parserTargetInfo->shortAlign = TI.getShortAlign();
    parserTargetInfo->shortWidth = TI.getShortWidth();
    parserTargetInfo->intAlign = TI.getIntAlign();
    parserTargetInfo->intWidth = TI.getIntWidth();
    parserTargetInfo->intMaxTWidth = TI.getIntMaxTWidth();
    parserTargetInfo->longAlign = TI.getLongAlign();
    parserTargetInfo->longWidth = TI.getLongWidth();
    parserTargetInfo->longDoubleAlign = TI.getLongDoubleAlign();
    parserTargetInfo->longDoubleWidth = TI.getLongDoubleWidth();
    parserTargetInfo->longLongAlign = TI.getLongLongAlign();
    parserTargetInfo->longLongWidth = TI.getLongLongWidth();
    parserTargetInfo->pointerAlign = TI.getPointerAlign(0);
    parserTargetInfo->pointerWidth = TI.getPointerWidth(0);
    parserTargetInfo->wCharAlign = TI.getWCharAlign();
    parserTargetInfo->wCharWidth = TI.getWCharWidth();
    parserTargetInfo->float128Align = TI.getFloat128Align();
    parserTargetInfo->float128Width = TI.getFloat128Width();

    return parserTargetInfo;
}

Declaration* Parser::GetDeclarationFromFriend(clang::NamedDecl* FriendDecl)
{
    Declaration* Decl = WalkDeclarationDef(FriendDecl);
    if (!Decl) return nullptr;

    int MinLineNumberStart = std::numeric_limits<int>::max();
    int MinLineNumberEnd = std::numeric_limits<int>::max();
    auto& SM = c->getSourceManager();
    for (auto it = FriendDecl->redecls_begin(); it != FriendDecl->redecls_end(); it++)
    {
        if (it->getLocation() != FriendDecl->getLocation())
        {
            auto DecomposedLocStart = SM.getDecomposedLoc(it->getLocation());
            int NewLineNumberStart = SM.getLineNumber(DecomposedLocStart.first, DecomposedLocStart.second);
            auto DecomposedLocEnd = SM.getDecomposedLoc(it->getEndLoc());
            int NewLineNumberEnd = SM.getLineNumber(DecomposedLocEnd.first, DecomposedLocEnd.second);
            if (NewLineNumberStart < MinLineNumberStart)
            {
                MinLineNumberStart = NewLineNumberStart;
                MinLineNumberEnd = NewLineNumberEnd;
            }
        }
    }
    if (MinLineNumberStart < std::numeric_limits<int>::max())
    {
        Decl->lineNumberStart = MinLineNumberStart;
        Decl->lineNumberEnd = MinLineNumberEnd;
    }
    return Decl;
}










// ----------------------------------------------------------------------------
// <auto-generated>
// This is autogenerated code by CppSharp.
// Do not edit this file or all your changes will be lost after re-generation.
// </auto-generated>
// ----------------------------------------------------------------------------


#include "AST.h"
#include "Parser.h"
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtCXX.h>

namespace CppSharp {
    namespace CppParser {

        AST::Stmt* Parser::WalkStatement(const clang::Stmt* Stmt)
        {
            if (Stmt == nullptr)
                return nullptr;

            AST::Stmt* _Stmt = 0;

            switch (Stmt->getStmtClass())
            {
            case clang::Stmt::DeclStmtClass:
            {
                auto S = const_cast<clang::DeclStmt*>(llvm::cast<clang::DeclStmt>(Stmt));
                auto _S = new AST::DeclStmt();
                _S->isSingleDecl = S->isSingleDecl();
                if (S->isSingleDecl())
                    _S->singleDecl = static_cast<AST::Declaration*>(WalkDeclaration(S->getSingleDecl()));
                for (auto _E : S->decls())
                {
                    auto _ES = WalkDeclaration(_E);
                    _S->adddecls(_ES);
                }
                _Stmt = _S;
                break;
            }
            case clang::Stmt::NullStmtClass:
            {
                auto S = const_cast<clang::NullStmt*>(llvm::cast<clang::NullStmt>(Stmt));
                auto _S = new AST::NullStmt();
                _S->hasLeadingEmptyMacro = S->hasLeadingEmptyMacro();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::CompoundStmtClass:
            {
                auto S = const_cast<clang::CompoundStmt*>(llvm::cast<clang::CompoundStmt>(Stmt));
                auto _S = new AST::CompoundStmt();
                _S->body_empty = S->body_empty();
                _S->size = S->size();
                _S->body_front = static_cast<AST::Stmt*>(WalkStatement(S->body_front()));
                _S->body_back = static_cast<AST::Stmt*>(WalkStatement(S->body_back()));
                for (auto _E : S->body())
                {
                    auto _ES = WalkStatement(_E);
                    _S->addbody(_ES);
                }
                _Stmt = _S;
                break;
            }
            case clang::Stmt::CaseStmtClass:
            {
                auto S = const_cast<clang::CaseStmt*>(llvm::cast<clang::CaseStmt>(Stmt));
                auto _S = new AST::CaseStmt();
                _S->subStmt = static_cast<AST::Stmt*>(WalkStatement(S->getSubStmt()));
                _S->lHS = static_cast<AST::Expr*>(WalkExpression(S->getLHS()));
                _S->rHS = static_cast<AST::Expr*>(WalkExpression(S->getRHS()));
                _S->subStmt = static_cast<AST::Stmt*>(WalkStatement(S->getSubStmt()));
                _S->caseStmtIsGNURange = S->caseStmtIsGNURange();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::DefaultStmtClass:
            {
                auto S = const_cast<clang::DefaultStmt*>(llvm::cast<clang::DefaultStmt>(Stmt));
                auto _S = new AST::DefaultStmt();
                _S->subStmt = static_cast<AST::Stmt*>(WalkStatement(S->getSubStmt()));
                _S->subStmt = static_cast<AST::Stmt*>(WalkStatement(S->getSubStmt()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::LabelStmtClass:
            {
                auto S = const_cast<clang::LabelStmt*>(llvm::cast<clang::LabelStmt>(Stmt));
                auto _S = new AST::LabelStmt();
                _S->subStmt = static_cast<AST::Stmt*>(WalkStatement(S->getSubStmt()));
                _S->name = S->getName();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::AttributedStmtClass:
            {
                auto S = const_cast<clang::AttributedStmt*>(llvm::cast<clang::AttributedStmt>(Stmt));
                auto _S = new AST::AttributedStmt();
                _S->subStmt = static_cast<AST::Stmt*>(WalkStatement(S->getSubStmt()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::IfStmtClass:
            {
                auto S = const_cast<clang::IfStmt*>(llvm::cast<clang::IfStmt>(Stmt));
                auto _S = new AST::IfStmt();
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->then = static_cast<AST::Stmt*>(WalkStatement(S->getThen()));
                _S->_else = static_cast<AST::Stmt*>(WalkStatement(S->getElse()));
                _S->init = static_cast<AST::Stmt*>(WalkStatement(S->getInit()));
                _S->_constexpr = S->isConstexpr();
                _S->hasInitStorage = S->hasInitStorage();
                _S->hasVarStorage = S->hasVarStorage();
                _S->hasElseStorage = S->hasElseStorage();
                _S->conditionVariableDeclStmt = static_cast<AST::DeclStmt*>(WalkStatement(S->getConditionVariableDeclStmt()));
                _S->isObjCAvailabilityCheck = S->isObjCAvailabilityCheck();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::SwitchStmtClass:
            {
                auto S = const_cast<clang::SwitchStmt*>(llvm::cast<clang::SwitchStmt>(Stmt));
                auto _S = new AST::SwitchStmt();
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->body = static_cast<AST::Stmt*>(WalkStatement(S->getBody()));
                _S->init = static_cast<AST::Stmt*>(WalkStatement(S->getInit()));
                _S->hasInitStorage = S->hasInitStorage();
                _S->hasVarStorage = S->hasVarStorage();
                _S->conditionVariableDeclStmt = static_cast<AST::DeclStmt*>(WalkStatement(S->getConditionVariableDeclStmt()));
                _S->isAllEnumCasesCovered = S->isAllEnumCasesCovered();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::WhileStmtClass:
            {
                auto S = const_cast<clang::WhileStmt*>(llvm::cast<clang::WhileStmt>(Stmt));
                auto _S = new AST::WhileStmt();
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->body = static_cast<AST::Stmt*>(WalkStatement(S->getBody()));
                _S->hasVarStorage = S->hasVarStorage();
                _S->conditionVariableDeclStmt = static_cast<AST::DeclStmt*>(WalkStatement(S->getConditionVariableDeclStmt()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::DoStmtClass:
            {
                auto S = const_cast<clang::DoStmt*>(llvm::cast<clang::DoStmt>(Stmt));
                auto _S = new AST::DoStmt();
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->body = static_cast<AST::Stmt*>(WalkStatement(S->getBody()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::ForStmtClass:
            {
                auto S = const_cast<clang::ForStmt*>(llvm::cast<clang::ForStmt>(Stmt));
                auto _S = new AST::ForStmt();
                _S->init = static_cast<AST::Stmt*>(WalkStatement(S->getInit()));
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->inc = static_cast<AST::Expr*>(WalkExpression(S->getInc()));
                _S->body = static_cast<AST::Stmt*>(WalkStatement(S->getBody()));
                _S->conditionVariableDeclStmt = static_cast<AST::DeclStmt*>(WalkStatement(S->getConditionVariableDeclStmt()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::GotoStmtClass:
            {
                auto S = const_cast<clang::GotoStmt*>(llvm::cast<clang::GotoStmt>(Stmt));
                auto _S = new AST::GotoStmt();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::IndirectGotoStmtClass:
            {
                auto S = const_cast<clang::IndirectGotoStmt*>(llvm::cast<clang::IndirectGotoStmt>(Stmt));
                auto _S = new AST::IndirectGotoStmt();
                _S->target = static_cast<AST::Expr*>(WalkExpression(S->getTarget()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::ContinueStmtClass:
            {
                auto S = const_cast<clang::ContinueStmt*>(llvm::cast<clang::ContinueStmt>(Stmt));
                auto _S = new AST::ContinueStmt();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::BreakStmtClass:
            {
                auto S = const_cast<clang::BreakStmt*>(llvm::cast<clang::BreakStmt>(Stmt));
                auto _S = new AST::BreakStmt();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::ReturnStmtClass:
            {
                auto S = const_cast<clang::ReturnStmt*>(llvm::cast<clang::ReturnStmt>(Stmt));
                auto _S = new AST::ReturnStmt();
                _S->retValue = static_cast<AST::Expr*>(WalkExpression(S->getRetValue()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::GCCAsmStmtClass:
            {
                auto S = const_cast<clang::GCCAsmStmt*>(llvm::cast<clang::GCCAsmStmt>(Stmt));
                auto _S = new AST::GCCAsmStmt();
                _S->simple = S->isSimple();
                _S->_volatile = S->isVolatile();
                _S->numOutputs = S->getNumOutputs();
                _S->numPlusOperands = S->getNumPlusOperands();
                _S->numInputs = S->getNumInputs();
                _S->numClobbers = S->getNumClobbers();
                for (auto _E : S->inputs())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addinputs(_ES);
                }
                for (auto _E : S->outputs())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addoutputs(_ES);
                }
                _Stmt = _S;
                break;
            }
            case clang::Stmt::MSAsmStmtClass:
            {
                auto S = const_cast<clang::MSAsmStmt*>(llvm::cast<clang::MSAsmStmt>(Stmt));
                auto _S = new AST::MSAsmStmt();
                _S->simple = S->isSimple();
                _S->_volatile = S->isVolatile();
                _S->numOutputs = S->getNumOutputs();
                _S->numPlusOperands = S->getNumPlusOperands();
                _S->numInputs = S->getNumInputs();
                _S->numClobbers = S->getNumClobbers();
                for (auto _E : S->inputs())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addinputs(_ES);
                }
                for (auto _E : S->outputs())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addoutputs(_ES);
                }
                _S->hasBraces = S->hasBraces();
                _S->numAsmToks = S->getNumAsmToks();
                _S->asmString = S->getAsmString().str();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::SEHExceptStmtClass:
            {
                auto S = const_cast<clang::SEHExceptStmt*>(llvm::cast<clang::SEHExceptStmt>(Stmt));
                auto _S = new AST::SEHExceptStmt();
                _S->filterExpr = static_cast<AST::Expr*>(WalkExpression(S->getFilterExpr()));
                _S->block = static_cast<AST::CompoundStmt*>(WalkStatement(S->getBlock()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::SEHFinallyStmtClass:
            {
                auto S = const_cast<clang::SEHFinallyStmt*>(llvm::cast<clang::SEHFinallyStmt>(Stmt));
                auto _S = new AST::SEHFinallyStmt();
                _S->block = static_cast<AST::CompoundStmt*>(WalkStatement(S->getBlock()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::SEHTryStmtClass:
            {
                auto S = const_cast<clang::SEHTryStmt*>(llvm::cast<clang::SEHTryStmt>(Stmt));
                auto _S = new AST::SEHTryStmt();
                _S->isCXXTry = S->getIsCXXTry();
                _S->tryBlock = static_cast<AST::CompoundStmt*>(WalkStatement(S->getTryBlock()));
                _S->handler = static_cast<AST::Stmt*>(WalkStatement(S->getHandler()));
                _S->exceptHandler = static_cast<AST::SEHExceptStmt*>(WalkStatement(S->getExceptHandler()));
                _S->finallyHandler = static_cast<AST::SEHFinallyStmt*>(WalkStatement(S->getFinallyHandler()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::SEHLeaveStmtClass:
            {
                auto S = const_cast<clang::SEHLeaveStmt*>(llvm::cast<clang::SEHLeaveStmt>(Stmt));
                auto _S = new AST::SEHLeaveStmt();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::CapturedStmtClass:
            {
                auto S = const_cast<clang::CapturedStmt*>(llvm::cast<clang::CapturedStmt>(Stmt));
                auto _S = new AST::CapturedStmt();
                _S->capturedStmt = static_cast<AST::Stmt*>(WalkStatement(S->getCapturedStmt()));
                _S->capture_size = S->capture_size();
                for (auto _E : S->capture_inits())
                {
                    auto _ES = WalkExpression(_E);
                    _S->addcapture_inits(_ES);
                }
                _Stmt = _S;
                break;
            }
            case clang::Stmt::CXXCatchStmtClass:
            {
                auto S = const_cast<clang::CXXCatchStmt*>(llvm::cast<clang::CXXCatchStmt>(Stmt));
                auto _S = new AST::CXXCatchStmt();
                _S->caughtType = GetQualifiedType(S->getCaughtType());
                _S->handlerBlock = static_cast<AST::Stmt*>(WalkStatement(S->getHandlerBlock()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::CXXTryStmtClass:
            {
                auto S = const_cast<clang::CXXTryStmt*>(llvm::cast<clang::CXXTryStmt>(Stmt));
                auto _S = new AST::CXXTryStmt();
                _S->tryBlock = static_cast<AST::CompoundStmt*>(WalkStatement(S->getTryBlock()));
                _S->numHandlers = S->getNumHandlers();
                _Stmt = _S;
                break;
            }
            case clang::Stmt::CXXForRangeStmtClass:
            {
                auto S = const_cast<clang::CXXForRangeStmt*>(llvm::cast<clang::CXXForRangeStmt>(Stmt));
                auto _S = new AST::CXXForRangeStmt();
                _S->init = static_cast<AST::Stmt*>(WalkStatement(S->getInit()));
                _S->rangeInit = static_cast<AST::Expr*>(WalkExpression(S->getRangeInit()));
                _S->cond = static_cast<AST::Expr*>(WalkExpression(S->getCond()));
                _S->inc = static_cast<AST::Expr*>(WalkExpression(S->getInc()));
                _S->body = static_cast<AST::Stmt*>(WalkStatement(S->getBody()));
                _S->rangeStmt = static_cast<AST::DeclStmt*>(WalkStatement(S->getRangeStmt()));
                _S->beginStmt = static_cast<AST::DeclStmt*>(WalkStatement(S->getBeginStmt()));
                _S->endStmt = static_cast<AST::DeclStmt*>(WalkStatement(S->getEndStmt()));
                _S->loopVarStmt = static_cast<AST::DeclStmt*>(WalkStatement(S->getLoopVarStmt()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::MSDependentExistsStmtClass:
            {
                auto S = const_cast<clang::MSDependentExistsStmt*>(llvm::cast<clang::MSDependentExistsStmt>(Stmt));
                auto _S = new AST::MSDependentExistsStmt();
                _S->isIfExists = S->isIfExists();
                _S->isIfNotExists = S->isIfNotExists();
                _S->subStmt = static_cast<AST::CompoundStmt*>(WalkStatement(S->getSubStmt()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::CoroutineBodyStmtClass:
            {
                auto S = const_cast<clang::CoroutineBodyStmt*>(llvm::cast<clang::CoroutineBodyStmt>(Stmt));
                auto _S = new AST::CoroutineBodyStmt();
                _S->hasDependentPromiseType = S->hasDependentPromiseType();
                _S->body = static_cast<AST::Stmt*>(WalkStatement(S->getBody()));
                _S->promiseDeclStmt = static_cast<AST::Stmt*>(WalkStatement(S->getPromiseDeclStmt()));
                _S->initSuspendStmt = static_cast<AST::Stmt*>(WalkStatement(S->getInitSuspendStmt()));
                _S->finalSuspendStmt = static_cast<AST::Stmt*>(WalkStatement(S->getFinalSuspendStmt()));
                _S->exceptionHandler = static_cast<AST::Stmt*>(WalkStatement(S->getExceptionHandler()));
                _S->fallthroughHandler = static_cast<AST::Stmt*>(WalkStatement(S->getFallthroughHandler()));
                _S->allocate = static_cast<AST::Expr*>(WalkExpression(S->getAllocate()));
                _S->deallocate = static_cast<AST::Expr*>(WalkExpression(S->getDeallocate()));
                _S->returnValueInit = static_cast<AST::Expr*>(WalkExpression(S->getReturnValueInit()));
                _S->resultDecl = static_cast<AST::Stmt*>(WalkStatement(S->getResultDecl()));
                _S->returnStmt = static_cast<AST::Stmt*>(WalkStatement(S->getReturnStmt()));
                _S->returnStmtOnAllocFailure = static_cast<AST::Stmt*>(WalkStatement(S->getReturnStmtOnAllocFailure()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::CoreturnStmtClass:
            {
                auto S = const_cast<clang::CoreturnStmt*>(llvm::cast<clang::CoreturnStmt>(Stmt));
                auto _S = new AST::CoreturnStmt();
                _S->isImplicit = S->isImplicit();
                _S->operand = static_cast<AST::Expr*>(WalkExpression(S->getOperand()));
                _S->promiseCall = static_cast<AST::Expr*>(WalkExpression(S->getPromiseCall()));
                _Stmt = _S;
                break;
            }
            case clang::Stmt::ConstantExprClass:
            case clang::Stmt::OpaqueValueExprClass:
            case clang::Stmt::DeclRefExprClass:
            case clang::Stmt::IntegerLiteralClass:
            case clang::Stmt::FixedPointLiteralClass:
            case clang::Stmt::CharacterLiteralClass:
            case clang::Stmt::FloatingLiteralClass:
            case clang::Stmt::ImaginaryLiteralClass:
            case clang::Stmt::StringLiteralClass:
            case clang::Stmt::PredefinedExprClass:
            case clang::Stmt::ParenExprClass:
            case clang::Stmt::UnaryOperatorClass:
            case clang::Stmt::OffsetOfExprClass:
            case clang::Stmt::UnaryExprOrTypeTraitExprClass:
            case clang::Stmt::ArraySubscriptExprClass:
            case clang::Stmt::CallExprClass:
            case clang::Stmt::MemberExprClass:
            case clang::Stmt::CompoundLiteralExprClass:
            case clang::Stmt::ImplicitCastExprClass:
            case clang::Stmt::CStyleCastExprClass:
            case clang::Stmt::BinaryOperatorClass:
            case clang::Stmt::CompoundAssignOperatorClass:
            case clang::Stmt::ConditionalOperatorClass:
            case clang::Stmt::BinaryConditionalOperatorClass:
            case clang::Stmt::AddrLabelExprClass:
            case clang::Stmt::StmtExprClass:
            case clang::Stmt::ShuffleVectorExprClass:
            case clang::Stmt::ConvertVectorExprClass:
            case clang::Stmt::ChooseExprClass:
            case clang::Stmt::GNUNullExprClass:
            case clang::Stmt::VAArgExprClass:
            case clang::Stmt::InitListExprClass:
            case clang::Stmt::DesignatedInitExprClass:
            case clang::Stmt::NoInitExprClass:
            case clang::Stmt::DesignatedInitUpdateExprClass:
            case clang::Stmt::ArrayInitLoopExprClass:
            case clang::Stmt::ArrayInitIndexExprClass:
            case clang::Stmt::ImplicitValueInitExprClass:
            case clang::Stmt::ParenListExprClass:
            case clang::Stmt::GenericSelectionExprClass:
            case clang::Stmt::ExtVectorElementExprClass:
            case clang::Stmt::BlockExprClass:
            case clang::Stmt::AsTypeExprClass:
            case clang::Stmt::PseudoObjectExprClass:
            case clang::Stmt::AtomicExprClass:
            case clang::Stmt::TypoExprClass:
            case clang::Stmt::CXXOperatorCallExprClass:
            case clang::Stmt::CXXMemberCallExprClass:
            case clang::Stmt::CUDAKernelCallExprClass:
            case clang::Stmt::CXXStaticCastExprClass:
            case clang::Stmt::CXXDynamicCastExprClass:
            case clang::Stmt::CXXReinterpretCastExprClass:
            case clang::Stmt::CXXConstCastExprClass:
            case clang::Stmt::UserDefinedLiteralClass:
            case clang::Stmt::CXXBoolLiteralExprClass:
            case clang::Stmt::CXXNullPtrLiteralExprClass:
            case clang::Stmt::CXXStdInitializerListExprClass:
            case clang::Stmt::CXXTypeidExprClass:
            case clang::Stmt::MSPropertyRefExprClass:
            case clang::Stmt::MSPropertySubscriptExprClass:
            case clang::Stmt::CXXUuidofExprClass:
            case clang::Stmt::CXXThisExprClass:
            case clang::Stmt::CXXThrowExprClass:
            case clang::Stmt::CXXDefaultArgExprClass:
            case clang::Stmt::CXXDefaultInitExprClass:
            case clang::Stmt::CXXBindTemporaryExprClass:
            case clang::Stmt::CXXConstructExprClass:
            case clang::Stmt::CXXInheritedCtorInitExprClass:
            case clang::Stmt::CXXFunctionalCastExprClass:
            case clang::Stmt::CXXTemporaryObjectExprClass:
            case clang::Stmt::LambdaExprClass:
            case clang::Stmt::CXXScalarValueInitExprClass:
            case clang::Stmt::CXXNewExprClass:
            case clang::Stmt::CXXDeleteExprClass:
            case clang::Stmt::CXXPseudoDestructorExprClass:
            case clang::Stmt::TypeTraitExprClass:
            case clang::Stmt::ArrayTypeTraitExprClass:
            case clang::Stmt::ExpressionTraitExprClass:
            case clang::Stmt::UnresolvedLookupExprClass:
            case clang::Stmt::DependentScopeDeclRefExprClass:
            case clang::Stmt::ExprWithCleanupsClass:
            case clang::Stmt::CXXUnresolvedConstructExprClass:
            case clang::Stmt::CXXDependentScopeMemberExprClass:
            case clang::Stmt::UnresolvedMemberExprClass:
            case clang::Stmt::CXXNoexceptExprClass:
            case clang::Stmt::PackExpansionExprClass:
            case clang::Stmt::SizeOfPackExprClass:
            case clang::Stmt::SubstNonTypeTemplateParmExprClass:
            case clang::Stmt::SubstNonTypeTemplateParmPackExprClass:
            case clang::Stmt::FunctionParmPackExprClass:
            case clang::Stmt::MaterializeTemporaryExprClass:
            case clang::Stmt::CXXFoldExprClass:
            case clang::Stmt::CoawaitExprClass:
            case clang::Stmt::DependentCoawaitExprClass:
            case clang::Stmt::CoyieldExprClass:
            {
                return WalkExpression(llvm::cast<clang::Expr>(Stmt));
            }
            default:
                printf("Unhandled statement kind: %s\n", Stmt->getStmtClassName());
            }

            return _Stmt;
        }

    }
}






#include "Sources.h"

namespace CppSharp {
    namespace CppParser {

        SourceLocation::SourceLocation()
            : ID(0)
        {
        }

        SourceLocation::SourceLocation(unsigned ID)
            : ID(ID)
        {
        }

    }
}





// ----------------------------------------------------------------------------
// <auto-generated>
// This is autogenerated code by CppSharp.
// Do not edit this file or all your changes will be lost after re-generation.
// </auto-generated>
// ----------------------------------------------------------------------------


#include "Sources.h"
#include "Stmt.h"

namespace CppSharp {
    namespace CppParser {
        namespace AST {

            Stmt::Stmt()
                : stmtClass(StmtClass::NoStmt)
                , sourceRange(SourceRange())
                , beginLoc(SourceLocation())
                , endLoc(SourceLocation())
            {
            }

            Stmt::Stmt(StmtClass klass)
                : stmtClass(klass)
                , sourceRange(SourceRange())
                , beginLoc(SourceLocation())
                , endLoc(SourceLocation())
            {
            }

            DeclStmt::DeclStmt()
                : Stmt(StmtClass::DeclStmt)
                , isSingleDecl(0)
                , singleDecl(nullptr)
            {
            }

            DEF_VECTOR(DeclStmt, Declaration*, decls)

                NullStmt::NullStmt()
                : Stmt(StmtClass::NullStmt)
                , semiLoc(SourceLocation())
                , hasLeadingEmptyMacro(0)
            {
            }

            CompoundStmt::CompoundStmt()
                : Stmt(StmtClass::CompoundStmt)
                , body_empty(0)
                , size(0)
                , body_front(nullptr)
                , body_back(nullptr)
                , lBracLoc(SourceLocation())
                , rBracLoc(SourceLocation())
            {
            }

            DEF_VECTOR(CompoundStmt, Stmt*, body)

                SwitchCase::SwitchCase()
                : Stmt(StmtClass::NoStmt)
                , keywordLoc(SourceLocation())
                , colonLoc(SourceLocation())
                , subStmt(nullptr)
            {
            }

            SwitchCase::SwitchCase(StmtClass klass)
                : Stmt(klass)
                , keywordLoc(SourceLocation())
                , colonLoc(SourceLocation())
                , subStmt(nullptr)
            {
            }

            CaseStmt::CaseStmt()
                : SwitchCase(StmtClass::CaseStmt)
                , caseLoc(SourceLocation())
                , ellipsisLoc(SourceLocation())
                , lHS(nullptr)
                , rHS(nullptr)
                , caseStmtIsGNURange(0)
            {
            }

            DefaultStmt::DefaultStmt()
                : SwitchCase(StmtClass::DefaultStmt)
                , defaultLoc(SourceLocation())
            {
            }

            LabelStmt::LabelStmt()
                : Stmt(StmtClass::LabelStmt)
                , identLoc(SourceLocation())
                , subStmt(nullptr)
                , name(nullptr)
            {
            }

            AttributedStmt::AttributedStmt()
                : Stmt(StmtClass::AttributedStmt)
                , attrLoc(SourceLocation())
                , subStmt(nullptr)
            {
            }

            IfStmt::IfStmt()
                : Stmt(StmtClass::IfStmt)
                , cond(nullptr)
                , then(nullptr)
                , _else(nullptr)
                , init(nullptr)
                , ifLoc(SourceLocation())
                , elseLoc(SourceLocation())
                , _constexpr(0)
                , hasInitStorage(0)
                , hasVarStorage(0)
                , hasElseStorage(0)
                , conditionVariableDeclStmt(nullptr)
                , isObjCAvailabilityCheck(0)
            {
            }

            SwitchStmt::SwitchStmt()
                : Stmt(StmtClass::SwitchStmt)
                , cond(nullptr)
                , body(nullptr)
                , init(nullptr)
                , switchLoc(SourceLocation())
                , hasInitStorage(0)
                , hasVarStorage(0)
                , conditionVariableDeclStmt(nullptr)
                , isAllEnumCasesCovered(0)
            {
            }

            WhileStmt::WhileStmt()
                : Stmt(StmtClass::WhileStmt)
                , cond(nullptr)
                , body(nullptr)
                , whileLoc(SourceLocation())
                , hasVarStorage(0)
                , conditionVariableDeclStmt(nullptr)
            {
            }

            DoStmt::DoStmt()
                : Stmt(StmtClass::DoStmt)
                , cond(nullptr)
                , body(nullptr)
                , doLoc(SourceLocation())
                , whileLoc(SourceLocation())
                , rParenLoc(SourceLocation())
            {
            }

            ForStmt::ForStmt()
                : Stmt(StmtClass::ForStmt)
                , init(nullptr)
                , cond(nullptr)
                , inc(nullptr)
                , body(nullptr)
                , forLoc(SourceLocation())
                , lParenLoc(SourceLocation())
                , rParenLoc(SourceLocation())
                , conditionVariableDeclStmt(nullptr)
            {
            }

            GotoStmt::GotoStmt()
                : Stmt(StmtClass::GotoStmt)
                , gotoLoc(SourceLocation())
                , labelLoc(SourceLocation())
            {
            }

            IndirectGotoStmt::IndirectGotoStmt()
                : Stmt(StmtClass::IndirectGotoStmt)
                , gotoLoc(SourceLocation())
                , starLoc(SourceLocation())
                , target(nullptr)
            {
            }

            ContinueStmt::ContinueStmt()
                : Stmt(StmtClass::ContinueStmt)
                , continueLoc(SourceLocation())
            {
            }

            BreakStmt::BreakStmt()
                : Stmt(StmtClass::BreakStmt)
                , breakLoc(SourceLocation())
            {
            }

            ReturnStmt::ReturnStmt()
                : Stmt(StmtClass::ReturnStmt)
                , retValue(nullptr)
                , returnLoc(SourceLocation())
            {
            }

            AsmStmt::AsmStmt()
                : Stmt(StmtClass::NoStmt)
                , asmLoc(SourceLocation())
                , simple(0)
                , _volatile(0)
                , numOutputs(0)
                , numPlusOperands(0)
                , numInputs(0)
                , numClobbers(0)
            {
            }

            AsmStmt::AsmStmt(StmtClass klass)
                : Stmt(klass)
                , asmLoc(SourceLocation())
                , simple(0)
                , _volatile(0)
                , numOutputs(0)
                , numPlusOperands(0)
                , numInputs(0)
                , numClobbers(0)
            {
            }

            DEF_VECTOR(AsmStmt, Expr*, inputs)

                DEF_VECTOR(AsmStmt, Expr*, outputs)

                GCCAsmStmt::AsmStringPiece::AsmStringPiece()
            {
            }

            GCCAsmStmt::GCCAsmStmt()
                : AsmStmt(StmtClass::GCCAsmStmt)
                , rParenLoc(SourceLocation())
            {
            }

            MSAsmStmt::MSAsmStmt()
                : AsmStmt(StmtClass::MSAsmStmt)
                , lBraceLoc(SourceLocation())
                , hasBraces(0)
                , numAsmToks(0)
            {
            }

            SEHExceptStmt::SEHExceptStmt()
                : Stmt(StmtClass::SEHExceptStmt)
                , exceptLoc(SourceLocation())
                , filterExpr(nullptr)
                , block(nullptr)
            {
            }

            SEHFinallyStmt::SEHFinallyStmt()
                : Stmt(StmtClass::SEHFinallyStmt)
                , finallyLoc(SourceLocation())
                , block(nullptr)
            {
            }

            SEHTryStmt::SEHTryStmt()
                : Stmt(StmtClass::SEHTryStmt)
                , tryLoc(SourceLocation())
                , isCXXTry(0)
                , tryBlock(nullptr)
                , handler(nullptr)
                , exceptHandler(nullptr)
                , finallyHandler(nullptr)
            {
            }

            SEHLeaveStmt::SEHLeaveStmt()
                : Stmt(StmtClass::SEHLeaveStmt)
                , leaveLoc(SourceLocation())
            {
            }

            CapturedStmt::Capture::Capture()
            {
            }

            CapturedStmt::CapturedStmt()
                : Stmt(StmtClass::CapturedStmt)
                , capturedStmt(nullptr)
                , capture_size(0)
            {
            }

            DEF_VECTOR(CapturedStmt, Expr*, capture_inits)

                CXXCatchStmt::CXXCatchStmt()
                : Stmt(StmtClass::CXXCatchStmt)
                , catchLoc(SourceLocation())
                , caughtType(QualifiedType())
                , handlerBlock(nullptr)
            {
            }

            CXXTryStmt::CXXTryStmt()
                : Stmt(StmtClass::CXXTryStmt)
                , tryLoc(SourceLocation())
                , tryBlock(nullptr)
                , numHandlers(0)
            {
            }

            CXXForRangeStmt::CXXForRangeStmt()
                : Stmt(StmtClass::CXXForRangeStmt)
                , init(nullptr)
                , rangeInit(nullptr)
                , cond(nullptr)
                , inc(nullptr)
                , body(nullptr)
                , rangeStmt(nullptr)
                , beginStmt(nullptr)
                , endStmt(nullptr)
                , loopVarStmt(nullptr)
                , forLoc(SourceLocation())
                , coawaitLoc(SourceLocation())
                , colonLoc(SourceLocation())
                , rParenLoc(SourceLocation())
            {
            }

            MSDependentExistsStmt::MSDependentExistsStmt()
                : Stmt(StmtClass::MSDependentExistsStmt)
                , keywordLoc(SourceLocation())
                , isIfExists(0)
                , isIfNotExists(0)
                , subStmt(nullptr)
            {
            }

            CoroutineBodyStmt::CtorArgs::CtorArgs()
            {
            }

            CoroutineBodyStmt::CoroutineBodyStmt()
                : Stmt(StmtClass::CoroutineBodyStmt)
                , hasDependentPromiseType(0)
                , body(nullptr)
                , promiseDeclStmt(nullptr)
                , initSuspendStmt(nullptr)
                , finalSuspendStmt(nullptr)
                , exceptionHandler(nullptr)
                , fallthroughHandler(nullptr)
                , allocate(nullptr)
                , deallocate(nullptr)
                , returnValueInit(nullptr)
                , resultDecl(nullptr)
                , returnStmt(nullptr)
                , returnStmtOnAllocFailure(nullptr)
            {
            }

            CoreturnStmt::CoreturnStmt()
                : Stmt(StmtClass::CoreturnStmt)
                , isImplicit(0)
                , keywordLoc(SourceLocation())
                , operand(nullptr)
                , promiseCall(nullptr)
            {
            }

        }
    }
}









/************************************************************************
*
* CppSharp
* Licensed under the simplified BSD license. All rights reserved.
*
************************************************************************/

#include "Target.h"

namespace CppSharp {
    namespace CppParser {

        ParserTargetInfo::ParserTargetInfo() :
            boolAlign(0),
            boolWidth(0),
            charAlign(0),
            charWidth(0),
            char16Align(0),
            char16Width(0),
            char32Align(0),
            char32Width(0),
            halfAlign(0),
            halfWidth(0),
            floatAlign(0),
            floatWidth(0),
            doubleAlign(0),
            doubleWidth(0),
            shortAlign(0),
            shortWidth(0),
            intAlign(0),
            intWidth(0),
            intMaxTWidth(0),
            longAlign(0),
            longWidth(0),
            longDoubleAlign(0),
            longDoubleWidth(0),
            longLongAlign(0),
            longLongWidth(0),
            pointerAlign(0),
            pointerWidth(0),
            wCharAlign(0),
            wCharWidth(0),
            float128Align(0),
            float128Width(0)
        {
        }

        ParserTargetInfo::~ParserTargetInfo() {}

    }
}