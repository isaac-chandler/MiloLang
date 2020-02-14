#include "Basic.h"
#include "Ast.h"

Type TYPE_S8  = { 1, 1, TYPE_NUMBER_IS_SIGNED, TypeFlavor::INTEGER };
Type TYPE_S16 = { 2, 2, TYPE_NUMBER_IS_SIGNED, TypeFlavor::INTEGER };
Type TYPE_S32 = { 4, 4, TYPE_NUMBER_IS_SIGNED, TypeFlavor::INTEGER };
Type TYPE_S64 = { 8, 8, TYPE_NUMBER_IS_SIGNED, TypeFlavor::INTEGER };

Type TYPE_U8  = { 1, 1, 0, TypeFlavor::INTEGER };
Type TYPE_U16 = { 2, 2, 0, TypeFlavor::INTEGER };
Type TYPE_U32 = { 4, 4, 0, TypeFlavor::INTEGER };
Type TYPE_U64 = { 8, 8, 0, TypeFlavor::INTEGER };

Type TYPE_UNSIGNED_INT_LITERAL = { 0, 0, TYPE_IS_INTERNAL, TypeFlavor::INTEGER };
Type TYPE_SIGNED_INT_LITERAL = { 0, 0, TYPE_NUMBER_IS_SIGNED | TYPE_IS_INTERNAL, TypeFlavor::INTEGER };

Type TYPE_F32 = { 4, 4, 0, TypeFlavor::FLOAT };
Type TYPE_F64 = { 8, 8, 0, TypeFlavor::FLOAT };

Type TYPE_FLOAT_LITERAL = { 0, 0, TYPE_IS_INTERNAL, TypeFlavor::FLOAT };

Type TYPE_VOID = { 1, 1, 0, TypeFlavor::VOID }; // Give void size and alignment of 1, so *void math just adds raw memory
Type TYPE_BOOL = { 1, 1, 0, TypeFlavor::BOOL };

Type TYPE_TYPE = { 0, 0, 0, TypeFlavor::TYPE };

Type TYPE_AUTO_CAST = { 0, 0, TYPE_IS_INTERNAL, TypeFlavor::AUTO_CAST };

TypePointer TYPE_VOID_POINTER = { 8, 8, 0, TypeFlavor::POINTER, &TYPE_VOID };