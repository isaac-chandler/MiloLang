#include "TypeTable.h"


constexpr u32 S_UDT = 0x1108;
constexpr u32 T_VOID = 0x0003;
constexpr u32 T_64PVOID = 0x0603;
constexpr u32 T_64PUCHAR = 0x0620;
constexpr u32 T_INT1 = 0x0068;
constexpr u32 T_64PINT1 = 0x0668;
constexpr u32 T_UINT1 = 0x0069;
constexpr u32 T_64PUINT1 = 0x0669;
constexpr u32 T_INT2 = 0x0072;
constexpr u32 T_64PINT2 = 0x0672;
constexpr u32 T_UINT2 = 0x0073;
constexpr u32 T_64PUINT2 = 0x0673;
constexpr u32 T_INT4 = 0x0074;
constexpr u32 T_64PINT4 = 0x0674;
constexpr u32 T_UINT4 = 0x0075;
constexpr u32 T_64PUINT4 = 0x0675;
constexpr u32 T_INT8 = 0x0076;
constexpr u32 T_64PINT8 = 0x0676;
constexpr u32 T_UINT8 = 0x0077;
constexpr u32 T_64PUINT8 = 0x0677;
constexpr u32 T_REAL32 = 0x0040;
constexpr u32 T_64PREAL32 = 0x0640;
constexpr u32 T_REAL64 = 0x0041;
constexpr u32 T_64PREAL64 = 0x0641;
constexpr u32 T_BOOL08 = 0x0030;
constexpr u32 T_64PBOOL08 = 0x0630;

u32 getCoffTypeIndex(Type *type);
void exportTypeTableToDebugTSection(BucketedArenaAllocator *debugTypes_);
bool appendCoffName(BucketedArenaAllocator *debugSymbols, Type *type);
void alignDebugTypes(BucketedArenaAllocator *debugTypes);
u32 createFunctionIDType(ExprFunction *function);
