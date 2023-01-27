cl /LD /MT /O2 /TC stb_truetype.h /DSTB_TRUETYPE_IMPLEMENTATION /DSTBTT_DEF=__declspec(dllexport)
del stb_truetype.obj
del stb_truetype.exp
del stb_truetype.lib
cl /c /Zl /O2 /TC stb_truetype.h /DSTB_TRUETYPE_IMPLEMENTATION /DSTBTT_DEF=extern
lib stb_truetype.obj
del stb_truetype.obj