#pragma once

#include "Basic.h"

#if BUILD_WINDOWS
//
// Author:   Jonathan Blow
// Version:  2
// Date:     7 May, 2019  (update to original version released on 31 August, 2018).
//
// This code is released under the MIT license, which you can find at
//
//          https://opensource.org/licenses/MIT
//
//
//
// See the comments for how to use this library just below the includes.
//


#include <windows.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <sys/stat.h>

#include <stdint.h>
#include <io.h>         // For _get_osfhandle

//
//
// HOW TO USE THIS CODE
//
// The purpose of this file is to find the folders that contain libraries
// you may need to link against, on Windows, if you are linking with any
// compiled C or C++ code. This will be necessary for many non-C++ programming
// language environments that want to provide compatibility.
//
// We find the place where the Visual Studio libraries live (for example,
// libvcruntime.lib), where the linker and compiler executables live
// (for example, link.exe), and where the Windows SDK libraries reside
// (kernel32.lib, libucrt.lib).
//
// We all wish you didn't have to worry about so many weird dependencies,
// but we don't really have a choice about this, sadly.
//
// I don't claim that this is the absolute best way to solve this problem,
// and so far we punt on things (if you have multiple versions of Visual Studio
// installed, we return the first one, rather than the newest). But it
// will solve the basic problem for you as simply as I know how to do it,
// and because there isn't too much code here, it's easy to modify and expand.
//
//
// Here is the API you need to know about:
//

struct Find_Result {
    int windows_sdk_version;   // Zero if no Windows SDK found.

    wchar_t *windows_sdk_root = NULL;
    wchar_t *windows_sdk_bin = NULL;
    wchar_t *windows_sdk_um_library_path = NULL;
    wchar_t *windows_sdk_ucrt_library_path = NULL;

    wchar_t *vs_exe_path = NULL;
    wchar_t *vs_library_path = NULL;
};

Find_Result find_visual_studio_and_windows_sdk();

void free_resources(Find_Result *result);
#endif