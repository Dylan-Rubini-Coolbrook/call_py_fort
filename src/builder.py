# file plugin_build.py
import os
import sys

import cffi

ffibuilder = cffi.FFI()

header = """
extern int set_state_py(char *, char *, char *, int*, int*, int*);
extern int get_state_py(char *, char *, char *, int*);
extern int set_state_char(char *, char *);
extern int get_state_char(char *, char *, int *);
extern int set_state_bool(char *, _Bool *);
extern int get_state_bool(char *, _Bool *);
extern int get_state_scalar_real(char *, float *);
extern int get_state_scalar_real8(char *, double *);
extern int get_state_scalar_integer(char *, int *);
extern int set_state_scalar_real8(char *, double *);
extern int set_state_scalar_real(char *, float *);
extern int set_state_scalar_integer(char *, int *);
extern int call_function(char *, char *);
"""

with open("plugin.h", "w") as f:
    f.write(header)

ffibuilder.embedding_api(header)

ffibuilder.set_source(
    "my_plugin",
    r"""
    #include "plugin.h"
""",
)

with open(sys.argv[1]) as f:
    ffibuilder.embedding_init_code(f.read())

ffibuilder.emit_c_code("plugin.c")
# ffibuilder.compile(target="libplugin.so", verbose=True)
