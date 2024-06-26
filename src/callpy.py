import importlib
import logging
import sys

import numpy as np
from my_plugin import ffi

logging.basicConfig(level=logging.INFO)

# Global state array
STATE = {}

# Create the dictionary mapping ctypes to np dtypes.
ctype2dtype = {}

# Integer types
for prefix in ("int", "uint"):
    for log_bytes in range(4):
        ctype = "%s%d_t" % (prefix, 8 * (2**log_bytes))
        dtype = "%s%d" % (prefix[0], 2**log_bytes)
        # print( ctype )
        # print( dtype )
        ctype2dtype[ctype] = np.dtype(dtype)

# Floating point types
ctype2dtype["float"] = np.dtype("f4")
ctype2dtype["double"] = np.dtype("f8")
ctype2dtype["_Bool"] = np.dtype(np.bool_)


def asarray(ffi, ptr, shape, dtype, **kwargs):
    length = np.prod(shape)
    # Get the canonical C type of the elements of ptr as a string.
    T = dtype
    # print( T )
    # print( ffi.sizeof( T ) )

    if T not in ctype2dtype:
        raise RuntimeError("Cannot create an array for element type: %s" % T)

    return np.frombuffer(ffi.buffer(ptr, length * ffi.sizeof(T)),
                         ctype2dtype[T]).reshape(shape, **kwargs)


@ffi.def_extern(error=1)
def set_state_py(tag, dtype, t, nx, ny, nz, nn):
    shape = (nn[0], nz[0], ny[0], nx[0])
    shape = [n for n in shape if n != -1]

    tag = ffi.string(tag).decode("UTF-8")
    dtype = ffi.string(dtype).decode("UTF-8")
    arr = asarray(ffi, t, shape, dtype)
    STATE[tag] = arr
    return 0


@ffi.def_extern(error=1)
def set_state_bool(tag, t):
    tag = ffi.string(tag).decode("UTF-8")
    STATE[tag] = t[0]
    return 0


@ffi.def_extern(error=1)
def set_state_scalar_real(tag, t):
    tag = ffi.string(tag).decode("UTF-8")
    STATE[tag] = t[0]
    return 0


@ffi.def_extern(error=1)
def set_state_scalar_real8(tag, t):
    tag = ffi.string(tag).decode("UTF-8")
    STATE[tag] = t[0]
    return 0


@ffi.def_extern(error=1)
def set_state_scalar_integer(tag, t):
    tag = ffi.string(tag).decode("UTF-8")
    STATE[tag] = t[0]
    return 0


@ffi.def_extern(error=1)
def set_state_char(*args):
    tag, chr = [ffi.string(x).decode("UTF-8") for x in args]
    STATE[tag] = chr
    return 0


@ffi.def_extern(error=1)
def get_state_char(tag_ptr, value_ptr, size_ptr):
    tag = ffi.string(tag_ptr).decode("UTF-8")
    value = STATE[tag]
    size = size_ptr[0]
    assert isinstance(value, str)
    value_encoded = value.encode("UTF-8")
    destination_buffer = ffi.buffer(value_ptr, size)
    destination_buffer[:len(value_encoded)] = value_encoded
    return 0


@ffi.def_extern(error=1)
def get_state_bool(tag, t):
    tag = ffi.string(tag).decode("UTF-8")
    ttype = "_Bool"
    destination_buffer = np.frombuffer(ffi.buffer(t, ffi.sizeof(ttype)),
                                       ctype2dtype[ttype])
    destination_buffer[:] = STATE[tag]
    return 0


@ffi.def_extern(error=1)
def get_state_scalar_integer(tag, t):
    tag = ffi.string(tag).decode("UTF-8")
    ttype = "int32_t"
    destination_buffer = np.frombuffer(ffi.buffer(t, ffi.sizeof(ttype)),
                                       ctype2dtype[ttype])
    destination_buffer[:] = STATE[tag]
    return 0


@ffi.def_extern(error=1)
def get_state_scalar_real(tag, t):
    tag = ffi.string(tag).decode("UTF-8")
    ttype = "float"
    destination_buffer = np.frombuffer(ffi.buffer(t, ffi.sizeof(ttype)),
                                       ctype2dtype[ttype])
    destination_buffer[:] = STATE[tag]
    return 0


@ffi.def_extern(error=1)
def get_state_scalar_real8(tag, t):
    tag = ffi.string(tag).decode("UTF-8")
    ttype = "double"
    destination_buffer = np.frombuffer(ffi.buffer(t, ffi.sizeof(ttype)),
                                       ctype2dtype[ttype])
    destination_buffer[:] = STATE[tag]
    return 0


@ffi.def_extern(error=1)
def get_state_py(tag, dtype, t, n):
    tag = ffi.string(tag).decode("UTF-8")
    dtype = ffi.string(dtype).decode("UTF-8")
    arr = asarray(ffi, t, (n[0], ), dtype)

    src = STATE.get(tag)
    arr[:] = src.ravel()
    return 0


@ffi.def_extern(error=1)
def call_function(module_name, function_name):
    """Call a python function by name"""

    module_name = ffi.string(module_name).decode("UTF-8")
    function_name = ffi.string(function_name).decode("UTF-8")

    # import the python module
    mod = importlib.import_module(module_name)

    # the function we want to call
    fun = getattr(mod, function_name)

    # call the function
    # this function can edit STATE inplace
    fun(STATE)

    return 0
