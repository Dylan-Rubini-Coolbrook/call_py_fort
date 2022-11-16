# This code only runs once at the initialization you can e.g. load an ML model
# from a file here
from time import perf_counter

import numpy as np


def function(STATE):

    start_full = perf_counter()

    a = STATE.get("a")
    b = STATE.get("b")
    c = STATE.get("c")
    d = STATE.get("d")
    e = STATE.get("e")

    start = perf_counter()
    STATE["c"] = 2.0 * a + 3.0 * b
    stop = perf_counter()
    print(f"Time scaler multiply = {stop - start} s")

    start = perf_counter()
    STATE["d"] = a * b
    stop = perf_counter()
    print(f"Time dot product = {stop - start} s")

    start = perf_counter()
    STATE["e"] = np.matmul(a, b)
    stop = perf_counter()
    print(f"Time matmul = {stop - start} s")

    stop_full = perf_counter()
    print(f"Time Wrap Func = {stop_full - start_full} s")
