from time import perf_counter

import numpy as np


def function(a, b):

    start_full = perf_counter()

    start = perf_counter()
    c = 2.0 * a + 3.0 * b
    stop = perf_counter()
    print(f"Time scaler multiply = {stop - start} s")

    start = perf_counter()
    c = a * b
    stop = perf_counter()
    print(f"Time dot product = {stop - start} s")

    start = perf_counter()
    c = np.matmul(a, b)
    stop = perf_counter()
    print(f"Time matmul = {stop - start} s")

    stop_full = perf_counter()
    print(f"Time Func = {stop_full - start_full} s")

    print(c[0, 0])
    print(c[100, 100])
    print(c[330, 200])

    return c


n = 5000
a = 3.0 * np.ones(shape=(n, n), dtype="f")
b = 2.0 * np.ones(shape=(n, n), dtype="f")
c = np.zeros(shape=(n, n), dtype="f")

start = perf_counter()
c = function(a, b)
stop = perf_counter()

print(f"Time = {stop - start} s")
