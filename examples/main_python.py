
import numpy as np
from time import perf_counter
def function(a, b):

    return 2.0 * a  + 3.0 * b

n = 10000
a = 3.0 * np.ones(shape=(n, n), dtype="f8")
b = 2.0 * np.ones(shape=(n, n), dtype="f8")
c = np.zeros(shape=(n, n), dtype="f8")

start = perf_counter()
c = function(a, b)
stop = perf_counter()

print(f"Time = {stop - start} s")