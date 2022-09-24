# This code only runs once at the initialization you can e.g. load an ML model
# from a file here
import numpy as np

global_test = 0.0

def function(STATE):
    global global_test
    a = STATE.get("a")
    b = STATE.get("b")
    c = STATE.get("c")

    print(global_test)
    global_test = 10.0      


    STATE["c"] = 2.0 * a  + 3.0 * b
