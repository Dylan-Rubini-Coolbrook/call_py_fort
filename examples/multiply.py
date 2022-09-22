# This code only runs once at the initialization you can e.g. load an ML model
# from a file here
import numpy as np

def function(STATE):
    a = STATE.get("a")
    b = STATE.get("b")
    c = STATE.get("c")        

    STATE["c"] = 2.0 * a  + 3.0 * b
