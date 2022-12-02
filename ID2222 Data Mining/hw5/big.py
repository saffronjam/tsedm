#!/usr/bin/python
import os
import numpy as np
import time

t = 3
d = 0.001
i = 0
for t in np.arange(1.0, 4.0, 0.25):

    for d in np.arange(0.0001, 0.01, 0.001):
        i += 1
        if i % 12:
            print(f"sleeping with T={t} delay={d}")
            time.sleep(2)

        os.system(
            f"java -Xmx5000m -jar target/assignment4-jabeja-1.0-jar-with-dependencies.jar -graph=./graphs/3elt.graph -temp={t} -delta={d} $@ >> /dev/null &")
