#!/usr/bin/python
import os
import numpy as np
import time

i = 0
for t in np.arange(4.0, 5.0, 0.1):

    for d in np.arange(0.0001, 0.05, 0.01):
        i += 1
        if i % 12 == 0:
            print(f"sleeping with T={t} delay={d}")
            time.sleep(5)

        os.system(
            f"java -Xmx5000m -jar target/assignment4-jabeja-1.0-jar-with-dependencies.jar -graph=./graphs/ws-250.graph -temp={t} -delta={d} $@ >> /dev/null &")
