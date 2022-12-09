#!/usr/bin/python
import os
import numpy as np
import time

i = 0
for t in np.arange(0.8, 1, 0.04):
    for d in np.arange(0.005, 0.05, 0.005):
        for t_min in np.arange(0.000005, 0.00005, 0.000005):
            i += 1
            if i % 12 == 0:
                print(f"sleeping with T={t} delay={d} t_min{t_min}")
                time.sleep(15)

            os.system(
                f"java -Xmx5000m -jar target/assignment4-jabeja-1.0-jar-with-dependencies.jar -graph=./graphs/3elt.graph -temp={t} -delta={d} -tmin={t_min} $@ >> /dev/null &")
