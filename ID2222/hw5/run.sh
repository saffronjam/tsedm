#!/bin/bash
java -Xmx5000m -jar target/assignment4-jabeja-1.0-jar-with-dependencies.jar -graph=./graphs/twitter.graph -temp=0.88 -delta=0.01 -tmin=0.000015 $@
