Researching of applying different performance optimizations while implementing a genetic algorithm. 
Optimizations will be applied step by step and each improvement will be measured and compared to initial exec time.
The initial version is written to follow functional programming idioms as much as possible. 
One of the goals of the researching is to understand how functional style of programming in scala affects performance. 

CPU of the computer used to run measurement: Intel Core i5-4460 @ 3.2 GHz.
OS: Linux Ubuntu 17, kernel 4.13.0-17

To try it yourself run: sbt clean pack && ./target/pack/bin/main.

Optimization steps and measurements: 

1. Without optimization it runs 100000 generations for about 12413058.75 ms (~ 206 minutes)
2. Array[Gen] is replaced by Array[Long], : 193615 ms, ~ 64 times faster than initial one
3. Gray code is optimized: 123769 ms, ~ 100 times faster than initial one
4. Random is replaced by ThreadLocalRandom: 127226 ms, ~ 97 times faster than initial one. Regression!
