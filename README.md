Researching of applying different performance optimizations while implementing a genetic algorithm.
Optimizations will be applied step by step and each improvement will be measured and compared to initial exec time.
The initial version is written to follow functional programming idioms as much as possible.
One of the goals of the researching is to compare it to scala implementation of the same algorithm.

CPU of the computer used to run measurement: Intel(R) Core(TM) i9-10900K CPU @ 3.70GHz.

OS: headless Linux nixos 6.12.63

JDK: OpenJdk 21

To try it yourself run: `sbt clean assembly` && `java -jar ./target/scala-3.7.4/genetic-algorithm`

Optimization steps and measurements:

1. Without optimization, it runs 100000 generations for about 4488088 ms (~ 75 minutes)
2. Replace Array[Gen] by Array[Long], it runs for about 92191 ms, ~ 49 times faster than initial one (~ 92  seconds)
3. Optimize Gray code, it runs for about 44863 ms, ~ 100 times faster than initial one (~ 44 seconds)
