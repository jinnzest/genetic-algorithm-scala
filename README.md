Researching of applying different performance optimizations while implementing a genetic algorithm. 
Optimizations will be applied step by step and each improvement will be measured and compared to initial exec time.
The initial version is written to follow functional programming idioms as much as possible. 
One of the goals of the researching is to understand how functional style of programming in scala affects performance. 

CPU of the computer used to run measurement: Intel(R) Core(TM) i9-10900K CPU @ 3.70GHz.

OS: headless linux, version 6.1.77-2-MANJARO, 

JDK: openjdk version "23-ea" 2024-09-17

To try it yourself run: `sbt clean assembly` && `java -jar ./target/scala-2.13/genetic-algorithm`.

Optimization steps and measurements: 

1. Without optimization it runs 100000 generations for about 4832584 ms (~ 80 minutes)

