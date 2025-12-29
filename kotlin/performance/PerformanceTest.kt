// brew install kotlin

// kotlinc PerformanceTest.kt -include-runtime -d PerformanceTest.jar

// java -jar PerformanceTest.jar

import kotlin.system.measureTimeMillis

fun main() {
    val numbers = (1..200_000_000).toList()
    
    val time = measureTimeMillis {
        val result = numbers.parallelStream()
            .map { it * it }
            .filter { it % 2 == 0 }
            .count()
        println("Processed $result items")
    }
    
    println("Burst task completed in: ${time}ms")
}
