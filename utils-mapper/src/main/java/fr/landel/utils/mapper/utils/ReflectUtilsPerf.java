/*
 * #%L
 * utils-mapper
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.mapper.utils;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

/**
 * (Description)
 *
 * @since 15 mai 2016
 * @author Gilles
 *
 */
public class ReflectUtilsPerf {
    // private static final Logger LOGGER =
    // LoggerFactory.getLogger(ReflectUtilsPerf.class);

    @Benchmark
    public void hello() {
        // a dummy method to check the overhead
    }

    /*
     * It is better to run the benchmark from command-line instead of IDE.
     * 
     * To run, in command-line: $ mvn clean install exec:exec
     */
    public static void main(String[] args) throws RunnerException {
        Options options = new OptionsBuilder().include(ReflectUtilsPerf.class.getSimpleName()).forks(1).build();

        new Runner(options).run();
    }
}