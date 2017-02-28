package fr.landel.utils.commons;

import static org.junit.Assert.assertNotNull;

import java.io.IOException;

import org.junit.Test;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.runner.RunnerException;

import fr.landel.utils.microbenchmark.AbstractMicrobenchmark;

/**
 * Check {@link StringUtils} performance
 *
 * @since Feb 28, 2017
 * @author Gilles
 *
 */
@State(Scope.Benchmark)
public class StringUtilsPerf extends AbstractMicrobenchmark {

    @Override
    protected double getExpectedMinNbOpsPerSeconds() {
        return 200_000d;
    }

    /**
     * Test method for
     * {@link StringUtils#inject(java.lang.CharSequence, java.lang.Object[])}.
     */
    @Benchmark
    public void testInject() {
        StringUtils.inject("I'll go to the beach this afternoon");
        StringUtils.inject("I'll go to {} {3} {} {2}");
        StringUtils.inject("I'll go to {} {3} {} {2}", "the", "this", "afternoon", "beach");
        StringUtils.inject("I'll go to {{}}{3} {} {2}{{0}} {4} {text}", "the", "this", "afternoon", "beach");
    }

    /**
     * Test method for
     * {@link StringUtils#inject(java.lang.CharSequence, java.lang.Object[])}.
     */
    // @Benchmark
    public void testFormat() {
        // last comparison
        // StringUtils.inject: 570 449,731 ops/s
        // String.format: 140 260,855 ops/s

        String.format("I'll go to the beach this afternoon");
        String.format("I'll go to %%s %%4$s %%s %%3$s");
        String.format("I'll go to %s %4$s %s %3$s", "the", "this", "afternoon", "beach");
        String.format("I'll go to %%4$s %s %3$s%%s %%5$s %%text", "the", "this", "afternoon", "beach");
    }

    @Test
    public void testPerf() throws IOException, RunnerException {
        assertNotNull(super.run());
    }
}
