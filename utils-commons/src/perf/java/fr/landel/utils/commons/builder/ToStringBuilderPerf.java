package fr.landel.utils.commons.builder;

import static org.junit.Assert.assertNotNull;

import java.awt.Color;
import java.io.IOException;
import java.util.Optional;
import java.util.function.Function;

import org.junit.Test;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.runner.RunnerException;

import fr.landel.utils.microbenchmark.AbstractMicrobenchmark;

/**
 * Check performance {@link ToStringBuilder}
 *
 * @since Mar 6, 2017
 * @author Gilles
 *
 */
@State(Scope.Benchmark)
public class ToStringBuilderPerf extends AbstractMicrobenchmark {

    @Override
    protected double getExpectedMinNbOpsPerSeconds() {
        return 200_000d;
    }

    /**
     * Test method for {@link ToStringBuilder}.
     */
    @Benchmark
    public void testBuild() {
        final Function<String, CharSequence> upper = text -> text.toUpperCase();

        final ToStringBuilder builder = new ToStringBuilder("test");
        builder.append(Color.BLACK);
        builder.appendAndFormat(Color.BLACK, color -> String.valueOf(color.getBlue()));
        builder.append("blue", Color.BLUE);
        builder.appendAndFormat("value", 120_156.568_9, ToStringBuilder.NUMBER_FORMATTER);
        builder.appendIfPresent(Optional.empty());
        builder.appendIfPresent(Optional.of("optional"));
        builder.appendAndFormatIfPresent(Optional.of("optional"), upper);
        builder.appendIfPresent("optional", Optional.ofNullable(null));
        builder.appendIfPresent("optional", Optional.of("optional"));
        builder.appendAndFormatIfPresent("optional", Optional.of("optional"), upper);
        builder.build();
    }

    @Test
    public void testPerf() throws IOException, RunnerException {
        assertNotNull(super.run());
    }
}
