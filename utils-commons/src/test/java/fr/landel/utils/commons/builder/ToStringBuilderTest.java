/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.builder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.awt.Color;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;

import org.junit.Test;

/**
 * Check {@link ToStringBuilder}
 *
 * @since Mar 5, 2017
 * @author Gilles
 *
 */
public class ToStringBuilderTest {

    /**
     * Test method for {@link ToStringBuilder#build()}.
     */
    @Test
    public void testBuild() {
        // Bug Java 8 (ambiguous)
        final Supplier<String> supplier = () -> "supplier";
        final Function<String, CharSequence> upper = text -> text.toUpperCase();

        // Default

        StringBuilder expected = new StringBuilder("test");
        expected.append("[");
        expected.append("java.awt.Color[r=0,g=0,b=0]");
        expected.append(",");
        expected.append("0");
        expected.append(",");
        expected.append("blue=java.awt.Color[r=0,g=0,b=255]");
        expected.append(",");
        expected.append("value=1 153 120 156,569");
        expected.append(",");
        expected.append("supplier");
        expected.append(",");
        expected.append("SUPPLIER");
        expected.append(",");
        expected.append("supplier=12");
        // XXX expected.append(",");
        // expected.append("supplier=12");
        expected.append(",");
        expected.append("supplier=SUPPLIER");
        expected.append(",");
        expected.append("optional");
        expected.append(",");
        expected.append("OPTIONAL");
        expected.append(",");
        expected.append("optional=optional");
        expected.append(",");
        expected.append("optional=OPTIONAL");
        expected.append("]");

        ToStringBuilder builder = new ToStringBuilder("test");
        builder.append(Color.BLACK);
        builder.append(Color.BLACK, color -> String.valueOf(color.getBlue()));
        builder.append("blue", Color.BLUE);
        builder.append("value", 1_153_120_156.568_9, ToStringBuilder.NUMBER_FORMATTER);
        builder.append(supplier);
        builder.append(supplier, upper);
        // XXX Java 8.121 Ambiguous // builder.append(() -> "supplier", text ->
        // text.toUpperCase());
        builder.append("supplier", () -> 12);
        // XXX Java 8.121 Ambiguous // builder.append("supplier", () -> 12,
        // ToStringStyle.NUMBER_FORMATTER);
        builder.append("supplier", supplier, upper);
        builder.appendIfPresent(Optional.empty());
        builder.appendIfPresent(Optional.of("optional"));
        builder.appendIfPresent(Optional.of("optional"), text -> text.toUpperCase());
        builder.appendIfPresent("optional", Optional.ofNullable(null));
        builder.appendIfPresent("optional", Optional.of("optional"));
        builder.appendIfPresent("optional", Optional.of("optional"), text -> text.toUpperCase());

        assertEquals(expected.toString(), builder.build());

        // Class

        builder = new ToStringBuilder(ToStringBuilderTest.class);

        assertEquals(ToStringBuilderTest.class.getCanonicalName(), builder.build());

        // Class with specified style

        builder = new ToStringBuilder(ToStringBuilderTest.class, ToStringStyle.DEFAULT);

        assertEquals(ToStringBuilderTest.class.getCanonicalName(), builder.build());

        // Object instance

        builder = new ToStringBuilder(Color.BLACK);

        assertEquals(Color.class.getCanonicalName(), builder.build());

        // null object

        try {
            builder = new ToStringBuilder(null, ToStringStyle.DEFAULT);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        // JSON

        expected = new StringBuilder("{\"test\":");
        expected.append("{");
        expected.append("\"java.awt.Color[r=0,g=0,b=0]\"");
        expected.append(",");
        expected.append("\"0\"");
        expected.append(",");
        expected.append("\"blue\":\"java.awt.Color[r=0,g=0,b=255]\"");
        expected.append(",");
        expected.append("\"value\":\"120 156,569\"");
        expected.append(",");
        expected.append("\"supplier\"");
        expected.append(",");
        expected.append("\"SUPPLIER\"");
        expected.append(",");
        expected.append("\"supplier\":\"12\"");
        // XXX expected.append(",");
        // expected.append("\"supplier\":\"12\"");
        expected.append(",");
        expected.append("\"supplier\":\"SUPPLIER\"");
        expected.append(",");
        expected.append("\"optional\"");
        expected.append(",");
        expected.append("\"OPTIONAL\"");
        expected.append(",");
        expected.append("\"optional\":\"optional\"");
        expected.append(",");
        expected.append("\"optional\":\"OPTIONAL\"");
        expected.append("}}");

        builder = new ToStringBuilder("test", ToStringStyle.JSON);
        builder.append(Color.BLACK);
        builder.append(Color.BLACK, color -> String.valueOf(color.getBlue()));
        builder.append("blue", Color.BLUE);
        builder.append("value", 120_156.568_9, ToStringBuilder.NUMBER_FORMATTER);
        builder.append(supplier);
        builder.append(supplier, upper);
        builder.append("supplier", () -> 12);
        // XXX Java 8.121 Ambiguous // builder.append("supplier", () -> 12,
        // ToStringStyle.NUMBER_FORMATTER);
        builder.append("supplier", supplier, upper);
        builder.appendIfPresent(Optional.empty());
        builder.appendIfPresent(Optional.of("optional"));
        builder.appendIfPresent(Optional.of("optional"), text -> text.toUpperCase());
        builder.appendIfPresent("optional", Optional.ofNullable(null));
        builder.appendIfPresent("optional", Optional.of("optional"));
        builder.appendIfPresent("optional", Optional.of("optional"), text -> text.toUpperCase());

        assertEquals(expected.toString(), builder.build());

        // READABLE

        expected = new StringBuilder("test = ");
        expected.append("\n[");
        expected.append("'java.awt.Color[r=0,g=0,b=0]'");
        expected.append(",\n");
        expected.append("'0'");
        expected.append(",\n");
        expected.append("'blue' = 'java.awt.Color[r=0,g=0,b=255]'");
        expected.append(",\n");
        expected.append("'value' = '120 156,569'");
        expected.append(",\n");
        expected.append("'supplier'");
        expected.append(",\n");
        expected.append("'SUPPLIER'");
        expected.append(",\n");
        expected.append("'supplier' = '12'");
        expected.append(",\n");
        // XXX expected.append("'supplier' = '12'");
        // expected.append(",\n");
        expected.append("'supplier' = 'SUPPLIER'");
        expected.append(",\n");
        expected.append("'optional'");
        expected.append(",\n");
        expected.append("'OPTIONAL'");
        expected.append(",\n");
        expected.append("'optional' = 'optional'");
        expected.append(",\n");
        expected.append("'optional' = 'OPTIONAL'");
        expected.append("]");

        builder = new ToStringBuilder("test", ToStringStyle.READABLE);
        builder.append(Color.BLACK);
        builder.append(Color.BLACK, color -> String.valueOf(color.getBlue()));
        builder.append("blue", Color.BLUE);
        builder.append("value", 120_156.568_9, ToStringBuilder.NUMBER_FORMATTER);
        builder.append(supplier);
        builder.append(supplier, upper);
        builder.append("supplier", () -> 12);
        // XXX Java 8.121 Ambiguous // builder.append("supplier", () -> 12,
        // ToStringStyle.NUMBER_FORMATTER);
        builder.append("supplier", supplier, upper);
        builder.appendIfPresent(Optional.empty());
        builder.appendIfPresent(Optional.of("optional"));
        builder.appendIfPresent(Optional.of("optional"), text -> text.toUpperCase());
        builder.appendIfPresent("optional", Optional.ofNullable(null));
        builder.appendIfPresent("optional", Optional.of("optional"));
        builder.appendIfPresent("optional", Optional.of("optional"), text -> text.toUpperCase());

        assertEquals(expected.toString(), builder.build());
        assertEquals(expected.toString(), builder.toString());
    }
}
