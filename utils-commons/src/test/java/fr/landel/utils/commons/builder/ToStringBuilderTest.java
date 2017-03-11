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

import org.junit.Test;

import fr.landel.utils.commons.Default;
import fr.landel.utils.commons.Result;

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
    public void testToStringStyleDefaultBuild() {
        // Default

        StringBuilder expected = new StringBuilder("test");
        expected.append("[");
        expected.append("java.awt.Color[r=0,g=0,b=0]");
        expected.append(",");
        expected.append("java.awt.Color[r=255,g=0,b=0]");
        expected.append(",");
        expected.append("0");
        expected.append(",");
        expected.append("blue=java.awt.Color[r=0,g=0,b=255]");
        expected.append(",");
        expected.append("red=java.awt.Color[r=255,g=0,b=0]");
        expected.append(",");
        expected.append("value=").append(String.format("%,.3f", 1_153_120_156.569));
        expected.append(",");

        expected.append("value");
        expected.append(",");
        expected.append("value2");
        expected.append(",");
        expected.append("VALUE");
        expected.append(",");
        expected.append("key=value");
        expected.append(",");
        expected.append("key=value2");
        expected.append(",");
        expected.append("key=VALUE");
        expected.append(",");

        expected.append("default");
        expected.append(",");
        expected.append("default");
        expected.append(",");
        expected.append("value");
        expected.append(",");
        expected.append("VALUE");
        expected.append(",");
        expected.append("default=default");
        expected.append(",");
        expected.append("default=value");
        expected.append(",");
        expected.append("default=value2");
        expected.append(",");
        expected.append("default=VALUE");
        expected.append(",");

        expected.append("optional");
        expected.append(",");
        expected.append("optional");
        expected.append(",");
        expected.append("OPTIONAL");
        expected.append(",");
        expected.append("optional=optional");
        expected.append(",");
        expected.append("optional2=optional");
        expected.append(",");
        expected.append("optional=OPTIONAL");
        expected.append(",");

        expected.append("result");
        expected.append(",");
        expected.append("result2");
        expected.append(",");
        expected.append("RESULT");
        expected.append(",");
        expected.append("result=null");
        expected.append(",");
        expected.append("result=result");
        expected.append(",");
        expected.append("result=result2");
        expected.append(",");
        expected.append("result=RESULT");
        expected.append("]");

        ToStringBuilder builder = new ToStringBuilder("test");
        builder.append(Color.BLACK);
        builder.appendIf(Color.GREEN, c -> c.getRed() > 0);
        builder.appendIf(Color.RED, c -> c.getRed() > 0);
        builder.appendAndFormat(Color.BLACK, color -> String.valueOf(color.getBlue()));
        builder.appendAndFormatIf(Color.RED, color -> color.getBlue() > 0, color -> String.valueOf(color.getBlue()));
        builder.append("blue", Color.BLUE);
        builder.appendIf("green", Color.GREEN, c -> c.getRed() > 0);
        builder.appendIf("red", Color.RED, c -> c.getRed() > 0);
        builder.appendAndFormat("value", 1_153_120_156.568_9, ToStringBuilder.NUMBER_FORMATTER);
        builder.appendAndFormatIf("red", Color.RED, color -> color.getBlue() > 0, color -> String.valueOf(color.getBlue()));

        builder.appendIfNotNull((Object) null);
        builder.appendIfNotNullIf((Object) null, v -> v.equals("Check NullPointerException"));
        builder.appendIfNotNull("value");
        builder.appendIfNotNullIf("value1", s -> s.startsWith("a"));
        builder.appendIfNotNullIf("value2", s -> s.startsWith("v"));
        builder.appendAndFormatIfNotNull("value", text -> text.toUpperCase());
        builder.appendAndFormatIfNotNullIf("value", v -> v.startsWith("a"), text -> text.toUpperCase());
        builder.appendIfNotNull("key", (String) null);
        builder.appendIfNotNullIf("key", (Object) null, v -> v.equals("Check NullPointerException"));
        builder.appendIfNotNull("key", "value");
        builder.appendIfNotNullIf("key", "value1", s -> s.startsWith("a"));
        builder.appendIfNotNullIf("key", "value2", s -> s.startsWith("v"));
        builder.appendAndFormatIfNotNull("key", "value", text -> text.toUpperCase());
        builder.appendAndFormatIfNotNullIf("key", "value", s -> s.startsWith("a"), text -> text.toUpperCase());

        builder.appendDefault(Default.empty("default"));
        builder.appendDefaultIf(Default.empty("default"), v -> v.length() == 0);
        builder.appendDefaultIf(Default.empty("default"), v -> v.length() > 0);
        builder.appendDefault(Default.of("value"));
        builder.appendAndFormatDefault(Default.of("value"), text -> text.toUpperCase());
        builder.appendAndFormatDefaultIf(Default.of("value"), v -> v.isEmpty(), text -> text.toUpperCase());
        builder.appendDefault("default", Default.ofNullable(null, "default"));
        builder.appendDefault("default", Default.of("value"));
        builder.appendDefaultIf("default", Default.of("value1"), v -> v.startsWith("x"));
        builder.appendDefaultIf("default", Default.of("value2"), v -> v.startsWith("v"));
        builder.appendAndFormatDefault("default", Default.of("value"), text -> text.toUpperCase());
        builder.appendAndFormatDefaultIf("default", Default.of("value"), s -> s.startsWith("a"), text -> text.toUpperCase());

        builder.appendIfPresent(Optional.empty());
        builder.appendIfPresent(Optional.of("optional"));
        builder.appendIfPresentIf(Optional.of("optional"), v -> v.contains("e"));
        builder.appendIfPresentIf(Optional.of("optional"), v -> v.contains("i"));
        builder.appendAndFormatIfPresent(Optional.of("optional"), text -> text.toUpperCase());
        builder.appendAndFormatIfPresentIf(Optional.of("optional"), v -> v.isEmpty(), text -> text.toUpperCase());
        builder.appendIfPresent("optional", Optional.ofNullable(null));
        builder.appendIfPresent("optional", Optional.of("optional"));
        builder.appendIfPresentIf("optional1", Optional.of("optional"), v -> v.contains("e"));
        builder.appendIfPresentIf("optional2", Optional.of("optional"), v -> v.contains("i"));
        builder.appendAndFormatIfPresent("optional", Optional.of("optional"), text -> text.toUpperCase());
        builder.appendAndFormatIfPresentIf("optional", Optional.of("optional"), v -> v.contains("e"), text -> text.toUpperCase());

        builder.appendIfPresent(Result.empty());
        builder.appendIfPresentIf(Result.empty(), v -> v != null);
        builder.appendIfPresent(Result.of("result"));
        builder.appendIfPresentIf(Result.of("result1"), v -> v.startsWith("e"));
        builder.appendIfPresentIf(Result.of("result2"), v -> v.startsWith("r"));
        builder.appendAndFormatIfPresent(Result.of("result"), text -> text.toUpperCase());
        builder.appendAndFormatIfPresent(Result.<String> empty(), text -> text.toUpperCase());
        builder.appendAndFormatIfPresentIf(Result.of("result"), v -> v.isEmpty(), text -> text.toUpperCase());
        builder.appendIfPresent("result", Result.ofNullable(null));
        builder.appendIfPresent("result", Result.of("result"));
        builder.appendIfPresentIf("result", Result.of("result1"), v -> v.startsWith("e"));
        builder.appendIfPresentIf("result", Result.of("result2"), v -> v.startsWith("r"));
        builder.appendAndFormatIfPresent("result", Result.of("result"), text -> text.toUpperCase());
        builder.appendAndFormatIfPresentIf("result", Result.of("result"), v -> v.isEmpty(), text -> text.toUpperCase());
        builder.appendAndFormatIfPresent("result", Result.<String> empty(), text -> text.toUpperCase());
        builder.appendAndFormatIfPresentIf("result", Result.<String> empty(), v -> v.isEmpty(), text -> text.toUpperCase());

        assertEquals(expected.toString(), builder.build());
    }

    /**
     * Test method for {@link ToStringBuilder#build()}.
     */
    @Test
    public void testToStringStyleJSONBuild() {
        StringBuilder expected = new StringBuilder("{test:");
        expected.append("{");
        expected.append("java.awt.Color[r=0,g=0,b=0]");
        expected.append(",");
        expected.append("0");
        expected.append(",");
        expected.append("blue:java.awt.Color[r=0,g=0,b=255]");
        expected.append(",");
        expected.append("value:").append(String.format("%,.3f", 120156.569)).append("");
        expected.append(",");
        expected.append("optional");
        expected.append(",");
        expected.append("OPTIONAL");
        expected.append(",");
        expected.append("optional:optional");
        expected.append(",");
        expected.append("optional:OPTIONAL");
        expected.append("}}");

        ToStringBuilder builder = new ToStringBuilder("test", ToStringStyles.JSON);
        builder.append(Color.BLACK);
        builder.appendAndFormat(Color.BLACK, color -> String.valueOf(color.getBlue()));
        builder.append("blue", Color.BLUE);
        builder.appendAndFormat("value", 120_156.568_9, ToStringBuilder.NUMBER_FORMATTER);
        builder.appendIfPresent(Optional.empty());
        builder.appendIfPresent(Optional.of("optional"));
        builder.appendAndFormatIfPresent(Optional.of("optional"), text -> text.toUpperCase());
        builder.appendIfPresent("optional", Optional.ofNullable(null));
        builder.appendIfPresent("optional", Optional.of("optional"));
        builder.appendAndFormatIfPresent("optional", Optional.of("optional"), text -> text.toUpperCase());

        assertEquals(expected.toString(), builder.build());
    }

    /**
     * Test method for {@link ToStringBuilder#build()}.
     */
    @Test
    public void testToStringStyleJSONSpacedBuild() {
        StringBuilder expected = new StringBuilder("{test: ");
        expected.append("{");
        expected.append("java.awt.Color[r=0,g=0,b=0]");
        expected.append(", ");
        expected.append("0");
        expected.append(", ");
        expected.append("blue: java.awt.Color[r=0,g=0,b=255]");
        expected.append(", ");
        expected.append("value: ").append(String.format("%,.3f", 120156.569)).append("");
        expected.append(", ");
        expected.append("optional");
        expected.append(", ");
        expected.append("OPTIONAL");
        expected.append(", ");
        expected.append("optional: optional");
        expected.append(", ");
        expected.append("optional: OPTIONAL");
        expected.append("}}");

        ToStringBuilder builder = new ToStringBuilder("test", ToStringStyles.JSON_SPACED);
        builder.append(Color.BLACK);
        builder.appendAndFormat(Color.BLACK, color -> String.valueOf(color.getBlue()));
        builder.append("blue", Color.BLUE);
        builder.appendAndFormat("value", 120_156.568_9, ToStringBuilder.NUMBER_FORMATTER);
        builder.appendIfPresent(Optional.empty());
        builder.appendIfPresent(Optional.of("optional"));
        builder.appendAndFormatIfPresent(Optional.of("optional"), text -> text.toUpperCase());
        builder.appendIfPresent("optional", Optional.ofNullable(null));
        builder.appendIfPresent("optional", Optional.of("optional"));
        builder.appendAndFormatIfPresent("optional", Optional.of("optional"), text -> text.toUpperCase());

        assertEquals(expected.toString(), builder.build());
    }

    /**
     * Test method for {@link ToStringBuilder#build()}.
     */
    @Test
    public void testToStringStyleJSONQuotedBuild() {
        StringBuilder expected = new StringBuilder("{\"test\":");
        expected.append("{");
        expected.append("\"java.awt.Color[r=0,g=0,b=0]\"");
        expected.append(",");
        expected.append("\"0\"");
        expected.append(",");
        expected.append("\"blue\":\"java.awt.Color[r=0,g=0,b=255]\"");
        expected.append(",");
        expected.append("\"value\":\"").append(String.format("%,.3f", 120156.569)).append("\"");
        expected.append(",");
        expected.append("\"optional\"");
        expected.append(",");
        expected.append("\"OPTIONAL\"");
        expected.append(",");
        expected.append("\"optional\":\"optional\"");
        expected.append(",");
        expected.append("\"optional\":\"OPTIONAL\"");
        expected.append("}}");

        ToStringBuilder builder = new ToStringBuilder("test", ToStringStyles.JSON_QUOTED);
        builder.append(Color.BLACK);
        builder.appendAndFormat(Color.BLACK, color -> String.valueOf(color.getBlue()));
        builder.append("blue", Color.BLUE);
        builder.appendAndFormat("value", 120_156.568_9, ToStringBuilder.NUMBER_FORMATTER);
        builder.appendIfPresent(Optional.empty());
        builder.appendIfPresent(Optional.of("optional"));
        builder.appendAndFormatIfPresent(Optional.of("optional"), text -> text.toUpperCase());
        builder.appendIfPresent("optional", Optional.ofNullable(null));
        builder.appendIfPresent("optional", Optional.of("optional"));
        builder.appendAndFormatIfPresent("optional", Optional.of("optional"), text -> text.toUpperCase());

        assertEquals(expected.toString(), builder.build());
    }

    /**
     * Test method for {@link ToStringBuilder#build()}.
     */
    @Test
    public void testToStringStyleReadableBuild() {
        StringBuilder expected = new StringBuilder("test = ");
        expected.append("\n[");
        expected.append("'java.awt.Color[r=0,g=0,b=0]'");
        expected.append(",\n");
        expected.append("'0'");
        expected.append(",\n");
        expected.append("'blue' = 'java.awt.Color[r=0,g=0,b=255]'");
        expected.append(",\n");
        expected.append("'value' = '").append(String.format("%,.3f", 120_156.569)).append("'");
        expected.append(",\n");
        expected.append("'optional'");
        expected.append(",\n");
        expected.append("'OPTIONAL'");
        expected.append(",\n");
        expected.append("'optional' = 'optional'");
        expected.append(",\n");
        expected.append("'optional' = 'OPTIONAL'");
        expected.append("]");

        ToStringBuilder builder = new ToStringBuilder("test", ToStringStyles.READABLE);
        builder.append(Color.BLACK);
        builder.appendAndFormat(Color.BLACK, color -> String.valueOf(color.getBlue()));
        builder.append("blue", Color.BLUE);
        builder.appendAndFormat("value", 120_156.568_9, ToStringBuilder.NUMBER_FORMATTER);
        builder.appendIfPresent(Optional.empty());
        builder.appendIfPresent(Optional.of("optional"));
        builder.appendAndFormatIfPresent(Optional.of("optional"), text -> text.toUpperCase());
        builder.appendIfPresent("optional", Optional.ofNullable(null));
        builder.appendIfPresent("optional", Optional.of("optional"));
        builder.appendAndFormatIfPresent("optional", Optional.of("optional"), text -> text.toUpperCase());

        assertEquals(expected.toString(), builder.build());
        assertEquals(expected.toString(), builder.toString());
    }

    /**
     * Test method for {@link ToStringBuilder#build()}.
     */
    @Test
    public void testToStringStyleParenthesisBuild() {
        StringBuilder expected = new StringBuilder("(test:");
        expected.append("(");
        expected.append("java.awt.Color[r=0,g=0,b=0]");
        expected.append(",");
        expected.append("0");
        expected.append(",");
        expected.append("blue:java.awt.Color[r=0,g=0,b=255]");
        expected.append(",");
        expected.append("value:").append(String.format("%,.3f", 120156.569)).append("");
        expected.append(",");
        expected.append("optional");
        expected.append(",");
        expected.append("OPTIONAL");
        expected.append(",");
        expected.append("optional:optional");
        expected.append(",");
        expected.append("optional:OPTIONAL");
        expected.append("))");

        ToStringBuilder builder = new ToStringBuilder("test", ToStringStyles.PARENTHESIS);
        builder.append(Color.BLACK);
        builder.appendAndFormat(Color.BLACK, color -> String.valueOf(color.getBlue()));
        builder.append("blue", Color.BLUE);
        builder.appendAndFormat("value", 120_156.568_9, ToStringBuilder.NUMBER_FORMATTER);
        builder.appendIfPresent(Optional.empty());
        builder.appendIfPresent(Optional.of("optional"));
        builder.appendAndFormatIfPresent(Optional.of("optional"), text -> text.toUpperCase());
        builder.appendIfPresent("optional", Optional.ofNullable(null));
        builder.appendIfPresent("optional", Optional.of("optional"));
        builder.appendAndFormatIfPresent("optional", Optional.of("optional"), text -> text.toUpperCase());

        assertEquals(expected.toString(), builder.build());
        assertEquals(expected.toString(), builder.toString());
    }

    /**
     * Test method for {@link ToStringBuilder#build()}.
     */
    @Test
    public void testBuild() {

        // My style

        ToStringBuilder builder = new ToStringBuilder(MyStyle::new);

        assertEquals("", builder.build());

        // Class

        builder = new ToStringBuilder(ToStringBuilderTest.class);

        assertEquals(ToStringBuilderTest.class.getCanonicalName() + "[]", builder.build());

        // Nothing

        builder = new ToStringBuilder();

        assertEquals("[]", builder.build());

        // Nothing but with style ;)

        builder = new ToStringBuilder(ToStringStyles.JSON);

        assertEquals("{}", builder.build());

        // Class with specified style

        builder = new ToStringBuilder(ToStringBuilderTest.class, ToStringStyles.DEFAULT);

        assertEquals(ToStringBuilderTest.class.getCanonicalName() + "[]", builder.build());

        // Object instance

        builder = new ToStringBuilder(Color.BLACK);

        assertEquals(Color.class.getCanonicalName() + "[]", builder.build());

        // null object

        try {
            builder = new ToStringBuilder(null, ToStringStyles.DEFAULT);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    private class MyStyle extends AbstractToStringStyle {

        /**
         * serialVersionUID
         */
        private static final long serialVersionUID = -3706414639327763525L;

        @Override
        protected String getStart() {
            return EMPTY;
        }

        @Override
        protected String getTitleStart() {
            return EMPTY;
        }

        @Override
        protected String getTitleEnd() {
            return EMPTY;
        }

        @Override
        protected String getTitleSeparator() {
            return EMPTY;
        }

        @Override
        protected String getPropertiesStart() {
            return EMPTY;
        }

        @Override
        protected String getKeyStart() {
            return EMPTY;
        }

        @Override
        protected String getKeyEnd() {
            return EMPTY;
        }

        @Override
        protected String getPropertySeparator() {
            return EMPTY;
        }

        @Override
        protected String getValueStart() {
            return EMPTY;
        }

        @Override
        protected String getValueEnd() {
            return EMPTY;
        }

        @Override
        protected String getPropertiesSeparator() {
            return EMPTY;
        }

        @Override
        protected String getPropertiesEnd() {
            return EMPTY;
        }

        @Override
        protected String getEnd() {
            return EMPTY;
        }
    }
}
