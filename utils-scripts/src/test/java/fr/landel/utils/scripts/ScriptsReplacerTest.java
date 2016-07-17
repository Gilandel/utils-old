/*
 * #%L
 * utils-scripts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.scripts;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Check scripts replacer
 *
 * @since 1 déc. 2015
 * @author Gilles
 *
 */
public class ScriptsReplacerTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(ScriptsReplacerTest.class);

    /**
     * Test replacer
     */
    @Test
    public void replaceTest() {
        StringBuilder sb;
        final Map<String, String> replacements = new HashMap<>();
        final ScriptsReplacer replacer = new ScriptsReplacer();

        replacements.put("var.iable", "value");
        replacements.put("var1", "test");
        replacements.put("var2", "d''ata");
        replacements.put("var3", "super");
        replacements.put("var4", "toto");

        final Map<String, String> inputs = new LinkedHashMap<>();

        // simple
        inputs.put("{var.iable}", "value");
        inputs.put("   {var.iable}", "   value");
        inputs.put("   {var.iable}  ", "   value  ");
        inputs.put("   { var.iable  }", "   value");
        inputs.put("   { var.iable  }  {var2}", "   value  d''ata");

        // condition
        inputs.put("{var.iable??var.iable}", "var.iable");
        inputs.put("{var.iable??var.iable::default}", "var.iable");
        inputs.put(" {var.iable??ok}  ", " ok  ");
        inputs.put("{var.iable  ??  ok ::  ko }", "  ok ");
        inputs.put("{var.iable::default}", "value");
        inputs.put("{  var.iable  ??  ok ::  ko }", "  ok ");
        inputs.put("{var.iable??{var.iable}}", "value");
        inputs.put("{var.iable??{var.iable::ko}", "{var.iable??value");
        inputs.put("{var.iable??var.iable}}", "var.iable}");
        inputs.put("{var.iable?? ok='{var.iable}{var2}' ::default}", " ok='valued''ata' ");
        inputs.put("{var.iable&&var2::default}", "valued''ata");
        inputs.put("{var.iable&&var2?? ok='{var.iable}{var2}' ::default}", " ok='valued''ata' ");
        inputs.put("{var.iable&&!var2?? ok='{var.iable}{var2}' ::default}", "default");
        inputs.put("{var.iable&&!var2}", "");
        inputs.put("{var.iable&&!var2::default}", "default");
        inputs.put("{var.iable&& ! var2::default}", "default");
        inputs.put("{var.iable||!var2::default}", "value");
        inputs.put("{!var.iable||var2::default}", "d''ata");
        inputs.put("{var.iable&&(var2||unknown)&&(var3||unknown)::default}", "d''atasupervalue");

        // condition + variable inconnu
        inputs.put("{unknown}", "");
        inputs.put("{unknown::default}", "default");
        inputs.put("{!unknown::default}", "");
        inputs.put("{!unknown??value::default}", "value");
        inputs.put("{var.iable?? ok='{var.iable}{var2}{unknown}' ::default}", " ok='valued''ata' ");
        inputs.put("{unknown?? ok='{var.iable}{var2}'}", "");
        inputs.put("{unknown?? ok='{var.iable}{var2}' ::default}", "default");
        inputs.put("{!unknown?? ok='{var.iable}{var2}' ::default}", " ok='valued''ata' ");
        inputs.put("{var.iable&&unknown?? ok='{var.iable}{var2}' ::default}", "default");
        inputs.put("{var.iable&&!unknown?? ok='{var.iable}{var2}' ::default}", " ok='valued''ata' ");
        inputs.put("{var.iable||!unknown?? ok='{var.iable}{var2}' ::default}", " ok='valued''ata' ");
        inputs.put("{var.iable||unknown?? ok='{var.iable}{var2}' ::default}", " ok='valued''ata' ");
        inputs.put("{!var.iable||!unknown?? ok='{var.iable}{var2}' ::default}", " ok='valued''ata' ");
        inputs.put("{!var.iable||unknown?? ok='{var.iable}{var2}' ::default}", "default");
        inputs.put("{!var.iable||(unknown&&var3||(var2&&!unknown))?? ok='{var.iable}{var2}' ::default}", " ok='valued''ata' ");
        inputs.put("{!var.iable||(unknown&&var3||(var2&&unknown))?? ok='{var.iable}{var2}' ::default}", "default");

        inputs.put("{(var1&&var2)||(var3&&var4)??{var3} {var4}::default}", "super toto");
        inputs.put("{(var1&&var2)||(var3&&!var4)??{var3} {var4}::default}", "super toto");
        inputs.put("{(var1&&!var2)||(var3&&!var4)??{var3} {var4}::default}", "default");

        inputs.put("{var.iable&&!unknown??{var.iable&&!unknown?? ok='{var.iable}{var2}' ::default}::default}", " ok='valued''ata' ");
        inputs.put("{var.iable&&unknown??{var3}::{var.iable&&var3??{var4}::default}}", "toto");
        inputs.put("{var.iable&&!unknown??{var.iable&&unknown?? ok='{var.iable}{var2}' ::default}{var2}::default}", "defaultd''ata");
        inputs.put("{var.iable&&!unknown??\r\n{var.iable&&unknown?? \r\nok='{var.iable}{var2}' ::default}{var2}::default}",
                "\r\ndefaultd''ata");

        int inputWithoutExceptions = inputs.size();

        // exceptions
        inputs.put("   { var.iable", "   { var.iable");
        inputs.put("   { var.iable ", "   { var.iable ");

        int i = 0;
        for (Entry<String, String> input : inputs.entrySet()) {
            sb = new StringBuilder(input.getKey());

            if (i < inputWithoutExceptions) {
                replacer.replace(sb, replacements);

                assertEquals("input: " + input.getKey(), input.getValue(), sb.toString());
            } else {
                try {
                    replacer.replace(sb, replacements);

                    fail("Has to throw exception: " + sb.toString());
                } catch (IllegalArgumentException e) {
                    LOGGER.info("Expected exception", e);
                }
            }

            i++;
        }
    }

    /**
     * Test replacer 2
     */
    @Test
    public void replaceTest2() {
        StringBuilder sb;
        final Map<String, String> replacements = new HashMap<>();
        final ScriptsReplacer replacer = new ScriptsReplacer();
        final Map<String, String> inputs = new LinkedHashMap<>();

        replacements.put("var2", "dat'a");
        inputs.put("{var.iable}", "value");

        try {
            for (Entry<String, String> input : inputs.entrySet()) {
                sb = new StringBuilder(input.getKey());

                replacer.replace(sb, replacements);

                fail("Has to throw exception: " + sb.toString());
            }
        } catch (IllegalArgumentException e) {
            LOGGER.info("Expected exception", e);
        }
    }

    /**
     * Test replacer 3
     */
    @Test
    public void replaceTest3() {
        StringBuilder sb;
        final Map<String, String> replacements = new HashMap<>();
        final ScriptsReplacer replacer = new ScriptsReplacer();
        final Map<String, String> inputs = new LinkedHashMap<>();

        replacements.put("var2", "'dat'a");
        inputs.put("{var.iable}", "value");

        try {
            for (Entry<String, String> input : inputs.entrySet()) {
                sb = new StringBuilder(input.getKey());

                replacer.replace(sb, replacements);

                fail("Has to throw exception: " + sb.toString());
            }
        } catch (IllegalArgumentException e) {
            LOGGER.info("Expected exception", e);
        }
    }
}