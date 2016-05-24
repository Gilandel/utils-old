/*
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.query;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;

/**
 * 
 * Test class to check performance of this version of split based on original
 * JDK 1.4)
 *
 * @since 23 mai 2016
 * @author Gilles
 *
 */
public class ImprovedSplitMain {
    // private static final Logger LOGGER =
    // LoggerFactory.getLogger(ImprovedSplitMain.class);

    public static void main(String[] args) {

        long diff;
        long start;
        String res[] = new String[0];

        System.out.println(Arrays.asList(ImprovedSplitMain.split("test;2;dd;;h;", ';', true)));
        System.out.println(Arrays.asList(ImprovedSplitMain.split("test;2;dd;;h;", ';', false)));

        List<String> list = new ArrayList<>();
        for (int i = 0; i < 10_000_000; i++) {
            list.add(String.format("%d", Math.round(Math.random() * 1000)));
        }

        for (int j = 0; j < 5; j++) {
            start = System.currentTimeMillis();
            for (String test : list) {
                if (ImprovedSplitMain.indexOf(test, '1', 0) > -1) {
                    res = test.split("1");
                }
            }
            diff = System.currentTimeMillis() - start;

            System.out.println("index gilles: " + diff + ", length: " + res.length);

            start = System.currentTimeMillis();
            for (String test : list) {
                if (test.indexOf('1') > -1) {
                    res = test.split("1");
                }
            }
            diff = System.currentTimeMillis() - start;

            System.out.println("index: " + diff + ", length: " + res.length);

            start = System.currentTimeMillis();
            for (String test : list) {
                res = test.split("1");
            }
            diff = System.currentTimeMillis() - start;

            System.out.println("split: " + diff + ", length: " + res.length);

            start = System.currentTimeMillis();
            for (String test : list) {
                res = ImprovedSplitMain.split(test, '1', true);
            }
            diff = System.currentTimeMillis() - start;

            System.out.println("split worker: " + diff + ", length: " + res.length);
        }
    }

    /**
     * Performs the logic for the {@code split} and
     * {@code splitPreserveAllTokens} methods that do not return a maximum array
     * length.
     *
     * @param str
     *            the String to parse, may be {@code null}
     * @param separatorChar
     *            the separate character
     * @return an array of parsed Strings, {@code null} if null String input
     */
    private static String[] split(final String str, final char separatorChar, final boolean preserve) {
        // Performance tuned for 2.0 (JDK1.4)

        if (str == null) {
            return null;
        }
        final int len = str.length();
        if (len == 0) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }

        final List<String> list = new ArrayList<>();
        int off = 0;
        int next = 0;

        while ((next = ImprovedSplitMain.indexOf(str, separatorChar, off)) != -1) {
            if (preserve || next > off) {
                list.add(str.substring(off, next));
            }
            off = next + 1;
        }
        if (preserve || len > off) {
            list.add(str.substring(off, len));
        }
        return list.toArray(new String[list.size()]);
    }

    private static int indexOf(final String str, final char charSeparator, final int fromIndex) {
        for (int i = fromIndex; i < str.length(); i++) {
            if (str.charAt(i) == charSeparator) {
                return i;
            }
        }
        return -1;
    }
}
