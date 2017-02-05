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
package fr.landel.utils.commons;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A basic class to manage Maven version in comparator. This class allows to
 * compare version like Maven but also manages other formats (ex: 1.0.5.3.4.41 ;
 * it's managed by Maven as a {@link String} not by this class).
 *
 * @since Nov 15, 2016
 * @author Gilles
 *
 */
public class Version implements Comparable<Version> {
    private static final Pattern VERSION_PATTERN = Pattern.compile("([\\d\\w]+)");
    private static final String SNAPSHOT = "SNAPSHOT";

    private final String version;
    private final String[] array;

    public Version(final String version) {
        Objects.requireNonNull(version, "Version parameter cannot be null");

        this.version = version;
        this.array = this.getArray();
    }

    public String getVersion() {
        return this.version;
    }

    @Override
    public int compareTo(final Version v2) {
        String[] a1 = this.array;
        String[] a2 = v2.array;

        for (int i = 0; i < a1.length || i < a2.length; ++i) {
            if (i < a1.length && i < a2.length) {
                String group1 = a1[i];
                String group2 = a2[i];

                if (!group1.equals(group2)) {
                    boolean digit1 = NumberUtils.isDigits(group1);
                    boolean digit2 = NumberUtils.isDigits(group2);
                    boolean isSnapshot1 = !digit1 && SNAPSHOT.equals(group1);
                    boolean isSnapshot2 = !digit2 && SNAPSHOT.equals(group2);

                    if (digit1 && digit2) {
                        return Integer.compare(Integer.parseInt(group1), Integer.parseInt(group2));
                    } else if (digit1) {
                        if (!isSnapshot2 && Integer.parseInt(group1) == 0) {
                            return -1;
                        } else {
                            return 1;
                        }
                    } else if (digit2) {
                        if (!isSnapshot1 && Integer.parseInt(group2) == 0) {
                            return 1;
                        } else {
                            return -1;
                        }
                    } else if (isSnapshot1) {
                        return -1;
                    } else if (isSnapshot2) {
                        return 1;
                    } else {
                        return group1.compareTo(group2);
                    }
                }
            } else if (i < a1.length) {
                String group1 = a1[i];

                if (SNAPSHOT.equals(group1)) {
                    return -1;
                } else if (NumberUtils.isDigits(group1)) {
                    if (Integer.parseInt(group1) == 0) {
                        return 0;
                    }
                } else {
                    return -1;
                }
                return 1;
            } else {
                String group2 = a2[i];

                if (SNAPSHOT.equals(group2)) {
                    return 1;
                } else if (NumberUtils.isDigits(group2)) {
                    if (Integer.parseInt(group2) == 0) {
                        return 0;
                    }
                } else {
                    return 1;
                }
                return -1;
            }
        }

        return 0;
    }

    private String[] getArray() {
        final Matcher matcher = VERSION_PATTERN.matcher(this.version);
        String[] array = new String[this.version.length()];
        int i = 0;
        while (matcher.find()) {
            array[i++] = matcher.group(1);
        }
        return ArrayUtils.subarray(array, 0, i);
    }

    @Override
    public String toString() {
        return this.getVersion();
    }
}
