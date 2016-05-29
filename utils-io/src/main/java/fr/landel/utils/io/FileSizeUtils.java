/*
 * #%L
 * utils-io
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.io;

import java.io.File;
import java.text.NumberFormat;
import java.util.Collections;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import fr.landel.utils.commons.EnumChar;

import java.util.TreeMap;

/**
 * This class contains the list of file sizes
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class FileSizeUtils {

    /**
     * Octets in kibioctets (kilo)
     */
    public static final long KIO = 1024;

    /**
     * Octets in mebioctets (mega)
     */
    public static final long MIO = KIO * KIO;

    /**
     * Octets in gibioctets (giba)
     */
    public static final long GIO = KIO * MIO;

    /**
     * Octets in tebioctets (tera)
     */
    public static final long TIO = KIO * GIO;

    /**
     * Octets in pebioctets (peta)
     */
    public static final long PIO = KIO * TIO;

    /**
     * Octets in exabioctets (exa)
     */
    public static final long EIO = KIO * PIO;

    /**
     * The map of sizes
     */
    public static final Map<Long, String> SIZE_UNITIES;

    static {
        Map<Long, String> map = new TreeMap<>();

        map.put(0L, "Octet");
        map.put(KIO, "Kio");
        map.put(MIO, "Mio");
        map.put(GIO, "Gio");
        map.put(TIO, "Tio");
        map.put(PIO, "Pio");
        map.put(EIO, "Eio");

        SIZE_UNITIES = Collections.unmodifiableMap(map);
    }

    /**
     * Constructor.
     *
     */
    private FileSizeUtils() {
    }

    /**
     * Format a file size (use current locale).
     * 
     * @param size
     *            The file size
     * @return The size formatted
     */
    public static String formatSize(final long size) {
        return formatSize(size, null);
    }

    /**
     * Format a file size.
     * 
     * @param size
     *            The file size
     * @param locale
     *            The locale (can be null)
     * @return The size formatted
     */
    public static String formatSize(final long size, final Locale locale) {
        double unitySize;
        final NumberFormat numberFormatter;
        if (locale != null) {
            numberFormatter = NumberFormat.getNumberInstance(locale);
        } else {
            numberFormatter = NumberFormat.getNumberInstance();
        }

        final Map<Long, String> mapReversed = new TreeMap<>(Collections.reverseOrder());
        mapReversed.putAll(SIZE_UNITIES);

        for (Entry<Long, String> entry : mapReversed.entrySet()) {
            if (size >= entry.getKey() && entry.getKey() > 0) {
                unitySize = (double) size / entry.getKey();
                return numberFormatter.format(unitySize) + EnumChar.SPACE.getUnicode() + entry.getValue();
            }
        }
        if (size > 1) {
            return size + EnumChar.SPACE.getUnicode() + mapReversed.get(0L) + "s";
        }
        return size + EnumChar.SPACE.getUnicode() + mapReversed.get(0L);
    }

    /**
     * Get the size of a file or a directory (doesn't support symbolic links).
     * 
     * @param path
     *            The path
     * @return the size (0, if the path doesn't exist)
     */
    public static long getSize(final String path) {
        long size = 0;
        if (path != null) {
            size = getSize(new File(path));
        }
        return size;
    }

    /**
     * Get the size of a file or a directory (doesn't support symbolic links).
     * 
     * @param file
     *            The file (file or directory)
     * @return The size (0, if not a file)
     */
    public static long getSize(final File file) {
        long size = 0;
        if (file != null) {
            if (file.isFile()) {
                size = file.length();
            } else if (file.isDirectory()) {
                File[] files = file.listFiles();
                if (files != null) {
                    for (File f : files) {
                        size += getSize(f);
                    }
                }
            }
        }
        return size;
    }
}
