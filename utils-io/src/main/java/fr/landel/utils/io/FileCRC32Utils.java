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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.CRC32;

/**
 * This class is used to get the CRC32 of a file or a directory.
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class FileCRC32Utils {

    private static final CRC32 CRC32 = new CRC32();

    private static final int BUFFER_SIZE = 10240;
    private static final byte[] BUFFER = new byte[BUFFER_SIZE];

    /**
     * Constructor.
     *
     */
    private FileCRC32Utils() {
    }

    /**
     * Get the CRC32 of a file or a directory.
     * 
     * @param path
     *            The path of the file or directory
     * @return The CRC 32 finger print
     * @throws IOException
     *             Exception thrown if problems occurs during accessing to the
     *             specified path
     */
    public static Long getCRC32(final String path) throws IOException {
        return getCRC32(path, (FilenameFilter) null);
    }

    /**
     * Get the CRC32 of a file or a directory.
     * 
     * @param file
     *            The file or directory
     * @return The CRC 32 finger print
     * @throws IOException
     *             Exception thrown if problems occurs during accessing to the
     *             specified path
     */
    public static Long getCRC32(final File file) throws IOException {
        return getCRC32(file, (FilenameFilter) null);
    }

    /**
     * Get the CRC32 of a file or a directory following a filter.
     * 
     * @param path
     *            The path of the file or directory
     * @param filter
     *            The filter to limit the check of a directory
     * @return The CRC 32 finger print
     * @throws IOException
     *             Exception thrown if problems occurs during accessing to the
     *             specified path
     */
    public static Long getCRC32(final String path, final FilenameFilter filter) throws IOException {
        return getCRC32(new File(path), filter);
    }

    /**
     * Get the CRC32 of a file or a directory following a filter.
     * 
     * @param path
     *            The path of the file or directory
     * @param filter
     *            The filter to limit the check of a directory
     * @return The CRC 32 finger print
     * @throws IOException
     *             Exception thrown if problems occurs during accessing to the
     *             specified path
     */
    public static Long getCRC32(final String path, final FileFilter filter) throws IOException {
        return getCRC32(new File(path), filter);
    }

    /**
     * Get the CRC32 of a file or a directory following a filter.
     * 
     * @param file
     *            The file or directory
     * @param filter
     *            The filter to limit the check of a directory
     * @return The CRC 32 finger print
     * @throws IOException
     *             Exception thrown if problems occurs during accessing to the
     *             specified path
     */
    public static Long getCRC32(final File file, final FileFilter filter) throws IOException {
        CRC32.reset();

        recurisiveCRC32(file, filter, null);

        return CRC32.getValue();
    }

    /**
     * Get the CRC32 of a file or a directory following a filter.
     * 
     * @param file
     *            The file or directory
     * @param filter
     *            The filter to limit the check of a directory
     * @return The CRC 32 finger print
     * @throws IOException
     *             Exception thrown if problems occurs during accessing to the
     *             specified path
     */
    public static Long getCRC32(final File file, final FilenameFilter filter) throws IOException {
        CRC32.reset();

        recurisiveCRC32(file, null, filter);

        return CRC32.getValue();
    }

    private static void recurisiveCRC32(final File file, final FileFilter fileFilter, final FilenameFilter filenameFilter)
            throws IOException {
        if (file.isFile()) {
            getCRC32File(file);
        } else if (file.isDirectory()) {
            File[] files = FileSystemUtils.listFiles(file, fileFilter, filenameFilter);
            if (files != null) {
                for (File subFile : files) {
                    recurisiveCRC32(subFile.getAbsoluteFile(), fileFilter, filenameFilter);
                }
            }
        }
    }

    /**
     * Get the CRC32 of a file (finally the stream is closed).
     * 
     * @param inputStream
     *            inputStream representing the file
     * @throws IOException
     *             Exception thrown if problems occurs during accessing to the
     *             specified path
     * @return The generated CRC32
     */
    public static Long getCRC32(final InputStream inputStream) throws IOException {
        CRC32.reset();

        int bufferReadSize;

        CloseableManager.addCloseable(inputStream.hashCode(), inputStream);

        while ((bufferReadSize = inputStream.read(BUFFER, 0, BUFFER_SIZE)) >= 0) {
            CRC32.update(BUFFER, 0, bufferReadSize);
        }

        CloseableManager.close(inputStream.hashCode());

        return CRC32.getValue();
    }

    /**
     * Get the CRC32 of a file.
     * 
     * @param file
     *            The path of the file
     * @throws IOException
     *             Exception thrown if problems occurs during accessing to the
     *             specified path
     */
    private static void getCRC32File(final File file) throws IOException {
        int bufferReadSize;

        // Internal: The CRC object isn't reset

        final BufferedInputStream bis = StreamUtils.createBufferedInputStream(file);

        while ((bufferReadSize = bis.read(BUFFER, 0, BUFFER_SIZE)) >= 0) {
            CRC32.update(BUFFER, 0, bufferReadSize);
        }

        CloseableManager.close(file);
    }
}
