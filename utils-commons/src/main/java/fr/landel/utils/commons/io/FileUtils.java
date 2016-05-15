/*
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Arrays;

import org.apache.commons.lang3.ArrayUtils;

import fr.landel.utils.commons.asserts.AssertUtils;

/**
 * This class is used to read and write files.
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class FileUtils {

    private static final int BUFFER_SIZE = 10240;
    private static final byte[] BUFFER = new byte[BUFFER_SIZE];

    /**
     * Constructor.
     *
     */
    private FileUtils() {
    }

    /**
     * Get the content of a file (charset used: US_ASCII).
     * 
     * @param path
     *            The path of the file
     * @return The buffered content
     * @throws IOException
     *             Exception thrown if problems occurs during reading
     */
    public static StringBuilder getFileContent(final String path) throws IOException {
        return getFileContent(path, EncodingUtils.CHARSET_US_ASCII);
    }

    /**
     * Get the content of a file (charset used: US_ASCII).
     * 
     * @param file
     *            The file
     * @return The buffered content
     * @throws IOException
     *             Exception thrown if problems occurs during reading
     */
    public static StringBuilder getFileContent(final File file) throws IOException {
        return getFileContent(file, EncodingUtils.CHARSET_US_ASCII);
    }

    /**
     * Get the content of a file.
     * 
     * @param path
     *            The path of the file
     * @param charset
     *            The file charset
     * @return The buffered content or {@code null} if path is {@code null}
     * @throws IOException
     *             Exception thrown if problems occurs during reading
     */
    public static StringBuilder getFileContent(final String path, final Charset charset) throws IOException {
        if (path != null) {
            return FileUtils.getFileContent(new File(path), charset);
        }
        return null;
    }

    /**
     * Get the content of a file.
     * 
     * @param file
     *            The file
     * @param charset
     *            The file charset
     * @return The buffered content
     * @throws IOException
     *             Exception thrown if problems occurs during reading
     */
    public static StringBuilder getFileContent(final File file, final Charset charset) throws IOException {
        final StringBuilder buffer;

        final BufferedInputStream bis = StreamUtils.createBufferedInputStream(file);

        buffer = getFileContent(bis, charset);

        CloseableManager.close(file);

        return buffer;
    }

    /**
     * Get the content of a input stream (charset used: US_ASCII).
     * 
     * @param inputStream
     *            The input stream
     * @return The buffered content
     * @throws IOException
     *             Exception thrown if problems occurs during reading
     */
    public static StringBuilder getFileContent(final InputStream inputStream) throws IOException {
        return FileUtils.getFileContent(inputStream, EncodingUtils.CHARSET_US_ASCII);
    }

    /**
     * Get the content of a input stream.
     * 
     * @param inputStream
     *            The input stream
     * @param charset
     *            The file charset
     * @return The buffered content
     * @throws IOException
     *             Exception thrown if problems occurs during reading
     */
    public static StringBuilder getFileContent(final InputStream inputStream, final Charset charset) throws IOException {
        int bufferReadSize;
        final StringBuilder buffer = new StringBuilder();

        while ((bufferReadSize = inputStream.read(BUFFER, 0, BUFFER_SIZE)) >= 0) {
            buffer.append(new String(BUFFER, 0, bufferReadSize, charset));
        }

        return buffer;
    }

    /**
     * Get the content of a file from class loader (from classpath root).
     * 
     * @param path
     *            The path
     * @param charset
     *            The file charset
     * @param classLoader
     *            The class loader (advice: use a Class in the same JAR of the
     *            file to load), if null use the class loader of the current
     *            thread
     * @return The buffered content
     * @throws IOException
     *             Exception thrown if problems occurs during reading
     */
    public static StringBuilder getFileContent(final String path, final Charset charset, final ClassLoader classLoader) throws IOException {
        AssertUtils.isNotBlank(path, "The 'path' parameter cannot be null or blank");
        AssertUtils.isNotNull(charset, "The 'charset' parameter cannot be null");

        ClassLoader loader = classLoader;
        int bufferReadSize;
        final StringBuilder buffer = new StringBuilder();

        if (loader == null) {
            loader = Thread.currentThread().getContextClassLoader();
        }

        try (InputStream is = loader.getResourceAsStream(path)) {
            while ((bufferReadSize = is.read(BUFFER, 0, BUFFER_SIZE)) >= 0) {
                buffer.append(new String(BUFFER, 0, bufferReadSize, charset));
            }
        }

        return buffer;
    }

    /**
     * Write the content of the buffer into a file and create intermediate
     * directories if necessary.
     * 
     * @param buffer
     *            the buffer
     * @param file
     *            The file
     * @param charset
     *            The charset
     * @throws IOException
     *             Exception thrown if problems occurs during writing
     */
    public static void writeFileContent(final StringBuilder buffer, final File file, final Charset charset) throws IOException {
        if (buffer != null && file != null && (file.getParentFile().isDirectory() || file.getParentFile().mkdirs())) {

            final BufferedOutputStream bos = StreamUtils.createBufferedOutputStream(file);

            bos.write(buffer.toString().getBytes(charset));

            CloseableManager.close(file);

        }
    }

    /**
     * Write the content of the buffer into a file and create intermediate
     * directories if necessary.
     * 
     * @param buffer
     *            the buffer
     * @param path
     *            The path of the file
     * @param charset
     *            The charset
     * @throws IOException
     *             Exception thrown if problems occurs during writing
     */
    public static void writeFileContent(final StringBuilder buffer, final String path, final Charset charset) throws IOException {
        if (path != null) {
            writeFileContent(buffer, new File(path), charset);
        }
    }

    /**
     * Write the content of the input stream into a file and create intermediate
     * directories if necessary.
     * 
     * @param inputStream
     *            The input stream
     * @param path
     *            The path of the file
     * @throws IOException
     *             Exception thrown if problems occurs during reading
     */
    public static void writeFileContent(final InputStream inputStream, final String path) throws IOException {
        final BufferedOutputStream bos = StreamUtils.createBufferedOutputStream(path);

        writeStream(inputStream, bos);

        CloseableManager.close(path);
    }

    /**
     * Write the content of the input stream into a file and create intermediate
     * directories if necessary.
     * 
     * @param inputStream
     *            The input stream
     * @param file
     *            The file
     * @throws IOException
     *             Exception thrown if problems occurs during reading
     */
    public static void writeFileContent(final InputStream inputStream, final File file) throws IOException {
        final BufferedOutputStream bos = StreamUtils.createBufferedOutputStream(file);

        writeStream(inputStream, bos);

        CloseableManager.close(file);
    }

    /**
     * Write a stream content into another.
     * 
     * @param inputStream
     *            The input stream
     * @param outputStream
     *            The output stream
     * @throws IOException
     *             thrown if problems occurs during reading
     */
    public static void writeStream(final InputStream inputStream, final OutputStream outputStream) throws IOException {
        int bufferReadSize;

        while ((bufferReadSize = inputStream.read(BUFFER, 0, BUFFER_SIZE)) >= 0) {
            outputStream.write(BUFFER, 0, bufferReadSize);
        }

        outputStream.flush();
    }

    /**
     * Compare two files
     * 
     * @param path1
     *            The path of the first file
     * @param path2
     *            The path of the second file
     * @return true, if files match
     * @throws IllegalArgumentException
     *             If parameters are null or not files
     */
    public static boolean isEqual(final String path1, final String path2) {
        if (path1 != null && path2 != null) {
            return FileUtils.isEqual(new File(path1), new File(path2));
        }
        throw new IllegalArgumentException("The paths paramater are invalid");
    }

    /**
     * Compare two files
     * 
     * @param file1
     *            The first file
     * @param file2
     *            The second file
     * @return true, if files match
     * @throws IllegalArgumentException
     *             If parameters are null or not files
     */
    public static boolean isEqual(final File file1, final File file2) {
        if (file1 == null || !file1.isFile()) {
            throw new IllegalArgumentException("The first file isn't valid");
        } else if (file2 == null || !file2.isFile()) {
            throw new IllegalArgumentException("The second file isn't valid");
        }

        if (file1.length() != file2.length()) {
            return false;
        } else if (file1.length() == 0) {
            return true;
        }

        return FileUtils.isIdentical(file1, file2);
    }

    private static boolean isIdentical(final File file1, final File file2) {
        boolean result = true;

        try (BufferedInputStream bis1 = new BufferedInputStream(new FileInputStream(file1))) {
            try (BufferedInputStream bis2 = new BufferedInputStream(new FileInputStream(file2))) {
                final byte[] buffer1 = new byte[BUFFER_SIZE];
                final byte[] buffer2 = new byte[BUFFER_SIZE];

                while (bis1.read(buffer1, 0, BUFFER_SIZE) > 0 && bis2.read(buffer2, 0, BUFFER_SIZE) > 0 && result) {
                    if (!ArrayUtils.isSameLength(buffer1, buffer2) || !Arrays.equals(buffer1, buffer2)) {
                        result = false;
                    }
                }
            }
        } catch (IOException e) {
            result = false;
        }

        return result;
    }
}
