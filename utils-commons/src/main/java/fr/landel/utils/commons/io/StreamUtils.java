/*
 * #%L
 * gl-utils-commons
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
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;

/**
 * Utility class to manage streams.
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class StreamUtils {

    /**
     * Default encoding
     */
    private static final String DEFAULT_ENCODING = EncodingUtils.ENCODING_UTF_8;

    /**
     * Error : file null
     */
    private static final String ERROR_FILE_PARAM_NULL = "The file parameter is null";
    /**
     * Error : filename null
     */
    private static final String ERROR_FILE_NAME_PARAM_NULL = "The fileName parameter is null";

    /**
     * Constructor.
     * 
     */
    private StreamUtils() {
    }

    /**
     * Create a buffered reader from the specified path
     * 
     * @param fileName
     *            The input path
     * @param encoding
     *            The encoding, if null: UTF-8 is used
     * @return The buffered reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized BufferedReader createBufferedReader(final String fileName, final String encoding)
            throws FileNotFoundException, UnsupportedEncodingException {
        if (fileName != null) {
            return createBufferedReader(new File(fileName), encoding);
        }
        throw new FileNotFoundException(ERROR_FILE_NAME_PARAM_NULL);
    }

    /**
     * Create a buffered reader from the specified file
     * 
     * @param file
     *            The input file
     * @param encoding
     *            The encoding, if null: UTF-8 is used
     * @return The buffered reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized BufferedReader createBufferedReader(final File file, final String encoding) throws FileNotFoundException,
            UnsupportedEncodingException {
        if (file != null) {
            final BufferedReader br = new BufferedReader(createInputStreamReader(file, encoding));
            CloseableManager.addCloseable(file, br);
            return br;
        }
        throw new FileNotFoundException(ERROR_FILE_PARAM_NULL);
    }

    /**
     * Create an input stream reader from the specified path
     * 
     * @param fileName
     *            The input path
     * @param encoding
     *            The encoding, if null: UTF-8 is used
     * @return The input stream reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized InputStreamReader createInputStreamReader(final String fileName, final String encoding)
            throws FileNotFoundException, UnsupportedEncodingException {
        if (fileName != null) {
            return createInputStreamReader(new File(fileName), encoding);
        }
        throw new FileNotFoundException(ERROR_FILE_NAME_PARAM_NULL);
    }

    /**
     * Create an input stream reader from the specified file
     * 
     * @param file
     *            The input file
     * @param encoding
     *            The encoding, if null: UTF-8 is used
     * @return The input stream reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized InputStreamReader createInputStreamReader(final File file, final String encoding)
            throws FileNotFoundException, UnsupportedEncodingException {
        final InputStreamReader isr;

        if (encoding != null) {
            isr = new InputStreamReader(createBufferedInputStream(file), encoding);
        } else {
            isr = new InputStreamReader(createBufferedInputStream(file), DEFAULT_ENCODING);
        }

        CloseableManager.addCloseable(file, isr);
        return isr;
    }

    /**
     * Create an input stream reader from the specified URL
     * 
     * @param url
     *            The input URL
     * @param encoding
     *            The encoding, if null: UTF-8 is used
     * @return The input stream reader
     * @throws IOException
     *             Thrown if file wasn't found or in case of opening URL stream
     */
    public static synchronized InputStreamReader createInputStreamReader(final URL url, final String encoding) throws IOException {
        final InputStreamReader isr;

        if (encoding != null) {
            isr = new InputStreamReader(createBufferedInputStream(url), encoding);
        } else {
            isr = new InputStreamReader(createBufferedInputStream(url), DEFAULT_ENCODING);
        }

        CloseableManager.addCloseable(url, isr);
        return isr;
    }

    /**
     * Create a buffered input stream from the specified file name
     * 
     * @param fileName
     *            The input file name
     * @return The buffered input stream
     * @throws FileNotFoundException
     *             Error thrown if file wasn't found
     */
    public static synchronized BufferedInputStream createBufferedInputStream(final String fileName) throws FileNotFoundException {
        if (fileName != null) {
            return createBufferedInputStream(new File(fileName));
        }
        throw new FileNotFoundException(ERROR_FILE_NAME_PARAM_NULL);
    }

    /**
     * Create a buffered input stream from the specified file
     * 
     * @param file
     *            The input file
     * @return The buffered input stream
     * @throws FileNotFoundException
     *             Error thrown if file wasn't found
     */
    public static synchronized BufferedInputStream createBufferedInputStream(final File file) throws FileNotFoundException {
        if (file != null) {
            final FileInputStream fis = new FileInputStream(file);
            CloseableManager.addCloseable(file, fis);

            final BufferedInputStream bis = new BufferedInputStream(fis);
            CloseableManager.addCloseable(file, bis);

            return bis;
        }
        throw new FileNotFoundException(ERROR_FILE_PARAM_NULL);
    }

    /**
     * Create a buffered input stream from the specified URL
     * 
     * @param url
     *            The input URL
     * @return The buffered input stream
     * @throws IOException
     *             Thrown if file wasn't found or in case of opening URL stream
     */
    public static synchronized BufferedInputStream createBufferedInputStream(final URL url) throws IOException {
        if (url != null) {
            final InputStream is = url.openStream();
            CloseableManager.addCloseable(url, is);

            final BufferedInputStream bis = new BufferedInputStream(is);
            CloseableManager.addCloseable(url, bis);

            return bis;
        }
        throw new FileNotFoundException(ERROR_FILE_PARAM_NULL);
    }

    /**
     * Create an output stream writer from the specified path
     * 
     * @param fileName
     *            The output path
     * @return The output stream reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized OutputStreamWriter createBufferedWriter(final String fileName) throws FileNotFoundException,
            UnsupportedEncodingException {
        return createBufferedWriter(fileName, null, false);
    }

    /**
     * Create an output stream writer from the specified path
     * 
     * @param fileName
     *            The output path
     * @param encoding
     *            The encoding, if null: UTF-8 is used
     * @return The output stream reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized OutputStreamWriter createBufferedWriter(final String fileName, final String encoding)
            throws FileNotFoundException, UnsupportedEncodingException {
        return createBufferedWriter(fileName, encoding, false);
    }

    /**
     * Create an output stream writer from the specified path
     * 
     * @param fileName
     *            The output path
     * @param append
     *            if true, then bytes will be written to the end of the file
     *            rather than the beginning
     * @return The output stream reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized OutputStreamWriter createBufferedWriter(final String fileName, final boolean append)
            throws FileNotFoundException, UnsupportedEncodingException {
        return createBufferedWriter(fileName, null, append);
    }

    /**
     * Create an output stream writer from the specified file
     * 
     * @param file
     *            The output file
     * @return The output stream reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized OutputStreamWriter createBufferedWriter(final File file) throws FileNotFoundException,
            UnsupportedEncodingException {
        return createBufferedWriter(file, null, false);
    }

    /**
     * Create an output stream writer from the specified file
     * 
     * @param file
     *            The output file
     * @param encoding
     *            The encoding, if null: UTF-8 is used
     * @return The output stream reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized OutputStreamWriter createBufferedWriter(final File file, final String encoding)
            throws FileNotFoundException, UnsupportedEncodingException {
        return createBufferedWriter(file, encoding, false);
    }

    /**
     * Create an output stream writer from the specified file
     * 
     * @param file
     *            The output file
     * @param append
     *            if true, then bytes will be written to the end of the file
     *            rather than the beginning
     * @return The output stream reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized OutputStreamWriter createBufferedWriter(final File file, final boolean append) throws FileNotFoundException,
            UnsupportedEncodingException {
        return createBufferedWriter(file, null, append);
    }

    /**
     * Create an output stream writer from the specified path
     * 
     * @param fileName
     *            The output path
     * @param encoding
     *            The encoding, if null: UTF-8 is used
     * @param append
     *            if true, then bytes will be written to the end of the file
     *            rather than the beginning
     * @return The output stream reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized OutputStreamWriter createBufferedWriter(final String fileName, final String encoding, final boolean append)
            throws FileNotFoundException, UnsupportedEncodingException {
        if (fileName != null) {
            return createBufferedWriter(new File(fileName), encoding, append);
        }
        throw new FileNotFoundException(ERROR_FILE_NAME_PARAM_NULL);
    }

    /**
     * Create an output stream writer from the specified file
     * 
     * @param file
     *            The output file
     * @param encoding
     *            The encoding, if null: UTF-8 is used
     * @param append
     *            if true, then bytes will be written to the end of the file
     *            rather than the beginning
     * @return The output stream reader
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown if encoding doesn't match
     */
    public static synchronized OutputStreamWriter createBufferedWriter(final File file, final String encoding, final boolean append)
            throws FileNotFoundException, UnsupportedEncodingException {

        final OutputStreamWriter osw;

        if (encoding != null) {
            osw = new OutputStreamWriter(createBufferedOutputStream(file, append), encoding);
        } else {
            osw = new OutputStreamWriter(createBufferedOutputStream(file, append), DEFAULT_ENCODING);
        }

        CloseableManager.addCloseable(file, osw);

        return osw;
    }

    /**
     * Create a buffered output stream from the specified path
     * 
     * @param fileName
     *            The output file name
     * @return The buffered output stream
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown on encoding problem
     */
    public static synchronized BufferedOutputStream createBufferedOutputStream(final String fileName) throws FileNotFoundException,
            UnsupportedEncodingException {
        return createBufferedOutputStream(fileName, false);
    }

    /**
     * Create a buffered output stream from the specified path
     * 
     * @param fileName
     *            The output file name
     * @param append
     *            if true, then bytes will be written to the end of the file
     *            rather than the beginning
     * @return The buffered output stream
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown on encoding problem
     */
    public static synchronized BufferedOutputStream createBufferedOutputStream(final String fileName, final boolean append)
            throws FileNotFoundException, UnsupportedEncodingException {
        if (fileName != null) {
            return createBufferedOutputStream(new File(fileName), append);
        }
        throw new FileNotFoundException(ERROR_FILE_NAME_PARAM_NULL);
    }

    /**
     * Create a buffered output stream from the specified file
     * 
     * @param file
     *            The output file
     * @return The buffered output stream
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown on encoding problem
     */
    public static synchronized BufferedOutputStream createBufferedOutputStream(final File file) throws FileNotFoundException,
            UnsupportedEncodingException {
        return createBufferedOutputStream(file, false);
    }

    /**
     * Create a buffered output stream from the specified file
     * 
     * @param file
     *            The output file
     * @param append
     *            if true, then bytes will be written to the end of the file
     *            rather than the beginning
     * @return The buffered output stream
     * @throws FileNotFoundException
     *             Error thrown if wasn't found
     * @throws UnsupportedEncodingException
     *             Error thrown on encoding problem
     */
    public static synchronized BufferedOutputStream createBufferedOutputStream(final File file, final boolean append)
            throws FileNotFoundException, UnsupportedEncodingException {
        if (file != null) {
            final FileOutputStream fos = new FileOutputStream(file, append);
            CloseableManager.addCloseable(file, fos);

            final BufferedOutputStream bos = new BufferedOutputStream(fos);
            CloseableManager.addCloseable(file, bos);

            return bos;
        }
        throw new FileNotFoundException(ERROR_FILE_PARAM_NULL);
    }
}
