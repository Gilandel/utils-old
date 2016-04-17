/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * 
 * This code is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons.io;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility class to manage closeables.
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class CloseableManager {

    private static final Map<Integer, List<Closeable>> CLOSEABLES_INTEGER = new HashMap<>();
    private static final Map<File, List<Closeable>> CLOSEABLES_FILE = new HashMap<>();
    private static final Map<Class<?>, List<Closeable>> CLOSEABLES_CLASS = new HashMap<>();
    private static final Map<String, List<Closeable>> CLOSEABLES_URL = new HashMap<>();

    /**
     * Error close : hascode
     */
    private static final String ERROR_CLOSEABLE_HASHCODE_NOT_CLOSEABLE = "closeable attached to the hashcode '%s' can't be closed";
    /**
     * Error close : file
     */
    private static final String ERROR_CLOSEABLE_FILE_NOT_CLOSEABLE = "closeable attached to the file '%s' can't be closed";
    /**
     * Error close : class
     */
    private static final String ERROR_CLOSEABLE_CLASS_NOT_CLOSEABLE = "closeable attached to the class '%s' can't be closed";
    /**
     * Error close: URL
     */
    private static final String ERROR_CLOSEABLE_URL_NOT_CLOSEABLE = "closeable attached to the url '%s' can't be closed";
    /**
     * Error close
     */
    private static final String ERROR_CLOSEABLE_NOT_CLOSEABLE = "closeable can't be closed";

    private static Logger logger = LoggerFactory.getLogger(CloseableManager.class.getName());

    /**
     * Constructor.
     * 
     */
    private CloseableManager() {
    }

    /**
     * Define the current logger.
     * 
     * @param logger
     *            The logger
     */
    public static synchronized void defineLogger(final Logger logger) {
        CloseableManager.logger = logger;
    }

    /**
     * Check if a closeable is already created.
     * 
     * @param clazz
     *            The associated class
     * @return true, if found
     */
    public static synchronized boolean isCloseable(final Class<?> clazz) {
        if (clazz != null) {
            return CLOSEABLES_CLASS.containsKey(clazz);
        }
        return false;
    }

    /**
     * Check if a closeable is already created.
     * 
     * @param hashcode
     *            The associated hashcode
     * @return true, if found
     */
    public static synchronized boolean isCloseable(final Integer hashcode) {
        if (hashcode != null) {
            return CLOSEABLES_INTEGER.containsKey(hashcode);
        }
        return false;
    }

    /**
     * Check if a closeable is already created.
     * 
     * @param fileName
     *            The associated filename
     * @return true, if found
     */
    public static synchronized boolean isCloseable(final String fileName) {
        if (fileName != null) {
            return CLOSEABLES_FILE.containsKey(new File(fileName));
        }
        return false;
    }

    /**
     * Check if a closeable is already created.
     * 
     * @param file
     *            The associated file
     * @return true, if found
     */
    public static synchronized boolean isCloseable(final File file) {
        if (file != null) {
            return CLOSEABLES_FILE.containsKey(file);
        }
        return false;
    }

    /**
     * Add closeable to the list associated to the path.
     * 
     * @param fileName
     *            The path to associated file to the closeable
     * @param closeable
     *            The closeable to be added
     */
    public static synchronized void addCloseable(final String fileName, final Closeable closeable) {
        if (fileName != null && closeable != null) {
            addCloseable(new File(fileName), closeable);
        }
    }

    /**
     * Add closeable to the list associated closeable to the hashcode.
     * 
     * @param hashcode
     *            The hashcode associated to the closeable
     * @param closeable
     *            The closeable to be added
     */
    public static synchronized void addCloseable(final Integer hashcode, final Closeable closeable) {
        if (hashcode != null && closeable != null) {
            if (!CLOSEABLES_INTEGER.containsKey(hashcode)) {
                CLOSEABLES_INTEGER.put(hashcode, new ArrayList<Closeable>());
            }
            CLOSEABLES_INTEGER.get(hashcode).add(closeable);
        }
    }

    /**
     * Add closeable to the list associated to the file.
     * 
     * @param file
     *            The associated file to the closeable
     * @param closeable
     *            The closeable to be added
     */
    public static synchronized void addCloseable(final File file, final Closeable closeable) {
        if (file != null && closeable != null) {
            if (!CLOSEABLES_FILE.containsKey(file)) {
                CLOSEABLES_FILE.put(file, new ArrayList<Closeable>());
            }
            CLOSEABLES_FILE.get(file).add(closeable);
        }
    }

    /**
     * Add closeable to the list associated to the url.
     * 
     * @param url
     *            The associated url to the closeable
     * @param closeable
     *            The closeable to be added
     */
    public static synchronized void addCloseable(final URL url, final Closeable closeable) {
        if (url != null && closeable != null) {
            String path = url.getPath();
            if (!CLOSEABLES_URL.containsKey(path)) {
                CLOSEABLES_URL.put(path, new ArrayList<Closeable>());
            }
            CLOSEABLES_URL.get(path).add(closeable);
        }
    }

    /**
     * Add closeable to the list associated to the class
     * 
     * @param clazz
     *            The class associated to the closeable
     * @param closeable
     *            The closeable to be added
     */
    public static synchronized void addCloseable(final Class<?> clazz, final Closeable closeable) {
        if (clazz != null && closeable != null) {
            if (!CLOSEABLES_CLASS.containsKey(clazz)) {
                CLOSEABLES_CLASS.put(clazz, new ArrayList<Closeable>());
            }
            CLOSEABLES_CLASS.get(clazz).add(closeable);
        }
    }

    /**
     * Reverse the order of the element of the list
     * 
     * @param closeables
     *            The list to be reversed
     * @return The reverse list
     */
    private static synchronized List<Closeable> reverseList(final List<Closeable> closeables) {
        final List<Closeable> reverseCloseables = new ArrayList<Closeable>(closeables);
        Collections.reverse(reverseCloseables);
        return reverseCloseables;
    }

    /**
     * Close all managed closeables associated with hashcodes
     */
    public static synchronized void closeAllHashcode() {
        for (Entry<Integer, List<Closeable>> entry : CLOSEABLES_INTEGER.entrySet()) {
            final List<Closeable> reversedCloseables = reverseList(entry.getValue());
            for (Closeable closeable : reversedCloseables) {
                try {
                    closeable.close();
                } catch (IOException e) {
                    logger.error(String.format(ERROR_CLOSEABLE_HASHCODE_NOT_CLOSEABLE, entry.getKey()), e);
                }
            }
            entry.getValue().clear();
        }
        CLOSEABLES_INTEGER.clear();
    }

    /**
     * Close all managed closeables associated with files
     */
    public static synchronized void closeAllFiles() {
        for (Entry<File, List<Closeable>> entry : CLOSEABLES_FILE.entrySet()) {
            final List<Closeable> reversedCloseables = reverseList(entry.getValue());
            for (Closeable closeable : reversedCloseables) {
                try {
                    closeable.close();
                } catch (IOException e) {
                    logger.error(String.format(ERROR_CLOSEABLE_FILE_NOT_CLOSEABLE, entry.getKey().getAbsolutePath()), e);
                }
            }
            entry.getValue().clear();
        }
        CLOSEABLES_FILE.clear();
    }

    /**
     * Close all managed closeables associated with classes
     */
    public static synchronized void closeAllClasses() {
        for (Entry<Class<?>, List<Closeable>> entry : CLOSEABLES_CLASS.entrySet()) {
            final List<Closeable> reversedCloseables = reverseList(entry.getValue());
            for (Closeable closeable : reversedCloseables) {
                try {
                    closeable.close();
                } catch (IOException e) {
                    logger.error(String.format(ERROR_CLOSEABLE_CLASS_NOT_CLOSEABLE, entry.getKey().getName()), e);
                }
            }
            entry.getValue().clear();
        }
        CLOSEABLES_CLASS.clear();
    }

    /**
     * Close all managed closeables associated with urls
     */
    public static synchronized void closeAllURLs() {
        for (Entry<String, List<Closeable>> entry : CLOSEABLES_URL.entrySet()) {
            final List<Closeable> reversedCloseables = reverseList(entry.getValue());
            for (Closeable closeable : reversedCloseables) {
                try {
                    closeable.close();
                } catch (IOException e) {
                    logger.error(String.format(ERROR_CLOSEABLE_URL_NOT_CLOSEABLE, entry.getKey()), e);
                }
            }
            entry.getValue().clear();
        }
        CLOSEABLES_URL.clear();
    }

    /**
     * Close all managed closeables
     */
    public static synchronized void closeAll() {
        closeAllHashcode();
        closeAllFiles();
        closeAllClasses();
        closeAllURLs();
    }

    /**
     * Close all managed closeables associated to the path
     * 
     * @param fileName
     *            The path of the associated file to the closeable
     */
    public static synchronized void close(final String fileName) {
        if (fileName != null) {
            close(new File(fileName));
        }
    }

    /**
     * Close all managed closeables associated to the file
     * 
     * @param file
     *            The associated file to the closeable
     */
    public static synchronized void close(final File file) {
        if (file != null && CLOSEABLES_FILE.containsKey(file)) {
            final List<Closeable> reversedCloseables = reverseList(CLOSEABLES_FILE.get(file));
            for (Closeable closeable : reversedCloseables) {
                try {
                    closeable.close();
                } catch (IOException e) {
                    logger.error(String.format(ERROR_CLOSEABLE_FILE_NOT_CLOSEABLE, file.getAbsolutePath()), e);
                }
            }
            CLOSEABLES_FILE.get(file).clear();
            CLOSEABLES_FILE.remove(file);
        }
    }

    /**
     * Close all managed closeables associated to the hashcode
     * 
     * @param hashcode
     *            The hashcode associated to the closeable
     */
    public static synchronized void close(final Integer hashcode) {
        if (hashcode != null && CLOSEABLES_INTEGER.containsKey(hashcode)) {
            final List<Closeable> reversedCloseables = reverseList(CLOSEABLES_INTEGER.get(hashcode));
            for (Closeable closeable : reversedCloseables) {
                try {
                    closeable.close();
                } catch (IOException e) {
                    logger.error(String.format(ERROR_CLOSEABLE_HASHCODE_NOT_CLOSEABLE, hashcode), e);
                }
            }
            CLOSEABLES_INTEGER.get(hashcode).clear();
            CLOSEABLES_INTEGER.remove(hashcode);
        }
    }

    /**
     * Close all managed closeables associated to the class
     * 
     * @param clazz
     *            The class associated to the closeable
     */
    public static synchronized void close(final Class<?> clazz) {
        if (clazz != null && CLOSEABLES_CLASS.containsKey(clazz)) {
            final List<Closeable> reversedCloseables = reverseList(CLOSEABLES_CLASS.get(clazz));
            for (Closeable closeable : reversedCloseables) {
                try {
                    closeable.close();
                } catch (IOException e) {
                    logger.error(String.format(ERROR_CLOSEABLE_CLASS_NOT_CLOSEABLE, clazz.getName()), e);
                }
            }
            CLOSEABLES_CLASS.get(clazz).clear();
            CLOSEABLES_CLASS.remove(clazz);
        }
    }

    /**
     * Close all managed closeables associated to the url
     * 
     * @param url
     *            The url associated to the closeable
     */
    public static synchronized void close(final URL url) {
        if (url != null) {
            String path = url.getPath();
            if (CLOSEABLES_URL.containsKey(path)) {
                final List<Closeable> reversedCloseables = reverseList(CLOSEABLES_URL.get(path));
                for (Closeable closeable : reversedCloseables) {
                    try {
                        closeable.close();
                    } catch (IOException e) {
                        logger.error(String.format(ERROR_CLOSEABLE_URL_NOT_CLOSEABLE, path), e);
                    }
                }
                CLOSEABLES_URL.get(path).clear();
                CLOSEABLES_URL.remove(path);
            }
        }
    }

    /**
     * Close the closeable
     * 
     * @param closeable
     *            The closeable to close
     */
    public static synchronized void close(final Closeable closeable) {
        if (closeable != null) {
            try {
                final File linkedFile = getFile(closeable);
                if (linkedFile != null) {
                    close(linkedFile);
                } else {
                    closeable.close();
                }
            } catch (IOException e) {
                logger.error(ERROR_CLOSEABLE_NOT_CLOSEABLE, e);
            }
        }
    }

    /**
     * Get the linked file to the closeable
     * 
     * @param closeable
     *            The closeable to search
     * @return The linked file
     */
    private static synchronized File getFile(final Closeable closeable) {
        if (closeable != null) {
            for (Entry<File, List<Closeable>> entry : CLOSEABLES_FILE.entrySet()) {
                for (Closeable linkedCloseable : entry.getValue()) {
                    if (closeable.equals(linkedCloseable)) {
                        return entry.getKey();
                    }
                }
            }
        }
        return null;
    }
}
