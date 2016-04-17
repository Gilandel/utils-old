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
package org.gl.utils.commons.mdc;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.slf4j.Logger;
import org.slf4j.MDC;

/**
 * MDC Thread Safe (only checked with logback adpater (thread safe)).<br>
 * <br>
 * To be sure, the thread method where you log, have to be synchronized or you
 * have to use the context map.<br>
 * <br>
 * Ex:<br>
 * public class Task implements Runnable {<br>
 * &nbsp;&nbsp;public synchronized void run(){<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;MDCTS.put("prop.key1", "value");<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;MDCTS.put("prop.key2", 2);<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;LOG.info("My log");<br>
 * &nbsp;&nbsp;}<br>
 * } <br>
 * <br>
 * Or:<br>
 * public class Task implements Runnable {<br>
 * &nbsp;&nbsp;private Map&lt;String, String&gt; contextMap =
 * MDCTS.getCopyOfContextMap();<br>
 * &nbsp;&nbsp;public void run(){<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;MDCTS.setContextMap(this.contextMap);<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;MDCTS.put("prop.key", "value");<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;LOG.info("My log");<br>
 * &nbsp;&nbsp;}<br>
 * }
 *
 * @since 11 déc. 2015
 * @author Gilles Landel
 *
 */
public abstract class MDCTS {

    private static ConcurrentMap<Long, ConcurrentMap<String, String>> globalContextMap = new ConcurrentHashMap<>();

    /**
     * Utility class
     */
    public MDCTS() {
    }

    /**
     * Get the context map of the current thread
     * 
     * @param threadId
     *            The thread identifier
     * @return The concurrent context map
     */
    protected static ConcurrentMap<String, String> getContextMap(final Long threadId) {
        MDCTS.globalContextMap.putIfAbsent(threadId, new ConcurrentHashMap<String, String>());

        return MDCTS.globalContextMap.get(threadId);
    }

    /**
     * The key to add or update
     * 
     * @param threadId
     *            The thread identifier
     * @param key
     *            The key
     * @param value
     *            The value
     */
    public static synchronized void put(final Long threadId, final String key, final String value) {
        MDCTS.getContextMap(threadId).put(key, value);
    }

    /**
     * Check if the key is already in the context map
     * 
     * @param threadId
     *            The thread identifier
     * @param key
     *            The key
     * @return True, if the value exists in map
     */
    public static synchronized boolean containsKey(final Long threadId, final String key) {
        return MDCTS.getContextMap(threadId).containsKey(key);
    }

    /**
     * Check if the value is already in the context map
     * 
     * @param threadId
     *            The thread identifier
     * @param value
     *            The value
     * @return True, if the value exists in map
     */
    public static synchronized boolean containsValue(final Long threadId, final String value) {
        return MDCTS.getContextMap(threadId).containsValue(value);
    }

    /**
     * Retrieve the value from the key
     * 
     * @param threadId
     *            The thread identifier
     * @param key
     *            The key
     * @return The value
     */
    public static synchronized String get(final Long threadId, final String key) {
        return MDCTS.getContextMap(threadId).get(key);
    }

    /**
     * Remove the property
     * 
     * @param threadId
     *            The thread identifier
     * @param key
     *            The key to remove
     */
    public static synchronized void remove(final Long threadId, final String key) {
        MDCTS.getContextMap(threadId).remove(key);
    }

    /**
     * Clear all properties
     * 
     * @param threadId
     *            The thread identifier
     */
    public static synchronized void clear(final Long threadId) {
        MDCTS.getContextMap(threadId).clear();
    }

    /**
     * Log the trace message (set the context map)
     * 
     * @param threadId
     *            The thread identifier
     * @param logger
     *            The logger
     * @param message
     *            The message
     * @param throwable
     *            The cause exception
     */
    public static synchronized void trace(final Long threadId, final Logger logger, final String message, final Throwable throwable) {
        if (logger.isTraceEnabled()) {
            MDC.setContextMap(MDCTS.getContextMap(threadId));
            logger.trace(message, throwable);
        }
    }

    /**
     * Log the trace message (set the context map)
     * 
     * @param threadId
     *            The thread identifier
     * @param logger
     *            The logger
     * @param message
     *            The message
     * @param arguments
     *            the arguments
     */
    public static synchronized void trace(final Long threadId, final Logger logger, final String message, final Object... arguments) {
        if (logger.isTraceEnabled()) {
            MDC.setContextMap(MDCTS.getContextMap(threadId));
            logger.trace(message, arguments);
        }
    }

    /**
     * Log the debug message (set the context map)
     * 
     * @param threadId
     *            The thread identifier
     * @param logger
     *            The logger
     * @param message
     *            The message
     * @param throwable
     *            The cause exception
     */
    public static synchronized void debug(final Long threadId, final Logger logger, final String message, final Throwable throwable) {
        if (logger.isDebugEnabled()) {
            MDC.setContextMap(MDCTS.getContextMap(threadId));
            logger.debug(message, throwable);
        }
    }

    /**
     * Log the debug message (set the context map)
     * 
     * @param threadId
     *            The thread identifier
     * @param logger
     *            The logger
     * @param message
     *            The message
     * @param arguments
     *            the arguments
     */
    public static synchronized void debug(final Long threadId, final Logger logger, final String message, final Object... arguments) {
        if (logger.isDebugEnabled()) {
            MDC.setContextMap(MDCTS.getContextMap(threadId));
            logger.debug(message, arguments);
        }
    }

    /**
     * Log the info message (set the context map)
     * 
     * @param threadId
     *            The thread identifier
     * @param logger
     *            The logger
     * @param message
     *            The message
     * @param throwable
     *            The cause exception
     */
    public static synchronized void info(final Long threadId, final Logger logger, final String message, final Throwable throwable) {
        if (logger.isInfoEnabled()) {
            MDC.setContextMap(MDCTS.getContextMap(threadId));
            logger.info(message, throwable);
        }
    }

    /**
     * Log the info message (set the context map)
     * 
     * @param threadId
     *            The thread identifier
     * @param logger
     *            The logger
     * @param message
     *            The message
     * @param arguments
     *            the arguments
     */
    public static synchronized void info(final Long threadId, final Logger logger, final String message, final Object... arguments) {
        if (logger.isInfoEnabled()) {
            MDC.setContextMap(MDCTS.getContextMap(threadId));
            logger.info(message, arguments);
        }
    }

    /**
     * Log the warning message (set the context map)
     * 
     * @param threadId
     *            The thread identifier
     * @param logger
     *            The logger
     * @param message
     *            The message
     * @param throwable
     *            The cause exception
     */
    public static synchronized void warn(final Long threadId, final Logger logger, final String message, final Throwable throwable) {
        if (logger.isWarnEnabled()) {
            MDC.setContextMap(MDCTS.getContextMap(threadId));
            logger.warn(message, throwable);
        }
    }

    /**
     * Log the warning message (set the context map)
     * 
     * @param threadId
     *            The thread identifier
     * @param logger
     *            The logger
     * @param message
     *            The message
     * @param arguments
     *            the arguments
     */
    public static synchronized void warn(final Long threadId, final Logger logger, final String message, final Object... arguments) {
        if (logger.isWarnEnabled()) {
            MDC.setContextMap(MDCTS.getContextMap(threadId));
            logger.warn(message, arguments);
        }
    }

    /**
     * Log the error message (set the context map)
     * 
     * @param threadId
     *            The thread identifier
     * @param logger
     *            The logger
     * @param message
     *            The message
     * @param throwable
     *            The cause exception
     */
    public static synchronized void error(final Long threadId, final Logger logger, final String message, final Throwable throwable) {
        if (logger.isErrorEnabled()) {
            MDC.setContextMap(MDCTS.getContextMap(threadId));
            logger.error(message, throwable);
        }
    }

    /**
     * Log the error message (set the context map)
     * 
     * @param threadId
     *            The thread identifier
     * @param logger
     *            The logger
     * @param message
     *            The message
     * @param arguments
     *            the arguments
     */
    public static synchronized void error(final Long threadId, final Logger logger, final String message, final Object... arguments) {
        if (logger.isErrorEnabled()) {
            MDC.setContextMap(MDCTS.getContextMap(threadId));
            logger.error(message, arguments);
        }
    }
}
