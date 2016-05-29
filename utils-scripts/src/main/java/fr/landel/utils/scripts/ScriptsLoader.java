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

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.landel.utils.commons.StringUtils;
import fr.landel.utils.io.EncodingUtils;
import fr.landel.utils.io.FileUtils;

/**
 * Scripts loader (load scripts from classpath, and remove comments and blank
 * lines)
 *
 * @since 1 déc. 2015
 * @author Gilles
 *
 */
public class ScriptsLoader {

    /**
     * The SQL comment tag (one line)
     */
    public static final String COMMENT_SQL = "--";

    /**
     * The standard comment tag (one line)
     */
    public static final String COMMENT_STANDARD = "//";

    /**
     * The open comment tag (multi-lines)
     */
    public static final String COMMENT_OPEN = "/*";

    /**
     * The close comment tag (multi-lines)
     */
    public static final String COMMENT_CLOSE = "*/";

    /**
     * Scripts path
     */
    private static final String DEFAULT_PATH = "scripts/";

    private static final String NEW_LINE = "\n";
    private static final String LINE_RETURN = "\r";

    private static final Logger LOGGER = LoggerFactory.getLogger(ScriptsLoader.class);

    private final Map<ScriptsList<?>, StringBuilder> scripts;
    private final ScriptsReplacer replacer;

    private boolean removeComments;
    private boolean removeBlankLines;
    private String path;
    private String oneLineCommentOperator;
    private String multiLineCommentOperatorOpen;
    private String multiLineCommentOperatorClose;

    /**
     * Constructor
     */
    public ScriptsLoader() {
        super();

        this.scripts = new HashMap<>();
        this.replacer = new ScriptsReplacer();

        this.removeComments = Boolean.TRUE;
        this.removeBlankLines = Boolean.TRUE;
        this.path = DEFAULT_PATH;

        this.oneLineCommentOperator = COMMENT_SQL;
        this.multiLineCommentOperatorOpen = COMMENT_OPEN;
        this.multiLineCommentOperatorClose = COMMENT_CLOSE;
    }

    /**
     * @param path
     *            The base path (default path: scripts/)
     */
    public void setPath(final String path) {
        this.path = path;
    }

    /**
     * Load all scripts from classpath
     * 
     * @param loader
     *            The current class loader
     * @param scriptsList
     *            The scripts list
     */
    public void init(final ClassLoader loader, final ScriptsList<?>... scriptsList) {
        for (ScriptsList<?> value : scriptsList) {
            final StringBuilder sb = new StringBuilder();
            this.scripts.put(value, sb);

            try (final InputStream is = loader.getResourceAsStream(this.path + value.getName())) {
                sb.append(FileUtils.getFileContent(is, value.getCharset()));
            } catch (IOException e) {
                LOGGER.error("Error occurred while loading " + value.getName(), e);
            }
        }
    }

    /**
     * Load all scripts from classpath
     * 
     * @param scriptsList
     *            The scripts list
     */
    public void init(final ScriptsList<?>... scriptsList) {
        for (ScriptsList<?> value : scriptsList) {
            final StringBuilder sb = new StringBuilder();
            this.scripts.put(value, sb);

            try (final InputStream is = ScriptsLoader.class.getClassLoader().getResourceAsStream(this.path + value.getName())) {
                sb.append(FileUtils.getFileContent(is, EncodingUtils.CHARSET_UTF_8));
            } catch (IOException e) {
                LOGGER.error("Error occurred while loading " + value.getName(), e);
            }
        }
    }

    /**
     * @param oneLineCommentOperator
     *            the oneLineCommentOperator to set (default: "--")
     */
    public void setOneLineCommentOperator(final String oneLineCommentOperator) {
        this.oneLineCommentOperator = oneLineCommentOperator;
    }

    /**
     * The multi line comment operator
     * 
     * @param multiLineCommentOperatorOpen
     *            the multi line open comment operator (default: "/*")
     * @param multiLineCommentOperatorClose
     *            the multi line close comment operator (default: "&#42;/")
     */
    public void setMultiLineCommentOperators(final String multiLineCommentOperatorOpen, final String multiLineCommentOperatorClose) {
        this.multiLineCommentOperatorOpen = multiLineCommentOperatorOpen;
        this.multiLineCommentOperatorClose = multiLineCommentOperatorClose;
    }

    /**
     * @return the removeComments
     */
    public boolean isRemoveComments() {
        return this.removeComments;
    }

    /**
     * @param removeComments
     *            the removeComments to set
     */
    public void setRemoveComments(boolean removeComments) {
        this.removeComments = removeComments;
    }

    /**
     * @return the removeBlankLines
     */
    public boolean isRemoveBlankLines() {
        return this.removeBlankLines;
    }

    /**
     * @param removeBlankLines
     *            the removeBlankLines to set
     */
    public void setRemoveBlankLines(boolean removeBlankLines) {
        this.removeBlankLines = removeBlankLines;
    }

    /**
     * Get the scripts file
     * 
     * @param path
     *            The scripts path
     * @return The StringBuilder
     */
    public StringBuilder get(final ScriptsList<?> path) {
        return this.get(path, new HashMap<String, String>(), false);
    }

    /**
     * Get the scripts file
     * 
     * @param path
     *            The scripts path
     * @param key
     *            The key to replace
     * @param value
     *            The replacement value
     * @return The StringBuilder
     */
    public StringBuilder get(final ScriptsList<?> path, final String key, final String value) {
        return this.get(path, key, value, true);
    }

    /**
     * Get the scripts file
     * 
     * @param path
     *            The scripts path
     * @param key
     *            The key to replace
     * @param value
     *            The replacement value
     * @param checkVariables
     *            En/Disable variables to avoid SQL injections
     * @return The StringBuilder
     */
    public StringBuilder get(final ScriptsList<?> path, final String key, final String value, final boolean checkVariables) {
        if (key != null) {
            final Map<String, String> replacements = new HashMap<>();
            replacements.put(key, value);

            return this.get(path, replacements, checkVariables);
        }
        return null;
    }

    /**
     * Get the scripts file
     * 
     * @param path
     *            The scripts path
     * @param replacements
     *            The entries (keys to replace, replacement values)
     * @return The StringBuilder
     */
    public StringBuilder get(final ScriptsList<?> path, final Map<String, String> replacements) {
        return this.get(path, replacements, true);
    }

    /**
     * Get the scripts file
     * 
     * @param path
     *            The scripts path
     * @param replacements
     *            The entries (keys to replace, replacement values)
     * @param checkVariables
     *            En/Disable variables to avoid SQL injections
     * @return The StringBuilder
     */
    public StringBuilder get(final ScriptsList<?> path, final Map<String, String> replacements, final boolean checkVariables) {
        if (this.scripts.containsKey(path)) {
            final StringBuilder builder = new StringBuilder(this.scripts.get(path));

            LOGGER.info("remove comments");

            if (this.removeComments) {
                this.removeComments(builder);
            }

            LOGGER.info("replacing");

            this.replacer.replace(builder, replacements, checkVariables);

            LOGGER.info("removeBlankLines");

            if (this.removeBlankLines) {
                this.removeBlankLines(builder);
            }

            return builder;
        }
        return null;
    }

    private void removeComments(final StringBuilder builder) {
        int startComments = 0;
        int endComments = 0;

        // removes line comments
        while ((startComments = builder.indexOf(this.oneLineCommentOperator)) > -1) {
            // For Windows / Mac / Unix
            int cr = builder.indexOf(NEW_LINE, startComments);
            int lf = builder.indexOf(LINE_RETURN, startComments);
            if (cr > -1 && lf > -1) {
                endComments = Math.min(cr, lf);
            } else if (cr > -1) {
                endComments = cr;
            } else if (lf > -1) {
                endComments = lf;
            } else {
                endComments = builder.length();
            }
            if (endComments > startComments) {
                builder.delete(startComments, endComments);
            }
        }

        // removes multi-lines comments
        while ((startComments = builder.indexOf(this.multiLineCommentOperatorOpen)) > -1) {
            endComments = builder.indexOf(this.multiLineCommentOperatorClose, startComments);
            if (endComments > startComments) {
                builder.delete(startComments, endComments + 2);
            } else {
                builder.delete(startComments, builder.length());
            }
        }
    }

    private void removeBlankLines(final StringBuilder builder) {
        int lineSeparator = 0;
        int startLine = 0;
        int endLine = 0;

        // removes return character to simplify the remove of blank lines
        while ((lineSeparator = builder.indexOf(LINE_RETURN)) > -1) {
            builder.delete(lineSeparator, lineSeparator + 1);
        }

        // removes blank lines
        while ((endLine = builder.indexOf(NEW_LINE, startLine)) > -1) {
            if (endLine >= startLine && StringUtils.isBlank(builder.substring(startLine, endLine))) {
                builder.delete(startLine, endLine + 1);
            } else {
                startLine = endLine + 1;
            }
        }
    }
}
