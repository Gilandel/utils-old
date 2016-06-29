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
     * Scripts path
     */
    private static final String DEFAULT_PATH = "scripts/";

    private static final String NEW_LINE = "\n";
    private static final String LINE_RETURN = "\r";

    private static final Logger LOGGER = LoggerFactory.getLogger(ScriptsLoader.class);

    private final Map<ScriptsList<?>, StringBuilder> scripts;
    private final ScriptsReplacer replacer;

    private String path;

    /**
     * Constructor
     */
    public ScriptsLoader() {
        super();

        this.scripts = new HashMap<>();
        this.replacer = new ScriptsReplacer();

        this.path = DEFAULT_PATH;

        this.replacer.setTemplate(AbstractScriptsTemplate.TEMPLATE_SQL);
    }

    /**
     * @return the replacer
     */
    public ScriptsReplacer getReplacer() {
        return this.replacer;
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
     * Get the scripts file
     * 
     * @param path
     *            The scripts path
     * @param <E>
     *            The type of script list
     * @return The StringBuilder
     */
    public <E extends ScriptsList<E>> StringBuilder get(final ScriptsList<E> path) {
        return this.get(path, new HashMap<String, String>());
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
     * @param <E>
     *            The type of script list
     * @param <V>
     *            The type of replacement value
     * @return The StringBuilder
     */
    public <E extends ScriptsList<E>, V> StringBuilder get(final ScriptsList<E> path, final String key, final V value) {
        if (key != null) {
            final Map<String, V> replacements = new HashMap<>();
            replacements.put(key, value);

            return this.get(path, replacements);
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
     * @param <E>
     *            The type of script list
     * @param <V>
     *            The type of replacement values
     * @return The StringBuilder
     */
    public <E extends ScriptsList<E>, V> StringBuilder get(final ScriptsList<E> path, final Map<String, V> replacements) {
        if (this.scripts.containsKey(path)) {
            final StringBuilder builder = new StringBuilder(this.scripts.get(path));

            if (this.replacer.getTemplate().isRemoveComments()) {
                this.removeComments(builder);
            }

            this.replacer.replace(builder, replacements);

            if (this.replacer.getTemplate().isRemoveBlankLines()) {
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
        while ((startComments = builder.indexOf(this.replacer.getTemplate().getOneLineCommentOperator())) > -1) {
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
        while ((startComments = builder.indexOf(this.replacer.getTemplate().getMultiLineCommentOperatorOpen())) > -1) {
            endComments = builder.indexOf(this.replacer.getTemplate().getMultiLineCommentOperatorClose(), startComments);
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
