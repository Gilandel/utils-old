/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.landel.utils.commons.EnumChar;

/**
 * Assertor constants
 *
 * @since 30 juil. 2016
 * @author Gilles
 *
 */
public class Constants {

    /**
     * The logger
     */
    protected static final Logger LOGGER = LoggerFactory.getLogger(Assertor.class);

    // ---------- OPERATORS

    /**
     * AND operator
     */
    protected static final int AND = 0;

    /**
     * OR operator
     */
    protected static final int OR = 1;

    /**
     * XOR operator
     */
    protected static final int XOR = 2;

    /**
     * The operator strings
     */
    protected static final String[] OPERATORS = {" AND ", " OR ", " XOR "};

    /**
     * The not operator
     */
    protected static final String NOT = "NOT (";

    // ---------- TYPES

    /**
     * Type constants
     *
     * @since 30 juil. 2016
     * @author Gilles
     *
     */
    protected static class TYPE {

        /**
         * Unknown type
         */
        protected static final int UNKNOWN = 0;

        /**
         * Boolean type
         */
        protected static final int BOOLEAN = 1;

        /**
         * Number, integer type
         */
        protected static final int NUMBER_INTEGER = 2;

        /**
         * Number, floating point type
         */
        protected static final int NUMBER_DECIMAL = 3;

        /**
         * Date type
         */
        protected static final int DATE = 4;

        /**
         * String type
         */
        protected static final int CHAR_SEQUENCE = 5;

        /**
         * Class type
         */
        protected static final int CLASS = 6;

        /**
         * Iterable type
         */
        protected static final int ITERABLE = 7;

        /**
         * Map type
         */
        protected static final int MAP = 8;

        /**
         * Array type
         */
        protected static final int ARRAY = 9;
    }

    // ---------- DEFAULT

    /**
     * Default assertion
     */
    protected static final String DEFAULT_ASSERTION = "Assertion failed";

    /**
     * Default assertion prefix
     */
    protected static final String ASSERTION_PREFIX = "";

    /**
     * Default assertion suffix
     */
    protected static final String ASSERTION_SUFFIX = "";

    // ---------- PROPERTIES / MESSAGES

    /**
     * Messages properties
     */
    protected static final Properties PROPS;
    static {
        PROPS = new Properties();
        try (InputStream is = Assertor.class.getClassLoader().getResourceAsStream("assertor_messages.properties");) {
            PROPS.load(is);
        } catch (IOException e) {
            LOGGER.error("Cannot load the assertor configuration file");
        }
    }

    /**
     * Messages constants
     *
     * @since 30 juil. 2016
     * @author Gilles
     *
     */
    protected static class MSG {

        /**
         * Suffix for properties of type NOT
         */
        protected static final String NOT = ".not";

        /**
         * Suffix for properties of type prerequisites
         */
        protected static final String PRE = ".pre";

        /**
         * Object constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        protected static class OBJECT {
            protected static final String NULL = "object.null";
            protected static final String EQUALS = "object.equals";
            protected static final String INSTANCE = "object.instance";
            protected static final String ASSIGNABLE = "object.assignable";
            protected static final String MATCHES = "object.matches";
            protected static final String VALIDATES = "object.validate";
        }

        /**
         * Boolean constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        protected static class BOOLEAN {
            protected static final String TRUE = "boolean.true";
            protected static final String FALSE = "boolean.false";
        }

        /**
         * Class constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        protected static class CLASS {
            protected static final String ASSIGNABLE = "class.assignable";
        }

        /**
         * Number constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        protected static class NUMBER {
            protected static final String EQUALS = "number.equals";
            protected static final String GT = "number.gt";
            protected static final String GTE = "number.gte";
            protected static final String LT = "number.lt";
            protected static final String LTE = "number.lte";
        }

        /**
         * Char sequence constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        protected static class CSQ {
            protected static final String LENGTH = "csq.length";
            protected static final String EMPTY = "csq.empty";
            protected static final String BLANK = "csq.blank";
            protected static final String CONTAINS = "csq.contains";
            protected static final String STARTS = "csq.starts";
            protected static final String ENDS = "csq.ends";
            protected static final String MATCHES = "csq.matches";
            protected static final String FIND = "csq.find";
        }

        /**
         * Date constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        protected static class DATE {
            protected static final String EQUALS = "date.equals";
            protected static final String AROUND = "date.around";
            protected static final String AFTER = "date.after";
            protected static final String AFTER_OR_EQUALS = "date.afterOrEquals";
            protected static final String BEFORE = "date.before";
            protected static final String BEFORE_OR_EQUALS = "date.beforeOrEquals";
        }

        /**
         * Array constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        protected static class ARRAY {
            protected static final String LENGTH = "array.length";
            protected static final String EMPTY = "array.empty";
            protected static final String CONTAINS_NULL = "array.contains.null";
            protected static final String CONTAINS_OBJECT = "array.contains.object";
            protected static final String CONTAINS_ALL = "array.contains.array.all";
            protected static final String CONTAINS_ANY = "array.contains.array.any";
        }

        /**
         * Iterable constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        protected static class ITERABLE {
            protected static final String SIZE = "iterable.size";
            protected static final String EMPTY = "iterable.empty";
            protected static final String CONTAINS_OBJECT = "iterable.contains.object";
            protected static final String CONTAINS_ALL = "iterable.contains.iterable.all";
            protected static final String CONTAINS_ANY = "iterable.contains.iterable.any";
        }

        /**
         * MAP constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        protected static class MAP {

            /**
             * Message key for map size
             */
            protected static final String SIZE = "map.size";

            /**
             * Message key for empty map
             */
            protected static final String EMPTY = "map.empty";

            /**
             * Message key for contains key
             */
            protected static final String CONTAINS_KEY = "map.contains.key";

            /**
             * Message key for contains key/value pair
             */
            protected static final String CONTAINS_PAIR = "map.contains.pair";

            /**
             * Message key for contains all keys
             */
            protected static final String CONTAINS_KEYS_ALL = "map.contains.keys.all";

            /**
             * Message key for contains any keys
             */
            protected static final String CONTAINS_KEYS_ANY = "map.contains.keys.any";

            /**
             * Message key for contains all map entries
             */
            protected static final String CONTAINS_MAP_ALL = "map.contains.map.all";

            /**
             * Message key for contains any map entries
             */
            protected static final String CONTAINS_MAP_ANY = "map.contains.map.any";
        }
    }

    // ---------- OTHERS

    protected static final Map<Integer, String> CALENDAR_FIELDS;
    static {
        Map<Integer, String> map = new HashMap<>();
        map.put(Calendar.ERA, "ERA");
        map.put(Calendar.YEAR, "YEAR");
        map.put(Calendar.MONTH, "MONTH");
        map.put(Calendar.WEEK_OF_YEAR, "WEEK_OF_YEAR");
        map.put(Calendar.WEEK_OF_MONTH, "WEEK_OF_MONTH");
        map.put(Calendar.DATE, "DATE");
        map.put(Calendar.DAY_OF_MONTH, "DAY_OF_MONTH");
        map.put(Calendar.DAY_OF_YEAR, "DAY_OF_YEAR");
        map.put(Calendar.DAY_OF_WEEK, "DAY_OF_WEEK");
        map.put(Calendar.DAY_OF_WEEK_IN_MONTH, "DAY_OF_WEEK_IN_MONTH");
        map.put(Calendar.AM_PM, "AM_PM");
        map.put(Calendar.HOUR, "HOUR");
        map.put(Calendar.HOUR_OF_DAY, "HOUR_OF_DAY");
        map.put(Calendar.MINUTE, "MINUTE");
        map.put(Calendar.SECOND, "SECOND");
        map.put(Calendar.MILLISECOND, "MILLISECOND");
        CALENDAR_FIELDS = Collections.unmodifiableMap(map);
    }

    // ---------- PRIVATE

    /**
     * Returns the property associated to the key with replaced arguments or the
     * default string if not found {@link Constants#DEFAULT_ASSERTION}.
     * 
     * @param key
     *            The property key
     * @param arguments
     *            The arguments to replace
     * @return The property
     */
    protected static CharSequence getProperty(final CharSequence key, final CharSequence... arguments) {
        String property = PROPS.getProperty(String.valueOf(key));
        if (property != null) {
            if (ArrayUtils.isNotEmpty(arguments)) {
                for (int i = 0; i < arguments.length; i++) {
                    property = property.replace(new StringBuilder(EnumChar.BRACE_OPEN.toString()).append(i).append(EnumChar.BRACE_CLOSE),
                            arguments[i]);
                }
            }

            return property;
        } else {
            return DEFAULT_ASSERTION;
        }
    }
}