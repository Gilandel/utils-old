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
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.function.BiFunction;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.tuple.Triple;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.landel.utils.commons.EnumChar;

/**
 * Assertor constants
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 */
public class AssertorConstants {

    /**
     * The logger
     */
    protected static final Logger LOGGER = LoggerFactory.getLogger(Assertor.class);

    // ---------- DEFAULT

    /**
     * Default assertion
     */
    protected static final String DEFAULT_ASSERTION = "Assertion failed";

    /**
     * Default exception builder
     */
    protected static final BiFunction<String, List<Triple<Object, EnumType, Boolean>>, IllegalArgumentException> DEFAULT_EXCEPTION_BUILDER = (
            String errors, List<Triple<Object, EnumType, Boolean>> parameters) -> new IllegalArgumentException(
                    AssertorHelper.getMessage(AssertorConstants.DEFAULT_ASSERTION, null, errors, parameters, null));

    // ---------- PROPERTIES / MESSAGES

    /**
     * Messages properties (for now doesn't support locale, maybe later)
     */
    protected static final Properties PROPS;
    static {
        PROPS = new Properties();
        try (InputStream is = Assertor.class.getClassLoader().getResourceAsStream("assertor_messages.properties")) {
            PROPS.load(is);
        } catch (IOException e) {
            LOGGER.error("Cannot load the assertor configuration file");
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

    // ---------- PROPERTIES LOADER

    /**
     * Returns the property associated to the key with replaced arguments or the
     * default string if not found {@link AssertorConstants#DEFAULT_ASSERTION}.
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

    // ---------- SUB PROPERTIES

    /**
     * Messages constants
     *
     * @since 30 juil. 2016
     * @author Gilles
     *
     */
    protected static interface MSG {

        /**
         * Suffix for properties of type NOT
         */
        String NOT = ".not";

        /**
         * Suffix for properties of type prerequisites
         */
        String PRE = ".pre";

        /**
         * Object constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        static interface OBJECT {

            /**
             * Message key for object null
             */
            String NULL = "object.null";

            /**
             * Message key for object equals
             */
            String EQUALS = "object.equals";

            /**
             * Message key for object instance of
             */
            String INSTANCE = "object.instance";

            /**
             * Message key for object assignable from
             */
            String ASSIGNABLE = "object.assignable";

            /**
             * Message key for object validates predicate
             */
            String VALIDATES = "object.validates";
        }

        /**
         * Boolean constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        static interface BOOLEAN {

            /**
             * Message key for boolean is true
             */
            String TRUE = "boolean.true";

            /**
             * Message key for boolean is false
             */
            String FALSE = "boolean.false";
        }

        /**
         * Class constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        static interface CLASS {

            /**
             * Message key for class assignable from
             */
            String ASSIGNABLE = "class.assignable";
        }

        /**
         * Number constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        static interface NUMBER {

            /**
             * Message key for number equals
             */
            String EQUALS = "number.equals";

            /**
             * Message key for number greater than
             */
            String GT = "number.gt";

            /**
             * Message key for number greater than or equal to
             */
            String GTE = "number.gte";

            /**
             * Message key for number lower than
             */
            String LT = "number.lt";

            /**
             * Message key for number lower than or equal to
             */
            String LTE = "number.lte";
        }

        /**
         * Char sequence constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        static interface CSQ {

            /**
             * Message key for char sequence length
             */
            String LENGTH = "csq.length";

            /**
             * Message key for empty char sequence
             */
            String EMPTY = "csq.empty";

            /**
             * Message key for char sequence blank
             */
            String BLANK = "csq.blank";

            /**
             * Message key for char sequence contains substring
             */
            String CONTAINS = "csq.contains";

            /**
             * Message key for char sequence starts with
             */
            String STARTS = "csq.starts";

            /**
             * Message key for char sequence ends with
             */
            String ENDS = "csq.ends";

            /**
             * Message key for char sequence matches
             */
            String MATCHES = "csq.matches";

            /**
             * Message key for char sequence find
             */
            String FIND = "csq.find";
        }

        /**
         * Date constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        static interface DATE {

            /**
             * Message key for date equals
             */
            String EQUALS = "date.equals";

            /**
             * Message key for date around
             */
            String AROUND = "date.around";

            /**
             * Message key for date after
             */
            String AFTER = "date.after";

            /**
             * Message key for date after or equals
             */
            String AFTER_OR_EQUALS = "date.afterOrEquals";

            /**
             * Message key for date before
             */
            String BEFORE = "date.before";

            /**
             * Message key for date before or equals
             */
            String BEFORE_OR_EQUALS = "date.beforeOrEquals";
        }

        /**
         * Array constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        static interface ARRAY {

            /**
             * Message key for array length
             */
            String LENGTH = "array.length";

            /**
             * Message key for empty array
             */
            String EMPTY = "array.empty";

            /**
             * Message key for array contains object
             */
            String CONTAINS_OBJECT = "array.contains.object";

            /**
             * Message key for array contains all values
             */
            String CONTAINS_ALL = "array.contains.array.all";

            /**
             * Message key for array contains any values
             */
            String CONTAINS_ANY = "array.contains.array.any";
        }

        /**
         * Iterable constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        static interface ITERABLE {

            /**
             * Message key for iterable size
             */
            String SIZE = "iterable.size";

            /**
             * Message key for empty iterable
             */
            String EMPTY = "iterable.empty";

            /**
             * Message key for iterable contains object
             */
            String CONTAINS_OBJECT = "iterable.contains.object";

            /**
             * Message key for iterable contains all values
             */
            String CONTAINS_ALL = "iterable.contains.iterable.all";

            /**
             * Message key for iterable contains any values
             */
            String CONTAINS_ANY = "iterable.contains.iterable.any";
        }

        /**
         * MAP constants
         *
         * @since 30 juil. 2016
         * @author Gilles
         *
         */
        static interface MAP {

            /**
             * Message key for map size
             */
            String SIZE = "map.size";

            /**
             * Message key for empty map
             */
            String EMPTY = "map.empty";

            /**
             * Message key for map contains key
             */
            String CONTAINS_KEY = "map.contains.key";

            /**
             * Message key for map contains key/value pair
             */
            String CONTAINS_PAIR = "map.contains.pair";

            /**
             * Message key for map contains all keys
             */
            String CONTAINS_KEYS_ALL = "map.contains.keys.all";

            /**
             * Message key for map contains any keys
             */
            String CONTAINS_KEYS_ANY = "map.contains.keys.any";

            /**
             * Message key for map contains all map entries
             */
            String CONTAINS_MAP_ALL = "map.contains.map.all";

            /**
             * Message key for map contains any map entries
             */
            String CONTAINS_MAP_ANY = "map.contains.map.any";
        }
    }
}
