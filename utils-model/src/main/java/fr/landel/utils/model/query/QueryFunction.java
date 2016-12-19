/*-
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.query;

import java.io.Serializable;

import fr.landel.utils.model.AbstractEntity;

/**
 * (Description)
 *
 * @since 19 nov. 2016
 * @author Gilles
 *
 */
public class QueryFunction<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractQueryBuilder1 {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 6819402721948130934L;

    private static final String MAX = "MAX";
    private static final String MIN = "MIN";
    private static final String AVG = "AVG";
    private static final String SUM = "SUM";
    private static final String COUNT = "COUNT";

    private static final String UPPER = "UPPER";
    private static final String LOWER = "LOWER";
    private static final String SUBSTRING = "SUBSTRING";
    private static final String TRIM = "TRIM";
    private static final String LENGTH = "LENGTH";
    private static final String BIT_LENGTH = "BIT_LENGTH";
    private static final String LOCATE = "LOCATE";
    private static final String TRUNC = "TRUNC";
    private static final String RTRIM = "RTRIM";
    private static final String LTRIM = "LTRIM";

    private static final String ABS = "ABS";
    private static final String SQRT = "SQRT";
    private static final String MOD = "MOD";
    private static final String SIGN = "SIGN";

    private static final String COS = "COS";
    private static final String SIN = "SIN";
    private static final String TAN = "TAN";
    private static final String ACOS = "ACOS";
    private static final String ASIN = "ASIN";
    private static final String ATAN = "ATAN";
    private static final String COT = "COT";
    private static final String ATAN2 = "ATAN2";

    private static final String STR = "STR";
    private static final String CAST = "CAST";
    private static final String AS = "AS";
    private static final String EXTRACT = "EXTRACT";
    private static final String FROM = "FROM";
    private static final String CONCAT = "CONCAT";

    private static final String COALESCE = "COALESCE";
    private static final String NULLIF = "NULLIF";

    private static final String CURRENT_DATE = "CURRENT_DATE";
    private static final String CURRENT_TIME = "CURRENT_TIME";
    private static final String CURRENT_TIMESTAMP = "CURRENT_TIMESTAMP";

    private static final String SECOND = "SECOND";
    private static final String MINUTE = "MINUTE";
    private static final String HOUR = "HOUR";
    private static final String DAY = "DAY";
    private static final String MONTH = "MONTH";
    private static final String YEAR = "YEAR";

    private static final String SIZE = "SIZE";
    private static final String MIN_ELEMENT = "MINELEMENT";
    private static final String MAX_ELEMENT = "MAXELEMENT";
    private static final String MIN_INDEX = "MININDEX";
    private static final String MAX_INDEX = "MAXINDEX";
    private static final String ELEMENTS = "ELEMENTS";
    private static final String INDICES = "INDICES";

    private static final String INDEX = "INDEX";

    private QueryFunction(final String function, final boolean distinct, final CharSequence field) {
        add(function);
        add(PARENTHESIS_OPEN);
        if (distinct) {
            add(DISTINCT);
        }
        add(field);
        add(PARENTHESIS_CLOSE);
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> max(
            final CharSequence field) {
        return new QueryFunction<>(MAX, false, field);
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> max(
            final QueryBuilder<E, K> subquery) {
        return max(subquery.build());
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> min(
            final CharSequence field) {
        return new QueryFunction<>(MIN, false, field);
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> min(
            final QueryBuilder<E, K> subquery) {
        return min(subquery.build());
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> avg(
            final CharSequence field) {
        return new QueryFunction<>(AVG, false, field);
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> avg(
            final QueryBuilder<E, K> subquery) {
        return avg(subquery.build());
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> sum(
            final CharSequence field) {
        return new QueryFunction<>(SUM, false, field);
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> sum(
            final QueryBuilder<E, K> subquery) {
        return sum(subquery.build());
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> count(
            final CharSequence field) {
        return count(field, false);
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> count(
            final QueryBuilder<E, K> subquery) {
        return count(subquery.build());
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> count(
            final CharSequence field, final boolean distinct) {
        return new QueryFunction<>(COUNT, distinct, field);
    }

    public static <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryFunction<E, K> upper(
            final CharSequence field) {
        return new QueryFunction<>(UPPER, false, field);
    }
}
