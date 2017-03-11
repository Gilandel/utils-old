/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package samples;

import java.util.Objects;
import java.util.function.Predicate;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.lang3.StringUtils;

import fr.landel.utils.commons.ArrayUtils;

/**
 * (Description)
 *
 * @since Mar 7, 2017
 * @author Gilles
 *
 */
public class ObjectPredicator {

    public static <T> Predicate<T> isNull() {
        return Objects::isNull;
    }

    public static <T> Predicate<T> isNotNull() {
        return Objects::nonNull;
    }

    public static <T> Predicate<T> isEqual(final Object object) {
        return t -> Objects.equals(t, object);
    }

    public static <T> Predicate<T> isNotEqual(final Object object) {
        return t -> !Objects.equals(t, object);
    }

    public static <T extends CharSequence> Predicate<T> isEmpty() {
        return StringUtils::isEmpty;
    }

    public static <T extends CharSequence> Predicate<T> isNotEmpty2() {
        return StringUtils::isNotEmpty;
    }

    public static <T> Predicate<T[]> isNotEmpty() {
        return ArrayUtils::isNotEmpty;
    }

    public static <T extends CharSequence> Predicate<T> isBlank() {
        return StringUtils::isBlank;
    }

    public static <T extends CharSequence> Predicate<T> isNotBlank() {
        return StringUtils::isNotBlank;
    }

    public static <T extends CharSequence> Predicate<T> startsWith(final CharSequence prefix) {
        return t -> StringUtils.startsWith(t, prefix);
    }

    public static <T extends CharSequence> Predicate<T> endsWith(final CharSequence suffix) {
        return t -> StringUtils.endsWith(t, suffix);
    }

    public static <T extends CharSequence> Predicate<T> contains(final CharSequence subtext) {
        return t -> StringUtils.contains(t, subtext);
    }

    public static <T extends CharSequence> Predicate<T> equalsIgnoreCase(final CharSequence suffix) {
        return t -> StringUtils.equalsIgnoreCase(t, suffix);
    }

    public static <T> Predicate<Iterable<T>> hasLength(final int length) {
        return t -> CollectionUtils.size(t) == length;
    }

    public static <T> Predicate<Iterable<T>> isEmptyCollection() {
        return CollectionUtils::sizeIsEmpty;
    }

    public static <T> Predicate<Iterable<T>> contains(final T element) {
        return t -> IterableUtils.contains(t, element);
    }

    public static void main(String... strings) {
        System.out.println(isEmpty().or(isNotBlank()).test("toto"));
        System.out.println(isEmpty().or(isNotBlank()).test(""));
        System.out.println(isEmpty().or(isNotBlank()).test("   "));
        System.out.println(isEmpty().or(contains("test")).test("  test "));
        System.out.println(isEmpty().or(contains("test")).test(""));
        System.out.println(isEmpty().or(contains("test")).test("tesot"));
        System.out.println(isEmpty().or(contains("test")).or(equalsIgnoreCase("TeSoT")).test("tesot"));
    }
}
