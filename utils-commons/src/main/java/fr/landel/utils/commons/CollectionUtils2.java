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
package fr.landel.utils.commons;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections4.Transformer;

/**
 * Utility class to manage collections.
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class CollectionUtils2 {

    /**
     * Hidden constructor.
     */
    private CollectionUtils2() {
    }

    /**
     * To array a collection (take the type of the first element, otherwise use
     * the default 'toArray(new T[0])')
     * 
     * @param list
     *            The input list
     * @param <T>
     *            The type of object in collection
     * @return The array
     */
    @SuppressWarnings("unchecked")
    public static <T> T[] toArray(final Collection<T> list) {
        if (list != null && !list.isEmpty()) {
            Class<?> typeClass = list.iterator().next().getClass();
            return list.toArray((T[]) Array.newInstance(typeClass, list.size()));
        } else {
            return null;
        }
    }

    /**
     * Transform an array into an array
     * 
     * @param input
     *            the input array
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted array
     */
    public static <I, O> O[] transformIntoArray(final I[] input, final Transformer<I, O> transformer) {
        final List<O> list = new ArrayList<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return CollectionUtils2.toArray(list);
    }

    /**
     * Transform an array into an array of String (null value are kept)
     * 
     * @param input
     *            the input array
     * @param <I>
     *            The input type
     * @return The converted array
     */
    public static <I> String[] transformIntoArray(final I[] input) {
        return CollectionUtils2.transformIntoArray(input, false);
    }

    /**
     * Transform an array into an array of String
     * 
     * @param input
     *            the input array
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted array
     */
    public static <I> String[] transformIntoArray(final I[] input, final boolean stringifyNull) {
        final List<String> list = new ArrayList<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        if (!list.isEmpty()) {
            return list.toArray(new String[list.size()]);
        } else {
            return null;
        }
    }

    /**
     * Transform an iterable into an array
     * 
     * @param input
     *            the input iterable
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted array
     */
    public static <I, O> O[] transformIntoArray(final Iterable<I> input, final Transformer<I, O> transformer) {
        final List<O> list = new ArrayList<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return CollectionUtils2.toArray(list);
    }

    /**
     * Transform an iterable into an array of String (null value are kept)
     * 
     * @param input
     *            the input iterable
     * @param <I>
     *            The input type
     * @return The converted array
     */
    public static <I> String[] transformIntoArray(final Iterable<I> input) {
        return CollectionUtils2.transformIntoArray(input, false);
    }

    /**
     * Transform an iterable into an array of String
     * 
     * @param input
     *            the input iterable
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted array
     */
    public static <I> String[] transformIntoArray(final Iterable<I> input, final boolean stringifyNull) {
        final List<String> list = new ArrayList<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        if (!list.isEmpty()) {
            return list.toArray(new String[list.size()]);
        } else {
            return null;
        }
    }

    /**
     * Transform an array into a list
     * 
     * @param input
     *            the input array
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted list
     */
    public static <I, O> List<O> transformIntoList(final I[] input, final Transformer<I, O> transformer) {
        final List<O> list = new ArrayList<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return list;
    }

    /**
     * Transform an array into a list of String (null value are kept)
     * 
     * @param input
     *            the input array
     * @param <I>
     *            The input type
     * @return The converted list
     */
    public static <I> List<String> transformIntoList(final I[] input) {
        return CollectionUtils2.transformIntoList(input, false);
    }

    /**
     * Transform an array into a list of String
     * 
     * @param input
     *            the input array
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted list
     */
    public static <I> List<String> transformIntoList(final I[] input, final boolean stringifyNull) {
        final List<String> list = new ArrayList<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        return list;
    }

    /**
     * Transform an iterable into a list
     * 
     * @param input
     *            the input iterable
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted list
     */
    public static <I, O> List<O> transformIntoList(final Iterable<I> input, final Transformer<I, O> transformer) {
        final List<O> list = new ArrayList<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return list;
    }

    /**
     * Transform an iterable into a list of String (null value are kept)
     * 
     * @param input
     *            the input iterable
     * @param <I>
     *            The input type
     * @return The converted list
     */
    public static <I> List<String> transformIntoList(final Iterable<I> input) {
        return CollectionUtils2.transformIntoList(input, false);
    }

    /**
     * Transform an iterable into a list of String
     * 
     * @param input
     *            the input iterable
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted list
     */
    public static <I> List<String> transformIntoList(final Iterable<I> input, final boolean stringifyNull) {
        final List<String> list = new ArrayList<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        return list;
    }

    /**
     * Transform an array into a set
     * 
     * @param input
     *            the input array
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted set
     */
    public static <I, O> Set<O> transformIntoSet(final I[] input, final Transformer<I, O> transformer) {
        final Set<O> list = new HashSet<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return list;
    }

    /**
     * Transform an array into a set of String (null value are kept)
     * 
     * @param input
     *            the input array
     * @param <I>
     *            The input type
     * @return The converted set
     */
    public static <I> Set<String> transformIntoSet(final I[] input) {
        return CollectionUtils2.transformIntoSet(input, false);
    }

    /**
     * Transform an array into a set of String
     * 
     * @param input
     *            the input array
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted set
     */
    public static <I> Set<String> transformIntoSet(final I[] input, final boolean stringifyNull) {
        final Set<String> list = new HashSet<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        return list;
    }

    /**
     * Transform an iterable into a set
     * 
     * @param input
     *            the input iterable
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted set
     */
    public static <I, O> Set<O> transformIntoSet(final Iterable<I> input, final Transformer<I, O> transformer) {
        final Set<O> list = new HashSet<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return list;
    }

    /**
     * Transform an iterable into a set of String (null value are kept)
     * 
     * @param input
     *            the input iterable
     * @param <I>
     *            The input type
     * @return The converted set
     */
    public static <I> Set<String> transformIntoSet(final Iterable<I> input) {
        return CollectionUtils2.transformIntoSet(input, false);
    }

    /**
     * Transform an iterable into a set of String
     * 
     * @param input
     *            the input iterable
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted set
     */
    public static <I> Set<String> transformIntoSet(final Iterable<I> input, final boolean stringifyNull) {
        final Set<String> list = new HashSet<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        return list;
    }

    private static <T> void addToString(final Collection<String> output, final T element, final boolean stringifyNull) {
        if (element != null || stringifyNull) {
            output.add(String.valueOf(element));
        } else {
            output.add(null);
        }
    }
}
