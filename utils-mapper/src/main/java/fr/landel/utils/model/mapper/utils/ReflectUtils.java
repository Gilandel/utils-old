/*
 * #%L
 * utils-mapper
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.mapper.utils;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import org.apache.commons.beanutils.PropertyUtilsBean;
import org.apache.commons.lang3.StringUtils;

import fr.landel.utils.commons.asserts.AssertUtils;
import fr.landel.utils.commons.exception.AbstractException;
import fr.landel.utils.commons.stream.FunctionThrowable;
import fr.landel.utils.model.mapper.MapperException;
import fr.landel.utils.model.mapper.mappable.Mappable;
import fr.landel.utils.model.mapper.mappable.MappableProperty;

/**
 * Utilities to map.
 *
 * @since 13 juil. 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 *
 */
public class ReflectUtils {

    private static final String ERROR_MSG = "Reflection failed";

    private static final String IS_PREFIX = "is";
    private static final String GET_PREFIX = "get";
    private static final String SET_PREFIX = "set";

    private PropertyUtilsBean pub;

    /**
     * Default constructor.
     */
    public ReflectUtils() {
        this.pub = new PropertyUtilsBean();
    }

    /**
     * Get the property utils bean.
     * 
     * @return the property utils bean
     */
    public PropertyUtilsBean getPropertyUtilsBean() {
        return this.pub;
    }

    /**
     * Get the list of all the fields in the specified class.
     * 
     * @param clazz
     *            The class (required)
     * @return the Map
     */
    public Map<String, Field> getAllFields(final Class<?> clazz) {

        AssertUtils.isNotNull(clazz, "The class is null");

        return this.getAllFieldsRec(clazz, new HashMap<String, Field>());
    }

    /**
     * Get all fields recursively.
     * 
     * @param clazz
     *            class (required)
     * @param map
     *            map (required)
     * @return the map of fields
     */
    private Map<String, Field> getAllFieldsRec(final Class<?> clazz, final Map<String, Field> map) {
        Class<?> superClazz = clazz.getSuperclass();
        if (superClazz != null) {
            this.getAllFieldsRec(superClazz, map);
        }

        for (Field field : clazz.getDeclaredFields()) {
            if (!field.isSynthetic()) {
                map.put(field.getName(), field);
            }
        }
        return map;
    }

    /**
     * Check if getter is available and the field readable.
     * 
     * @param sourceField
     *            the source field (required)
     * @param sourceObject
     *            The source object (required)
     * @param <S>
     *            The object source type
     * @return If getter is available
     */
    public <S> boolean isGettable(Field sourceField, S sourceObject) {

        AssertUtils.isNotNull(sourceField, "The source field is null");
        AssertUtils.isNotNull(sourceObject, "The source object is null");

        return this.pub.isReadable(sourceObject, sourceField.getName());
    }

    /**
     * Check if setter is available and field writable.
     * 
     * @param sourceField
     *            the source field (required)
     * @param sourceObject
     *            The source object (required)
     * @param <S>
     *            The object source type
     * @return If setter is available
     */
    public <S> boolean isSettable(final Field sourceField, final S sourceObject) {

        AssertUtils.isNotNull(sourceField, "The source field is null");
        AssertUtils.isNotNull(sourceObject, "The source object is null");

        return this.pub.isWriteable(sourceObject, sourceField.getName());
    }

    /**
     * Invoke the getter.
     * 
     * @param sourceField
     *            the source field (required)
     * @param sourceObject
     *            The source object (required)
     * @param <S>
     *            The object source type
     * @param <E>
     *            The post action exception type
     * @param postAction
     *            Call the function before casting the object (optional)
     * @return The value
     * @throws MapperException
     *             On invoking failed
     * @throws IllegalArgumentException
     *             On null field
     */
    public <S, E extends AbstractException> Object invokeGetter(final Field sourceField, final S sourceObject,
            final FunctionThrowable<Object, Object, E> postAction) throws MapperException {

        AssertUtils.isNotNull(sourceField, "The source field is null");
        AssertUtils.isNotNull(sourceObject, "The source object is null");

        Object object;
        try {
            object = this.pub.getProperty(sourceObject, sourceField.getName());
            if (postAction != null) {
                return postAction.applyThrows(object);
            }
            return object;
        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | AbstractException e) {
            throw new MapperException(ERROR_MSG, e);
        }
    }

    /**
     * Call the getter method from the field and the object.
     * 
     * @param field
     *            The field (required)
     * @param instance
     *            The instance (required)
     * @param resultClass
     *            The class of the result type (required)
     * @param postAction
     *            Call the function before casting the object (optional)
     * @param <S>
     *            The source type
     * @param <R>
     *            The return type
     * @return The result
     * @throws MapperException
     *             On invocation exception
     */
    public <R, S> R invokeGetter(final Field field, final S instance, final Class<R> resultClass,
            final FunctionThrowable<Object, R, AbstractException> postAction) throws MapperException {

        AssertUtils.isNotNull(field, "The field is null");
        AssertUtils.isNotNull(instance, "The instance is null");
        AssertUtils.isNotNull(resultClass, "The result class is null");

        try {
            final Object object = this.pub.getProperty(instance, field.getName());
            if (postAction != null) {
                return postAction.applyThrows(object);
            } else if (object == null) {
                return null;
            } else if (resultClass.isAssignableFrom(object.getClass())) {
                return resultClass.cast(object);
            } else {
                throw new MapperException(ERROR_MSG);
            }
        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | AbstractException e) {
            throw new MapperException(ERROR_MSG, e);
        }
    }

    /**
     * Invoke the setter.
     * 
     * @param sourceField
     *            the source field (required)
     * @param sourceObject
     *            The source object (required)
     * @param value
     *            The value to set (required, can be null)
     * @param <S>
     *            The object source type
     * @throws MapperException
     *             On invoking failed
     */
    public <S> void invokeSetter(final Field sourceField, final S sourceObject, final Object value) throws MapperException {

        AssertUtils.isNotNull(sourceField, "The source field is null");
        AssertUtils.isNotNull(sourceObject, "The source object is null");

        try {
            this.pub.setProperty(sourceObject, sourceField.getName(), value);
        } catch (IllegalArgumentException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new MapperException(ERROR_MSG, e);
        }
    }

    /**
     * Invoke the method.
     * 
     * @param method
     *            the method to invoke (required)
     * @param sourceObject
     *            The source object (required)
     * @param parameters
     *            the parameters (optional, cannot be null)
     * @param <S>
     *            The object source type
     * @return The value
     * @throws MapperException
     *             On invoking failed
     */
    public <S> Object invoke(final Method method, final S sourceObject, final Object... parameters) throws MapperException {

        AssertUtils.isNotNull(method, "The method is null");
        AssertUtils.isNotNull(sourceObject, "The source object is null");
        AssertUtils.isNotNull(parameters, "The parameters is null");

        try {
            return method.invoke(sourceObject, parameters);
        } catch (IllegalAccessException | InvocationTargetException | IllegalArgumentException e) {
            throw new MapperException(ERROR_MSG, e);
        }
    }

    /**
     * Check if the class is mappable.
     * 
     * @param clazz
     *            The class (required)
     * @return If the class has the mappable annotation
     */
    public Boolean isMappableClass(final Class<?> clazz) {

        AssertUtils.isNotNull(clazz, "The class is null");

        return clazz.getAnnotation(Mappable.class) != null;
    }

    /**
     * Check if the field is mappable.
     * 
     * @param field
     *            The field (required)
     * @return If the field has the mappable annotation
     */
    public Boolean isMappableField(final Field field) {

        AssertUtils.isNotNull(field, "The field is null");

        MappableProperty[] mappable = field.getDeclaredAnnotationsByType(MappableProperty.class);
        if (mappable != null && mappable.length > 0) {
            return true;
        }

        boolean ret = false;
        try {
            Method method = this.getGetterMethod(field);

            if (method != null) {
                mappable = method.getDeclaredAnnotationsByType(MappableProperty.class);
                ret = mappable != null && mappable.length > 0;
            }
        } catch (MapperException e) {
            // Do nothing
        }
        return ret;
    }

    /**
     * Get the type of the reverse.
     * 
     * @param mappableClass
     *            The mappable class (required)
     * @param <E>
     *            The reverse object type
     * @param <D>
     *            The object type
     * @return The class of the object
     */
    @SuppressWarnings("unchecked")
    public <D, E> Class<E>[] getClassesFromObject(final Class<D> mappableClass) {

        AssertUtils.isNotNull(mappableClass, "The mappable class is null");

        final Mappable mappableReverse = mappableClass.getAnnotation(Mappable.class);
        if (mappableReverse != null && mappableReverse.value().length > 0) {
            return (Class<E>[]) mappableReverse.value();
        }
        return null;
    }

    /**
     * Create a new instance of the specified class.
     * 
     * @param clazz
     *            The class to instantiate (required)
     * @param <I>
     *            The class type
     * @return The new instance
     * @throws MapperException
     *             If creation failed
     */
    public <I> I newInstance(final Class<I> clazz) throws MapperException {

        try {
            return clazz.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new MapperException(ERROR_MSG, e);
        }
    }

    /**
     * Create a new instance of a collection (List, Set or Queue).
     * 
     * @param clazz
     *            The class to instantiate (required)
     * @param <X>
     *            The collection type
     * @return The new instance of the collection
     * @throws MapperException
     *             On creation failed
     */
    public <X> Collection<X> newInstanceCollection(final Class<X> clazz) throws MapperException {

        AssertUtils.isNotNull(clazz, "The class is null");

        if (Queue.class.isAssignableFrom(clazz)) {
            return new LinkedList<X>();
        } else if (List.class.isAssignableFrom(clazz)) {
            return new ArrayList<X>();
        } else if (Set.class.isAssignableFrom(clazz)) {
            return new HashSet<X>();
        }

        return null;
    }

    /**
     * Create a new instance of a map.
     * 
     * @param classMap
     *            The map class (required)
     * @param classKey
     *            The key class (required)
     * @param classValue
     *            The class of the value (required)
     * @param <K>
     *            The key type
     * @param <V>
     *            The value type
     * @return The new typed map
     * @throws MapperException
     *             On creation failed
     */
    public <K, V> Map<K, V> newInstanceMap(final Class<?> classMap, final Class<K> classKey, final Class<V> classValue)
            throws MapperException {

        AssertUtils.isNotNull(classMap, "The map class is null");
        AssertUtils.isNotNull(classKey, "The map key is null");
        AssertUtils.isNotNull(classValue, "The value class is null");

        if (Map.class.isAssignableFrom(classMap)) {
            return new HashMap<>();
        }
        return null;
    }

    /**
     * Create a new instance of a map.
     * 
     * @param classMap
     *            The map class (required)
     * @param classValue
     *            The class of the value (required)
     * @param <V>
     *            The value type
     * @return The new typed map
     * @throws MapperException
     *             On creation failed
     */
    public <V> Map<?, V> newInstanceMap(final Class<?> classMap, final Class<V> classValue) throws MapperException {

        AssertUtils.isNotNull(classMap, "The map class is null");
        AssertUtils.isNotNull(classValue, "The value class is null");

        if (Map.class.isAssignableFrom(classMap)) {
            return new HashMap<>();
        }
        return null;
    }

    /**
     * Create a new instance of a map.
     * 
     * @param classMap
     *            The map class (required)
     * @return The new typed map
     * @throws MapperException
     *             On creation failed
     */
    public Map<?, ?> newInstanceMap(final Class<?> classMap) throws MapperException {

        AssertUtils.isNotNull(classMap, "The map class is null");

        if (Map.class.isAssignableFrom(classMap)) {
            return new HashMap<>();
        }
        return null;
    }

    /**
     * Get the method from the name and its parameter types.
     * 
     * @param clazz
     *            The class (required)
     * @param name
     *            The method name (required and cannot empty be blank)
     * @param parameterTypes
     *            The parameter types (optional, cannot be null)
     * @return The matching method
     * @throws MapperException
     *             On reflection failed
     */
    public Method getMethod(final Class<?> clazz, final String name, final Class<?>... parameterTypes) throws MapperException {

        AssertUtils.isNotNull(clazz, "The class is null");
        AssertUtils.isNotBlank(name, "The name is null or blank");
        AssertUtils.isNotNull(parameterTypes, "The parameter types array is null");

        Method method;
        try {
            if (parameterTypes.length > 0) {
                method = clazz.getDeclaredMethod(name, parameterTypes);
            } else {
                method = clazz.getDeclaredMethod(name);
            }
        } catch (NoSuchMethodException | SecurityException e) {
            if (clazz.getSuperclass() != null) {
                method = this.getMethod(clazz.getSuperclass(), name, parameterTypes);
            } else {
                throw new MapperException(ERROR_MSG, e);
            }
        }
        return method;
    }

    /**
     * Get the getter method from the field.
     * 
     * @param field
     *            The field name (required)
     * @return The matching method
     * @throws MapperException
     *             On reflection failed
     */
    public Method getGetterMethod(final Field field) throws MapperException {

        AssertUtils.isNotNull(field, "The field is null");

        Method method;
        String capitalizedName;

        capitalizedName = StringUtils.capitalize(field.getName());

        if (Boolean.TYPE.isAssignableFrom(field.getType())) {
            try {
                method = this.getMethod(field.getDeclaringClass(), IS_PREFIX + capitalizedName);
            } catch (MapperException e) {
                method = this.getMethod(field.getDeclaringClass(), GET_PREFIX + capitalizedName);
            }
        } else {
            method = this.getMethod(field.getDeclaringClass(), GET_PREFIX + capitalizedName);
        }

        return method;
    }

    /**
     * Get the setter method from the field.
     *
     * @param field
     *            The field name (required)
     * @return The matching method
     * @throws MapperException
     *             On reflection failed
     */
    public Method getSetterMethod(final Field field) throws MapperException {

        AssertUtils.isNotNull(field, "The field is null");

        Method method;
        String capitalizedName;

        capitalizedName = StringUtils.capitalize(field.getName());

        method = this.getMethod(field.getDeclaringClass(), SET_PREFIX + capitalizedName);

        return method;
    }
}
