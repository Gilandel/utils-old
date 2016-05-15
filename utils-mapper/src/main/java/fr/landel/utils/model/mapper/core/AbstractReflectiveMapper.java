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
package fr.landel.utils.model.mapper.core;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.StringUtils;

import fr.landel.utils.commons.exception.AbstractException;
import fr.landel.utils.commons.stream.FunctionThrowable;
import fr.landel.utils.model.mapper.DTOIdentifier;
import fr.landel.utils.model.mapper.EnumMode;
import fr.landel.utils.model.mapper.MapperException;
import fr.landel.utils.model.mapper.mappable.Mappable;
import fr.landel.utils.model.mapper.mappable.MappableProperty;
import fr.landel.utils.model.mapper.utils.ReflectUtils;

/**
 * Reflective mapper (DTO 1 &lt;-&gt; DTO 2)
 *
 * @since 29 juil. 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 *
 */
public abstract class AbstractReflectiveMapper {

    // TODO manages deep in MappableProperty and name in MappablesProperty

    /**
     * Reflection utility.
     */
    private ReflectUtils reflectionUtil;

    /**
     * The DTO identifier manager.
     */
    private DTOIdentifierManager dtoIdentifierManager;

    private List<Class<?>> targetables;

    /**
     * Constructor.
     */
    public AbstractReflectiveMapper() {
        this.targetables = new ArrayList<>();

        this.reflectionUtil = new ReflectUtils();
        this.dtoIdentifierManager = new DTOIdentifierManager();
    }

    /**
     * @return the reflectionUtil
     */
    public final ReflectUtils getReflectionUtil() {
        return this.reflectionUtil;
    }

    /**
     * @return the dtoIdentifierManager
     */
    public final DTOIdentifierManager getDtoIdentifierManager() {
        return this.dtoIdentifierManager;
    }

    /**
     * @return the targetable classes
     */
    public final List<Class<?>> getTargetables() {
        return this.targetables;
    }

    /**
     * Map a collection.
     * 
     * @param targetField
     *            target field
     * @param sourceField
     *            source field
     * @param sourceObject
     *            source object
     * @param targetCollection
     *            target collection
     * @param identifier
     *            identifier
     * @param deep
     *            deep
     * @param mode
     *            mapping mode
     * @return the mapped collection
     * @throws MapperException
     *             core exception
     */
    @SuppressWarnings("unchecked")
    private <S, T, E extends AbstractException> Collection<T> mapCollection(final Field targetField, final Field sourceField,
            final S sourceObject, final Collection<T> targetCollection, final DTOIdentifier identifier, final int deep, final EnumMode mode,
            final FunctionThrowable<Object, Object, E> postAction) throws MapperException {

        Collection<T> values = null;
        final Collection<S> collection = (Collection<S>) this.reflectionUtil.invokeGetter(sourceField, sourceObject, postAction);

        if (collection != null) {
            if (targetCollection == null) {
                values = (Collection<T>) this.reflectionUtil.newInstanceCollection(targetField.getType());
            } else {
                values = targetCollection;
            }

            for (S sourceEntityInCollection : collection) {
                T targetEntityInCollection = this.mapObject(sourceEntityInCollection, identifier, deep, mode, postAction);

                if (!values.contains(targetEntityInCollection)) {
                    values.add(targetEntityInCollection);
                } else {
                    T target = null;
                    Iterator<T> it = values.iterator();
                    while (target == null && it.hasNext()) {
                        T obj = it.next();
                        if (obj.equals(targetEntityInCollection)) {
                            target = obj;
                        }
                    }
                    this.mapObject(sourceEntityInCollection, identifier, target, deep, mode, postAction);
                }
            }
        }
        return values;
    }

    /**
     * Map a map.
     * 
     * @param targetField
     *            target field
     * @param sourceField
     *            source field
     * @param sourceObject
     *            source object
     * @param targetMap
     *            target map
     * @param identifier
     *            identifier
     * @param deep
     *            deep
     * @param mode
     *            mapping mode
     * @return the mapped map
     * @throws MapperException
     *             core exception
     */
    @SuppressWarnings("unchecked")
    private <S, T, E extends AbstractException> Map<Object, T> mapMap(final Field targetField, final Field sourceField,
            final S sourceObject, final Map<Object, T> targetMap, final DTOIdentifier identifier, final int deep, final EnumMode mode,
            final FunctionThrowable<Object, Object, E> postAction) throws MapperException {

        Map<Object, T> map = null;
        final Map<Object, S> sourceMap = (Map<Object, S>) this.reflectionUtil.invokeGetter(sourceField, sourceObject, postAction);

        if (sourceMap != null) {
            if (targetMap == null) {
                map = (Map<Object, T>) this.reflectionUtil.newInstanceMap(targetField.getType(), Object.class, sourceObject.getClass());
            } else {
                map = targetMap;
            }

            for (Entry<Object, S> entry : sourceMap.entrySet()) {
                T targetEntityInCollection = this.mapObject(entry.getValue(), identifier, deep, mode, postAction);
                if (!map.containsKey(entry.getKey())) {
                    map.put(entry.getKey(), targetEntityInCollection);
                } else {
                    map.put(entry.getKey(),
                            (T) this.mapObject(entry.getValue(), identifier, targetEntityInCollection, deep, mode, postAction));
                }
            }
        }
        return map;
    }

    /**
     * Map an object (from collection or map).
     * 
     * @param sourceObject
     *            source object
     * @param identifier
     *            identifier
     * @param deep
     *            deep
     * @param mode
     *            mapping mode
     * @return the mapped object
     * @throws MapperException
     *             core exception
     */
    @SuppressWarnings("unchecked")
    private <S, T, E extends AbstractException> T mapObject(final S sourceObject, final DTOIdentifier identifier, final int deep,
            final EnumMode mode, final FunctionThrowable<Object, Object, E> postAction) throws MapperException {
        T targetInstance = null;
        if (sourceObject != null) {
            if (this.reflectionUtil.isMappableClass(sourceObject.getClass())) {
                targetInstance = this.mapObject(sourceObject, identifier, (T) null, deep, mode, postAction);
            } else {
                targetInstance = (T) sourceObject;
            }
        }
        return targetInstance;
    }

    /**
     * Map an object.
     * 
     * @param sourceObject
     *            source object
     * @param identifier
     *            identifier
     * @param targetObject
     *            target object
     * @param deep
     *            deep
     * @param mode
     *            mapping mode
     * @return the mapped object
     * @throws MapperException
     *             core exception
     */
    @SuppressWarnings("unchecked")
    private <S, T, E extends AbstractException> T mapObject(final S sourceObject, final DTOIdentifier identifier, final T targetObject,
            final int deep, final EnumMode mode, final FunctionThrowable<Object, Object, E> postAction) throws MapperException {
        try {
            if (sourceObject != null) {
                final Class<T> targetClass = (Class<T>) this.reflectionUtil.getClassesFromObject(sourceObject.getClass())[0];

                boolean targetExists = false;
                T targetInstance = null;
                if (targetObject != null) {
                    targetInstance = targetObject;
                    targetExists = true;
                } else if (sourceObject.getClass().isEnum()) {
                    targetInstance = (T) sourceObject;
                } else {
                    targetInstance = this.reflectionUtil.newInstance(targetClass);
                }

                final Map<String, Field> sourceFields = this.reflectionUtil.getAllFields(sourceObject.getClass());
                final Map<String, Field> targetFields = this.reflectionUtil.getAllFields(targetClass);

                final Map<Field, Field> sourceTargetFields = new HashMap<>();
                if (sourceObject.getClass().getAnnotation(Mappable.class) != null) {
                    this.getFields(sourceTargetFields, sourceFields, targetFields, false, mode);
                } else if (targetClass.getAnnotation(Mappable.class) != null) {
                    this.getFields(sourceTargetFields, targetFields, sourceFields, true, mode);
                }

                this.prepareMapping(sourceObject, targetInstance, identifier, deep, mode);

                if (EnumMode.PRELOAD.getOrder() < mode.getOrder()) {
                    for (Entry<Field, Field> fields : sourceTargetFields.entrySet()) {
                        this.invokeSetterIfMappable(fields, identifier, sourceObject, targetInstance, targetExists, deep, mode, postAction);
                    }
                }

                this.postMapping(sourceObject, targetInstance, identifier, deep, mode);

                return targetInstance;
            }
            return null;
        } catch (SecurityException e) {
            throw new MapperException("Cannot map the object", e);
        }
    }

    /**
     * Invoke setter on fields if mappable.
     * 
     * @param fields
     *            fields
     * @param identifier
     *            identifier
     * @param sourceObject
     *            source object
     * @param targetInstance
     *            target instance
     * @param targetExists
     *            target exists
     * @param deep
     *            deep
     * @param mode
     *            mapping mode
     * @throws MapperException
     *             core exception
     */
    private <S, T, E extends AbstractException> void invokeSetterIfMappable(final Entry<Field, Field> fields,
            final DTOIdentifier identifier, final S sourceObject, final T targetInstance, final boolean targetExists, final int deep,
            final EnumMode mode, final FunctionThrowable<Object, Object, E> postAction) throws MapperException {

        final EnumMode mapMode = this.canMapField(fields, identifier, deep, mode);
        if (mapMode != null) {
            if (this.reflectionUtil.isGettable(fields.getKey(), sourceObject)) {
                final Object value;
                final int deepFinal;
                if (deep != DTOIdentifier.MAX_DEEP) {
                    deepFinal = deep - 1;
                } else {
                    deepFinal = DTOIdentifier.MAX_DEEP;
                }

                final Object targetObject = this.getTargetInstance(targetInstance, targetExists);

                value = this.getValue(fields.getValue(), fields.getKey(), sourceObject, identifier, targetObject, deepFinal, mapMode,
                        postAction);

                if (this.reflectionUtil.isSettable(fields.getKey(), sourceObject)) {
                    this.reflectionUtil.invokeSetter(fields.getValue(), targetInstance, value);
                }
            }
        }
    }

    /**
     * Get the target instance.
     * 
     * @param targetInstance
     *            target instance
     * @param targetExists
     *            target exists
     * @return the target instance
     */
    private <X> X getTargetInstance(final X targetInstance, final boolean targetExists) {
        X targetObject = null;
        if (targetExists) {
            if (isTargetable(targetInstance.getClass())) {
                targetObject = targetInstance;
            } else if (Collection.class.isAssignableFrom(targetInstance.getClass())) {
                targetObject = targetInstance;
            } else if (Map.class.isAssignableFrom(targetInstance.getClass())) {
                targetObject = targetInstance;
            }
        }
        return targetObject;
    }

    private boolean isTargetable(Class<?> clazz) {
        for (Class<?> targetable : this.targetables) {
            if (targetable.isAssignableFrom(clazz)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Get the fields.
     * 
     * @param sourceTargetFields
     *            source target fields
     * @param sourceFields
     *            source fields
     * @param targetFields
     *            target fields
     * @param reverse
     *            reverse
     * @param mode
     *            The target mode
     */
    private void getFields(final Map<Field, Field> sourceTargetFields, final Map<String, Field> sourceFields,
            final Map<String, Field> targetFields, final boolean reverse, final EnumMode mode) {
        for (Field sourceField : sourceFields.values()) {
            final Field targetField = this.getTargetField(sourceField, targetFields, mode);

            if (targetField != null) {
                if (reverse) {
                    sourceTargetFields.put(targetField, sourceField);
                } else {
                    sourceTargetFields.put(sourceField, targetField);
                }
            }
        }
    }

    /**
     * Get the target field.
     * 
     * @param sourceField
     *            source field
     * @param targetFields
     *            target fields
     * @param mode
     *            The target mode
     * @return the target field
     */
    private Field getTargetField(final Field sourceField, final Map<String, Field> targetFields, final EnumMode mode) {
        final List<MappableProperty> mappablesPropertySource = this.getMappablesProperty(sourceField, mode);

        Field targetField = null;
        if (!mappablesPropertySource.isEmpty() && StringUtils.isNotEmpty(mappablesPropertySource.get(0).name())) {
            targetField = targetFields.get(mappablesPropertySource.get(0).name());
        } else if (!targetFields.containsKey(sourceField.getName())) {
            for (Field targetField1 : targetFields.values()) {
                final List<MappableProperty> mappablesPropertyTarget = this.getMappablesProperty(targetField1, mode);
                if (!mappablesPropertyTarget.isEmpty() && StringUtils.isNotEmpty(mappablesPropertyTarget.get(0).name())
                        && sourceField.getName().equals(mappablesPropertyTarget.get(0).name())) {
                    targetField = targetField1;
                    break;
                }
            }
        }
        if (targetField == null) {
            targetField = targetFields.get(sourceField.getName());
        }
        return targetField;
    }

    /**
     * Get the value.
     * 
     * @param targetField
     *            target field
     * @param sourceField
     *            source field
     * @param sourceObject
     *            source object
     * @param identifier
     *            identifier
     * @param outObject
     *            out object
     * @param deep
     *            deep
     * @param mode
     *            mapping mode
     * @return the value
     * @throws MapperException
     *             map exception
     */
    @SuppressWarnings("unchecked")
    private <S, T, E extends AbstractException> Object getValue(final Field targetField, final Field sourceField, final S sourceObject,
            final DTOIdentifier identifier, final Object outObject, final int deep, final EnumMode mode,
            final FunctionThrowable<Object, Object, E> postAction) throws MapperException {
        try {
            Object value = null;
            Object targetObject = this.getTargetObject(targetField, outObject, postAction);

            if (Collection.class.isAssignableFrom(targetField.getType())) {
                value = this.mapCollection(targetField, sourceField, sourceObject, null, identifier, deep, mode, postAction);
            } else if (Map.class.isAssignableFrom(targetField.getType())) {
                value = this.mapMap(targetField, sourceField, sourceObject, null, identifier, deep, mode, postAction);
            } else if (this.reflectionUtil.isMappableField(targetField)) {
                value = this.mapObject((S) this.reflectionUtil.invokeGetter(sourceField, sourceObject, postAction), identifier,
                        (T) targetObject, deep, mode, postAction);
            } else {
                value = this.reflectionUtil.invokeGetter(sourceField, sourceObject, postAction);
            }
            return value;
        } catch (MapperException e) {
            throw e;
        }
    }

    /**
     * Get the target object.
     * 
     * @param targetField
     *            target field
     * @param outObject
     *            out object
     * @return the target object
     * @throws MapperException
     *             core exception
     */
    private <E extends AbstractException> Object getTargetObject(final Field targetField, final Object outObject,
            final FunctionThrowable<Object, Object, E> postAction) throws MapperException {
        Object targetObject = null;
        if (outObject != null) {
            if (outObject.getClass().equals(this.getMappedMirrorClass(targetField))) {
                targetObject = outObject;
            } else {
                targetObject = this.reflectionUtil.invokeGetter(targetField, outObject, postAction);
            }
        }
        return targetObject;
    }

    /**
     * Get the mapped mirror class.
     * 
     * @param sourceClass
     *            The class
     * @return the mapped mirror class
     */
    @SuppressWarnings("unchecked")
    private <X> Class<X> getMappedMirrorClass(final Class<?> sourceClass) {
        Mappable mappable = sourceClass.getAnnotation(Mappable.class);
        if (mappable != null) {
            return (Class<X>) mappable.value()[0];
        }
        return null;
    }

    /**
     * Get the mapped mirror class.
     * 
     * @param field
     *            field
     * @return the mapped mirror class
     */
    private <X> Class<X> getMappedMirrorClass(final Field field) {
        return this.getMappedMirrorClass(field.getType());
    }

    /**
     * Is a field mappable.
     * 
     * @param field
     *            field
     * @param identifier
     *            identifier
     * @param deep
     *            deep
     * @param mode
     *            mapping mode
     * @return the loading mode of the target object
     */
    private EnumMode canMapField(final Entry<Field, Field> field, final DTOIdentifier identifier, final int deep, final EnumMode mode) {
        EnumMode result = null;
        if (!Modifier.isFinal(field.getValue().getModifiers()) && (deep == DTOIdentifier.MAX_DEEP || deep > 0)) {
            if (identifier != null) {
                result = this.checkIdentifierAndGetMode(identifier, field.getValue(), mode);
                if (result != null) {
                    result = this.checkIdentifierAndGetMode(identifier, field.getKey(), mode);
                }
            } else {
                result = EnumMode.DEFAULT;
            }
        }
        return result;
    }

    /**
     * Check if the identifier is in the mapping list and return the real mode
     * to load, preload or save the properties. If the loading mode is
     * <code>MODE_LOAD</code> and the identifier is only found in the preload
     * annotation of the property, the result will be <code>MODE_PRELOAD</code>.
     * For others, case if its not found the result will be
     * <code>MODE_UNKNOWN</code>.
     * 
     * @param identifier
     *            identifier
     * @param mappable
     *            mappable
     * @param mode
     *            mapping mode
     * @return the loading mode of the target object
     */
    private EnumMode checkIdentifierAndGetMode(final DTOIdentifier identifier, final Field field, final EnumMode mode) {
        final EnumMode mapMode;

        if (EnumMode.LOAD.equals(mode) && this.checkIdentifiers(identifier, field, EnumMode.LOAD)) {
            mapMode = EnumMode.LOAD;
        } else if (EnumMode.PRELOAD.equals(mode) && this.checkIdentifiers(identifier, field, EnumMode.PRELOAD)) {
            mapMode = EnumMode.PRELOAD;
        } else if (EnumMode.SAVE.equals(mode) && this.checkIdentifiers(identifier, field, EnumMode.SAVE)) {
            mapMode = EnumMode.SAVE;
        } else {
            mapMode = null;
        }

        return mapMode;
    }

    private List<MappableProperty> getMappablesProperty(final Field field, final EnumMode mode) {
        final List<MappableProperty> mappablesProperty = new ArrayList<>();
        final List<MappableProperty> mappables = new ArrayList<>();

        MappableProperty[] mappableContainer = field.getAnnotationsByType(MappableProperty.class);
        mappables.addAll(Arrays.asList(mappableContainer));

        if (!mappables.isEmpty()) {
            if (!EnumMode.DEFAULT.equals(mode)) {
                for (MappableProperty mappable : mappables) {
                    if (Arrays.binarySearch(mappable.mode(), mode) > -1) {
                        mappablesProperty.add(mappable);
                    }
                }
            } else {
                mappablesProperty.addAll(mappables);
            }
        }

        return mappablesProperty;
    }

    /**
     * Check in the identifier tree, if the identifiers is mapped following the
     * mode (through arrays parameter)
     * 
     * @param identifier
     *            The identifier to check
     * @param mappables
     *            The mappables of identifiers following the mode (preload, load
     *            or save)
     * @param mode
     *            The target mode
     * @return
     */
    private boolean checkIdentifiers(final DTOIdentifier identifier, final Field field, final EnumMode mode) {
        final List<DTOIdentifier> mappedIdentifiers = new ArrayList<>();

        for (MappableProperty mappable : this.getMappablesProperty(field, mode)) {
            for (String input : mappable.value()) {
                if (this.dtoIdentifierManager.containsKey(input)) {
                    mappedIdentifiers.add(this.dtoIdentifierManager.get(input));
                }
            }
        }

        if (mappedIdentifiers.size() > 0) {
            for (DTOIdentifier ident : identifier.getIdentifiersRecursively()) {
                for (DTOIdentifier identRef : mappedIdentifiers) {
                    if (ident.getIdentifiersRecursively().contains(identRef)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Map an entity to a dto.
     * 
     * @param inObject
     *            in object
     * @param outObject
     *            out object
     * @param identifier
     *            identifier
     * @param deep
     *            deep
     * @param mode
     *            mapping mode
     * @param <S>
     *            The souce type
     * @param <T>
     *            The target type
     * @return the dto
     * @throws MapperException
     *             core exception
     */
    protected <S, T, E extends AbstractException> T map(final S inObject, final T outObject, final DTOIdentifier identifier, final int deep,
            final EnumMode mode, final FunctionThrowable<Object, Object, E> postAction) throws MapperException {
        try {
            T result = outObject;
            if (inObject != null) {
                result = this.mapObject(inObject, identifier, result, deep, mode, postAction);
            }
            return result;
        } catch (MapperException e) {
            throw e;
        }
    }

    /**
     * Map an entity to a dto.
     * 
     * @param inObject
     *            in object
     * @param outObject
     *            out object
     * @param identifier
     *            identifier
     * @param deep
     *            deep
     * @param mode
     *            mapping mode
     * @param <S>
     *            The souce type
     * @param <T>
     *            The target type
     * @return the dto
     * @throws MapperException
     *             core exception
     */
    protected <S, T> T map(final S inObject, final T outObject, final DTOIdentifier identifier, final int deep, final EnumMode mode)
            throws MapperException {
        return this.map(inObject, outObject, identifier, deep, mode, null);
    }

    /**
     * Prepare the mapping
     * 
     * @param sourceObject
     *            The source instance
     * @param targetObject
     *            The target instance
     * @param identifier
     *            The identifier
     * @param deep
     *            The current deep of loading (No limit = -1)
     * @param mode
     *            mapping mode (preload, load or save)
     * @param <A>
     *            The source type
     * @param <B>
     *            The target type
     */
    protected abstract <A, B> void prepareMapping(final A sourceObject, final B targetObject, final DTOIdentifier identifier,
            final int deep, final EnumMode mode);

    /**
     * Process post mapping
     * 
     * @param sourceObject
     *            The source instance
     * @param targetObject
     *            The target instance
     * @param identifier
     *            The identifier
     * @param deep
     *            The current deep of loading (No limit = -1)
     * @param mode
     *            mapping mode (preload, load or save)
     * @param <A>
     *            The source type
     * @param <B>
     *            The target type
     */
    protected abstract <A, B> void postMapping(final A sourceObject, final B targetObject, final DTOIdentifier identifier, final int deep,
            final EnumMode mode);
}