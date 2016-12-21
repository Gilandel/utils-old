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
package fr.landel.utils.mapper.core;

import fr.landel.utils.commons.exception.AbstractException;
import fr.landel.utils.commons.function.FunctionThrowable;
import fr.landel.utils.mapper.DTOIdentifier;
import fr.landel.utils.mapper.EnumMode;
import fr.landel.utils.mapper.MapperException;

/**
 * Reflective mapper (DTO &lt;-&gt; DTO)
 *
 * @since Jul 29, 2015
 * @author Gilles
 *
 */
public class ReflectiveMapper extends AbstractReflectiveMapper {

    @Override
    public <S, T> T map(final S inObject, final T outObject, final DTOIdentifier identifier, final int deep, final EnumMode mode)
            throws MapperException {
        return super.map(inObject, outObject, identifier, deep, mode);
    }

    @Override
    public <S, T, E extends AbstractException> T map(final S inObject, final T outObject, final DTOIdentifier identifier, final int deep,
            final EnumMode mode, final FunctionThrowable<Object, Object, E> postAction) throws MapperException {
        return super.map(inObject, outObject, identifier, deep, mode, postAction);
    }

    @Override
    protected <S, T> void prepareMapping(final S sourceObject, final T targetObject, final DTOIdentifier identifier, final int deep,
            final EnumMode mode) {
        // DO nothing
    }

    @Override
    protected <S, T> void postMapping(final S sourceObject, final T targetObject, final DTOIdentifier identifier, final int deep,
            final EnumMode mode) {
        // DO nothing
    }
}
