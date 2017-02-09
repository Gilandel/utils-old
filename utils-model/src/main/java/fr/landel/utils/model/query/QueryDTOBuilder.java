/*-
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.query;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import fr.landel.utils.model.AbstractEntity;

public class QueryDTOBuilder<D> implements Serializable {

    private Class<D> classDTO;
    private List<CharSequence> list;

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 2740314136451683490L;

    public QueryDTOBuilder(Class<D> classDTO) {
        this.list = new ArrayList<>();
        this.classDTO = classDTO;
    }

    public static <D> QueryDTOBuilder<D> of(final Class<D> entityClass) {
        return new QueryDTOBuilder<>(entityClass);
    }

    public QueryDTOBuilder<D> append(final CharSequence sequence) {
        this.list.add(sequence);
        return this;
    }

    public QueryDTOBuilder<D> append(final AbstractQueryBuilder1 builder) {
        this.list.add(builder.toString());
        return this;
    }

    public <E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> QueryDTOBuilder<D> append(
            final AbstractBuilder<E, K> builder) {
        this.list.add(
                new StringBuilder(AbstractQueryBuilder1.PARENTHESIS_OPEN).append(builder).append(AbstractQueryBuilder1.PARENTHESIS_CLOSE));
        return this;
    }

    public QueryDTO build() {
        return new QueryDTO(this.classDTO, this.list);
    }
}
