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
package fr.landel.utils.commons.builder;

import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;

import fr.landel.utils.commons.StringUtils;

/**
 * ToString base style
 *
 * @since Mar 5, 2017
 * @author Gilles
 *
 */
public abstract class AbstractToStringStyle extends ArrayList<CharSequence> implements ToStringStyle {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 8130375854086601461L;

    private CharSequence title;

    @Override
    public ToStringStyle setObject(final Object object) {
        Objects.requireNonNull(object);

        if (object instanceof Class) {
            this.title = ((Class<?>) object).getCanonicalName();
        } else if (CharSequence.class.isAssignableFrom(object.getClass())) {
            this.title = String.valueOf(object);
        } else {
            this.title = object.getClass().getCanonicalName();
        }
        return this;
    }

    @Override
    public <T> void append(final T object) {
        this.add(new StringBuilder(this.getValueStart()).append(object).append(this.getValueEnd()));
    }

    @Override
    public <T> void append(final T object, final Function<T, CharSequence> formatter) {
        this.append(formatter.apply(object));
    }

    @Override
    public <T> void append(final CharSequence key, final T value) {
        this.add(new StringBuilder(this.getKeyStart()).append(key).append(this.getKeyEnd()).append(this.getPropertySeparator())
                .append(this.getValueStart()).append(value).append(this.getValueEnd()));
    }

    @Override
    public <T> void append(final CharSequence key, final T value, final Function<T, CharSequence> formatter) {
        this.append(key, formatter.apply(value));
    }

    @Override
    public <T> void append(final Supplier<T> value) {
        this.append(value.get());
    }

    @Override
    public <T> void append(final Supplier<T> value, final Function<T, CharSequence> formatter) {
        this.append(formatter.apply(value.get()));
    }

    @Override
    public <T> void append(final CharSequence key, final Supplier<T> value) {
        this.append(key, value.get());
    }

    @Override
    public <T> void append(final CharSequence key, final Supplier<T> value, final Function<T, CharSequence> formatter) {
        this.append(key, formatter.apply(value.get()));
    }

    @Override
    public <T> void appendIfPresent(final Optional<T> value) {
        this.appendIfPresent(value, null);
    }

    @Override
    public <T> void appendIfPresent(final Optional<T> value, final Function<T, CharSequence> formatter) {
        if (value.isPresent()) {
            if (formatter != null) {
                this.append(formatter.apply(value.get()));
            } else {
                this.append(value.get());
            }
        }
    }

    @Override
    public <T> void appendIfPresent(final CharSequence key, final Optional<T> value) {
        this.appendIfPresent(key, value, null);
    }

    @Override
    public <T> void appendIfPresent(final CharSequence key, final Optional<T> value, final Function<T, CharSequence> formatter) {
        if (value.isPresent()) {
            if (formatter != null) {
                this.append(key, formatter.apply(value.get()));
            } else {
                this.append(key, value.get());
            }
        }
    }

    @Override
    public String build() {
        final StringBuilder builder = new StringBuilder(this.getTitleStart()).append(this.title).append(this.getTitleEnd());

        if (!this.isEmpty()) {
            builder.append(this.getPropertiesStart());
            builder.append(StringUtils.join(this, this.getPropertiesSeparator()));
            builder.append(this.getPropertiesEnd());
        }

        return builder.toString();
    }

    /**
     * @return the title start tag
     */
    protected abstract String getTitleStart();

    /**
     * @return the title end tag
     */
    protected abstract String getTitleEnd();

    /**
     * @return the global properties start tag
     */
    protected abstract String getPropertiesStart();

    /**
     * @return the key start tag
     */
    protected abstract String getKeyStart();

    /**
     * @return the key end tag
     */
    protected abstract String getKeyEnd();

    /**
     * @return the property separator (ex: '=')
     */
    protected abstract String getPropertySeparator();

    /**
     * @return the value start tag
     */
    protected abstract String getValueStart();

    /**
     * @return the value end tag
     */
    protected abstract String getValueEnd();

    /**
     * @return the properties separator (ex: ',')
     */
    protected abstract String getPropertiesSeparator();

    /**
     * @return the global properties end tag
     */
    protected abstract String getPropertiesEnd();
}
