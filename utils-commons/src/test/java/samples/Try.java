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
import java.util.Optional;

import fr.landel.utils.commons.ClassUtils;
import fr.landel.utils.commons.function.ThrowableSupplier;

/**
 * Just a try test for {@link ThrowableSupplier}
 * 
 * <p>
 * Not good for production, because this hides bugs. (can only be used, if a
 * library throws {@link Exception} or {@link Throwable}...)
 * </p>
 *
 * @since Aug 11, 2016
 * @author Gilles
 *
 */
public class Try {

    @SuppressWarnings("unchecked")
    public static <E extends Throwable> Optional<Catch<E>> that(final ThrowableSupplier<E> exceptionSupplier) {
        Objects.requireNonNull(exceptionSupplier);

        try {
            exceptionSupplier.throwException();
        } catch (Throwable e) {
            return Optional.of(new Catch<>((E) e));
        }
        return Optional.empty();
    }

    public static class Catch<E extends Throwable> {
        private final E exception;
        private final Class<E> clazz;

        private Catch(final E exception) {
            this.exception = exception;
            this.clazz = ClassUtils.getClass(exception);
        }

        public boolean is(final Class<?> clazz) {
            Objects.requireNonNull(clazz);

            return clazz.isAssignableFrom(this.clazz);
        }

        public boolean has(final String message) {
            if (message != null) {
                return message.equals(this.exception.getMessage());
            }
            return this.exception.getMessage() == null;
        }

        public E get() {
            return this.exception;
        }
    }
}
