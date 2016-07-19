/*
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

import org.apache.commons.lang3.ArrayUtils;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.StringDescription;

import fr.landel.utils.commons.CastGenerics;
import fr.landel.utils.commons.ClassUtils;
import fr.landel.utils.commons.stream.FunctionThrowable;

/**
 * Assertion utility class that assists in validating arguments for objects.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <A>
 *            the assert class type
 * @param <T>
 *            The object type
 */
public class AssertObject<A extends AssertObject<A, T>, T> {

    private final T object;
    private final Class<T> clazz;
    private final Operator<A, T> operator;
    private int paramIndex;
    private StringBuilder param;
    private Object[] parameters;
    private int condition;
    private boolean valid = true;
    private StringBuilder message;
    private AssertObject<?, ?> previousAssertor;
    private boolean not;

    /**
     * 
     * Constructor
     *
     * @param object
     *            The object to check
     */
    @SuppressWarnings("unchecked")
    protected AssertObject(final T object) {
        this.object = object;
        this.clazz = CastGenerics.getClass(object);
        this.operator = new Operator<>((A) this);
        this.parameters = new Object[] {object};
        this.paramIndex = 1;
        this.param = AssertObject.getParam(this.paramIndex);
        this.message = new StringBuilder();
    }

    /**
     * Clear the assertor
     */
    protected void clear() {
        this.parameters = new Object[] {this.object};
        this.paramIndex = 1;
        this.param = AssertObject.getParam(this.paramIndex);
        this.message = new StringBuilder();
        this.valid = true;
        this.condition = Assertor.AND;
        this.not = false;

        if (this.previousAssertor != null) {
            this.previousAssertor.clear();
        }
    }

    /**
     * @return the paramIndex
     */
    protected int getParamIndex() {
        return this.paramIndex;
    }

    /**
     * @return the param
     */
    protected StringBuilder getParam() {
        return this.param;
    }

    /**
     * @return The previous condition value
     */
    protected int getCondition() {
        return this.condition;
    }

    /**
     * @param condition
     *            The previous condition value
     */
    protected void setCondition(final int condition) {
        this.condition = condition;
    }

    /**
     * @return the valid
     */
    protected boolean isValid() {
        return this.valid;
    }

    /**
     * @param valid
     *            the valid to set
     */
    protected void setValid(final boolean valid) {
        this.valid = valid;
    }

    /**
     * @return the operator
     */
    protected Operator<A, T> getOperator() {
        return this.operator;
    }

    /**
     * @return the object to check
     */
    protected T get() {
        return this.object;
    }

    /**
     * @return the class object to check
     */
    protected Class<T> getObjectClass() {
        return this.clazz;
    }

    /**
     * @return the message
     */
    protected StringBuilder getMessage() {
        return this.message;
    }

    /**
     * @return the arguments
     */
    protected Object[] getParameters() {
        return this.parameters;
    }

    /**
     * Combine the current boolean value with the new one
     * 
     * @param assertor
     *            The previous assertor
     * @return The operator
     */
    protected Operator<A, T> combine(final A assertor) {
        if (this.not) {
            throw new IllegalArgumentException("'Not' cannot be followed by a condition");
        }
        // reset the valid state
        this.valid = assertor.isValid();
        // set the previous for clearing
        this.previousAssertor = assertor;
        final Operator<A, T> operator = this.combine(assertor.isValid(), assertor.getMessage());
        // override parameters to keep order
        this.parameters = ArrayUtils.addAll(assertor.getParameters(), this.parameters);
        // prepare variables (for easy access)
        this.paramIndex = this.parameters.length;
        this.param = AssertObject.getParam(this.paramIndex);
        return operator;
    }

    /**
     * Combine the current boolean value with the new one
     * 
     * @param condition
     *            The new condition
     * @param message
     *            The message on error
     * @param parameters
     *            The parameters
     * @return The operator
     */
    protected Operator<A, T> combine(final boolean condition, final CharSequence message, final Object... parameters) {
        final boolean c;

        if (this.not) {
            c = !condition;
            this.not = false;
        } else {
            c = condition;
        }

        switch (this.condition) {
        case Assertor.AND:
            this.valid &= c;
            break;
        case Assertor.OR:
            this.valid |= c;
            break;
        case Assertor.XOR:
            this.valid = (this.valid && !c) || (!this.valid && c);
            break;
        default:
        }

        if (!this.valid) {
            if (this.message.length() > 0) {
                this.message.append(Assertor.OPERATORS[this.condition]);
            }
            this.message.append(message.toString());
        }

        this.parameters = ArrayUtils.addAll(this.parameters, parameters);

        return this.getOperator();
    }

    /**
     * Apply a not on the next condition.
     * 
     * <pre>
     * Assertor.that(object).not().isNull().toThrow();
     * // Same expression
     * Assertor.that(object).isNotNull().toThrow();
     * </pre>
     * 
     * @return This
     */
    @SuppressWarnings("unchecked")
    public A not() {
        this.not = !this.not;
        return (A) this;
    }

    /**
     * Assert that an object is {@code null}.
     * 
     * <pre>
     * Assertor.that(object).isNull().toThrow(anException);
     * </pre>
     * 
     * @return The operator
     */
    public Operator<A, T> isNull() {
        return this.combine(object == null, new StringBuilder("the object argument '").append(this.getParam()).append("' must be null"));
    }

    /**
     * Assert that an object is not {@code null}.
     * 
     * <pre>
     * Assertor.that(object).isNotNull().toThrow(anException);
     * </pre>
     * 
     * @return The operator
     */
    public Operator<A, T> isNotNull() {
        return this.combine(object != null,
                new StringBuilder("the object argument '").append(this.getParam()).append("' must be not null"));
    }

    /**
     * Assert that the first object is not equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * Assertor.that(object1).isNotEqual(object2);
     * </pre>
     * 
     * @param object
     *            the second object
     * @return the operator
     */
    public Operator<A, T> isNotEqual(final Object object) {
        boolean status = true;
        final StringBuilder secondObject = AssertObject.getParam(this.getParamIndex() + 1);
        StringBuilder message = new StringBuilder("Objects ['").append(this.getParam()).append("', '").append(secondObject)
                .append("'] are equal");

        if (this.object == null && object == null) {
            status = false;
            message = new StringBuilder("Both objects are null");

        } else if (this.object != null && object != null) {
            if (this.object.equals(object)) {
                message = new StringBuilder("Object '").append(this.getParam()).append("' is equal to Object '").append(secondObject)
                        .append("'");
                status = false;

            } else if (CharSequence.class.isAssignableFrom(this.object.getClass()) && CharSequence.class.isAssignableFrom(object.getClass())
                    && this.object.toString().equals(object.toString())) {
                message = new StringBuilder("CharSequence '").append(this.getParam()).append("' is equal to CharSequence '")
                        .append(secondObject).append("'");
                status = false;
            }
        }

        return this.combine(status, message, object);
    }

    /**
     * Assert that the first object is equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * Assertor.that(object1).isEqual(object2);
     * </pre>
     * 
     * @param object
     *            the second object
     * @return the operator
     */
    public Operator<A, T> isEqual(final Object object) {
        boolean status = true;
        final StringBuilder secondObject = AssertObject.getParam(this.getParamIndex() + 1);
        StringBuilder message = new StringBuilder("Objects ['").append(this.getParam()).append("', '").append(secondObject)
                .append("'] are not equal");

        if (this.object != null && object != null && !this.object.equals(object)) {
            if (!CharSequence.class.isAssignableFrom(this.object.getClass()) || !CharSequence.class.isAssignableFrom(object.getClass())
                    || !this.object.toString().equals(object.toString())) {
                message = new StringBuilder("Object '" + this.getParam() + "' is not equal to Object '" + secondObject).append("'");
                status = false;

            }
        } else if (this.object == null && object != null) {
            message = new StringBuilder("Object '" + this.getParam()).append("' is null but not Object '" + secondObject).append("'");
            status = false;

        } else if (this.object != null && object == null) {
            message = new StringBuilder("Object '").append(secondObject).append("' is null but not Object '").append(this.getParam())
                    .append("'");
            status = false;

        }

        return this.combine(status, message, object);
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * Assertor.that(object).is(Foo.class);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @return the operator
     * @see Class#isInstance
     */
    public Operator<A, T> isInstanceOf(final Class<?> type) {

        Assertor.that(type).isNotNull().toThrow("Type to check against must not be null");

        final StringBuilder sb = new StringBuilder("Object of class [");
        sb.append(ClassUtils.getName(this.object));
        sb.append("] must be an instance of '%" + (this.getParamIndex() + 1) + "$p'");

        return this.combine(type.isInstance(this.object), sb, type);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass), exceptionToThrowOnError);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @return the operator
     */
    public Operator<A, T> isAssignableFrom(final Class<?> type) {
        return this.combine(AssertClass.isAssignable(type, CastGenerics.getClass(this.object)),
                "Type '%" + (this.getParamIndex() + 1) + "$p' is not assignable", type);
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param matcher
     *            an hamcrest matcher
     * @return the operator
     */
    public Operator<A, T> matches(final Matcher<? super T> matcher) {
        if (!matcher.matches(this.object)) {
            final Description description = new StringDescription();
            description.appendText("Expected: ");
            description.appendDescriptionOf(matcher).appendText("\n     but: ");
            matcher.describeMismatch(this.object, description);

            return this.combine(false, description.toString());
        } else {
            return this.combine(true, "");
        }
    }

    /**
     * Validates the current object through a checker function.
     * 
     * <pre>
     * Assertor.that(object).validate((Object obj) -&gt; {
     *     return obj != null;
     * }).toThrow();
     * 
     * Assertor.that("/var/log/dev.log").validate((String path) -&gt; {
     *     return Paths.get(path).endsWith("dev.log");
     * }).getResult();
     * 
     * // Exceptions are catched, and returns a false result
     * Assertor.that("/var/log/dev.log").validate((String path) -&gt; {
     *     if (!new File(path).exists()) {
     *         throw new IOException();
     *     }
     *     return true;
     * }).getResult();
     * </pre>
     * 
     * @param function
     *            a check function
     * @return the operator
     */
    public <E extends Throwable> Operator<A, T> validate(final FunctionThrowable<T, Boolean, E> function) {
        try {
            return this.combine(function.applyThrows(this.get()), "The function returns that the element is invalid");
        } catch (Throwable e) {
            return this.combine(false, new StringBuilder("The function raises an '").append(ClassUtils.getName(e)).append("' exception"));
        }
    }

    /**
     * @param index
     *            the index
     * @return the parameter string
     */
    protected static StringBuilder getParam(final int index) {
        return new StringBuilder("%").append(index).append("$p");
    }
}
