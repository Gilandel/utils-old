/*
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
package fr.landel.utils.model.dialect;

import org.hibernate.dialect.HSQLDialect;

/**
 * Workaround.
 * 
 * @see <a href="https://hibernate.onjira.com/browse/HHH-7002">JIRA HHH-7002</a>
 *
 * @since 10 déc. 2015
 * @author Gilles
 *
 */
public class ImprovedHSQLDialect extends HSQLDialect {

    @Override
    public String getDropSequenceString(String sequenceName) {
        return super.getDropSequenceString("if exists " + sequenceName);
    }

    @Override
    public boolean dropConstraints() {
        return false;
    }

    @Override
    public boolean supportsIfExistsBeforeTableName() {
        return true;
    }

    @Override
    public boolean supportsIfExistsAfterTableName() {
        return false;
    }

    @Override
    public String getCascadeConstraintsString() {
        return " cascade ";
    }
}
