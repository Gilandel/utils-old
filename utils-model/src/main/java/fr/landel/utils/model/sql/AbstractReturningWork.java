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
package fr.landel.utils.model.sql;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.hibernate.jdbc.ReturningWork;

import fr.landel.utils.commons.StringUtils;

/**
 * Abstract returning work (manage the injection of parameters in native SQL
 * scripts, and multi-statements in one query (ex: insert in temporary table
 * (create view like) and select))
 *
 * @since Dec 1, 2015
 * @author Gilles
 *
 * @param <T>
 *            The return type
 */
public abstract class AbstractReturningWork<T> implements ReturningWork<T> {

    @Override
    public final T execute(final Connection connection) throws SQLException {
        final CharSequence query = this.preparedQuery();

        PreparedStatement stmt = connection.prepareStatement(query.toString());

        final int countStatments = StringUtils.countMatches(query, ';');

        this.defineParameters(stmt);

        boolean isResult = stmt.execute();

        // Specific code to manage JDBC results...
        // when you have multiple statements in one query, the first return
        // false, you need to loop over the result until it becomes true.
        // Theoretically the sub-queries (ex: insert into temporary tables),
        // return false and an update result over 0,
        // but in our case we have also use sub-queries like 'select', and to
        // avoid an infinite loop,
        // we limit the number of loop to the the number of statements (guess
        // based on the count of semicolons).

        // Skip over update counts, limited by the number of statements.
        int loop = 0;
        while (!isResult) {
            loop++;

            if (stmt.getUpdateCount() <= 0 && countStatments < loop) {
                // End of results.
                return null;
            }
            isResult = stmt.getMoreResults();
        }

        if (isResult) {
            final ResultSet rs = stmt.getResultSet();
            if (rs != null) {
                final T result = this.processResult(rs);
                // Checks the reference (null or anything)
                if (!rs.equals(result)) {
                    rs.close();
                }
                return result;
            }
        }

        return null;
    }

    /**
     * Prepares the query to create the prepared statement
     * 
     * @return The query
     */
    public abstract CharSequence preparedQuery();

    /**
     * Defines the parameters
     * 
     * @param stmt
     *            The statement
     * @throws SQLException
     *             On defining error
     */
    public abstract void defineParameters(PreparedStatement stmt) throws SQLException;

    /**
     * Process the result set into the waiting object
     * 
     * @param rs
     *            The result set
     * @return The typed object
     * @throws SQLException
     *             On processing error
     */
    public abstract T processResult(ResultSet rs) throws SQLException;
}
