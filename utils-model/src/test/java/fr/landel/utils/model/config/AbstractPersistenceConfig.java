/*-
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
package fr.landel.utils.model.config;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Properties;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;

import org.apache.commons.lang3.StringUtils;
import org.apache.tomcat.dbcp.dbcp2.BasicDataSource;
import org.apache.tomcat.dbcp.dbcp2.BasicDataSourceFactory;
import org.hibernate.dialect.PostgreSQL9Dialect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.core.env.Environment;
import org.springframework.core.env.StandardEnvironment;
import org.springframework.core.io.support.ResourcePropertySource;
import org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.JpaVendorAdapter;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.Database;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;

import com.fasterxml.jackson.core.JsonParseException;

import fr.landel.utils.model.exception.ModelException;

/**
 * Persistence configuration.
 * 
 * ignoreResourceNotFound only for tests
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
public abstract class AbstractPersistenceConfig {

    /**
     * Key in property file
     */
    protected static final String PACKAGES_MODEL_TO_SCAN = "model.packages.to.scan";

    /**
     * list of packages to scan to find models
     */
    protected static String packagesToScan;

    private static Properties hibernateProperties;

    @Autowired
    private Environment env;

    /**
     * Creates the transaction manager
     * 
     * @param emf
     *            The entity manager factory
     * @return The transaction manager bean
     */
    @Bean
    public PlatformTransactionManager transactionManager(final EntityManagerFactory emf) {
        return new JpaTransactionManager(emf);
    }

    /**
     * Creates the entity manager factory
     * 
     * @param dataSource
     *            The data source
     * @return The entity manager factory bean
     * @throws ModelException
     *             On bean initialization failed
     */
    @Bean
    public LocalContainerEntityManagerFactoryBean entityManagerFactory(final DataSource dataSource) throws ModelException {
        final LocalContainerEntityManagerFactoryBean entityManagerFactory = new LocalContainerEntityManagerFactoryBean();

        entityManagerFactory.setDataSource(dataSource);

        entityManagerFactory.setJpaVendorAdapter(jpaVendorAdapter());
        entityManagerFactory.setJpaProperties(hibernateProperties(this.env));
        entityManagerFactory.setPackagesToScan(StringUtils.split(packagesToScan, ','));

        return entityManagerFactory;
    }

    /**
     * Creates the JPA vendor
     * 
     * @return The JPA vendor bean
     */
    private static JpaVendorAdapter jpaVendorAdapter() {
        HibernateJpaVendorAdapter adapter = new HibernateJpaVendorAdapter();

        adapter.setDatabase(Database.POSTGRESQL);
        adapter.setShowSql(true);
        adapter.setGenerateDdl(true);

        adapter.setDatabasePlatform(PostgreSQL9Dialect.class.getCanonicalName());

        return adapter;
    }

    /**
     * Creates the bean to manage the translation of exceptions
     * 
     * @return The exception translation bean
     */
    @Bean
    public PersistenceExceptionTranslationPostProcessor exceptionTranslation() {
        return new PersistenceExceptionTranslationPostProcessor();
    }

    /**
     * Creates the data source, first from environment variable "jndi" (from
     * application properties) and if not found try to load the data source from
     * the properties file
     * 
     * @return The data source bean
     * @throws ModelException
     *             On jndi error (mismatch type and not found)
     * @throws IOException
     *             On IO error
     * @throws MalformedURLException
     *             If JSON URL is malformed
     * @throws JsonParseException
     *             On JSON parsing error
     */
    @Bean
    public DataSource dataSource() throws ModelException, JsonParseException, MalformedURLException, IOException {
        BasicDataSource dataSource = getDataSourceFromProperties();

        if (this.env.containsProperty("jdbc.initPoolSize")) {
            dataSource.setInitialSize(Integer.parseInt(this.env.getProperty("jdbc.initPoolSize")));
        } else {
            final int defaultInitPoolSize = 5;
            dataSource.setInitialSize(defaultInitPoolSize);
        }

        return dataSource;
    }

    private BasicDataSource getDataSourceFromProperties() throws ModelException {
        final String username = this.env.getProperty("jdbc.username");
        final String password = this.env.getProperty("jdbc.password");
        final String jdbcUrl = this.env.getProperty("jdbc.url");

        return getDataSource(username, password, jdbcUrl);
    }

    private BasicDataSource getDataSource(final String username, final String password, final String jdbcUrl) throws ModelException {

        final Properties properties = new Properties();

        properties.setProperty("driverClassName", this.env.getProperty("jdbc.driverClassName"));
        properties.setProperty("username", username);
        properties.setProperty("password", password);
        properties.setProperty("url", jdbcUrl);

        if (this.env.containsProperty("jdbc.maxIdle")) {
            properties.setProperty("maxIdle", this.env.getProperty("jdbc.maxIdle"));
        }
        if (this.env.containsProperty("jdbc.maxTotal")) {
            properties.setProperty("maxTotal", this.env.getProperty("jdbc.maxTotal"));
        }
        if (this.env.containsProperty("jdbc.maxWaitMillis")) {
            properties.setProperty("maxWaitMillis", this.env.getProperty("jdbc.maxWaitMillis"));
        }

        try {
            return BasicDataSourceFactory.createDataSource(properties);
        } catch (Exception e) {
            throw new ModelException("Cannot create datasource", e);
        }
    }

    private static Properties hibernateProperties(final Environment env) {
        if (hibernateProperties == null) {
            final String hibernatePrefix = "hibernate.";
            hibernateProperties = new Properties();

            final StandardEnvironment ssEnv = (StandardEnvironment) env;
            final Iterator<org.springframework.core.env.PropertySource<?>> iterator = ssEnv.getPropertySources().iterator();
            while (iterator.hasNext()) {
                final org.springframework.core.env.PropertySource<?> propertySource = iterator.next();
                if (ResourcePropertySource.class.isAssignableFrom(propertySource.getClass())) {
                    final ResourcePropertySource resoucePropertySource = (ResourcePropertySource) propertySource;

                    for (final Entry<String, Object> entry : resoucePropertySource.getSource().entrySet()) {
                        if (entry.getKey().startsWith(hibernatePrefix)) {
                            hibernateProperties.setProperty(entry.getKey(), (String) entry.getValue());
                        }
                    }

                    resoucePropertySource.getSource();
                }
            }
        }

        return hibernateProperties;
    }
}
