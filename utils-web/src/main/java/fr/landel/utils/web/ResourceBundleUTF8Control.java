/*
 * #%L
 * utils-web
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.web;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Locale;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;
import java.util.ResourceBundle.Control;

import fr.landel.utils.io.EncodingUtils;

/**
 * UTF-8 resources bundle loader.
 *
 * @see <a href="https://gist.github.com/DemkaAge/8999236">https://gist.github.
 *      com/DemkaAge/8999236</a>
 * 
 * @since 27 nov. 2015
 * @author Gilles
 *
 */
public class ResourceBundleUTF8Control extends Control {

    @Override
    public ResourceBundle newBundle(final String baseName, final Locale locale, final String format, final ClassLoader loader,
            final boolean reload) throws IllegalAccessException, InstantiationException, IOException {

        // The below is a copy of the default implementation.
        String bundleName = toBundleName(baseName, locale);
        String resourceName = toResourceName(bundleName, "properties");
        ResourceBundle bundle = null;

        try (InputStream stream = this.getResourceStream(resourceName, loader, reload)) {
            // Only this line is changed to make it to read properties files
            // as UTF-8.
            if (stream != null) {
                bundle = new PropertyResourceBundle(new InputStreamReader(stream, EncodingUtils.CHARSET_UTF_8));
            }
        }
        return bundle;
    }

    private InputStream getResourceStream(final String resourceName, final ClassLoader loader, final boolean reload) throws IOException {
        InputStream stream = null;
        if (reload) {
            URL url = loader.getResource(resourceName);
            if (url != null) {
                URLConnection connection = url.openConnection();
                if (connection != null) {
                    connection.setUseCaches(false);
                    stream = connection.getInputStream();
                }
            }
        } else {
            stream = loader.getResourceAsStream(resourceName);
        }
        return stream;
    }
}
