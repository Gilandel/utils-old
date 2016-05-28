/*
 * #%L
 * utils-io
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

/**
 * This class is used to:<br>
 * - move or copy a file or a directory,<br>
 * - delete a directory from disk
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class FileSystemUtils {

    /**
     * The line separator string (windows = \r\n, unix = \n, macOS = \r)
     */
    public static final String LINE_SEPARATOR = System.getProperty("line.separator");

    private static final int BUFFER_SIZE = 10240;
    private static final byte[] BUFFER = new byte[BUFFER_SIZE];

    private static final String ERROR_PARAM_NULL = "At least one parameter is null";

    private static final Pattern PATTERN_SPECIAL_CHARACTERS = Pattern.compile("[\\\\/:*?\"<>|]");

    /**
     * Constructor.
     *
     */
    private FileSystemUtils() {
    }

    /**
     * Replace all special characters in filename by the replacement string.
     * 
     * @param filename
     *            The input filename
     * @param replacement
     *            The replacement string
     * @return The filename processed
     */
    public static String replaceSpecialCharacters(final String filename, final String replacement) {
        if (replacement != null) {
            return PATTERN_SPECIAL_CHARACTERS.matcher(filename).replaceAll(replacement);
        }
        return PATTERN_SPECIAL_CHARACTERS.matcher(filename).replaceAll("");
    }

    /**
     * Move a file.
     * 
     * @param src
     *            The source file name
     * @param dest
     *            The destination file name
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void moveFile(final String src, final String dest) throws IOException {
        copyFile(src, dest, true);
    }

    /**
     * Move a file.
     * 
     * @param src
     *            The source file name
     * @param dest
     *            The destination file name
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void moveFile(final File src, final File dest) throws IOException {
        copyFile(src, dest, true);
    }

    /**
     * Copy a file.
     * 
     * @param src
     *            The source file name
     * @param dest
     *            The destination file name
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void copyFile(final String src, final String dest) throws IOException {
        copyFile(src, dest, false);
    }

    /**
     * Copy a file.
     * 
     * @param src
     *            The source file name
     * @param dest
     *            The destination file name
     * @param removeSource
     *            Remove the source after copy
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    private static void copyFile(final String src, final String dest, final boolean removeSource) throws IOException {
        if (src != null && dest != null) {
            final File srcFile = new File(src);
            final File dstFile = new File(dest);

            if (dstFile != null && (dstFile.getParentFile().isDirectory() || dstFile.getParentFile().mkdirs())) {
                copyFile(srcFile, dstFile, removeSource);
            }
            return;
        }
        throw new FileNotFoundException(ERROR_PARAM_NULL);
    }

    /**
     * Copy a file.
     * 
     * @param src
     *            The source file name
     * @param dest
     *            The destination file name
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void copyFile(final File src, final File dest) throws IOException {
        copyFile(src, dest, false);
    }

    /**
     * Copy a file.
     * 
     * @param src
     *            The source file name
     * @param dest
     *            The destination file name
     * @param removeSource
     *            Remove the source after copy
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    private static void copyFile(final File src, final File dest, final boolean removeSource) throws IOException {
        int bufferReadSize;

        final File target;
        if (dest.isDirectory()) {
            target = new File(dest, src.getName());
        } else {
            target = dest;
        }

        final BufferedInputStream bis = StreamUtils.createBufferedInputStream(src);
        final BufferedOutputStream bos = StreamUtils.createBufferedOutputStream(target);

        while ((bufferReadSize = bis.read(BUFFER, 0, BUFFER_SIZE)) >= 0) {
            bos.write(BUFFER, 0, bufferReadSize);
        }

        CloseableManager.close(target);
        CloseableManager.close(src);

        if (removeSource && !src.delete()) {
            throw new IOException("Cannot remove the source file");
        }
    }

    /**
     * Move a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void moveDirectory(final String src, final String dest) throws IOException {
        copyDirectory(src, dest, null, null, true);
    }

    /**
     * Move a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @param fileFilter
     *            The filter to limit file to be copied
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void moveDirectory(final String src, final String dest, final FileFilter fileFilter) throws IOException {
        copyDirectory(src, dest, fileFilter, null, true);
    }

    /**
     * Move a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @param filenameFilter
     *            The filename filter to limit file to be copied
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void moveDirectory(final String src, final String dest, final FilenameFilter filenameFilter) throws IOException {
        copyDirectory(src, dest, null, filenameFilter, true);
    }

    /**
     * Move a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void moveDirectory(final File src, final File dest) throws IOException {
        copyDirectory(src, dest, null, null, true);
    }

    /**
     * Move a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @param fileFilter
     *            The filter to limit file to be copied
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void moveDirectory(final File src, final File dest, final FileFilter fileFilter) throws IOException {
        copyDirectory(src, dest, fileFilter, null, true);
    }

    /**
     * Move a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @param filenameFilter
     *            The filename filter to limit file to be copied
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void moveDirectory(final File src, final File dest, final FilenameFilter filenameFilter) throws IOException {
        copyDirectory(src, dest, null, filenameFilter, true);
    }

    /**
     * Copy a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void copyDirectory(final String src, final String dest) throws IOException {
        copyDirectory(src, dest, null, null, false);
    }

    /**
     * Copy a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @param fileFilter
     *            The filter to limit file to be copied
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void copyDirectory(final String src, final String dest, final FileFilter fileFilter) throws IOException {
        copyDirectory(src, dest, fileFilter, null, false);
    }

    /**
     * Copy a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @param filenameFilter
     *            The filename filter to limit file to be copied
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void copyDirectory(final String src, final String dest, final FilenameFilter filenameFilter) throws IOException {
        copyDirectory(src, dest, null, filenameFilter, false);
    }

    /**
     * Copy a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void copyDirectory(final File src, final File dest) throws IOException {
        copyDirectory(src, dest, null, null, false);
    }

    /**
     * Copy a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @param fileFilter
     *            The filter to limit file to be copied
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void copyDirectory(final File src, final File dest, final FileFilter fileFilter) throws IOException {
        copyDirectory(src, dest, fileFilter, null, false);
    }

    /**
     * Copy a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @param filenameFilter
     *            The filename filter to limit file to be copied
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    public static void copyDirectory(final File src, final File dest, final FilenameFilter filenameFilter) throws IOException {
        copyDirectory(src, dest, null, filenameFilter, false);
    }

    /**
     * Copy a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @param fileFilter
     *            The filter to limit file to be copied
     * @param filenameFilter
     *            The filename filter to limit file to be copied
     * @param removeSource
     *            Remove the source after copy
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    private static void copyDirectory(final String src, final String dest, final FileFilter fileFilter,
            final FilenameFilter filenameFilter, final boolean removeSource) throws IOException {
        if (src != null && dest != null) {
            final File srcFile = new File(src);
            final File destDir = new File(dest);

            copyDirectory(srcFile, destDir, fileFilter, filenameFilter, removeSource);
            return;
        }
        throw new FileNotFoundException(ERROR_PARAM_NULL);
    }

    /**
     * Copy a directory recursively.
     * 
     * @param src
     *            The source directory name
     * @param dest
     *            The destination directory name, the directory is created if it
     *            not exists
     * @param fileFilter
     *            The filter to limit file to be copied
     * @param filenameFilter
     *            The filename filter to limit file to be copied
     * @param removeSource
     *            Remove the source after copy
     * @throws IOException
     *             Exception thrown if problems occurs during coping
     */
    private static void copyDirectory(final File src, final File dest, final FileFilter fileFilter, final FilenameFilter filenameFilter,
            final boolean removeSource) throws IOException {

        if (src != null && dest != null) {
            if (src.isDirectory()) {
                // creation du repertoire si necessaire
                if (dest.isDirectory() || dest.mkdirs()) {
                    // creation de la liste des fichiers et repertoires
                    File[] filesToCopy;
                    if (fileFilter != null) {
                        filesToCopy = src.listFiles(fileFilter);
                    } else if (filenameFilter != null) {
                        filesToCopy = src.listFiles(filenameFilter);
                    } else {
                        filesToCopy = src.listFiles();
                    }

                    if (filesToCopy != null) {
                        copy(filesToCopy, dest, fileFilter, filenameFilter, removeSource);
                    }
                }
            } else if (src.isFile()) {
                copyFile(src, dest, removeSource);
            }
            return;
        }
        throw new FileNotFoundException(ERROR_PARAM_NULL);
    }

    /**
     * Copy files.
     * 
     * @param filesToCopy
     *            files to copy
     * @param dest
     *            destination file
     * @param fileFilter
     *            file filter
     * @param filenameFilter
     *            filename filter
     * @param removeSource
     *            Remove the source after copy
     * @throws IOException
     *             IOexception
     */
    private static void copy(final File[] filesToCopy, final File dest, final FileFilter fileFilter, final FilenameFilter filenameFilter,
            final boolean removeSource) throws IOException {
        for (int i = 0; i < filesToCopy.length; i++) {
            // Check if element is a file or a directory
            File current = filesToCopy[i];
            if (current.isDirectory()) {
                // Create the directory
                copyDirectory(filesToCopy[i], createFile(dest, filesToCopy[i].getName()), fileFilter, filenameFilter, removeSource);
            } else {
                // Copy the file
                copyFile(filesToCopy[i].getAbsolutePath(), dest + File.separator + filesToCopy[i].getName(), removeSource);
            }
        }
    }

    /**
     * Remove a directory.
     * 
     * @param dir
     *            The directory name
     * @return true, if full deleted
     */
    public static boolean deleteDirectory(final String dir) {
        return deleteDirectory(dir, null);
    }

    /**
     * Remove a directory.
     * 
     * @param dir
     *            The directory name
     * @return true, if full deleted
     */
    public static boolean deleteDirectory(final File dir) {
        return deleteDirectory(dir, null);
    }

    /**
     * Remove a directory.
     * 
     * @param dir
     *            The directory name
     * @param filter
     *            The file filter to apply (not used if null)
     * @return true, if full deleted
     */
    public static boolean deleteDirectory(final String dir, final FileFilter filter) {
        return deleteDirectory(new File(dir), filter);
    }

    /**
     * Remove a directory.
     * 
     * @param dir
     *            The directory name
     * @param filter
     *            The file filter to apply (not used if null)
     * @return true, if full deleted
     */
    public static boolean deleteDirectory(final File dir, final FileFilter filter) {
        boolean notDeleted = false;
        if (dir != null && dir.isDirectory()) {
            // Create the list of files and directories
            File[] tab = null;
            if (filter != null) {
                tab = dir.listFiles(filter);
            } else {
                tab = dir.listFiles();
            }

            if (tab != null) {
                for (int i = 0; i < tab.length; i++) {
                    // Check if the element is a directory or a file
                    File current = tab[i];
                    if (current.isDirectory()) {
                        notDeleted |= !deleteDirectory(current, filter);
                    } else {
                        // Delete the file
                        notDeleted |= !current.delete();
                    }
                }
            }

            if (isDirectoryEmpty(dir.getAbsolutePath())) {
                // Delete the empty directory
                notDeleted |= !dir.delete();
            }
        }
        return !notDeleted;
    }

    /**
     * Check if the specified directory is empty.
     * 
     * @param dir
     *            The directory to check
     * @return true if directory is empty
     */
    public static boolean isDirectoryEmpty(final String dir) {
        final File theFile = new File(dir);
        if (theFile.isDirectory()) {
            File[] files = theFile.listFiles();
            if (files == null || files.length == 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * To get the absolute path.
     * 
     * @param base
     *            The base directory
     * @param filePath
     *            The file to check
     * @return filePath, if it's absolute otherwise return 'base + filePath'
     */
    public static String getAbsolutePath(final String base, final String filePath) {
        File file = new File(filePath);
        if (!file.isAbsolute()) {
            file = new File(base, filePath);
        }
        return file.getAbsolutePath();
    }

    /**
     * Extract the extension part.
     * 
     * @param fileName
     *            The file name
     * @return The extension part or null
     */
    public static String getExtensionPart(final String fileName) {
        if (fileName != null) {
            final int index = fileName.lastIndexOf('.');
            if (index > -1 && index + 1 < fileName.length()) {
                return fileName.substring(index + 1);
            }
        }
        return null;
    }

    /**
     * Extract the extension part.
     * 
     * @param file
     *            The file
     * @return The extension part or null
     */
    public static String getExtensionPart(final File file) {
        if (file.isFile()) {
            return getExtensionPart(file.getName());
        }
        return null;
    }

    /**
     * Checks if the file has an extension including in the specified list.
     * 
     * @param fileName
     *            The file name
     * @param extensions
     *            The extensions list to check
     * @return true, if one extension matchs
     */
    public static boolean hasExtensionPart(final String fileName, final String... extensions) {
        if (fileName != null && extensions != null && extensions.length > 0) {
            String ext = getExtensionPart(fileName);
            if (ext != null) {
                List<String> exts = Arrays.asList(StringUtils.join(extensions, ",").toLowerCase().split(","));
                return exts.contains(ext.toLowerCase());
            }
        }
        return false;
    }

    /**
     * Checks if the file has an extension including in the specified list.
     * 
     * @param file
     *            The file
     * @param extensions
     *            The extensions list to check
     * @return true, if one extension matchs
     */
    public static boolean hasExtensionPart(final File file, final String... extensions) {
        if (file.isFile()) {
            return hasExtensionPart(file.getName(), extensions);
        }
        return false;
    }

    /**
     * Extract the file name part without extension.
     * 
     * @param fileName
     *            The file name
     * @return The file name part or null
     */
    public static String getFileNamePart(final String fileName) {
        if (fileName != null) {
            final int index = fileName.lastIndexOf('.');
            if (index > -1 && index + 1 < fileName.length()) {
                return fileName.substring(0, index);
            }
        }
        return null;
    }

    /**
     * Extract the file name part without extension.
     * 
     * @param file
     *            The file
     * @return The file name part or null
     */
    public static String getFileNamePart(final File file) {
        if (file.isFile()) {
            return getFileNamePart(file.getName());
        }
        return null;
    }

    /**
     * Create filename filter for directory tree.
     * 
     * @param exts
     *            The allowed extensions
     * @return The filter
     */
    public static FilenameFilter createFilenameFilter(final String... exts) {
        final List<String> allowedExts = new ArrayList<String>();
        for (String ext : exts) {
            allowedExts.add(ext.toLowerCase());
        }
        return new FilenameFilter() {
            @Override
            public boolean accept(File file, String name) {
                if (allowedExts.size() > 0) {
                    final String ext = FileSystemUtils.getExtensionPart(name);
                    if (ext != null) {
                        return allowedExts.contains(ext.toLowerCase());
                    }
                }
                return true;
            }
        };
    }

    /**
     * Create File from base and add sub directories/file.
     * 
     * @param base
     *            The base directory
     * @param subFiles
     *            The list of sub directories/file
     * @return The concatenation of the arguments
     */
    public static File createFile(final File base, final String... subFiles) {
        File generatedFile = null;

        if (base != null && subFiles.length > 0) {
            generatedFile = base;
            for (String subFile : subFiles) {
                generatedFile = new File(generatedFile, subFile);
            }
        }

        return generatedFile;

    }
}
