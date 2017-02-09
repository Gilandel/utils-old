/*
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
package fr.landel.utils.commons;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Hex utility class.
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public final class HexUtils {

    /**
     * Hex digits
     */
    public static final List<Character> HEX_DIGITS;

    private static final Character[] HEX_ARRAY = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

    static {
        HEX_DIGITS = Collections.unmodifiableList(Arrays.asList(HEX_ARRAY));
    }

    private static final int HEX_0F = 0x0F;
    private static final int HEX_FF = 0xFF;

    private static final int CHAR_SIZE = 2;
    private static final int INTEGER_SIZE = 4;
    private static final int PERCENT_MAX = 100;

    /**
     * Hidden constructor.
     */
    private HexUtils() {
    }

    /**
     * Convert an integer (0 to 255) into a byte (-128 to 127).
     * 
     * @param i
     *            integer
     * @return the byte
     */
    public static byte intToByte(final int i) {
        int percent = (i * Byte.MAX_VALUE) / PERCENT_MAX;

        byte ret = (byte) (percent & HEX_FF);

        return ret;
    }

    /**
     * Convert an integer into a byte array.
     * 
     * @param integer
     *            integer
     * @return the byte array
     */
    public static byte[] intToBytes(final int integer) {
        return ByteBuffer.allocate(INTEGER_SIZE).order(ByteOrder.LITTLE_ENDIAN).putInt(integer).array();
    }

    /**
     * Convert a byte array into an hex string.
     * 
     * @param bytes
     *            byte array
     * @return the hex string
     */
    public static String byteArrayToHexString(final byte[] bytes) {

        char[] hexChars = new char[bytes.length * CHAR_SIZE];
        int v;
        for (int j = 0; j < bytes.length; j++) {
            v = bytes[j] & HEX_FF;
            hexChars[j * CHAR_SIZE] = HEX_ARRAY[v >>> INTEGER_SIZE];
            hexChars[j * CHAR_SIZE + 1] = HEX_ARRAY[v & HEX_0F];
        }

        StringBuilder returnStr = new StringBuilder(new String(hexChars));

        return returnStr.toString();
    }
}
