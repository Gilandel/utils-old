package fr.landel.utils.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.io.IOException;

import org.junit.Test;

/**
 * Check {@link FileCRC32Utils}
 *
 * @since Aug 11, 2016
 * @author Gilles
 *
 */
public class FileCRC32UtilsTest {

    private static final String XML_EXT = "xml";
    private static final FilenameFilter XML_FILENAME_FILTER = FileSystemUtils.createFilenameFilter(XML_EXT);
    private static final FileFilter XML_FILE_FILTER = (file) -> XML_EXT.equalsIgnoreCase(FileSystemUtils.getExtensionPart(file));

    private static final String CHECK_CRC32_PATH = "src/test/resources/io";
    private static final String CHECK_CRC32_FILE = CHECK_CRC32_PATH + "/checkCRC32.xml";
    private static final Long CHECK_CRC32_VALUE = 3_893_630_386L;
    private static final Long CHECK_CRC32_DIR_UNIX_VALUE = 3_440_695_467L;
    private static final Long CHECK_CRC32_DIR_WIN_VALUE = 580_225_974L;

    /**
     * Test method for {@link FileCRC32Utils#getCRC32}.
     */
    @Test
    public void testGetCRC32() {
        try {
            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(CHECK_CRC32_FILE));
            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(new File(CHECK_CRC32_FILE)));
            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(StreamUtils.createBufferedInputStream(CHECK_CRC32_FILE)));

            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(CHECK_CRC32_PATH, XML_FILENAME_FILTER));
            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(CHECK_CRC32_PATH, XML_FILE_FILTER));

            final long crc = FileCRC32Utils.getCRC32(CHECK_CRC32_PATH);
            assertTrue(crc == CHECK_CRC32_DIR_WIN_VALUE || crc == CHECK_CRC32_DIR_UNIX_VALUE);

            final File emptyDir = new File("target/empty");
            assertTrue(FileSystemUtils.createDirectory(emptyDir));
            assertEquals(Long.valueOf(0L), FileCRC32Utils.getCRC32(emptyDir));

            final File unknownDir = new File("target/unknown");

            assertEquals(Long.valueOf(0L), FileCRC32Utils.getCRC32(unknownDir));
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }
}
