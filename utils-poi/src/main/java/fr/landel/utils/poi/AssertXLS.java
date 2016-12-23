/*
 * #%L
 * utils-poi
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.poi;

import static fr.landel.utils.assertor.Assertor.that;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.List;

import org.apache.poi.hssf.usermodel.HSSFAnchor;
import org.apache.poi.hssf.usermodel.HSSFComment;
import org.apache.poi.hssf.usermodel.HSSFFont;
import org.apache.poi.hssf.usermodel.HSSFName;
import org.apache.poi.hssf.usermodel.HSSFObjectData;
import org.apache.poi.hssf.usermodel.HSSFPatriarch;
import org.apache.poi.hssf.usermodel.HSSFPicture;
import org.apache.poi.hssf.usermodel.HSSFPictureData;
import org.apache.poi.hssf.usermodel.HSSFShape;
import org.apache.poi.hssf.usermodel.HSSFShapeGroup;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFSimpleShape;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Comment;
import org.apache.poi.ss.usermodel.Row;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is used to check XLS files comparator
 *
 * @since Dec 11, 2015
 * @author Gilles
 *
 */
public final class AssertXLS {

    private static final String CELL_POSITION = " [%d, %d]";

    private final Logger logger;

    private final File expectedFile;
    private final File fileToCheck;

    private final HSSFWorkbook workbookExpected;
    private final HSSFWorkbook workbook;

    private AssertXLS(final File expectedFile, final File fileToCheck, final FileInputStream isExpected, final FileInputStream isToCheck)
            throws IOException {
        this.logger = LoggerFactory.getLogger(this.getClass());

        this.expectedFile = expectedFile;
        this.fileToCheck = fileToCheck;

        this.workbookExpected = new HSSFWorkbook(isExpected);
        this.workbook = new HSSFWorkbook(isToCheck);
    }

    /**
     * Compare two XLS files
     * 
     * @param expectedFile
     *            The file used as validator
     * @param fileToCheck
     *            The file to check
     * @throws IllegalArgumentException
     *             if doesn't match
     */
    public static void assertEquals(final File expectedFile, final File fileToCheck) {
        try (FileInputStream isExpected = new FileInputStream(expectedFile); FileInputStream isToCheck = new FileInputStream(fileToCheck)) {
            final AssertXLS assertXLS = new AssertXLS(expectedFile, fileToCheck, isExpected, isToCheck);

            assertXLS.checkSheets();
            assertXLS.checkFonts();
            assertXLS.checkNames();
            assertXLS.checkEmbeddedObjects();
            assertXLS.checkPictures();
        } catch (IOException e) {
            throw new IllegalArgumentException("files cannot be loaded. ", e);
        }
    }

    private void checkSheets() {
        isEqual(this.workbookExpected.getActiveSheetIndex(), this.workbook.getActiveSheetIndex(), "Sheet active index");

        isEqual(this.workbookExpected.getNumberOfSheets(), this.workbook.getNumberOfSheets(), "Sheet number");

        for (int i = 0; i < this.workbookExpected.getNumberOfSheets(); i++) {
            HSSFSheet sheetExpected = this.workbookExpected.getSheetAt(i);
            HSSFSheet sheet = this.workbook.getSheetAt(i);

            this.checkPatriarch(sheetExpected.getDrawingPatriarch(), sheet.getDrawingPatriarch());

            isEqual(sheetExpected.getPhysicalNumberOfRows(), sheet.getPhysicalNumberOfRows(), "Sheet physical number rows");

            for (int r = 0; r < sheet.getPhysicalNumberOfRows(); r++) {
                Row rowExpected = sheetExpected.getRow(r);
                Row row = sheet.getRow(r);
                if (rowExpected != null || row != null) {
                    that(rowExpected).isNotNull().toThrow("Row expected");
                    that(row).isNotNull().toThrow("Row " + r);

                    this.checkRow(rowExpected, row, r);
                }
            }
        }
    }

    private void checkPatriarch(final HSSFPatriarch patriarchExpected, final HSSFPatriarch patriarch) {
        if (patriarchExpected != null || patriarch != null) {
            isNotNull(patriarchExpected, "Patriarch expected");
            isNotNull(patriarch, "Patriarch");

            isEqual(patriarchExpected.getX1(), patriarch.getX1(), "Patriarch X1");
            isEqual(patriarchExpected.getX2(), patriarch.getX2(), "Patriarch X2");
            isEqual(patriarchExpected.getY1(), patriarch.getY1(), "Patriarch Y1");
            isEqual(patriarchExpected.getY2(), patriarch.getY2(), "Patriarch Y2");

            this.checkShapes(patriarchExpected.getChildren(), patriarch.getChildren());
        }
    }

    private void checkShapes(final List<HSSFShape> shapesExpected, final List<HSSFShape> shapes) {
        isEqual(shapesExpected.size(), shapes.size(), "Shapes children");

        for (int c = 0; c < shapesExpected.size(); c++) {
            HSSFShape shapeExpected = shapesExpected.get(c);
            HSSFShape shape = shapes.get(c);
            if (shapeExpected != null || shape != null) {
                isNotNull(shapeExpected, "Shape expected");
                isNotNull(shape, "Shape");

                isEqual(shapeExpected.getClass(), shape.getClass(), "Shape class");

                this.checkAnchors(shapeExpected.getAnchor(), shape.getAnchor());

                if (HSSFPicture.class.isAssignableFrom(shapeExpected.getClass())) {
                    HSSFPicture pictureExpected = (HSSFPicture) shapeExpected;
                    HSSFPicture picture = (HSSFPicture) shape;

                    final String errorBytesLength = "Images don't match. Expected: %s (" + pictureExpected.getFileName() + ") but was: %s ("
                            + picture.getFileName() + ")";

                    isEqual(pictureExpected.getFileName(), picture.getFileName(), "Picture file name");
                    isEqual(pictureExpected.getImageDimension(), picture.getImageDimension(), "Picture dimension");
                    isEqual(pictureExpected.getPictureIndex(), picture.getPictureIndex(), "Picture index");
                    isEqual(pictureExpected.getShapeType(), picture.getShapeType(), "Picture shape type");

                    this.checkPictureData(errorBytesLength, pictureExpected.getPictureData(), picture.getPictureData());
                } else if (HSSFComment.class.isAssignableFrom(shapeExpected.getClass())) {
                    HSSFComment commentExpected = (HSSFComment) shapeExpected;
                    HSSFComment comment = (HSSFComment) shape;

                    isEqual(commentExpected.getBackgroundImageId(), comment.getBackgroundImageId(), "Comment background");
                    isEqual(commentExpected.getClientAnchor(), comment.getClientAnchor(), "Comment client anchor");

                    checkComment(commentExpected, comment, commentExpected.getRow(), commentExpected.getColumn());
                } else if (HSSFShapeGroup.class.isAssignableFrom(shapeExpected.getClass())) {
                    HSSFShapeGroup shapeGroupExpected = (HSSFShapeGroup) shapeExpected;
                    HSSFShapeGroup shapeGroup = (HSSFShapeGroup) shape;
                    isEqual(shapeGroupExpected.countOfAllChildren(), shapeGroup.countOfAllChildren(), "Shape group count");

                    this.checkShapes(shapeGroupExpected.getChildren(), shapeGroup.getChildren());
                } else if (HSSFSimpleShape.class.isAssignableFrom(shapeExpected.getClass())) {
                    isEqual(shapeExpected.isNoFill(), shape.isNoFill(), "Shape no fill");
                    isEqual(shapeExpected.getFillColor(), shape.getFillColor(), "Shape fill color");
                    isEqual(shapeExpected.getLineStyle(), shape.getLineStyle(), "Shape line style");
                    isEqual(shapeExpected.getLineStyleColor(), shape.getLineStyleColor(), "Shape line style color");
                    isEqual(shapeExpected.getLineWidth(), shape.getLineWidth(), "Shape line width");
                    isEqual(shapeExpected.getRotationDegree(), shape.getRotationDegree(), "Shape rotation degree");
                    isEqual(shapeExpected.isFlipHorizontal(), shape.isFlipHorizontal(), "Shape flip horizontal");
                    isEqual(shapeExpected.isFlipVertical(), shape.isFlipVertical(), "Shape flip vertical");
                } else {
                    throw new IllegalArgumentException("not implemented");
                }
            }
        }
    }

    private void checkRow(final Row rowExpected, final Row row, final int rowIndex) {
        that(rowExpected.getPhysicalNumberOfCells()).isEqual(row.getPhysicalNumberOfCells());
        this.checkStyle(rowExpected.getRowStyle(), row.getRowStyle(), rowIndex, -1);

        for (int c = 0; c < row.getPhysicalNumberOfCells(); c++) {
            final String cellPosition = String.format(CELL_POSITION, rowIndex, c);

            Cell cellExpected = rowExpected.getCell(c);
            Cell cell = row.getCell(c);

            if (cellExpected != null || cell != null) {
                isNotNull(cellExpected, "Cell expected" + cellPosition);
                isNotNull(cell, "Cell" + cellPosition);

                isEqual(cellExpected.getCellType(), cell.getCellType(), "Cell type" + cellPosition);
                this.checkCellType(cellExpected, cell, rowIndex, c);

                this.checkComment(cellExpected.getCellComment(), cell.getCellComment(), rowIndex, c);

                this.checkStyle(cellExpected.getCellStyle(), cell.getCellStyle(), rowIndex, c);
            }
        }
    }

    private void checkComment(final Comment commentExpected, final Comment comment, final int rowIndex, final int columnIndex) {
        final String cellPosition = String.format(CELL_POSITION, rowIndex, columnIndex);

        if (commentExpected != null || comment != null) {
            isNotNull(commentExpected, "Comment expected" + cellPosition);
            isNotNull(comment, "Comment" + cellPosition);

            isEqual(commentExpected.getAuthor(), comment.getAuthor(), "Comment author" + cellPosition);
            isEqual(commentExpected.getColumn(), comment.getColumn(), "Comment column" + cellPosition);
            isEqual(commentExpected.getRow(), comment.getRow(), "Comment row" + cellPosition);
            isEqual(commentExpected.isVisible(), comment.isVisible(), "Comment visible" + cellPosition);

            isEqual(commentExpected.getString().getString(), comment.getString().getString(), "Comment text" + cellPosition);
        }
    }

    private void checkStyle(final CellStyle styleExpected, final CellStyle style, final int rowIndex, final int columnIndex) {
        final String cellPosition = String.format(CELL_POSITION, rowIndex, columnIndex);

        if (styleExpected != null || style != null) {
            isNotNull(styleExpected, "Style expected" + cellPosition);
            isNotNull(style, "Style" + cellPosition);

            isEqual(styleExpected.getDataFormatString(), style.getDataFormatString(), "Style data format string" + cellPosition);
            isEqual(styleExpected.getAlignmentEnum(), style.getAlignmentEnum(), "Style alignment" + cellPosition);
            isEqual(styleExpected.getVerticalAlignmentEnum(), style.getVerticalAlignmentEnum(), "Style vertical algnment" + cellPosition);
            isEqual(styleExpected.getBorderBottomEnum(), style.getBorderBottomEnum(), "Style border bottom" + cellPosition);
            isEqual(styleExpected.getBorderLeftEnum(), style.getBorderLeftEnum(), "Style border left" + cellPosition);
            isEqual(styleExpected.getBorderRightEnum(), style.getBorderRightEnum(), "Style border right" + cellPosition);
            isEqual(styleExpected.getBorderTopEnum(), style.getBorderTopEnum(), "Style border top" + cellPosition);
            isEqual(styleExpected.getBottomBorderColor(), style.getBottomBorderColor(), "Style bottom border color" + cellPosition);
            isEqual(styleExpected.getLeftBorderColor(), style.getLeftBorderColor(), "Style left border color" + cellPosition);
            isEqual(styleExpected.getRightBorderColor(), style.getRightBorderColor(), "Style right border color" + cellPosition);
            isEqual(styleExpected.getTopBorderColor(), style.getTopBorderColor(), "Style top border color" + cellPosition);
            isEqual(styleExpected.getDataFormat(), style.getDataFormat(), "Style data format" + cellPosition);
            isEqual(styleExpected.getFillBackgroundColor(), style.getFillBackgroundColor(), "Style fill background color" + cellPosition);
            isEqual(styleExpected.getFillForegroundColor(), style.getFillForegroundColor(), "Style fill foreground color" + cellPosition);
            isEqual(styleExpected.getFillPatternEnum(), style.getFillPatternEnum(), "Style fill pattern" + cellPosition);
            isEqual(styleExpected.getFontIndex(), style.getFontIndex(), "Style font index" + cellPosition);
            isEqual(styleExpected.getHidden(), style.getHidden(), "Style hidden" + cellPosition);
            isEqual(styleExpected.getIndention(), style.getIndention(), "Style indentation" + cellPosition);
            // isEqual(styleExpected.getIndex(), style.getIndex(), "Style index"
            // + cellPosition);
            isEqual(styleExpected.getLocked(), style.getLocked(), "Style locked" + cellPosition);
            isEqual(styleExpected.getRotation(), style.getRotation(), "Style rotation" + cellPosition);
            isEqual(styleExpected.getWrapText(), style.getWrapText(), "Style wrap text" + cellPosition);
        }
    }

    private void checkCellType(final Cell cellExpected, final Cell cell, final int rowIndex, final int columnIndex) {
        final String cellPosition = String.format(CELL_POSITION, rowIndex, columnIndex);

        switch (cell.getCellTypeEnum()) {
        case NUMERIC:
            isEqual((Double) cellExpected.getNumericCellValue(), (Double) cell.getNumericCellValue(), "Cell type numeric" + cellPosition);
            break;
        case STRING:
            isEqual(cellExpected.getRichStringCellValue(), cell.getRichStringCellValue(), "Cell type string" + cellPosition);
            break;
        case FORMULA:
            isEqual(cellExpected.getCellFormula(), cell.getCellFormula(), "Cell type formula" + cellPosition);
            break;
        case BOOLEAN:
            isEqual(cellExpected.getBooleanCellValue(), cell.getBooleanCellValue(), "Cell type boolean" + cellPosition);
            break;
        case ERROR:
            isEqual(cellExpected.getErrorCellValue(), cell.getErrorCellValue(), "Cell type error" + cellPosition);
            break;
        default: // Cell.CELL_TYPE_BLANK or DATE or Hyperlink
            if (cellExpected.getDateCellValue() != null || cell.getDateCellValue() != null) {
                isNotNull(cellExpected.getDateCellValue(), "Cell type expected date" + cellPosition);
                isNotNull(cell.getDateCellValue(), "Cell type date" + cellPosition);

                isEqual(cellExpected.getDateCellValue().getTime(), cell.getDateCellValue().getTime(),
                        "Cell type date compare" + cellPosition);
            }

            if (cellExpected.getHyperlink() != null || cell.getHyperlink() != null) {
                isNotNull(cellExpected.getHyperlink(), "Cell type expected hyperlink" + cellPosition);
                isNotNull(cell.getHyperlink(), "Cell type hyperlink" + cellPosition);

                isEqual(cellExpected.getHyperlink().getAddress(), cell.getHyperlink().getAddress(),
                        "Cell type hyperlink address" + cellPosition);
                isEqual(cellExpected.getHyperlink().getLabel(), cell.getHyperlink().getLabel(), "Cell type hyperlink label" + cellPosition);
            }
            break;
        }
    }

    private void checkFonts() {
        that(this.workbookExpected.getNumberOfFonts()).isEqual(this.workbook.getNumberOfFonts());

        for (short i = 0; i < this.workbookExpected.getNumberOfFonts(); i++) {
            HSSFFont fontExpected = this.workbookExpected.getFontAt(i);
            HSSFFont font = this.workbook.getFontAt(i);

            isEqual(fontExpected.getFontName(), font.getFontName(), "Font name");
            isEqual(fontExpected.getBoldweight(), font.getBoldweight(), "Font bold weight");
            isEqual(fontExpected.getCharSet(), font.getCharSet(), "Font charset");
            isEqual(fontExpected.getColor(), font.getColor(), "Font color");
            isEqual(fontExpected.getFontHeight(), font.getFontHeight(), "Font height");
            isEqual(fontExpected.getFontHeightInPoints(), font.getFontHeightInPoints(), "Font height in points");
            isEqual(fontExpected.getIndex(), font.getIndex(), "Font index");
            isEqual(fontExpected.getItalic(), font.getItalic(), "Font italic");
            isEqual(fontExpected.getStrikeout(), font.getStrikeout(), "Font strikeout");
            isEqual(fontExpected.getTypeOffset(), font.getTypeOffset(), "Font type offset");
            isEqual(fontExpected.getUnderline(), font.getUnderline(), "Font underline");
        }
    }

    private void checkNames() {
        that(this.workbookExpected.getNumberOfNames()).isEqual(this.workbook.getNumberOfNames());

        for (int i = 0; i < this.workbookExpected.getNumberOfNames(); i++) {
            HSSFName nameExpected = this.workbookExpected.getNameAt(i);
            HSSFName name = this.workbook.getNameAt(i);

            isEqual(nameExpected.getSheetName(), name.getSheetName(), "Name sheet");
            isEqual(nameExpected.getNameName(), name.getNameName(), "Name name");
            isEqual(nameExpected.getComment(), name.getComment(), "Name comment");
            isEqual(nameExpected.getRefersToFormula(), name.getRefersToFormula(), "Name formula");
            isEqual(nameExpected.getSheetIndex(), name.getSheetIndex(), "Name sheet index");
            isEqual(nameExpected.isDeleted(), name.isDeleted(), "Name sheet");
            isEqual(nameExpected.isFunctionName(), name.isFunctionName(), "Name function");
        }
    }

    private void checkEmbeddedObjects() {
        final List<HSSFObjectData> embeddedObjectsExpected = this.workbookExpected.getAllEmbeddedObjects();
        final List<HSSFObjectData> embeddedObjects = this.workbook.getAllEmbeddedObjects();

        if (embeddedObjectsExpected != null || embeddedObjects != null) {
            isNotNull(embeddedObjectsExpected, "Embedded expected");
            isNotNull(embeddedObjects, "Embedded");

            isEqual(embeddedObjectsExpected.size(), embeddedObjects.size(), "Embedded size");

            for (int i = 0; i < embeddedObjectsExpected.size(); i++) {
                HSSFObjectData objectExpected = embeddedObjectsExpected.get(i);
                HSSFObjectData object = embeddedObjects.get(i);

                isEqual(objectExpected.countOfAllChildren(), object.countOfAllChildren(), "Embedded count children");
                isEqual(objectExpected.getFileName(), object.getFileName(), "Embedded file name");
                isEqual(objectExpected.getOLE2ClassName(), object.getOLE2ClassName(), "Embedded OLE2 class");

                this.checkAnchors(objectExpected.getAnchor(), object.getAnchor());

                isEqual(objectExpected.getFillColor(), object.getFillColor(), "Embedded fill color");
                isEqual(objectExpected.getImageDimension(), object.getImageDimension(), "Embedded image dimension");
                isEqual(objectExpected.getLineStyle(), object.getLineStyle(), "Embedded line style");
                isEqual(objectExpected.getLineStyleColor(), object.getLineStyleColor(), "Embedded line style color");
                isEqual(objectExpected.getLineWidth(), object.getLineWidth(), "Embedded line width");
                isEqual(objectExpected.getPictureIndex(), object.getPictureIndex(), "Embedded picture index");
                isEqual(objectExpected.getRotationDegree(), object.getRotationDegree(), "Embedded rotation degree");
                isEqual(objectExpected.getShapeType(), object.getShapeType(), "Embedded shape type");
                isEqual(objectExpected.isFlipHorizontal(), object.isFlipHorizontal(), "Embedded flip horizontal");
                isEqual(objectExpected.isFlipVertical(), object.isFlipVertical(), "Embedded flip vertical");
                isEqual(objectExpected.isNoFill(), object.isNoFill(), "Embedded no fill");

                if (objectExpected.getString() != null || object.getString() != null) {
                    isNotNull(objectExpected.getString(), "Embedded expected string");
                    isNotNull(object.getString(), "Embedded string");
                    isEqual("Embedded string string", objectExpected.getString().getString(), object.getString().getString());
                }

                isEqual(objectExpected.getWrapText(), object.getWrapText(), "Embedded wrap text");
            }
        }
    }

    private void checkAnchors(final HSSFAnchor anchorExpected, final HSSFAnchor anchor) {
        if (anchorExpected != null || anchor != null) {
            isNotNull(anchorExpected, "Embedded anchor expected");
            isNotNull(anchor, "Embedded anchor");

            isEqual(anchorExpected.getDx1(), anchor.getDx1(), "Embedded anchor DX1");
            isEqual(anchorExpected.getDx2(), anchor.getDx2(), "Embedded anchor DX2");
            isEqual(anchorExpected.getDy1(), anchor.getDy1(), "Embedded anchor DY1");
            isEqual(anchorExpected.getDy2(), anchor.getDy2(), "Embedded anchor DY2");
            isEqual(anchorExpected.isHorizontallyFlipped(), anchor.isHorizontallyFlipped(), "Embedded anchor horizontally");
            isEqual(anchorExpected.isVerticallyFlipped(), anchor.isVerticallyFlipped(), "Embedded anchor vertically");
        }
    }

    private void checkPictures() {
        final List<HSSFPictureData> picturesExpected = this.workbookExpected.getAllPictures();
        final List<HSSFPictureData> pictures = this.workbook.getAllPictures();

        final String errorBytesLength = "Images don't match. Expected: %s (" + this.expectedFile.getName() + ") but was: %s ("
                + this.fileToCheck.getName() + ")";

        if (picturesExpected != null || pictures != null) {
            isNotNull(picturesExpected, "Picture expected");
            isNotNull(pictures, "Picture");

            isEqual(picturesExpected.size(), pictures.size(), "Picture size");

            for (int i = 0; i < picturesExpected.size(); i++) {
                HSSFPictureData pictureExpected = picturesExpected.get(i);
                HSSFPictureData picture = pictures.get(i);

                this.checkPictureData(errorBytesLength, pictureExpected, picture);
            }
        }
    }

    private void checkPictureData(final String errorBytesLength, final HSSFPictureData pictureExpected, final HSSFPictureData picture) {
        isEqual(pictureExpected.getMimeType(), picture.getMimeType(), "Picture mime type");
        isEqual(pictureExpected.getFormat(), picture.getFormat(), "Picture format");

        byte[] expectedBytes = pictureExpected.getData();
        byte[] bytes = picture.getData();

        if (expectedBytes.length != bytes.length) {
            this.logger.error(String.format(errorBytesLength, expectedBytes.length, bytes.length));
        }

        isEqual(expectedBytes.length, bytes.length, "Picture bytes length");

        for (int j = 0; j < expectedBytes.length; j++) {
            isEqual(expectedBytes[j], bytes[j], "Picture bytes");
        }
    }

    private static <T> void isEqual(final T obj1, final T obj2, String message) {
        that(obj1).isEqual(obj2).toThrow(message);
    }

    private static <T> void isNotNull(final T obj, String message) {
        that(obj).isNotNull().toThrow(message);
    }
}
