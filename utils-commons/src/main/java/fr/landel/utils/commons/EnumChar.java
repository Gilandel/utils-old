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

/**
 * Characters utility class.
 *
 * @since Dec 1, 2015
 * @author Gilles Landel
 *
 */
public enum EnumChar {

    /**
     * Nul or null (code: 0, unicode: u0000)
     */
    NUL(0, "\u0000"),

    /**
     * Start Of Header (code: 1, unicode: u0001)
     */
    SOH(1, "\u0001"),

    /**
     * Start of Text (code: 2, unicode: u0002)
     */
    STX(2, "\u0002"),

    /**
     * End of Text (code: 3, unicode: u0003, HTML: <code>&amp;hearts;</code> )
     */
    ETX(3, "\u0003", "&hearts;"),

    /**
     * End Of Transmission (code: 4, unicode: u0004, HTML:
     * <code>&amp;diams;</code>)
     */
    EOT(4, "\u0004", "&diams;"),

    /**
     * Enquiry (code: 5, unicode: u0005, HTML: <code>&amp;clubs;</code>)
     */
    ENQ(5, "\u0005", "&clubs;"),

    /**
     * Acknowledge (code: 6, unicode: u001e, HTML: <code>&amp;spades;</code>)
     */
    ACK(6, "\u0006", "&spades;"),

    /**
     * Bell (code: 7, unicode: u0007)
     */
    BEL(7, "\u0007"),

    /**
     * Backspace (code: 8, unicode: u0008)
     */
    BS('\b', "\u0008"),

    /**
     * Horizontal Tab (code: 9, unicode: <code>\t</code> (u0009), HTML:
     * <code>&amp;#09;</code>)
     */
    HT('\t', "\u0009", "&#9;"),

    /**
     * Horizontal Tab (code: 9, unicode: <code>\t</code> (u0009), HTML:
     * <code>&amp;#09;</code>)
     */
    TAB(HT),

    /**
     * Line Feed (code: 10, unicode: <code>\n</code> (u000a), HTML:
     * <code>&amp;NewLine;</code>)
     */
    LF('\n', "\n", "&NewLine;"),

    /**
     * Line Feed (code: 10, unicode: <code>\n</code> (u000a), HTML:
     * <code>&amp;NewLine;</code>)
     */
    LINE_FEED(LF),

    /**
     * Line Feed (code: 10, unicode: <code>\n</code> (u000a), HTML:
     * <code>&amp;NewLine;</code>)
     */
    NEW_LINE(LF),

    /**
     * Vertical Tab (code: 11, unicode: u001e, HTML: <code>&amp;male;</code>)
     */
    VT(11, "\u000b", "&male;"),

    /**
     * Form Feed (code: 12, unicode: <code>\f</code> (u000c), HTML:
     * <code>&amp;female;</code>)
     */
    FF('\f', "\f", "&female;"),

    /**
     * Carriage Return (code: 13, unicode: <code>\r</code> (u000d))
     */
    CR('\r', "\r"),

    /**
     * Carriage Return (code: 13, unicode: <code>\r</code> (u000d))
     */
    CARRIAGE_RETURN(CR),

    /**
     * Carriage Return (code: 13, unicode: <code>\r</code> (u000d))
     */
    LINE_RETURN(CR),

    /**
     * Shift Out (code: 14, unicode: u000e)
     */
    SO(14, "\u000e"),

    /**
     * Shift In (code: 15, unicode: u000f)
     */
    SI(15, "\u000f"),

    /**
     * Data Link Escape (code: 16, unicode: u0010)
     */
    DLE(16, "\u0010"),

    /**
     * Device Control 1, XON resume transmission (code: 17, unicode: u0011)
     */
    DC1(17, "\u0011"),

    /**
     * Device Control 2 (code: 18, unicode: u0012)
     */
    DC2(18, "\u0012"),

    /**
     * Device Control 3, XOFF pause transmission (code: 19, unicode: u0013)
     */
    DC3(19, "\u0013"),

    /**
     * Device Control 4 (code: 20, unicode: u0014)
     */
    DC4(20, "\u0014"),

    /**
     * Negative Acknowledbge (code: 21, unicode: u0015)
     */
    NAK(21, "\u0015"),

    /**
     * Synchronise (code: 22, unicode: u0016)
     */
    SYN(22, "\u0016"),

    /**
     * End Text Block (code: 23, unicode: u0017)
     */
    ETB(23, "\u0017"),

    /**
     * Cancel (code: 24, unicode: u0018)
     */
    CAN(24, "\u0018"),

    /**
     * End of Medium (code: 25, unicode: u0019)
     */
    EM(25, "\u0019"),

    /**
     * Substitute (code: 26, unicode: u001a)
     */
    SUB(26, "\u001a"),

    /**
     * Escape (code: 27, unicode: u001b)
     */
    ESC(27, "\u001b"),

    /**
     * File Separator, originally used to separate files in a stream (code: 28,
     * unicode: u001c)
     */
    FS(28, "\u001c"),

    /**
     * Group Separator, originally used to separate groups of similar records
     * (tables) in a stream (code: 29, unicode: u001d)
     */
    GS(29, "\u001d"),

    /**
     * Record Separator, originally used to separate records (code: 30, unicode:
     * u001e)
     */
    RS(30, "\u001e"),

    /**
     * Unit Separator, originally used to separate fields (or units as they were
     * once called) (code: 31, unicode: u001f)
     */
    US(31, "\u001f"),

    /**
     * Comma (symbol: ",",code: 44, unicode: u002e, HTML:
     * <code>&amp;comma;</code>)
     */
    COMMA(',', "\u002c", "&comma;"),

    /**
     * Space (symbol: " ", code: 32, unicode: u0020, HTML:
     * <code>&amp;#32;</code>)
     */
    SPACE(' ', "\u0020", "&#32;"),

    /**
     * Non-breaking space (symbol: " ", code: 255, unicode: u00ff, HTML:
     * <code>&amp;nbsp;</code>)
     */
    NON_BREAKING_SPACE(' ', "\u00ff", "&nbsp;"),

    /**
     * Dot or period (symbol: ".", code: 46, unicode: u002e, HTML:
     * <code>&amp;period;</code>)
     */
    DOT('.', "\u002e", "&period;"),

    /**
     * Dot or period (symbol: ".", code: 46, unicode: u002e, HTML:
     * <code>&amp;period;</code>)
     */
    PERIOD(DOT),

    /**
     * Horizontal ellipsis (symbol: "…", code: 8230, unicode: u2026, HTML:
     * <code>&amp;#8230;</code>)
     */
    ELLIPSIS('…', "\u2026", "&#8230;"),

    /**
     * Hyphen or common dash (symbol: "-", code: 45, unicode: u002d, HTML:
     * <code>&amp;dash;</code>)
     */
    HYPHEN('-', "\u002d", "&dash;"),

    /**
     * En-dash (symbol: "–", code: 8211, unicode: u2013, HTML:
     * <code>&amp;ndash;</code>)
     */
    DASH('–', "\u2013", "&ndash;"),

    /**
     * Em-dash (symbol: "—", code: 8212, unicode: u2014, HTML:
     * <code>&amp;mdash;</code>)
     */
    EM_DASH('—', "\u2014", "&mdash;"),

    /**
     * Underscore (symbol: "_", code: 95, unicode: u005f, HTML:
     * <code>&amp;lowbar;</code>)
     */
    UNDERSCORE('_', "\u005f", "&lowbar;"),

    /**
     * Colon (symbol: ":", code: 58, unicode: u003a, HTML:
     * <code>&amp;colon;</code>)
     */
    COLON(':', "\u003a", "&colon;"),

    /**
     * Semicolon (symbol: ";", code: 59, unicode: u003b, HTML:
     * <code>&amp;semi;</code>)
     */
    SEMICOLON(';', "\u003b", "&semi;"),
    /**
     * Question mark (symbol: "?", code: 63, unicode: u003f, HTML:
     * <code>&amp;quest;</code>)
     */
    QUESTION('?', "\u003f", "&quest;"),

    /**
     * Inverted question mark (symbol: "¿", code: 168, unicode: u00bf, HTML:
     * <code>&amp;iquest;</code>)
     */
    INVERTED_QUESTION('¿', "\u00bf", "&iquest;"),

    /**
     * Exclamation (symbol: "!", code: 33, unicode: u0021, HTML: &amp;excl;)
     */
    BANG('!', "\u0021", "&excl;"),

    /**
     * Exclamation (symbol: "!", code: 33, unicode: u0021, HTML: &amp;excl;)
     */
    EXCLAMATION(BANG),

    /**
     * Inverted exclamation (symbol: "¡", code: 173, unicode: u00a1, HTML:
     * &amp;iexcl;)
     */
    INVERTED_BANG('¡', "\u00a1", "&iexcl;"),

    /**
     * Inverted exclamation (symbol: "¡", code: 173, unicode: u00a1, HTML:
     * &amp;iexcl;)
     */
    INVERTED_EXCLAMATION(INVERTED_BANG),

    /**
     * Asterisk or star (symbol: "*", code: 42, unicode: u002a, HTML: &amp;ast;)
     */
    ASTERISK('*', "\u002a", "&ast;"),

    /**
     * Asterisk or star (symbol: "*", code: 42, unicode: u002a, HTML: &amp;ast;)
     */
    STAR(ASTERISK),

    /**
     * Slash, forward slash, fraction bar, division slash (symbol: "/", code:
     * 47, unicode: u002f, HTML: &amp;frasl;)
     */
    SLASH('/', "\u002f", "&frasl;"),

    /**
     * Backslash or reverse slash (symbol: "\", code: 92, unicode:
     * <code>\</code> (u005c), HTML: &amp;bsol;)
     */
    BACKSLASH('\\', "\\", "&bsol;"),

    /**
     * Quote (symbol: """, code: 34, unicode: <code>"</code> (u0022), HTML:
     * &amp;quot;)
     */
    QUOTE('\"', "\"", "&quot;"),

    /**
     * Apostrophe (symbol: "'", code: 39, unicode: u0027, HTML: &amp;apos;)
     */
    APOSTROPHE('\'', "\u0027", "&apos;"),

    /**
     * Apostrophe (symbol: "'", code: 39, unicode: u0027, HTML: &amp;apos;)
     */
    SINGLE_QUOTE(APOSTROPHE),

    /**
     * Grave (symbol: "`", code: 96, unicode: u0060, HTML: &amp;grave;)
     */
    GRAVE('`', "\u0060", "&grave;"),

    /**
     * Grave (symbol: "´", code: 239, unicode: u00b4, HTML: &amp;acute;)
     */
    ACUTE('´', "\u00b4", "&acute;"),

    /**
     * Ampersand (symbol: "&amp;", code: 38, unicode: u0026, HTML: &amp;amp;)
     */
    AMPERSAND('&', "\u0026", "&amp;"),

    /**
     * At sign (symbol: "@", code: 64, unicode: u0040, HTML: &amp;commat;)
     */
    AT('@', "\u0040", "&commat;"),

    /**
     * Percent sign (symbol: "%", code: 37, unicode: u0025, HTML: &amp;percnt;)
     */
    PERCENT('%', "\u0025", "&percnt;"),

    /**
     * Per mil sign (symbol: "‰", code: 8240, unicode: u2030, HTML: &amp;#8240;)
     */
    PER_MIL('‰', "\u2030", "&#8240;"),

    /**
     * Per ten mil sign (symbol: "‱", code: 8241, unicode: u2031, HTML:
     * &amp;#8241;)
     */
    PER_TEN_MIL('‱', "\u2031", "&#8241;"),

    /**
     * Hat, caret or circumflex (symbol: "^", code: 94, unicode: u005e, HTML:
     * &amp;Hat;)
     */
    CARET('^', "\u005e", "&Hat;"),

    /**
     * Hat, caret or circumflex (symbol: "^", code: 94, unicode: u005e, HTML:
     * &amp;Hat;)
     */
    HAT(CARET),

    /**
     * Hat, caret or circumflex (symbol: "^", code: 94, unicode: u005e, HTML:
     * &amp;Hat;)
     */
    CIRCUMFLEX(CARET),

    /**
     * Bullet (symbol: "^", code: 8226, unicode: u2022, HTML: &amp;#8226;)
     */
    BULLET('•', "\u2022", "&#8226;"),

    /**
     * Black square or box (symbol: "■", code: 9632, unicode: u25a0, HTML:
     * &amp;squf;)
     */
    BLACK_SQUARE('■', "\u25a0", "&#9632;"),

    /**
     * Black square or box (symbol: "■", code: 9632, unicode: u25a0, HTML:
     * &amp;squf;)
     */
    BOX(BLACK_SQUARE),

    /**
     * Degree (symbol: "°", code: 248, unicode: u00b0, HTML: &amp;deg;)
     */
    DEGREE('°', "\u00b0", "&deg;"),

    /**
     * Numero (symbol: "№", code: 8470, unicode: u2116, HTML: &amp;#8470;)
     */
    NUMERO('№', "\u2116", "&#8470;"),

    /**
     * Tilde or swung dash (symbol: "~", code: 126, unicode: u2116, HTML:
     * &amp;tilde;)
     */
    TILDE('~', "\u007e", "&tilde;"),

    /**
     * Pipe or vertical-bar (symbol: "|", code: 124, unicode: u007c, HTML:
     * &amp;vert;)
     */
    PIPE('|', "\u007c", "&vert;"),

    /**
     * Pipe or vertical-bar (symbol: "|", code: 124, unicode: u007c, HTML:
     * &amp;vert;)
     */
    VERTICAL_BAR(PIPE),

    /**
     * Pilcrow or paragraph sign (symbol: "¶", code: 182, unicode: u00b6, HTML:
     * &amp;para;)
     */
    PILCROW('¶', "\u00b6", "&para;"),

    /**
     * Pilcrow or paragraph sign (symbol: "¶", code: 182, unicode: u00b6, HTML:
     * &amp;para;)
     */
    PARAGRAPH(PILCROW),

    /**
     * Section sign (symbol: "§", code: 167, unicode: u00a7, HTML: &amp;sect;)
     */
    SECTION('§', "\u00a7", "&sect;"),

    /**
     * Number, pound or sharp (symbol: "#", code: 35, unicode: u0023, HTML:
     * &amp;num;)
     */
    HASH('#', "\u0023", "&num;"),

    /**
     * Number, pound or sharp (symbol: "#", code: 35, unicode: u0023, HTML:
     * &amp;num;)
     */
    SHARP(HASH),

    /**
     * Dagger (symbol: "†", code: 8224, unicode: u2020, HTML: &amp;#8224;)
     */
    DAGGER('†', "\u2020", "&#8224;"),

    /**
     * Double dagger (symbol: "‡", code: 8225, unicode: u2021, HTML:
     * &amp;#8225;)
     */
    DOUBLE_DAGGER('‡', "\u2021", "&#8225;"),

    /**
     * Double dagger (symbol: "‡", code: 8225, unicode: u2021, HTML:
     * &amp;#8225;)
     */
    DAGGER_DOUBLE(DOUBLE_DAGGER),

    /**
     * Prime (symbol: "′", code: 8242, unicode: u2032, HTML: &amp;#8242;)
     */
    PRIME('′', "\u2032", "&#8242;"),

    /**
     * Prime (symbol: "″", code: 8243, unicode: u2033, HTML: &amp;#8243;)
     */
    DOUBLE_PRIME('″', "\u2033", "&#8243;"),

    /**
     * Prime (symbol: "‴", code: 8244, unicode: u2034, HTML: &amp;#8244;)
     */
    TRIPLE_PRIME('‴', "\u2034", "&#8244;"),

    /**
     * Generic currency sign (symbol: "¤", code: 164, unicode: u00a4, HTML:
     * &amp;curren;)
     */
    CURRENCY('¤', "\u00a4", "&curren;"),

    /**
     * Dollar currency sign (symbol: "$", code: 36, unicode: u0024, HTML:
     * &amp;dollar;)
     */
    DOLLAR('$', "\u0024", "&dollar;"),

    /**
     * Pound Sterling currency sign (symbol: "£", code: 163, unicode: u00a3,
     * HTML: &amp;pound;)
     */
    POUND('£', "\u00a3", "&pound;"),

    /**
     * Euro currency sign (symbol: "€", code: 8364, unicode: u20ac, HTML:
     * &amp;euro;)
     */
    EURO('€', "\u20ac", "&euro;"),

    /**
     * Yen or Yuan currency sign (symbol: "¥", code: 165, unicode: u00a5, HTML:
     * &amp;yen;)
     */
    YEN('¥', "\u00a5", "&yen;"),

    /**
     * Yen or Yuan currency sign (symbol: "¥", code: 165, unicode: u00a5, HTML:
     * &amp;yen;)
     */
    YUAN(YEN),

    /**
     * Cent currency sign (symbol: "¢", code: 162, unicode: u00a2, HTML:
     * &amp;cent;)
     */
    CENT('¢', "\u00a2", "&cent;"),

    /**
     * Copyright (symbol: "©", code: 169, unicode: u00a9, HTML: &amp;copy;)
     */
    COPYRIGHT('©', "\u00a9", "&copy;"),

    /**
     * Sound recording copyright (symbol: "℗", code: 8471, unicode: u2117, HTML:
     * &amp;#8471;)
     */
    SOUND_RECORDING_COPYRIGHT('℗', "\u2117", "&#8471;"),

    /**
     * Service mark (symbol: "℠", code: 8480, unicode: u2120, HTML: &amp;#8480;)
     */
    SERVICE_MARK('℠', "\u2120", "&#8480;"),

    /**
     * Registered trademark (symbol: "®", code: 174, unicode: u00ae, HTML:
     * &amp;reg;)
     */
    REGISTERED_TRADEMARK('®', "\u00ae", "&reg;"),

    /**
     * Registered trademark (symbol: "®", code: 174, unicode: u00ae, HTML:
     * &amp;reg;)
     */
    TRADEMARK_REGISTERED(REGISTERED_TRADEMARK),

    /**
     * Trademark (symbol: "™", code: 8482, unicode: u2122, HTML: &amp;#8482;)
     */
    TRADEMARK('™', "\u2122", "&#8482;"),

    /**
     * Left parenthesis or left round bracket (symbol: "(", code: 40, unicode:
     * u0028, HTML: &amp;lpar;)
     */
    PARENTHESIS_OPEN('(', "\u0028", "&lpar;"),

    /**
     * Left parenthesis or left round bracket (symbol: "(", code: 40, unicode:
     * u0028, HTML: &amp;lpar;)
     */
    PARENTHESIS_LEFT(PARENTHESIS_OPEN),

    /**
     * Right parenthesis or right round bracket (symbol: ")", code: 41, unicode:
     * u0029, HTML: &amp;rpar;)
     */
    PARENTHESIS_CLOSE(')', "\u0029", "&rpar;"),

    /**
     * Right parenthesis or right round bracket (symbol: ")", code: 41, unicode:
     * u0029, HTML: &amp;rpar;)
     */
    PARENTHESIS_RIGHT(PARENTHESIS_CLOSE),

    /**
     * Left square bracket (symbol: "[", code: 91, unicode: u005b, HTML:
     * &amp;lbrack;)
     */
    BRACKET_OPEN('[', "\u005b", "&lbrack;"),

    /**
     * Left square bracket (symbol: "[", code: 91, unicode: u005b, HTML:
     * &amp;lbrack;)
     */
    BRACKET_LEFT(BRACKET_OPEN),

    /**
     * Right square bracket (symbol: "]", code: 93, unicode: u005d, HTML:
     * &amp;rbrack;)
     */
    BRACKET_CLOSE(']', "\u005d", "&rbrack;"),

    /**
     * Right square bracket (symbol: "]", code: 93, unicode: u005d, HTML:
     * &amp;rbrack;)
     */
    BRACKET_RIGHT(BRACKET_CLOSE),

    /**
     * Left brace or left curly bracket (symbol: "{", code: 123, unicode: u007b,
     * HTML: &amp;lbrace;)
     */
    BRACE_OPEN('{', "\u007b", "&lbrace;"),

    /**
     * Left brace or left curly bracket (symbol: "{", code: 123, unicode: u007b,
     * HTML: &amp;lbrace;)
     */
    BRACE_LEFT(BRACE_OPEN),

    /**
     * Left brace or left curly bracket (symbol: "{", code: 123, unicode: u007b,
     * HTML: &amp;lbrace;)
     */
    CURLY_BRACKET_OPEN(BRACE_OPEN),

    /**
     * Left brace or left curly bracket (symbol: "{", code: 123, unicode: u007b,
     * HTML: &amp;lbrace;)
     */
    CURLY_BRACKET_LEFT(BRACE_OPEN),

    /**
     * Right brace or right curly bracket (symbol: "}", code: 125, unicode:
     * u007d, HTML: &amp;rbrace;)
     */
    BRACE_CLOSE('}', "\u007d", "&rbrace;"),

    /**
     * Right brace or right curly bracket (symbol: "}", code: 125, unicode:
     * u007d, HTML: &amp;rbrace;)
     */
    BRACE_RIGHT(BRACE_CLOSE),

    /**
     * Right brace or right curly bracket (symbol: "}", code: 125, unicode:
     * u007d, HTML: &amp;rbrace;)
     */
    CURLY_BRACKET_CLOSE(BRACE_CLOSE),

    /**
     * Right brace or right curly bracket (symbol: "}", code: 125, unicode:
     * u007d, HTML: &amp;rbrace;)
     */
    CURLY_BRACKET_RIGHT(BRACE_CLOSE),

    /**
     * Lower than, less than or left chevron (symbol: "&lt;", code: 60, unicode:
     * u003c, HTML: &amp;lt;)
     */
    LOWER_THAN('<', "\u003c", "&lt;"),

    /**
     * Less than, less than or left chevron (symbol: "&lt;", code: 60, unicode:
     * u003c, HTML: &amp;lt;)
     */
    LESS_THAN(LOWER_THAN),

    /**
     * Lower than, less than or left chevron (symbol: "&lt;", code: 60, unicode:
     * u003c, HTML: &amp;lt;)
     */
    CHEVRON_OPEN(LOWER_THAN),

    /**
     * Lower than, less than or left chevron (symbol: "&lt;", code: 60, unicode:
     * u003c, HTML: &amp;lt;)
     */
    CHEVRON_LEFT(LOWER_THAN),

    /**
     * Greater than, more than or right chevron (symbol: "&gt;", code: 63,
     * unicode: u003e, HTML: &amp;gt;)
     */
    GREATER_THAN('>', "\u003e", "&gt;"),

    /**
     * Greater than, more than or right chevron (symbol: "&gt;", code: 63,
     * unicode: u003e, HTML: &amp;gt;)
     */
    MORE_THAN(GREATER_THAN),

    /**
     * Greater than, more than or right chevron (symbol: "&gt;", code: 63,
     * unicode: u003e, HTML: &amp;gt;)
     */
    CHEVRON_CLOSE(GREATER_THAN),

    /**
     * Greater than, more than or right chevron (symbol: "&gt;", code: 63,
     * unicode: u003e, HTML: &amp;gt;)
     */
    CHEVRON_RIGHT(GREATER_THAN),

    /**
     * Left angle quote or left guillemot (symbol: "«", code: 171, unicode:
     * u00ab, HTML: &amp;laquo;)
     */
    ANGLE_QUOTE_OPEN('«', "\u00ab", "&laquo;"),

    /**
     * Left angle quote or left guillemot (symbol: "«", code: 171, unicode:
     * u00ab, HTML: &amp;laquo;)
     */
    ANGLE_QUOTE_LEFT(ANGLE_QUOTE_OPEN),

    /**
     * Left angle quote or left guillemot (symbol: "«", code: 171, unicode:
     * u00ab, HTML: &amp;laquo;)
     */
    QUOTE_ANGLE_OPEN(ANGLE_QUOTE_OPEN),

    /**
     * Left angle quote or left guillemot (symbol: "«", code: 171, unicode:
     * u00ab, HTML: &amp;laquo;)
     */
    QUOTE_ANGLE_LEFT(ANGLE_QUOTE_OPEN),

    /**
     * Left angle quote or left guillemot (symbol: "«", code: 171, unicode:
     * u00ab, HTML: &amp;laquo;)
     */
    GUILLEMOT_OPEN(ANGLE_QUOTE_OPEN),

    /**
     * Left angle quote or left guillemot (symbol: "«", code: 171, unicode:
     * u00ab, HTML: &amp;laquo;)
     */
    GUILLEMOT_LEFT(ANGLE_QUOTE_OPEN),

    /**
     * Right angle quote or right guillemot (symbol: "«", code: 187, unicode:
     * u00bb, HTML: &amp;raquo;)
     */
    ANGLE_QUOTE_CLOSE('»', "\u00bb", "&raquo;"),

    /**
     * Right angle quote or right guillemot (symbol: "«", code: 187, unicode:
     * u00bb, HTML: &amp;raquo;)
     */
    ANGLE_QUOTE_RIGHT(ANGLE_QUOTE_CLOSE),

    /**
     * Right angle quote or right guillemot (symbol: "«", code: 187, unicode:
     * u00bb, HTML: &amp;raquo;)
     */
    QUOTE_ANGLE_CLOSE(ANGLE_QUOTE_CLOSE),

    /**
     * Right angle quote or right guillemot (symbol: "«", code: 187, unicode:
     * u00bb, HTML: &amp;raquo;)
     */
    QUOTE_ANGLE_RIGHT(ANGLE_QUOTE_CLOSE),

    /**
     * Right angle quote or right guillemot (symbol: "«", code: 187, unicode:
     * u00bb, HTML: &amp;raquo;)
     */
    GUILLEMOT_CLOSE(ANGLE_QUOTE_CLOSE),

    /**
     * Right angle quote or right guillemot (symbol: "«", code: 187, unicode:
     * u00bb, HTML: &amp;raquo;)
     */
    GUILLEMOT_RIGHT(ANGLE_QUOTE_CLOSE),

    /**
     * Equals sign (symbol: "=", code: 61, unicode: u003d, HTML: &amp;equals;)
     */
    EQUALS('=', "\u003d", "&equals;"),

    /**
     * Equivalent sign (symbol: "≡", code: 8801, unicode: u2261, HTML:
     * &amp;#8801;)
     */
    EQUIV('≡', "\u2261", "&#8801;"),

    /**
     * Plus sign (symbol: "+", code: 43, unicode: u002b, HTML: &amp;plus;)
     */
    PLUS('+', "\u002b", "&plus;"),

    /**
     * Minus sign (symbol: "−", code: 8722, unicode: u2212, HTML: &amp;minus;)
     */
    MINUS('−', "\u2212", "&minus;"),

    /**
     * Divide sign or Obelus (symbol: "÷", code: 247, unicode: u00f7, HTML:
     * &amp;divide;)
     */
    DIVISION('÷', "\u00f7", "&divide;"),

    /**
     * Divide sign or Obelus (symbol: "÷", code: 247, unicode: u00f7, HTML:
     * &amp;divide;)
     */
    DIVIDE(DIVISION),

    /**
     * Divide sign or Obelus (symbol: "÷", code: 247, unicode: u00f7, HTML:
     * &amp;divide;)
     */
    OBELUS(DIVISION),

    /**
     * Multiplication sign (symbol: "×", code: 215, unicode: u00d7, HTML:
     * &amp;times;)
     */
    MULTIPLICATION('×', "\u00d7", "&times;"),

    /**
     * Multiplication sign (symbol: "×", code: 215, unicode: u00d7, HTML:
     * &amp;times;)
     */
    TIMES(MULTIPLICATION),

    /**
     * Plus-minus sign (symbol: "±", code: 177, unicode: u00b1, HTML:
     * &amp;plusmn;)
     */
    PLUS_MINUS('±', "\u00b1", "&plusmn;"),

    /**
     * One half (symbol: "½", code: 189, unicode: u00bd, HTML: &amp;frac12;)
     */
    HALF('½', "\u00bd", "&frac12;"),

    /**
     * Quarter or one fourth (symbol: "½", code: 188, unicode: u00bc, HTML:
     * &amp;frac14;)
     */
    QUARTER('¼', "\u00bc", "&frac14;"),

    /**
     * Three quarters or three-fourths (symbol: "¾", code: 190, unicode: u00be,
     * HTML: &amp;frac34;)
     */
    THREE_QUARTERS('¾', "\u00be", "&frac34;"),

    /**
     * Superscript one, exponent 1 or first power (symbol: "¹", code: 185,
     * unicode: u00b9, HTML: &amp;frac34;)
     */
    SUPERSCRIPT_ONE('¹', "\u00b9", "&sup1;"),

    /**
     * Superscript one, exponent 1 or first power (symbol: "¹", code: 185,
     * unicode: u00b9, HTML: &amp;frac34;)
     */
    EXPONENT_ONE(SUPERSCRIPT_ONE),

    /**
     * Superscript two, exponent 2, square or second power (symbol: "²", code:
     * 178, unicode: u00b2, HTML: &amp;frac34;)
     */
    SUPERSCRIPT_TWO('²', "\u00b2", "&sup2;"),

    /**
     * Superscript two, exponent 2, square or second power (symbol: "²", code:
     * 178, unicode: u00b2, HTML: &amp;frac34;)
     */
    EXPONENT_TWO(SUPERSCRIPT_TWO),

    /**
     * Superscript two, exponent 2, square or second power (symbol: "²", code:
     * 178, unicode: u00b2, HTML: &amp;frac34;)
     */
    SQUARE(SUPERSCRIPT_TWO),

    /**
     * Superscript three, exponent 3, cube or third power (symbol: "³", code:
     * 179, unicode: u00b3, HTML: &amp;frac34;)
     */
    SUPERSCRIPT_THREE('³', "\u00b3", "&sup3;"),

    /**
     * Superscript three, exponent 3, cube or third power (symbol: "³", code:
     * 179, unicode: u00b3, HTML: &amp;frac34;)
     */
    EXPONENT_THREE(SUPERSCRIPT_THREE),

    /**
     * Superscript three, exponent 3, cube or third power (symbol: "³", code:
     * 179, unicode: u00b3, HTML: &amp;frac34;)
     */
    CUBE(SUPERSCRIPT_THREE),

    /**
     * Function sign or Florin sign (symbol: "ƒ", code: 402, unicode: u0192,
     * HTML: &amp;#402;)
     */
    FUNCTION('ƒ', "\u0192", "&#402;"),

    /**
     * Function sign or Florin sign (symbol: "ƒ", code: 402, unicode: u0192,
     * HTML: &amp;#402;)
     */
    FLORIN(FUNCTION),

    /**
     * Uppercase slashed zero or empty set (symbol: "Ø", code: 216, unicode:
     * u00d8, HTML: &amp;Oslash;)
     */
    ZERO('Ø', "\u00d8", "&Oslash;"),

    /**
     * Uppercase slashed zero or empty set (symbol: "Ø", code: 216, unicode:
     * u00d8, HTML: &amp;Oslash;)
     */
    EMPTY_SET(ZERO),

    /**
     * Lowercase letter Mu, micro sign or micron (symbol: "µ", code: 181,
     * unicode: u00b5, HTML: &amp;micro;)
     */
    MU('µ', "\u00b5", "&micro;"),

    /**
     * Lowercase letter Mu, micro sign or micron (symbol: "µ", code: 181,
     * unicode: u00b5, HTML: &amp;micro;)
     */
    MICRO(MU),

    /**
     * Lowercase letter Mu, micro sign or micron (symbol: "µ", code: 181,
     * unicode: u00b5, HTML: &amp;micro;)
     */
    MICRON(MU),

    /**
     * Macro symbol (symbol: "¯", code: 175, unicode: u00af, HTML: &amp;macr;)
     */
    MACRON('¯', "\u00af", "&macr;"),

    /**
     * Feminine ordinal indicator (symbol: "ª", code: 170, unicode: u00aa, HTML:
     * &amp;ordf;)
     */
    FEMININE_ORDINAL_INDICATOR('ª', "\u00aa", "&ordf;"),

    /**
     * Feminine ordinal indicator (symbol: "ª", code: 170, unicode: u00aa, HTML:
     * &amp;ordf;)
     */
    ORDINAL_FEMININE_INDICATOR(FEMININE_ORDINAL_INDICATOR),

    /**
     * Masculine ordinal indicator (symbol: "º", code: 186, unicode: u00ba,
     * HTML: &amp;ordm;)
     */
    MASCULINE_ORDINAL_INDICATOR('º', "\u00ba", "&ordm;"),

    /**
     * Masculine ordinal indicator (symbol: "º", code: 186, unicode: u00ba,
     * HTML: &amp;ordm;)
     */
    ORDINAL_MASCULINE_INDICATOR(MASCULINE_ORDINAL_INDICATOR);

    /**
     * The size of a character in memory
     */
    public static final int SIZE = 2;

    private char character;
    private String unicode;
    private String html;

    EnumChar(final char character, final String unicode) {
        this.character = character;
        this.unicode = unicode;
    }

    EnumChar(final int character, final String unicode) {
        this((char) character, unicode);
    }

    EnumChar(final char character, final String unicode, final String html) {
        this(character, unicode);
        this.html = html;
    }

    EnumChar(final int character, final String unicode, final String html) {
        this((char) character, unicode, html);
    }

    EnumChar(final EnumChar character) {
        this(character.character, character.unicode, character.html);
    }

    /**
     * @return the character
     */
    public char getCharacter() {
        return this.character;
    }

    /**
     * @return the character
     */
    public int getCode() {
        return this.character;
    }

    /**
     * @return the Unicode
     */
    public String getUnicode() {
        return this.unicode;
    }

    /**
     * @return the HTML
     */
    public String getHTML() {
        return this.html;
    }

    @Override
    public String toString() {
        return this.getUnicode();
    }
}
