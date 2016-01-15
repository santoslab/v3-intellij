/*
 Copyright (c) 2016, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// Generated from /Users/robby/Repositories/Sireum/sireum-v3-intellij/sireum-v3/logika/jvm/src/main/resources/org/sireum/logika/parser/Antlr4Logika.g4 by ANTLR 4.5.1
package org.sireum.intellij.logika.lexer;

// @formatter:off

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class Antlr4LogikaLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.5.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, T__21=22, T__22=23, T__23=24, 
		T__24=25, T__25=26, T__26=27, T__27=28, T__28=29, T__29=30, T__30=31, 
		T__31=32, T__32=33, T__33=34, T__34=35, T__35=36, T__36=37, T__37=38, 
		T__38=39, T__39=40, T__40=41, T__41=42, T__42=43, T__43=44, T__44=45, 
		T__45=46, T__46=47, T__47=48, T__48=49, T__49=50, T__50=51, T__51=52, 
		T__52=53, T__53=54, T__54=55, T__55=56, T__56=57, T__57=58, T__58=59, 
		T__59=60, T__60=61, T__61=62, T__62=63, T__63=64, T__64=65, T__65=66, 
		T__66=67, T__67=68, T__68=69, T__69=70, T__70=71, T__71=72, T__72=73, 
		T__73=74, T__74=75, T__75=76, T__76=77, T__77=78, T__78=79, T__79=80, 
		T__80=81, T__81=82, T__82=83, T__83=84, T__84=85, T__85=86, T__86=87, 
		T__87=88, T__88=89, T__89=90, T__90=91, T__91=92, T__92=93, T__93=94, 
		T__94=95, T__95=96, T__96=97, T__97=98, T__98=99, T__99=100, T__100=101, 
		T__101=102, T__102=103, T__103=104, T__104=105, T__105=106, T__106=107, 
		T__107=108, T__108=109, T__109=110, T__110=111, T__111=112, T__112=113, 
		T__113=114, T__114=115, T__115=116, T__116=117, T__117=118, T__118=119, 
		T__119=120, T__120=121, T__121=122, T__122=123, HLINE=124, NUM=125, ID=126, 
		RESERVED=127, NL=128, LINE_COMMENT=129, COMMENT=130, WS=131, STRING=132, 
		ERROR_CHAR=133;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "T__7", "T__8", 
		"T__9", "T__10", "T__11", "T__12", "T__13", "T__14", "T__15", "T__16", 
		"T__17", "T__18", "T__19", "T__20", "T__21", "T__22", "T__23", "T__24", 
		"T__25", "T__26", "T__27", "T__28", "T__29", "T__30", "T__31", "T__32", 
		"T__33", "T__34", "T__35", "T__36", "T__37", "T__38", "T__39", "T__40", 
		"T__41", "T__42", "T__43", "T__44", "T__45", "T__46", "T__47", "T__48", 
		"T__49", "T__50", "T__51", "T__52", "T__53", "T__54", "T__55", "T__56", 
		"T__57", "T__58", "T__59", "T__60", "T__61", "T__62", "T__63", "T__64", 
		"T__65", "T__66", "T__67", "T__68", "T__69", "T__70", "T__71", "T__72", 
		"T__73", "T__74", "T__75", "T__76", "T__77", "T__78", "T__79", "T__80", 
		"T__81", "T__82", "T__83", "T__84", "T__85", "T__86", "T__87", "T__88", 
		"T__89", "T__90", "T__91", "T__92", "T__93", "T__94", "T__95", "T__96", 
		"T__97", "T__98", "T__99", "T__100", "T__101", "T__102", "T__103", "T__104", 
		"T__105", "T__106", "T__107", "T__108", "T__109", "T__110", "T__111", 
		"T__112", "T__113", "T__114", "T__115", "T__116", "T__117", "T__118", 
		"T__119", "T__120", "T__121", "T__122", "HLINE", "NUM", "ID", "RESERVED", 
		"NL", "LINE_COMMENT", "COMMENT", "WS", "STRING", "StringCharacters", "StringCharacter", 
		"EscapeSequence", "UnicodeEscape", "HexDigit", "ERROR_CHAR"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "','", "'|-'", "'⊢'", "'{'", "'}'", "'.'", "':'", "'assume'", "'true'", 
		"'T'", "'⊤'", "'false'", "'F'", "'_|_'", "'⊥'", "'('", "')'", "'result'", 
		"'Z'", "'ZS'", "'*'", "'/'", "'%'", "'+'", "'-'", "'+:'", "':+'", "'='", 
		"'=='", "'!='", "'≠'", "'not'", "'neg'", "'!'", "'~'", "'¬'", "'<'", "'<='", 
		"'≤'", "'>'", "'>='", "'≥'", "'and'", "'&'", "'^'", "'∧'", "'or'", "'|'", 
		"'V'", "'∨'", "'implies'", "'->'", "'→'", "'forall'", "'all'", "'A'", 
		"'∀'", "'exists'", "'some'", "'E'", "'∃'", "'..'", "'B'", "'premise'", 
		"'andi'", "'ande1'", "'ande2'", "'ori1'", "'Vi1'", "'ori2'", "'Vi2'", 
		"'ore'", "'Ve'", "'impliesi'", "'impliese'", "'noti'", "'negi'", "'note'", 
		"'nege'", "'bottome'", "'falsee'", "'pbc'", "'subst1'", "'subst2'", "'algebra'", 
		"'foralli'", "'alli'", "'Ai'", "'foralle'", "'alle'", "'Ae'", "'existsi'", 
		"'somei'", "'Ei'", "'existse'", "'somee'", "'Ee'", "'invariant'", "'auto'", 
		"'import'", "'_'", "'fact'", "'def'", "'var'", "'val'", "'assert'", "'if'", 
		"'else'", "'while'", "'l\"\"\"'", "'\"\"\"'", "'print'", "'println'", 
		"'@'", "'Unit'", "'return'", "'randomInt'", "'readInt'", "'modifies'", 
		"'requires'", "'pre'", "'ensures'", "'post'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, "HLINE", "NUM", "ID", "RESERVED", "NL", "LINE_COMMENT", 
		"COMMENT", "WS", "STRING", "ERROR_CHAR"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public Antlr4LogikaLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Antlr4Logika.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\u0087\u0430\b\1\4"+
		"\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n"+
		"\4\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t"+
		" \4!\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t"+
		"+\4,\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64"+
		"\t\64\4\65\t\65\4\66\t\66\4\67\t\67\48\t8\49\t9\4:\t:\4;\t;\4<\t<\4=\t"+
		"=\4>\t>\4?\t?\4@\t@\4A\tA\4B\tB\4C\tC\4D\tD\4E\tE\4F\tF\4G\tG\4H\tH\4"+
		"I\tI\4J\tJ\4K\tK\4L\tL\4M\tM\4N\tN\4O\tO\4P\tP\4Q\tQ\4R\tR\4S\tS\4T\t"+
		"T\4U\tU\4V\tV\4W\tW\4X\tX\4Y\tY\4Z\tZ\4[\t[\4\\\t\\\4]\t]\4^\t^\4_\t_"+
		"\4`\t`\4a\ta\4b\tb\4c\tc\4d\td\4e\te\4f\tf\4g\tg\4h\th\4i\ti\4j\tj\4k"+
		"\tk\4l\tl\4m\tm\4n\tn\4o\to\4p\tp\4q\tq\4r\tr\4s\ts\4t\tt\4u\tu\4v\tv"+
		"\4w\tw\4x\tx\4y\ty\4z\tz\4{\t{\4|\t|\4}\t}\4~\t~\4\177\t\177\4\u0080\t"+
		"\u0080\4\u0081\t\u0081\4\u0082\t\u0082\4\u0083\t\u0083\4\u0084\t\u0084"+
		"\4\u0085\t\u0085\4\u0086\t\u0086\4\u0087\t\u0087\4\u0088\t\u0088\4\u0089"+
		"\t\u0089\4\u008a\t\u008a\4\u008b\t\u008b\3\2\3\2\3\3\3\3\3\3\3\4\3\4\3"+
		"\5\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n"+
		"\3\n\3\n\3\13\3\13\3\f\3\f\3\r\3\r\3\r\3\r\3\r\3\r\3\16\3\16\3\17\3\17"+
		"\3\17\3\17\3\20\3\20\3\21\3\21\3\22\3\22\3\23\3\23\3\23\3\23\3\23\3\23"+
		"\3\23\3\24\3\24\3\25\3\25\3\25\3\26\3\26\3\27\3\27\3\30\3\30\3\31\3\31"+
		"\3\32\3\32\3\33\3\33\3\33\3\34\3\34\3\34\3\35\3\35\3\36\3\36\3\36\3\37"+
		"\3\37\3\37\3 \3 \3!\3!\3!\3!\3\"\3\"\3\"\3\"\3#\3#\3$\3$\3%\3%\3&\3&\3"+
		"\'\3\'\3\'\3(\3(\3)\3)\3*\3*\3*\3+\3+\3,\3,\3,\3,\3-\3-\3.\3.\3/\3/\3"+
		"\60\3\60\3\60\3\61\3\61\3\62\3\62\3\63\3\63\3\64\3\64\3\64\3\64\3\64\3"+
		"\64\3\64\3\64\3\65\3\65\3\65\3\66\3\66\3\67\3\67\3\67\3\67\3\67\3\67\3"+
		"\67\38\38\38\38\39\39\3:\3:\3;\3;\3;\3;\3;\3;\3;\3<\3<\3<\3<\3<\3=\3="+
		"\3>\3>\3?\3?\3?\3@\3@\3A\3A\3A\3A\3A\3A\3A\3A\3B\3B\3B\3B\3B\3C\3C\3C"+
		"\3C\3C\3C\3D\3D\3D\3D\3D\3D\3E\3E\3E\3E\3E\3F\3F\3F\3F\3G\3G\3G\3G\3G"+
		"\3H\3H\3H\3H\3I\3I\3I\3I\3J\3J\3J\3K\3K\3K\3K\3K\3K\3K\3K\3K\3L\3L\3L"+
		"\3L\3L\3L\3L\3L\3L\3M\3M\3M\3M\3M\3N\3N\3N\3N\3N\3O\3O\3O\3O\3O\3P\3P"+
		"\3P\3P\3P\3Q\3Q\3Q\3Q\3Q\3Q\3Q\3Q\3R\3R\3R\3R\3R\3R\3R\3S\3S\3S\3S\3T"+
		"\3T\3T\3T\3T\3T\3T\3U\3U\3U\3U\3U\3U\3U\3V\3V\3V\3V\3V\3V\3V\3V\3W\3W"+
		"\3W\3W\3W\3W\3W\3W\3X\3X\3X\3X\3X\3Y\3Y\3Y\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3["+
		"\3[\3[\3[\3[\3\\\3\\\3\\\3]\3]\3]\3]\3]\3]\3]\3]\3^\3^\3^\3^\3^\3^\3_"+
		"\3_\3_\3`\3`\3`\3`\3`\3`\3`\3`\3a\3a\3a\3a\3a\3a\3b\3b\3b\3c\3c\3c\3c"+
		"\3c\3c\3c\3c\3c\3c\3d\3d\3d\3d\3d\3e\3e\3e\3e\3e\3e\3e\3f\3f\3g\3g\3g"+
		"\3g\3g\3h\3h\3h\3h\3i\3i\3i\3i\3j\3j\3j\3j\3k\3k\3k\3k\3k\3k\3k\3l\3l"+
		"\3l\3m\3m\3m\3m\3m\3n\3n\3n\3n\3n\3n\3o\3o\3o\3o\3o\3p\3p\3p\3p\3q\3q"+
		"\3q\3q\3q\3q\3r\3r\3r\3r\3r\3r\3r\3r\3s\3s\3t\3t\3t\3t\3t\3u\3u\3u\3u"+
		"\3u\3u\3u\3v\3v\3v\3v\3v\3v\3v\3v\3v\3v\3w\3w\3w\3w\3w\3w\3w\3w\3x\3x"+
		"\3x\3x\3x\3x\3x\3x\3x\3y\3y\3y\3y\3y\3y\3y\3y\3y\3z\3z\3z\3z\3{\3{\3{"+
		"\3{\3{\3{\3{\3{\3|\3|\3|\3|\3|\3}\3}\3}\6}\u032d\n}\r}\16}\u032e\3~\3"+
		"~\3~\7~\u0334\n~\f~\16~\u0337\13~\5~\u0339\n~\3\177\3\177\7\177\u033d"+
		"\n\177\f\177\16\177\u0340\13\177\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080\3\u0080"+
		"\3\u0080\3\u0080\5\u0080\u03eb\n\u0080\3\u0081\5\u0081\u03ee\n\u0081\3"+
		"\u0081\3\u0081\3\u0082\3\u0082\3\u0082\3\u0082\7\u0082\u03f6\n\u0082\f"+
		"\u0082\16\u0082\u03f9\13\u0082\3\u0082\3\u0082\3\u0083\3\u0083\3\u0083"+
		"\3\u0083\7\u0083\u0401\n\u0083\f\u0083\16\u0083\u0404\13\u0083\3\u0083"+
		"\3\u0083\3\u0083\3\u0083\3\u0083\3\u0084\6\u0084\u040c\n\u0084\r\u0084"+
		"\16\u0084\u040d\3\u0084\3\u0084\3\u0085\3\u0085\5\u0085\u0414\n\u0085"+
		"\3\u0085\3\u0085\3\u0086\6\u0086\u0419\n\u0086\r\u0086\16\u0086\u041a"+
		"\3\u0087\3\u0087\5\u0087\u041f\n\u0087\3\u0088\3\u0088\3\u0088\5\u0088"+
		"\u0424\n\u0088\3\u0089\3\u0089\3\u0089\3\u0089\3\u0089\3\u0089\3\u0089"+
		"\3\u008a\3\u008a\3\u008b\3\u008b\3\u0402\2\u008c\3\3\5\4\7\5\t\6\13\7"+
		"\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\23%\24\'\25"+
		")\26+\27-\30/\31\61\32\63\33\65\34\67\359\36;\37= ?!A\"C#E$G%I&K\'M(O"+
		")Q*S+U,W-Y.[/]\60_\61a\62c\63e\64g\65i\66k\67m8o9q:s;u<w=y>{?}@\177A\u0081"+
		"B\u0083C\u0085D\u0087E\u0089F\u008bG\u008dH\u008fI\u0091J\u0093K\u0095"+
		"L\u0097M\u0099N\u009bO\u009dP\u009fQ\u00a1R\u00a3S\u00a5T\u00a7U\u00a9"+
		"V\u00abW\u00adX\u00afY\u00b1Z\u00b3[\u00b5\\\u00b7]\u00b9^\u00bb_\u00bd"+
		"`\u00bfa\u00c1b\u00c3c\u00c5d\u00c7e\u00c9f\u00cbg\u00cdh\u00cfi\u00d1"+
		"j\u00d3k\u00d5l\u00d7m\u00d9n\u00dbo\u00ddp\u00dfq\u00e1r\u00e3s\u00e5"+
		"t\u00e7u\u00e9v\u00ebw\u00edx\u00efy\u00f1z\u00f3{\u00f5|\u00f7}\u00f9"+
		"~\u00fb\177\u00fd\u0080\u00ff\u0081\u0101\u0082\u0103\u0083\u0105\u0084"+
		"\u0107\u0085\u0109\u0086\u010b\2\u010d\2\u010f\2\u0111\2\u0113\2\u0115"+
		"\u0087\3\2\f\3\2\63;\3\2\62;\4\2C\\c|\6\2\62;C\\aac|\4\2%%BB\4\2\f\f\17"+
		"\17\5\2\13\13\16\16\"\"\4\2$$^^\n\2$$))^^ddhhppttvv\5\2\62;CHch\u0458"+
		"\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2"+
		"\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2"+
		"\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2"+
		"\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2"+
		"\2\2\61\3\2\2\2\2\63\3\2\2\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3"+
		"\2\2\2\2=\3\2\2\2\2?\3\2\2\2\2A\3\2\2\2\2C\3\2\2\2\2E\3\2\2\2\2G\3\2\2"+
		"\2\2I\3\2\2\2\2K\3\2\2\2\2M\3\2\2\2\2O\3\2\2\2\2Q\3\2\2\2\2S\3\2\2\2\2"+
		"U\3\2\2\2\2W\3\2\2\2\2Y\3\2\2\2\2[\3\2\2\2\2]\3\2\2\2\2_\3\2\2\2\2a\3"+
		"\2\2\2\2c\3\2\2\2\2e\3\2\2\2\2g\3\2\2\2\2i\3\2\2\2\2k\3\2\2\2\2m\3\2\2"+
		"\2\2o\3\2\2\2\2q\3\2\2\2\2s\3\2\2\2\2u\3\2\2\2\2w\3\2\2\2\2y\3\2\2\2\2"+
		"{\3\2\2\2\2}\3\2\2\2\2\177\3\2\2\2\2\u0081\3\2\2\2\2\u0083\3\2\2\2\2\u0085"+
		"\3\2\2\2\2\u0087\3\2\2\2\2\u0089\3\2\2\2\2\u008b\3\2\2\2\2\u008d\3\2\2"+
		"\2\2\u008f\3\2\2\2\2\u0091\3\2\2\2\2\u0093\3\2\2\2\2\u0095\3\2\2\2\2\u0097"+
		"\3\2\2\2\2\u0099\3\2\2\2\2\u009b\3\2\2\2\2\u009d\3\2\2\2\2\u009f\3\2\2"+
		"\2\2\u00a1\3\2\2\2\2\u00a3\3\2\2\2\2\u00a5\3\2\2\2\2\u00a7\3\2\2\2\2\u00a9"+
		"\3\2\2\2\2\u00ab\3\2\2\2\2\u00ad\3\2\2\2\2\u00af\3\2\2\2\2\u00b1\3\2\2"+
		"\2\2\u00b3\3\2\2\2\2\u00b5\3\2\2\2\2\u00b7\3\2\2\2\2\u00b9\3\2\2\2\2\u00bb"+
		"\3\2\2\2\2\u00bd\3\2\2\2\2\u00bf\3\2\2\2\2\u00c1\3\2\2\2\2\u00c3\3\2\2"+
		"\2\2\u00c5\3\2\2\2\2\u00c7\3\2\2\2\2\u00c9\3\2\2\2\2\u00cb\3\2\2\2\2\u00cd"+
		"\3\2\2\2\2\u00cf\3\2\2\2\2\u00d1\3\2\2\2\2\u00d3\3\2\2\2\2\u00d5\3\2\2"+
		"\2\2\u00d7\3\2\2\2\2\u00d9\3\2\2\2\2\u00db\3\2\2\2\2\u00dd\3\2\2\2\2\u00df"+
		"\3\2\2\2\2\u00e1\3\2\2\2\2\u00e3\3\2\2\2\2\u00e5\3\2\2\2\2\u00e7\3\2\2"+
		"\2\2\u00e9\3\2\2\2\2\u00eb\3\2\2\2\2\u00ed\3\2\2\2\2\u00ef\3\2\2\2\2\u00f1"+
		"\3\2\2\2\2\u00f3\3\2\2\2\2\u00f5\3\2\2\2\2\u00f7\3\2\2\2\2\u00f9\3\2\2"+
		"\2\2\u00fb\3\2\2\2\2\u00fd\3\2\2\2\2\u00ff\3\2\2\2\2\u0101\3\2\2\2\2\u0103"+
		"\3\2\2\2\2\u0105\3\2\2\2\2\u0107\3\2\2\2\2\u0109\3\2\2\2\2\u0115\3\2\2"+
		"\2\3\u0117\3\2\2\2\5\u0119\3\2\2\2\7\u011c\3\2\2\2\t\u011e\3\2\2\2\13"+
		"\u0120\3\2\2\2\r\u0122\3\2\2\2\17\u0124\3\2\2\2\21\u0126\3\2\2\2\23\u012d"+
		"\3\2\2\2\25\u0132\3\2\2\2\27\u0134\3\2\2\2\31\u0136\3\2\2\2\33\u013c\3"+
		"\2\2\2\35\u013e\3\2\2\2\37\u0142\3\2\2\2!\u0144\3\2\2\2#\u0146\3\2\2\2"+
		"%\u0148\3\2\2\2\'\u014f\3\2\2\2)\u0151\3\2\2\2+\u0154\3\2\2\2-\u0156\3"+
		"\2\2\2/\u0158\3\2\2\2\61\u015a\3\2\2\2\63\u015c\3\2\2\2\65\u015e\3\2\2"+
		"\2\67\u0161\3\2\2\29\u0164\3\2\2\2;\u0166\3\2\2\2=\u0169\3\2\2\2?\u016c"+
		"\3\2\2\2A\u016e\3\2\2\2C\u0172\3\2\2\2E\u0176\3\2\2\2G\u0178\3\2\2\2I"+
		"\u017a\3\2\2\2K\u017c\3\2\2\2M\u017e\3\2\2\2O\u0181\3\2\2\2Q\u0183\3\2"+
		"\2\2S\u0185\3\2\2\2U\u0188\3\2\2\2W\u018a\3\2\2\2Y\u018e\3\2\2\2[\u0190"+
		"\3\2\2\2]\u0192\3\2\2\2_\u0194\3\2\2\2a\u0197\3\2\2\2c\u0199\3\2\2\2e"+
		"\u019b\3\2\2\2g\u019d\3\2\2\2i\u01a5\3\2\2\2k\u01a8\3\2\2\2m\u01aa\3\2"+
		"\2\2o\u01b1\3\2\2\2q\u01b5\3\2\2\2s\u01b7\3\2\2\2u\u01b9\3\2\2\2w\u01c0"+
		"\3\2\2\2y\u01c5\3\2\2\2{\u01c7\3\2\2\2}\u01c9\3\2\2\2\177\u01cc\3\2\2"+
		"\2\u0081\u01ce\3\2\2\2\u0083\u01d6\3\2\2\2\u0085\u01db\3\2\2\2\u0087\u01e1"+
		"\3\2\2\2\u0089\u01e7\3\2\2\2\u008b\u01ec\3\2\2\2\u008d\u01f0\3\2\2\2\u008f"+
		"\u01f5\3\2\2\2\u0091\u01f9\3\2\2\2\u0093\u01fd\3\2\2\2\u0095\u0200\3\2"+
		"\2\2\u0097\u0209\3\2\2\2\u0099\u0212\3\2\2\2\u009b\u0217\3\2\2\2\u009d"+
		"\u021c\3\2\2\2\u009f\u0221\3\2\2\2\u00a1\u0226\3\2\2\2\u00a3\u022e\3\2"+
		"\2\2\u00a5\u0235\3\2\2\2\u00a7\u0239\3\2\2\2\u00a9\u0240\3\2\2\2\u00ab"+
		"\u0247\3\2\2\2\u00ad\u024f\3\2\2\2\u00af\u0257\3\2\2\2\u00b1\u025c\3\2"+
		"\2\2\u00b3\u025f\3\2\2\2\u00b5\u0267\3\2\2\2\u00b7\u026c\3\2\2\2\u00b9"+
		"\u026f\3\2\2\2\u00bb\u0277\3\2\2\2\u00bd\u027d\3\2\2\2\u00bf\u0280\3\2"+
		"\2\2\u00c1\u0288\3\2\2\2\u00c3\u028e\3\2\2\2\u00c5\u0291\3\2\2\2\u00c7"+
		"\u029b\3\2\2\2\u00c9\u02a0\3\2\2\2\u00cb\u02a7\3\2\2\2\u00cd\u02a9\3\2"+
		"\2\2\u00cf\u02ae\3\2\2\2\u00d1\u02b2\3\2\2\2\u00d3\u02b6\3\2\2\2\u00d5"+
		"\u02ba\3\2\2\2\u00d7\u02c1\3\2\2\2\u00d9\u02c4\3\2\2\2\u00db\u02c9\3\2"+
		"\2\2\u00dd\u02cf\3\2\2\2\u00df\u02d4\3\2\2\2\u00e1\u02d8\3\2\2\2\u00e3"+
		"\u02de\3\2\2\2\u00e5\u02e6\3\2\2\2\u00e7\u02e8\3\2\2\2\u00e9\u02ed\3\2"+
		"\2\2\u00eb\u02f4\3\2\2\2\u00ed\u02fe\3\2\2\2\u00ef\u0306\3\2\2\2\u00f1"+
		"\u030f\3\2\2\2\u00f3\u0318\3\2\2\2\u00f5\u031c\3\2\2\2\u00f7\u0324\3\2"+
		"\2\2\u00f9\u0329\3\2\2\2\u00fb\u0338\3\2\2\2\u00fd\u033a\3\2\2\2\u00ff"+
		"\u03ea\3\2\2\2\u0101\u03ed\3\2\2\2\u0103\u03f1\3\2\2\2\u0105\u03fc\3\2"+
		"\2\2\u0107\u040b\3\2\2\2\u0109\u0411\3\2\2\2\u010b\u0418\3\2\2\2\u010d"+
		"\u041e\3\2\2\2\u010f\u0423\3\2\2\2\u0111\u0425\3\2\2\2\u0113\u042c\3\2"+
		"\2\2\u0115\u042e\3\2\2\2\u0117\u0118\7.\2\2\u0118\4\3\2\2\2\u0119\u011a"+
		"\7~\2\2\u011a\u011b\7/\2\2\u011b\6\3\2\2\2\u011c\u011d\7\u22a4\2\2\u011d"+
		"\b\3\2\2\2\u011e\u011f\7}\2\2\u011f\n\3\2\2\2\u0120\u0121\7\177\2\2\u0121"+
		"\f\3\2\2\2\u0122\u0123\7\60\2\2\u0123\16\3\2\2\2\u0124\u0125\7<\2\2\u0125"+
		"\20\3\2\2\2\u0126\u0127\7c\2\2\u0127\u0128\7u\2\2\u0128\u0129\7u\2\2\u0129"+
		"\u012a\7w\2\2\u012a\u012b\7o\2\2\u012b\u012c\7g\2\2\u012c\22\3\2\2\2\u012d"+
		"\u012e\7v\2\2\u012e\u012f\7t\2\2\u012f\u0130\7w\2\2\u0130\u0131\7g\2\2"+
		"\u0131\24\3\2\2\2\u0132\u0133\7V\2\2\u0133\26\3\2\2\2\u0134\u0135\7\u22a6"+
		"\2\2\u0135\30\3\2\2\2\u0136\u0137\7h\2\2\u0137\u0138\7c\2\2\u0138\u0139"+
		"\7n\2\2\u0139\u013a\7u\2\2\u013a\u013b\7g\2\2\u013b\32\3\2\2\2\u013c\u013d"+
		"\7H\2\2\u013d\34\3\2\2\2\u013e\u013f\7a\2\2\u013f\u0140\7~\2\2\u0140\u0141"+
		"\7a\2\2\u0141\36\3\2\2\2\u0142\u0143\7\u22a7\2\2\u0143 \3\2\2\2\u0144"+
		"\u0145\7*\2\2\u0145\"\3\2\2\2\u0146\u0147\7+\2\2\u0147$\3\2\2\2\u0148"+
		"\u0149\7t\2\2\u0149\u014a\7g\2\2\u014a\u014b\7u\2\2\u014b\u014c\7w\2\2"+
		"\u014c\u014d\7n\2\2\u014d\u014e\7v\2\2\u014e&\3\2\2\2\u014f\u0150\7\\"+
		"\2\2\u0150(\3\2\2\2\u0151\u0152\7\\\2\2\u0152\u0153\7U\2\2\u0153*\3\2"+
		"\2\2\u0154\u0155\7,\2\2\u0155,\3\2\2\2\u0156\u0157\7\61\2\2\u0157.\3\2"+
		"\2\2\u0158\u0159\7\'\2\2\u0159\60\3\2\2\2\u015a\u015b\7-\2\2\u015b\62"+
		"\3\2\2\2\u015c\u015d\7/\2\2\u015d\64\3\2\2\2\u015e\u015f\7-\2\2\u015f"+
		"\u0160\7<\2\2\u0160\66\3\2\2\2\u0161\u0162\7<\2\2\u0162\u0163\7-\2\2\u0163"+
		"8\3\2\2\2\u0164\u0165\7?\2\2\u0165:\3\2\2\2\u0166\u0167\7?\2\2\u0167\u0168"+
		"\7?\2\2\u0168<\3\2\2\2\u0169\u016a\7#\2\2\u016a\u016b\7?\2\2\u016b>\3"+
		"\2\2\2\u016c\u016d\7\u2262\2\2\u016d@\3\2\2\2\u016e\u016f\7p\2\2\u016f"+
		"\u0170\7q\2\2\u0170\u0171\7v\2\2\u0171B\3\2\2\2\u0172\u0173\7p\2\2\u0173"+
		"\u0174\7g\2\2\u0174\u0175\7i\2\2\u0175D\3\2\2\2\u0176\u0177\7#\2\2\u0177"+
		"F\3\2\2\2\u0178\u0179\7\u0080\2\2\u0179H\3\2\2\2\u017a\u017b\7\u00ae\2"+
		"\2\u017bJ\3\2\2\2\u017c\u017d\7>\2\2\u017dL\3\2\2\2\u017e\u017f\7>\2\2"+
		"\u017f\u0180\7?\2\2\u0180N\3\2\2\2\u0181\u0182\7\u2266\2\2\u0182P\3\2"+
		"\2\2\u0183\u0184\7@\2\2\u0184R\3\2\2\2\u0185\u0186\7@\2\2\u0186\u0187"+
		"\7?\2\2\u0187T\3\2\2\2\u0188\u0189\7\u2267\2\2\u0189V\3\2\2\2\u018a\u018b"+
		"\7c\2\2\u018b\u018c\7p\2\2\u018c\u018d\7f\2\2\u018dX\3\2\2\2\u018e\u018f"+
		"\7(\2\2\u018fZ\3\2\2\2\u0190\u0191\7`\2\2\u0191\\\3\2\2\2\u0192\u0193"+
		"\7\u2229\2\2\u0193^\3\2\2\2\u0194\u0195\7q\2\2\u0195\u0196\7t\2\2\u0196"+
		"`\3\2\2\2\u0197\u0198\7~\2\2\u0198b\3\2\2\2\u0199\u019a\7X\2\2\u019ad"+
		"\3\2\2\2\u019b\u019c\7\u222a\2\2\u019cf\3\2\2\2\u019d\u019e\7k\2\2\u019e"+
		"\u019f\7o\2\2\u019f\u01a0\7r\2\2\u01a0\u01a1\7n\2\2\u01a1\u01a2\7k\2\2"+
		"\u01a2\u01a3\7g\2\2\u01a3\u01a4\7u\2\2\u01a4h\3\2\2\2\u01a5\u01a6\7/\2"+
		"\2\u01a6\u01a7\7@\2\2\u01a7j\3\2\2\2\u01a8\u01a9\7\u2194\2\2\u01a9l\3"+
		"\2\2\2\u01aa\u01ab\7h\2\2\u01ab\u01ac\7q\2\2\u01ac\u01ad\7t\2\2\u01ad"+
		"\u01ae\7c\2\2\u01ae\u01af\7n\2\2\u01af\u01b0\7n\2\2\u01b0n\3\2\2\2\u01b1"+
		"\u01b2\7c\2\2\u01b2\u01b3\7n\2\2\u01b3\u01b4\7n\2\2\u01b4p\3\2\2\2\u01b5"+
		"\u01b6\7C\2\2\u01b6r\3\2\2\2\u01b7\u01b8\7\u2202\2\2\u01b8t\3\2\2\2\u01b9"+
		"\u01ba\7g\2\2\u01ba\u01bb\7z\2\2\u01bb\u01bc\7k\2\2\u01bc\u01bd\7u\2\2"+
		"\u01bd\u01be\7v\2\2\u01be\u01bf\7u\2\2\u01bfv\3\2\2\2\u01c0\u01c1\7u\2"+
		"\2\u01c1\u01c2\7q\2\2\u01c2\u01c3\7o\2\2\u01c3\u01c4\7g\2\2\u01c4x\3\2"+
		"\2\2\u01c5\u01c6\7G\2\2\u01c6z\3\2\2\2\u01c7\u01c8\7\u2205\2\2\u01c8|"+
		"\3\2\2\2\u01c9\u01ca\7\60\2\2\u01ca\u01cb\7\60\2\2\u01cb~\3\2\2\2\u01cc"+
		"\u01cd\7D\2\2\u01cd\u0080\3\2\2\2\u01ce\u01cf\7r\2\2\u01cf\u01d0\7t\2"+
		"\2\u01d0\u01d1\7g\2\2\u01d1\u01d2\7o\2\2\u01d2\u01d3\7k\2\2\u01d3\u01d4"+
		"\7u\2\2\u01d4\u01d5\7g\2\2\u01d5\u0082\3\2\2\2\u01d6\u01d7\7c\2\2\u01d7"+
		"\u01d8\7p\2\2\u01d8\u01d9\7f\2\2\u01d9\u01da\7k\2\2\u01da\u0084\3\2\2"+
		"\2\u01db\u01dc\7c\2\2\u01dc\u01dd\7p\2\2\u01dd\u01de\7f\2\2\u01de\u01df"+
		"\7g\2\2\u01df\u01e0\7\63\2\2\u01e0\u0086\3\2\2\2\u01e1\u01e2\7c\2\2\u01e2"+
		"\u01e3\7p\2\2\u01e3\u01e4\7f\2\2\u01e4\u01e5\7g\2\2\u01e5\u01e6\7\64\2"+
		"\2\u01e6\u0088\3\2\2\2\u01e7\u01e8\7q\2\2\u01e8\u01e9\7t\2\2\u01e9\u01ea"+
		"\7k\2\2\u01ea\u01eb\7\63\2\2\u01eb\u008a\3\2\2\2\u01ec\u01ed\7X\2\2\u01ed"+
		"\u01ee\7k\2\2\u01ee\u01ef\7\63\2\2\u01ef\u008c\3\2\2\2\u01f0\u01f1\7q"+
		"\2\2\u01f1\u01f2\7t\2\2\u01f2\u01f3\7k\2\2\u01f3\u01f4\7\64\2\2\u01f4"+
		"\u008e\3\2\2\2\u01f5\u01f6\7X\2\2\u01f6\u01f7\7k\2\2\u01f7\u01f8\7\64"+
		"\2\2\u01f8\u0090\3\2\2\2\u01f9\u01fa\7q\2\2\u01fa\u01fb\7t\2\2\u01fb\u01fc"+
		"\7g\2\2\u01fc\u0092\3\2\2\2\u01fd\u01fe\7X\2\2\u01fe\u01ff\7g\2\2\u01ff"+
		"\u0094\3\2\2\2\u0200\u0201\7k\2\2\u0201\u0202\7o\2\2\u0202\u0203\7r\2"+
		"\2\u0203\u0204\7n\2\2\u0204\u0205\7k\2\2\u0205\u0206\7g\2\2\u0206\u0207"+
		"\7u\2\2\u0207\u0208\7k\2\2\u0208\u0096\3\2\2\2\u0209\u020a\7k\2\2\u020a"+
		"\u020b\7o\2\2\u020b\u020c\7r\2\2\u020c\u020d\7n\2\2\u020d\u020e\7k\2\2"+
		"\u020e\u020f\7g\2\2\u020f\u0210\7u\2\2\u0210\u0211\7g\2\2\u0211\u0098"+
		"\3\2\2\2\u0212\u0213\7p\2\2\u0213\u0214\7q\2\2\u0214\u0215\7v\2\2\u0215"+
		"\u0216\7k\2\2\u0216\u009a\3\2\2\2\u0217\u0218\7p\2\2\u0218\u0219\7g\2"+
		"\2\u0219\u021a\7i\2\2\u021a\u021b\7k\2\2\u021b\u009c\3\2\2\2\u021c\u021d"+
		"\7p\2\2\u021d\u021e\7q\2\2\u021e\u021f\7v\2\2\u021f\u0220\7g\2\2\u0220"+
		"\u009e\3\2\2\2\u0221\u0222\7p\2\2\u0222\u0223\7g\2\2\u0223\u0224\7i\2"+
		"\2\u0224\u0225\7g\2\2\u0225\u00a0\3\2\2\2\u0226\u0227\7d\2\2\u0227\u0228"+
		"\7q\2\2\u0228\u0229\7v\2\2\u0229\u022a\7v\2\2\u022a\u022b\7q\2\2\u022b"+
		"\u022c\7o\2\2\u022c\u022d\7g\2\2\u022d\u00a2\3\2\2\2\u022e\u022f\7h\2"+
		"\2\u022f\u0230\7c\2\2\u0230\u0231\7n\2\2\u0231\u0232\7u\2\2\u0232\u0233"+
		"\7g\2\2\u0233\u0234\7g\2\2\u0234\u00a4\3\2\2\2\u0235\u0236\7r\2\2\u0236"+
		"\u0237\7d\2\2\u0237\u0238\7e\2\2\u0238\u00a6\3\2\2\2\u0239\u023a\7u\2"+
		"\2\u023a\u023b\7w\2\2\u023b\u023c\7d\2\2\u023c\u023d\7u\2\2\u023d\u023e"+
		"\7v\2\2\u023e\u023f\7\63\2\2\u023f\u00a8\3\2\2\2\u0240\u0241\7u\2\2\u0241"+
		"\u0242\7w\2\2\u0242\u0243\7d\2\2\u0243\u0244\7u\2\2\u0244\u0245\7v\2\2"+
		"\u0245\u0246\7\64\2\2\u0246\u00aa\3\2\2\2\u0247\u0248\7c\2\2\u0248\u0249"+
		"\7n\2\2\u0249\u024a\7i\2\2\u024a\u024b\7g\2\2\u024b\u024c\7d\2\2\u024c"+
		"\u024d\7t\2\2\u024d\u024e\7c\2\2\u024e\u00ac\3\2\2\2\u024f\u0250\7h\2"+
		"\2\u0250\u0251\7q\2\2\u0251\u0252\7t\2\2\u0252\u0253\7c\2\2\u0253\u0254"+
		"\7n\2\2\u0254\u0255\7n\2\2\u0255\u0256\7k\2\2\u0256\u00ae\3\2\2\2\u0257"+
		"\u0258\7c\2\2\u0258\u0259\7n\2\2\u0259\u025a\7n\2\2\u025a\u025b\7k\2\2"+
		"\u025b\u00b0\3\2\2\2\u025c\u025d\7C\2\2\u025d\u025e\7k\2\2\u025e\u00b2"+
		"\3\2\2\2\u025f\u0260\7h\2\2\u0260\u0261\7q\2\2\u0261\u0262\7t\2\2\u0262"+
		"\u0263\7c\2\2\u0263\u0264\7n\2\2\u0264\u0265\7n\2\2\u0265\u0266\7g\2\2"+
		"\u0266\u00b4\3\2\2\2\u0267\u0268\7c\2\2\u0268\u0269\7n\2\2\u0269\u026a"+
		"\7n\2\2\u026a\u026b\7g\2\2\u026b\u00b6\3\2\2\2\u026c\u026d\7C\2\2\u026d"+
		"\u026e\7g\2\2\u026e\u00b8\3\2\2\2\u026f\u0270\7g\2\2\u0270\u0271\7z\2"+
		"\2\u0271\u0272\7k\2\2\u0272\u0273\7u\2\2\u0273\u0274\7v\2\2\u0274\u0275"+
		"\7u\2\2\u0275\u0276\7k\2\2\u0276\u00ba\3\2\2\2\u0277\u0278\7u\2\2\u0278"+
		"\u0279\7q\2\2\u0279\u027a\7o\2\2\u027a\u027b\7g\2\2\u027b\u027c\7k\2\2"+
		"\u027c\u00bc\3\2\2\2\u027d\u027e\7G\2\2\u027e\u027f\7k\2\2\u027f\u00be"+
		"\3\2\2\2\u0280\u0281\7g\2\2\u0281\u0282\7z\2\2\u0282\u0283\7k\2\2\u0283"+
		"\u0284\7u\2\2\u0284\u0285\7v\2\2\u0285\u0286\7u\2\2\u0286\u0287\7g\2\2"+
		"\u0287\u00c0\3\2\2\2\u0288\u0289\7u\2\2\u0289\u028a\7q\2\2\u028a\u028b"+
		"\7o\2\2\u028b\u028c\7g\2\2\u028c\u028d\7g\2\2\u028d\u00c2\3\2\2\2\u028e"+
		"\u028f\7G\2\2\u028f\u0290\7g\2\2\u0290\u00c4\3\2\2\2\u0291\u0292\7k\2"+
		"\2\u0292\u0293\7p\2\2\u0293\u0294\7x\2\2\u0294\u0295\7c\2\2\u0295\u0296"+
		"\7t\2\2\u0296\u0297\7k\2\2\u0297\u0298\7c\2\2\u0298\u0299\7p\2\2\u0299"+
		"\u029a\7v\2\2\u029a\u00c6\3\2\2\2\u029b\u029c\7c\2\2\u029c\u029d\7w\2"+
		"\2\u029d\u029e\7v\2\2\u029e\u029f\7q\2\2\u029f\u00c8\3\2\2\2\u02a0\u02a1"+
		"\7k\2\2\u02a1\u02a2\7o\2\2\u02a2\u02a3\7r\2\2\u02a3\u02a4\7q\2\2\u02a4"+
		"\u02a5\7t\2\2\u02a5\u02a6\7v\2\2\u02a6\u00ca\3\2\2\2\u02a7\u02a8\7a\2"+
		"\2\u02a8\u00cc\3\2\2\2\u02a9\u02aa\7h\2\2\u02aa\u02ab\7c\2\2\u02ab\u02ac"+
		"\7e\2\2\u02ac\u02ad\7v\2\2\u02ad\u00ce\3\2\2\2\u02ae\u02af\7f\2\2\u02af"+
		"\u02b0\7g\2\2\u02b0\u02b1\7h\2\2\u02b1\u00d0\3\2\2\2\u02b2\u02b3\7x\2"+
		"\2\u02b3\u02b4\7c\2\2\u02b4\u02b5\7t\2\2\u02b5\u00d2\3\2\2\2\u02b6\u02b7"+
		"\7x\2\2\u02b7\u02b8\7c\2\2\u02b8\u02b9\7n\2\2\u02b9\u00d4\3\2\2\2\u02ba"+
		"\u02bb\7c\2\2\u02bb\u02bc\7u\2\2\u02bc\u02bd\7u\2\2\u02bd\u02be\7g\2\2"+
		"\u02be\u02bf\7t\2\2\u02bf\u02c0\7v\2\2\u02c0\u00d6\3\2\2\2\u02c1\u02c2"+
		"\7k\2\2\u02c2\u02c3\7h\2\2\u02c3\u00d8\3\2\2\2\u02c4\u02c5\7g\2\2\u02c5"+
		"\u02c6\7n\2\2\u02c6\u02c7\7u\2\2\u02c7\u02c8\7g\2\2\u02c8\u00da\3\2\2"+
		"\2\u02c9\u02ca\7y\2\2\u02ca\u02cb\7j\2\2\u02cb\u02cc\7k\2\2\u02cc\u02cd"+
		"\7n\2\2\u02cd\u02ce\7g\2\2\u02ce\u00dc\3\2\2\2\u02cf\u02d0\7n\2\2\u02d0"+
		"\u02d1\7$\2\2\u02d1\u02d2\7$\2\2\u02d2\u02d3\7$\2\2\u02d3\u00de\3\2\2"+
		"\2\u02d4\u02d5\7$\2\2\u02d5\u02d6\7$\2\2\u02d6\u02d7\7$\2\2\u02d7\u00e0"+
		"\3\2\2\2\u02d8\u02d9\7r\2\2\u02d9\u02da\7t\2\2\u02da\u02db\7k\2\2\u02db"+
		"\u02dc\7p\2\2\u02dc\u02dd\7v\2\2\u02dd\u00e2\3\2\2\2\u02de\u02df\7r\2"+
		"\2\u02df\u02e0\7t\2\2\u02e0\u02e1\7k\2\2\u02e1\u02e2\7p\2\2\u02e2\u02e3"+
		"\7v\2\2\u02e3\u02e4\7n\2\2\u02e4\u02e5\7p\2\2\u02e5\u00e4\3\2\2\2\u02e6"+
		"\u02e7\7B\2\2\u02e7\u00e6\3\2\2\2\u02e8\u02e9\7W\2\2\u02e9\u02ea\7p\2"+
		"\2\u02ea\u02eb\7k\2\2\u02eb\u02ec\7v\2\2\u02ec\u00e8\3\2\2\2\u02ed\u02ee"+
		"\7t\2\2\u02ee\u02ef\7g\2\2\u02ef\u02f0\7v\2\2\u02f0\u02f1\7w\2\2\u02f1"+
		"\u02f2\7t\2\2\u02f2\u02f3\7p\2\2\u02f3\u00ea\3\2\2\2\u02f4\u02f5\7t\2"+
		"\2\u02f5\u02f6\7c\2\2\u02f6\u02f7\7p\2\2\u02f7\u02f8\7f\2\2\u02f8\u02f9"+
		"\7q\2\2\u02f9\u02fa\7o\2\2\u02fa\u02fb\7K\2\2\u02fb\u02fc\7p\2\2\u02fc"+
		"\u02fd\7v\2\2\u02fd\u00ec\3\2\2\2\u02fe\u02ff\7t\2\2\u02ff\u0300\7g\2"+
		"\2\u0300\u0301\7c\2\2\u0301\u0302\7f\2\2\u0302\u0303\7K\2\2\u0303\u0304"+
		"\7p\2\2\u0304\u0305\7v\2\2\u0305\u00ee\3\2\2\2\u0306\u0307\7o\2\2\u0307"+
		"\u0308\7q\2\2\u0308\u0309\7f\2\2\u0309\u030a\7k\2\2\u030a\u030b\7h\2\2"+
		"\u030b\u030c\7k\2\2\u030c\u030d\7g\2\2\u030d\u030e\7u\2\2\u030e\u00f0"+
		"\3\2\2\2\u030f\u0310\7t\2\2\u0310\u0311\7g\2\2\u0311\u0312\7s\2\2\u0312"+
		"\u0313\7w\2\2\u0313\u0314\7k\2\2\u0314\u0315\7t\2\2\u0315\u0316\7g\2\2"+
		"\u0316\u0317\7u\2\2\u0317\u00f2\3\2\2\2\u0318\u0319\7r\2\2\u0319\u031a"+
		"\7t\2\2\u031a\u031b\7g\2\2\u031b\u00f4\3\2\2\2\u031c\u031d\7g\2\2\u031d"+
		"\u031e\7p\2\2\u031e\u031f\7u\2\2\u031f\u0320\7w\2\2\u0320\u0321\7t\2\2"+
		"\u0321\u0322\7g\2\2\u0322\u0323\7u\2\2\u0323\u00f6\3\2\2\2\u0324\u0325"+
		"\7r\2\2\u0325\u0326\7q\2\2\u0326\u0327\7u\2\2\u0327\u0328\7v\2\2\u0328"+
		"\u00f8\3\2\2\2\u0329\u032a\7/\2\2\u032a\u032c\7/\2\2\u032b\u032d\7/\2"+
		"\2\u032c\u032b\3\2\2\2\u032d\u032e\3\2\2\2\u032e\u032c\3\2\2\2\u032e\u032f"+
		"\3\2\2\2\u032f\u00fa\3\2\2\2\u0330\u0339\7\62\2\2\u0331\u0335\t\2\2\2"+
		"\u0332\u0334\t\3\2\2\u0333\u0332\3\2\2\2\u0334\u0337\3\2\2\2\u0335\u0333"+
		"\3\2\2\2\u0335\u0336\3\2\2\2\u0336\u0339\3\2\2\2\u0337\u0335\3\2\2\2\u0338"+
		"\u0330\3\2\2\2\u0338\u0331\3\2\2\2\u0339\u00fc\3\2\2\2\u033a\u033e\t\4"+
		"\2\2\u033b\u033d\t\5\2\2\u033c\u033b\3\2\2\2\u033d\u0340\3\2\2\2\u033e"+
		"\u033c\3\2\2\2\u033e\u033f\3\2\2\2\u033f\u00fe\3\2\2\2\u0340\u033e\3\2"+
		"\2\2\u0341\u0342\7c\2\2\u0342\u0343\7d\2\2\u0343\u0344\7u\2\2\u0344\u0345"+
		"\7v\2\2\u0345\u0346\7t\2\2\u0346\u0347\7c\2\2\u0347\u0348\7e\2\2\u0348"+
		"\u03eb\7v\2\2\u0349\u034a\7e\2\2\u034a\u034b\7c\2\2\u034b\u034c\7u\2\2"+
		"\u034c\u03eb\7g\2\2\u034d\u034e\7e\2\2\u034e\u034f\7c\2\2\u034f\u0350"+
		"\7v\2\2\u0350\u0351\7e\2\2\u0351\u03eb\7j\2\2\u0352\u0353\7e\2\2\u0353"+
		"\u0354\7n\2\2\u0354\u0355\7c\2\2\u0355\u0356\7u\2\2\u0356\u03eb\7u\2\2"+
		"\u0357\u0358\7f\2\2\u0358\u03eb\7q\2\2\u0359\u035a\7g\2\2\u035a\u035b"+
		"\7z\2\2\u035b\u035c\7v\2\2\u035c\u035d\7g\2\2\u035d\u035e\7p\2\2\u035e"+
		"\u035f\7f\2\2\u035f\u03eb\7u\2\2\u0360\u0361\7h\2\2\u0361\u0362\7k\2\2"+
		"\u0362\u0363\7p\2\2\u0363\u0364\7c\2\2\u0364\u03eb\7n\2\2\u0365\u0366"+
		"\7h\2\2\u0366\u0367\7k\2\2\u0367\u0368\7p\2\2\u0368\u0369\7c\2\2\u0369"+
		"\u036a\7n\2\2\u036a\u036b\7n\2\2\u036b\u03eb\7{\2\2\u036c\u036d\7h\2\2"+
		"\u036d\u036e\7q\2\2\u036e\u03eb\7t\2\2\u036f\u0370\7h\2\2\u0370\u0371"+
		"\7q\2\2\u0371\u0372\7t\2\2\u0372\u0373\7U\2\2\u0373\u0374\7q\2\2\u0374"+
		"\u0375\7o\2\2\u0375\u03eb\7g\2\2\u0376\u0377\7k\2\2\u0377\u0378\7o\2\2"+
		"\u0378\u0379\7r\2\2\u0379\u037a\7n\2\2\u037a\u037b\7k\2\2\u037b\u037c"+
		"\7e\2\2\u037c\u037d\7k\2\2\u037d\u03eb\7v\2\2\u037e\u037f\7n\2\2\u037f"+
		"\u0380\7c\2\2\u0380\u0381\7|\2\2\u0381\u03eb\7{\2\2\u0382\u0383\7o\2\2"+
		"\u0383\u0384\7c\2\2\u0384\u0385\7e\2\2\u0385\u0386\7t\2\2\u0386\u03eb"+
		"\7q\2\2\u0387\u0388\7o\2\2\u0388\u0389\7c\2\2\u0389\u038a\7v\2\2\u038a"+
		"\u038b\7e\2\2\u038b\u03eb\7j\2\2\u038c\u038d\7p\2\2\u038d\u038e\7g\2\2"+
		"\u038e\u03eb\7y\2\2\u038f\u0390\7p\2\2\u0390\u0391\7w\2\2\u0391\u0392"+
		"\7n\2\2\u0392\u03eb\7n\2\2\u0393\u0394\7q\2\2\u0394\u0395\7d\2\2\u0395"+
		"\u0396\7l\2\2\u0396\u0397\7g\2\2\u0397\u0398\7e\2\2\u0398\u03eb\7v\2\2"+
		"\u0399\u039a\7q\2\2\u039a\u039b\7x\2\2\u039b\u039c\7g\2\2\u039c\u039d"+
		"\7t\2\2\u039d\u039e\7t\2\2\u039e\u039f\7k\2\2\u039f\u03a0\7f\2\2\u03a0"+
		"\u03eb\7g\2\2\u03a1\u03a2\7r\2\2\u03a2\u03a3\7c\2\2\u03a3\u03a4\7e\2\2"+
		"\u03a4\u03a5\7m\2\2\u03a5\u03a6\7c\2\2\u03a6\u03a7\7i\2\2\u03a7\u03eb"+
		"\7g\2\2\u03a8\u03a9\7r\2\2\u03a9\u03aa\7t\2\2\u03aa\u03ab\7k\2\2\u03ab"+
		"\u03ac\7x\2\2\u03ac\u03ad\7c\2\2\u03ad\u03ae\7v\2\2\u03ae\u03eb\7g\2\2"+
		"\u03af\u03b0\7r\2\2\u03b0\u03b1\7t\2\2\u03b1\u03b2\7q\2\2\u03b2\u03b3"+
		"\7v\2\2\u03b3\u03b4\7g\2\2\u03b4\u03b5\7e\2\2\u03b5\u03b6\7v\2\2\u03b6"+
		"\u03b7\7g\2\2\u03b7\u03eb\7f\2\2\u03b8\u03b9\7u\2\2\u03b9\u03ba\7g\2\2"+
		"\u03ba\u03bb\7c\2\2\u03bb\u03bc\7n\2\2\u03bc\u03bd\7g\2\2\u03bd\u03eb"+
		"\7f\2\2\u03be\u03bf\7u\2\2\u03bf\u03c0\7w\2\2\u03c0\u03c1\7r\2\2\u03c1"+
		"\u03c2\7g\2\2\u03c2\u03eb\7t\2\2\u03c3\u03c4\7v\2\2\u03c4\u03c5\7j\2\2"+
		"\u03c5\u03c6\7k\2\2\u03c6\u03eb\7u\2\2\u03c7\u03c8\7v\2\2\u03c8\u03c9"+
		"\7j\2\2\u03c9\u03ca\7t\2\2\u03ca\u03cb\7q\2\2\u03cb\u03eb\7y\2\2\u03cc"+
		"\u03cd\7v\2\2\u03cd\u03ce\7t\2\2\u03ce\u03cf\7c\2\2\u03cf\u03d0\7k\2\2"+
		"\u03d0\u03eb\7v\2\2\u03d1\u03d2\7v\2\2\u03d2\u03d3\7t\2\2\u03d3\u03eb"+
		"\7{\2\2\u03d4\u03d5\7v\2\2\u03d5\u03d6\7{\2\2\u03d6\u03d7\7r\2\2\u03d7"+
		"\u03eb\7g\2\2\u03d8\u03d9\7y\2\2\u03d9\u03da\7k\2\2\u03da\u03db\7v\2\2"+
		"\u03db\u03eb\7j\2\2\u03dc\u03dd\7{\2\2\u03dd\u03de\7k\2\2\u03de\u03df"+
		"\7g\2\2\u03df\u03e0\7n\2\2\u03e0\u03eb\7f\2\2\u03e1\u03e2\7>\2\2\u03e2"+
		"\u03eb\7/\2\2\u03e3\u03e4\7>\2\2\u03e4\u03eb\7<\2\2\u03e5\u03e6\7>\2\2"+
		"\u03e6\u03eb\7\'\2\2\u03e7\u03e8\7@\2\2\u03e8\u03eb\7<\2\2\u03e9\u03eb"+
		"\t\6\2\2\u03ea\u0341\3\2\2\2\u03ea\u0349\3\2\2\2\u03ea\u034d\3\2\2\2\u03ea"+
		"\u0352\3\2\2\2\u03ea\u0357\3\2\2\2\u03ea\u0359\3\2\2\2\u03ea\u0360\3\2"+
		"\2\2\u03ea\u0365\3\2\2\2\u03ea\u036c\3\2\2\2\u03ea\u036f\3\2\2\2\u03ea"+
		"\u0376\3\2\2\2\u03ea\u037e\3\2\2\2\u03ea\u0382\3\2\2\2\u03ea\u0387\3\2"+
		"\2\2\u03ea\u038c\3\2\2\2\u03ea\u038f\3\2\2\2\u03ea\u0393\3\2\2\2\u03ea"+
		"\u0399\3\2\2\2\u03ea\u03a1\3\2\2\2\u03ea\u03a8\3\2\2\2\u03ea\u03af\3\2"+
		"\2\2\u03ea\u03b8\3\2\2\2\u03ea\u03be\3\2\2\2\u03ea\u03c3\3\2\2\2\u03ea"+
		"\u03c7\3\2\2\2\u03ea\u03cc\3\2\2\2\u03ea\u03d1\3\2\2\2\u03ea\u03d4\3\2"+
		"\2\2\u03ea\u03d8\3\2\2\2\u03ea\u03dc\3\2\2\2\u03ea\u03e1\3\2\2\2\u03ea"+
		"\u03e3\3\2\2\2\u03ea\u03e5\3\2\2\2\u03ea\u03e7\3\2\2\2\u03ea\u03e9\3\2"+
		"\2\2\u03eb\u0100\3\2\2\2\u03ec\u03ee\7\17\2\2\u03ed\u03ec\3\2\2\2\u03ed"+
		"\u03ee\3\2\2\2\u03ee\u03ef\3\2\2\2\u03ef\u03f0\7\f\2\2\u03f0\u0102\3\2"+
		"\2\2\u03f1\u03f2\7\61\2\2\u03f2\u03f3\7\61\2\2\u03f3\u03f7\3\2\2\2\u03f4"+
		"\u03f6\n\7\2\2\u03f5\u03f4\3\2\2\2\u03f6\u03f9\3\2\2\2\u03f7\u03f5\3\2"+
		"\2\2\u03f7\u03f8\3\2\2\2\u03f8\u03fa\3\2\2\2\u03f9\u03f7\3\2\2\2\u03fa"+
		"\u03fb\b\u0082\2\2\u03fb\u0104\3\2\2\2\u03fc\u03fd\7\61\2\2\u03fd\u03fe"+
		"\7,\2\2\u03fe\u0402\3\2\2\2\u03ff\u0401\13\2\2\2\u0400\u03ff\3\2\2\2\u0401"+
		"\u0404\3\2\2\2\u0402\u0403\3\2\2\2\u0402\u0400\3\2\2\2\u0403\u0405\3\2"+
		"\2\2\u0404\u0402\3\2\2\2\u0405\u0406\7,\2\2\u0406\u0407\7\61\2\2\u0407"+
		"\u0408\3\2\2\2\u0408\u0409\b\u0083\2\2\u0409\u0106\3\2\2\2\u040a\u040c"+
		"\t\b\2\2\u040b\u040a\3\2\2\2\u040c\u040d\3\2\2\2\u040d\u040b\3\2\2\2\u040d"+
		"\u040e\3\2\2\2\u040e\u040f\3\2\2\2\u040f\u0410\b\u0084\3\2\u0410\u0108"+
		"\3\2\2\2\u0411\u0413\7$\2\2\u0412\u0414\5\u010b\u0086\2\u0413\u0412\3"+
		"\2\2\2\u0413\u0414\3\2\2\2\u0414\u0415\3\2\2\2\u0415\u0416\7$\2\2\u0416"+
		"\u010a\3\2\2\2\u0417\u0419\5\u010d\u0087\2\u0418\u0417\3\2\2\2\u0419\u041a"+
		"\3\2\2\2\u041a\u0418\3\2\2\2\u041a\u041b\3\2\2\2\u041b\u010c\3\2\2\2\u041c"+
		"\u041f\n\t\2\2\u041d\u041f\5\u010f\u0088\2\u041e\u041c\3\2\2\2\u041e\u041d"+
		"\3\2\2\2\u041f\u010e\3\2\2\2\u0420\u0421\7^\2\2\u0421\u0424\t\n\2\2\u0422"+
		"\u0424\5\u0111\u0089\2\u0423\u0420\3\2\2\2\u0423\u0422\3\2\2\2\u0424\u0110"+
		"\3\2\2\2\u0425\u0426\7^\2\2\u0426\u0427\7w\2\2\u0427\u0428\5\u0113\u008a"+
		"\2\u0428\u0429\5\u0113\u008a\2\u0429\u042a\5\u0113\u008a\2\u042a\u042b"+
		"\5\u0113\u008a\2\u042b\u0112\3\2\2\2\u042c\u042d\t\13\2\2\u042d\u0114"+
		"\3\2\2\2\u042e\u042f\13\2\2\2\u042f\u0116\3\2\2\2\20\2\u032e\u0335\u0338"+
		"\u033e\u03ea\u03ed\u03f7\u0402\u040d\u0413\u041a\u041e\u0423\4\2\3\2\b"+
		"\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}