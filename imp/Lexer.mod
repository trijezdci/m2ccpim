(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Lexer;

(* Lexer for Modula-2 R10 Core Compiler *)

IMPORT ASCII, Capabilities, Source, Token, Symbol;

FROM Source IMPORT SourceT;
FROM Token IMPORT TokenT;
FROM Symbol IMPORT SymbolT;


(* Lexer Type *)

TYPE Lexer = POINTER TO LexerDescriptor;

TYPE LexerDescriptor = RECORD
  source     : SourceT;
  nextSymbol : SymbolT;
  warnings,
  errors     : CARDINAL;
  lastStatus : Status
END; (* LexerDescriptor *)


(* Operations *)

(* ---------------------------------------------------------------------------
 * procedure New ( newLexer, filename, status )
 *  creates a new lexer instance, associated with filename
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE New ( VAR newLexer : Lexer; filename : Filename; VAR s : Status );

VAR
  source : SourceT;
  sourceStatus : Source.Status;

BEGIN
 
  (* lexer must not have been initialised *)
  IF newLexer # NIL THEN
    status := Status.AlreadyInitialised;
    RETURN
  END;
  
  (* allocate and initialise source *)
  Source.New(source, filename, sourceStatus);
  IF sourceStatus # Source.Status.Success THEN
    s := Status.UnableToAllocate;
    RETURN
  END;
  
  (* allocate a lexer instance *)
  NEW newLexer;
  IF newLexer = NIL THEN
    s := Status.UnableToAllocate;
    RELEASE source;
    RETURN
  END;
  
  (* initialise lexer *)
  newLexer^.source := source;
  newLexer^.warnings := 0;
  newLexer^.errors := 0;
  newLexer^.lastStatus := Status.Success;
  
  (* read the first symbol to be returned *)
  newLexer^.nextSymbol := Lexer.consumeSym(newLexer);
  
  s := Status.Success;
  RETURN
END New;


(* ---------------------------------------------------------------------------
 * procedure GetSym ( lexer, symbol, lookaheadSymbol )
 *  passes and consumes current lookahead symbol, passes new lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE GetSym ( lexer : Lexer; VAR sym, next : SymbolT );

BEGIN
  
  (* nextSymbol holds current lookahead, pass it back in sym *)
  sym := self^.nextSymbol;
  
  (* consume the current lookahead,
     read the new lookahead symbol, pass it back in next *)
  next := Lexer.consumeSym(lexer);
  
  RETURN
END GetSym;


(* ---------------------------------------------------------------------------
 * procedure consumeSym ( lexer )
 *  consumes current lookahead symbol and returns new lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE consumeSym ( lexer : Lexer ) : SymbolT;

VAR
  ch, next, la2 : CHAR;
  source : Source;
  sym : Symbol;

BEGIN
  (* ensure source is valid *)
  IF lexer = NIL THEN
  (* TO DO: report and handle error *)
    RETURN
  END;
  
  source := lexer^.source;
  
  (* all decisions are based on lookahead *)
  next := source.lookaheadChar();
  
  (* skip any whitespace, tab and new line *)
  WHILE NOT Source.eof(source) AND
    (next = ASCII.SPACE OR next = ASCII.TAB OR next = ASCII.NEWLINE) DO
    Source.GetChar(source, ch, next)
  END; (* WHILE *)
  
  (* get current position *)
  Source.GetLineAndColumn(source, sym.line, sym.column);

  (* check for end-of-file *)
  IF Source.eof(source) THEN
    sym.token := TokenT.EOF;
    sym.lexeme := 0
  
  (* check for reserved word or identifier *)
  ELSIF ASCII.isLetter(next) THEN
    Source.MarkLexeme(source, sym.line, sym.column);
    MatchResWordOrIdent(source, sym.token);
    Source.CopyLexeme(source, lexer^.dict, sym.lexeme)

  (* check for numeric literal *)
  ELSIF next >= "0" AND next <= "9" THEN 
    Source.MarkLexeme(source, sym.line, sym.column);
    MatchNumericLiteral(source, sym.token);
    Source.CopyLexeme(source, lexer^.dict, sym.lexeme)

  (* check for quoted literal *)
  ELSIF next = ASCII.SINGLEQUOTE OR next = ASCII.DOUBLEQUOTE THEN
    Source.MarkLexeme(source, sym.line, sym.column);
    MatchQuotedLiteral(source, sym.token);
    Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
    
  (* check for optional OpenVMS identifier starting with "$" *)
  ELSIF next = "$" AND
        Capabilities.isEnabled(Capabilities.OpenVMSIdentifiers) THEN
    Source.MarkLexeme(source, sym.line, sym.column);
    MatchOpenVMSIdent(source, sym.token);
    Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
    
  (* check for any other symbol *)
  ELSE
    CASE next OF
    
    (* next symbol is line comment *)
    | "!" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLineComment(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
    
    (* next symbol is "#" *)
    | "#" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.NotEqual;
        sym.lexeme := Token.lexemeForToken(TokenT.NotEqual)
    
    (* next symbol is "&" *)
    | "&" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Concat;
        sym.lexeme := Token.lexemeForToken(TokenT.Concat)
    
    (* next symbol is "(" or block comment *)
    | "(" :
        IF Source.la2Char(source) = "*" THEN (* found block comment *)
          Source.MarkLexeme(source, sym.line, sym.column);
          MatchBlockComment(source, sym.token);
          Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
        
        ELSE (* found "(" *)
          Source.ConsumeChar(source);
          Source.GetLineAndColumn(source, sym.line, sym.column);
          sym.token := TokenT.LParen;
          sym.lexeme := Token.lexemeForToken(TokenT.LParen)
          
        END (* "(" and block comment *)
    
    (* next symbol is ")" *)
    | ")" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.value := TokenT.RParen;
        sym.lexeme := Token.lexemeForToken(TokenT.RParen)
    
    (* next symbol is "*" or "**" *)
    | "*" :
        Source.GetChar(source, ch, next);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next # "*" THEN (* found sole "*" *)
          sym.token := TokenT.Asterisk;
          sym.lexeme := Token.lexemeForToken(TokenT.Asterisk)
        
        ELSE (* found "**" *)
          Source.ConsumeChar(source);
          sym.token := TokenT.Power;
          sym.lexeme := Token.lexemeForToken(TokenT.Power)
        
        END (* "*" or "**" *)
    
    (* next symbol is "+" or "++" *)
    | "+" :
        Source.GetChar(source, ch, next);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next # "+" THEN (* found sole "+" *)
          sym.token := TokenT.Plus;
          sym.lexeme := Token.lexemeForToken(TokenT.Plus)
        
        ELSE (* found "++" *)
          Source.ConsumeChar(source);
          sym.token := TokenT.PlusPlus;
          sym.lexeme := Token.lexemeForToken(TokenT.PlusPlus)
        
        END (* "+" and "++" *)
      
    (* next symbol is "," *)
    | "," :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Comma;
        sym.lexeme := Token.lexemeForToken(TokenT.Comma)
    
    (* next symbol is "-", "--" or "->" *)
    | "-" :
        Source.GetChar(source, ch, next);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next = "-" THEN (* found "--" *)
          Source.ConsumeChar(source);
          sym.token := TokenT.MinusMinus;
          sym.lexeme := Token.lexemeForToken(TokenT.MinusMinus)
        
        ELSIF next = ">" THEN (* found "->" *)
          Source.ConsumeChar(source);
          sym.token := TokenT.OneWayDep;
          sym.lexeme := Token.lexemeForToken(TokenT.OneWayDep)
        
        ELSE (* found sole "-" *)
          sym.token := TokenT.Minus;
          sym.lexeme := Token.lexemeForToken(TokenT.Minus)
        
        END (* "-", "--" or "->" *)
    
    (* next symbol is ".", ".." or ".*" *)
    | "." :
        Source.GetChar(source, ch, next);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next = "." THEN (* found ".." *)
          Source.ConsumeChar(source);
          sym.token := TokenT.DotDot;
          sym.lexeme := Token.lexemeForToken(TokenT.DotDot)
        
        ELSIF next = "*" THEN (* found ".*" *)
          Source.ConsumeChar(source);
          sym.token := TokenT.DotStar;
          sym.lexeme := Token.lexemeForToken(TokenT.DotStar)
        
        ELSE (* found sole "." *)
          sym.token := TokenT.Dot;
          sym.lexeme := Token.lexemeForToken(TokenT.Dot)
        
        END (* ".", ".." and ".*" *)
      
    (* next symbol is "/" *)
    | "/" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.RealDiv;
        sym.lexeme := Token.lexemeForToken(TokenT.RealDiv)
    
    (* next symbol is ":", ":=" or "::" *)
    | ":" :
        Source.GetChar(source, ch, next);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next = "=" THEN (* found ":=" *)
          Source.ConsumeChar(source);
          sym.token := TokenT.Assign;
          sym.lexeme := Token.lexemeForToken(TokenT.Assign)
        
        ELSIF next = ":" THEN (* found "::" *)
          Source.ConsumeChar(source);
          sym.token := TokenT.TypeConv;
          sym.lexeme := Token.lexemeForToken(TokenT.TypeConv)
        
        ELSE (* found sole ":" *)
          sym.token := TokenT.Colon;
          sym.lexeme := Token.lexemeForToken(TokenT.Colon)
        
        END (* ":", ":=" and "::" *)
    
    (* next symbol is ";" *)
    | ";" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Semicolon;
        sym.lexeme := Token.lexemeForToken(TokenT.Semicolon)
    
    (* next symbol is "<", "<=", "<>", chevron text or pragma *)
    | "<" :
        la2 := Source.la2Char(source);
        
        IF la2 = "<" THEN (* found "<<" *)
          Source.MarkLexeme(source, sym.line, sym.column);
          MatchChevronText(source, sym.token);
          Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
        
        ELSIF la2 = "*" THEN (* found "<*" *)
          Source.MarkLexeme(source, sym.line, sym.column);
          MatchPragma(source, sym.token);
          Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
          
        ELSE (* "<", "<=" or "<> "*)
          Source.GetChar(source, ch, next);
          Source.GetLineAndColumn(source, sym.line, sym.column);
                  
          IF next = "=" THEN (* found "<=" *)
            Source.ConsumeChar(source);
            sym.token := TokenT.LessEq;
            sym.lexeme := Token.lexemeForToken(TokenT.LessEq)
            
          ELSIF next = ">" THEN (* found "<>" *)
            sym.token := TokenT.MutualDep;
            sym.lexeme := Token.lexemeForToken(TokenT.MutualDep)
            
          ELSE (* found "<" *)
            sym.token := TokenT.Less;
            sym.lexeme := Token.lexemeForToken(TokenT.Less)
          
          END (* "<", "<=" or "<>" *)
          
        END (* chevron text or pragma *)
    
    (* next symbol is "=" or "==" *)
    | "=" :
        Source.GetChar(source, ch, next);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next # "=" THEN (* found "=" *)
          sym.token := TokenT.Equal;
          sym.lexeme := Token.lexemeForToken(TokenT.Equal)
        
        ELSE (* found "==" *)
          Source.ConsumeChar(source);
          sym.token := TokenT.Identity;
          sym.lexeme := Token.lexemeForToken(TokenT.Identity)
        
        END (* "=" or "==" *)
    
    (* next symbol is ">", ">=" or "><" *)
    | ">" :
        Source.GetChar(source, ch, next);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next = "=" THEN (* found ">=" *)
          Source.ConsumeChar(source);
          sym.token := TokenT.GreaterEq;
          sym.lexeme := Token.lexemeForToken(TokenT.GreaterEq)
        
        ELSIF next = "<" THEN (* found "><" *)
          Source.ConsumeChar(source);
          sym.token := TokenT.MutualExcl;
          sym.lexeme := Token.lexemeForToken(TokenT.MutualExcl)
          
        ELSE (* found sole ">" *)
          sym.token := TokenT.Greater;
          sym.lexeme := Token.lexemeForToken(TokenT.Greater)
        
        END (* ">", ">=" or "><" *)
    
    (* next symbol is "[" *)
    | "[" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.LBracket;
        sym.lexeme := Token.lexemeForToken(TokenT.LBracket)
    
    (* next symbol is backslash *)
    | ASCII.BACKSLASH :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.SetDiff;
        sym.lexeme := Token.lexemeForToken(TokenT.SetDiff)
    
    (* next symbol is "]" *)
    | "]" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.RBracket;
        sym.lexeme := Token.lexemeForToken(TokenT.RBracket)
    
    (* next symbol is "^" *)
    | "^" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Deref;
        sym.lexeme := Token.lexemeForToken(TokenT.Deref)
    
    (* next symbol is "{" *)
    | "{" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.LBrace;
        sym.lexeme := Token.lexemeForToken(TokenT.LBrace)
    
    (* next symbol is "|" *)
    | "|" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.VerticalBar;
        sym.lexeme := Token.lexemeForToken(TokenT.VerticalBar)
    
    (* next symbol is "}" *)
    | "}" :
        Source.ConsumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.RBrace;
        sym.lexeme := Token.lexemeForToken(TokenT.RBrace)
    
    (* next symbol is invalid *)
    ELSE
      Source.MarkLexeme(source, sym.line, sym.column);
      Source.ConsumeChar(source);
      sym.token := TokenT.Invalid;
      Source.CopyLexeme(source, lexer^.dict, sym.lexeme);
      lexer^.errors++
      
    END; (* CASE *)
  
  END (* IF *);
  
  (* store symbol for use by lookaheadSym *)
  lexer^.nextSymbol := sym;
  
  RETURN
END consumeSym;


(* ---------------------------------------------------------------------------
 * procedure lookaheadSym ( lexer ) : M2Symbol
 *  returns current lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE lookaheadSym ( lexer : Lexer ) : M2Symbol;

BEGIN
  RETURN lexer^.nextSymbol
END lookaheadSym;


(* ---------------------------------------------------------------------------
 * procedure GetStatus ( lexer, status )
 *  returns status of last operation
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE status ( lexer : Lexer ) : Status;

BEGIN

  IF lexer = NIL THEN
    RETURN Status.NotInitialised
  ELSE
    RETURN lexer^.lastStatus
  END

END status;


(* ---------------------------------------------------------------------------
 * procedure warnCount ( lexer ) : CARDINAL
 *  returns current lexical warning count
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE warnCount ( lexer : Lexer ) : CARDINAL;
 (* Returns the lexer's accumulated warning count. *)

BEGIN
  RETURN lexer^.warnings
END warnCount;


(* ---------------------------------------------------------------------------
 * procedure errorCount ( lexer ) : CARDINAL
 *  returns current lexical error count
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE errorCount ( lexer : Lexer ) : CARDINAL;
 (* Returns the lexer's accumulated error count. *)

BEGIN
  RETURN lexer^.errors
END errorCount;


(* ---------------------------------------------------------------------------
 * procedure release ( lexer )
 *  releases lexer instance
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) lexer must not be NIL
 *
 * post-conditions:
 *  (1) lexer is deallocated
 *  (2) NIL is passed back in lexer
 *
 * error-conditions:
 *  (1) reference to lexer remains unmodified
 * ---------------------------------------------------------------------------
 *)
PROCEDURE Release ( VAR lexer : Lexer );
  
BEGIN

  (* lexer must not be NIL *)
  IF lexer = NIL THEN
    RETURN
  END;
  
  (* release source and lexer *)
  Source.Release(lexer^.source);
  RELEASE lexer
  
END Release;


(* Private Operations *)

(* ---------------------------------------------------------------------------
 * procedure matchResWordOrIdent ( s, t, diag )
 *  matches the input in s to a reserved word or identifier
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the RW or identifier.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the RW or identifier whose first character was the
 *      lookahead of s upon entry into the procedure.
 *  (2) if the input represents a reserved word or dual-use identifier,
 *       its token value is passed back in token.
 *      if the input represents any other identifier,
 *       token value identifier is passed back in token.
 *
 * error-conditions:
 *  (1) identifier consists entirely of non-alphanumeric characters
 *       TO DO
 *  (2) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE MatchResWordOrIdent
  ( source : Source; VAR token : TokenT; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  allChars, upperChars, nonStdChars : CARDINAL;
  
BEGIN
  
  allChars := 0;
  upperChars := 0;
  nonStdChars := 0;
  
  REPEAT
    Source.GetChar(source, ch, next);
    allChars++;
    
    IF (ch >= "A") AND (ch <= "Z") THEN
      upperChars++
    END;
    
    IF (ch = "_") OR (ch = "$") THEN
      nonStdChars++
    END
    
  UNTIL Source.eof(source) OR NOT ASCII.isIdentChar(next);
  
  IF allChars = upperChars THEN (* possibly reserved word found *)
    (* TO DO : check for reserved word *)
    
  ELSIF allChars = nonStdChars THEN (* illegal identifier found *)
    LexDiag.New(diag, illegalIdent, 0, 0, "")
    
  END
  
END MatchResWordOrIdent;


(* ---------------------------------------------------------------------------
 * procedure matchNumericLiteral ( s, t, diag )
 *  matches the input in s to numeric literal syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first digit of the literal.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last digit
 *      of the literal whose first digit was the lookahead of s upon entry
 *      into the procedure.
 *  (2) if the numeric literal represents a whole number,
 *       token value wholeNumber is passed back in token.
 *      if the numeric literal represents a character code,
 *       token value quotedChar is passed back in token.
 *      if the numeric literal represents a real number,
 *       token value realNumber is passed back in token.
 *
 * error-conditions:
 *  (1) missing digit after prefix
 *       TO DO
 *  (2) missing fractional part after decimal point
 *       TO DO
 *  (3) missing exponent part after exponent prefix
 *       TO DO
 *  (4) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE MatchNumericLiteral
  ( source : Source; VAR token : TokenT; VAR diag : Diagnostic );

BEGIN
  
  source.GetChar(ch, next);
  IF ch = "0" THEN
          
    CASE next OF
    | "'" : (* decimal number *)
    | "." : (* real number or range *)
    | "b" : (* base-2 whole number *)
    | "u" : (* base-16 character code *)
    | "x" : (* base-16 whole number *)
    
    ELSE (* single digit zero *)
      (* TO DO *)
    
    END; (* CASE *)
      
  ELSE
  
  END
  (* TO DO *)

END MatchNumericLiteral;


(* ---------------------------------------------------------------------------
 * procedure matchQuotedLiteral ( s, t, diag )
 *  matches the input in s to quoted literal syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening quotation mark of the literal.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the closing
 *      quotation mark that closes the literal whose opening quotation mark
 *      was the lookahead of s upon entry into the procedure.
 *  (2) if the quoted literal represents the empty string or a single
 *      character, token value quotedChar is passed back in token.
 *      Otherwise, token value quotedString is passed back in token.
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) unescaped backslash encountered
 *       TO DO
 *  (4) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE MatchQuotedLiteral
  ( source : Source; VAR token : TokenT; VAR diag : Diagnostic );

VAR
  delimiter, ch, next : CHAR;
  len : CARDINAL;

BEGIN

  (* TO DO : update, following change to M2Source *)
  
  len := 0;
  source.GetChar(delimiter, next);
  
  WHILE NOT Source.eof(source) AND
        next # delimiter AND
        ASCII.isPrintable(next) DO
    
    IF next # ASCII.BACKSLASH THEN
      Source.GetChar(source, ch, next);
      len++
      
    ELSE (* backslash *)
      MatchEscapeSequence(source, success);
      
      IF NOT success THEN (* unescaped backslash found *)
        (* TO DO : handle error *)
        
      END
    END
  END; (* WHILE *)
  
  IF next = delimiter THEN
    Source.ConsumeChar(source);
    len++
    
  ELSE (* illegal character in string literal found *)
    (* TO DO : handle error *)
    
  END;
  
  IF len <= 1 THEN
    token := TokenT.QuotedChar
    
  ELSE (* len > 1 *)
    token := TokenT.QuotedString
    
  END
  
END MatchQuotedLiteral;


(* ---------------------------------------------------------------------------
 * procedure MatchLineComment ( s, t, diag )
 *  matches the input in s to line comment syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening exclamation point of a line comment.
 *
 * post-conditions:
 *  (1) if the comment is terminated by end-of-line:
 *       lookahead of s is the new-line character that closes the line comment
 *       whose opening exclamation point was the lookahead of s upon entry
 *       into the procedure, or
 *      if the comment is terminated by end-of-file:
 *       the last character in input s has been consumed.
 *  (2) token value lineComment is passed back in token
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 *  (2) maximum comment length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchLineComment
  ( source : Source; VAR token : TokenT; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  
BEGIN

  REPEAT
    Source.GetChar(source, ch, next)
  UNTIL Source.eof(source) OR (next = ASCII.NEWLINE);
  
  token := TokenT.LineComment

END MatchLineComment;


(* ---------------------------------------------------------------------------
 * procedure MatchBlockComment ( s, t, diag )
 *  matches the input in s to block comment syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening parenthesis of a block comment.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the closing
 *      parenthesis that closes the block comment whose opening parenthesis
 *      was the lookahead of s upon entry into the procedure.
 *  (2) token value blockComment is passed back in token
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum comment length exceeded
 *       TO DO
 *  (4) maximum nesting level exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE MatchBlockComment
  ( source : Source; VAR token : TokenT; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  nestLevel : CARDINAL;
  
BEGIN
  
  nestLevel := 1;
  
  WHILE NOT Source.eof(source) AND (nestLevel > 0) DO
    Source.GetChar(source, ch, next);
    
    IF (ch = "*") AND (next = ")") THEN
      Source.ConsumeChar(source);
      nestLevel--
    
    ELSIF (ch = "(") AND (next = "*") THEN
      Source.ConsumeChar(source);
      nestLevel++
      
    END;
    
    Source.ConsumeChar(source)
    
  END; (* WHILE *)
  
  (* TO DO *)

END MatchBlockComment;


(* ---------------------------------------------------------------------------
 * procedure MatchChevronText ( s, t, diag )
 *  matches the input in s to chevron text syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the opening chevron.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the closing chevron that closes the chevron text whose
 *      opening delimiter was the lookahead of s upon entry into the procedure.
 *  (2) token value chevronText is passed back in token
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE MatchChevronText
  ( source : Source; VAR token : TokenT; VAR diag : Diagnostic );

BEGIN

  (* TO DO *)

END MatchChevronText;


(* ---------------------------------------------------------------------------
 * procedure MatchPragma ( s, t, diag )
 *  matches the input in s to "<*" any legal characters "*>"
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the opening pragma delimiter.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the closing delimiter that closes the pragma whose
 *      opening delimiter was the lookahead of s upon entry into the procedure.
 *  (2) token value pragma is passed back in token
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE MatchPragma
  ( source : Source; VAR token : TokenT; VAR diag : Diagnostic );

BEGIN

  (* TO DO *)

END MatchPragma;


END Lexer.