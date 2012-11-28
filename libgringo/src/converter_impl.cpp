/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is included that follows the "include" declaration
** in the input grammar file. */
#include <stdio.h>
#line 18 "converter_impl.y"

#include "gringo/gringo.h"
#include "converter_impl.h"
#include "gringo/converter.h"
#include "gringo/storage.h"

#define ONE   Val::create(Val::NUM, 1)
#define UNDEF Val::create()
#define PRIO  Val::create(Val::NUM, pConverter->level())

struct Aggr
{
	GroundProgramBuilder::Type type;
	uint32_t n;
	static void create(Aggr &a, GroundProgramBuilder::Type t, uint32_t n)
	{
		a.type = t;
		a.n    = n;
	}
};

#line 30 "converter_impl.c"
/* Next is all token values, in a form suitable for use by makeheaders.
** This section will be null unless lemon is run with the -m switch.
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
/* Make sure the INTERFACE macro is defined.
*/
#ifndef INTERFACE
# define INTERFACE 1
#endif
/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 terminals
**                       and nonterminals.  "int" is used otherwise.
**    YYNOCODE           is a number of type YYCODETYPE which corresponds
**                       to no legal terminal or nonterminal number.  This
**                       number is used to fill in empty slots of the hash 
**                       table.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       have fall-back values which should be used if the
**                       original value of the token will not parse.
**    YYACTIONTYPE       is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 rules and
**                       states combined.  "int" is used otherwise.
**    converterTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is converterTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    converterARG_SDECL     A static variable declaration for the %extra_argument
**    converterARG_PDECL     A parameter declaration for the %extra_argument
**    converterARG_STORE     Code to store %extra_argument into yypParser
**    converterARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
#define YYCODETYPE unsigned char
#define YYNOCODE 82
#define YYACTIONTYPE unsigned short int
#define converterTOKENTYPE  Converter::Token 
typedef union {
  int yyinit;
  converterTOKENTYPE yy0;
  uint32_t yy51;
  Aggr yy158;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 0
#endif
#define converterARG_SDECL  Converter *pConverter ;
#define converterARG_PDECL , Converter *pConverter 
#define converterARG_FETCH  Converter *pConverter  = yypParser->pConverter 
#define converterARG_STORE yypParser->pConverter  = pConverter 
#define YYNSTATE 206
#define YYNRULE 114
#define YY_NO_ACTION      (YYNSTATE+YYNRULE+2)
#define YY_ACCEPT_ACTION  (YYNSTATE+YYNRULE+1)
#define YY_ERROR_ACTION   (YYNSTATE+YYNRULE)

/* The yyzerominor constant is used to initialize instances of
** YYMINORTYPE objects to zero. */
static const YYMINORTYPE yyzerominor = { 0 };

/* Define the yytestcase() macro to be a no-op if is not already defined
** otherwise.
**
** Applications can choose to define yytestcase() in the %include section
** to a macro that can assist in verifying code coverage.  For production
** code the yytestcase() macro should be turned off.  But it is useful
** for testing.
*/
#ifndef yytestcase
# define yytestcase(X)
#endif


/* Next are the tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == YYNSTATE+YYNRULE              A syntax error has occurred.
**
**   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
**
**   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as
**
**      yy_action[ yy_shift_ofst[S] + X ]
**
** If the index value yy_shift_ofst[S]+X is out of range or if the value
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
** is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
** and that yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
**
**  yy_action[]        A single table containing all actions.
**  yy_lookahead[]     A table containing the lookahead for each entry in
**                     yy_action.  Used to detect hash collisions.
**  yy_shift_ofst[]    For each state, the offset into yy_action for
**                     shifting terminals.
**  yy_reduce_ofst[]   For each state, the offset into yy_action for
**                     shifting non-terminals after a reduce.
**  yy_default[]       Default action for each state.
*/
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   116,  125,   81,   59,    7,   19,  126,  123,  129,  130,
 /*    10 */   148,   90,   79,  121,   18,  128,  115,  146,  147,   61,
 /*    20 */    89,  186,  187,  321,    1,  134,   83,  131,  132,  133,
 /*    30 */    51,   52,  128,  151,   39,    6,   61,  123,  183,  122,
 /*    40 */   149,  182,  134,   83,  131,  132,  133,   51,   52,  184,
 /*    50 */    90,   39,  125,   94,  128,  151,   44,  142,   61,  145,
 /*    60 */   183,   48,  150,  182,  134,   83,  131,  132,  133,   51,
 /*    70 */    52,  128,  151,   39,   58,   61,   49,  183,   12,  149,
 /*    80 */   182,  134,   83,  131,  132,  133,   51,   52,  248,  248,
 /*    90 */    39,  206,  151,   41,   42,   93,   43,  125,   67,    3,
 /*   100 */    96,  192,  126,  123,  129,  130,  239,   80,  155,  124,
 /*   110 */    92,   21,   62,   99,   30,   50,  103,  105,  109,   70,
 /*   120 */    71,   72,   75,   78,  128,  152,   81,  194,   61,  181,
 /*   130 */   153,  123,  129,  130,  134,   83,  131,  132,  133,   51,
 /*   140 */    52,  128,   87,   39,  143,   82,  128,   62,  125,   86,
 /*   150 */   120,  134,   83,  131,  132,  133,  136,  171,  156,   64,
 /*   160 */   172,  128,   88,  152,  144,   82,   93,   62,  125,   94,
 /*   170 */    53,  134,   83,  131,  132,  133,  136,  177,   63,  128,
 /*   180 */   178,  185,   81,   82,   93,  100,  101,  123,  128,  134,
 /*   190 */    83,  131,  132,  133,  136,  137,  151,   84,  128,   93,
 /*   200 */    57,  127,   82,    9,  159,  100,  102,  221,  134,   83,
 /*   210 */   131,  132,  133,  136,  137,   66,  151,  128,    8,   93,
 /*   220 */    25,   82,  188,   31,  159,   93,  128,  134,   83,  131,
 /*   230 */   132,  133,  136,  137,  151,  128,   68,   93,  191,   82,
 /*   240 */   128,   10,  160,  100,  112,  134,   83,  131,  132,  133,
 /*   250 */   136,  137,  193,   91,  151,  151,  128,   93,   93,  127,
 /*   260 */    82,   13,  159,   56,  105,  109,  134,   83,  131,  132,
 /*   270 */   133,  136,  137,  231,  190,  195,  251,  251,  125,   67,
 /*   280 */   222,  135,    5,  126,  123,  129,  130,  239,  128,   14,
 /*   290 */    14,   11,   82,   65,   38,   54,  138,  141,  134,   83,
 /*   300 */   131,  132,  133,  136,   85,  128,   14,   14,  198,   82,
 /*   310 */    55,  100,  117,  140,   69,  134,   83,  131,  132,  133,
 /*   320 */   136,  139,  151,   26,  128,   93,   32,    2,   82,   16,
 /*   330 */   159,    4,   27,   33,  134,   83,  131,  132,  133,  136,
 /*   340 */   168,  125,   67,   11,  189,    5,  126,  123,  129,  130,
 /*   350 */   239,   20,  125,   67,  197,   73,    8,  126,  123,  129,
 /*   360 */   130,  239,   95,   97,  125,   81,  118,  197,   76,  126,
 /*   370 */   123,  129,  130,  239,  151,  151,   34,   93,   93,   91,
 /*   380 */    36,   17,   56,   96,   28,   38,  157,  151,  161,   45,
 /*   390 */    93,  154,   29,  196,  195,   56,  128,   92,   21,  203,
 /*   400 */    99,   30,  158,  103,  128,   22,  196,  195,  131,  132,
 /*   410 */   133,  164,   40,   95,   98,  165,  131,  132,  133,  179,
 /*   420 */    95,  104,  162,  180,  163,   23,  151,  151,   37,   93,
 /*   430 */    93,   15,  169,  151,   96,  107,   93,  106,  108,  106,
 /*   440 */   110,   96,  154,   24,  167,   33,  170,   35,  173,  154,
 /*   450 */   151,  175,  151,   93,  151,   93,  202,   93,   60,   46,
 /*   460 */   107,  174,  107,   95,  111,   95,  113,  201,   74,  166,
 /*   470 */   199,  166,  176,   47,  204,  205,  151,  322,  151,   93,
 /*   480 */   322,   93,  201,   77,   96,  151,   96,  322,   93,  322,
 /*   490 */   100,  114,  154,   60,  154,  322,  322,  322,  322,  322,
 /*   500 */   151,  151,  322,   93,   93,  200,  322,  322,   60,  159,
 /*   510 */   100,  119,  322,  322,  322,  322,  322,  322,  322,  322,
 /*   520 */   200,  151,  322,  322,   93,  322,  322,  322,  322,  159,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    42,    6,    7,   49,   50,   51,   11,   12,   13,   14,
 /*    10 */    35,   36,   54,   55,   51,   57,   58,   59,   60,   61,
 /*    20 */    62,   63,   64,   52,   53,   67,   68,   69,   70,   71,
 /*    30 */    72,   73,   57,   58,   76,   50,   61,   12,   63,   57,
 /*    40 */    65,   66,   67,   68,   69,   70,   71,   72,   73,   35,
 /*    50 */    36,   76,    6,    7,   57,   58,   10,   57,   61,   57,
 /*    60 */    63,   17,   65,   66,   67,   68,   69,   70,   71,   72,
 /*    70 */    73,   57,   58,   76,   80,   61,   33,   63,   15,   65,
 /*    80 */    66,   67,   68,   69,   70,   71,   72,   73,   25,   26,
 /*    90 */    76,    0,   58,    2,    3,   61,    5,    6,    7,    8,
 /*   100 */    66,   79,   11,   12,   13,   14,   15,   56,   74,   58,
 /*   110 */    18,   19,   61,   21,   22,   33,   24,   25,   26,   27,
 /*   120 */    28,   30,   31,   32,   57,   58,    7,   79,   61,   76,
 /*   130 */    63,   12,   13,   14,   67,   68,   69,   70,   71,   72,
 /*   140 */    73,   57,   56,   76,   58,   61,   57,   61,    6,    7,
 /*   150 */     1,   67,   68,   69,   70,   71,   72,   73,   69,   37,
 /*   160 */    76,   57,   56,   58,   58,   61,   61,   61,    6,    7,
 /*   170 */     4,   67,   68,   69,   70,   71,   72,   73,   37,   57,
 /*   180 */    76,   58,    7,   61,   61,   47,   48,   12,   57,   67,
 /*   190 */    68,   69,   70,   71,   72,   73,   58,   37,   57,   61,
 /*   200 */    69,   12,   61,   15,   66,   47,   48,    4,   67,   68,
 /*   210 */    69,   70,   71,   72,   73,   37,   58,   57,   15,   61,
 /*   220 */    19,   61,   58,   22,   66,   61,   57,   67,   68,   69,
 /*   230 */    70,   71,   72,   73,   58,   57,   37,   61,   69,   61,
 /*   240 */    57,   15,   66,   47,   48,   67,   68,   69,   70,   71,
 /*   250 */    72,   73,   69,    6,   58,   58,   57,   61,   61,   12,
 /*   260 */    61,    9,   66,   66,   25,   26,   67,   68,   69,   70,
 /*   270 */    71,   72,   73,    1,   77,   78,   25,   26,    6,    7,
 /*   280 */     4,   16,   10,   11,   12,   13,   14,   15,   57,    9,
 /*   290 */     9,   15,   61,    6,    9,    4,   16,   16,   67,   68,
 /*   300 */    69,   70,   71,   72,   73,   57,    9,    9,   23,   61,
 /*   310 */     4,   47,   48,   16,   16,   67,   68,   69,   70,   71,
 /*   320 */    72,   73,   58,   19,   57,   61,   22,    8,   61,   19,
 /*   330 */    66,    9,   22,    9,   67,   68,   69,   70,   71,   72,
 /*   340 */    73,    6,    7,   15,   20,   10,   11,   12,   13,   14,
 /*   350 */    15,   19,    6,    7,   38,   39,   15,   11,   12,   13,
 /*   360 */    14,   15,   45,   46,    6,    7,   12,   38,   39,   11,
 /*   370 */    12,   13,   14,   15,   58,   58,   22,   61,   61,    6,
 /*   380 */     9,   19,   66,   66,   22,    9,   20,   58,   23,   17,
 /*   390 */    61,   74,   22,   77,   78,   66,   57,   18,   19,   23,
 /*   400 */    21,   22,   20,   24,   57,   19,   77,   78,   69,   70,
 /*   410 */    71,   72,    9,   45,   46,   76,   69,   70,   71,   72,
 /*   420 */    45,   46,   23,   76,   20,   19,   58,   58,    9,   61,
 /*   430 */    61,   17,   20,   58,   66,   66,   61,   43,   44,   43,
 /*   440 */    44,   66,   74,   19,   75,    9,   20,   22,   20,   74,
 /*   450 */    58,   20,   58,   61,   58,   61,   20,   61,   66,   29,
 /*   460 */    66,   23,   66,   45,   46,   45,   46,   40,   41,   75,
 /*   470 */    78,   75,   23,   29,   23,   23,   58,   81,   58,   61,
 /*   480 */    81,   61,   40,   41,   66,   58,   66,   81,   61,   81,
 /*   490 */    47,   48,   74,   66,   74,   81,   81,   81,   81,   81,
 /*   500 */    58,   58,   81,   61,   61,   78,   81,   81,   66,   66,
 /*   510 */    47,   48,   81,   81,   81,   81,   81,   81,   81,   81,
 /*   520 */    78,   58,   81,   81,   61,   81,   81,   81,   81,   66,
};
#define YY_SHIFT_USE_DFLT (-6)
#define YY_SHIFT_MAX 119
static const short yy_shift_ofst[] = {
 /*     0 */    -6,   91,  272,  272,  335,  346,  358,  358,   -5,   -5,
 /*    10 */    -5,   -5,   -5,   -5,   -5,   -5,   46,   46,  119,  119,
 /*    20 */    46,   46,   46,   46,   46,   46,   46,   46,   46,   46,
 /*    30 */    46,   46,   46,   46,   46,   46,   46,   46,   46,   92,
 /*    40 */    46,  142,  142,  142,  162,  175,  162,  162,  175,  175,
 /*    50 */   175,  379,  239,   25,   25,   25,   44,   43,   82,   -6,
 /*    60 */    -6,   63,  203,  280,  297,  276,  281,  247,  298,  251,
 /*    70 */   201,  304,  310,  324,  285,  362,  436,  376,  354,  149,
 /*    80 */   166,  189,  188,  226,  252,  265,  287,  291,  306,  319,
 /*    90 */   322,  328,  332,  341,  373,  371,  372,  366,  382,  370,
 /*   100 */   403,  365,  399,  386,  404,  406,  419,  414,  412,  424,
 /*   110 */   426,  428,  438,  431,  449,  430,  444,  451,  425,  452,
};
#define YY_REDUCE_USE_DFLT (-47)
#define YY_REDUCE_MAX 60
static const short yy_reduce_ofst[] = {
 /*     0 */   -29,  -42,  -25,   14,   -3,   67,   84,  104,  122,  141,
 /*    10 */   160,  178,  199,  231,  248,  267,  316,  329,  339,  347,
 /*    20 */   317,  368,  375,  394,  396,  418,  420,  427,  442,  138,
 /*    30 */   158,  196,  443,  197,  264,  463,   34,  369,  392,  -46,
 /*    40 */   176,   51,   86,  106,  105,   89,  123,  164,  131,  169,
 /*    50 */   183,  -37,  -15,  -18,    0,    2,   -6,   22,   48,   53,
 /*    60 */    -6,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   207,  283,  283,  283,  283,  283,  283,  283,  239,  239,
 /*    10 */   239,  239,  239,  239,  239,  239,  307,  307,  283,  283,
 /*    20 */   255,  255,  255,  270,  270,  255,  255,  311,  311,  263,
 /*    30 */   263,  263,  263,  320,  263,  263,  320,  320,  320,  320,
 /*    40 */   320,  210,  211,  320,  320,  320,  320,  320,  320,  320,
 /*    50 */   320,  252,  320,  320,  320,  320,  314,  313,  313,  283,
 /*    60 */   314,  294,  294,  320,  320,  296,  320,  320,  320,  293,
 /*    70 */   320,  320,  320,  320,  320,  320,  320,  320,  320,  320,
 /*    80 */   320,  320,  248,  320,  320,  298,  320,  320,  320,  225,
 /*    90 */   232,  296,  320,  294,  320,  256,  258,  320,  320,  320,
 /*   100 */   264,  320,  320,  320,  320,  320,  271,  273,  320,  320,
 /*   110 */   320,  320,  320,  320,  320,  226,  282,  320,  320,  320,
 /*   120 */   208,  209,  212,  240,  213,  220,  238,  241,  242,  243,
 /*   130 */   244,  245,  246,  247,  249,  250,  252,  297,  251,  298,
 /*   140 */   293,  295,  214,  215,  216,  217,  218,  219,  223,  229,
 /*   150 */   230,  233,  234,  237,  253,  254,  257,  259,  260,  261,
 /*   160 */   262,  265,  266,  267,  288,  290,  268,  269,  272,  274,
 /*   170 */   275,  284,  286,  276,  277,  278,  279,  285,  287,  289,
 /*   180 */   291,  292,  235,  236,  224,  280,  227,  228,  281,  299,
 /*   190 */   306,  315,  316,  318,  319,  317,  305,  308,  301,  310,
 /*   200 */   309,  312,  300,  302,  303,  304,
};
#define YY_SZ_ACTTAB (int)(sizeof(yy_action)/sizeof(yy_action[0]))

/* The next table maps tokens into fallback tokens.  If a construct
** like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammar, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
*/
#ifdef YYFALLBACK
static const YYCODETYPE yyFallback[] = {
};
#endif /* YYFALLBACK */

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
*/
struct yyStackEntry {
  YYACTIONTYPE stateno;  /* The state-number */
  YYCODETYPE major;      /* The major token value.  This is the code
                         ** number for the token at this stack level */
  YYMINORTYPE minor;     /* The user-supplied minor token value.  This
                         ** is the value of the token  */
};
typedef struct yyStackEntry yyStackEntry;

/* The state of the parser is completely contained in an instance of
** the following structure */
struct yyParser {
  int yyidx;                    /* Index of top element in stack */
#ifdef YYTRACKMAXSTACKDEPTH
  int yyidxMax;                 /* Maximum value of yyidx */
#endif
  int yyerrcnt;                 /* Shifts left before out of the error */
  converterARG_SDECL                /* A place to hold %extra_argument */
#if YYSTACKDEPTH<=0
  int yystksz;                  /* Current side of the stack */
  yyStackEntry *yystack;        /* The parser's stack */
#else
  yyStackEntry yystack[YYSTACKDEPTH];  /* The parser's stack */
#endif
};
typedef struct yyParser yyParser;

#ifndef NDEBUG
#include <stdio.h>
static FILE *yyTraceFILE = 0;
static char *yyTracePrompt = 0;
#endif /* NDEBUG */

#ifndef NDEBUG
/* 
** Turn parser tracing on by giving a stream to which to write the trace
** and a prompt to preface each trace message.  Tracing is turned off
** by making either argument NULL 
**
** Inputs:
** <ul>
** <li> A FILE* to which trace output should be written.
**      If NULL, then tracing is turned off.
** <li> A prefix string written at the beginning of every
**      line of trace output.  If NULL, then tracing is
**      turned off.
** </ul>
**
** Outputs:
** None.
*/
void converterTrace(FILE *TraceFILE, char *zTracePrompt){
  yyTraceFILE = TraceFILE;
  yyTracePrompt = zTracePrompt;
  if( yyTraceFILE==0 ) yyTracePrompt = 0;
  else if( yyTracePrompt==0 ) yyTraceFILE = 0;
}
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
static const char *const yyTokenName[] = { 
  "$",             "DOT",           "HIDE",          "SHOW",        
  "SLASH",         "EXTERNAL",      "IDENTIFIER",    "MINUS",       
  "IF",            "COMMA",         "NOT",           "STRING",      
  "NUMBER",        "SUP",           "INF",           "LBRAC",       
  "RBRAC",         "ASSIGN",        "SUM",           "LSBRAC",      
  "RSBRAC",        "COUNT",         "LCBRAC",        "RCBRAC",      
  "AVG",           "MIN",           "MAX",           "EVEN",        
  "ODD",           "VBAR",          "MINIMIZE",      "MAXIMIZE",    
  "COMPUTE",       "AT",            "error",         "body",        
  "nbody",         "termlist",      "nprio_list",    "prio_list",   
  "nprio_set",     "prio_set",      "head_ccondlist",  "nweightlist", 
  "weightlist",    "nnumweightlist",  "numweightlist",  "ncondlist",   
  "condlist",      "aggr",          "aggr_any",      "aggr_num",    
  "start",         "program",       "line",          "rule",        
  "signed",        "posnumber",     "predicate",     "optimize",    
  "compute",       "id",            "head",          "aggr_atom",   
  "disjunction",   "body_literal",  "predlit",       "string",      
  "empty",         "number",        "supremum",      "infimum",     
  "numterm",       "term",          "numweightlit",  "weightlit",   
  "undef",         "weightedpriolit",  "priolit",       "prio",        
  "one",         
};
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
 /*   0 */ "start ::= program",
 /*   1 */ "program ::=",
 /*   2 */ "program ::= program line DOT",
 /*   3 */ "line ::= rule",
 /*   4 */ "line ::= HIDE",
 /*   5 */ "line ::= SHOW",
 /*   6 */ "line ::= HIDE signed SLASH posnumber",
 /*   7 */ "line ::= HIDE predicate",
 /*   8 */ "line ::= SHOW signed SLASH posnumber",
 /*   9 */ "line ::= SHOW predicate",
 /*  10 */ "line ::= EXTERNAL predicate",
 /*  11 */ "line ::= EXTERNAL signed SLASH posnumber",
 /*  12 */ "line ::= optimize",
 /*  13 */ "line ::= compute",
 /*  14 */ "id ::= IDENTIFIER",
 /*  15 */ "signed ::= id",
 /*  16 */ "signed ::= MINUS IDENTIFIER",
 /*  17 */ "rule ::= head IF body",
 /*  18 */ "rule ::= IF body",
 /*  19 */ "rule ::= head",
 /*  20 */ "head ::= predicate",
 /*  21 */ "head ::= aggr_atom",
 /*  22 */ "head ::= disjunction",
 /*  23 */ "nbody ::= body_literal",
 /*  24 */ "nbody ::= nbody COMMA body_literal",
 /*  25 */ "body ::=",
 /*  26 */ "body ::= nbody",
 /*  27 */ "predlit ::= predicate",
 /*  28 */ "predlit ::= NOT predicate",
 /*  29 */ "body_literal ::= predlit",
 /*  30 */ "body_literal ::= aggr_atom",
 /*  31 */ "body_literal ::= NOT aggr_atom",
 /*  32 */ "string ::= STRING",
 /*  33 */ "empty ::=",
 /*  34 */ "posnumber ::= NUMBER",
 /*  35 */ "number ::= MINUS NUMBER",
 /*  36 */ "number ::= posnumber",
 /*  37 */ "supremum ::= SUP",
 /*  38 */ "infimum ::= INF",
 /*  39 */ "numterm ::= number",
 /*  40 */ "numterm ::= supremum",
 /*  41 */ "numterm ::= infimum",
 /*  42 */ "term ::= id",
 /*  43 */ "term ::= string",
 /*  44 */ "term ::= empty LBRAC termlist COMMA term RBRAC",
 /*  45 */ "term ::= id LBRAC termlist RBRAC",
 /*  46 */ "term ::= numterm",
 /*  47 */ "nnumweightlist ::= numweightlit",
 /*  48 */ "nnumweightlist ::= nnumweightlist COMMA numweightlit",
 /*  49 */ "numweightlist ::=",
 /*  50 */ "numweightlist ::= nnumweightlist",
 /*  51 */ "numweightlit ::= predlit ASSIGN number",
 /*  52 */ "numweightlit ::= predlit",
 /*  53 */ "aggr_num ::= SUM LSBRAC numweightlist RSBRAC",
 /*  54 */ "aggr_num ::= LSBRAC numweightlist RSBRAC",
 /*  55 */ "ncondlist ::= predlit",
 /*  56 */ "ncondlist ::= ncondlist COMMA predlit",
 /*  57 */ "condlist ::=",
 /*  58 */ "condlist ::= ncondlist",
 /*  59 */ "aggr_num ::= COUNT LCBRAC condlist RCBRAC",
 /*  60 */ "aggr_num ::= LCBRAC condlist RCBRAC",
 /*  61 */ "aggr_num ::= AVG LSBRAC numweightlist RSBRAC",
 /*  62 */ "nweightlist ::= weightlit",
 /*  63 */ "nweightlist ::= nweightlist COMMA weightlit",
 /*  64 */ "weightlist ::=",
 /*  65 */ "weightlist ::= nweightlist",
 /*  66 */ "weightlit ::= predlit ASSIGN term",
 /*  67 */ "weightlit ::= predlit",
 /*  68 */ "aggr_any ::= MIN LSBRAC weightlist RSBRAC",
 /*  69 */ "aggr_any ::= MAX LSBRAC weightlist RSBRAC",
 /*  70 */ "aggr ::= EVEN LSBRAC numweightlist RSBRAC",
 /*  71 */ "aggr ::= EVEN LCBRAC condlist RCBRAC",
 /*  72 */ "aggr ::= ODD LSBRAC numweightlist RSBRAC",
 /*  73 */ "aggr ::= ODD LCBRAC condlist RCBRAC",
 /*  74 */ "head_ccondlist ::= predicate VBAR predicate",
 /*  75 */ "head_ccondlist ::= head_ccondlist VBAR predicate",
 /*  76 */ "disjunction ::= head_ccondlist",
 /*  77 */ "undef ::=",
 /*  78 */ "aggr_atom ::= term aggr_any term",
 /*  79 */ "aggr_atom ::= undef aggr_any term",
 /*  80 */ "aggr_atom ::= term aggr_any undef",
 /*  81 */ "aggr_atom ::= undef aggr_any undef",
 /*  82 */ "aggr_atom ::= numterm aggr_num numterm",
 /*  83 */ "aggr_atom ::= undef aggr_num numterm",
 /*  84 */ "aggr_atom ::= numterm aggr_num undef",
 /*  85 */ "aggr_atom ::= undef aggr_num undef",
 /*  86 */ "aggr_atom ::= undef aggr undef",
 /*  87 */ "predicate ::= id LBRAC termlist RBRAC",
 /*  88 */ "predicate ::= id",
 /*  89 */ "predicate ::= MINUS IDENTIFIER LBRAC termlist RBRAC",
 /*  90 */ "predicate ::= MINUS IDENTIFIER",
 /*  91 */ "termlist ::= term",
 /*  92 */ "termlist ::= termlist COMMA term",
 /*  93 */ "optimize ::= MINIMIZE LSBRAC prio_list RSBRAC",
 /*  94 */ "optimize ::= MAXIMIZE LSBRAC prio_list RSBRAC",
 /*  95 */ "optimize ::= MINIMIZE LCBRAC prio_set RCBRAC",
 /*  96 */ "optimize ::= MAXIMIZE LCBRAC prio_set RCBRAC",
 /*  97 */ "compute ::= COMPUTE LCBRAC condlist RCBRAC",
 /*  98 */ "compute ::= COMPUTE NUMBER LCBRAC condlist RCBRAC",
 /*  99 */ "nprio_list ::= weightedpriolit",
 /* 100 */ "nprio_list ::= prio_list COMMA weightedpriolit",
 /* 101 */ "prio_list ::=",
 /* 102 */ "prio_list ::= nprio_list",
 /* 103 */ "nprio_set ::= priolit",
 /* 104 */ "nprio_set ::= prio_set COMMA priolit",
 /* 105 */ "prio_set ::=",
 /* 106 */ "prio_set ::= nprio_set",
 /* 107 */ "prio ::=",
 /* 108 */ "one ::=",
 /* 109 */ "weightedpriolit ::= predlit ASSIGN number AT number",
 /* 110 */ "weightedpriolit ::= predlit ASSIGN number prio",
 /* 111 */ "weightedpriolit ::= priolit",
 /* 112 */ "priolit ::= predlit one AT number",
 /* 113 */ "priolit ::= predlit one prio",
};
#endif /* NDEBUG */


#if YYSTACKDEPTH<=0
/*
** Try to increase the size of the parser stack.
*/
static void yyGrowStack(yyParser *p){
  int newSize;
  yyStackEntry *pNew;

  newSize = p->yystksz*2 + 100;
  pNew = (yyStackEntry *)realloc(p->yystack, newSize*sizeof(pNew[0]));
  if( pNew ){
    p->yystack = pNew;
    p->yystksz = newSize;
#ifndef NDEBUG
    if( yyTraceFILE ){
      fprintf(yyTraceFILE,"%sStack grows to %d entries!\n",
              yyTracePrompt, p->yystksz);
    }
#endif
  }
}
#endif

/* 
** This function allocates a new parser.
** The only argument is a pointer to a function which works like
** malloc.
**
** Inputs:
** A pointer to the function used to allocate memory.
**
** Outputs:
** A pointer to a parser.  This pointer is used in subsequent calls
** to converter and converterFree.
*/
void *converterAlloc(void *(*mallocProc)(size_t)){
  yyParser *pParser;
  pParser = (yyParser*)(*mallocProc)( (size_t)sizeof(yyParser) );
  if( pParser ){
    pParser->yyidx = -1;
#ifdef YYTRACKMAXSTACKDEPTH
    pParser->yyidxMax = 0;
#endif
#if YYSTACKDEPTH<=0
    pParser->yystack = NULL;
    pParser->yystksz = 0;
    yyGrowStack(pParser);
#endif
  }
  return pParser;
}

/* The following function deletes the value associated with a
** symbol.  The symbol can be either a terminal or nonterminal.
** "yymajor" is the symbol code, and "yypminor" is a pointer to
** the value.
*/
static void yy_destructor(
  yyParser *yypParser,    /* The parser */
  YYCODETYPE yymajor,     /* Type code for object to destroy */
  YYMINORTYPE *yypminor   /* The object to be destroyed */
){
  converterARG_FETCH;
  switch( yymajor ){
    /* Here is inserted the actions which take place when a
    ** terminal or non-terminal is destroyed.  This can happen
    ** when the symbol is popped from the stack during a
    ** reduce or during error processing or when a parser is 
    ** being destroyed before it is finished parsing.
    **
    ** Note: during a reduce, the only symbols destroyed are those
    ** which appear on the RHS of the rule, but which are not used
    ** inside the C code.
    */
      /* TERMINAL Destructor */
    case 1: /* DOT */
    case 2: /* HIDE */
    case 3: /* SHOW */
    case 4: /* SLASH */
    case 5: /* EXTERNAL */
    case 6: /* IDENTIFIER */
    case 7: /* MINUS */
    case 8: /* IF */
    case 9: /* COMMA */
    case 10: /* NOT */
    case 11: /* STRING */
    case 12: /* NUMBER */
    case 13: /* SUP */
    case 14: /* INF */
    case 15: /* LBRAC */
    case 16: /* RBRAC */
    case 17: /* ASSIGN */
    case 18: /* SUM */
    case 19: /* LSBRAC */
    case 20: /* RSBRAC */
    case 21: /* COUNT */
    case 22: /* LCBRAC */
    case 23: /* RCBRAC */
    case 24: /* AVG */
    case 25: /* MIN */
    case 26: /* MAX */
    case 27: /* EVEN */
    case 28: /* ODD */
    case 29: /* VBAR */
    case 30: /* MINIMIZE */
    case 31: /* MAXIMIZE */
    case 32: /* COMPUTE */
    case 33: /* AT */
{
#line 47 "converter_impl.y"
 (void)pConverter; (void)(yypminor->yy0); 
#line 676 "converter_impl.c"
}
      break;
    default:  break;   /* If no destructor action specified: do nothing */
  }
}

/*
** Pop the parser's stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
**
** Return the major token number for the symbol popped.
*/
static int yy_pop_parser_stack(yyParser *pParser){
  YYCODETYPE yymajor;
  yyStackEntry *yytos = &pParser->yystack[pParser->yyidx];

  if( pParser->yyidx<0 ) return 0;
#ifndef NDEBUG
  if( yyTraceFILE && pParser->yyidx>=0 ){
    fprintf(yyTraceFILE,"%sPopping %s\n",
      yyTracePrompt,
      yyTokenName[yytos->major]);
  }
#endif
  yymajor = yytos->major;
  yy_destructor(pParser, yymajor, &yytos->minor);
  pParser->yyidx--;
  return yymajor;
}

/* 
** Deallocate and destroy a parser.  Destructors are all called for
** all stack elements before shutting the parser down.
**
** Inputs:
** <ul>
** <li>  A pointer to the parser.  This should be a pointer
**       obtained from converterAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
void converterFree(
  void *p,                    /* The parser to be deleted */
  void (*freeProc)(void*)     /* Function used to reclaim memory */
){
  yyParser *pParser = (yyParser*)p;
  if( pParser==0 ) return;
  while( pParser->yyidx>=0 ) yy_pop_parser_stack(pParser);
#if YYSTACKDEPTH<=0
  free(pParser->yystack);
#endif
  (*freeProc)((void*)pParser);
}

/*
** Return the peak depth of the stack for a parser.
*/
#ifdef YYTRACKMAXSTACKDEPTH
int converterStackPeak(void *p){
  yyParser *pParser = (yyParser*)p;
  return pParser->yyidxMax;
}
#endif

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_shift_action(
  yyParser *pParser,        /* The parser */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
  int stateno = pParser->yystack[pParser->yyidx].stateno;
 
  if( stateno>YY_SHIFT_MAX || (i = yy_shift_ofst[stateno])==YY_SHIFT_USE_DFLT ){
    return yy_default[stateno];
  }
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
  if( i<0 || i>=YY_SZ_ACTTAB || yy_lookahead[i]!=iLookAhead ){
    if( iLookAhead>0 ){
#ifdef YYFALLBACK
      YYCODETYPE iFallback;            /* Fallback token */
      if( iLookAhead<sizeof(yyFallback)/sizeof(yyFallback[0])
             && (iFallback = yyFallback[iLookAhead])!=0 ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE, "%sFALLBACK %s => %s\n",
             yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[iFallback]);
        }
#endif
        return yy_find_shift_action(pParser, iFallback);
      }
#endif
#ifdef YYWILDCARD
      {
        int j = i - iLookAhead + YYWILDCARD;
        if( j>=0 && j<YY_SZ_ACTTAB && yy_lookahead[j]==YYWILDCARD ){
#ifndef NDEBUG
          if( yyTraceFILE ){
            fprintf(yyTraceFILE, "%sWILDCARD %s => %s\n",
               yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[YYWILDCARD]);
          }
#endif /* NDEBUG */
          return yy_action[j];
        }
      }
#endif /* YYWILDCARD */
    }
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_reduce_action(
  int stateno,              /* Current state number */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
#ifdef YYERRORSYMBOL
  if( stateno>YY_REDUCE_MAX ){
    return yy_default[stateno];
  }
#else
  assert( stateno<=YY_REDUCE_MAX );
#endif
  i = yy_reduce_ofst[stateno];
  assert( i!=YY_REDUCE_USE_DFLT );
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
#ifdef YYERRORSYMBOL
  if( i<0 || i>=YY_SZ_ACTTAB || yy_lookahead[i]!=iLookAhead ){
    return yy_default[stateno];
  }
#else
  assert( i>=0 && i<YY_SZ_ACTTAB );
  assert( yy_lookahead[i]==iLookAhead );
#endif
  return yy_action[i];
}

/*
** The following routine is called if the stack overflows.
*/
static void yyStackOverflow(yyParser *yypParser, YYMINORTYPE *yypMinor){
   (void)yypMinor;
   converterARG_FETCH;
   yypParser->yyidx--;
#ifndef NDEBUG
   if( yyTraceFILE ){
     fprintf(yyTraceFILE,"%sStack Overflow!\n",yyTracePrompt);
   }
#endif
   while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
   converterARG_STORE; /* Suppress warning about unused %extra_argument var */
}

/*
** Perform a shift action.
*/
static void yy_shift(
  yyParser *yypParser,          /* The parser to be shifted */
  int yyNewState,               /* The new state to shift in */
  int yyMajor,                  /* The major token to shift in */
  YYMINORTYPE *yypMinor         /* Pointer to the minor token to shift in */
){
  yyStackEntry *yytos;
  yypParser->yyidx++;
#ifdef YYTRACKMAXSTACKDEPTH
  if( yypParser->yyidx>yypParser->yyidxMax ){
    yypParser->yyidxMax = yypParser->yyidx;
  }
#endif
#if YYSTACKDEPTH>0 
  if( yypParser->yyidx>=YYSTACKDEPTH ){
    yyStackOverflow(yypParser, yypMinor);
    return;
  }
#else
  if( yypParser->yyidx>=yypParser->yystksz ){
    yyGrowStack(yypParser);
    if( yypParser->yyidx>=yypParser->yystksz ){
      yyStackOverflow(yypParser, yypMinor);
      return;
    }
  }
#endif
  yytos = &yypParser->yystack[yypParser->yyidx];
  yytos->stateno = (YYACTIONTYPE)yyNewState;
  yytos->major = (YYCODETYPE)yyMajor;
  yytos->minor = *yypMinor;
#ifndef NDEBUG
  if( yyTraceFILE && yypParser->yyidx>0 ){
    int i;
    fprintf(yyTraceFILE,"%sShift %d\n",yyTracePrompt,yyNewState);
    fprintf(yyTraceFILE,"%sStack:",yyTracePrompt);
    for(i=1; i<=yypParser->yyidx; i++)
      fprintf(yyTraceFILE," %s",yyTokenName[yypParser->yystack[i].major]);
    fprintf(yyTraceFILE,"\n");
  }
#endif
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
static const struct {
  YYCODETYPE lhs;         /* Symbol on the left-hand side of the rule */
  unsigned char nrhs;     /* Number of right-hand side symbols in the rule */
} yyRuleInfo[] = {
  { 52, 1 },
  { 53, 0 },
  { 53, 3 },
  { 54, 1 },
  { 54, 1 },
  { 54, 1 },
  { 54, 4 },
  { 54, 2 },
  { 54, 4 },
  { 54, 2 },
  { 54, 2 },
  { 54, 4 },
  { 54, 1 },
  { 54, 1 },
  { 61, 1 },
  { 56, 1 },
  { 56, 2 },
  { 55, 3 },
  { 55, 2 },
  { 55, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 36, 1 },
  { 36, 3 },
  { 35, 0 },
  { 35, 1 },
  { 66, 1 },
  { 66, 2 },
  { 65, 1 },
  { 65, 1 },
  { 65, 2 },
  { 67, 1 },
  { 68, 0 },
  { 57, 1 },
  { 69, 2 },
  { 69, 1 },
  { 70, 1 },
  { 71, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 6 },
  { 73, 4 },
  { 73, 1 },
  { 45, 1 },
  { 45, 3 },
  { 46, 0 },
  { 46, 1 },
  { 74, 3 },
  { 74, 1 },
  { 51, 4 },
  { 51, 3 },
  { 47, 1 },
  { 47, 3 },
  { 48, 0 },
  { 48, 1 },
  { 51, 4 },
  { 51, 3 },
  { 51, 4 },
  { 43, 1 },
  { 43, 3 },
  { 44, 0 },
  { 44, 1 },
  { 75, 3 },
  { 75, 1 },
  { 50, 4 },
  { 50, 4 },
  { 49, 4 },
  { 49, 4 },
  { 49, 4 },
  { 49, 4 },
  { 42, 3 },
  { 42, 3 },
  { 64, 1 },
  { 76, 0 },
  { 63, 3 },
  { 63, 3 },
  { 63, 3 },
  { 63, 3 },
  { 63, 3 },
  { 63, 3 },
  { 63, 3 },
  { 63, 3 },
  { 63, 3 },
  { 58, 4 },
  { 58, 1 },
  { 58, 5 },
  { 58, 2 },
  { 37, 1 },
  { 37, 3 },
  { 59, 4 },
  { 59, 4 },
  { 59, 4 },
  { 59, 4 },
  { 60, 4 },
  { 60, 5 },
  { 38, 1 },
  { 38, 3 },
  { 39, 0 },
  { 39, 1 },
  { 40, 1 },
  { 40, 3 },
  { 41, 0 },
  { 41, 1 },
  { 79, 0 },
  { 80, 0 },
  { 77, 5 },
  { 77, 4 },
  { 77, 1 },
  { 78, 4 },
  { 78, 3 },
};

static void yy_accept(yyParser*);  /* Forward Declaration */

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
static void yy_reduce(
  yyParser *yypParser,         /* The parser */
  int yyruleno                 /* Number of the rule by which to reduce */
){
  int yygoto;                     /* The next state */
  int yyact;                      /* The next action */
  YYMINORTYPE yygotominor;        /* The LHS of the rule reduced */
  yyStackEntry *yymsp;            /* The top of the parser's stack */
  int yysize;                     /* Amount to pop the stack */
  converterARG_FETCH;
  yymsp = &yypParser->yystack[yypParser->yyidx];
#ifndef NDEBUG
  if( yyTraceFILE && yyruleno>=0 
        && yyruleno<(int)(sizeof(yyRuleName)/sizeof(yyRuleName[0])) ){
    fprintf(yyTraceFILE, "%sReduce [%s].\n", yyTracePrompt,
      yyRuleName[yyruleno]);
  }
#endif /* NDEBUG */

  /* Silence complaints from purify about yygotominor being uninitialized
  ** in some cases when it is copied into the stack after the following
  ** switch.  yygotominor is uninitialized when a rule reduces that does
  ** not set the value of its left-hand side nonterminal.  Leaving the
  ** value of the nonterminal uninitialized is utterly harmless as long
  ** as the value is never used.  So really the only thing this code
  ** accomplishes is to quieten purify.  
  **
  ** 2007-01-16:  The wireshark project (www.wireshark.org) reports that
  ** without this code, their parser segfaults.  I'm not sure what there
  ** parser is doing to make this happen.  This is the second bug report
  ** from wireshark this week.  Clearly they are stressing Lemon in ways
  ** that it has not been previously stressed...  (SQLite ticket #2172)
  */
  /*memset(&yygotominor, 0, sizeof(yygotominor));*/
  yygotominor = yyzerominor;


  switch( yyruleno ){
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line <lineno> <grammarfile>
  **     { ... }           // User supplied code
  **  #line <lineno> <thisfile>
  **     break;
  */
      case 2: /* program ::= program line DOT */
#line 73 "converter_impl.y"
{
  yy_destructor(yypParser,1,&yymsp[0].minor);
}
#line 1079 "converter_impl.c"
        break;
      case 4: /* line ::= HIDE */
#line 76 "converter_impl.y"
{ pConverter->add(Converter::META_GLOBALSHOW);   yy_destructor(yypParser,2,&yymsp[0].minor);
}
#line 1085 "converter_impl.c"
        break;
      case 5: /* line ::= SHOW */
#line 77 "converter_impl.y"
{ pConverter->add(Converter::META_GLOBALHIDE);   yy_destructor(yypParser,3,&yymsp[0].minor);
}
#line 1091 "converter_impl.c"
        break;
      case 6: /* line ::= HIDE signed SLASH posnumber */
#line 78 "converter_impl.y"
{ pConverter->add(Converter::META_HIDE);   yy_destructor(yypParser,2,&yymsp[-3].minor);
  yy_destructor(yypParser,4,&yymsp[-1].minor);
}
#line 1098 "converter_impl.c"
        break;
      case 7: /* line ::= HIDE predicate */
#line 79 "converter_impl.y"
{ pConverter->add(Converter::STM_HIDE);   yy_destructor(yypParser,2,&yymsp[-1].minor);
}
#line 1104 "converter_impl.c"
        break;
      case 8: /* line ::= SHOW signed SLASH posnumber */
#line 80 "converter_impl.y"
{ pConverter->add(Converter::META_SHOW);   yy_destructor(yypParser,3,&yymsp[-3].minor);
  yy_destructor(yypParser,4,&yymsp[-1].minor);
}
#line 1111 "converter_impl.c"
        break;
      case 9: /* line ::= SHOW predicate */
#line 81 "converter_impl.y"
{ pConverter->add(Converter::STM_SHOW);   yy_destructor(yypParser,3,&yymsp[-1].minor);
}
#line 1117 "converter_impl.c"
        break;
      case 10: /* line ::= EXTERNAL predicate */
#line 82 "converter_impl.y"
{ pConverter->add(Converter::STM_EXTERNAL);   yy_destructor(yypParser,5,&yymsp[-1].minor);
}
#line 1123 "converter_impl.c"
        break;
      case 11: /* line ::= EXTERNAL signed SLASH posnumber */
#line 83 "converter_impl.y"
{ pConverter->add(Converter::META_EXTERNAL);   yy_destructor(yypParser,5,&yymsp[-3].minor);
  yy_destructor(yypParser,4,&yymsp[-1].minor);
}
#line 1130 "converter_impl.c"
        break;
      case 14: /* id ::= IDENTIFIER */
#line 87 "converter_impl.y"
{ pConverter->addSigned(yymsp[0].minor.yy0.index, false); }
#line 1135 "converter_impl.c"
        break;
      case 16: /* signed ::= MINUS IDENTIFIER */
#line 90 "converter_impl.y"
{ pConverter->addSigned(yymsp[0].minor.yy0.index, true);   yy_destructor(yypParser,7,&yymsp[-1].minor);
}
#line 1141 "converter_impl.c"
        break;
      case 17: /* rule ::= head IF body */
#line 92 "converter_impl.y"
{ pConverter->add(Converter::STM_RULE, yymsp[0].minor.yy51);   yy_destructor(yypParser,8,&yymsp[-1].minor);
}
#line 1147 "converter_impl.c"
        break;
      case 18: /* rule ::= IF body */
#line 93 "converter_impl.y"
{ pConverter->add(Converter::STM_CONSTRAINT, yymsp[0].minor.yy51);   yy_destructor(yypParser,8,&yymsp[-1].minor);
}
#line 1153 "converter_impl.c"
        break;
      case 19: /* rule ::= head */
#line 94 "converter_impl.y"
{ pConverter->add(Converter::STM_RULE, 0); }
#line 1158 "converter_impl.c"
        break;
      case 23: /* nbody ::= body_literal */
      case 47: /* nnumweightlist ::= numweightlit */ yytestcase(yyruleno==47);
      case 55: /* ncondlist ::= predlit */ yytestcase(yyruleno==55);
      case 62: /* nweightlist ::= weightlit */ yytestcase(yyruleno==62);
      case 91: /* termlist ::= term */ yytestcase(yyruleno==91);
      case 99: /* nprio_list ::= weightedpriolit */ yytestcase(yyruleno==99);
      case 103: /* nprio_set ::= priolit */ yytestcase(yyruleno==103);
#line 100 "converter_impl.y"
{ yygotominor.yy51 = 1; }
#line 1169 "converter_impl.c"
        break;
      case 24: /* nbody ::= nbody COMMA body_literal */
      case 48: /* nnumweightlist ::= nnumweightlist COMMA numweightlit */ yytestcase(yyruleno==48);
      case 56: /* ncondlist ::= ncondlist COMMA predlit */ yytestcase(yyruleno==56);
      case 63: /* nweightlist ::= nweightlist COMMA weightlit */ yytestcase(yyruleno==63);
      case 92: /* termlist ::= termlist COMMA term */ yytestcase(yyruleno==92);
      case 100: /* nprio_list ::= prio_list COMMA weightedpriolit */ yytestcase(yyruleno==100);
      case 104: /* nprio_set ::= prio_set COMMA priolit */ yytestcase(yyruleno==104);
#line 101 "converter_impl.y"
{ yygotominor.yy51 = yymsp[-2].minor.yy51 + 1;   yy_destructor(yypParser,9,&yymsp[-1].minor);
}
#line 1181 "converter_impl.c"
        break;
      case 25: /* body ::= */
      case 49: /* numweightlist ::= */ yytestcase(yyruleno==49);
      case 57: /* condlist ::= */ yytestcase(yyruleno==57);
      case 64: /* weightlist ::= */ yytestcase(yyruleno==64);
      case 101: /* prio_list ::= */ yytestcase(yyruleno==101);
      case 105: /* prio_set ::= */ yytestcase(yyruleno==105);
#line 102 "converter_impl.y"
{ yygotominor.yy51 = 0; }
#line 1191 "converter_impl.c"
        break;
      case 26: /* body ::= nbody */
      case 50: /* numweightlist ::= nnumweightlist */ yytestcase(yyruleno==50);
      case 58: /* condlist ::= ncondlist */ yytestcase(yyruleno==58);
      case 65: /* weightlist ::= nweightlist */ yytestcase(yyruleno==65);
      case 102: /* prio_list ::= nprio_list */ yytestcase(yyruleno==102);
      case 106: /* prio_set ::= nprio_set */ yytestcase(yyruleno==106);
#line 103 "converter_impl.y"
{ yygotominor.yy51 = yymsp[0].minor.yy51; }
#line 1201 "converter_impl.c"
        break;
      case 28: /* predlit ::= NOT predicate */
      case 31: /* body_literal ::= NOT aggr_atom */ yytestcase(yyruleno==31);
#line 106 "converter_impl.y"
{ pConverter->addSign();   yy_destructor(yypParser,10,&yymsp[-1].minor);
}
#line 1208 "converter_impl.c"
        break;
      case 32: /* string ::= STRING */
#line 112 "converter_impl.y"
{ pConverter->addVal(Val::create(Val::ID, yymsp[0].minor.yy0.index)); }
#line 1213 "converter_impl.c"
        break;
      case 33: /* empty ::= */
#line 113 "converter_impl.y"
{ pConverter->addVal(Val::create(Val::ID, pConverter->storage()->index(""))); }
#line 1218 "converter_impl.c"
        break;
      case 34: /* posnumber ::= NUMBER */
#line 114 "converter_impl.y"
{ pConverter->addVal(Val::create(Val::NUM, yymsp[0].minor.yy0.number)); }
#line 1223 "converter_impl.c"
        break;
      case 35: /* number ::= MINUS NUMBER */
#line 115 "converter_impl.y"
{ pConverter->addVal(Val::create(Val::NUM, -yymsp[0].minor.yy0.number));   yy_destructor(yypParser,7,&yymsp[-1].minor);
}
#line 1229 "converter_impl.c"
        break;
      case 37: /* supremum ::= SUP */
#line 117 "converter_impl.y"
{ pConverter->addVal(Val::sup());   yy_destructor(yypParser,13,&yymsp[0].minor);
}
#line 1235 "converter_impl.c"
        break;
      case 38: /* infimum ::= INF */
#line 118 "converter_impl.y"
{ pConverter->addVal(Val::inf());   yy_destructor(yypParser,14,&yymsp[0].minor);
}
#line 1241 "converter_impl.c"
        break;
      case 39: /* numterm ::= number */
      case 40: /* numterm ::= supremum */ yytestcase(yyruleno==40);
      case 41: /* numterm ::= infimum */ yytestcase(yyruleno==41);
      case 42: /* term ::= id */ yytestcase(yyruleno==42);
      case 43: /* term ::= string */ yytestcase(yyruleno==43);
#line 120 "converter_impl.y"
{ pConverter->add(Converter::TERM, 0); }
#line 1250 "converter_impl.c"
        break;
      case 44: /* term ::= empty LBRAC termlist COMMA term RBRAC */
#line 125 "converter_impl.y"
{ pConverter->add(Converter::TERM, yymsp[-3].minor.yy51+1);   yy_destructor(yypParser,15,&yymsp[-4].minor);
  yy_destructor(yypParser,9,&yymsp[-2].minor);
  yy_destructor(yypParser,16,&yymsp[0].minor);
}
#line 1258 "converter_impl.c"
        break;
      case 45: /* term ::= id LBRAC termlist RBRAC */
#line 126 "converter_impl.y"
{ pConverter->add(Converter::TERM, yymsp[-1].minor.yy51);   yy_destructor(yypParser,15,&yymsp[-2].minor);
  yy_destructor(yypParser,16,&yymsp[0].minor);
}
#line 1265 "converter_impl.c"
        break;
      case 51: /* numweightlit ::= predlit ASSIGN number */
      case 66: /* weightlit ::= predlit ASSIGN term */ yytestcase(yyruleno==66);
#line 134 "converter_impl.y"
{
  yy_destructor(yypParser,17,&yymsp[-1].minor);
}
#line 1273 "converter_impl.c"
        break;
      case 52: /* numweightlit ::= predlit */
      case 67: /* weightlit ::= predlit */ yytestcase(yyruleno==67);
      case 108: /* one ::= */ yytestcase(yyruleno==108);
#line 135 "converter_impl.y"
{ pConverter->addVal(ONE); }
#line 1280 "converter_impl.c"
        break;
      case 53: /* aggr_num ::= SUM LSBRAC numweightlist RSBRAC */
#line 137 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_SUM, yymsp[-1].minor.yy51);   yy_destructor(yypParser,18,&yymsp[-3].minor);
  yy_destructor(yypParser,19,&yymsp[-2].minor);
  yy_destructor(yypParser,20,&yymsp[0].minor);
}
#line 1288 "converter_impl.c"
        break;
      case 54: /* aggr_num ::= LSBRAC numweightlist RSBRAC */
#line 138 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_SUM, yymsp[-1].minor.yy51);   yy_destructor(yypParser,19,&yymsp[-2].minor);
  yy_destructor(yypParser,20,&yymsp[0].minor);
}
#line 1295 "converter_impl.c"
        break;
      case 59: /* aggr_num ::= COUNT LCBRAC condlist RCBRAC */
#line 145 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_COUNT, yymsp[-1].minor.yy51);   yy_destructor(yypParser,21,&yymsp[-3].minor);
  yy_destructor(yypParser,22,&yymsp[-2].minor);
  yy_destructor(yypParser,23,&yymsp[0].minor);
}
#line 1303 "converter_impl.c"
        break;
      case 60: /* aggr_num ::= LCBRAC condlist RCBRAC */
#line 146 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_COUNT, yymsp[-1].minor.yy51);   yy_destructor(yypParser,22,&yymsp[-2].minor);
  yy_destructor(yypParser,23,&yymsp[0].minor);
}
#line 1310 "converter_impl.c"
        break;
      case 61: /* aggr_num ::= AVG LSBRAC numweightlist RSBRAC */
#line 148 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_AVG, yymsp[-1].minor.yy51);   yy_destructor(yypParser,24,&yymsp[-3].minor);
  yy_destructor(yypParser,19,&yymsp[-2].minor);
  yy_destructor(yypParser,20,&yymsp[0].minor);
}
#line 1318 "converter_impl.c"
        break;
      case 68: /* aggr_any ::= MIN LSBRAC weightlist RSBRAC */
#line 158 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_MIN, yymsp[-1].minor.yy51);   yy_destructor(yypParser,25,&yymsp[-3].minor);
  yy_destructor(yypParser,19,&yymsp[-2].minor);
  yy_destructor(yypParser,20,&yymsp[0].minor);
}
#line 1326 "converter_impl.c"
        break;
      case 69: /* aggr_any ::= MAX LSBRAC weightlist RSBRAC */
#line 159 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_MAX, yymsp[-1].minor.yy51);   yy_destructor(yypParser,26,&yymsp[-3].minor);
  yy_destructor(yypParser,19,&yymsp[-2].minor);
  yy_destructor(yypParser,20,&yymsp[0].minor);
}
#line 1334 "converter_impl.c"
        break;
      case 70: /* aggr ::= EVEN LSBRAC numweightlist RSBRAC */
#line 161 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_EVEN, yymsp[-1].minor.yy51);   yy_destructor(yypParser,27,&yymsp[-3].minor);
  yy_destructor(yypParser,19,&yymsp[-2].minor);
  yy_destructor(yypParser,20,&yymsp[0].minor);
}
#line 1342 "converter_impl.c"
        break;
      case 71: /* aggr ::= EVEN LCBRAC condlist RCBRAC */
#line 162 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_EVEN_SET, yymsp[-1].minor.yy51);   yy_destructor(yypParser,27,&yymsp[-3].minor);
  yy_destructor(yypParser,22,&yymsp[-2].minor);
  yy_destructor(yypParser,23,&yymsp[0].minor);
}
#line 1350 "converter_impl.c"
        break;
      case 72: /* aggr ::= ODD LSBRAC numweightlist RSBRAC */
#line 163 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_ODD, yymsp[-1].minor.yy51);   yy_destructor(yypParser,28,&yymsp[-3].minor);
  yy_destructor(yypParser,19,&yymsp[-2].minor);
  yy_destructor(yypParser,20,&yymsp[0].minor);
}
#line 1358 "converter_impl.c"
        break;
      case 73: /* aggr ::= ODD LCBRAC condlist RCBRAC */
#line 164 "converter_impl.y"
{ Aggr::create(yygotominor.yy158, Converter::AGGR_ODD_SET, yymsp[-1].minor.yy51);   yy_destructor(yypParser,28,&yymsp[-3].minor);
  yy_destructor(yypParser,22,&yymsp[-2].minor);
  yy_destructor(yypParser,23,&yymsp[0].minor);
}
#line 1366 "converter_impl.c"
        break;
      case 74: /* head_ccondlist ::= predicate VBAR predicate */
#line 166 "converter_impl.y"
{ yygotominor.yy51 = 2;   yy_destructor(yypParser,29,&yymsp[-1].minor);
}
#line 1372 "converter_impl.c"
        break;
      case 75: /* head_ccondlist ::= head_ccondlist VBAR predicate */
#line 167 "converter_impl.y"
{ yygotominor.yy51 = yymsp[-2].minor.yy51 + 1;   yy_destructor(yypParser,29,&yymsp[-1].minor);
}
#line 1378 "converter_impl.c"
        break;
      case 76: /* disjunction ::= head_ccondlist */
#line 169 "converter_impl.y"
{ pConverter->add(Converter::AGGR_DISJUNCTION, yymsp[0].minor.yy51); }
#line 1383 "converter_impl.c"
        break;
      case 77: /* undef ::= */
#line 171 "converter_impl.y"
{ pConverter->addVal(UNDEF); }
#line 1388 "converter_impl.c"
        break;
      case 78: /* aggr_atom ::= term aggr_any term */
      case 79: /* aggr_atom ::= undef aggr_any term */ yytestcase(yyruleno==79);
      case 80: /* aggr_atom ::= term aggr_any undef */ yytestcase(yyruleno==80);
      case 81: /* aggr_atom ::= undef aggr_any undef */ yytestcase(yyruleno==81);
      case 82: /* aggr_atom ::= numterm aggr_num numterm */ yytestcase(yyruleno==82);
      case 83: /* aggr_atom ::= undef aggr_num numterm */ yytestcase(yyruleno==83);
      case 84: /* aggr_atom ::= numterm aggr_num undef */ yytestcase(yyruleno==84);
      case 85: /* aggr_atom ::= undef aggr_num undef */ yytestcase(yyruleno==85);
      case 86: /* aggr_atom ::= undef aggr undef */ yytestcase(yyruleno==86);
#line 173 "converter_impl.y"
{ pConverter->add(yymsp[-1].minor.yy158.type, yymsp[-1].minor.yy158.n); }
#line 1401 "converter_impl.c"
        break;
      case 87: /* predicate ::= id LBRAC termlist RBRAC */
#line 185 "converter_impl.y"
{ pConverter->add(Converter::LIT, yymsp[-1].minor.yy51);   yy_destructor(yypParser,15,&yymsp[-2].minor);
  yy_destructor(yypParser,16,&yymsp[0].minor);
}
#line 1408 "converter_impl.c"
        break;
      case 88: /* predicate ::= id */
#line 186 "converter_impl.y"
{ pConverter->add(Converter::LIT, 0); }
#line 1413 "converter_impl.c"
        break;
      case 89: /* predicate ::= MINUS IDENTIFIER LBRAC termlist RBRAC */
#line 187 "converter_impl.y"
{ pConverter->addSigned(yymsp[-3].minor.yy0.index, true); pConverter->add(Converter::LIT, yymsp[-1].minor.yy51);   yy_destructor(yypParser,7,&yymsp[-4].minor);
  yy_destructor(yypParser,15,&yymsp[-2].minor);
  yy_destructor(yypParser,16,&yymsp[0].minor);
}
#line 1421 "converter_impl.c"
        break;
      case 90: /* predicate ::= MINUS IDENTIFIER */
#line 188 "converter_impl.y"
{ pConverter->addSigned(yymsp[0].minor.yy0.index, true); pConverter->add(Converter::LIT, 0);   yy_destructor(yypParser,7,&yymsp[-1].minor);
}
#line 1427 "converter_impl.c"
        break;
      case 93: /* optimize ::= MINIMIZE LSBRAC prio_list RSBRAC */
#line 193 "converter_impl.y"
{ pConverter->add(Converter::STM_MINIMIZE, yymsp[-1].minor.yy51); pConverter->nextLevel();   yy_destructor(yypParser,30,&yymsp[-3].minor);
  yy_destructor(yypParser,19,&yymsp[-2].minor);
  yy_destructor(yypParser,20,&yymsp[0].minor);
}
#line 1435 "converter_impl.c"
        break;
      case 94: /* optimize ::= MAXIMIZE LSBRAC prio_list RSBRAC */
#line 194 "converter_impl.y"
{ pConverter->add(Converter::STM_MAXIMIZE, yymsp[-1].minor.yy51); pConverter->nextLevel();   yy_destructor(yypParser,31,&yymsp[-3].minor);
  yy_destructor(yypParser,19,&yymsp[-2].minor);
  yy_destructor(yypParser,20,&yymsp[0].minor);
}
#line 1443 "converter_impl.c"
        break;
      case 95: /* optimize ::= MINIMIZE LCBRAC prio_set RCBRAC */
#line 195 "converter_impl.y"
{ pConverter->add(Converter::STM_MINIMIZE_SET, yymsp[-1].minor.yy51); pConverter->nextLevel();   yy_destructor(yypParser,30,&yymsp[-3].minor);
  yy_destructor(yypParser,22,&yymsp[-2].minor);
  yy_destructor(yypParser,23,&yymsp[0].minor);
}
#line 1451 "converter_impl.c"
        break;
      case 96: /* optimize ::= MAXIMIZE LCBRAC prio_set RCBRAC */
#line 196 "converter_impl.y"
{ pConverter->add(Converter::STM_MAXIMIZE_SET, yymsp[-1].minor.yy51); pConverter->nextLevel();   yy_destructor(yypParser,31,&yymsp[-3].minor);
  yy_destructor(yypParser,22,&yymsp[-2].minor);
  yy_destructor(yypParser,23,&yymsp[0].minor);
}
#line 1459 "converter_impl.c"
        break;
      case 97: /* compute ::= COMPUTE LCBRAC condlist RCBRAC */
#line 198 "converter_impl.y"
{ pConverter->add(Converter::STM_COMPUTE, yymsp[-1].minor.yy51);   yy_destructor(yypParser,32,&yymsp[-3].minor);
  yy_destructor(yypParser,22,&yymsp[-2].minor);
  yy_destructor(yypParser,23,&yymsp[0].minor);
}
#line 1467 "converter_impl.c"
        break;
      case 98: /* compute ::= COMPUTE NUMBER LCBRAC condlist RCBRAC */
#line 199 "converter_impl.y"
{ pConverter->add(Converter::STM_COMPUTE, yymsp[-1].minor.yy51);   yy_destructor(yypParser,32,&yymsp[-4].minor);
  yy_destructor(yypParser,12,&yymsp[-3].minor);
  yy_destructor(yypParser,22,&yymsp[-2].minor);
  yy_destructor(yypParser,23,&yymsp[0].minor);
}
#line 1476 "converter_impl.c"
        break;
      case 107: /* prio ::= */
#line 211 "converter_impl.y"
{ pConverter->addVal(PRIO); }
#line 1481 "converter_impl.c"
        break;
      case 109: /* weightedpriolit ::= predlit ASSIGN number AT number */
#line 214 "converter_impl.y"
{
  yy_destructor(yypParser,17,&yymsp[-3].minor);
  yy_destructor(yypParser,33,&yymsp[-1].minor);
}
#line 1489 "converter_impl.c"
        break;
      case 110: /* weightedpriolit ::= predlit ASSIGN number prio */
#line 215 "converter_impl.y"
{
  yy_destructor(yypParser,17,&yymsp[-2].minor);
}
#line 1496 "converter_impl.c"
        break;
      case 112: /* priolit ::= predlit one AT number */
#line 218 "converter_impl.y"
{
  yy_destructor(yypParser,33,&yymsp[-1].minor);
}
#line 1503 "converter_impl.c"
        break;
      default:
      /* (0) start ::= program */ yytestcase(yyruleno==0);
      /* (1) program ::= */ yytestcase(yyruleno==1);
      /* (3) line ::= rule */ yytestcase(yyruleno==3);
      /* (12) line ::= optimize */ yytestcase(yyruleno==12);
      /* (13) line ::= compute */ yytestcase(yyruleno==13);
      /* (15) signed ::= id */ yytestcase(yyruleno==15);
      /* (20) head ::= predicate */ yytestcase(yyruleno==20);
      /* (21) head ::= aggr_atom */ yytestcase(yyruleno==21);
      /* (22) head ::= disjunction */ yytestcase(yyruleno==22);
      /* (27) predlit ::= predicate */ yytestcase(yyruleno==27);
      /* (29) body_literal ::= predlit */ yytestcase(yyruleno==29);
      /* (30) body_literal ::= aggr_atom */ yytestcase(yyruleno==30);
      /* (36) number ::= posnumber */ yytestcase(yyruleno==36);
      /* (46) term ::= numterm */ yytestcase(yyruleno==46);
      /* (111) weightedpriolit ::= priolit */ yytestcase(yyruleno==111);
      /* (113) priolit ::= predlit one prio */ yytestcase(yyruleno==113);
        break;
  };
  yygoto = yyRuleInfo[yyruleno].lhs;
  yysize = yyRuleInfo[yyruleno].nrhs;
  yypParser->yyidx -= yysize;
  yyact = yy_find_reduce_action(yymsp[-yysize].stateno,(YYCODETYPE)yygoto);
  if( yyact < YYNSTATE ){
#ifdef NDEBUG
    /* If we are not debugging and the reduce action popped at least
    ** one element off the stack, then we can push the new element back
    ** onto the stack here, and skip the stack overflow test in yy_shift().
    ** That gives a significant speed improvement. */
    if( yysize ){
      yypParser->yyidx++;
      yymsp -= yysize-1;
      yymsp->stateno = (YYACTIONTYPE)yyact;
      yymsp->major = (YYCODETYPE)yygoto;
      yymsp->minor = yygotominor;
    }else
#endif
    {
      yy_shift(yypParser,yyact,yygoto,&yygotominor);
    }
  }else{
    assert( yyact == YYNSTATE + YYNRULE + 1 );
    yy_accept(yypParser);
  }
}

/*
** The following code executes when the parse fails
*/
#ifndef YYNOERRORRECOVERY
static void yy_parse_failed(
  yyParser *yypParser           /* The parser */
){
  converterARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sFail!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
#line 43 "converter_impl.y"
 pConverter->parseError(); 
#line 1569 "converter_impl.c"
  converterARG_STORE; /* Suppress warning about unused %extra_argument variable */
}
#endif /* YYNOERRORRECOVERY */

/*
** The following code executes when a syntax error first occurs.
*/
static void yy_syntax_error(
  yyParser *yypParser,           /* The parser */
  int yymajor,                   /* The major type of the error token */
  YYMINORTYPE yyminor            /* The minor type of the error token */
){
  (void)yymajor;
  (void)yyminor;
  converterARG_FETCH;
#define TOKEN (yyminor.yy0)
#line 44 "converter_impl.y"
 pConverter->syntaxError(); 
#line 1588 "converter_impl.c"
  converterARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  converterARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sAccept!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
  converterARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "converterAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
void converter(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  converterTOKENTYPE yyminor       /* The value for the token */
  converterARG_PDECL               /* Optional %extra_argument parameter */
){
  YYMINORTYPE yyminorunion;
  int yyact;            /* The parser action. */
  int yyendofinput;     /* True if we are at the end of input */
#ifdef YYERRORSYMBOL
  int yyerrorhit = 0;   /* True if yymajor has invoked an error */
#endif
  yyParser *yypParser;  /* The parser */

  /* (re)initialize the parser, if necessary */
  yypParser = (yyParser*)yyp;
  if( yypParser->yyidx<0 ){
#if YYSTACKDEPTH<=0
    if( yypParser->yystksz <=0 ){
      /*memset(&yyminorunion, 0, sizeof(yyminorunion));*/
      yyminorunion = yyzerominor;
      yyStackOverflow(yypParser, &yyminorunion);
      return;
    }
#endif
    yypParser->yyidx = 0;
    yypParser->yyerrcnt = -1;
    yypParser->yystack[0].stateno = 0;
    yypParser->yystack[0].major = 0;
  }
  yyminorunion.yy0 = yyminor;
  yyendofinput = (yymajor==0);
  converterARG_STORE;

#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sInput %s\n",yyTracePrompt,yyTokenName[yymajor]);
  }
#endif

  do{
    yyact = yy_find_shift_action(yypParser,(YYCODETYPE)yymajor);
    if( yyact<YYNSTATE ){
      assert( !yyendofinput );  /* Impossible to shift the $ token */
      yy_shift(yypParser,yyact,yymajor,&yyminorunion);
      yypParser->yyerrcnt--;
      yymajor = YYNOCODE;
    }else if( yyact < YYNSTATE + YYNRULE ){
      yy_reduce(yypParser,yyact-YYNSTATE);
    }else{
      assert( yyact == YY_ERROR_ACTION );
#ifdef YYERRORSYMBOL
      int yymx;
#endif
#ifndef NDEBUG
      if( yyTraceFILE ){
        fprintf(yyTraceFILE,"%sSyntax Error!\n",yyTracePrompt);
      }
#endif
#ifdef YYERRORSYMBOL
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token "ERROR".  
      **
      ** This is what we do if the grammar does define ERROR:
      **
      **  * Call the %syntax_error function.
      **
      **  * Begin popping the stack until we enter a state where
      **    it is legal to shift the error symbol, then shift
      **    the error symbol.
      **
      **  * Set the error count to three.
      **
      **  * Begin accepting and shifting new tokens.  No new error
      **    processing will occur until three tokens have been
      **    shifted successfully.
      **
      */
      if( yypParser->yyerrcnt<0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yymx = yypParser->yystack[yypParser->yyidx].major;
      if( yymx==YYERRORSYMBOL || yyerrorhit ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE,"%sDiscard input token %s\n",
             yyTracePrompt,yyTokenName[yymajor]);
        }
#endif
        yy_destructor(yypParser, (YYCODETYPE)yymajor,&yyminorunion);
        yymajor = YYNOCODE;
      }else{
         while(
          yypParser->yyidx >= 0 &&
          yymx != YYERRORSYMBOL &&
          (yyact = yy_find_reduce_action(
                        yypParser->yystack[yypParser->yyidx].stateno,
                        YYERRORSYMBOL)) >= YYNSTATE
        ){
          yy_pop_parser_stack(yypParser);
        }
        if( yypParser->yyidx < 0 || yymajor==0 ){
          yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
          yy_parse_failed(yypParser);
          yymajor = YYNOCODE;
        }else if( yymx!=YYERRORSYMBOL ){
          YYMINORTYPE u2;
          u2.YYERRSYMDT = 0;
          yy_shift(yypParser,yyact,YYERRORSYMBOL,&u2);
        }
      }
      yypParser->yyerrcnt = 3;
      yyerrorhit = 1;
#elif defined(YYNOERRORRECOVERY)
      /* If the YYNOERRORRECOVERY macro is defined, then do not attempt to
      ** do any kind of error recovery.  Instead, simply invoke the syntax
      ** error routine and continue going as if nothing had happened.
      **
      ** Applications can set this macro (for example inside %include) if
      ** they intend to abandon the parse upon the first syntax error seen.
      */
      yy_syntax_error(yypParser,yymajor,yyminorunion);
      yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
      yymajor = YYNOCODE;
      
#else  /* YYERRORSYMBOL is not defined */
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if( yypParser->yyerrcnt<=0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yypParser->yyerrcnt = 3;
      yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
      if( yyendofinput ){
        yy_parse_failed(yypParser);
      }
      yymajor = YYNOCODE;
#endif
    }
  }while( yymajor!=YYNOCODE && yypParser->yyidx>=0 );
  return;
}
