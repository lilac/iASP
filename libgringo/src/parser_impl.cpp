/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is included that follows the "include" declaration
** in the input grammar file. */
#include <stdio.h>
#line 18 "parser_impl.y"

#include "parser_impl.h"
#include "gringo/gringo.h"
#include "gringo/parser.h"
#include "gringo/grounder.h"
#include "gringo/output.h"
#include "gringo/domain.h"

#include "gringo/rule.h"
#include "gringo/optimize.h"
#include "gringo/display.h"
#include "gringo/external.h"
#include "gringo/compute.h"

#include "gringo/predlit.h"
#include "gringo/rellit.h"

#include "gringo/sumaggrlit.h"
#include "gringo/avgaggrlit.h"
#include "gringo/junctionaggrlit.h"
#include "gringo/minmaxaggrlit.h"
#include "gringo/parityaggrlit.h"

#include "gringo/constterm.h"
#include "gringo/varterm.h"
#include "gringo/mathterm.h"
#include "gringo/poolterm.h"
#include "gringo/argterm.h"
#include "gringo/rangeterm.h"
#include "gringo/functerm.h"
#include "gringo/luaterm.h"

#define GRD pParser->grounder()
#define OUT pParser->grounder()->output()
#define ONE(loc) new ConstTerm(loc, Val::create(Val::NUM, 1))
#define ZERO(loc) new ConstTerm(loc, Val::create(Val::NUM, 0))
#define MINUSONE(loc) new ConstTerm(loc, Val::create(Val::NUM, -1))

template <class T>
void del(T x)
{
	delete x;
}

template <class T>
boost::ptr_vector<T> *vec1(T *x)
{
	boost::ptr_vector<T> *v = new boost::ptr_vector<T>();
	v->push_back(x);
	return v;
}

#line 61 "parser_impl.c"
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
**    parserTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is parserTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    parserARG_SDECL     A static variable declaration for the %extra_argument
**    parserARG_PDECL     A parameter declaration for the %extra_argument
**    parserARG_STORE     Code to store %extra_argument into yypParser
**    parserARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
#define YYCODETYPE unsigned char
#define YYNOCODE 125
#define YYACTIONTYPE unsigned short int
#define parserTOKENTYPE  Parser::Token 
typedef union {
  int yyinit;
  parserTOKENTYPE yy0;
  LitPtrVec* yy52;
  Term* yy83;
  Compute* yy104;
  VarSigVec* yy117;
  MathTerm::Func* yy153;
  PredLit* yy155;
  Rule* yy179;
  RelLit::Type yy183;
  Optimize* yy184;
  Lit* yy212;
  AggrLit* yy221;
  CondLitVec* yy226;
  TermPtrVec* yy243;
  CondLit* yy244;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 0
#endif
#define parserARG_SDECL  Parser *pParser ;
#define parserARG_PDECL , Parser *pParser 
#define parserARG_FETCH  Parser *pParser  = yypParser->pParser 
#define parserARG_STORE yypParser->pParser  = pParser 
#define YYNSTATE 341
#define YYNRULE 173
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
 /*     0 */   341,  265,  104,   54,  236,  258,  236,  258,   88,  515,
 /*    10 */     1,   56,  174,   57,  175,   23,  173,  169,  204,  203,
 /*    20 */    21,   75,  293,  166,  239,   49,   50,  240,  184,  136,
 /*    30 */    99,  101,   40,   23,   51,  191,  192,  277,  179,  238,
 /*    40 */     3,   63,   64,   67,   68,   65,   66,   69,   70,  241,
 /*    50 */   242,  243,  172,  176,   84,  195,    6,   13,  200,  202,
 /*    60 */     9,   12,  161,  162,  206,  208,  224,  225,  164,   60,
 /*    70 */    59,   72,   73,   71,   61,   62,   63,   64,   67,   68,
 /*    80 */    65,   66,   69,   70,   60,   59,   72,   73,   71,   61,
 /*    90 */    62,   63,   64,   67,   68,   65,   66,   69,   70,   61,
 /*   100 */    62,   63,   64,   67,   68,   65,   66,   69,   70,   34,
 /*   110 */   259,  260,  261,  262,  263,  264,  300,   92,  169,  361,
 /*   120 */    23,   85,  195,    6,  227,  200,  202,    9,   85,  236,
 /*   130 */    55,  206,  208,   54,   88,   95,  236,  258,  174,  179,
 /*   140 */   175,   42,  173,   57,  110,  300,   92,   75,  205,  203,
 /*   150 */   239,  100,  293,  240,  458,  136,   52,  121,   40,  236,
 /*   160 */   258,  362,  321,  320,  319,  214,   57,   16,  198,  196,
 /*   170 */   284,   21,  226,  337,  187,  241,  242,  243,  172,  176,
 /*   180 */    84,  195,    6,   38,  200,  202,    9,  268,  161,  162,
 /*   190 */   206,  208,   54,   88,  123,  236,  258,  174,  132,  175,
 /*   200 */   131,  173,   57,  110,  120,  143,   75,  210,  203,  239,
 /*   210 */   237,  293,  240,  306,  136,   52,   15,   40,  236,  258,
 /*   220 */    14,  322,  320,  319,  238,   57,  140,  199,  196,  284,
 /*   230 */   329,  220,   41,   58,  241,  242,  243,  172,  176,   84,
 /*   240 */   195,    6,  141,  200,  202,    9,  249,  161,  162,  206,
 /*   250 */   208,   60,   59,   72,   73,   71,   61,   62,   63,   64,
 /*   260 */    67,   68,   65,   66,   69,   70,   60,   59,   72,   73,
 /*   270 */    71,   61,   62,   63,   64,   67,   68,   65,   66,   69,
 /*   280 */    70,   60,   59,   72,   73,   71,   61,   62,   63,   64,
 /*   290 */    67,   68,   65,   66,   69,   70,   54,  286,  288,  236,
 /*   300 */   258,  150,   21,  151,  195,    6,   57,  200,  202,    9,
 /*   310 */   152,  212,  203,  206,  208,  293,   83,   23,  237,  103,
 /*   320 */   174,  153,  175,  154,  173,   33,  332,  219,  155,   75,
 /*   330 */   124,   94,  239,  236,   48,  240,  105,  171,   52,   23,
 /*   340 */    40,  236,  258,  305,  300,   93,  283,  238,   57,  308,
 /*   350 */   201,  196,  284,  167,   99,   41,   82,  241,  242,  243,
 /*   360 */   172,  176,   84,  195,    6,  326,  220,  202,    9,  251,
 /*   370 */   182,  218,  206,  208,  324,   60,   59,   72,   73,   71,
 /*   380 */    61,   62,   63,   64,   67,   68,   65,   66,   69,   70,
 /*   390 */    60,   59,   72,   73,   71,   61,   62,   63,   64,   67,
 /*   400 */    68,   65,   66,   69,   70,  332,  219,  156,   41,   82,
 /*   410 */    41,   82,  236,   48,  157,   86,  259,  260,  261,  262,
 /*   420 */   263,  264,  255,  228,  256,   41,   82,  135,   41,   82,
 /*   430 */   133,  259,  260,  261,  262,  263,  264,  134,  232,  257,
 /*   440 */   330,  220,  138,  193,  223,   52,  111,  102,  236,  258,
 /*   450 */   325,  236,   53,  324,   33,   57,   27,  207,  196,  284,
 /*   460 */   265,  108,  216,  236,  258,  107,  318,  215,  120,  145,
 /*   470 */    56,  223,  305,  300,   93,  316,  112,  317,  236,   53,
 /*   480 */   165,   88,  278,  279,  217,  174,  221,  175,  113,  173,
 /*   490 */    96,  222,  186,  334,   75,   41,   82,  239,   24,  189,
 /*   500 */   240,  114,  136,  120,  144,   40,  120,  142,  115,  139,
 /*   510 */   227,  181,  238,   98,  100,  236,   55,  116,  335,  117,
 /*   520 */   334,   24,  241,  242,  243,  172,  176,   84,  331,  149,
 /*   530 */    54,   88,  158,  236,  258,  174,   98,  175,  118,  173,
 /*   540 */    57,  120,  178,   91,   75,  120,  146,  239,  252,  294,
 /*   550 */   240,   21,  136,  120,  147,   40,  253,   52,  229,  337,
 /*   560 */   236,  258,  180,  188,  100,  120,  148,   57,  122,  209,
 /*   570 */   196,  284,  241,  242,  243,  172,  176,   84,  289,  197,
 /*   580 */   123,   83,  331,  163,  271,  174,  124,  175,  125,  173,
 /*   590 */   295,  288,  126,  227,   75,   69,   70,  239,  236,   55,
 /*   600 */   240,   45,  171,  127,   52,   40,  254,  236,  258,  339,
 /*   610 */   220,  128,  238,   37,   57,   46,  211,  196,  284,  119,
 /*   620 */   129,  130,  241,  242,  243,  172,  176,   84,   47,   19,
 /*   630 */   234,   83,  315,  235,  267,  174,  270,  175,  160,  173,
 /*   640 */   273,  287,  338,  230,   75,  307,  231,  239,  168,  170,
 /*   650 */   240,  233,  171,  183,   52,   40,  250,  236,  258,  177,
 /*   660 */    38,   39,  238,   74,   57,   76,  213,  196,  284,   78,
 /*   670 */    82,   80,  241,  242,  243,  172,  176,   84,   36,   42,
 /*   680 */   159,   83,   87,  266,  185,  174,  106,  175,   89,  173,
 /*   690 */   272,  190,  269,  274,   75,    4,  275,  239,  276,    2,
 /*   700 */   240,    5,  171,   52,   20,   40,  236,  258,   90,    8,
 /*   710 */    22,  516,  238,   57,  290,   30,    7,  285,   35,  291,
 /*   720 */   292,   10,  241,  242,  243,  172,  176,   84,   11,  296,
 /*   730 */    31,   83,  297,  298,   18,  174,  299,  175,   24,  173,
 /*   740 */   302,  301,  304,  303,   75,   17,   25,  239,  323,   26,
 /*   750 */   240,   29,  137,  265,  327,   40,  236,  258,  333,  485,
 /*   760 */   516,  486,  238,   56,  336,   28,  516,  340,  516,  516,
 /*   770 */   516,  516,  241,  242,  243,  172,  176,   84,   60,   59,
 /*   780 */    72,   73,   71,   61,   62,   63,   64,   67,   68,   65,
 /*   790 */    66,   69,   70,   60,   59,   72,   73,   71,   61,   62,
 /*   800 */    63,   64,   67,   68,   65,   66,   69,   70,  516,  244,
 /*   810 */   516,  516,  245,  516,  516,   60,   59,   72,   73,   71,
 /*   820 */    61,   62,   63,   64,   67,   68,   65,   66,   69,   70,
 /*   830 */   516,  516,  516,   77,  516,  516,   60,   59,   72,   73,
 /*   840 */    71,   61,   62,   63,   64,   67,   68,   65,   66,   69,
 /*   850 */    70,   60,   59,   72,   73,   71,   61,   62,   63,   64,
 /*   860 */    67,   68,   65,   66,   69,   70,  516,  246,  516,   79,
 /*   870 */    60,   59,   72,   73,   71,   61,   62,   63,   64,   67,
 /*   880 */    68,   65,   66,   69,   70,   60,   59,   72,   73,   71,
 /*   890 */    61,   62,   63,   64,   67,   68,   65,   66,   69,   70,
 /*   900 */   516,  247,  516,   81,   60,   59,   72,   73,   71,   61,
 /*   910 */    62,   63,   64,   67,   68,   65,   66,   69,   70,   60,
 /*   920 */    59,   72,   73,   71,   61,   62,   63,   64,   67,   68,
 /*   930 */    65,   66,   69,   70,  516,  248,  109,  309,  281,  236,
 /*   940 */   258,  516,  280,  194,  516,  516,   32,  265,  328,  516,
 /*   950 */   236,  258,  516,  313,  516,  516,  312,   56,  516,  516,
 /*   960 */   516,  516,  516,  516,  305,  300,   93,  310,  311,  516,
 /*   970 */    97,  109,  309,  281,  236,  258,  516,  314,  194,  516,
 /*   980 */   516,   32,  516,  516,  516,  516,  516,  516,  313,  516,
 /*   990 */   516,  312,  516,  516,  516,  516,  516,  516,  516,  305,
 /*  1000 */   300,   93,  310,  311,   60,   59,   72,   73,   71,   61,
 /*  1010 */    62,   63,   64,   67,   68,   65,   66,   69,   70,   59,
 /*  1020 */    72,   73,   71,   61,   62,   63,   64,   67,   68,   65,
 /*  1030 */    66,   69,   70,  109,  309,  282,  236,  258,  516,  516,
 /*  1040 */   516,  516,  516,   32,  516,  516,  516,  516,  516,  516,
 /*  1050 */   313,  516,  516,  312,  516,  516,  516,  516,  516,  516,
 /*  1060 */   516,  305,  300,   93,  310,  311,   72,   73,   71,   61,
 /*  1070 */    62,   63,   64,   67,   68,   65,   66,   69,   70,   73,
 /*  1080 */    71,   61,   62,   63,   64,   67,   68,   65,   66,   69,
 /*  1090 */    70,   71,   61,   62,   63,   64,   67,   68,   65,   66,
 /*  1100 */    69,   70,  477,  477,  477,  516,  516,  476,  476,  476,
 /*  1110 */   516,  516,  477,  516,   43,  516,  516,  476,  516,   44,
 /*  1120 */   477,  516,  477,  516,  516,  476,  516,  476,  516,  516,
 /*  1130 */   516,  516,  516,  516,  516,  477,  516,  516,  516,  477,
 /*  1140 */   476,  516,  516,  477,  476,  516,  516,  516,  476,  516,
 /*  1150 */   516,  516,  516,  516,  516,  516,  475,  475,  475,  516,
 /*  1160 */   516,  474,  474,  474,  516,  516,  475,  516,  516,  516,
 /*  1170 */   516,  474,  516,  516,  475,  516,  475,  516,  516,  474,
 /*  1180 */   516,  474,  516,  516,  516,  516,  516,  516,  516,  475,
 /*  1190 */   516,  516,  516,  475,  474,  516,  516,  475,  474,  516,
 /*  1200 */   516,  516,  474,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */     0,   76,   77,   76,   79,   80,   79,   80,    8,  110,
 /*    10 */   111,   86,   12,   86,   14,   38,   16,    8,   91,   92,
 /*    20 */    38,   21,   95,   23,   24,   25,   26,   27,   28,   29,
 /*    30 */    53,   31,   32,   38,   34,   35,   36,   37,   29,   39,
 /*    40 */    40,    9,   10,   11,   12,   13,   14,   15,   16,   49,
 /*    50 */    50,   51,   52,   53,   54,   55,   56,   56,   58,   59,
 /*    60 */    60,   60,   62,   63,   64,   65,   66,   67,   68,    2,
 /*    70 */     3,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*    80 */    13,   14,   15,   16,    2,    3,    4,    5,    6,    7,
 /*    90 */     8,    9,   10,   11,   12,   13,   14,   15,   16,    7,
 /*   100 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   42,
 /*   110 */    43,   44,   45,   46,   47,   48,  105,  106,    8,   10,
 /*   120 */    38,   81,   55,   56,   74,   58,   59,   60,   81,   79,
 /*   130 */    80,   64,   65,   76,    8,   53,   79,   80,   12,   29,
 /*   140 */    14,   32,   16,   86,   79,  105,  106,   21,   91,   92,
 /*   150 */    24,   41,   95,   27,   21,   29,   76,   86,   32,   79,
 /*   160 */    80,   10,   97,   98,   99,   39,   86,   41,   88,   89,
 /*   170 */    90,   38,  122,  123,   20,   49,   50,   51,   52,   53,
 /*   180 */    54,   55,   56,   32,   58,   59,   60,   33,   62,   63,
 /*   190 */    64,   65,   76,    8,   86,   79,   80,   12,   86,   14,
 /*   200 */    86,   16,   86,   79,   86,   87,   21,   91,   92,   24,
 /*   210 */    79,   95,   27,  105,   29,   76,   56,   32,   79,   80,
 /*   220 */    60,   97,   98,   99,   39,   86,   86,   88,   89,   90,
 /*   230 */   102,  103,   19,   20,   49,   50,   51,   52,   53,   54,
 /*   240 */    55,   56,   86,   58,   59,   60,   33,   62,   63,   64,
 /*   250 */    65,    2,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   260 */    11,   12,   13,   14,   15,   16,    2,    3,    4,    5,
 /*   270 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   280 */    16,    2,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   290 */    11,   12,   13,   14,   15,   16,   76,  100,  101,   79,
 /*   300 */    80,   86,   38,   86,   55,   56,   86,   58,   59,   60,
 /*   310 */    86,   91,   92,   64,   65,   95,    8,   38,   79,   79,
 /*   320 */    12,   86,   14,   86,   16,   86,   72,   73,   86,   21,
 /*   330 */    86,   30,   24,   79,   80,   27,   79,   29,   76,   38,
 /*   340 */    32,   79,   80,  104,  105,  106,  107,   39,   86,  105,
 /*   350 */    88,   89,   90,  113,   53,   19,   20,   49,   50,   51,
 /*   360 */    52,   53,   54,   55,   56,  102,  103,   59,   60,   33,
 /*   370 */   113,  117,   64,   65,  120,    2,    3,    4,    5,    6,
 /*   380 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   390 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   400 */    12,   13,   14,   15,   16,   72,   73,   86,   19,   20,
 /*   410 */    19,   20,   79,   80,   86,   42,   43,   44,   45,   46,
 /*   420 */    47,   48,   33,   27,   33,   19,   20,   86,   19,   20,
 /*   430 */    86,   43,   44,   45,   46,   47,   48,   86,   70,   33,
 /*   440 */   102,  103,   33,   75,   72,   76,   86,   79,   79,   80,
 /*   450 */   117,   79,   80,  120,   86,   86,   60,   88,   89,   90,
 /*   460 */    76,   77,   94,   79,   80,   79,   98,   99,   86,   87,
 /*   470 */    86,   72,  104,  105,  106,  107,   86,  109,   79,   80,
 /*   480 */   112,    8,  114,  115,  116,   12,  118,   14,   86,   16,
 /*   490 */    30,  119,  113,  121,   21,   19,   20,   24,   38,  113,
 /*   500 */    27,   86,   29,   86,   87,   32,   86,   87,   86,   33,
 /*   510 */    74,    8,   39,   53,   41,   79,   80,   86,  119,   86,
 /*   520 */   121,   38,   49,   50,   51,   52,   53,   54,  102,  103,
 /*   530 */    76,    8,   29,   79,   80,   12,   53,   14,   86,   16,
 /*   540 */    86,   86,   87,   30,   21,   86,   87,   24,   86,   95,
 /*   550 */    27,   38,   29,   86,   87,   32,   86,   76,  122,  123,
 /*   560 */    79,   80,   39,    8,   41,   86,   87,   86,   86,   88,
 /*   570 */    89,   90,   49,   50,   51,   52,   53,   54,  100,  101,
 /*   580 */    86,    8,  102,  103,   29,   12,   86,   14,   86,   16,
 /*   590 */   100,  101,   86,   74,   21,   15,   16,   24,   79,   80,
 /*   600 */    27,   86,   29,   86,   76,   32,   33,   79,   80,  102,
 /*   610 */   103,   86,   39,   86,   86,   86,   88,   89,   90,   86,
 /*   620 */    86,   86,   49,   50,   51,   52,   53,   54,   86,   38,
 /*   630 */    84,    8,  101,   84,   84,   12,   39,   14,   71,   16,
 /*   640 */    84,  101,  123,   22,   21,  101,   24,   24,   10,   29,
 /*   650 */    27,   27,   29,   10,   76,   32,   33,   79,   80,   29,
 /*   660 */    32,   32,   39,   32,   86,   32,   88,   89,   90,   32,
 /*   670 */    20,   32,   49,   50,   51,   52,   53,   54,   32,   32,
 /*   680 */    29,    8,   30,   27,   29,   12,   32,   14,   30,   16,
 /*   690 */    29,   10,   39,   27,   21,   20,   29,   24,   29,   40,
 /*   700 */    27,   56,   29,   76,   20,   32,   79,   80,   30,   60,
 /*   710 */    20,  124,   39,   86,   57,   21,   56,   90,   30,   57,
 /*   720 */    57,   56,   49,   50,   51,   52,   53,   54,   56,   61,
 /*   730 */    21,    8,   61,   57,   20,   12,   57,   14,   38,   16,
 /*   740 */    57,   61,   57,   61,   21,   56,   60,   24,   57,   20,
 /*   750 */    27,   20,   29,   76,   77,   32,   79,   80,   61,   60,
 /*   760 */   124,   60,   39,   86,   61,   60,  124,   61,  124,  124,
 /*   770 */   124,  124,   49,   50,   51,   52,   53,   54,    2,    3,
 /*   780 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   790 */    14,   15,   16,    2,    3,    4,    5,    6,    7,    8,
 /*   800 */     9,   10,   11,   12,   13,   14,   15,   16,  124,   33,
 /*   810 */   124,  124,   21,  124,  124,    2,    3,    4,    5,    6,
 /*   820 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   830 */   124,  124,  124,   20,  124,  124,    2,    3,    4,    5,
 /*   840 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   850 */    16,    2,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   860 */    11,   12,   13,   14,   15,   16,  124,   33,  124,   20,
 /*   870 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   880 */    12,   13,   14,   15,   16,    2,    3,    4,    5,    6,
 /*   890 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   900 */   124,   33,  124,   20,    2,    3,    4,    5,    6,    7,
 /*   910 */     8,    9,   10,   11,   12,   13,   14,   15,   16,    2,
 /*   920 */     3,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   930 */    13,   14,   15,   16,  124,   33,   76,   77,   78,   79,
 /*   940 */    80,  124,   82,   83,  124,  124,   86,   76,   77,  124,
 /*   950 */    79,   80,  124,   93,  124,  124,   96,   86,  124,  124,
 /*   960 */   124,  124,  124,  124,  104,  105,  106,  107,  108,  124,
 /*   970 */    53,   76,   77,   78,   79,   80,  124,   82,   83,  124,
 /*   980 */   124,   86,  124,  124,  124,  124,  124,  124,   93,  124,
 /*   990 */   124,   96,  124,  124,  124,  124,  124,  124,  124,  104,
 /*  1000 */   105,  106,  107,  108,    2,    3,    4,    5,    6,    7,
 /*  1010 */     8,    9,   10,   11,   12,   13,   14,   15,   16,    3,
 /*  1020 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*  1030 */    14,   15,   16,   76,   77,   78,   79,   80,  124,  124,
 /*  1040 */   124,  124,  124,   86,  124,  124,  124,  124,  124,  124,
 /*  1050 */    93,  124,  124,   96,  124,  124,  124,  124,  124,  124,
 /*  1060 */   124,  104,  105,  106,  107,  108,    4,    5,    6,    7,
 /*  1070 */     8,    9,   10,   11,   12,   13,   14,   15,   16,    5,
 /*  1080 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*  1090 */    16,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*  1100 */    15,   16,   20,   21,   22,  124,  124,   20,   21,   22,
 /*  1110 */   124,  124,   30,  124,   32,  124,  124,   30,  124,   32,
 /*  1120 */    38,  124,   40,  124,  124,   38,  124,   40,  124,  124,
 /*  1130 */   124,  124,  124,  124,  124,   53,  124,  124,  124,   57,
 /*  1140 */    53,  124,  124,   61,   57,  124,  124,  124,   61,  124,
 /*  1150 */   124,  124,  124,  124,  124,  124,   20,   21,   22,  124,
 /*  1160 */   124,   20,   21,   22,  124,  124,   30,  124,  124,  124,
 /*  1170 */   124,   30,  124,  124,   38,  124,   40,  124,  124,   38,
 /*  1180 */   124,   40,  124,  124,  124,  124,  124,  124,  124,   53,
 /*  1190 */   124,  124,  124,   57,   53,  124,  124,   61,   57,  124,
 /*  1200 */   124,  124,   61,
};
#define YY_SHIFT_USE_DFLT (-24)
#define YY_SHIFT_MAX 229
static const short yy_shift_ofst[] = {
 /*     0 */   -24,    0,  126,  126,  126,  473,  473,  473,  473,  473,
 /*    10 */   473,  473,  473,  473,  473,  473,  185,  110,  110,  523,
 /*    20 */   473,  523,  473,  523,  523,  110,  110,  110,  110,  110,
 /*    30 */     9,    9,   67,  249,  308,  308,  573,   82,  673,  673,
 /*    40 */   673,  673,  673,  673,  673,  264,  279,  279,  301,  503,
 /*    50 */   503,  503,  513,  -23,  -18,   -5,  373,  388,  623,  673,
 /*    60 */   673,  673,  673,  673,  673,  673,  673,  673,  673,  673,
 /*    70 */   673,  673,  673,  673,  673,  673,  673,  673,  673,  673,
 /*    80 */   673,  673,  673,  673,  673,  673,  673,  673,  723,  673,
 /*    90 */   673,  673,  673,  673,  673,  673,  673,  673,  673,  673,
 /*   100 */     9,  555,  133,  591,  591,  591,  597,  591,  -18,  -18,
 /*   110 */   -18,  776,  791,  813,  834,  849,  868,  883,  902,  917,
 /*   120 */  1002, 1002, 1002, 1002, 1002, 1002, 1002, 1002, 1002, 1002,
 /*   130 */  1002, 1016, 1062, 1074, 1085,   92, 1082, 1087, 1136, 1141,
 /*   140 */    32,   32,  213,  336,  389,  391,  406,  409,  476,  460,
 /*   150 */   580,  580,  580,  580,  580,  580,  580,  580,  109,  151,
 /*   160 */   154,    1,  160,  483,  396,  621,  622,  638,  624,  620,
 /*   170 */   628,  629,  631,  633,  637,  639,  630,  646,  650,  647,
 /*   180 */   652,  651,  643,  656,  655,  658,  654,  653,  661,  681,
 /*   190 */   666,  667,  669,  659,  675,  645,  684,  678,  657,  662,
 /*   200 */   660,  663,  649,  690,  668,  671,  665,  676,  672,  679,
 /*   210 */   680,  683,  682,  685,  688,  694,  709,  689,  691,  714,
 /*   220 */   700,  686,  697,  729,  699,  701,  703,  731,  705,  706,
};
#define YY_REDUCE_USE_DFLT (-102)
#define YY_REDUCE_MAX 110
static const short yy_reduce_ofst[] = {
 /*     0 */  -101,  368,  860,  895,  957,   80,  139,  262,  -73,   57,
 /*    10 */   369,  481,  116,  528,  220,  578,  239,  254,  333,  -75,
 /*    20 */   627,  384,  454,  677,  871,  372,  399,   50,  436,  519,
 /*    30 */    65,  124,   40,   11,  108,  244,  118,  128,  382,  417,
 /*    40 */   420,  455,  459,  467,  479,  197,  263,  338,  426,  240,
 /*    50 */   257,  386,  478,  480,  490,  507,   47,   47,   71,  112,
 /*    60 */   114,  140,  156,  215,  217,  224,  235,  237,  242,  321,
 /*    70 */   328,  341,  344,  351,  360,  390,  402,  415,  422,  431,
 /*    80 */   433,  452,   71,  462,  470,  482,  494,  500,  462,  502,
 /*    90 */   506,  515,  517,  525,  527,  529,  533,  534,  535,  542,
 /*   100 */   131,  379,  531,  546,  549,  550,  567,  556,  540,  544,
 /*   110 */   531,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   342,  514,  375,  375,  514,  432,  432,  432,  446,  446,
 /*    10 */   432,  432,  446,  432,  446,  432,  514,  487,  487,  514,
 /*    20 */   514,  514,  514,  514,  514,  491,  491,  509,  509,  514,
 /*    30 */   514,  514,  514,  514,  514,  514,  514,  505,  514,  514,
 /*    40 */   514,  514,  514,  514,  514,  439,  505,  505,  505,  346,
 /*    50 */   347,  514,  439,  505,  439,  505,  514,  514,  514,  514,
 /*    60 */   514,  514,  514,  514,  514,  514,  514,  514,  514,  514,
 /*    70 */   514,  514,  514,  514,  514,  514,  514,  514,  514,  514,
 /*    80 */   514,  514,  514,  514,  514,  514,  514,  514,  514,  514,
 /*    90 */   514,  514,  472,  469,  514,  514,  514,  514,  514,  514,
 /*   100 */   514,  514,  370,  363,  363,  363,  514,  363,  437,  381,
 /*   110 */   458,  514,  514,  514,  514,  514,  514,  514,  514,  498,
 /*   120 */   478,  479,  380,  383,  382,  352,  434,  470,  471,  497,
 /*   130 */   501,  404,  403,  416,  417,  415,  397,  397,  423,  423,
 /*   140 */   405,  406,  514,  514,  514,  514,  514,  514,  514,  506,
 /*   150 */   407,  408,  409,  410,  411,  412,  413,  414,  477,  476,
 /*   160 */   514,  514,  514,  506,  514,  514,  514,  514,  514,  514,
 /*   170 */   476,  397,  514,  514,  514,  514,  514,  514,  480,  477,
 /*   180 */   396,  514,  514,  514,  514,  514,  514,  514,  514,  514,
 /*   190 */   514,  514,  514,  369,  376,  514,  433,  440,  514,  514,
 /*   200 */   514,  514,  514,  447,  514,  514,  514,  514,  514,  514,
 /*   210 */   514,  514,  514,  514,  396,  514,  465,  514,  514,  489,
 /*   220 */   506,  514,  514,  493,  483,  484,  514,  511,  514,  514,
 /*   230 */   343,  344,  345,  348,  349,  364,  377,  378,  396,  398,
 /*   240 */   399,  400,  401,  402,  418,  419,  420,  421,  422,  424,
 /*   250 */   425,  426,  428,  429,  427,  423,  474,  475,  379,  390,
 /*   260 */   391,  392,  393,  394,  395,  381,  350,  351,  353,  366,
 /*   270 */   365,  361,  362,  354,  355,  356,  357,  358,  359,  360,
 /*   280 */   367,  373,  374,  386,  430,  431,  435,  438,  440,  436,
 /*   290 */   441,  442,  443,  444,  445,  448,  449,  450,  466,  467,
 /*   300 */   468,  451,  453,  452,  454,  473,  388,  455,  387,  384,
 /*   310 */   385,  389,  456,  457,  368,  459,  371,  372,  462,  460,
 /*   320 */   461,  463,  464,  481,  488,  490,  495,  503,  504,  496,
 /*   330 */   500,  502,  499,  482,  492,  494,  507,  510,  512,  513,
 /*   340 */   508,
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
  parserARG_SDECL                /* A place to hold %extra_argument */
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
void parserTrace(FILE *TraceFILE, char *zTracePrompt){
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
  "$",             "Rule",          "SEM",           "DOTS",        
  "XOR",           "QUESTION",      "AND",           "PLUS",        
  "MINUS",         "MULT",          "SLASH",         "MOD",         
  "PMOD",          "DIV",           "PDIV",          "POW",         
  "PPOW",          "UMINUS",        "UBNOT",         "DSEM",        
  "COMMA",         "VBAR",          "DOT",           "INCLUDE",     
  "STRING",        "HIDE",          "SHOW",          "NUMBER",      
  "CONST",         "IDENTIFIER",    "ASSIGN",        "DOMAIN",      
  "LBRAC",         "RBRAC",         "EXTERNAL",      "CUMULATIVE",  
  "VOLATILE",      "BASE",          "COLON",         "VARIABLE",    
  "IF",            "NOT",           "CASSIGN",       "GREATER",     
  "LOWER",         "GTHAN",         "LTHAN",         "EQUAL",       
  "INEQUAL",       "ANONYMOUS",     "INFIMUM",       "SUPREMUM",    
  "PABS",          "AT",            "BNOT",          "SUM",         
  "LSBRAC",        "RSBRAC",        "AVG",           "COUNT",       
  "LCBRAC",        "RCBRAC",        "EVEN",          "ODD",         
  "MIN",           "MAX",           "MINIMIZE",      "MAXIMIZE",    
  "COMPUTE",       "error",         "rule",          "var_list",    
  "priolit",       "weightedpriolit",  "computelit",    "head",        
  "lit",           "literal",       "body_literal",  "predicate",   
  "predlit",       "cmp",           "body",          "nbody",       
  "cond",          "op",            "term",          "termlist",    
  "weightlist",    "nweightlist",   "weightlit",     "condlist",    
  "ncondlist",     "ccondlist",     "head_ccondlist",  "condlit",     
  "ccondlit",      "head_ccondlit",  "head_ccondlit_cond",  "head_ccondlit_nocond",
  "weightcond",    "nweightcond",   "priolit_cond",  "npriolit_cond",
  "aggr",          "aggr_ass",      "aggr_num",      "aggr_atom",   
  "conjunction",   "disjunction",   "start",         "program",     
  "line",          "signed",        "optimize",      "compute",     
  "soptimize",     "prio_list",     "soptimize_set",  "prio_set",    
  "nprio_list",    "nprio_set",     "compute_list",  "ncompute_list",
};
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
 /*   0 */ "start ::= program",
 /*   1 */ "program ::=",
 /*   2 */ "program ::= program line DOT",
 /*   3 */ "line ::= INCLUDE STRING",
 /*   4 */ "line ::= rule",
 /*   5 */ "line ::= HIDE",
 /*   6 */ "line ::= SHOW",
 /*   7 */ "line ::= HIDE signed SLASH NUMBER",
 /*   8 */ "line ::= HIDE predicate cond",
 /*   9 */ "line ::= SHOW signed SLASH NUMBER",
 /*  10 */ "line ::= SHOW predicate cond",
 /*  11 */ "line ::= CONST IDENTIFIER ASSIGN term",
 /*  12 */ "line ::= DOMAIN signed LBRAC var_list RBRAC",
 /*  13 */ "line ::= EXTERNAL predicate cond",
 /*  14 */ "line ::= EXTERNAL signed SLASH NUMBER",
 /*  15 */ "line ::= CUMULATIVE IDENTIFIER",
 /*  16 */ "line ::= VOLATILE IDENTIFIER",
 /*  17 */ "line ::= BASE",
 /*  18 */ "line ::= optimize",
 /*  19 */ "line ::= compute",
 /*  20 */ "signed ::= IDENTIFIER",
 /*  21 */ "signed ::= MINUS IDENTIFIER",
 /*  22 */ "cond ::=",
 /*  23 */ "cond ::= COLON literal cond",
 /*  24 */ "var_list ::= VARIABLE",
 /*  25 */ "var_list ::= var_list COMMA VARIABLE",
 /*  26 */ "rule ::= head IF body",
 /*  27 */ "rule ::= IF body",
 /*  28 */ "rule ::= head",
 /*  29 */ "head ::= predicate",
 /*  30 */ "head ::= aggr_atom",
 /*  31 */ "head ::= disjunction",
 /*  32 */ "nbody ::= body_literal",
 /*  33 */ "nbody ::= nbody COMMA body_literal",
 /*  34 */ "body ::=",
 /*  35 */ "body ::= nbody",
 /*  36 */ "predlit ::= predicate",
 /*  37 */ "predlit ::= NOT predicate",
 /*  38 */ "lit ::= predlit",
 /*  39 */ "lit ::= term cmp term",
 /*  40 */ "literal ::= lit",
 /*  41 */ "literal ::= VARIABLE ASSIGN term",
 /*  42 */ "literal ::= term CASSIGN term",
 /*  43 */ "body_literal ::= literal",
 /*  44 */ "body_literal ::= aggr_atom",
 /*  45 */ "body_literal ::= NOT aggr_atom",
 /*  46 */ "body_literal ::= VARIABLE ASSIGN aggr_ass",
 /*  47 */ "body_literal ::= term CASSIGN aggr_ass",
 /*  48 */ "body_literal ::= conjunction",
 /*  49 */ "cmp ::= GREATER",
 /*  50 */ "cmp ::= LOWER",
 /*  51 */ "cmp ::= GTHAN",
 /*  52 */ "cmp ::= LTHAN",
 /*  53 */ "cmp ::= EQUAL",
 /*  54 */ "cmp ::= INEQUAL",
 /*  55 */ "term ::= VARIABLE",
 /*  56 */ "term ::= IDENTIFIER",
 /*  57 */ "term ::= STRING",
 /*  58 */ "term ::= NUMBER",
 /*  59 */ "term ::= ANONYMOUS",
 /*  60 */ "term ::= INFIMUM",
 /*  61 */ "term ::= SUPREMUM",
 /*  62 */ "term ::= term DOTS term",
 /*  63 */ "term ::= term SEM term",
 /*  64 */ "term ::= term PLUS term",
 /*  65 */ "term ::= term MINUS term",
 /*  66 */ "term ::= term MULT term",
 /*  67 */ "term ::= term SLASH term",
 /*  68 */ "term ::= term DIV term",
 /*  69 */ "term ::= term PDIV term",
 /*  70 */ "term ::= term MOD term",
 /*  71 */ "term ::= term PMOD term",
 /*  72 */ "term ::= term POW term",
 /*  73 */ "term ::= term PPOW term",
 /*  74 */ "term ::= term AND term",
 /*  75 */ "term ::= term XOR term",
 /*  76 */ "term ::= term QUESTION term",
 /*  77 */ "term ::= PABS LBRAC term RBRAC",
 /*  78 */ "term ::= VBAR term VBAR",
 /*  79 */ "term ::= PPOW LBRAC term COMMA term RBRAC",
 /*  80 */ "term ::= PMOD LBRAC term COMMA term RBRAC",
 /*  81 */ "term ::= PDIV LBRAC term COMMA term RBRAC",
 /*  82 */ "term ::= IDENTIFIER LBRAC termlist RBRAC",
 /*  83 */ "term ::= LBRAC termlist RBRAC",
 /*  84 */ "term ::= LBRAC termlist COMMA RBRAC",
 /*  85 */ "term ::= AT IDENTIFIER LBRAC termlist RBRAC",
 /*  86 */ "term ::= AT IDENTIFIER LBRAC RBRAC",
 /*  87 */ "term ::= MINUS term",
 /*  88 */ "term ::= BNOT term",
 /*  89 */ "nweightlist ::= weightlit",
 /*  90 */ "nweightlist ::= nweightlist COMMA weightlit",
 /*  91 */ "weightlist ::=",
 /*  92 */ "weightlist ::= nweightlist",
 /*  93 */ "weightlit ::= lit nweightcond ASSIGN term",
 /*  94 */ "weightlit ::= lit ASSIGN term weightcond",
 /*  95 */ "weightlit ::= lit weightcond",
 /*  96 */ "nweightcond ::= COLON literal",
 /*  97 */ "nweightcond ::= COLON literal nweightcond",
 /*  98 */ "weightcond ::=",
 /*  99 */ "weightcond ::= nweightcond",
 /* 100 */ "aggr_ass ::= SUM LSBRAC weightlist RSBRAC",
 /* 101 */ "aggr_ass ::= LSBRAC weightlist RSBRAC",
 /* 102 */ "aggr_num ::= AVG LSBRAC weightlist RSBRAC",
 /* 103 */ "ncondlist ::= condlit",
 /* 104 */ "ncondlist ::= ncondlist COMMA condlit",
 /* 105 */ "condlist ::=",
 /* 106 */ "condlist ::= ncondlist",
 /* 107 */ "condlit ::= lit weightcond",
 /* 108 */ "aggr_ass ::= COUNT LCBRAC condlist RCBRAC",
 /* 109 */ "aggr_ass ::= LCBRAC condlist RCBRAC",
 /* 110 */ "aggr ::= EVEN LCBRAC condlist RCBRAC",
 /* 111 */ "aggr ::= ODD LCBRAC condlist RCBRAC",
 /* 112 */ "aggr ::= EVEN LSBRAC weightlist RSBRAC",
 /* 113 */ "aggr ::= ODD LSBRAC weightlist RSBRAC",
 /* 114 */ "ccondlit ::= lit nweightcond",
 /* 115 */ "ccondlist ::= ccondlit",
 /* 116 */ "conjunction ::= ccondlist",
 /* 117 */ "head_ccondlit_nocond ::= predicate",
 /* 118 */ "head_ccondlit_cond ::= predicate nweightcond",
 /* 119 */ "head_ccondlit ::= head_ccondlit_nocond",
 /* 120 */ "head_ccondlit ::= head_ccondlit_cond",
 /* 121 */ "head_ccondlist ::= head_ccondlit_cond",
 /* 122 */ "head_ccondlist ::= head_ccondlit_nocond VBAR head_ccondlit",
 /* 123 */ "head_ccondlist ::= head_ccondlist VBAR head_ccondlit",
 /* 124 */ "disjunction ::= head_ccondlist",
 /* 125 */ "aggr_ass ::= MIN LSBRAC weightlist RSBRAC",
 /* 126 */ "aggr_ass ::= MAX LSBRAC weightlist RSBRAC",
 /* 127 */ "aggr_num ::= aggr_ass",
 /* 128 */ "aggr ::= aggr_num",
 /* 129 */ "aggr_atom ::= term aggr_num term",
 /* 130 */ "aggr_atom ::= aggr_num term",
 /* 131 */ "aggr_atom ::= term aggr_num",
 /* 132 */ "aggr_atom ::= aggr",
 /* 133 */ "predicate ::= MINUS IDENTIFIER LBRAC termlist RBRAC",
 /* 134 */ "predicate ::= IDENTIFIER LBRAC termlist RBRAC",
 /* 135 */ "predicate ::= MINUS IDENTIFIER",
 /* 136 */ "predicate ::= IDENTIFIER",
 /* 137 */ "termlist ::= term",
 /* 138 */ "termlist ::= termlist COMMA term",
 /* 139 */ "termlist ::= termlist DSEM termlist",
 /* 140 */ "optimize ::= soptimize LSBRAC prio_list RSBRAC",
 /* 141 */ "optimize ::= soptimize_set LCBRAC prio_set RCBRAC",
 /* 142 */ "soptimize ::= MINIMIZE",
 /* 143 */ "soptimize ::= MAXIMIZE",
 /* 144 */ "soptimize_set ::= MINIMIZE",
 /* 145 */ "soptimize_set ::= MAXIMIZE",
 /* 146 */ "prio_list ::=",
 /* 147 */ "prio_list ::= nprio_list",
 /* 148 */ "nprio_list ::= weightedpriolit",
 /* 149 */ "nprio_list ::= weightedpriolit COMMA prio_list",
 /* 150 */ "prio_set ::=",
 /* 151 */ "prio_set ::= nprio_set",
 /* 152 */ "nprio_set ::= priolit",
 /* 153 */ "nprio_set ::= priolit COMMA prio_set",
 /* 154 */ "weightedpriolit ::= predlit ASSIGN term AT term priolit_cond",
 /* 155 */ "weightedpriolit ::= predlit ASSIGN term priolit_cond",
 /* 156 */ "weightedpriolit ::= predlit npriolit_cond ASSIGN term AT term",
 /* 157 */ "weightedpriolit ::= predlit npriolit_cond ASSIGN term",
 /* 158 */ "weightedpriolit ::= priolit",
 /* 159 */ "priolit ::= predlit AT term priolit_cond",
 /* 160 */ "priolit ::= predlit npriolit_cond AT term",
 /* 161 */ "priolit ::= predlit priolit_cond",
 /* 162 */ "npriolit_cond ::= COLON literal",
 /* 163 */ "npriolit_cond ::= npriolit_cond COLON literal",
 /* 164 */ "priolit_cond ::=",
 /* 165 */ "priolit_cond ::= npriolit_cond",
 /* 166 */ "compute ::= COMPUTE LCBRAC compute_list RCBRAC",
 /* 167 */ "compute ::= COMPUTE NUMBER LCBRAC compute_list RCBRAC",
 /* 168 */ "compute_list ::=",
 /* 169 */ "compute_list ::= ncompute_list",
 /* 170 */ "ncompute_list ::= computelit",
 /* 171 */ "ncompute_list ::= computelit COMMA ncompute_list",
 /* 172 */ "computelit ::= predlit priolit_cond",
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
** to parser and parserFree.
*/
void *parserAlloc(void *(*mallocProc)(size_t)){
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
  parserARG_FETCH;
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
    case 1: /* Rule */
    case 2: /* SEM */
    case 3: /* DOTS */
    case 4: /* XOR */
    case 5: /* QUESTION */
    case 6: /* AND */
    case 7: /* PLUS */
    case 8: /* MINUS */
    case 9: /* MULT */
    case 10: /* SLASH */
    case 11: /* MOD */
    case 12: /* PMOD */
    case 13: /* DIV */
    case 14: /* PDIV */
    case 15: /* POW */
    case 16: /* PPOW */
    case 17: /* UMINUS */
    case 18: /* UBNOT */
    case 19: /* DSEM */
    case 20: /* COMMA */
    case 21: /* VBAR */
    case 22: /* DOT */
    case 23: /* INCLUDE */
    case 24: /* STRING */
    case 25: /* HIDE */
    case 26: /* SHOW */
    case 27: /* NUMBER */
    case 28: /* CONST */
    case 29: /* IDENTIFIER */
    case 30: /* ASSIGN */
    case 31: /* DOMAIN */
    case 32: /* LBRAC */
    case 33: /* RBRAC */
    case 34: /* EXTERNAL */
    case 35: /* CUMULATIVE */
    case 36: /* VOLATILE */
    case 37: /* BASE */
    case 38: /* COLON */
    case 39: /* VARIABLE */
    case 40: /* IF */
    case 41: /* NOT */
    case 42: /* CASSIGN */
    case 43: /* GREATER */
    case 44: /* LOWER */
    case 45: /* GTHAN */
    case 46: /* LTHAN */
    case 47: /* EQUAL */
    case 48: /* INEQUAL */
    case 49: /* ANONYMOUS */
    case 50: /* INFIMUM */
    case 51: /* SUPREMUM */
    case 52: /* PABS */
    case 53: /* AT */
    case 54: /* BNOT */
    case 55: /* SUM */
    case 56: /* LSBRAC */
    case 57: /* RSBRAC */
    case 58: /* AVG */
    case 59: /* COUNT */
    case 60: /* LCBRAC */
    case 61: /* RCBRAC */
    case 62: /* EVEN */
    case 63: /* ODD */
    case 64: /* MIN */
    case 65: /* MAX */
    case 66: /* MINIMIZE */
    case 67: /* MAXIMIZE */
    case 68: /* COMPUTE */
{
#line 80 "parser_impl.y"
 pParser = pParser; 
#line 989 "parser_impl.c"
}
      break;
    case 71: /* var_list */
{
#line 89 "parser_impl.y"
 del((yypminor->yy117)); 
#line 996 "parser_impl.c"
}
      break;
    case 72: /* priolit */
    case 73: /* weightedpriolit */
{
#line 92 "parser_impl.y"
 del((yypminor->yy184)); 
#line 1004 "parser_impl.c"
}
      break;
    case 74: /* computelit */
{
#line 98 "parser_impl.y"
 del((yypminor->yy104)); 
#line 1011 "parser_impl.c"
}
      break;
    case 75: /* head */
    case 76: /* lit */
    case 77: /* literal */
    case 78: /* body_literal */
{
#line 104 "parser_impl.y"
 del((yypminor->yy212)); 
#line 1021 "parser_impl.c"
}
      break;
    case 79: /* predicate */
    case 80: /* predlit */
{
#line 112 "parser_impl.y"
 del((yypminor->yy155)); 
#line 1029 "parser_impl.c"
}
      break;
    case 82: /* body */
    case 83: /* nbody */
    case 84: /* cond */
    case 100: /* weightcond */
    case 101: /* nweightcond */
    case 102: /* priolit_cond */
    case 103: /* npriolit_cond */
{
#line 119 "parser_impl.y"
 del((yypminor->yy52)); 
#line 1042 "parser_impl.c"
}
      break;
    case 85: /* op */
{
#line 124 "parser_impl.y"
 del((yypminor->yy153)); 
#line 1049 "parser_impl.c"
}
      break;
    case 86: /* term */
{
#line 127 "parser_impl.y"
 del((yypminor->yy83)); 
#line 1056 "parser_impl.c"
}
      break;
    case 87: /* termlist */
{
#line 130 "parser_impl.y"
 del((yypminor->yy243)); 
#line 1063 "parser_impl.c"
}
      break;
    case 88: /* weightlist */
    case 89: /* nweightlist */
    case 91: /* condlist */
    case 92: /* ncondlist */
    case 93: /* ccondlist */
    case 94: /* head_ccondlist */
{
#line 134 "parser_impl.y"
 del((yypminor->yy226)); 
#line 1075 "parser_impl.c"
}
      break;
    case 90: /* weightlit */
    case 95: /* condlit */
    case 96: /* ccondlit */
    case 97: /* head_ccondlit */
    case 98: /* head_ccondlit_cond */
    case 99: /* head_ccondlit_nocond */
{
#line 138 "parser_impl.y"
 del((yypminor->yy244)); 
#line 1087 "parser_impl.c"
}
      break;
    case 104: /* aggr */
    case 105: /* aggr_ass */
    case 106: /* aggr_num */
    case 107: /* aggr_atom */
    case 108: /* conjunction */
    case 109: /* disjunction */
{
#line 175 "parser_impl.y"
 del((yypminor->yy221)); 
#line 1099 "parser_impl.c"
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
**       obtained from parserAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
void parserFree(
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
int parserStackPeak(void *p){
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
   parserARG_FETCH;
   yypParser->yyidx--;
#ifndef NDEBUG
   if( yyTraceFILE ){
     fprintf(yyTraceFILE,"%sStack Overflow!\n",yyTracePrompt);
   }
#endif
   while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
   parserARG_STORE; /* Suppress warning about unused %extra_argument var */
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
  { 110, 1 },
  { 111, 0 },
  { 111, 3 },
  { 112, 2 },
  { 112, 1 },
  { 112, 1 },
  { 112, 1 },
  { 112, 4 },
  { 112, 3 },
  { 112, 4 },
  { 112, 3 },
  { 112, 4 },
  { 112, 5 },
  { 112, 3 },
  { 112, 4 },
  { 112, 2 },
  { 112, 2 },
  { 112, 1 },
  { 112, 1 },
  { 112, 1 },
  { 113, 1 },
  { 113, 2 },
  { 84, 0 },
  { 84, 3 },
  { 71, 1 },
  { 71, 3 },
  { 70, 3 },
  { 70, 2 },
  { 70, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 83, 1 },
  { 83, 3 },
  { 82, 0 },
  { 82, 1 },
  { 80, 1 },
  { 80, 2 },
  { 76, 1 },
  { 76, 3 },
  { 77, 1 },
  { 77, 3 },
  { 77, 3 },
  { 78, 1 },
  { 78, 1 },
  { 78, 2 },
  { 78, 3 },
  { 78, 3 },
  { 78, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 3 },
  { 86, 4 },
  { 86, 3 },
  { 86, 6 },
  { 86, 6 },
  { 86, 6 },
  { 86, 4 },
  { 86, 3 },
  { 86, 4 },
  { 86, 5 },
  { 86, 4 },
  { 86, 2 },
  { 86, 2 },
  { 89, 1 },
  { 89, 3 },
  { 88, 0 },
  { 88, 1 },
  { 90, 4 },
  { 90, 4 },
  { 90, 2 },
  { 101, 2 },
  { 101, 3 },
  { 100, 0 },
  { 100, 1 },
  { 105, 4 },
  { 105, 3 },
  { 106, 4 },
  { 92, 1 },
  { 92, 3 },
  { 91, 0 },
  { 91, 1 },
  { 95, 2 },
  { 105, 4 },
  { 105, 3 },
  { 104, 4 },
  { 104, 4 },
  { 104, 4 },
  { 104, 4 },
  { 96, 2 },
  { 93, 1 },
  { 108, 1 },
  { 99, 1 },
  { 98, 2 },
  { 97, 1 },
  { 97, 1 },
  { 94, 1 },
  { 94, 3 },
  { 94, 3 },
  { 109, 1 },
  { 105, 4 },
  { 105, 4 },
  { 106, 1 },
  { 104, 1 },
  { 107, 3 },
  { 107, 2 },
  { 107, 2 },
  { 107, 1 },
  { 79, 5 },
  { 79, 4 },
  { 79, 2 },
  { 79, 1 },
  { 87, 1 },
  { 87, 3 },
  { 87, 3 },
  { 114, 4 },
  { 114, 4 },
  { 116, 1 },
  { 116, 1 },
  { 118, 1 },
  { 118, 1 },
  { 117, 0 },
  { 117, 1 },
  { 120, 1 },
  { 120, 3 },
  { 119, 0 },
  { 119, 1 },
  { 121, 1 },
  { 121, 3 },
  { 73, 6 },
  { 73, 4 },
  { 73, 6 },
  { 73, 4 },
  { 73, 1 },
  { 72, 4 },
  { 72, 4 },
  { 72, 2 },
  { 103, 2 },
  { 103, 3 },
  { 102, 0 },
  { 102, 1 },
  { 115, 4 },
  { 115, 5 },
  { 122, 0 },
  { 122, 1 },
  { 123, 1 },
  { 123, 3 },
  { 74, 2 },
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
  parserARG_FETCH;
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
#line 199 "parser_impl.y"
{
  yy_destructor(yypParser,22,&yymsp[0].minor);
}
#line 1561 "parser_impl.c"
        break;
      case 3: /* line ::= INCLUDE STRING */
#line 201 "parser_impl.y"
{ pParser->include(yymsp[0].minor.yy0.index);   yy_destructor(yypParser,23,&yymsp[-1].minor);
}
#line 1567 "parser_impl.c"
        break;
      case 4: /* line ::= rule */
#line 202 "parser_impl.y"
{ pParser->add(yymsp[0].minor.yy179); }
#line 1572 "parser_impl.c"
        break;
      case 5: /* line ::= HIDE */
#line 203 "parser_impl.y"
{ OUT->show(false);   yy_destructor(yypParser,25,&yymsp[0].minor);
}
#line 1578 "parser_impl.c"
        break;
      case 6: /* line ::= SHOW */
#line 204 "parser_impl.y"
{ OUT->show(true);   yy_destructor(yypParser,26,&yymsp[0].minor);
}
#line 1584 "parser_impl.c"
        break;
      case 7: /* line ::= HIDE signed SLASH NUMBER */
#line 205 "parser_impl.y"
{ OUT->show(yymsp[-2].minor.yy0.index, yymsp[0].minor.yy0.number, false);   yy_destructor(yypParser,25,&yymsp[-3].minor);
  yy_destructor(yypParser,10,&yymsp[-1].minor);
}
#line 1591 "parser_impl.c"
        break;
      case 8: /* line ::= HIDE predicate cond */
#line 206 "parser_impl.y"
{ pParser->add(new Display(yymsp[-2].minor.yy0.loc(), false, yymsp[-1].minor.yy155, *yymsp[0].minor.yy52)); delete yymsp[0].minor.yy52; }
#line 1596 "parser_impl.c"
        break;
      case 9: /* line ::= SHOW signed SLASH NUMBER */
#line 207 "parser_impl.y"
{ OUT->show(yymsp[-2].minor.yy0.index, yymsp[0].minor.yy0.number, true);   yy_destructor(yypParser,26,&yymsp[-3].minor);
  yy_destructor(yypParser,10,&yymsp[-1].minor);
}
#line 1603 "parser_impl.c"
        break;
      case 10: /* line ::= SHOW predicate cond */
#line 208 "parser_impl.y"
{ pParser->add(new Display(yymsp[-2].minor.yy0.loc(), true, yymsp[-1].minor.yy155, *yymsp[0].minor.yy52)); delete yymsp[0].minor.yy52; }
#line 1608 "parser_impl.c"
        break;
      case 11: /* line ::= CONST IDENTIFIER ASSIGN term */
#line 209 "parser_impl.y"
{ pParser->constTerm(yymsp[-2].minor.yy0.index, yymsp[0].minor.yy83);   yy_destructor(yypParser,28,&yymsp[-3].minor);
  yy_destructor(yypParser,30,&yymsp[-1].minor);
}
#line 1615 "parser_impl.c"
        break;
      case 12: /* line ::= DOMAIN signed LBRAC var_list RBRAC */
#line 210 "parser_impl.y"
{ pParser->domainStm(yymsp[-3].minor.yy0.loc(), yymsp[-3].minor.yy0.index, *yymsp[-1].minor.yy117); del(yymsp[-1].minor.yy117);   yy_destructor(yypParser,31,&yymsp[-4].minor);
  yy_destructor(yypParser,32,&yymsp[-2].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 1623 "parser_impl.c"
        break;
      case 13: /* line ::= EXTERNAL predicate cond */
#line 211 "parser_impl.y"
{ pParser->add(new External(yymsp[-2].minor.yy0.loc(), yymsp[-1].minor.yy155, *yymsp[0].minor.yy52)); delete yymsp[0].minor.yy52; }
#line 1628 "parser_impl.c"
        break;
      case 14: /* line ::= EXTERNAL signed SLASH NUMBER */
#line 212 "parser_impl.y"
{ GRD->externalStm(yymsp[-2].minor.yy0.index, yymsp[0].minor.yy0.number);   yy_destructor(yypParser,34,&yymsp[-3].minor);
  yy_destructor(yypParser,10,&yymsp[-1].minor);
}
#line 1635 "parser_impl.c"
        break;
      case 15: /* line ::= CUMULATIVE IDENTIFIER */
#line 213 "parser_impl.y"
{ pParser->incremental(Parser::IPART_CUMULATIVE, yymsp[0].minor.yy0.index);   yy_destructor(yypParser,35,&yymsp[-1].minor);
}
#line 1641 "parser_impl.c"
        break;
      case 16: /* line ::= VOLATILE IDENTIFIER */
#line 214 "parser_impl.y"
{ pParser->incremental(Parser::IPART_VOLATILE, yymsp[0].minor.yy0.index);   yy_destructor(yypParser,36,&yymsp[-1].minor);
}
#line 1647 "parser_impl.c"
        break;
      case 17: /* line ::= BASE */
#line 215 "parser_impl.y"
{ pParser->incremental(Parser::IPART_BASE);   yy_destructor(yypParser,37,&yymsp[0].minor);
}
#line 1653 "parser_impl.c"
        break;
      case 20: /* signed ::= IDENTIFIER */
#line 219 "parser_impl.y"
{ yygotominor.yy0 = yymsp[0].minor.yy0; }
#line 1658 "parser_impl.c"
        break;
      case 21: /* signed ::= MINUS IDENTIFIER */
#line 220 "parser_impl.y"
{ yygotominor.yy0 = yymsp[-1].minor.yy0; yygotominor.yy0.index = GRD->index(std::string("-") + GRD->string(yymsp[0].minor.yy0.index)); }
#line 1663 "parser_impl.c"
        break;
      case 22: /* cond ::= */
      case 34: /* body ::= */ yytestcase(yyruleno==34);
      case 98: /* weightcond ::= */ yytestcase(yyruleno==98);
      case 164: /* priolit_cond ::= */ yytestcase(yyruleno==164);
#line 222 "parser_impl.y"
{ yygotominor.yy52 = new LitPtrVec(); }
#line 1671 "parser_impl.c"
        break;
      case 23: /* cond ::= COLON literal cond */
      case 97: /* nweightcond ::= COLON literal nweightcond */ yytestcase(yyruleno==97);
#line 223 "parser_impl.y"
{ yygotominor.yy52 = yymsp[0].minor.yy52; yygotominor.yy52->push_back(yymsp[-1].minor.yy212);   yy_destructor(yypParser,38,&yymsp[-2].minor);
}
#line 1678 "parser_impl.c"
        break;
      case 24: /* var_list ::= VARIABLE */
#line 225 "parser_impl.y"
{ yygotominor.yy117 = new VarSigVec(); yygotominor.yy117->push_back(VarSig(yymsp[0].minor.yy0.loc(), yymsp[0].minor.yy0.index)); }
#line 1683 "parser_impl.c"
        break;
      case 25: /* var_list ::= var_list COMMA VARIABLE */
#line 226 "parser_impl.y"
{ yymsp[-2].minor.yy117->push_back(VarSig(yymsp[0].minor.yy0.loc(), yymsp[0].minor.yy0.index)); yygotominor.yy117 = yymsp[-2].minor.yy117;   yy_destructor(yypParser,20,&yymsp[-1].minor);
}
#line 1689 "parser_impl.c"
        break;
      case 26: /* rule ::= head IF body */
#line 228 "parser_impl.y"
{ yygotominor.yy179 = new Rule(yymsp[-2].minor.yy212->loc(), yymsp[-2].minor.yy212, *yymsp[0].minor.yy52); del(yymsp[0].minor.yy52);   yy_destructor(yypParser,40,&yymsp[-1].minor);
}
#line 1695 "parser_impl.c"
        break;
      case 27: /* rule ::= IF body */
#line 229 "parser_impl.y"
{ yygotominor.yy179 = new Rule(yymsp[-1].minor.yy0.loc(), 0, *yymsp[0].minor.yy52); del(yymsp[0].minor.yy52); }
#line 1700 "parser_impl.c"
        break;
      case 28: /* rule ::= head */
#line 230 "parser_impl.y"
{ LitPtrVec v; yygotominor.yy179 = new Rule(yymsp[0].minor.yy212->loc(), yymsp[0].minor.yy212, v); }
#line 1705 "parser_impl.c"
        break;
      case 29: /* head ::= predicate */
      case 38: /* lit ::= predlit */ yytestcase(yyruleno==38);
#line 232 "parser_impl.y"
{ yygotominor.yy212 = yymsp[0].minor.yy155; }
#line 1711 "parser_impl.c"
        break;
      case 30: /* head ::= aggr_atom */
      case 31: /* head ::= disjunction */ yytestcase(yyruleno==31);
      case 44: /* body_literal ::= aggr_atom */ yytestcase(yyruleno==44);
      case 48: /* body_literal ::= conjunction */ yytestcase(yyruleno==48);
#line 233 "parser_impl.y"
{ yygotominor.yy212 = yymsp[0].minor.yy221; }
#line 1719 "parser_impl.c"
        break;
      case 32: /* nbody ::= body_literal */
#line 236 "parser_impl.y"
{ yygotominor.yy52 = vec1(yymsp[0].minor.yy212); }
#line 1724 "parser_impl.c"
        break;
      case 33: /* nbody ::= nbody COMMA body_literal */
#line 237 "parser_impl.y"
{ yygotominor.yy52 = yymsp[-2].minor.yy52; yygotominor.yy52->push_back(yymsp[0].minor.yy212);   yy_destructor(yypParser,20,&yymsp[-1].minor);
}
#line 1730 "parser_impl.c"
        break;
      case 35: /* body ::= nbody */
      case 99: /* weightcond ::= nweightcond */ yytestcase(yyruleno==99);
      case 165: /* priolit_cond ::= npriolit_cond */ yytestcase(yyruleno==165);
#line 240 "parser_impl.y"
{ yygotominor.yy52 = yymsp[0].minor.yy52; }
#line 1737 "parser_impl.c"
        break;
      case 36: /* predlit ::= predicate */
#line 242 "parser_impl.y"
{ yygotominor.yy155 = yymsp[0].minor.yy155; }
#line 1742 "parser_impl.c"
        break;
      case 37: /* predlit ::= NOT predicate */
#line 243 "parser_impl.y"
{ yygotominor.yy155 = yymsp[0].minor.yy155; yymsp[0].minor.yy155->sign(true); yymsp[0].minor.yy155->loc(yymsp[-1].minor.yy0.loc()); }
#line 1747 "parser_impl.c"
        break;
      case 39: /* lit ::= term cmp term */
#line 246 "parser_impl.y"
{ yygotominor.yy212 = new RelLit(yymsp[-2].minor.yy83->loc(), yymsp[-1].minor.yy183, yymsp[-2].minor.yy83, yymsp[0].minor.yy83); }
#line 1752 "parser_impl.c"
        break;
      case 40: /* literal ::= lit */
      case 43: /* body_literal ::= literal */ yytestcase(yyruleno==43);
#line 248 "parser_impl.y"
{ yygotominor.yy212 = yymsp[0].minor.yy212; }
#line 1758 "parser_impl.c"
        break;
      case 41: /* literal ::= VARIABLE ASSIGN term */
#line 249 "parser_impl.y"
{ yygotominor.yy212 = new RelLit(yymsp[-2].minor.yy0.loc(), RelLit::ASSIGN, new VarTerm(yymsp[-2].minor.yy0.loc(), yymsp[-2].minor.yy0.index), yymsp[0].minor.yy83);   yy_destructor(yypParser,30,&yymsp[-1].minor);
}
#line 1764 "parser_impl.c"
        break;
      case 42: /* literal ::= term CASSIGN term */
#line 250 "parser_impl.y"
{ yygotominor.yy212 = new RelLit(yymsp[-2].minor.yy83->loc(), RelLit::ASSIGN, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,42,&yymsp[-1].minor);
}
#line 1770 "parser_impl.c"
        break;
      case 45: /* body_literal ::= NOT aggr_atom */
#line 254 "parser_impl.y"
{ yygotominor.yy212 = yymsp[0].minor.yy221; yymsp[0].minor.yy221->sign(true);   yy_destructor(yypParser,41,&yymsp[-1].minor);
}
#line 1776 "parser_impl.c"
        break;
      case 46: /* body_literal ::= VARIABLE ASSIGN aggr_ass */
#line 255 "parser_impl.y"
{ yygotominor.yy212 = yymsp[0].minor.yy221; yymsp[0].minor.yy221->assign(new VarTerm(yymsp[-2].minor.yy0.loc(), yymsp[-2].minor.yy0.index));   yy_destructor(yypParser,30,&yymsp[-1].minor);
}
#line 1782 "parser_impl.c"
        break;
      case 47: /* body_literal ::= term CASSIGN aggr_ass */
#line 256 "parser_impl.y"
{ yygotominor.yy212 = yymsp[0].minor.yy221; yymsp[0].minor.yy221->assign(yymsp[-2].minor.yy83);   yy_destructor(yypParser,42,&yymsp[-1].minor);
}
#line 1788 "parser_impl.c"
        break;
      case 49: /* cmp ::= GREATER */
#line 259 "parser_impl.y"
{ yygotominor.yy183 = RelLit::GREATER;   yy_destructor(yypParser,43,&yymsp[0].minor);
}
#line 1794 "parser_impl.c"
        break;
      case 50: /* cmp ::= LOWER */
#line 260 "parser_impl.y"
{ yygotominor.yy183 = RelLit::LOWER;   yy_destructor(yypParser,44,&yymsp[0].minor);
}
#line 1800 "parser_impl.c"
        break;
      case 51: /* cmp ::= GTHAN */
#line 261 "parser_impl.y"
{ yygotominor.yy183 = RelLit::GTHAN;   yy_destructor(yypParser,45,&yymsp[0].minor);
}
#line 1806 "parser_impl.c"
        break;
      case 52: /* cmp ::= LTHAN */
#line 262 "parser_impl.y"
{ yygotominor.yy183 = RelLit::LTHAN;   yy_destructor(yypParser,46,&yymsp[0].minor);
}
#line 1812 "parser_impl.c"
        break;
      case 53: /* cmp ::= EQUAL */
#line 263 "parser_impl.y"
{ yygotominor.yy183 = RelLit::EQUAL;   yy_destructor(yypParser,47,&yymsp[0].minor);
}
#line 1818 "parser_impl.c"
        break;
      case 54: /* cmp ::= INEQUAL */
#line 264 "parser_impl.y"
{ yygotominor.yy183 = RelLit::INEQUAL;   yy_destructor(yypParser,48,&yymsp[0].minor);
}
#line 1824 "parser_impl.c"
        break;
      case 55: /* term ::= VARIABLE */
#line 266 "parser_impl.y"
{ yygotominor.yy83 = new VarTerm(yymsp[0].minor.yy0.loc(), yymsp[0].minor.yy0.index); }
#line 1829 "parser_impl.c"
        break;
      case 56: /* term ::= IDENTIFIER */
#line 267 "parser_impl.y"
{ yygotominor.yy83 = pParser->term(Val::ID, yymsp[0].minor.yy0.loc(), yymsp[0].minor.yy0.index); }
#line 1834 "parser_impl.c"
        break;
      case 57: /* term ::= STRING */
#line 268 "parser_impl.y"
{ yygotominor.yy83 = pParser->term(Val::STRING, yymsp[0].minor.yy0.loc(), yymsp[0].minor.yy0.index); }
#line 1839 "parser_impl.c"
        break;
      case 58: /* term ::= NUMBER */
#line 269 "parser_impl.y"
{ yygotominor.yy83 = new ConstTerm(yymsp[0].minor.yy0.loc(), Val::create(Val::NUM, yymsp[0].minor.yy0.number)); }
#line 1844 "parser_impl.c"
        break;
      case 59: /* term ::= ANONYMOUS */
#line 270 "parser_impl.y"
{ yygotominor.yy83 = new VarTerm(yymsp[0].minor.yy0.loc()); }
#line 1849 "parser_impl.c"
        break;
      case 60: /* term ::= INFIMUM */
#line 271 "parser_impl.y"
{ yygotominor.yy83 = new ConstTerm(yymsp[0].minor.yy0.loc(), Val::create(Val::INF, 0)); }
#line 1854 "parser_impl.c"
        break;
      case 61: /* term ::= SUPREMUM */
#line 272 "parser_impl.y"
{ yygotominor.yy83 = new ConstTerm(yymsp[0].minor.yy0.loc(), Val::create(Val::SUP, 0)); }
#line 1859 "parser_impl.c"
        break;
      case 62: /* term ::= term DOTS term */
#line 274 "parser_impl.y"
{ yygotominor.yy83 = new RangeTerm(yymsp[-2].minor.yy83->loc(), yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,3,&yymsp[-1].minor);
}
#line 1865 "parser_impl.c"
        break;
      case 63: /* term ::= term SEM term */
#line 275 "parser_impl.y"
{ yygotominor.yy83 = new PoolTerm(yymsp[-2].minor.yy83->loc(), yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,2,&yymsp[-1].minor);
}
#line 1871 "parser_impl.c"
        break;
      case 64: /* term ::= term PLUS term */
#line 276 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::PLUS, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,7,&yymsp[-1].minor);
}
#line 1877 "parser_impl.c"
        break;
      case 65: /* term ::= term MINUS term */
#line 277 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::MINUS, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,8,&yymsp[-1].minor);
}
#line 1883 "parser_impl.c"
        break;
      case 66: /* term ::= term MULT term */
#line 278 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::MULT, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,9,&yymsp[-1].minor);
}
#line 1889 "parser_impl.c"
        break;
      case 67: /* term ::= term SLASH term */
#line 279 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::DIV, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,10,&yymsp[-1].minor);
}
#line 1895 "parser_impl.c"
        break;
      case 68: /* term ::= term DIV term */
#line 280 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::DIV, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,13,&yymsp[-1].minor);
}
#line 1901 "parser_impl.c"
        break;
      case 69: /* term ::= term PDIV term */
#line 281 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::DIV, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,14,&yymsp[-1].minor);
}
#line 1907 "parser_impl.c"
        break;
      case 70: /* term ::= term MOD term */
#line 282 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::MOD, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,11,&yymsp[-1].minor);
}
#line 1913 "parser_impl.c"
        break;
      case 71: /* term ::= term PMOD term */
#line 283 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::MOD, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,12,&yymsp[-1].minor);
}
#line 1919 "parser_impl.c"
        break;
      case 72: /* term ::= term POW term */
#line 284 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::POW, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,15,&yymsp[-1].minor);
}
#line 1925 "parser_impl.c"
        break;
      case 73: /* term ::= term PPOW term */
#line 285 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::POW, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,16,&yymsp[-1].minor);
}
#line 1931 "parser_impl.c"
        break;
      case 74: /* term ::= term AND term */
#line 286 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::AND, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,6,&yymsp[-1].minor);
}
#line 1937 "parser_impl.c"
        break;
      case 75: /* term ::= term XOR term */
#line 287 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::XOR, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,4,&yymsp[-1].minor);
}
#line 1943 "parser_impl.c"
        break;
      case 76: /* term ::= term QUESTION term */
#line 288 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-2].minor.yy83->loc(), MathTerm::OR, yymsp[-2].minor.yy83, yymsp[0].minor.yy83);   yy_destructor(yypParser,5,&yymsp[-1].minor);
}
#line 1949 "parser_impl.c"
        break;
      case 77: /* term ::= PABS LBRAC term RBRAC */
#line 289 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-1].minor.yy83->loc(), MathTerm::ABS, yymsp[-1].minor.yy83);   yy_destructor(yypParser,52,&yymsp[-3].minor);
  yy_destructor(yypParser,32,&yymsp[-2].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 1957 "parser_impl.c"
        break;
      case 78: /* term ::= VBAR term VBAR */
#line 290 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-1].minor.yy83->loc(), MathTerm::ABS, yymsp[-1].minor.yy83);   yy_destructor(yypParser,21,&yymsp[-2].minor);
  yy_destructor(yypParser,21,&yymsp[0].minor);
}
#line 1964 "parser_impl.c"
        break;
      case 79: /* term ::= PPOW LBRAC term COMMA term RBRAC */
#line 291 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-3].minor.yy83->loc(), MathTerm::POW, yymsp[-3].minor.yy83, yymsp[-1].minor.yy83);   yy_destructor(yypParser,16,&yymsp[-5].minor);
  yy_destructor(yypParser,32,&yymsp[-4].minor);
  yy_destructor(yypParser,20,&yymsp[-2].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 1973 "parser_impl.c"
        break;
      case 80: /* term ::= PMOD LBRAC term COMMA term RBRAC */
#line 292 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-3].minor.yy83->loc(), MathTerm::MOD, yymsp[-3].minor.yy83, yymsp[-1].minor.yy83);   yy_destructor(yypParser,12,&yymsp[-5].minor);
  yy_destructor(yypParser,32,&yymsp[-4].minor);
  yy_destructor(yypParser,20,&yymsp[-2].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 1982 "parser_impl.c"
        break;
      case 81: /* term ::= PDIV LBRAC term COMMA term RBRAC */
#line 293 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-3].minor.yy83->loc(), MathTerm::DIV, yymsp[-3].minor.yy83, yymsp[-1].minor.yy83);   yy_destructor(yypParser,14,&yymsp[-5].minor);
  yy_destructor(yypParser,32,&yymsp[-4].minor);
  yy_destructor(yypParser,20,&yymsp[-2].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 1991 "parser_impl.c"
        break;
      case 82: /* term ::= IDENTIFIER LBRAC termlist RBRAC */
#line 294 "parser_impl.y"
{ yygotominor.yy83 = new FuncTerm(yymsp[-3].minor.yy0.loc(), yymsp[-3].minor.yy0.index, *yymsp[-1].minor.yy243); delete yymsp[-1].minor.yy243;   yy_destructor(yypParser,32,&yymsp[-2].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 1998 "parser_impl.c"
        break;
      case 83: /* term ::= LBRAC termlist RBRAC */
#line 295 "parser_impl.y"
{ yygotominor.yy83 = yymsp[-1].minor.yy243->size() == 1 ? yymsp[-1].minor.yy243->pop_back().release() : new FuncTerm(yymsp[-2].minor.yy0.loc(), GRD->index(""), *yymsp[-1].minor.yy243); delete yymsp[-1].minor.yy243;   yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 2004 "parser_impl.c"
        break;
      case 84: /* term ::= LBRAC termlist COMMA RBRAC */
#line 296 "parser_impl.y"
{ yygotominor.yy83 = new FuncTerm(yymsp[-3].minor.yy0.loc(), GRD->index(""), *yymsp[-2].minor.yy243); delete yymsp[-2].minor.yy243;   yy_destructor(yypParser,20,&yymsp[-1].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 2011 "parser_impl.c"
        break;
      case 85: /* term ::= AT IDENTIFIER LBRAC termlist RBRAC */
#line 297 "parser_impl.y"
{ yygotominor.yy83 = new LuaTerm(yymsp[-3].minor.yy0.loc(), yymsp[-3].minor.yy0.index, *yymsp[-1].minor.yy243); delete yymsp[-1].minor.yy243;   yy_destructor(yypParser,53,&yymsp[-4].minor);
  yy_destructor(yypParser,32,&yymsp[-2].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 2019 "parser_impl.c"
        break;
      case 86: /* term ::= AT IDENTIFIER LBRAC RBRAC */
#line 298 "parser_impl.y"
{ TermPtrVec args; yygotominor.yy83 = new LuaTerm(yymsp[-2].minor.yy0.loc(), yymsp[-2].minor.yy0.index, args);   yy_destructor(yypParser,53,&yymsp[-3].minor);
  yy_destructor(yypParser,32,&yymsp[-1].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 2027 "parser_impl.c"
        break;
      case 87: /* term ::= MINUS term */
#line 299 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-1].minor.yy0.loc(), MathTerm::MINUS, ZERO(yymsp[-1].minor.yy0.loc()), yymsp[0].minor.yy83); }
#line 2032 "parser_impl.c"
        break;
      case 88: /* term ::= BNOT term */
#line 300 "parser_impl.y"
{ yygotominor.yy83 = new MathTerm(yymsp[-1].minor.yy0.loc(), MathTerm::XOR, MINUSONE(yymsp[-1].minor.yy0.loc()), yymsp[0].minor.yy83); }
#line 2037 "parser_impl.c"
        break;
      case 89: /* nweightlist ::= weightlit */
      case 103: /* ncondlist ::= condlit */ yytestcase(yyruleno==103);
#line 302 "parser_impl.y"
{ yygotominor.yy226 = vec1(yymsp[0].minor.yy244); }
#line 2043 "parser_impl.c"
        break;
      case 90: /* nweightlist ::= nweightlist COMMA weightlit */
      case 104: /* ncondlist ::= ncondlist COMMA condlit */ yytestcase(yyruleno==104);
#line 303 "parser_impl.y"
{ yygotominor.yy226 = yymsp[-2].minor.yy226; yygotominor.yy226->push_back(yymsp[0].minor.yy244);   yy_destructor(yypParser,20,&yymsp[-1].minor);
}
#line 2050 "parser_impl.c"
        break;
      case 91: /* weightlist ::= */
      case 105: /* condlist ::= */ yytestcase(yyruleno==105);
#line 304 "parser_impl.y"
{ yygotominor.yy226 = new CondLitVec(); }
#line 2056 "parser_impl.c"
        break;
      case 92: /* weightlist ::= nweightlist */
      case 106: /* condlist ::= ncondlist */ yytestcase(yyruleno==106);
#line 305 "parser_impl.y"
{ yygotominor.yy226 = yymsp[0].minor.yy226; }
#line 2062 "parser_impl.c"
        break;
      case 93: /* weightlit ::= lit nweightcond ASSIGN term */
#line 307 "parser_impl.y"
{ yygotominor.yy244 = new CondLit(yymsp[-3].minor.yy212->loc(), yymsp[-3].minor.yy212, yymsp[0].minor.yy83, *yymsp[-2].minor.yy52); delete yymsp[-2].minor.yy52;   yy_destructor(yypParser,30,&yymsp[-1].minor);
}
#line 2068 "parser_impl.c"
        break;
      case 94: /* weightlit ::= lit ASSIGN term weightcond */
#line 308 "parser_impl.y"
{ yygotominor.yy244 = new CondLit(yymsp[-3].minor.yy212->loc(), yymsp[-3].minor.yy212, yymsp[-1].minor.yy83, *yymsp[0].minor.yy52); delete yymsp[0].minor.yy52;   yy_destructor(yypParser,30,&yymsp[-2].minor);
}
#line 2074 "parser_impl.c"
        break;
      case 95: /* weightlit ::= lit weightcond */
      case 107: /* condlit ::= lit weightcond */ yytestcase(yyruleno==107);
      case 114: /* ccondlit ::= lit nweightcond */ yytestcase(yyruleno==114);
#line 309 "parser_impl.y"
{ yygotominor.yy244 = new CondLit(yymsp[-1].minor.yy212->loc(), yymsp[-1].minor.yy212, ONE(yymsp[-1].minor.yy212->loc()), *yymsp[0].minor.yy52); delete yymsp[0].minor.yy52; }
#line 2081 "parser_impl.c"
        break;
      case 96: /* nweightcond ::= COLON literal */
      case 162: /* npriolit_cond ::= COLON literal */ yytestcase(yyruleno==162);
#line 311 "parser_impl.y"
{ yygotominor.yy52 = vec1(yymsp[0].minor.yy212);   yy_destructor(yypParser,38,&yymsp[-1].minor);
}
#line 2088 "parser_impl.c"
        break;
      case 100: /* aggr_ass ::= SUM LSBRAC weightlist RSBRAC */
#line 317 "parser_impl.y"
{ yygotominor.yy221 = new SumAggrLit(yymsp[-3].minor.yy0.loc(), *yymsp[-1].minor.yy226, false); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,56,&yymsp[-2].minor);
  yy_destructor(yypParser,57,&yymsp[0].minor);
}
#line 2095 "parser_impl.c"
        break;
      case 101: /* aggr_ass ::= LSBRAC weightlist RSBRAC */
#line 318 "parser_impl.y"
{ yygotominor.yy221 = new SumAggrLit(yymsp[-2].minor.yy0.loc(), *yymsp[-1].minor.yy226, false); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,57,&yymsp[0].minor);
}
#line 2101 "parser_impl.c"
        break;
      case 102: /* aggr_num ::= AVG LSBRAC weightlist RSBRAC */
#line 320 "parser_impl.y"
{ yygotominor.yy221 = new AvgAggrLit(yymsp[-3].minor.yy0.loc(), *yymsp[-1].minor.yy226); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,56,&yymsp[-2].minor);
  yy_destructor(yypParser,57,&yymsp[0].minor);
}
#line 2108 "parser_impl.c"
        break;
      case 108: /* aggr_ass ::= COUNT LCBRAC condlist RCBRAC */
#line 329 "parser_impl.y"
{ yygotominor.yy221 = new SumAggrLit(yymsp[-3].minor.yy0.loc(), *yymsp[-1].minor.yy226, true); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,60,&yymsp[-2].minor);
  yy_destructor(yypParser,61,&yymsp[0].minor);
}
#line 2115 "parser_impl.c"
        break;
      case 109: /* aggr_ass ::= LCBRAC condlist RCBRAC */
#line 330 "parser_impl.y"
{ yygotominor.yy221 = new SumAggrLit(yymsp[-2].minor.yy0.loc(), *yymsp[-1].minor.yy226, true); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,61,&yymsp[0].minor);
}
#line 2121 "parser_impl.c"
        break;
      case 110: /* aggr ::= EVEN LCBRAC condlist RCBRAC */
#line 332 "parser_impl.y"
{ yygotominor.yy221 = new ParityAggrLit(yymsp[-3].minor.yy0.loc(), *yymsp[-1].minor.yy226, true, true); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,60,&yymsp[-2].minor);
  yy_destructor(yypParser,61,&yymsp[0].minor);
}
#line 2128 "parser_impl.c"
        break;
      case 111: /* aggr ::= ODD LCBRAC condlist RCBRAC */
#line 333 "parser_impl.y"
{ yygotominor.yy221 = new ParityAggrLit(yymsp[-3].minor.yy0.loc(), *yymsp[-1].minor.yy226, false, true); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,60,&yymsp[-2].minor);
  yy_destructor(yypParser,61,&yymsp[0].minor);
}
#line 2135 "parser_impl.c"
        break;
      case 112: /* aggr ::= EVEN LSBRAC weightlist RSBRAC */
#line 334 "parser_impl.y"
{ yygotominor.yy221 = new ParityAggrLit(yymsp[-3].minor.yy0.loc(), *yymsp[-1].minor.yy226, true, false); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,56,&yymsp[-2].minor);
  yy_destructor(yypParser,57,&yymsp[0].minor);
}
#line 2142 "parser_impl.c"
        break;
      case 113: /* aggr ::= ODD LSBRAC weightlist RSBRAC */
#line 335 "parser_impl.y"
{ yygotominor.yy221 = new ParityAggrLit(yymsp[-3].minor.yy0.loc(), *yymsp[-1].minor.yy226, false, false); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,56,&yymsp[-2].minor);
  yy_destructor(yypParser,57,&yymsp[0].minor);
}
#line 2149 "parser_impl.c"
        break;
      case 115: /* ccondlist ::= ccondlit */
      case 121: /* head_ccondlist ::= head_ccondlit_cond */ yytestcase(yyruleno==121);
#line 338 "parser_impl.y"
{ yygotominor.yy226 = new CondLitVec(); yygotominor.yy226->push_back(yymsp[0].minor.yy244); }
#line 2155 "parser_impl.c"
        break;
      case 116: /* conjunction ::= ccondlist */
      case 124: /* disjunction ::= head_ccondlist */ yytestcase(yyruleno==124);
#line 340 "parser_impl.y"
{ yygotominor.yy221 = new JunctionAggrLit(yymsp[0].minor.yy226->at(0).loc(), *yymsp[0].minor.yy226); delete yymsp[0].minor.yy226; }
#line 2161 "parser_impl.c"
        break;
      case 117: /* head_ccondlit_nocond ::= predicate */
#line 342 "parser_impl.y"
{ LitPtrVec cond; yygotominor.yy244 = new CondLit(yymsp[0].minor.yy155->loc(), yymsp[0].minor.yy155, ONE(yymsp[0].minor.yy155->loc()), cond); }
#line 2166 "parser_impl.c"
        break;
      case 118: /* head_ccondlit_cond ::= predicate nweightcond */
#line 343 "parser_impl.y"
{ yygotominor.yy244 = new CondLit(yymsp[-1].minor.yy155->loc(), yymsp[-1].minor.yy155, ONE(yymsp[-1].minor.yy155->loc()), *yymsp[0].minor.yy52); delete yymsp[0].minor.yy52; }
#line 2171 "parser_impl.c"
        break;
      case 119: /* head_ccondlit ::= head_ccondlit_nocond */
      case 120: /* head_ccondlit ::= head_ccondlit_cond */ yytestcase(yyruleno==120);
#line 344 "parser_impl.y"
{ yygotominor.yy244 = yymsp[0].minor.yy244; }
#line 2177 "parser_impl.c"
        break;
      case 122: /* head_ccondlist ::= head_ccondlit_nocond VBAR head_ccondlit */
#line 348 "parser_impl.y"
{ yygotominor.yy226 = new CondLitVec(); yygotominor.yy226->push_back(yymsp[-2].minor.yy244); yygotominor.yy226->push_back(yymsp[0].minor.yy244);   yy_destructor(yypParser,21,&yymsp[-1].minor);
}
#line 2183 "parser_impl.c"
        break;
      case 123: /* head_ccondlist ::= head_ccondlist VBAR head_ccondlit */
#line 349 "parser_impl.y"
{ yygotominor.yy226 = yymsp[-2].minor.yy226; yygotominor.yy226->push_back(yymsp[0].minor.yy244);   yy_destructor(yypParser,21,&yymsp[-1].minor);
}
#line 2189 "parser_impl.c"
        break;
      case 125: /* aggr_ass ::= MIN LSBRAC weightlist RSBRAC */
#line 353 "parser_impl.y"
{ yygotominor.yy221 = new MinMaxAggrLit(yymsp[-3].minor.yy0.loc(), *yymsp[-1].minor.yy226, false); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,56,&yymsp[-2].minor);
  yy_destructor(yypParser,57,&yymsp[0].minor);
}
#line 2196 "parser_impl.c"
        break;
      case 126: /* aggr_ass ::= MAX LSBRAC weightlist RSBRAC */
#line 354 "parser_impl.y"
{ yygotominor.yy221 = new MinMaxAggrLit(yymsp[-3].minor.yy0.loc(), *yymsp[-1].minor.yy226, true); delete yymsp[-1].minor.yy226;   yy_destructor(yypParser,56,&yymsp[-2].minor);
  yy_destructor(yypParser,57,&yymsp[0].minor);
}
#line 2203 "parser_impl.c"
        break;
      case 127: /* aggr_num ::= aggr_ass */
      case 128: /* aggr ::= aggr_num */ yytestcase(yyruleno==128);
      case 132: /* aggr_atom ::= aggr */ yytestcase(yyruleno==132);
#line 356 "parser_impl.y"
{ yygotominor.yy221 = yymsp[0].minor.yy221; }
#line 2210 "parser_impl.c"
        break;
      case 129: /* aggr_atom ::= term aggr_num term */
#line 359 "parser_impl.y"
{ yygotominor.yy221 = yymsp[-1].minor.yy221; yygotominor.yy221->lower(yymsp[-2].minor.yy83); yygotominor.yy221->upper(yymsp[0].minor.yy83); }
#line 2215 "parser_impl.c"
        break;
      case 130: /* aggr_atom ::= aggr_num term */
#line 360 "parser_impl.y"
{ yygotominor.yy221 = yymsp[-1].minor.yy221; yygotominor.yy221->upper(yymsp[0].minor.yy83); }
#line 2220 "parser_impl.c"
        break;
      case 131: /* aggr_atom ::= term aggr_num */
#line 361 "parser_impl.y"
{ yygotominor.yy221 = yymsp[0].minor.yy221; yygotominor.yy221->lower(yymsp[-1].minor.yy83); }
#line 2225 "parser_impl.c"
        break;
      case 133: /* predicate ::= MINUS IDENTIFIER LBRAC termlist RBRAC */
#line 364 "parser_impl.y"
{ yygotominor.yy155 = pParser->predLit(yymsp[-4].minor.yy0.loc(), yymsp[-3].minor.yy0.index, *yymsp[-1].minor.yy243, true); delete yymsp[-1].minor.yy243;   yy_destructor(yypParser,32,&yymsp[-2].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 2232 "parser_impl.c"
        break;
      case 134: /* predicate ::= IDENTIFIER LBRAC termlist RBRAC */
#line 365 "parser_impl.y"
{ yygotominor.yy155 = pParser->predLit(yymsp[-3].minor.yy0.loc(), yymsp[-3].minor.yy0.index, *yymsp[-1].minor.yy243, false); delete yymsp[-1].minor.yy243;   yy_destructor(yypParser,32,&yymsp[-2].minor);
  yy_destructor(yypParser,33,&yymsp[0].minor);
}
#line 2239 "parser_impl.c"
        break;
      case 135: /* predicate ::= MINUS IDENTIFIER */
#line 366 "parser_impl.y"
{ TermPtrVec terms; yygotominor.yy155 = pParser->predLit(yymsp[-1].minor.yy0.loc(), yymsp[0].minor.yy0.index, terms, true); }
#line 2244 "parser_impl.c"
        break;
      case 136: /* predicate ::= IDENTIFIER */
#line 367 "parser_impl.y"
{ TermPtrVec terms; yygotominor.yy155 = pParser->predLit(yymsp[0].minor.yy0.loc(), yymsp[0].minor.yy0.index, terms, false); }
#line 2249 "parser_impl.c"
        break;
      case 137: /* termlist ::= term */
#line 369 "parser_impl.y"
{ yygotominor.yy243 = vec1(yymsp[0].minor.yy83); }
#line 2254 "parser_impl.c"
        break;
      case 138: /* termlist ::= termlist COMMA term */
#line 370 "parser_impl.y"
{ yygotominor.yy243 = yymsp[-2].minor.yy243; yymsp[-2].minor.yy243->push_back(yymsp[0].minor.yy83);   yy_destructor(yypParser,20,&yymsp[-1].minor);
}
#line 2260 "parser_impl.c"
        break;
      case 139: /* termlist ::= termlist DSEM termlist */
#line 371 "parser_impl.y"
{ yygotominor.yy243 = yymsp[-2].minor.yy243; yymsp[-2].minor.yy243->push_back(new ArgTerm(yymsp[-1].minor.yy0.loc(), yygotominor.yy243->pop_back().release(), *yymsp[0].minor.yy243)); delete yymsp[0].minor.yy243; }
#line 2265 "parser_impl.c"
        break;
      case 140: /* optimize ::= soptimize LSBRAC prio_list RSBRAC */
#line 373 "parser_impl.y"
{ pParser->nextLevel();   yy_destructor(yypParser,56,&yymsp[-2].minor);
  yy_destructor(yypParser,57,&yymsp[0].minor);
}
#line 2272 "parser_impl.c"
        break;
      case 141: /* optimize ::= soptimize_set LCBRAC prio_set RCBRAC */
#line 374 "parser_impl.y"
{ pParser->nextLevel();   yy_destructor(yypParser,60,&yymsp[-2].minor);
  yy_destructor(yypParser,61,&yymsp[0].minor);
}
#line 2279 "parser_impl.c"
        break;
      case 142: /* soptimize ::= MINIMIZE */
#line 376 "parser_impl.y"
{ pParser->maximize(false); pParser->optimizeSet(false);   yy_destructor(yypParser,66,&yymsp[0].minor);
}
#line 2285 "parser_impl.c"
        break;
      case 143: /* soptimize ::= MAXIMIZE */
#line 377 "parser_impl.y"
{ pParser->maximize(true); pParser->optimizeSet(false);   yy_destructor(yypParser,67,&yymsp[0].minor);
}
#line 2291 "parser_impl.c"
        break;
      case 144: /* soptimize_set ::= MINIMIZE */
#line 378 "parser_impl.y"
{ pParser->maximize(false); pParser->optimizeSet(true);   yy_destructor(yypParser,66,&yymsp[0].minor);
}
#line 2297 "parser_impl.c"
        break;
      case 145: /* soptimize_set ::= MAXIMIZE */
#line 379 "parser_impl.y"
{ pParser->maximize(true); pParser->optimizeSet(true);   yy_destructor(yypParser,67,&yymsp[0].minor);
}
#line 2303 "parser_impl.c"
        break;
      case 148: /* nprio_list ::= weightedpriolit */
      case 152: /* nprio_set ::= priolit */ yytestcase(yyruleno==152);
#line 384 "parser_impl.y"
{ pParser->add(yymsp[0].minor.yy184); }
#line 2309 "parser_impl.c"
        break;
      case 149: /* nprio_list ::= weightedpriolit COMMA prio_list */
      case 153: /* nprio_set ::= priolit COMMA prio_set */ yytestcase(yyruleno==153);
#line 385 "parser_impl.y"
{ pParser->add(yymsp[-2].minor.yy184);   yy_destructor(yypParser,20,&yymsp[-1].minor);
}
#line 2316 "parser_impl.c"
        break;
      case 154: /* weightedpriolit ::= predlit ASSIGN term AT term priolit_cond */
#line 394 "parser_impl.y"
{ yygotominor.yy184 = new Optimize(yymsp[-5].minor.yy155->loc(), yymsp[-5].minor.yy155, yymsp[-3].minor.yy83, yymsp[-1].minor.yy83, *yymsp[0].minor.yy52, pParser->maximize()); pParser->setUniques(yygotominor.yy184); del(yymsp[0].minor.yy52);   yy_destructor(yypParser,30,&yymsp[-4].minor);
  yy_destructor(yypParser,53,&yymsp[-2].minor);
}
#line 2323 "parser_impl.c"
        break;
      case 155: /* weightedpriolit ::= predlit ASSIGN term priolit_cond */
#line 395 "parser_impl.y"
{ yygotominor.yy184 = new Optimize(yymsp[-3].minor.yy155->loc(), yymsp[-3].minor.yy155, yymsp[-1].minor.yy83, new ConstTerm(yymsp[-3].minor.yy155->loc(), Val::create(Val::NUM, pParser->level())), *yymsp[0].minor.yy52, pParser->maximize()); pParser->setUniques(yygotominor.yy184); del(yymsp[0].minor.yy52);   yy_destructor(yypParser,30,&yymsp[-2].minor);
}
#line 2329 "parser_impl.c"
        break;
      case 156: /* weightedpriolit ::= predlit npriolit_cond ASSIGN term AT term */
#line 396 "parser_impl.y"
{ yygotominor.yy184 = new Optimize(yymsp[-5].minor.yy155->loc(), yymsp[-5].minor.yy155, yymsp[-2].minor.yy83, yymsp[0].minor.yy83, *yymsp[-4].minor.yy52, pParser->maximize()); pParser->setUniques(yygotominor.yy184); del(yymsp[-4].minor.yy52);   yy_destructor(yypParser,30,&yymsp[-3].minor);
  yy_destructor(yypParser,53,&yymsp[-1].minor);
}
#line 2336 "parser_impl.c"
        break;
      case 157: /* weightedpriolit ::= predlit npriolit_cond ASSIGN term */
#line 397 "parser_impl.y"
{ yygotominor.yy184 = new Optimize(yymsp[-3].minor.yy155->loc(), yymsp[-3].minor.yy155, yymsp[0].minor.yy83, new ConstTerm(yymsp[-3].minor.yy155->loc(), Val::create(Val::NUM, pParser->level())), *yymsp[-2].minor.yy52, pParser->maximize()); pParser->setUniques(yygotominor.yy184); del(yymsp[-2].minor.yy52);   yy_destructor(yypParser,30,&yymsp[-1].minor);
}
#line 2342 "parser_impl.c"
        break;
      case 158: /* weightedpriolit ::= priolit */
#line 398 "parser_impl.y"
{ yygotominor.yy184 = yymsp[0].minor.yy184; }
#line 2347 "parser_impl.c"
        break;
      case 159: /* priolit ::= predlit AT term priolit_cond */
#line 400 "parser_impl.y"
{ yygotominor.yy184 = new Optimize(yymsp[-3].minor.yy155->loc(), yymsp[-3].minor.yy155, ONE(yymsp[-3].minor.yy155->loc()), yymsp[-1].minor.yy83, *yymsp[0].minor.yy52, pParser->maximize()); pParser->setUniques(yygotominor.yy184); del(yymsp[0].minor.yy52);   yy_destructor(yypParser,53,&yymsp[-2].minor);
}
#line 2353 "parser_impl.c"
        break;
      case 160: /* priolit ::= predlit npriolit_cond AT term */
#line 401 "parser_impl.y"
{ yygotominor.yy184 = new Optimize(yymsp[-3].minor.yy155->loc(), yymsp[-3].minor.yy155, ONE(yymsp[-3].minor.yy155->loc()), yymsp[0].minor.yy83, *yymsp[-2].minor.yy52, pParser->maximize()); pParser->setUniques(yygotominor.yy184); del(yymsp[-2].minor.yy52);   yy_destructor(yypParser,53,&yymsp[-1].minor);
}
#line 2359 "parser_impl.c"
        break;
      case 161: /* priolit ::= predlit priolit_cond */
#line 402 "parser_impl.y"
{ yygotominor.yy184 = new Optimize(yymsp[-1].minor.yy155->loc(), yymsp[-1].minor.yy155, ONE(yymsp[-1].minor.yy155->loc()), new ConstTerm(yymsp[-1].minor.yy155->loc(), Val::create(Val::NUM, pParser->level())), *yymsp[0].minor.yy52, pParser->maximize()); pParser->setUniques(yygotominor.yy184); del(yymsp[0].minor.yy52); }
#line 2364 "parser_impl.c"
        break;
      case 163: /* npriolit_cond ::= npriolit_cond COLON literal */
#line 405 "parser_impl.y"
{ yygotominor.yy52 = yymsp[-2].minor.yy52; yymsp[-2].minor.yy52->push_back(yymsp[0].minor.yy212);   yy_destructor(yypParser,38,&yymsp[-1].minor);
}
#line 2370 "parser_impl.c"
        break;
      case 166: /* compute ::= COMPUTE LCBRAC compute_list RCBRAC */
#line 410 "parser_impl.y"
{
  yy_destructor(yypParser,68,&yymsp[-3].minor);
  yy_destructor(yypParser,60,&yymsp[-2].minor);
  yy_destructor(yypParser,61,&yymsp[0].minor);
}
#line 2379 "parser_impl.c"
        break;
      case 167: /* compute ::= COMPUTE NUMBER LCBRAC compute_list RCBRAC */
#line 411 "parser_impl.y"
{
  yy_destructor(yypParser,68,&yymsp[-4].minor);
  yy_destructor(yypParser,27,&yymsp[-3].minor);
  yy_destructor(yypParser,60,&yymsp[-2].minor);
  yy_destructor(yypParser,61,&yymsp[0].minor);
}
#line 2389 "parser_impl.c"
        break;
      case 170: /* ncompute_list ::= computelit */
#line 416 "parser_impl.y"
{ pParser->add(yymsp[0].minor.yy104); }
#line 2394 "parser_impl.c"
        break;
      case 171: /* ncompute_list ::= computelit COMMA ncompute_list */
#line 417 "parser_impl.y"
{ pParser->add(yymsp[-2].minor.yy104);   yy_destructor(yypParser,20,&yymsp[-1].minor);
}
#line 2400 "parser_impl.c"
        break;
      case 172: /* computelit ::= predlit priolit_cond */
#line 419 "parser_impl.y"
{ yygotominor.yy104 = new Compute(yymsp[-1].minor.yy155->loc(), yymsp[-1].minor.yy155, *yymsp[0].minor.yy52); del(yymsp[0].minor.yy52); }
#line 2405 "parser_impl.c"
        break;
      default:
      /* (0) start ::= program */ yytestcase(yyruleno==0);
      /* (1) program ::= */ yytestcase(yyruleno==1);
      /* (18) line ::= optimize */ yytestcase(yyruleno==18);
      /* (19) line ::= compute */ yytestcase(yyruleno==19);
      /* (146) prio_list ::= */ yytestcase(yyruleno==146);
      /* (147) prio_list ::= nprio_list */ yytestcase(yyruleno==147);
      /* (150) prio_set ::= */ yytestcase(yyruleno==150);
      /* (151) prio_set ::= nprio_set */ yytestcase(yyruleno==151);
      /* (168) compute_list ::= */ yytestcase(yyruleno==168);
      /* (169) compute_list ::= ncompute_list */ yytestcase(yyruleno==169);
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
  parserARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sFail!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
#line 75 "parser_impl.y"
 pParser->parseError(); 
#line 2465 "parser_impl.c"
  parserARG_STORE; /* Suppress warning about unused %extra_argument variable */
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
  parserARG_FETCH;
#define TOKEN (yyminor.yy0)
#line 76 "parser_impl.y"
 pParser->syntaxError(); 
#line 2484 "parser_impl.c"
  parserARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  parserARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sAccept!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
  parserARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "parserAlloc" which describes the current state of the parser.
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
void parser(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  parserTOKENTYPE yyminor       /* The value for the token */
  parserARG_PDECL               /* Optional %extra_argument parameter */
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
  parserARG_STORE;

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
