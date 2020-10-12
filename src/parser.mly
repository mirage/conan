%{

%}

%token HWS
%token EOL
%token <string> STRING
%token <NUMBER> NUMBER

%token GREATER
%token AMPERSAND
%token LPAREN
%token RPAREN
%token DOT
%token STAR
%token PLUS
%token MINUS
%token PERCENT
%token BAR
%token TILDE

%%

magic: query+

level: GREATER
query: line ( level+ line )*

line: offset HWS ty HWS test HWS message EOL

offset: abs | rel | ind
abs: NUMBER
rel: AMPERSAND NUMBER
ind: indoff | rel_indoff

indoff: LPAREN (abs | rel) (DOT size)? (op disp)? RPAREN
rel_indoff: AMPERSAND indoff

size: byte | leshort | beshort | lelong | belong | melong

byte: B | b | C | c
leshort: s | h
beshort: S | H
lelong: l
belong: L
melong: m

op: invert? ( PLUS | MINUS | STAR | SLASH | PERCENT | AMPERSAND | BAR )
invert: TILDE

disp: NUMBER | memvalue
memvalue: LPAREN NUMBER RPAREN

ty: unsigned? ( numeric | strtype | default )
unsigned: u

numeric: ( numtype | datetype ) nummask?
numtype: byte | short | long | quad

byte: BYTE
short: SHORT | BESHORT | LESHORT
long: LONG | LELONG | BELONG | MELONG
quad: QUAD | LEQUAD | BEQUAD

datetype: udate32 | ldate32 | udate64 | ldate64
udate32: DATE | BEDATE | LEDATE | MEDATE
ldate32: LDATE | BELDATE | LELDATE | MELDATE
udate64: QDATE | LEQDATE | BEQDATE
ldate64: QLDATE | LEQDATE | BEQDATE

nummask: op NUMBER

strtype: regex | search | string8 | string16

regex: REGEX (SLASH regflag+)?
regflag: c | s | linecnt
linecnt: NUMBER

search: STRING (SLASH srchflag+)?
srchflag: strflag | srchcnt
srchcnt: NUMBER

string8: ( STRING | PSTRING ) (SLASH strflag+)
strflag: b | B | c | C

string16: BESTRING16 | LESTRING16

default: DEFAULT
