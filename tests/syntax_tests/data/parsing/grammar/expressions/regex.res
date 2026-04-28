let re = /a*[^a|b]/
let re = /.*[^a|b]/

// Test cases borrowed from https://hg.python.org/cpython/file/tip/Lib/test/re_tests.py

// Test ?P< and ?P= extensions
let re = /(?P<foo_123', '/      // Unterminated group identifier
let re = /(?P<1>a)/             // Begins with a digit
let re = /(?P<!>a)/             // Begins with an illegal char
let re = /(?P<foo!>a)/          // Begins with an illegal char

// Same tests, for the ?P= form
let re = /(?P<foo_123>a)(?P=foo_123/
let re = /(?P<foo_123>a)(?P=1)/
let re = /(?P<foo_123>a)(?P=!)/
let re = /(?P<foo_123>a)(?P=foo_124/  // Backref to undefined group

let re = /(?P<foo_123>a)/
let re = /(?P<foo_123>a)(?P=foo_123)/

// Test octal escapes
let re = /\\1/    // Backreference
let re = /[\\1]/  // Character
let re = /\\09/
let re = /\\141/
let re = /(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)\\119/

// Test \0 is handled everywhere
let re = /\0/
let re = /[\0a]/
let re = /[a\0]/
let re = /[^a\0]/

// Test various letter escapes
let re = /\a[\b]\f\n\r\t\v/
let re = /[\a][\b][\f][\n][\r][\t][\v]/
// NOTE: not an error under PCRE/PRE:
let re = /\u/    // A Perl escape
//let re = /\c\e\g\h\i\j\k\m\o\p\q\y\z/
let re = /\xff/
// new \x semantics
let re = /\x00ffffffffffffff/
let re = /\x00f/
let re = /\x00fe/
//let re = /\x00ffffffffffffff/
//let re = /\x00f/
//let re = /\x00fe/

let re = /^\w+=(\\[\000-\277]|[^\n\\])*", "SRC=eval.c g.c blah blah blah \\\\\n\tapes.c/

// Test that . only matches \n in DOTALL mode
let re = /a.b/
let re = /a.b/
let re = /a.*b/
let re = /a.{4,5}b/
let re = /a.b/
let re = /(?s)a.b/
let re = /(?s)a.*b/
let re = /(?s)a.{4,5}b/
let re = /(?s)a.b/

let re = /)/    // Unmatched right bracket
// let re = //     // Empty pattern
let re = /abc/
let re = /abc/
let re = /abc/
let re = /abc/
let re = /abc/
let re = /abc/
let re = /ab*c/
let re = /ab*bc/
let re = /ab*bc/
let re = /ab*bc/
let re = /ab+bc/
let re = /ab+bc/
let re = /ab+bc/
let re = /ab+bc/
let re = /ab?bc/
let re = /ab?bc/
let re = /ab?bc/
let re = /ab?c/
let re = /^abc$/
let re = /^abc$/
let re = /^abc/
let re = /^abc$/
let re = /abc$/
let re = /^/
let re = /$/
let re = /a.c/
let re = /a.c/
let re = /a.*c/
let re = /a.*c/
let re = /a[bc]d/
let re = /a[bc]d/
let re = /a[b-d]e/
let re = /a[b-d]e/
let re = /a[b-d]/
let re = /a[-b]/
let re = /a[\\-b]/
// NOTE: not an error under PCRE/PRE:
//let re = /a[b-]/
let re = /a[]b/
let re = /a\\/
let re = /a]/
let re = /a[]]b/
let re = /a[\\]]b/
let re = /a[^bc]d/
let re = /a[^bc]d/
let re = /a[^-b]c/
let re = /a[^-b]c/
let re = /a[^]b]c/
let re = /a[^]b]c/
let re = /\\ba\\b/
let re = /\\ba\\b/
let re = /\\ba\\b/
let re = /\\by\\b/
let re = /\\by\\b/
let re = /\\by\\b/
let re = /x\\b/
let re = /x\\B/
let re = /\\Bz/
let re = /z\\B/
let re = /\\Bx/
let re = /\\Ba\\B/
let re = /\\Ba\\B/
let re = /\\Ba\\B/
let re = /\\By\\B/
let re = /\\By\\B/
let re = /\\By\\b/
let re = /\\by\\B/
let re = /\\By\\B/
let re = /ab|cd/
let re = /ab|cd/
let re = /()ef/
let re = /$b/
let re = /a\\(b/
let re = /a\\(*b/
let re = /a\\(*b/
let re = /a\\\\b/
let re = /((a))/
let re = /(a)b(c)/
let re = /a+b+c/
let re = /(a+|b)*/
let re = /(a+|b)+/
let re = /(a+|b)?/
let re = /)(/
let re = /[^ab]*/
let re = /abc/
let re = /a*/
let re = /a|b|c|d|e/
let re = /(a|b|c|d|e)f/
let re = /abcd*efg/
let re = /ab*/
let re = /ab*/
let re = /(ab|cd)e/
let re = /[abhgefdc]ij/
let re = /^(ab|cd)e/
let re = /(abc|)ef/
let re = /(a|b)c*d/
let re = /(ab|ab*)bc/
let re = /a([bc]*)c*/
let re = /a([bc]*)(c*d)/
let re = /a([bc]+)(c*d)/
let re = /a([bc]*)(c+d)/
let re = /a[bcd]*dcdcde/
let re = /a[bcd]+dcdcde/
let re = /(ab|a)b*c/
let re = /((a)(b)c)(d)/
let re = /[a-zA-Z_][a-zA-Z0-9_]*/
let re = /^a(bc+|b[eh])g|.h$/
let re = /(bc+d$|ef*g.|h?i(j|k))/
let re = /(bc+d$|ef*g.|h?i(j|k))/
let re = /(bc+d$|ef*g.|h?i(j|k))/
let re = /(bc+d$|ef*g.|h?i(j|k))/
let re = /(bc+d$|ef*g.|h?i(j|k))/
let re = /(((((((((a)))))))))/
let re = /multiple words of text/
let re = /multiple words/
let re = /(.*)c(.*)/
let re = /\\((.*), (.*)\\)/
let re = /[k]/
let re = /a[-]?c/
let re = /(abc)\\1/
let re = /([a-c]*)\\1/
let re = /^(.+)?B/
let re = /(a+).\\1$/
let re = /^(a+).\\1$/
let re = /(abc)\\1/
let re = /([a-c]+)\\1/
let re = /(a)\\1/
let re = /(a+)\\1/
let re = /(a+)+\\1/
let re = /(a).+\\1/
let re = /(a)ba*\\1/
let re = /(aa|a)a\\1$/
let re = /(a|aa)a\\1$/
let re = /(a+)a\\1$/
let re = /([abc]*)\\1/
let re = /(a)(b)c|ab/
let re = /(a)+x/
let re = /([ac])+x/
// let re = /([^/]*/)*sub1//
let re = /([^.]*)\\.([^:]*):[T ]+(.*)/
let re = /([^N]*N)+/
let re = /([^N]*N)+/
let re = /([abc]*)x/
let re = /([abc]*)x/
let re = /([xyz]*)x/
let re = /(a)+b|aac/

// Test symbolic groups

let re = /(?P<i d>aaa)a/
let re = /(?P<id>aaa)a/
let re = /(?P<id>aa)(?P=id)/
let re = /(?P<id>aa)(?P=xd)/

// Test octal escapes/memory references

let re = /\\1/
let re = /\\09/
let re = /\\141/
let re = /(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)\\119/

// All tests from Perl

let re = /abc/
let re = /abc/
let re = /abc/
let re = /abc/
let re = /abc/
let re = /abc/
let re = /ab*c/
let re = /ab*bc/
let re = /ab*bc/
let re = /ab*bc/
let re = /ab{0,}bc/
let re = /ab+bc/
let re = /ab+bc/
let re = /ab+bc/
let re = /ab{1,}bc/
let re = /ab+bc/
let re = /ab{1,}bc/
let re = /ab{1,3}bc/
let re = /ab{3,4}bc/
let re = /ab{4,5}bc/
let re = /ab?bc/
let re = /ab?bc/
let re = /ab{0,1}bc/
let re = /ab?bc/
let re = /ab?c/
let re = /ab{0,1}c/
let re = /^abc$/
let re = /^abc$/
let re = /^abc/
let re = /^abc$/
let re = /abc$/
let re = /^/
let re = /$/
let re = /a.c/
let re = /a.c/
let re = /a.*c/
let re = /a.*c/
let re = /a[bc]d/
let re = /a[bc]d/
let re = /a[b-d]e/
let re = /a[b-d]e/
let re = /a[b-d]/
let re = /a[-b]/
let re = /a[b-]/
let re = /a[b-a]/
let re = /a[]b/
let re = /a]/
let re = /a[]]b/
let re = /a[^bc]d/
let re = /a[^bc]d/
let re = /a[^-b]c/
let re = /a[^-b]c/
let re = /a[^]b]c/
let re = /a[^]b]c/
let re = /ab|cd/
let re = /ab|cd/
let re = /()ef/
// let re = /*a/
let re = /(*)b/
let re = /$b/
let re = /a\\/
let re = /a\\(b/
let re = /a\\(*b/
let re = /a\\(*b/
let re = /a\\\\b/
let re = /((a))/
let re = /(a)b(c)/
let re = /a+b+c/
let re = /a{1,}b{1,}c/
let re = /a**/
let re = /a.+?c/
let re = /(a+|b)*/
let re = /(a+|b){0,}/
let re = /(a+|b)+/
let re = /(a+|b){1,}/
let re = /(a+|b)?/
let re = /(a+|b){0,1}/
let re = /)(/
let re = /[^ab]*/
let re = /abc/
let re = /a*/
let re = /([abc])*d/
let re = /([abc])*bcd/
let re = /a|b|c|d|e/
let re = /(a|b|c|d|e)f/
let re = /abcd*efg/
let re = /ab*/
let re = /ab*/
let re = /(ab|cd)e/
let re = /[abhgefdc]ij/
let re = /^(ab|cd)e/
let re = /(abc|)ef/
let re = /(a|b)c*d/
let re = /(ab|ab*)bc/
let re = /a([bc]*)c*/
let re = /a([bc]*)(c*d)/
let re = /a([bc]+)(c*d)/
let re = /a([bc]*)(c+d)/
let re = /a[bcd]*dcdcde/
let re = /a[bcd]+dcdcde/
let re = /(ab|a)b*c/
let re = /((a)(b)c)(d)/
let re = /[a-zA-Z_][a-zA-Z0-9_]*/
let re = /^a(bc+|b[eh])g|.h$/
let re = /(bc+d$|ef*g.|h?i(j|k))/
let re = /(bc+d$|ef*g.|h?i(j|k))/
let re = /(bc+d$|ef*g.|h?i(j|k))/
let re = /(bc+d$|ef*g.|h?i(j|k))/
let re = /(bc+d$|ef*g.|h?i(j|k))/
let re = /((((((((((a))))))))))/
let re = /((((((((((a))))))))))\\10/
// Python does not have the same rules for \\41 so this is a syntax error
//let re = /((((((((((a))))))))))\\41/
//let re = /((((((((((a))))))))))\\41/
let re = /((((((((((a))))))))))\\41/
let re = /(?i)((((((((((a))))))))))\\41/
let re = /(((((((((a)))))))))/
let re = /multiple words of text/
let re = /multiple words/
let re = /(.*)c(.*)/
let re = /\\((.*), (.*)\\)/
let re = /[k]/
let re = /a[-]?c/
let re = /(abc)\\1/
let re = /([a-c]*)\\1/
let re = /a(?!b)./
let re = /a(?=d)./
let re = /a(?=c|d)./
let re = /a(?:b|c|d)(.)/
let re = /a(?:b|c|d)*(.)/
let re = /a(?:b|c|d)+?(.)/
let re = /a(?:b|(c|e){1,2}?|d)+?(.)/
let re = /^(.+)?B/

// lookbehind: split by : but not if it is escaped by -.
let re = /(?<!-):(.*?)(?<!-):/
// escaping with \ as we know it
let re = /(?<!\\\\):(.*?)(?<!\\\\):/
// terminating with ' and escaping with ? as in edifact
let re = /(?<!\\?)'(.*?)(?<!\\?)'/

// Comments using the (?#...) syntax

let re = /w(?# comment/
let re = /w(?# comment 1)xy(?# comment 2)z/

// Check odd placement of embedded pattern modifiers

// not an error under PCRE/PRE:
let re = /(?i)w/
//let re = /w(?i)/

// Comments using the x embedded pattern modifier

//     ("""(?x)w# comment 1
//         x y
// // comment 2
//         z""", 'wxyz', SUCCEED, 'found', 'wxyz'),

// using the m embedded pattern modifier

//     ('^abc', """jkl
// abc
// xyz""", FAIL),
//     ('(?m)^abc', """jkl
// abc
// xyz""", SUCCEED, 'found', 'abc'),

//     ('(?m)abc$', """jkl
// xyzabc
// 123""", SUCCEED, 'found', 'abc'),

// using the s embedded pattern modifier

let re = /a.b/
let re = /(?s)a.b/

// test \w, etc. both inside and outside character classes

let re = /\\w+/
let re = /[\\w]+/
let re = /\\D+/
let re = /[\\D]+/
let re = /[\\da-fA-F]+/
// not an error under PCRE/PRE:
//let re = /[\\d-x]/
let re = /([\s]*)([\S]*)([\s]*)/
let re = /(\s*)(\S*)(\s*)/

let re = /\xff/
// new \x semantics
let re = /\x00ff/
//let re = /\x00ff/
let re = /\t\n\v\r\f\a/
let re = /\t\n\v\r\f\a/
let re = /\t\n\v\r\f\a/
let re = /[\t][\n][\v][\r][\f][\b]/

//
// post-1.5.2 additions

// xmllib problem
let re = /(([a-z]+):)?([a-z]+)$/
// bug 110866: reference to undefined group
let re = /((.)\1+)/
// bug 111869: search (PRE/PCRE fails on this one, SRE doesn't)
let re = /.*d/
// bug 112468: various expected syntax errors
let re = /(', '/
let re = /[\41]/
// bug 114033: nothing to repeat
let re = /(x?)?/
// bug 115040: rescan if flags are modified inside pattern
let re = /(?x) foo /
// bug 115618: negative lookahead
let re = /(?<!abc)(d.f)/
// bug 116251: character class bug
let re = /[\w-]+/
// bug 123769+127259: non-greedy backtracking bug
let re = /.*?\S *:/
let re = /a[ ]*?\ (\d+).*/
let re = /a[ ]*?\ (\d+).*/
// bug 127259: \Z shouldn't depend on multiline mode
let re = /(?ms).*?x\s*\Z(.*)/
// bug 128899: uppercase literals under the ignorecase flag
let re = /(?i)M+/
let re = /(?i)m+/
let re = /(?i)[M]+/
let re = /(?i)[m]+/
// bug 130748: ^* should be an error (nothing to repeat)
let re = /^*/
// bug 133283: minimizing repeat problem
let re = /"(?:\\"|[^"])*?"/
// bug 477728: minimizing repeat problem
let re = /^.*?$/
// bug 483789: minimizing repeat problem
let re = /a[^>]*?b/
// bug 490573: minimizing repeat problem
let re = /^a*?$/
// bug 470582: nested groups problem
let re = /^((a)c)?(ab)$/
// another minimizing repeat problem (capturing groups in assertions)
let re = /^([ab]*?)(?=(b)?)c/
let re = /^([ab]*?)(?!(b))c/
let re = /^([ab]*?)(?<!(a))c/

let re = /\.[^/.]+$/

// Leading ']' is literal; '/' inside class must not terminate
let re = /[^]]/
let re = /[/]/

// Additional leading ']' edge cases
let re = /[]]/
let re = /[\]]/
let re = /[[]]/
