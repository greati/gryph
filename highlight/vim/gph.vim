" Vim syntax file
" Language: Celestia Star Catalogs
" Maintainer: Kevin Lauder
" Latest Revision: 26 April 2008

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword gryphKeywords if else while for over where fun proc return
syn keyword gryphKeywords int string float char bool
syn keyword gryphTodo contained TODO FIXME NOTE

" Matches

" Integer with - + or nothing in front
syn match gryphNumber '\d\+'
syn match gryphNumber '[-+]\d\+'
syn match gryphBool '\(false\|true\)'
syn match gryphComment "#.*$" contains=gryphTodo

" Floating point number with decimal no E or e
syn match gryphNumber '[-+]\d\+\.\d*'

" Floating point like number with E and no decimal point (+,-)
syn match gryphNumber '[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+'
syn match gryphNumber '\d[[:digit:]]*[eE][\-+]\=\d\+'

" Floating point like number with E and decimal point (+,-)
syn match gryphNumber '[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'
syn match gryphNumber '\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'

" Regions

syn region gryphDescBlock start="{" end="}" fold transparent contains= gryphKeywords, gryphNumber, gryphBool, gryphString, gryphChar
syn region gryphString start='"' end='"' contained
syn region gryphChar start="'" end="'" contained

let b:current_syntax = "gph"
hi def link gryphTodo       Todo
hi def link gryphComment    Comment
hi def link gryphKeywords   Statement
hi def link gryphString     Constant
hi def link gryphChar       Constant
hi def link gryphNumber     Constant
hi def link gryphBool       Constant





