" Vim syntax file
" Language: Celestia Star Catalogs
" Maintainer: Kevin Lauder
" Latest Revision: 26 April 2008

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword gryphKeywords if else while for over where sub return when or and not add del from in print read println break bfs dfs use
syn keyword gryphTypes int string float char bool
syn keyword gryphTodo contained TODO FIXME NOTE

" Matches

" Integer with - + or nothing in front
syn match gryphIdentifier '[a-zA-Z][a-zA-Z0-9\_]*[']*'
syn match gryphNumber '\d\+'
syn match gryphBool '\(false\|true\)'
syn match gryphComment "#.*$" contains=gryphTodo
syn match gryphError '[a-zA-Z][a-zA-Z0-9\_]*[']\+[^\('| |;\)]\+' " improve regex!

" Floating point number with decimal no E or e
syn match gryphNumber '[-+]\d\+\.\d*'

" Floating point like number with E and no decimal point (+,-)
syn match gryphNumber '[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+'
syn match gryphNumber '\d[[:digit:]]*[eE][\-+]\=\d\+'

" Floating point like number with E and decimal point (+,-)
syn match gryphNumber '[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'
syn match gryphNumber '\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'

" Regions

syntax region gryphString start=/\v"/ skip=/\v\\./ end=/\v"/
syntax region gryphChar start=/\v'/ skip=/\v\\./ end=/\v'/

let b:current_syntax = "gph"
hi def link gryphTodo           Todo
hi def link gryphComment        Comment
hi def link gryphKeywords       Statement
hi def link gryphTypes          Type
hi def link gryphString         String
hi def link gryphChar           Character
hi def link gryphNumber         Constant
hi def link gryphBool           Constant
hi def link gryphIdentifier     Identifier
hi def link gryphError          Error





