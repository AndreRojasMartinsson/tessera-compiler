" Vim syntax file
" Language: tes
" Usage Instructions
" Put this file in .vim/syntax/tes.vim
" and add in your .vimrc file the next line:
" autocmd BufRead,BufNewFile *.tn set filetype=tes
if exists("b:current_syntax")
  finish
endif

" Enable commenting easily
setlocal commentstring=#\ %s

" Keywords
"syn keyword tesKW pub func import module out let const if for else while loop

syn keyword tesKw break continue pub return for while if else extern
syn keyword tesKw import nextgroup=tesModPath skipwhile skipempty
syn keyword tesKw module nextgroup=tesModPath skipwhile skipempty
syn keyword tesKw func nextgroup=tesFunctionName skipwhile skipempty
syn keyword tesKw let nextgroup=tesIdentifier skipwhile skipempty
syn keyword tesKw out nextgroup=tesString skipwhile skipempty

syn keyword tesBoolean true false

syn match tesModPath "\w\(\w\)*::[^<]"he=e-3,me=e-3
syn match tesModPathSep "::"

syn keyword tesTypedef def nextgroup=tesIdentifier skipwhile skipempty
syn keyword tesStructure struct enum union nextgroup=tesIdentifier skipwhile skipempty


" String
syn match     tesEscapeError   display contained /\\./
syn match     tesEscape        display contained /\\\([nrt0\\'"]\|x\x\{2}\)/
syn match     tesEscapeUnicode display contained /\\u{\%(\x_*\)\{1,6}}/
syn match     tesStringContinuation display contained /\\\n\s*/
syn region    tesString      matchgroup=tesStringDelimiter start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=tesEscape,tesEscapeUnicode,tesEscapeError,tesStringContinuation,@Spell
syn region    tesString      matchgroup=tesStringDelimiter start='b\?r\z(#*\)"' end='"\z1' contains=@Spell

" Types
syn keyword tesPrimitiveType usz isz u32 i32 u64 i64 double single str bool void


syn match tesDelimiter display /[:,\[\]\{\};\(\)]/

syn match tesOperator display "&&\|||\|<<\|>>\|<=\|>=\|==\|!="
syn match tesOperator display /[+-/%*]/
syn match tesOperator display /*<>\|+<>\|-<>\|\/<>\|%<>\|^<>\|&<>\||<>\|<>/
syn keyword tesOperator sizeof new

" Delimiters

syn keyword tesKw sizeof new

" Comment Items
syn match tesDocAttribute /\s@\w\+/ contained
syn keyword tesCommentItems TODO README XXX FIXME NOTE contained
syn region tesExample start=/`/ end=/`/ contained contains=tesKw,tesDelimiter,tesOperators,tesPrimitiveType
" Comments
syn region tesLineComment start=/#/ end=/\n/ contains=tesCommentItems,@Spell
syn region tesBlockComment start=/#\*/ end=/\*#/ contains=tesCommentItems,@Spell
syn region tesDocComment start=/###/ end=/###/ contains=tesCommentItems,tesDocAttribute,tesExample,@Spell

" Function
syn match tesFunctionName "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained
syn match tesFunctionCall    "\w\(\w\)*("he=e-1,me=e-1
syn match tesFunctionCall    "\w\(\w\)*::<"he=e-3,me=e-3 " foo::<T>();

" Identifiers
syn match tesIdentifier "[_a-zA-Z][_a-zA-Z0-9]*" display


" Number literals
syn match     tesDecNumber   display "\<[0-9][0-9_]*\%([iu]\%(sz\|32\|64\)\)\="
syn match     tesHexNumber   display "\<0x[a-fA-F0-9_]\+\%([iu]\%(sz\|32\|64\)\)\="
syn match     tesOctNumber   display "\<0o[0-7_]\+\%([iu]\%(sz\|32\|64\)\)\="
syn match     tesBinNumber   display "\<0b[01_]\+\%([iu]\%(sz\|32\|64\)\)\="

" Float literals
syn match     tesFloat       display "\<[0-9][0-9_]*\.\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\|\.\)\@!"
" To mark a number as a normal float, it must have at least one of the three things integral values don't have:
" a decimal point and more numbers; an exponent; and a type suffix.
syn match     tesFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=\(single\|double\)\="
syn match     tesFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\(single\|double\)\="
syn match     tesFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\=\(single\|double\)"

" Links
hi def link tesDecNumber tesNumber
hi def link tesHexNumber tesNumber
hi def link tesOctNumber tesNumber
hi def link tesBinNumber tesNumber

hi def link tesKw Keyword
hi def link tesTypedef Keyword
hi def link tesStructure Keyword
hi def link tesLineComment Comment
hi def link tesBlockComment Comment
hi def link tesDocComment Comment
hi def link tesPrimitiveType Type
hi def link tesNumber Number
hi def link tesFloat Float


hi def link tesEscape Special
hi def link tesEscapeUnicode tesEscape
hi def link tesEscapeError Error
hi def link tesStringContinuation Special
hi def link tesString String
hi def link tesStringDelimiter String

hi def link tesDocAttribute SpecialComment
hi def link tesDelimiter Delimiter
hi def link tesOperators Operator

hi def link tesModPath Include
hi def link tesBoolean Boolean
hi def link tesModPathSep Delimiter
hi def link tesFunctionName Function
hi def link tesFunctionCall Function
hi def link tesIdentifier Identifier



let b:current_syntax = "tes"

