" Vim syntax file
" Copyright (c) 2003, 2004 Tom Hawkins
" Language:     Confluence
" Maintainer:   Tom Hawkins
" Last Change:
" URL:          http://www.launchbird.com/misc/cf.vim

if exists("b:current_syntax")
  finish
endif

" Special
syn match cfSpecial "_"

" Identifiers
syn match    cfIdentifier /\<\(\u\|\l\)\w*\>/

" Errors
syn match    cfBraceErr   "}"
syn match    cfBrackErr   "\]"
syn match    cfParenErr   ")"
syn match    cfCommentErr "\*)"

" Some convenient clusters
syn cluster  cfAllErrs contains=cfBraceErr,cfBrackErr,cfParenErr,cfCommentErr
syn cluster  cfContained contains=cfTodo

" Enclosing delimiters
syn region   cfEncl transparent matchgroup=cfKeyword start="(" matchgroup=cfKeyword end=")" contains=ALLBUT,@cfContained,cfParenErr
syn region   cfEncl transparent matchgroup=cfKeyword start="{" matchgroup=cfKeyword end="}"  contains=ALLBUT,@cfContained,cfBraceErr
syn region   cfEncl transparent matchgroup=cfKeyword start="\[" matchgroup=cfKeyword end="\]" contains=ALLBUT,@cfContained,cfBrackErr

" Comments
syn region   cfComment start="(\*" end="\*)" contains=cfComment,cfTodo
syn region   cfDocComment start="(#" end="#)" contains=cfDocComment,cfTodo
syn keyword  cfTodo contained TODO FIXME XXX

syn keyword  cfKeyword comp prim if ef else end with is
syn keyword  cfKeyword local component
syn keyword  cfKeyword environment import rootenvironment fileloc

syn keyword  cfBoolean  true false

syn match cfOperator "'"
syn match cfOperator "\."
syn match cfOperator "-"
syn match cfOperator "'-'"

syn region   cfString   start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match    cfNil    "\[]"
syn match    cfEmpty  "()"
syn match    cfInteger       "\<\=\-\?\d\+\>"
syn match    cfInteger       "\<[0|1]x\x\+\>"
syn match    cfInteger       "\<[0|1]b[0|1]\+\>"
syn match    cfInteger       "@\S"
syn match    cfFloat         "\<\=\-\?\d\+\.\d*\([eE][\-]\=\d\+\)*\>"
syn match    cfConst         "'\([0|1]*\|[0|1]x\x*\|[0|1]b[0|1]*\)'"

syn match cfSpecial "\$"
syn match cfSpecial "="
syn match cfSpecial ":"

syn match cfOperator "`!`"
syn match cfOperator "`X`"
syn match cfOperator "`G`"
syn match cfOperator "`F`"
syn match cfOperator "`&&`"
syn match cfOperator "`^^`"
syn match cfOperator "`||`"
syn match cfOperator "`<->`"
syn match cfOperator "`->`"
syn match cfOperator "`U`"
syn match cfOperator "`W`"
syn match cfOperator "`B`"
syn match cfOperator "`V`"

syn keyword cfOperator then
syn match cfOperator "'then'"
syn match cfOperator "'else'"
syn match cfOperator "||"
syn match cfOperator "&&"
syn match cfOperator "|"
syn match cfOperator "'|'"
syn match cfOperator "\^"
syn match cfOperator "'\^'"
syn match cfOperator "&"
syn match cfOperator "'&'"
syn match cfOperator "!"
syn match cfOperator "=="
syn match cfOperator "!="
syn match cfOperator "'=='"
syn match cfOperator "'!='"
syn match cfOperator "++"
syn match cfOperator "'++'"
syn match cfOperator "::"
syn match cfOperator "#"
syn match cfOperator "'#'"
syn match cfOperator "<"
syn match cfOperator ">"
syn match cfOperator "<="
syn match cfOperator ">="
syn match cfOperator "'<'"
syn match cfOperator "'>'"
syn match cfOperator "'<='"
syn match cfOperator "'>='"
syn match cfOperator "'<+'"
syn match cfOperator "'>+'"
syn match cfOperator "'<=+'"
syn match cfOperator "'>=+'"
syn match cfOperator "<<"
syn match cfOperator ">>"
syn match cfOperator "'<<'"
syn match cfOperator "'>>'"
syn match cfOperator "'>>+'"
syn match cfOperator "+"
syn match cfOperator "'+'"
syn match cfOperator "\*"
syn match cfOperator "/"
syn match cfOperator "%"
syn match cfOperator "'\*'"
syn match cfOperator "'/'"
syn match cfOperator "'%'"
syn match cfOperator "'\*+'"
syn match cfOperator "\*\*"
syn match cfOperator "'\*\*'"
syn match cfOperator "\~"
syn keyword cfOperator head
syn keyword cfOperator tail
syn keyword cfOperator length
syn keyword cfOperator width
syn match cfOperator "'\~'"
syn match cfOperator "'msb'"
syn match cfOperator "'msbs'"
syn match cfOperator "'lsb'"
syn match cfOperator "'lsbs'"
syn match cfOperator "(\*)"
syn match cfOperator "(\*\*)"
syn match cfOperator "(#)"

syn match cfSpecial "<-"
syn match cfSpecial "->"

" Synchronization
syn sync minlines=50
syn sync maxlines=500

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet

if version >= 508 || !exists("did_cf_syntax_inits")
  if version < 508
    let did_cf_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

"  HiLink cfIdentifier   Identifier

  HiLink cfBraceErr     Error
  HiLink cfBrackErr     Error
  HiLink cfParenErr     Error

  HiLink cfCommentErr   Error

  HiLink cfComment      Comment
  HiLink cfDocComment   Comment

  HiLink cfKeyword      Keyword
  HiLink cfOperator     Keyword

  HiLink cfNil          Constant
  HiLink cfEmpty        Constant
  HiLink cfBoolean      Boolean
  HiLink cfInteger      Number
  HiLink cfFloat        Float
  HiLink cfConst        Constant
  HiLink cfString       String

  HiLink cfTodo         Todo

  HiLink cfEncl         Keyword

  HiLink cfSpecial      Type

  HiLink cfSeperator     Special

  delcommand HiLink
endif

let b:current_syntax = "confluence"


