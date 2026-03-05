highlight clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "alabastrine-bg"

" UI
hi! Normal        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! LineNr        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! CursorLineNR  ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! CursorLine    ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! CursorColumn  ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! SignColumn    ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! FoldColumn    ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Folded        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! VertSplit     ctermfg=8     ctermbg=NONE  cterm=NONE
hi! ColorColumn   ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! TabLine       ctermfg=NONE  ctermbg=8     cterm=NONE
hi! TabLineFill   ctermfg=NONE  ctermbg=8     cterm=NONE
hi! TabLineSel    ctermfg=NONE  ctermbg=NONE  cterm=bold
hi! StatusLine    ctermfg=NONE  ctermbg=NONE  cterm=reverse
hi! StatusLineNC  ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! WildMenu      ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Title         ctermfg=NONE  ctermbg=NONE  cterm=bold
hi! ModeMsg       ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! MoreMsg       ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Visual        ctermfg=NONE  ctermbg=12    cterm=NONE
hi! VisualNOS     ctermfg=NONE  ctermbg=12    cterm=NONE
hi! MatchParen    ctermfg=NONE  ctermbg=12    cterm=NONE
hi! Search        ctermfg=NONE  ctermbg=11    cterm=NONE
hi! IncSearch     ctermfg=NONE  ctermbg=11    cterm=NONE
hi! NonText       ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! SpecialKey    ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Directory     ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Todo          ctermfg=NONE  ctermbg=11    cterm=NONE
hi! Error         ctermfg=1     ctermbg=NONE  cterm=bold
hi! ErrorMsg      ctermfg=1     ctermbg=NONE  cterm=bold
hi! WarningMsg    ctermfg=3     ctermbg=NONE  cterm=bold
hi! Ignore        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Underlined    ctermfg=NONE  ctermbg=NONE  cterm=underline
hi! Pmenu         ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! PmenuSel      ctermfg=NONE  ctermbg=12    cterm=NONE
hi! PmenuSbar     ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! PmenuThumb    ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! DiffAdd       ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! DiffChange    ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! DiffDelete    ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! DiffText      ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! SpellBad      ctermfg=NONE  ctermbg=NONE  cterm=underline
hi! SpellCap      ctermfg=NONE  ctermbg=NONE  cterm=underline
hi! SpellLocal    ctermfg=NONE  ctermbg=NONE  cterm=underline
hi! SpellRare     ctermfg=NONE  ctermbg=NONE  cterm=underline

" Syntax — Alabaster principle: background tinting (Sublime-style)
" 10=bright green  11=bright yellow  12=bright blue  13=bright magenta
hi! Comment         ctermfg=NONE  ctermbg=11    cterm=NONE
hi! String          ctermfg=NONE  ctermbg=10    cterm=NONE
hi! StringDelimiter ctermfg=NONE  ctermbg=10    cterm=NONE
hi! Character       ctermfg=NONE  ctermbg=13    cterm=NONE
hi! Number          ctermfg=NONE  ctermbg=13    cterm=NONE
hi! Boolean         ctermfg=NONE  ctermbg=13    cterm=NONE
hi! Float           ctermfg=NONE  ctermbg=13    cterm=NONE
hi! Constant        ctermfg=NONE  ctermbg=13    cterm=NONE
hi! Function        ctermfg=NONE  ctermbg=12    cterm=NONE

" Everything else → default
hi! Identifier    ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Statement     ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Conditional   ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Repeat        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Label         ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Operator      ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Keyword       ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Exception     ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Special       ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! SpecialChar   ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Tag           ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Delimiter     ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! SpecialComment ctermfg=NONE ctermbg=NONE  cterm=NONE
hi! Debug         ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! PreProc       ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Include       ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Define        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Macro         ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! PreCondit     ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Type          ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! StorageClass  ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Structure     ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi! Typedef       ctermfg=NONE  ctermbg=NONE  cterm=NONE
