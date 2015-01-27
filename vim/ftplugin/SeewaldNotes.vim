"Global Variables
let g:notesAutocomplete = 1

"Overridden Settings
set matchpairs+=`:'
set matchpairs+=<:>  "this is for BNF stuff

"Autocompletion. This may be turned on and off.
if g:notesAutocomplete
    inoremap `? `??'<Left><Left>
    inoremap `! `!!'<Left><Left>
    inoremap `# `##'<Left><Left>
    inoremap `` ``''<Left><Left>
    inoremap `/ `//'<Left><Left>
    inoremap `~ `~~'<Left><Left>
endif

"highlighting
highlight notesUnresolvedQuestion ctermfg=1 guifg=#1a2355 guibg=#e2e1b3
highlight notesResolutionAnswer ctermfg=2   guifg=#b5d25b guibg=#e2e1b3
highlight notesSubjectivity ctermfg=3       guifg=#707a46 guibg=#e2e1b3
highlight notesCode ctermfg=4               guifg=#168487 guibg=#e2e1b3
highlight notesDefinition ctermfg=5      guifg=#36801b guibg=#e2e1b3
highlight notesJargon ctermfg=6             guifg=#b44d8b guibg=#e2e1b3
highlight notesFunc ctermfg=6             guifg=#b44d8b guibg=#e2e1b3

augroup notesHighlighting
    autocmd!
    autocmd BufEnter * syntax spell notoplevel
    autocmd BufEnter * syntax region notesUnresolvedQuestion start="`?" end="?'" contains=ALL
    autocmd BufEnter * syntax region notesResolutionAnswer start="`!" end="!'" contains=ALL
    autocmd BufEnter * syntax region notesSubjectivity start="`#" end="#'" contains=ALL
    autocmd BufEnter * syntax region notesDefinition start="`/" end="/'" contains=ALL
    autocmd BufEnter * syntax region notesCode start="``" end= "''" contains=ALL
    autocmd BufEnter * syntax match notesFunc '\(/e\)*\(/ord\)*\(/proc\)*:$'
    autocmd BufEnter * syntax match notesFunc '^\s\s*\*'
augroup END
