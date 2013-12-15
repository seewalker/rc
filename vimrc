                                "Plugin Stuff
execute pathogen#infect()
filetype plugin indent on
":SCROLL
"frisk, gundo.
let mapleader = '-'
let maplocalleader = "="
let $PAGER='' "Part of resetting man's pager from less to vim
let g:posHoldReg = 'n'
let g:spacing = 4

syntax enable
colorscheme default
set nocompatible
set smartindent
set smartcase
set expandtab
execute "set tabstop=" . g:spacing
execute "set shiftwidth=" . g:spacing
set nohlsearch
set virtualedit="" "if set to all, allows cursor to go where there are not characters.
set hidden       "Don't ask to write buffer before i switch to a new buffer.
set autowrite
set laststatus=2 "laststatus is overriden by the statusline plugin.
set undolevels=1001 "the default is 1000. Can't be too cautious.
set statusline=  "the presence of my statusline shows that the plugin is
                 "read later than this nullification.
set autochdir "makes the cwd of a buffer the directory of its file (this may
              "have side-effects with plugins
set novisualbell
set confirm "rather than spitting errors, ask if i know what i'm doing
set encoding=utf-8
set columns=100
set showcmd "puts normal mode commands so far in the command-line section.
set backspace=2 "allows backspace to delete newline characters.
set notildeop "with tildeop, tilde becomes a vim verb. The delay in waiting
              "for the motion makes it not worth it.
set timeoutlen=500 "less than the default, just to see how it goes
set nostartofline "prevents <C-U> and <C-D> from modifying horizontal
                  "cursor position
set clipboard=unnamedplus "the 'xclip' clipboard, to which I can pipe.
                          "Text-Editing time-savers
"Swaps character under cursor with next character. This is a frequent fix
"for typos. Later, in this rcfile, <localleader>s is mapped to calling this
"function.
function Swap()
    let b:currentline = getline(".")
    let b:currentchar = b:currentline[col(".")-1]
    let b:nextchar = b:currentline[col(".")]
    execute "normal r" . b:nextchar
    execute "normal lr" . b:currentchar . "h"
endfunction
     
function ToggleRelativeNumber()
    if &relativenumber
        set norelativenumber
    else
        set relativenumber
    endif
endfunction
 
"postcondition: normal mode
function BsTab()
    let i = 0
    normal i
    "there is a global variable g:spacing, but since it may be
    "overwritten, it is best to compare i to tabstop.
    while i < &tabstop
        execute "normal a\<BS>"
        let i += 1
    endwhile
    normal l
endfunction

"Trims tailing spaces from the end of lines.
function TrimSpaces()
    % s/\s\s*$//
endfunction

autocmd BufReadPost *
   \ if line("'\"") > 0 && line("'\"") <= line("$") |
   \ execute "normal! g`\"" |
   \ endif
"Next two lines are handy for changing the left hand side or right hand side
"of an assignment statement in c-like languages.
onoremap lhs :<c-u>execute "normal! ^vt="
onoremap rhs :<c-u>execute "normal! $vT="
"mapping jk to <ESC> is highly ergonomical and not a problem so long as
"one is not texting like a teenager.
inoremap jk <ESC>
vnoremap jk <ESC>
"The mapping below provides an 'undo' of actions so far done in insert mode,
"leaving one still "in insert mode.
inoremap <c-l> <ESC>ui
"When making headings, one often wants to center the current line and
"keep the next line left-justified.
inoremap cxc <ESC>:center<CR>j:left<CR>i
inoremap <localleader><BS> <ESC>l:call BsTab()<CR>i
nnoremap cxc <ESC>:center<CR>j:left<CR>i
nnoremap <localleader>s :call Swap()<CR>
"For making blank lines without leaving normal mode.
nnoremap <localleader>o o<ESC>
nnoremap <localleader>O O<ESC>
"On my keyboard, the first letter registers as upper-case if I enter command
"line mode quickly. Perhaps this is a problem with apple keyboards.
"These save a lot of red popping up.
command W w
command Q q
command Wq wq
"Plugin commands are long, so here are some abbreviations.
command St SyntasticToggleMode
command Gt GundoToggle
command! -nargs=+ Calc :py print <args>
python from math import *
"Opens up all files passed to vim as command line arguments in their
"own tab.
autocmd VimEnter * argdo tabedit
autocmd VimEnter * tabclose "This is here because the above command creates
"a tab for an empty file
"
" :args    shows list of arguments

"ex commands are anything simply starting with a colon
":changes   list of changes.
":chdir     changes working directory
"

"A stub for writing vim functions with the python interface. If you are
"confused by the python interface not working, try looking at the results of
"this ex-command: echo has('python'). If it returns 0, you will need to
"recompile vim so that it is configured with support for the python interface.
function PyRowCol()
python << endpython
import vim
(row,col) = vim.current.window.cursor
print row, col
endpython
endfunction

   "Functions That Allow Operator-Pending Commenting-in and Commenting-out.
function! Com(commentSign, markOne, markTwo)
   let comment = ComStr() . ' '
   if a:commentSign == 1
      execute "normal! '" . a:markOne . "_\<C-V>'" . a:markTwo . 'I' . comment
   elseif a:commentSign == 0
      "this is fucking unreadable
      execute "'".a:markOne",'".a:markTwo . 's/^\(\s*\)' . escape(comment,'/') . '/\1/e'
   endif
endfunction

function DoCommentOp(type)
   call Com(1,'[',']')
endfunction

function UnCommentOp(type)
   call Com(0,'[',']')
endfunction

function ComStr()
   if (&ft=='c' || &ft=='cpp')
      return '//'
   elseif (&ft=='haskell')
      return '--'
   elseif (&ft=='perl' || &ft=='python' || &ft=='make' || &ft=='sh')
      return '#'
   elseif (&ft=='vim')
      return '"'
   else
      return ''
   endif
endfunction

"Before one can use those operator-pending comment operations, one has to
"set opfunc. Since this is error prone, here are some reliable mappings.
nnoremap <localleader>c :set opfunc=DoCommentOp<CR>g@
nnoremap <localleader>C :set opfunc=UnCommentOp<CR>g@
vnoremap <localleader>c :call Com(1,'<','>')<CR>
vnoremap <localleader>C :call Com(0,'<','>')<CR>
nnoremap <localleader>h :set cursorcolumn
nnoremap <localleader>H :set nocursorcolumn
nnoremap <localleader>r :call ToggleRelativeNumber()<CR>

"My highlight groups
highlight alexTrailingWhitespace ctermbg=Blue
"this one allows number of columns being over to be more of a warning
"and less of an assertion.
highlight alexOverLength ctermbg=Yellow ctermfg=White
highlight alexMakeTabGroup ctermbg=Red
augroup HighLighting
    autocmd!
    autocmd InsertLeave * match alexTrailingWhitespace /\s\+$/
    "note : I should generalize this to adjust to columns variable
    if(&virtualedit != "")
        autocmd InsertLeave * match alexOverLength /\%81v.\+/
    endif
    if(&filetype == 'make')
        autocmd InsertLeave * match alexMakeTabGroup /^\t\+/
    endif   
    autocmd InsertLeave * redraw
augroup END   
augroup FileTypes
    autocmd!
    autocmd BufEnter *.pro set filetype=prolog
    autocmd BufEnter *.b set filetype=brainfuck
    autocmd BufEnter *.b set matchpairs=[:]
    autocmd FileType c ab pr printf
    autocmd FileType c,cpp set cindent
    autocmd FileType c,cpp  ab inc include
    "Preventing spaces/tabs mishaps in Makefiles.
    autocmd FileType make set noexpandtab "this overrides my default expandtab
    autocmd FileType make match MakeTabGroup /^\t\t*/
    autocmd FileType haskell set cinwords=where,do
    autocmd FileType html set matchpairs+=<:>
    "Making local foldmethods
    autocmd FileType python setlocal foldmethod=indent
    autocmd FileType python set cursorcolumn
augroup END
                            "Vim Filesystem Info.
"~/.vim/colors/
"   the color command looks for colorschemes here
"~/.vim/plugin
"   files in here will be run once every time Vim starts. They are meant to
"   contain startup code.
"~/.vim/ftdetect
"   functionally equivalent to plugin dir. These files should have filetype
"   specific settings, however.
"~/ftplugin
"   iff the name of the file is equal to the filetype variable, that file will
"run.
"This system is handy for my customizations, but it quickly becomes a mess
"with downloaded plugins. For that reason, the pathogen plugin exists. It
"handles namespacing issues by adding stuff to the runtimepath.

"Sourcing does not get rid of current configuration, it adds to it.
" . is for string concatentation
"Programming in vimscript:
"& means interpret it as an option, not a variable. e.g. let &expandtab = 0
"@ means interpret it as a register. e.g. let @e = 'mcsquared'
"Scoping
"  b:var    b:var is a variable local to the current buffer.
"  a:var    a:var is an argument in a function. Can be called by name or as
"     positional argument
"  g:var    g:var is a global variable (among buffers) (this is the default)
"     it is not, however, persistent among sessions.
"I can treat things as a variable when a hardcoded string is expected by
"doing the command in an execute string with the variable concatenated into
"the command string.
"when mapping with searches, I need to conclude the searches with a \<CR>
"in execute strings, I need to quote the < > characters to give them their
"special meaning.
"the current file symbol, %, is technically a register name.
