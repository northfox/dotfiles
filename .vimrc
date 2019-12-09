autocmd!
syntax enable

" for editing to vimrc
nnoremap <Space>. :<C-u>edit $MYVIMRC<Enter>
nnoremap <Space>s. :<C-u>source $MYVIMRC<Enter>

" highlight
autocmd WinEnter * setlocal cursorline
autocmd WinLeave * setlocal nocursorline

set number
set ruler
"set list
"set listchars=tab:>-,trail:-,nbsp:%,extends:>,precedes:<,eol:<
set incsearch
set hlsearch
set nowrap
set showmatch
set whichwrap=h,l
set nowrapscan
set ignorecase
set smartcase
set hidden
set history=2000
set autoindent
set expandtab
set tabstop=2
set shiftwidth=2
set helplang=en

colorscheme desert

nnoremap <Space>w  :<C-u>w<CR>
nnoremap <Space>q  :<C-u>q<CR>
nnoremap <Space>Q  :<C-u>q!<CR>

nnoremap <Space>h  ^
nnoremap <Space>l  $

nnoremap <Space>/  *<C-o>
noremap g<Space>/ g*<C-o>

nnoremap <expr> n <SID>search_forward_p() ? 'nzv' : 'Nzv'
nnoremap <expr> N <SID>search_forward_p() ? 'Nzv' : 'nzv'
vnoremap <expr> n <SID>search_forward_p() ? 'nzv' : 'Nzv'
vnoremap <expr> N <SID>search_forward_p() ? 'Nzv' : 'nzv'

function! s:search_forward_p()
  return exists('v:searchforward') ? v:searchforward : 1
endfunction


nnoremap <Space>o  :<C-u>for i in range(v:count1) \| call append(line('.'), '') \| endfor<CR>
nnoremap <Space>O  :<C-u>for i in range(v:count1) \| call append(line('.')-1, '') \| endfor<CR>

nnoremap <silent> tt  :<C-u>tabe<CR>
nnoremap <C-p>  gT
nnoremap <C-n>  gt

nnoremap <silent> <Esc><Esc> :<C-u>nohlsearch<CR>

nnoremap ZZ <Nop>
nnoremap ZQ <Nop>

nnoremap Q gq


onoremap aa  a>
onoremap ia  i>
onoremap ar  a]
onoremap ir  i]
onoremap ad  a"
onoremap id  i"


inoremap jk  <Esc>


nnoremap gs  :<C-u>%s///g<Left><Left><Left>
vnoremap gs  :s///g<Left><Left><Left>


" Emacs-like keybind
noremap  <C-p> <Up>
noremap  <C-n> <Down>
cnoremap <C-a> <Home>
cnoremap <C-b> <Left>
cnoremap <C-d> <Del>
cnoremap <C-e> <End>
cnoremap <C-f> <Right>
cnoremap <C-h> <Backspace>
cnoremap <C-k> <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos() - 2]<CR>
inoremap <C-a> <Home>
inoremap <C-b> <Left>
inoremap <C-d> <Del>
inoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-h> <Backspace>
inoremap <C-k> <C-o>D
inoremap <C-n> <Down>
inoremap <C-p> <Up>

" Refer to history in Command-line mode
cnoremap <C-p> <Up>
cnoremap <Up> <C-p>
cnoremap <C-n> <Down>
cnoremap <Down> <C-n>

" backupskip
set backupskip=/tmp/*,/private/tmp/*

"cnoremap <C-a> <Home>
"cnoremap <C-b> <Left>
"cnoremap <C-d> <Del>
"cnoremap <C-e> <End>
"cnoremap <C-f> <Right>
"cnoremap <C-h> <Backspace>
"cnoremap <C-k> <C-\>e 
"inoremap <C-a> <Home>
"inoremap <C-b> <Left>
"inoremap <C-d> <Del>
"inoremap <C-e> <End>
"inoremap <C-f> <Right>
"inoremap <C-h> <Backspace>
"inoremap <C-k> <C-o>
"inoremap <C-n> <Down>
"inoremap <C-p> <Up>
""cnoremap <C-f><Right>
"cnoremap <C-b>  <Left>
"cnoremap <C-a>  <C-b>
"cnoremap <C-e>  <C-e>
"cnoremap <C-u> <C-e><C-u>
"cnoremap <C-v> <C-f>a



" 
" 
" 
" \"---------------------------------------------------------------------------
" 
" 
" \" Copyright (C) 2011 KaoriYa/MURAOKA Taro
" 
" 
" \"#####表示設定#####
" set title \"編集中のファイル名を表示
" syntax on \"コードの色分け
" set tabstop=4 \"インデントをスペース4つ分に設定
" set smartindent \"オートインデント
" 
" \"#####検索設定#####
" set ignorecase \"大文字/小文字の区別なく検索する
" set smartcase \"検索文字列に大文字が含まれている場合は区別して検索する
" set wrapscan \"検索時に最後まで行ったら最初に戻る
" \"--------------------
" \"\" 基本的な設定
" \"--------------------
" \"新しい行のインデントを現在行と同じにする
" set autoindent 
" 
" \"バックアップファイルのディレクトリを指定する
" set backupdir=$HOME/vimbackup
" \"
" \"\"クリップボードをWindowsと連携する
" set clipboard=unnamed
" 
" \"vi互換をオフする
" \"\"スワップファイル用のディレクトリを指定する
" set directory=$HOME/vimbackup
" 
" \"タブの代わりに空白文字を指定する
" set expandtab
" \"
" \"\"変更中のファイルでも、保存しないで他のファイルを表示する
" set hidden
" 
" \"インクリメンタルサーチを行う
" set incsearch
" \"
" \"\"行番号を表示する
" set number
" 
" \"閉括弧が入力された時、対応する括弧を強調する
" set showmatch
" \"
" \"\"新しい行を作った時に高度な自動インデントを行う
" set smarttab
" 
" \" grep検索を設定する
" set grepformat=%f:%l:%m,%f:%l%m,%f\ \ %l%m,%f
" set grepprg=grep\ -nh
" \"
" \" \" 検索結果のハイライトをEsc連打でクリアする
" nnoremap <ESC><ESC> :nohlsearch<CR>
" \" mapping関連カスタマイズ
" map ¥ <leader>
" noremap ; :
" noremap : ;
" if has('vim_starting')
"           set runtimepath+=~/.vim/bundle/neobundle.vim/
"   endif
" 
" call neobundle#rc(expand('~/.vim/bundle/'))
" \" let NeoBundle manage NeoBundle
" NeoBundle 'Shougo/neobundle.vim'
" \"
" \" add plugins
" \"
" filetype plugin on
" \"
" NeoBundleCheck
" NeoBundle 'davidhalter/jedi-vim'
" NeoBundle 'Shougo/neocomplete.vim'
" NeoBundle 'haya14busa/vim-easymotion'
" NeoBundle 'thinca/vim-quickrun'
" NeoBundle 'Shougo/neocomplcache'
" NeoBundle 'nathanaelkane/vim-indent-guides'
" 
" let g:jedi#auto_vim_configuration = 0
" 
" if !exists('g:neocomplete#force_omni_input_patterns')
"                   let g:neocomplete#force_omni_input_patterns = {}
"           endif
" 
" let g:neocomplete#force_omni_input_patterns.python = '\h\w*\|[^. \t]\.\w*'
" 
" \" ホームポジションに近いキーを使う
" let g:EasyMotion_keys='hjklasdfgyuiopqwertnmzxcvbHJKLASDFGYUIOPQWERTNMZXCVB'
" \" 「'」 + 何かにマッピング
" let g:EasyMotion_leader_key=\"'\"
" \" \" 1 ストローク選択を優先する
" let g:EasyMotion_grouping=1
" \" \" カラー設定変更
" hi EasyMotionTarget ctermbg=none ctermfg=red
" hi EasyMotionShade  ctermbg=none ctermfg=blue
" autocmd FileType python let b:did_ftplugin = 1
" 
" 
" let g:lightline = {
"       \ 'colorscheme': 'powerline',
"       \ 'mode_map': {'c': 'NORMAL'},
"       \ 'active': {
"       \   'left': [ ['mode', 'paste'], ['fugitive', 'filename', 'cakephp', 'currenttag', 'anzu'] ]
"       \ },
"       \ 'component': {
"       \   'lineinfo': ' %3l:%-2v',
"       \ },
"       \ 'component_function': {
"       \   'modified': 'MyModified',
"       \   'readonly': 'MyReadonly',
"       \   'fugitive': 'MyFugitive',
"       \   'filename': 'MyFilename',
"       \   'fileformat': 'MyFileformat',
"       \   'filetype': 'MyFiletype',
"       \   'fileencoding': 'MyFileencoding',
"       \   'mode': 'MyMode',
"       \   'anzu': 'anzu#search_status',
"       \   'currenttag': 'MyCurrentTag',
"       \   'cakephp': 'MyCakephp',
"       \ }
"       \ }
" 
" 
" function! MyModified()
"   return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
" endfunction
" 
" function! MyReadonly()
"   return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? ' ' : ''
" endfunction
" 
" function! MyFilename()
"   return ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
"         \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
"         \  &ft == 'unite' ? unite#get_status_string() :
"         \  &ft == 'vimshell' ? vimshell#get_status_string() :
"         \ '' != expand('%:t') ? expand('%:t') : '[No Name]') .
"         \ ('' != MyModified() ? ' ' . MyModified() : '')
" endfunction
" 
" function! MyFugitive()
"   try
"     if &ft !~? 'vimfiler\|gundo' && exists('*fugitive#head') && strlen(fugitive#head())
"       return ' ' . fugitive#head()
"     endif
"   catch
"   endtry
"   return ''
" endfunction
" 
" function! MyFileformat()
"   return winwidth(0) > 70 ? &fileformat : ''
" endfunction
" 
" function! MyFiletype()
"   return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
" endfunction
" 
" function! MyFileencoding()
"   return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
" endfunction
" 
" function! MyMode()
"   return winwidth(0) > 60 ? lightline#mode() : ''
" endfunction
" 
" function! MyCurrentTag()
"   return tagbar#currenttag('%s', '')
" endfunction
" 
" function! MyCakephp()
"   return exists('*cake#buffer') ? cake#buffer('type') : ''
" endfunction
" 
" NeoBundle 'Shougo/unite.vim'
" NeoBundle 'Shougo/vimproc'
" NeoBundle 'majutsushi/tagbar'
" NeoBundle 'itchyny/lightline.vim'
" 
" let g:lightline = {
"       \ 'colorscheme': 'wombat'
"       \ }
" set list
" set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
" " 全角スペース・行末のスペース・タブの可視化
" if has("syntax")
"     syntax on
" 
"     " PODバグ対策
"     syn sync fromstart
" 
"     function! ActivateInvisibleIndicator()
"         " 下の行の"　"は全角スペース
"                 syntax match InvisibleJISX0208Space "　" display containedin=ALL
"                         highlight InvisibleJISX0208Space term=underline ctermbg=Blue guibg=darkgray gui=underline
"                                 "syntax match InvisibleTrailedSpace "[ \t]$" display containedin=ALL
"         "highlight InvisibleTrailedSpace term=underline ctermbg=Red guibg=NONE gui=undercurl guisp=darkorange
"                 "syntax match InvisibleTab "\t" display containedin=ALL
"         "highlight InvisibleTab term=underline ctermbg=white gui=undercurl guisp=darkslategray
"             endfunction
" 
"     augroup invisible
"             autocmd! invisible
"                     autocmd BufNew,BufRead * call ActivateInvisibleIndicator()
"                         augroup END
"                         endif
" 
" " 文字コードの自動認識
" if &encoding !=# 'utf-8'
"   set encoding=japan
"   set fileencoding=japan
" endif
" if has('iconv')
"   let s:enc_euc = 'euc-jp'
"   let s:enc_jis = 'iso-2022-jp'
"   " iconvがeucJP-msに対応しているかをチェック
"     if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
"         let s:enc_euc = 'eucjp-ms'
"             let s:enc_jis = 'iso-2022-jp-3'
"               " iconvがJISX0213に対応しているかをチェック
"   elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
"     let s:enc_euc = 'euc-jisx0213'
"     let s:enc_jis = 'iso-2022-jp-3'
"   endif
"   " fileencodingsを構築
"     if &encoding ==# 'utf-8'
"         let s:fileencodings_default = &fileencodings
"             let &fileencodings = s:enc_jis .','. s:enc_euc .',cp932'
"                 let &fileencodings = &fileencodings .','. s:fileencodings_default
"                     unlet s:fileencodings_default
"                       else
"                           let &fileencodings = &fileencodings .','. s:enc_jis
"                               set fileencodings+=utf-8,ucs-2le,ucs-2
"                                   if &encoding =~# '^\(euc-jp\|euc-jisx0213\|eucjp-ms\)$'
"                                         set fileencodings+=cp932
"                                               set fileencodings-=euc-jp
"                                                     set fileencodings-=euc-jisx0213
"                                                           set fileencodings-=eucjp-ms
"                                                                 let &encoding = s:enc_euc
"                                                                       let &fileencoding = s:enc_euc
"                                                                           else
"                                                                                 let &fileencodings = &fileencodings .','. s:enc_euc
"                                                                                     endif
"                                                                                       endif
"                                                                                         " 定数を処分
"   unlet s:enc_euc
"   unlet s:enc_jis
" endif
" 
" " 改行コードの自動認識
" set fileformats=unix,dos,mac
" " □とか○の文字があってもカーソル位置がずれないようにする
" if exists('&ambiwidth')
"   set ambiwidth=double
" endif
