" Description:	The demo-imitation of the "pace.vim" script
" Author:	Aliaksei Budavei (0x000c70 AT gmail DOT com)
" Version:	1.0
" Last Change:	2016-Oct-26
" Copyleft ())
"
" Dependencies:	cmdline_info, eval, reltime, and statusline features.
"
"		The "vimvat.txt" sonnet included.
"
" Usage:	Source the file: ":lcd %:p:h | so %".
"
" Notes:	In order to preview any other file, change s:demo.data
"		keys' values.
"
"		In order to adjust the typing pace, change s:demo.delay
"		values.
"
" Caveats:	The "winheight" option is set to 1.

let s:cpoptions	= &cpoptions						" {{{1
set cpoptions-=C					" Join line-breaks.

if !(has('reltime') && has('cmdline_info') && has('statusline') && len(reltime()) == 2)
	let &cpoptions	= s:cpoptions
	unlet s:cpoptions
	finish
endif

let s:demo	= {
	\ 'char':	0,
	\ 'sec':	0,
	\ 'gear':	4,
	\ 'micro':	len(reltime([0, 0], [0, -1])[1]),
	\ 'reg_z':	@z,
	\ 'handle':	expand('<sfile>'),
	\ 'begin':	reltime(),
	\ 'break':	reltime(),
	\ 'file':	[],
	\ 'delay':	[70, 90, 80, 60],
	\ 'state':	{
		\ 'buffer':		bufnr('%'),
		\ 'laststatus':		&laststatus,
		\ 'maxfuncdepth':	&maxfuncdepth,
		\ 'ruler':		&ruler,
		\ 'rulerformat':	&rulerformat,
		\ 'winheight':		&winheight,
		\ 'equalalways':	&equalalways,
		\ 'statusline':		&g:statusline,
	\ },
	\ 'data':	{
		\ 'fname':	'vimvat.txt',
		\ 'cols':	50,
		\ 'lines':	20,
		\ 'turn':	3,
		\ 'part':	[
			\ ['1st\ quatrain',	"'^Of _vim_'",		3],
			\ ['2nd\ quatrain',	"'^Mnem0nic\\$'",	3],
			\ ['3rd\ quatrain',	"'^No pop-ups'",	3],
			\ ['the\ couplet',	"'^Go to,'",		1],
		\ ],
	\ },
\ }			" [ buffer_name, line_match, line_offset ]

function! s:demo.eval() abort						" {{{1
	let l:tick	= reltime(l:self.break) + reltime(l:self.begin)
	let [l:self.char, l:self.sec]	= [(l:self.char + 1), l:tick[2]]
	let g:demo_info			= printf('%-9s %2i, %7i, %5i',
		\ l:tick[0].('.'.printf('%0*i', l:self.micro, l:tick[1]))[:2].',',
		\ (l:self.sec ? l:self.char / l:self.sec :	l:self.char),
		\ l:self.char, l:self.sec)
	let l:self.break		= reltime()
endfunction

function! s:demo.print(i, j, bname, lines) abort			" {{{1
	execute 'noautocmd belowright keepalt keepjumps '.a:lines.'split +setlocal
		\\ bufhidden=hide\ buftype=nofile\ foldcolumn&\ nobuflisted\ noswapfile
		\\ statusline=%<%f\\\ %h%m%r%=[%{g:demo_info}]\\\ %-14.14(%l,%c%V%)\\\ %P
		\\ textwidth=0\ winheight&\ winfixheight\ noequalalways +'.a:bname.'+'

	try
		let l:k		= localtime() % l:self.gear	" Seed [0-3].

		for l:c in split(join(l:self.file[a:i : a:j], "\n"), '\zs')
			let @z	= l:c
			normal! "zp
			call l:self.eval()
			execute "sleep ".l:self.delay[l:k % l:self.gear]."m"
			redrawstatus
			let l:k	+= 1
		endfor
	finally
		if l:self.data.turn
			call setbufvar(bufnr('%'), '&statusline', '')
			normal! gg
		endif

		setlocal nomodifiable
		redraw!
	endtry
endfunction

function! s:demo.run() abort						" {{{1
	let [l:self.begin, l:self.break]	= [reltime(), reltime()]

	for [l:bname, l:match, l:off] in l:self.data.part
		let l:at	= index(map(l:self.file[:], "v:val =~ ".l:match), 1)
		call l:self.print(l:at, l:at + l:off, l:bname, l:off + 1)
		let l:self.data.turn	-= 1
	endfor
endfunction

function! s:demo.errmsg(mess) abort					" {{{1
	echohl ErrorMsg| echomsg l:self.handle.': '.a:mess| echohl None
endfunction

try									" {{{1
	if !&g:modifiable
		throw 1024
	elseif !filereadable(s:demo.data.fname)
		throw 2048
	endif

	let s:demo.file		= readfile(s:demo.data.fname, '', s:demo.data.lines)
	lockvar s:demo.file
	let s:demo.data.cols	= max(map(s:demo.file[:], 'len(v:val)'))

	if len(s:demo.file) < s:demo.data.lines
		throw 4096
	elseif winwidth(0) < s:demo.data.cols
		throw 8192
	endif

	if has('autocmd') && &eventignore !~? '\v(all|vimresized)'
		augroup demo
			autocmd! demo
			autocmd VimResized	* redraw!
		augroup END
	endif

	setglobal maxfuncdepth& rulerformat& ruler
	setglobal statusline=%<%f\ %h%m%r%=%-14.14(%l,%c%V%)\ %P
	unlet! g:demo_info
	let g:demo_info	= printf('%-9s %2i, %7i, %5i', '0.00,', 0, 0, 0)
	let s:demo.gear		= len(s:demo.delay)
	let s:demo.data.turn	= len(s:demo.data.part) - 1

	if !&laststatus
		set laststatus&
	endif

	redraw!
	call s:demo.run()
catch	/\<1024\>/
	call s:demo.errmsg("Cannot make changes")
catch	/\<2048\>/
	call s:demo.errmsg('`'.s:demo.data.fname."': No such file")
catch	/\<4096\>/
	call s:demo.errmsg('`'.s:demo.data.fname
		\ ."': Invalid line count: ".len(s:demo.file)." < "
					\ .s:demo.data.lines)
catch	/\<8192\>/
	call s:demo.errmsg("Narrow width: ".winwidth(0)." < "
					\ .s:demo.data.cols)
finally
	let @z			= s:demo.reg_z
	let &g:statusline	= s:demo.state.statusline
	let &equalalways	= s:demo.state.equalalways
"	let &winheight		= s:demo.state.winheight
	let &rulerformat	= s:demo.state.rulerformat
	let &ruler		= s:demo.state.ruler
	let &maxfuncdepth	= s:demo.state.maxfuncdepth
	let &laststatus		= s:demo.state.laststatus
	let &cpoptions		= s:cpoptions

	try
		execute 'sbuffer '.s:demo.state.buffer
		lcd -
	catch	/.*/
		call s:demo.errmsg(v:exception)
	endtry

	unlet s:demo s:cpoptions
	silent! autocmd! demo
	silent! augroup! demo
endtry									" }}}1

" vim:fdm=marker:sw=8:ts=8:noet:nolist:nosta:
