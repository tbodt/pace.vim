" Description:	Measure the pace of typing (in Insert mode &c.)
" Author:	Aliaksei Budavei (0x000c70 AT gmail DOT com)
" Version:	1.1
" Last Change:	2016-Dec-26
" Copyleft ())
"
" Usage:	List all doc/ locations:
"		":echo finddir('doc', &runtimepath, -1)".
"
"		Generate the help tags: ":helptags doc/".
"		Read the documentation: ":help pace.txt".

let s:cpoptions	= &cpoptions						" {{{1
set cpoptions-=C					" Join line-breaks.

if exists('g:pace_lock') || !(has('reltime') && has('autocmd') &&
	\ has('cmdline_info') && has('statusline') && has('user_commands') &&
	\ len(reltime()) == 2)
	let &cpoptions	= s:cpoptions
	unlet s:cpoptions
	finish
elseif exists('g:pace_dump') && type(g:pace_dump) == type({}) &&
	\ has_key(g:pace_dump, '0') && !max(map(map(values(g:pace_dump),
	\ '(type(get((type(v:val) == type([]) ? v:val :	[]), 0)) == type([]) &&
	\ len(v:val[0]) == 4 ? v:val[0] :	[""])'),
	\ '(type(get(v:val, 0)) != type(0)) + (type(get(v:val, 1)) != type(0)) +
	\ (type(get(v:val, 2)) != type(0)) + (type(get(v:val, 3)) != type(0))'))
	let s:carryover	= deepcopy(g:pace_dump, 1)
else	" NOTE: Call either Pace_Dump() or Pace_Free() to procure g:pace_dump!
	let s:carryover	= {'0': [[0, 0, 0, 0]]}
endif	" NOTE: The 0th key value follows a uniform depth: [[]].

let s:pace	= {
	\ 'char':	-1,
	\ 'sec':	-1,
	\ 'cchar':	0,
	\ 'ssec':	0,
	\ 'load':	0,
	\ 'mark':	0,
	\ 'micro':	len(reltime([0, 0], [0, -1])[1]),
	\ 'buffer':	bufnr('%'),
	\ 'policy':	10007,
	\ 'begin':	reltime(),
	\ 'break':	reltime(),
	\ 'dump':	s:carryover,
	\ 'pool':	{},
	\ 'state':	{
		\ 'laststatus':		&laststatus,
		\ 'maxfuncdepth':	&maxfuncdepth,
		\ 'ruler':		&ruler,
		\ 'rulerformat':	&rulerformat,
		\ 'statusline':		&g:statusline,
	\ },
	\ 'status':	{},
\ }

function! s:pace.div(dividend, divisor) abort				" {{{1
	return (a:divisor ? a:dividend / a:divisor :	a:dividend)
endfunction	" Not used in pace.eval() since it sips ~1e-5 secs a call.

function! s:pace.eval() abort						" {{{1
	let l:tick	= reltime(l:self.break) + reltime(l:self.begin)
	let [l:self.char, l:self.sec]	= [(l:self.char + 1), l:tick[2]]
	let [l:char, l:sec]		= [l:self.char + l:self.cchar,
		\ l:self.sec + l:self.ssec]
	let g:pace_info			= printf('%-9s %2i, %7i, %5i',
		\ l:tick[0].('.'.printf('%0*i', l:self.micro, l:tick[1]))[:2].',',
		\ (l:sec ? l:char / l:sec :	l:char), l:char, l:sec)
	let l:self.break		= reltime()
endfunction	" On local machine reltime()[1] spits non-padded microseconds.

" Ponder before ridding of `cchar' and `ssec', and devolving the duties upon
" `char' and `sec'.
"
" `cchar': pace.enter() offers no way of telling event calls from command line
" calls; consider that one may quit typing with <Ctrl-c>, should now pace.sec
" serve to distinguish an aborted-`null' record from a normal-exit record, now
" pace.dump[0][0][2] == pace.char?  Should the rejected count be deducted from
" the pace.char figure in pace.enter()?
"
" `ssec': reltime() returns the time elapsed between events, whereas the total
" seconds spent typing is the sum of all such runs; therefore, provide another
" field that would hold the sum of all Normal-mode time and subtract its value
" from reltime(first_hit, last_hit) in pace.leave().
"
" Moreover, `char' and `sec' must accommodate any run count policy: single
" (0000), all (1000), or buffer (2000).

function! s:pace.test(pass) abort					" {{{1
	if exists('#pace#CursorMovedI#*')
		autocmd! pace CursorMovedI
	endif

	if exists('#pace#InsertLeave#*')
		autocmd! pace InsertLeave
	endif

	if exists('g:pace_policy') && type(g:pace_policy) == type(0)
		if g:pace_policy != l:self.policy &&
				\ g:pace_policy =~ '\<1[012][01][012][0-7]\>'
			echomsg split(expand('<sfile>'), '\.\.')[-1].': @'
				\ .localtime().': g:pace_policy: '
				\ .l:self.policy.'->'.g:pace_policy
			let l:self.policy	= g:pace_policy
		endif

		unlet g:pace_policy
	endif

	if &eventignore =~? '\v(all|insert(enter|leave)|cursormovedi)'
		echomsg split(expand('<sfile>'), '\.\.')[-1].': @'
			\ .localtime().': &eventignore mask'
		return 1	" Return the same value as the following.
	elseif !l:self.policy[4] ||
		\ (v:insertmode == 'i' && l:self.policy[4] !~ '[1357]') ||
		\ (v:insertmode == 'r' && l:self.policy[4] !~ '[2367]') ||
		\ (v:insertmode == 'v' && l:self.policy[4] !~ '[4567]')
		return 1	" Imitates and(l:self.policy[4], 1) &c.
	elseif l:self.char < 0
		return -1				" No leftovers.
	elseif !l:self.char && !l:self.policy[2]
		return 4				" Discard null.
	elseif !a:pass
		return 0	" The recursion base and :doautocmd exit.
	elseif !l:self.policy[3]
		return 2				" Discard rejects.
	endif

	try
		let l:self.mark	= l:self.policy[3] == 2	" Add (-) to hit.
		return l:self.leave()			" Accept rejects.
	finally
		let l:self.mark	= 0
	endtry
endfunction

function! s:pace.leave() abort						" {{{1
	if l:self.test(0)
		return 1
	elseif !has_key(l:self.dump, l:self.buffer)
		let l:self.dump[l:self.buffer]	= [[0, 0, 0, 0]]
	endif

	" Update the logged hits and the whole count.
	let l:whole		= l:self.dump[0][0]
	let l:whole[0:3]	+= [1, 0, l:self.char, l:self.sec]
	unlet! g:pace_amin
	let g:pace_amin		= l:self.div((l:whole[2] * 60), l:whole[3])
	lockvar g:pace_amin g:pace_info

	" Append a new hit instance and fetch the buffer total entry.
	let l:total		= add(l:self.dump[l:self.buffer],
			\ [(l:self.mark ? -l:whole[0] :	l:whole[0]),
			\ l:self.begin[0], l:self.char, l:self.sec])[0]
	let [l:total[0], l:total[1]]	= [(l:total[0] + 1), l:whole[0]]
	let [l:total[2], l:total[3]]	+= [l:self.char, l:self.sec]
	let [l:self.char, l:self.sec]	= [-1, -1]	" Invalidate the count.
	let l:self.pool		= {}			" Invalidate the cache.
endfunction

function! s:pace.swap(buffer) abort					" {{{1
	let l:status	= get(l:self.status, l:self.buffer, &g:statusline)

	if bufwinnr(l:self.buffer) > 0		" Protect from local change.
		" Ferret out any doppel-gänger windows.
		call filter(range(1, winnr('$')), "winbufnr(v:val) == l:self.buffer ?
				\ setwinvar(v:val, '&statusline', l:status) : 0")
	elseif bufexists(l:self.buffer)
		execute 'sbuffer '.l:self.buffer
		call setbufvar(l:self.buffer, '&statusline', l:status)
		silent! close!
	endif

	if l:self.buffer == a:buffer
		return 1
	endif

	let [l:self.status[a:buffer], l:self.buffer]	= [&l:statusline, a:buffer]
endfunction

function! s:pace.enter() abort						" {{{1
	if l:self.test(1) == 1		" An ignored mode policy, or &ei mask.
		return 1
	elseif !exists('#pace')
		return -1
	elseif !&laststatus
		set laststatus&
	endif

	" Pre-empt the statusline value and substitute it for the one assembled.
	if bufnr('%') != l:self.buffer || len(filter(range(1, winnr('$')),
				\ 'winbufnr(v:val) == l:self.buffer')) > 1
		call l:self.swap(bufnr('%'))
	endif

	unlet! g:pace_info	" Fits: 27:46:39 wait|type @ 99 char/sec pace.
	let g:pace_info	= printf('%-9s %2i, %7i, %5i', '0.00,', 0, 0, 0)

	if winnr('$') == 1
		set rulerformat=%-48([%{g:pace_info}]%)\ %<%l,%c%V\ %=%P
	else
		setlocal statusline=%<%f\ %h%m%r%=[%{g:pace_info}]
					\\ %-14.14(%l,%c%V%)\ %P rulerformat&
	endif

	let [l:self.cchar, l:self.ssec]	= l:self.policy[1] == 1	?
		\ [l:self.dump[0][0][2], l:self.dump[0][0][3]]	:
		\ l:self.policy[1] == 2 && has_key(l:self.dump, l:self.buffer)	?
		\ [l:self.dump[l:self.buffer][0][2],
		\ l:self.dump[l:self.buffer][0][3]]	:
		\ [0, 0]
	let l:self.dump[0][0][1]		+= 1	" All InsertEnter hits.
	let [l:self.char, l:self.sec]		= [0, 0]
	let [l:self.begin, l:self.break]	= [reltime(), reltime()]

	if !exists('#pace#CursorMovedI#*')
		autocmd pace CursorMovedI	* call s:pace.eval()
	endif

	if !exists('#pace#InsertLeave#*')
		autocmd pace InsertLeave	* call s:pace.leave()
	endif
endfunction

function! Pace_Load(entropy) abort					" {{{1
	if type(a:entropy) == type(0) && !a:entropy
		if !s:pace.load || mode() != 'n'
			return 1
		endif

		call s:pace.swap(bufnr('%'))
		let &g:statusline	= s:pace.state.statusline
		let &rulerformat	= s:pace.state.rulerformat
		let &ruler		= s:pace.state.ruler
		let &maxfuncdepth	= s:pace.state.maxfuncdepth
		let &laststatus		= s:pace.state.laststatus
		let s:pace.load		= 0
		silent! autocmd! pace
		return 2
	elseif &eventignore =~? '\v(all|insert(enter|leave)|cursormovedi)'
		echomsg split(expand('<sfile>'), '\s\+')[-1].': @'
			\ .localtime().': &eventignore mask'
		return -128
	elseif s:pace.load
		return -1
	endif

	let s:pace.state.laststatus	= &laststatus
	let s:pace.state.maxfuncdepth	= &maxfuncdepth
	let s:pace.state.ruler		= &ruler
	let s:pace.state.rulerformat	= &rulerformat
	let s:pace.state.statusline	= &g:statusline
	setglobal maxfuncdepth& rulerformat& ruler
	setglobal statusline=%<%f\ %h%m%r%=%-14.14(%l,%c%V%)\ %P
	let s:pace.buffer	= bufnr('%')
	let s:pace.load		= 1
	let s:pace.status[s:pace.buffer]	= &l:statusline

	augroup pace
		autocmd! pace
		autocmd InsertEnter	* call s:pace.enter()
	augroup END
endfunction

function! Pace_Dump(entropy) abort					" {{{1
	if type(a:entropy) == type(0) && !a:entropy
		return deepcopy(s:pace.dump, 1)
	elseif !empty(s:pace.pool)			" pace.leave() empties.
		return copy(s:pace.pool)
	endif

	let s:pace.pool	= {
		\ '_buffers':	'pace    chars     secs     hits',
		\ '_rejects':	printf('%+31i',
			\ (s:pace.dump[0][0][1] - s:pace.dump[0][0][0])),
	\ }

	for l:i in keys(s:pace.dump)
		let [l:hits, l:last, l:char, l:sec]	= s:pace.dump[l:i][0][0:3]
		let s:pace.pool[printf('%08i', l:i)]	= printf('%4i %8i %8i %8i',
			\ s:pace.div(l:char, l:sec), l:char, l:sec, l:hits)
	endfor

	return copy(s:pace.pool)
endfunction

function! Pace_Free() abort						" {{{1
	if !exists('s:pace') || mode() != 'n'
		return 0
	endif

	try
		let s:pace.load	= 1
		call Pace_Dump(1)
		call Pace_Load(0)
	catch	/^Vim\%((\a\+)\)\=:E117/		" An unknown function.
		call s:pace.swap(bufnr('%'))
		silent! autocmd! pace
	finally
		silent! delcommand PaceOn
		silent! delcommand PaceOff
		silent! delcommand PaceSum
		silent! delcommand PaceDump
		silent! delcommand PaceSaveTo
		silent! delfunction Pace_Load
		silent! delfunction Pace_Dump
		unlet! g:pace_dump g:pace_pool
		let g:pace_dump	= s:pace.dump
		let g:pace_pool	= s:pace.pool
		silent! augroup! pace
		unlet s:pace
	endtry

	return 1
endfunction

" COMMANDS								" {{{1
command! -bar PaceOn	:echo Pace_Load(1)
command! -bar PaceOff	:echo Pace_Load(0)
command! -bar PaceSum	:echo join(sort(items(Pace_Dump(1))), "\n")
command! -bar -nargs=*
	\ PaceDump	:echo len([<f-args>]) == 3 ?
	\ Pace_Dump(0)[[<f-args>][0]][[<f-args>][1]][[<f-args>][2]] :
	\ len([<f-args>]) == 2 ?
	\ Pace_Dump(0)[[<f-args>][0]][[<f-args>][1]] :
	\ len([<f-args>]) == 1 ?
	\ Pace_Dump(0)[[<f-args>][0]] :
	\ Pace_Dump(0)
command! -bar PaceFree	:echo Pace_Free()

if has('modify_fname')
command! -bar -nargs=1 -complete=dir
	\ PaceSaveTo	:echo writefile(['let g:pace_dump = '
		\ .string(Pace_Dump(0))], fnamemodify(expand(<q-args>), ':p')
		\ .'/pace_'.localtime())
command! -bar -nargs=1 -complete=file
	\ PaceDemo	:execute 'lcd '
		\ .fnamemodify(expand(<q-args>), ':p:h').' | source '
		\ .fnamemodify(expand(<q-args>), ':p')
endif									" }}}1

let g:pace_lock	= 1
let &cpoptions	= s:cpoptions
unlet s:carryover s:cpoptions

" vim:fdm=marker:sw=8:ts=8:noet:nolist:nosta:
