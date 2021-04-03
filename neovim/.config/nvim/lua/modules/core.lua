return { setup = function()
	vim.o.swapfile = false
	vim.o.undofile = true
	vim.bo.swapfile = false
	vim.bo.undofile = true

	vim.g.mapleader = ' '
	vim.api.nvim_set_keymap('n', '<Leader>', '', { noremap = true })

	vim.wo.number = true
	vim.wo.relativenumber = true
	vim.wo.signcolumn = 'yes'

	vim.o.spellcapcheck = ''
	vim.bo.spellcapcheck = ''

	vim.o.laststatus = 2
	vim.o.showtabline = 2
	vim.o.showmode = false
	vim.o.showcmd = false

	vim.o.equalalways = false
	vim.o.splitbelow = true
	vim.o.splitright = true

	vim.wo.linebreak = true
	vim.wo.wrap = false

	vim.o.tabstop = 2
	vim.o.shiftwidth = 2
	vim.o.expandtab = false
	vim.bo.tabstop = 2
	vim.bo.shiftwidth = 2
	vim.bo.expandtab = false

	vim.o.list = true
	vim.o.listchars = 'eol:↲,tab:» ,trail:⋅,extends:…,precedes:…,nbsp:⎵'
	vim.wo.list = true

	vim.o.scrolloff = 4
	vim.o.sidescrolloff = 4
	vim.wo.scrolloff = 4
	vim.wo.sidescrolloff = 4

	vim.o.clipboard = 'unnamedplus'
	vim.o.completeopt = 'menuone,noinsert'
	vim.o.confirm = true
	vim.o.foldlevelstart = 99
	vim.o.hidden = true
	vim.o.inccommand = 'nosplit'
	vim.o.iskeyword = 'a-z,A-Z,48-57,_,-'
	vim.o.lazyredraw = true
	vim.o.nrformats = 'alpha,octal,hex,bin'
	vim.o.timeoutlen = 500

	if vim.fn.executable('rg') == 1 then
		vim.o.grepprg = 'rg --hidden --vimgrep'
		vim.o.grepformat = '%f:%l:%c:%m'
	end

	vim.api.nvim_exec([[
	augroup custom_core
	autocmd!
	autocmd BufRead * lua vim.api.nvim_win_set_cursor(0, vim.api.nvim_buf_get_mark(0, '"'))
	autocmd TextYankPost * silent! lua vim.highlight.on_yank{ higroup = "Search", timeout = 300 }
	autocmd BufWritePre * call mkdir(expand('%:p:h'), 'p')
	augroup END
	]], false)

	-- FixCursorHold.nvim
	vim.api.nvim_command'packadd FixCursorHold.nvim'
	vim.g.cursorhold_updatetime = 1000

	-- vim-repeat
	vim.api.nvim_command'packadd vim-repeat'

	-- readline.vim
	vim.api.nvim_command'packadd readline.vim'
end }
