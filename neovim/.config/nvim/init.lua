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
vim.o.mouse = 'ar'

vim.o.completeopt = 'menuone,noinsert'
vim.o.confirm = true
vim.o.foldlevelstart = 99
vim.o.hidden = true
vim.o.inccommand = 'nosplit'
vim.o.iskeyword = 'a-z,A-Z,48-57,_,-'
vim.o.lazyredraw = true
vim.o.nrformats = 'alpha,octal,hex,bin'
vim.o.timeoutlen = 500

vim.o.background = 'dark'
vim.o.termguicolors = (os.getenv'COLORTERM' == 'truecolor')
vim.api.nvim_command[[autocmd ColorScheme * lua for i = 0, 15 do if vim.g['terminal_color_'..i] then vim.g['terminal_color_'..i] = nil end end]]

if vim.fn.executable('rg') == 1 then
	vim.o.grepprg = 'rg --hidden --vimgrep'
	vim.o.grepformat = '%f:%l:%c:%m'
end

local keymap_opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', 'Y', 'y$', keymap_opts)

vim.api.nvim_set_keymap('n', ']a', '<Cmd>next<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', '[a', '<Cmd>previous<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', ']A', '<Cmd>last<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', '[A', '<Cmd>first<CR>', keymap_opts)

vim.api.nvim_set_keymap('n', ']b', '<Cmd>bnext<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', '[b', '<Cmd>bprevious<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', ']B', '<Cmd>blast<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', '[B', '<Cmd>bfirst<CR>', keymap_opts)

vim.api.nvim_set_keymap('n', '<Leader>l', '<Cmd>lopen<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', '<Leader>q', '<Cmd>copen<CR>', keymap_opts)

vim.api.nvim_set_keymap('n', ']q', '<Cmd>cnext<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', '[q', '<Cmd>cprevious<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', ']Q', '<Cmd>clast<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', '[Q', '<Cmd>cfirst<CR>', keymap_opts)

vim.api.nvim_set_keymap('n', ']l', '<Cmd>lnext<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', '[l', '<Cmd>lprevious<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', ']L', '<Cmd>llast<CR>', keymap_opts)
vim.api.nvim_set_keymap('n', '[L', '<Cmd>lfirst<CR>', keymap_opts)

vim.g.netrw_banner = 0
vim.g.netrw_hide = 1
vim.g.netrw_keepdir = 0
vim.g.netrw_list_hide = [[^\.\.\?/$]]
vim.g.netrw_winsize = 25

vim.api.nvim_command[[command! Shell edit term://$SHELL]]
vim.api.nvim_command[[command! Hshell vsplit term://$SHELL]]
vim.api.nvim_command[[command! Lshell topleft vsplit term://$SHELL]]
vim.api.nvim_command[[command! Sshell split term://$SHELL]]
vim.api.nvim_command[[command! Vshell vsplit term://$SHELL]]
vim.api.nvim_command[[command! Tshell tabnew term://$SHELL]]

local use = require'use'.use

use {
	'antoinemadec/FixCursorHold.nvim',
	setup = function() vim.g.cursorhold_updatetime = 1000 end,
}

use 'ryvnf/readline.vim'
use 'tpope/vim-repeat'

use {
	'itchyny/lightline.vim',
	init = function()
		vim.g.lightline = {
			colorscheme = vim.g.colors_name,
			active = {
				left = {
					{ 'mode', 'paste' },
					{ 'readonly', 'filename', 'modified' },
				},
				right = {
					{ 'lineinfo' },
					{ 'percent' },
					{ 'fileformat', 'fileencoding', 'filetype' },
				},
			},
			inactive = {
				left = { {'filename' } },
				right = {
					{ 'lineinfo' },
					{ 'percent' },
				},
			},
			tabline = {
				left = { {'tabs' } },
				right = { },
			},
		}
	end,
	config = function()
		vim.api.nvim_command[[autocmd ColorScheme * let g:lightline.colorscheme = g:colors_name | call lightline#enable()]]
		vim.fn['lightline#enable']()
	end,
}
use {
	'norcalli/nvim-colorizer.lua',
	config = function()
		vim.api.nvim_command[[autocmd BufEnter * lua require'colorizer'.attach_to_buffer()]]
	end,
}

use 'nvim-lua/plenary.nvim'
use 'nvim-lua/popup.nvim'
use 'nvim-telescope/telescope-fzy-native.nvim'
use {
	'nvim-telescope/telescope.nvim',
	config = function()
		require'telescope'.setup{ defaults = {
			mappings = { i = {
				['<C-w>c'] = require'telescope.actions'.close,
				['<esc>'] = require'telescope.actions'.close,
				['<C-Y>'] = require'telescope.actions'.move_selection_previous,
				['<C-E>'] = require'telescope.actions'.move_selection_next,
			} },
			file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
		} }

		require'telescope'.load_extension('fzy_native')

		local keymap_opts = { noremap = true, silent = true }
		vim.api.nvim_set_keymap('n', '<Leader>f', [[<Cmd>lua require'telescope.builtin'.find_files{ hidden = true }<CR>]], keymap_opts)
		vim.api.nvim_set_keymap('n', '<Leader>b', [[<Cmd>lua require'telescope.builtin'.buffers()<CR>]], keymap_opts)
	end,
}

use {
	'lewis6991/gitsigns.nvim',
	config = function()
		require('gitsigns').setup{
			signs = {
				add          = { hl = 'GitGutterAdd',    text = '+' },
				change       = { hl = 'GitGutterChange', text = '~' },
				delete       = { hl = 'GitGutterDelete', text = '_' },
				topdelete    = { hl = 'GitGutterDelete', text = '‾' },
				changedelete = { hl = 'GitGutterChange', text = '~' },
			},
			keymaps = {},
		}
	end,
}

use 'tomtom/tcomment_vim'

use {
	'nvim-treesitter/nvim-treesitter',
	config = function()
		require'nvim-treesitter.configs'.setup{ highlight = { enable = true } }
	end,
}

use {
	'gruvbox-community/gruvbox',
	setup = function()
		vim.g.gruvbox_bold                 = 1
		vim.g.gruvbox_italic               = 1
		vim.g.gruvbox_transparent_bg       = 1
		vim.g.gruvbox_underline            = 1
		vim.g.gruvbox_undercurl            = 1
		vim.g.gruvbox_termcolors           = 256
		vim.g.gruvbox_contrast_dark        = 'medium'
		vim.g.gruvbox_contrast_light       = 'medium'
		vim.g.gruvbox_italicize_comments   = 1
		vim.g.gruvbox_italicize_strings    = 1
		vim.g.gruvbox_invert_selection     = 0
		vim.g.gruvbox_invert_signs         = 0
		vim.g.gruvbox_invert_indent_guides = 0
		vim.g.gruvbox_invert_tabline       = 0
		vim.g.gruvbox_improved_strings     = 1
		vim.g.gruvbox_improved_warnings    = 1
		vim.g.gruvbox_guisp_fallback       = 1
	end,
	config = function() vim.api.nvim_command'colorscheme gruvbox' end,
}

use {
	'ziglang/zig.vim',
	setup = function() vim.g.zig_fmt_autosave = false end,
}
