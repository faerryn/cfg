--- init.lua

-- modeline can be a security risk
vim.o.modeline = false

-- switch buffers without writing to disk
vim.o.hidden = true

-- space indents of 2
vim.o.shiftwidth  = 2
vim.bo.shiftwidth = 2
vim.o.tabstop     = 2
vim.bo.tabstop    = 2
vim.o.expandtab   = false
vim.bo.expandtab  = false

-- yes undofiles
vim.o.undofile = true
vim.bo.undofile = true

-- relative line numbers
vim.wo.number = true
vim.wo.relativenumber = true

-- always show signcolumn
vim.wo.signcolumn = "yes"

-- new splits go under and right
vim.o.splitbelow = true
vim.o.splitright = true

-- nice line wrapping
vim.wo.linebreak = true
vim.wo.wrap = true

-- show tabs and trailing spaces
vim.o.list = true
vim.wo.list = true

-- increment and decrement everything
vim.o.nrformats = "alpha,octal,hex,bin"

-- menus shouldn't cobble my text
vim.o.completeopt = "menuone,noinsert"

-- highlight matches incrementally
vim.o.inccommand = "nosplit"

-- X11 integration
vim.o.clipboard = "unnamedplus"
vim.o.mouse = "ar"

vim.api.nvim_set_keymap("", "<ScrollWheelUp>",     "<C-Y>", { noremap = true })
vim.api.nvim_set_keymap("", "<S-ScrollWheelUp>",   "<C-U>", { noremap = true })
vim.api.nvim_set_keymap("", "<ScrollWheelDown>",   "<C-E>", { noremap = true })
vim.api.nvim_set_keymap("", "<S-ScrollWheelDown>", "<C-D>", { noremap = true })

-- performance
vim.o.lazyredraw = true

-- colorscheme
vim.o.background = "dark"
vim.o.termguicolors = (os.getenv("COLORTERM") == "truecolor")

-- ripgrep for :grep
if vim.fn.executable("rg") == 1 then
	vim.o.grepprg = "rg --hidden --vimgrep"
	vim.o.grepformat = "%f:%l:%c:%m"
end

-- Y to eol
vim.api.nvim_set_keymap("", "Y", "y$", { noremap = true })

-- SPC as mapleader
vim.g.mapleader = " "
vim.api.nvim_set_keymap("n", "<Leader>", "", { noremap = true })

-- unimpaired bindings
vim.api.nvim_set_keymap("n", "]a",        "<Cmd>next<CR>",      { noremap = true })
vim.api.nvim_set_keymap("n", "[a",        "<Cmd>previous<CR>",  { noremap = true })
vim.api.nvim_set_keymap("n", "]A",        "<Cmd>last<CR>",      { noremap = true })
vim.api.nvim_set_keymap("n", "[A",        "<Cmd>first<CR>",     { noremap = true })

vim.api.nvim_set_keymap("n", "]b",        "<Cmd>bnext<CR>",     { noremap = true })
vim.api.nvim_set_keymap("n", "[b",        "<Cmd>bprevious<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "]B",        "<Cmd>blast<CR>",     { noremap = true })
vim.api.nvim_set_keymap("n", "[B",        "<Cmd>bfirst<CR>",    { noremap = true })

vim.api.nvim_set_keymap("n", "<Leader>l", "<Cmd>lopen<CR>",     { noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>q", "<Cmd>copen<CR>",     { noremap = true })

vim.api.nvim_set_keymap("n", "]q",        "<Cmd>cnext<CR>",     { noremap = true })
vim.api.nvim_set_keymap("n", "[q",        "<Cmd>cprevious<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "]Q",        "<Cmd>clast<CR>",     { noremap = true })
vim.api.nvim_set_keymap("n", "[Q",        "<Cmd>cfirst<CR>",    { noremap = true })

vim.api.nvim_set_keymap("n", "]l",        "<Cmd>lnext<CR>",     { noremap = true })
vim.api.nvim_set_keymap("n", "[l",        "<Cmd>lprevious<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "]L",        "<Cmd>llast<CR>",     { noremap = true })
vim.api.nvim_set_keymap("n", "[L",        "<Cmd>lfirst<CR>",    { noremap = true })

-- highlight yank
vim.api.nvim_command("autocmd TextYankPost * lua vim.highlight.on_yank()")

-- netrw
vim.g.netrw_banner    = 0
vim.g.netrw_hide      = 1
vim.g.netrw_keepdir   = 0
vim.g.netrw_list_hide = [[^\.\.\?/$]]
vim.g.netrw_winsize   = 25

-- shell split commands
vim.api.nvim_command("command! Shell  edit           term://"..vim.o.shell)
vim.api.nvim_command("command! Hshell vsplit         term://"..vim.o.shell)
vim.api.nvim_command("command! Lshell topleft vsplit term://"..vim.o.shell)
vim.api.nvim_command("command! Sshell split          term://"..vim.o.shell)
vim.api.nvim_command("command! Vshell vsplit         term://"..vim.o.shell)
vim.api.nvim_command("command! Tshell tabnew         term://"..vim.o.shell)

-- bootstrap user.nvim
local user_install_path = vim.fn.stdpath("data").."/site/pack/user/opt/faerryn/user.nvim/default/default"
if vim.fn.empty(vim.fn.glob(user_install_path)) > 0 then
	os.execute([[git clone --depth 1 https://github.com/faerryn/user.nvim.git "]]..user_install_path..[["]])
end
vim.api.nvim_command("packadd faerryn/user.nvim/default/default")

local user = require("user")
user.setup()
local use = user.use

use "faerryn/user.nvim"

-- Fixes neovim#12587
use "antoinemadec/FixCursorHold.nvim"

-- command mode shortcuts
use "ryvnf/readline.vim"

-- period (.) repeat for plugins
use "tpope/vim-repeat"

-- nvim lua library
use "nvim-lua/plenary.nvim"

-- see what is modified
use {
	"lewis6991/gitsigns.nvim",
	after = "nvim-lua/plenary.nvim",
	config = function()
		require("gitsigns").setup {
			signs = {
				add          = { hl = "GitGutterAdd",    text = "+" },
				change       = { hl = "GitGutterChange", text = "~" },
				delete       = { hl = "GitGutterDelete", text = "_" },
				topdelete    = { hl = "GitGutterDelete", text = "‾" },
				changedelete = { hl = "GitGutterChange", text = "~" },
			},
			keymaps = {},
		}
	end,
}

-- comment code in and out
use "tomtom/tcomment_vim"

-- align stuff
use {
	"tommcdo/vim-lion",
	init = function() vim.g.lion_squeeze_spaces = 1 end,
}

-- correct syntax highlighting
use {
	"nvim-treesitter/nvim-treesitter",
	update = function() vim.api.nvim_command("TSUpdate") end,
	config = function()
		require("nvim-treesitter.configs").setup {
			ensure_installed = "maintained",
			highlight = { enable = true },
			indent = { enable = true },
		}
	end,
}

-- Atom's one theme is noice
use {
	"sonph/onehalf",
	subdir = "vim",
	config = function() vim.api.nvim_command("colorscheme onehalfdark") end,
}

-- A light statusline
use {
	"itchyny/lightline.vim",
	config = function()
		vim.g.lightline = { colorscheme = vim.g.colors_name or "default" }
		vim.api.nvim_command("autocmd ColorScheme * let g:lightline.colorscheme = g:colors_name | call lightline#enable()")
	end,
}

-- c of the future
use {
	"ziglang/zig.vim",
	init = function() vim.g.zig_fmt_autosave = false end,
}

-- tpope's search and replace
use "tpope/vim-abolish"

-- wait for all installation and configs to finish
user.startup()
