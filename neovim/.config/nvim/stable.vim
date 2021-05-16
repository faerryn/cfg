"" stable.lua

" modeline can be a security risk
set nomodeline

" switch buffers without writing to disk
set hidden

" space indents of 2
set shiftwidth=2
set tabstop=2
set noexpandtab

" yes undofiles
set undofile

" relative line numbers
set number
set relativenumber

" always show signcolumn
set signcolumn=yes

" new splits go under and right
set splitbelow
set splitright

" nice line wrapping
set linebreak
set wrap

" show tabs and trailing spaces
set list

" increment and decrement everything
set nrformats=alpha,octal,hex,bin

" menus shouldn't cobble my text
set completeopt=menuone,noinsert

" highlight matches incrementally
set inccommand=nosplit

" X11 integration
set clipboard=unnamedplus
set mouse=ar

lua << EOF
vim.api.nvim_set_keymap("", "<ScrollWheelUp>",     "<C-Y>", { noremap = true })
vim.api.nvim_set_keymap("", "<S-ScrollWheelUp>",   "<C-U>", { noremap = true })
vim.api.nvim_set_keymap("", "<ScrollWheelDown>",   "<C-E>", { noremap = true })
vim.api.nvim_set_keymap("", "<S-ScrollWheelDown>", "<C-D>", { noremap = true })
EOF

" performance
set lazyredraw

" colorscheme
set background=dark
let &termguicolors = (getenv("COLORTERM") == "truecolor")

" ripgrep for :grep
if executable("rg")
	let &grepprg = "rg --hidden --vimgrep"
	let &grepformat = "%f:%l:%c:%m"
end

" Y to eol
lua vim.api.nvim_set_keymap("", "Y", "y$", { noremap = true })

" SPC as mapleader
let g:mapleader = " "
lua vim.api.nvim_set_keymap("n", "<Leader>", "", { noremap = true })

" unimpaired bindings
lua << EOF
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
EOF

" netrw
let g:netrw_banner    = 0
let g:netrw_hide      = 1
let g:netrw_keepdir   = 0
let g:netrw_list_hide = "[[^\.\.\?/$]]"
let g:netrw_winsize   = 25

" Hacks vim#7188
if executable("xdg-open")
	nnoremap gx <Cmd>!xdg-open <cfile>&<CR><CR>
endif

" shell split commands
execute "command! Shell  edit           term://"..&shell
execute "command! Hshell vsplit         term://"..&shell
execute "command! Lshell topleft vsplit term://"..&shell
execute "command! Sshell split          term://"..&shell
execute "command! Vshell vsplit         term://"..&shell
execute "command! Tshell tabnew         term://"..&shell

" bootstrap user.nvim
let s:user_branch = "backport-nvim-0.4.4"
let s:user_branch_escaped = "backport-snvim-s0.4.4"
let s:user_install_path = stdpath("data").."/site/pack/user/opt/faerryn/user.nvim/branch/"..s:user_branch_escaped
if empty(glob(s:user_install_path))
	silent execute "!git clone --branch \""..s:user_branch.."\" --depth 1 https://github.com/faerryn/user.nvim.git \""..s:user_install_path.."\""
end
execute "packadd faerryn/user.nvim/branch/"..s:user_branch_escaped

lua << EOF
local user = require("user")
user.setup()
local use = user.use

use {
	"faerryn/user.nvim",
	branch = "backport-nvim-0.4.4",
}

-- Fixes neovim#12587
use "antoinemadec/FixCursorHold.nvim"

-- command mode shortcuts
use "ryvnf/readline.vim"

-- period (.) repeat for plugins
use "tpope/vim-repeat"

-- comment code in and out
use "tomtom/tcomment_vim"

-- align stuff
use {
	"tommcdo/vim-lion",
	init = function() vim.api.nvim_command("let g:lion_squeeze_spaces = 1") end,
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
		vim.api.nvim_command([[let g:lightline = { "colorscheme": g:colors_name }]])
		vim.api.nvim_command("autocmd ColorScheme * let g:lightline.colorscheme = g:colors_name | call lightline#enable()")
	end,
}

-- c of the future
use {
	"ziglang/zig.vim",
	init = function() vim.api.nvim_command("let g:zig_fmt_autosave = 0") end,
}

-- you can't go wrong with the classic git plugin
use {
	"tpope/vim-fugitive",
	config = function()
		vim.api.nvim_command("autocmd! fugitive BufReadPost * call FugitiveDetect(resolve(expand('<amatch>:p')))")
		vim.api.nvim_set_keymap("n", "<Leader>g", "<Cmd>Git<CR>", { noremap = true })
	end,
}

-- tpope's search and replace
use "tpope/vim-abolish"

-- wait for all installation and configs to finish
user.startup()

EOF
