--- init.lua

-- autocmd group
vim.api.nvim_command("augroup custom | autocmd! | augroup END")

-- switch buffers without writing to disk
vim.o.hidden = true

-- indentation
vim.o.tabstop = 4
vim.bo.tabstop = 4
vim.o.shiftwidth = 4
vim.bo.shiftwidth = 4
vim.o.expandtab = true
vim.bo.expandtab = true

-- yes undofiles
vim.o.undofile = true
vim.bo.undofile = true

-- set terminal title
vim.o.title = true

-- show tabline
vim.o.showtabline = 2

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

-- clipboard
vim.o.clipboard = "unnamedplus"

-- mouse
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

-- highlight yank
vim.api.nvim_command("autocmd custom TextYankPost * lua vim.highlight.on_yank()")

-- SPC as mapleader
vim.g.mapleader = " "
vim.api.nvim_set_keymap("n", "<Leader>", "", { noremap = true })

-- netrw
vim.g.netrw_banner    = 0
vim.g.netrw_hide      = 1
vim.g.netrw_keepdir   = 0
vim.g.netrw_list_hide = [[^\.\.\?/$]]
vim.g.netrw_winsize   = 25

-- shell split commands
vim.api.nvim_command("command! Shell  edit           term://"..vim.o.shell)
vim.api.nvim_command("command! Hshell split          term://"..vim.o.shell)
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
use "tommcdo/vim-lion"

-- correct syntax highlighting
use {
    "nvim-treesitter/nvim-treesitter",
    update = function() vim.api.nvim_command("TSUpdate") end,
    config = function()
        require("nvim-treesitter.configs").setup {
            ensure_installed = { "bash", "c", "cpp", "lua" },
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

-- tpope's search and replace
use "tpope/vim-abolish"

-- tpope's indentation detector
use "tpope/vim-sleuth"

-- wait for all installation and configs to finish
user.startup()
