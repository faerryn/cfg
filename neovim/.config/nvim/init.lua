--- init.lua

-- autocmd group
vim.api.nvim_command("augroup custom | autocmd! | augroup END")

-- switch buffers without writing to disk
vim.opt.hidden = true

-- disable swapfile
vim.opt.swapfile = false

-- yes undofiles
vim.opt.undofile = true

-- set terminal title
vim.opt.title = true

-- show statusline and tabline
vim.opt.laststatus = 2
vim.opt.showtabline = 2

-- line numbers
vim.opt.number = true

-- always show signcolumn
vim.opt.signcolumn = "yes"

-- no wrap
vim.opt.wrap = false

-- new splits go under and right
vim.opt.splitbelow = true
vim.opt.splitright = true

-- show tabs and trailing spaces
vim.opt.list = true

-- increment and decrement everything
vim.opt.nrformats = "alpha,octal,hex,bin"

-- menus shouldn't cobble my text
vim.opt.completeopt = "menuone,noinsert"

-- highlight matches incrementally
vim.opt.inccommand = "nosplit"

-- clipboard
vim.opt.clipboard = "unnamedplus"

-- mouse
vim.opt.mouse = "ar"

-- performance
vim.opt.lazyredraw = true

-- colorscheme
vim.opt.background = "dark"
vim.opt.termguicolors = (os.getenv("COLORTERM") == "truecolor")

-- Y to eol
vim.api.nvim_set_keymap("", "Y", "y$", { noremap = true })

-- highlight yank
vim.api.nvim_command("autocmd custom TextYankPost * lua vim.highlight.on_yank()")

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

-- gui font
vim.opt.guifont = "Source Code Pro 12"

-- bootstrap user.nvim
local user_packadd_path = "faerryn/user.nvim/default/default"
local user_install_path = vim.fn.stdpath("data").."/site/pack/user/opt/"..user_packadd_path
if vim.fn.isdirectory(user_install_path) == 0 then
    os.execute([[git clone --depth 1 https://github.com/faerryn/user.nvim.git "]]..user_install_path..[["]])
end
vim.api.nvim_command("packadd "..user_packadd_path)

local user = require("user")
user.setup()
local use = user.use

use "faerryn/user.nvim"

-- command mode shortcuts
use "ryvnf/readline.vim"

-- period (.) repeat for plugins
use "tpope/vim-repeat"

-- tpope's indentation detector
use "tpope/vim-sleuth"

-- zig is the c of the future
use {
    "ziglang/zig.vim",
    init = function() vim.g.zig_fmt_autosave = 0 end,
}

-- Fixes neovim#12587
use "antoinemadec/FixCursorHold.nvim"

-- correct syntax highlighting
use {
    "nvim-treesitter/nvim-treesitter",
    update = function() vim.api.nvim_command("TSUpdate") end,
    config = function()
        require("nvim-treesitter.configs").setup {
            ensure_installed = { "bash", "c", "cpp", "lua", "zig" },
            highlight = { enable = true },
            indent = { enable = true },
        }
    end,
}

-- wait for all installation and configs to finish
user.flush()
