--- init.lua

-- teach vim object permanence
vim.opt.hidden = true

-- disable double-edit detection
vim.opt.swapfile = false

-- nice widget bars
vim.opt.title = true
vim.opt.laststatus = 2
vim.opt.showtabline = 2

-- visual aids
vim.opt.number = true
vim.opt.signcolumn = "yes"
vim.opt.list = true

-- split direction
vim.opt.splitbelow = true
vim.opt.splitright = true

-- indentation
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true

-- X11 integration
vim.opt.clipboard = "unnamedplus"
vim.opt.mouse = "ar"

-- nicer colors
vim.opt.background = "dark"
vim.opt.termguicolors = (os.getenv("COLORTERM") == "truecolor")

-- emacs-like cwd
vim.opt.autochdir = true

-- completion menu behavior
vim.opt.completeopt = "menuone,preview"

-- don't fold anything to begin with
vim.opt.foldlevelstart = 99

-- show the result of substitutions incrementally
vim.opt.inccommand = "nosplit"

-- don't draw scripted actions
vim.opt.lazyredraw = true

-- don't wrap text
vim.opt.wrap = false

-- Y should yank from cursor to EOL
vim.api.nvim_set_keymap("", "Y", "y$", {noremap = true})

-- <Tab> to omnifunc
vim.api.nvim_set_keymap("i", "<Tab>", "pumvisible() ? '<C-n>' : col('.') - 1 <= match(getline('.').'$', '\\S') ? '<Tab>' : len(&omnifunc) ? '<C-x><C-o>' : '<C-x><C-n>'", {expr = true, noremap = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "pumvisible() ? '<C-p>' : '<Tab>'", {expr = true, noremap = true})
vim.api.nvim_set_keymap("i", "<CR>", "pumvisible() ? '<C-y>' : '<CR>'", {expr = true, noremap = true})
vim.api.nvim_set_keymap("i", "<Esc>", "pumvisible() ? '<C-e>' : '<Esc>'", {expr = true, noremap = true})

-- define and clear augroup for `:source $MYVIMRC`
vim.api.nvim_command("augroup faerryn | autocmd! | augroup END")

-- highlight yanked text
vim.api.nvim_command("autocmd faerryn TextYankPost * lua vim.highlight.on_yank()")

-- shell split commands
for cmd, exedit in pairs({
  Hshell = "split",
  Lshell = "topleft",
  Shell = "edit",
  Sshell = "split",
  Tshell = "tabnew",
  Vshell = "vsplit",
}) do
vim.api.nvim_command(("command! " .. cmd .. " execute '" .. exedit .. " term://' . &shell"))
end

-- netrw configuration
vim.g.netrw_banner = 0
vim.g.netrw_hide = 1
vim.g.netrw_keepdir = 0
vim.g.netrw_list_hide = "^\\.\\.\\?/$"
vim.g.netrw_winsize = 25

-- user.nvim is my very own package manager!
local user_packadd_path = "faerryn/user.nvim/default"
local user_install_path = vim.fn.stdpath("data").."/site/pack/user/opt/"..user_packadd_path
if vim.fn.isdirectory(user_install_path) == 0 then
  os.execute("git clone --quiet --depth 1 https://github.com/faerryn/user.nvim.git "..vim.fn.shellescape(user_install_path))
end
vim.api.nvim_command("packadd "..vim.fn.fnameescape(user_packadd_path))
local user = require("user")
user.setup()
local use = user.use
use("faerryn/user.nvim")

-- fixes neovim#12587
use("antoinemadec/FixCursorHold.nvim")
vim.g.cursorhold_updatetime = 1000

-- readline-like keybindings in the vim command line
use("ryvnf/readline.vim")

-- a nice colorscheme
use("joshdick/onedark.vim")
vim.api.nvim_command("colorscheme onedark")

-- c of the future
use("ziglang/zig.vim")
vim.g.zig_fmt_autosave = 0

-- a lisp that is a lua, straight in Neovim!
local function build_fennel()
  os.execute("make LUA=luajit BIN_DIR=./bin LUA_LIB_DIR=./lua install")
end
use {
  "fennel",
  repo = "https://git.sr.ht/~technomancy/fennel",
  install = build_fennel,
  update = build_fennel,
}
local fennel = require("fennel")
vim.api.nvim_command("command! -nargs=* Fennel lua require('fennel').eval(<q-args>)")
vim.api.nvim_command("command! -nargs=1 -complete=file FennelFile lua require('fennel').dofile(vim.fn.expand(<q-args>))")

-- fennel syntax
use("bakpakin/fennel.vim")

-- Neovim can get smarter by sitting in (abstract syntax) trees!
use {
  "nvim-treesitter/nvim-treesitter",
  branch =  "0.5-compat",
  update = function () vim.api.nvim_command("TSUpdate") end,
}
require("nvim-treesitter.configs").setup {
  ensure_installed = {"bash", "c", "cpp", "fennel", "lua", "latex", "rust", "zig"},
  highlight = {enable = true},
  indent = {enable = true},
}
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

-- color my neovim with colors!
use("norcalli/nvim-colorizer.lua")
if vim.opt.termguicolors:get() then
  require("colorizer").setup({}, {
    RGB = true,
    RRGGBB = true,
    RRGGBBAA = false,
    css = false,
    css_fn = false,
    hsl_fn = false,
    names = false,
    rgb_fn = false,
    mode = "background",
  })
  vim.api.nvim_command("autocmd faerryn BufEnter * lua require('colorizer').attach_to_buffer(0)")
end

use("nvim-lua/plenary.nvim")
use("TimUntersberger/neogit")
require("neogit").setup()
vim.api.nvim_set_keymap("n", "<Space>g", "<Cmd>lua require('neogit').open({kind = 'split'})<CR>", {noremap = true})

use("neovim/nvim-lspconfig")
local lspconfig = require("lspconfig")
local function on_attach(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<Space>a", "<Cmd>lua vim.lsp.buf.code_action()<CR>", {noremap = true})
  vim.api.nvim_buf_set_keymap(bufnr, "x", "<Space>a", "<Cmd>lua vim.lsp.buf.range_code_action()<CR>", {noremap = true})
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<Space>r", "<Cmd>lua vim.lsp.buf.rename()<CR>", {noremap = true})
  vim.api.nvim_command("autocmd faerryn BufWritePre <buffer=" .. bufnr .. "> lua vim.lsp.buf.formatting_sync()")
  vim.api.nvim_command("autocmd faerryn CursorHold <buffer=" .. bufnr .. "> lua vim.lsp.buf.hover()")
  vim.api.nvim_command("autocmd faerryn CursorHoldI <buffer=" .. bufnr .. "> lua vim.lsp.buf.signature_help()")
end
local opts = {
  autostart = false,
  on_attach = on_attach,
}
for _, server in ipairs({"clangd", "rust_analyzer"}) do
  lspconfig[server].setup(opts)
end
