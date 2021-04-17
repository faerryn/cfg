-- modeline can be a security risk
vim.o.modeline = false

-- switch buffers without writing to disk
vim.o.hidden = true

-- space indents of 2
vim.o.shiftwidth = 2
vim.bo.shiftwidth = 2
vim.o.tabstop = 2
vim.bo.tabstop = 2
vim.o.expandtab = false
vim.bo.expandtab = false

-- no swapfiles
vim.o.swapfile = false
vim.bo.swapfile = false

-- yes undofiles
vim.o.undofile = true
vim.bo.undofile = true

-- relative line numbers
vim.wo.number = true
vim.wo.relativenumber = true

-- always show signcolumn
vim.wo.signcolumn = "yes"

-- no tabline or statusline
vim.o.laststatus = 0
vim.o.showtabline = 0

-- new splits go under and right
vim.o.splitbelow = true
vim.o.splitright = true

-- break lines at word boundaries, and disable wrap by default
vim.wo.linebreak = true
vim.wo.wrap = false

-- show tabs and trailing spaces
vim.o.list = true
vim.wo.list = true

vim.o.nrformats = "alpha,octal,hex,bin"

vim.o.completeopt = "menuone,noinsert"

vim.o.inccommand = "nosplit"

-- keybind timeout, cursorhold timeout
vim.o.timeoutlen = 500
vim.o.updatetime = 500

-- X11 integration
vim.o.clipboard = "unnamedplus"
vim.o.mouse = "ar"

vim.api.nvim_set_keymap("", "<ScrollWheelUp>", "<C-Y>", { noremap = true })
vim.api.nvim_set_keymap("", "<S-ScrollWheelUp>", "<C-U>", { noremap = true })
vim.api.nvim_set_keymap("", "<ScrollWheelDown>", "<C-E>", { noremap = true })
vim.api.nvim_set_keymap("", "<S-ScrollWheelDown>", "<C-D>", { noremap = true })

-- performance
vim.o.lazyredraw = true

-- colorscheme
vim.o.background = "dark"
vim.o.termguicolors = (os.getenv"COLORTERM" == "truecolor")
vim.api.nvim_command([[autocmd ColorScheme * lua if vim.g["terminal_color_0"] then for i = 0, 15 do vim.g["terminal_color_"..i] = nil end end]])

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
vim.api.nvim_set_keymap("n", "]a", "<Cmd>next<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "[a", "<Cmd>previous<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "]A", "<Cmd>last<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "[A", "<Cmd>first<CR>", { noremap = true })

vim.api.nvim_set_keymap("n", "]b", "<Cmd>bnext<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "[b", "<Cmd>bprevious<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "]B", "<Cmd>blast<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "[B", "<Cmd>bfirst<CR>", { noremap = true })

vim.api.nvim_set_keymap("n", "<Leader>l", "<Cmd>lopen<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<Leader>q", "<Cmd>copen<CR>", { noremap = true })

vim.api.nvim_set_keymap("n", "]q", "<Cmd>cnext<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "[q", "<Cmd>cprevious<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "]Q", "<Cmd>clast<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "[Q", "<Cmd>cfirst<CR>", { noremap = true })

vim.api.nvim_set_keymap("n", "]l", "<Cmd>lnext<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "[l", "<Cmd>lprevious<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "]L", "<Cmd>llast<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "[L", "<Cmd>lfirst<CR>", { noremap = true })

-- netrw settings
vim.g.netrw_banner = 0
vim.g.netrw_hide = 1
vim.g.netrw_keepdir = 0
vim.g.netrw_list_hide = [[^\.\.\?/$]]
vim.g.netrw_winsize = 25

-- Fixes vim#7188
vim.api.nvim_set_keymap("n", "gx", "<Cmd>!xdg-open <cfile><CR>", { noremap = true })
vim.api.nvim_set_keymap("v", "gx", "<Cmd>!xdg-open <cfile><CR>", { noremap = true })

-- shell split commands
vim.api.nvim_command("command! Shell edit term://$SHELL")
vim.api.nvim_command("command! Hshell vsplit term://$SHELL")
vim.api.nvim_command("command! Lshell topleft vsplit term://$SHELL")
vim.api.nvim_command("command! Sshell split term://$SHELL")
vim.api.nvim_command("command! Vshell vsplit term://$SHELL")
vim.api.nvim_command("command! Tshell tabnew term://$SHELL")

-- bootstrap user.nvim
local user_install_path = vim.fn.stdpath("data").."/site/pack/user/opt/faerryn/user.nvim"
if vim.fn.empty(vim.fn.glob(user_install_path)) > 0 then
	os.execute([[git clone --depth 1 https://github.com/faerryn/user.nvim.git ']]..user_install_path..[[']])
end
vim.api.nvim_command("packadd faerryn/user.nvim")

local user = require"user"
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

-- fuzzy finding interface
use "nvim-lua/popup.nvim"
use "nvim-telescope/telescope-fzy-native.nvim"
use {
	"nvim-telescope/telescope.nvim",
	after = {
		"nvim-lua/plenary.nvim",
		"nvim-lua/popup.nvim",
		"nvim-telescope/telescope-fzy-native.nvim",
	},
	config = function()
		local telescope = require("telescope")
		local actions = require("telescope.actions")

		telescope.setup { defaults = {
			mappings = { i = { ["<esc>"] = actions.close } },
			file_previewer = require("telescope.previewers").vim_buffer_cat.new,
		} }

		telescope.load_extension("fzy_native")

		vim.api.nvim_set_keymap("n", "<Leader>f", [[<Cmd>lua require("telescope.builtin").find_files{ hidden = true }<CR>]], { noremap = true })
		vim.api.nvim_set_keymap("n", "<Leader>b", [[<Cmd>lua require("telescope.builtin").buffers()<CR>]], { noremap = true })
	end,
}

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

-- correct syntax highlighting
use {
	"nvim-treesitter/nvim-treesitter",
	config = function()
		require("nvim-treesitter.configs").setup({ highlight = { enable = true } })
	end,
}

-- mandatory colorscheme
use {
	"gruvbox-community/gruvbox",
	init = function()
		vim.g.gruvbox_bold                 = 1
		vim.g.gruvbox_italic               = 1
		vim.g.gruvbox_transparent_bg       = 1
		vim.g.gruvbox_underline            = 1
		vim.g.gruvbox_undercurl            = 1
		vim.g.gruvbox_termcolors           = 256
		vim.g.gruvbox_contrast_dark        = "medium"
		vim.g.gruvbox_contrast_light       = "medium"
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
	config = function() vim.api.nvim_command("colorscheme gruvbox") end,
}

-- c of the future
use {
	"ziglang/zig.vim",
	init = function() vim.g.zig_fmt_autosave = false end,
}
