--- init.lua

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

-- menus shouldn't cobble my text
vim.opt.completeopt = "menuone,preview,noinsert"

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
vim.api.nvim_command("autocmd! TextYankPost * lua vim.highlight.on_yank()")

-- netrw
vim.g.netrw_banner    = 0
vim.g.netrw_hide      = 1
vim.g.netrw_keepdir   = 0
vim.g.netrw_list_hide = [[^\.\.\?/$]]
vim.g.netrw_winsize   = 25

-- shell split commands
vim.api.nvim_command("command! Shell  edit           term://"..vim.opt.shell:get())
vim.api.nvim_command("command! Hshell split          term://"..vim.opt.shell:get())
vim.api.nvim_command("command! Lshell topleft vsplit term://"..vim.opt.shell:get())
vim.api.nvim_command("command! Sshell split          term://"..vim.opt.shell:get())
vim.api.nvim_command("command! Vshell vsplit         term://"..vim.opt.shell:get())
vim.api.nvim_command("command! Tshell tabnew         term://"..vim.opt.shell:get())

-- bootstrap user.nvim
local user_packadd_path = "faerryn/user.nvim/default/default"
local user_install_path = vim.fn.stdpath("data").."/site/pack/user/opt/"..user_packadd_path
if vim.fn.isdirectory(user_install_path) == 0 then
  os.execute("git clone --quiet --depth 1 https://github.com/faerryn/user.nvim.git "..vim.fn.fnameescape(user_install_path))
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

-- nice colorscheme
use {
  "joshdick/onedark.vim",
  config = function() vim.api.nvim_command("colorscheme onedark") end,
}


-- cool statusline
use {
  "glepnir/galaxyline.nvim",
  config = function()
    local galaxyline = require("galaxyline")
    local colors = require('galaxyline.theme').default

    local comment_fg = string.format("#%x", vim.api.nvim_get_hl_by_name("Comment", true).foreground)

    galaxyline.section.left[1] = {
      LeftSpace = {
        provider = "WhiteSpace",
        highlight = { "NONE", colors.bg },
        separator = " ",
        separator_highlight = { "NONE", colors.bg },
      }
    }
    galaxyline.section.left[2] = {
      Modified = {
        provider = function()
          if not vim.opt.modified:get() then
            return "  "
          end
          return "● "
        end,
        highlight = { colors.red, colors.bg, "bold" },
      }
    }
    galaxyline.section.left[3] = {
      FileName = {
        provider = function()
          local bufname = vim.api.nvim_buf_get_name(0)
          if bufname:len() > 0 then
            return vim.fn.fnamemodify(bufname, ":t")
          else
            return "[No Name]"
          end
        end,
        highlight = { colors.fg, colors.bg, "bold" },
        separator = "  ",
        separator_highlight = { "NONE", colors.bg },
      }
    }
    galaxyline.section.left[4] = {
      LineColumn = {
        provider = function()
          local cursor = vim.api.nvim_win_get_cursor(0)
          return tostring(cursor[1])..":"..tostring(cursor[2])
        end,
        highlight = { colors.fg, colors.bg },
        separator = " ",
        separator_highlight = { "NONE", colors.bg },
      }
    }
    galaxyline.section.left[5] = {
      LinePercent = {
        provider = function()
          local wintop = vim.fn.line("w0")
          if wintop == 1 then
            return "Top"
          end

          local line_count = vim.api.nvim_buf_line_count(0)
          if vim.fn.line("w$") >= line_count then
            return "Bottom"
          end

          return string.format("%d%%", 100 * wintop / line_count)
        end,
        highlight = { comment_fg, colors.bg },
      }
    }

    galaxyline.section.right[1] = {
      GitBranch = {
        provider = "GitBranch",
        highlight = { comment_fg, colors.bg },
      }
    }
    galaxyline.section.right[2] = {
      FileTypeName = {
        provider = "FileTypeName",
        highlight = { colors.fg, colors.bg, "bold" },
        separator = "  ",
        separator_highlight = { "NONE", colors.bg },
      }
    }
    galaxyline.section.right[3] = {
      RightSpace = {
        provider = "WhiteSpace",
        highlight = { "NONE", colors.bg },
        separator = "   ",
        separator_highlight = { "NONE", colors.bg },
      }
    }
  end,
}

-- Fixes neovim#12587
use "antoinemadec/FixCursorHold.nvim"

-- good syntax highlighting
use {
  "nvim-treesitter/nvim-treesitter",
  update = function() vim.api.nvim_command("TSUpdate") end,
  config = function()
    require("nvim-treesitter.configs").setup {
      ensure_installed = { "bash", "c", "cpp", "lua", "latex" },
      highlight = { enable = true },
      indent = { enable = true },
    }
  end,
}

-- colorize hexes
use {
  "norcalli/nvim-colorizer.lua",
  config = function()
    require("colorizer").setup({}, {
      RGB = false,
      RRGGBB = true,
      names = false,
      RRGGBBAA = true,
      rgb_fn = false,
      hsl_fn = false,
      css = false,
      css_fn = false,
      mode = "background",
    })
    vim.api.nvim_command("augroup ColorizerSetup | autocmd! | augroup END")
    vim.api.nvim_command([[autocmd! ColorizerSetup BufEnter * lua require("colorizer").attach_to_buffer(0)]])
  end,
}

-- wait for all installation and configs to finish
user.flush()
