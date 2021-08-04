;; switch buffers without writing to disk
(set vim.opt.hidden true)

;; disable swapfile
(set vim.opt.swapfile false)

;; yes undofiles
(set vim.opt.undofile true)

;; set terminal title
(set vim.opt.title true)

;; show statusline and tabline
(set vim.opt.laststatus 2)
(set vim.opt.showtabline 2)

;; line numbers
(set vim.opt.number true)

;; always show signcolumn
(set vim.opt.signcolumn "yes")

;; no wrap
(set vim.opt.wrap false)

;; new splits go under and right
(set vim.opt.splitbelow true)
(set vim.opt.splitright true)

;; emacs-like autochdir
(set vim.opt.autochdir true)

;; indentation
(set vim.opt.tabstop 2)
(set vim.opt.shiftwidth 2)
(set vim.opt.expandtab true)

;; show tabs and trailing spaces
(set vim.opt.list true)

;; menus shouldn't cobble my text
(set vim.opt.completeopt "menuone,preview,noinsert")

;; highlight matches incrementally
(set vim.opt.inccommand "nosplit")

;; clipboard
(set vim.opt.clipboard "unnamedplus")

;; mouse
(set vim.opt.mouse "ar")

;; performance
(set vim.opt.lazyredraw true)

;; colorscheme
(set vim.opt.background "dark")
(set vim.opt.termguicolors (= (os.getenv "COLORTERM") "truecolor"))

;; Y to eol
(vim.api.nvim_set_keymap "" "Y" "y$" { "noremap" true })

;; highlight on yank
(vim.api.nvim_command "augroup faerryn | autocmd! | augroup END")
(vim.api.nvim_command "autocmd faerryn TextYankPost * lua vim.highlight.on_yank()")

;; netrw
(set vim.g.netrw_banner    0)
(set vim.g.netrw_hide      1)
(set vim.g.netrw_keepdir   0)
(set vim.g.netrw_list_hide "^\\.\\.\\?/$")
(set vim.g.netrw_winsize   25)

;; shell split commands
(vim.api.nvim_command (.. "command! Shell  edit           term://" (vim.opt.shell:get)))
(vim.api.nvim_command (.. "command! Hshell split          term://" (vim.opt.shell:get)))
(vim.api.nvim_command (.. "command! Lshell topleft vsplit term://" (vim.opt.shell:get)))
(vim.api.nvim_command (.. "command! Sshell split          term://" (vim.opt.shell:get)))
(vim.api.nvim_command (.. "command! Vshell vsplit         term://" (vim.opt.shell:get)))
(vim.api.nvim_command (.. "command! Tshell tabnew         term://" (vim.opt.shell:get)))

(let [use (. (require :user) :use)]

  ;; user.nvim manages user.nvim!
  (use "faerryn/user.nvim")

  ;; command mode shortcuts
  (use "ryvnf/readline.vim")

  ;; period (.) repeat for plugins
  (use "tpope/vim-repeat")

  ;; nice colorscheme
  (use "joshdick/onedark.vim")
  (vim.api.nvim_command "colorscheme onedark")

  ;; Fixes neovim#12587
  (use "antoinemadec/FixCursorHold.nvim")

  ;; good syntax highlighting
  (use {1 "nvim-treesitter/nvim-treesitter"
        :update (fn [] (vim.api.nvim_command "TSUpdate"))})
  ((. (require :nvim-treesitter.configs) :setup)
   {:ensure_installed ["bash" "c" "cpp" "lua" "latex" "rust"]
    :highlight {:enable true}
    :indent {:enable true}})

  ;; colorize hexes
  (when (vim.opt.termguicolors:get)
    (use "norcalli/nvim-colorizer.lua")
    ((. (require :colorizer) :setup) {}
     {:RGB true
      :RRGGBB true
      :names false
      :RRGGBBAA false
      :rgb_fn false
      :hsl_fn false
      :css false
      :css_fn false
      :mode "background"})
    (vim.api.nvim_command "autocmd faerryn BufEnter * lua require(\"colorizer\").attach_to_buffer(0)"))

  ;; magit for neovim
  (use "nvim-lua/plenary.nvim")
  (use "TimUntersberger/neogit")
  ((. (require :neogit) :setup))

  (vim.api.nvim_set_keymap "n" "<Space>g"
                           "<Cmd>lua require(\"neogit\").open({ kind = \"split\" })<CR>"
                           { :noremap true })

  ;; fennel.vim to highlight fennel files
  (use "bakpakin/fennel.vim"))
