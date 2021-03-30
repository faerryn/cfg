return { setup = function()
	vim.o.background = 'dark'
	vim.o.termguicolors = (os.getenv'COLORTERM' == 'truecolor')

	vim.api.nvim_command'packadd gruvbox'
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

	vim.api.nvim_command'colorscheme gruvbox'

	vim.api.nvim_exec([[
	augroup custom_colorscheme
	autocmd!
	autocmd ColorScheme * lua for i = 0, 15 do if vim.g['terminal_color_'..i] then vim.g['terminal_color_'..i] = nil end end
	augroup END
	]], false)
end }
