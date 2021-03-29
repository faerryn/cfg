return { setup = function()
	vim.o.background = 'dark'

	vim.api.nvim_exec([[
	augroup custom_colorscheme
	autocmd!
	autocmd ColorScheme * lua for i = 0, 15 do if vim.g['terminal_color_'..i] then vim.g['terminal_color_'..i] = nil end end
	augroup END
	]], false)
end }
