return { setup = function()
	vim.api.nvim_exec([[
	augroup custom_filetypes
	autocmd!
	autocmd BufNewFile *.zig setf zig
	autocmd BufReadPost *.zig setf zig
	augroup END
	]], false)
end }
