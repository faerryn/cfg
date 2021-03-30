return { setup = function()
	vim.api.nvim_command'packadd telescope.nvim'
	vim.api.nvim_command'packadd plenary.nvim'
	vim.api.nvim_command'packadd popup.nvim'

	require'telescope'.setup{ defaults = {
		mappings = { i = {
			['<C-w>c'] = require'telescope.actions'.close,
			['<esc>'] = require'telescope.actions'.close,
			['<C-Y>'] = require'telescope.actions'.move_selection_previous,
			['<C-E>'] = require'telescope.actions'.move_selection_next,
		} },
		file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
	} }

	vim.api.nvim_command'packadd telescope-fzy-native.nvim'
	require('telescope').load_extension('fzy_native')

	local keymap_opts = { noremap = true, silent = true }
	vim.api.nvim_set_keymap('n', '<Leader>f', [[<Cmd>lua require'telescope.builtin'.find_files{ hidden = true }<CR>]], keymap_opts)
	vim.api.nvim_set_keymap('n', '<Leader>b', [[<Cmd>lua require'telescope.builtin'.buffers()<CR>]], keymap_opts)
end }
