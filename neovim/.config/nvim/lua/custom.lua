local function follow_symlink()
	local filepath = vim.api.nvim_buf_get_name(0)
	local resolved = vim.fn.resolve(filepath)

	if filepath == resolved then
		return
	end

	vim.api.nvim_buf_set_name(0, resolved)
	vim.api.nvim_command("write!")
end

local function confirm_mkdir()
	local dir = vim.fn.expand("%:h")

	if vim.fn.empty(vim.fn.glob(dir)) == 0 then
		return
	end

	if vim.fn.confirm("Create "..dir.."?", "&Yes\n&No", 2, "Question") == 2 then
		return
	end

	vim.fn.mkdir(dir, "p")
end

return {
	follow_symlink = follow_symlink,
	confirm_mkdir = confirm_mkdir,
}
