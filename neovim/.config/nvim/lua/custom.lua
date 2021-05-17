local function confirm_mkdir()
	local dir = vim.fn.expand("%:p:h")

	if vim.fn.isdirectory(dir) > 0 then
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
